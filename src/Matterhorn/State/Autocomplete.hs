module Matterhorn.State.Autocomplete
  ( AutocompleteContext(..)
  , checkForAutocompletion
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Main ( viewportScroll, vScrollToBeginning )
import           Brick.Widgets.Edit ( editContentsL )
import qualified Brick.Widgets.List as L
import           Data.Char ( isSpace )
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import           Data.List ( sortBy, partition )
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Zipper as Z
import qualified Data.Vector as V
import           Lens.Micro.Platform ( (%=), (.=), (.~), _Just, preuse )
import qualified Skylighting.Types as Sky

import           Network.Mattermost.Types (userId, channelId, Command(..), TeamId)
import qualified Network.Mattermost.Endpoints as MM

import           Matterhorn.Constants ( userSigil, normalChannelSigil )
import {-# SOURCE #-} Matterhorn.Command ( commandList )
import           Matterhorn.State.Common
import {-# SOURCE #-} Matterhorn.State.Editing ( Direction(..), tabComplete )
import           Matterhorn.Types hiding ( newState )
import           Matterhorn.Emoji


data AutocompleteContext =
    AutocompleteContext { autocompleteManual :: Bool
                        -- ^ Whether the autocompletion was manual
                        -- (True) or automatic (False). The automatic
                        -- case is the case where the autocomplete
                        -- lookups and UI are triggered merely by
                        -- entering some initial text (such as "@").
                        -- The manual case is the case where the
                        -- autocomplete lookups and UI are triggered
                        -- explicitly by a user's TAB keypress.
                        , autocompleteFirstMatch :: Bool
                        -- ^ Once the results of the autocomplete lookup
                        -- are available, this flag determines whether
                        -- the user's input is replaced immediately
                        -- with the first available match (True) or not
                        -- (False).
                        }

-- | Check for whether the currently-edited word in the message editor
-- should cause an autocompletion UI to appear. If so, initiate a server
-- query or local cache lookup to present the completion alternatives
-- for the word at the cursor.
checkForAutocompletion :: AutocompleteContext -> MH ()
checkForAutocompletion ctx = do
    result <- getCompleterForInput ctx
    case result of
        Nothing -> resetAutocomplete
        Just (ty, runUpdater, searchString) -> do
            prevResult <- use (csCurrentTeam.tsEditState.cedAutocomplete)
            -- We should update the completion state if EITHER:
            --
            -- 1) The type changed
            --
            -- or
            --
            -- 2) The search string changed but the type did NOT change
            let shouldUpdate = ((maybe True ((/= searchString) . _acPreviousSearchString)
                                 prevResult) &&
                                (maybe True ((== ty) . _acType) prevResult)) ||
                               (maybe False ((/= ty) . _acType) prevResult)
            when shouldUpdate $ do
                csCurrentTeam.tsEditState.cedAutocompletePending .= Just searchString
                runUpdater ty ctx searchString

getCompleterForInput :: AutocompleteContext
                     -> MH (Maybe (AutocompletionType, AutocompletionType -> AutocompleteContext -> Text -> MH (), Text))
getCompleterForInput ctx = do
    z <- use (csCurrentTeam.tsEditState.cedEditor.editContentsL)

    let col = snd $ Z.cursorPosition z
        curLine = Z.currentLine z

    return $ case wordAtColumn col curLine of
        Just (startCol, w)
            | userSigil `T.isPrefixOf` w ->
                Just (ACUsers, doUserAutoCompletion, T.tail w)
            | normalChannelSigil `T.isPrefixOf` w ->
                Just (ACChannels, doChannelAutoCompletion, T.tail w)
            | ":" `T.isPrefixOf` w && autocompleteManual ctx ->
                Just (ACEmoji, doEmojiAutoCompletion, T.tail w)
            | "```" `T.isPrefixOf` w ->
                Just (ACCodeBlockLanguage, doSyntaxAutoCompletion, T.drop 3 w)
            | "/" `T.isPrefixOf` w && startCol == 0 ->
                Just (ACCommands, doCommandAutoCompletion, T.tail w)
        _ -> Nothing

-- Completion implementations

doEmojiAutoCompletion :: AutocompletionType -> AutocompleteContext -> Text -> MH ()
doEmojiAutoCompletion ty ctx searchString = do
    session <- getSession
    em <- use (csResources.crEmoji)
    tId <- use csCurrentTeamId
    withCachedAutocompleteResults tId ctx ty searchString $
        doAsyncWith Preempt $ do
            results <- getMatchingEmoji session em searchString
            let alts = EmojiCompletion <$> results
            return $ Just $ setCompletionAlternatives tId ctx searchString alts ty

doSyntaxAutoCompletion :: AutocompletionType -> AutocompleteContext -> Text -> MH ()
doSyntaxAutoCompletion ty ctx searchString = do
    mapping <- use (csResources.crSyntaxMap)
    tId <- use csCurrentTeamId
    let allNames = Sky.sShortname <$> M.elems mapping
        (prefixed, notPrefixed) = partition isPrefixed $ filter match allNames
        match = (((T.toLower searchString) `T.isInfixOf`) . T.toLower)
        isPrefixed = (((T.toLower searchString) `T.isPrefixOf`) . T.toLower)
        alts = SyntaxCompletion <$> (sort prefixed <> sort notPrefixed)
    setCompletionAlternatives tId ctx searchString alts ty

-- | This list of server commands should be hidden because they make
-- assumptions about a web-based client or otherwise just don't make
-- sense for Matterhorn.
--
-- It's worth mentioning that other official mattermost client
-- implementations use this technique, too. The web client maintains
-- a list of commands to exclude when they aren't supported in the
-- mobile client. (Really this is a design flaw; they should never be
-- advertised by the server to begin with.)
hiddenServerCommands :: [Text]
hiddenServerCommands =
    -- These commands all only work in the web client.
    [ "settings"
    , "help"
    , "collapse"
    , "expand"

    -- We don't think this command makes sense for Matterhorn.
    , "logout"

    -- We provide a version of /leave with confirmation.
    , "leave"

    -- We provide our own join UI.
    , "join"

    -- We provide our own version of this command that opens our own
    -- help UI.
    , "shortcuts"

    -- Hidden because we provide other mechanisms to switch between
    -- channels.
    , "open"
    ]

hiddenCommand :: Command -> Bool
hiddenCommand c = (T.toLower $ commandTrigger c) `elem` hiddenServerCommands

isDeletedCommand :: Command -> Bool
isDeletedCommand cmd = commandDeleteAt cmd > commandCreateAt cmd

doCommandAutoCompletion :: AutocompletionType -> AutocompleteContext -> Text -> MH ()
doCommandAutoCompletion ty ctx searchString = do
    session <- getSession
    myTid <- use csCurrentTeamId

    mCache <- preuse (csTeam(myTid).tsEditState.cedAutocomplete._Just.acCachedResponses)
    mActiveTy <- preuse (csTeam(myTid).tsEditState.cedAutocomplete._Just.acType)

    -- Command completion works a little differently than the other
    -- modes. To do command autocompletion, we want to query the server
    -- for the list of available commands and merge that list with
    -- our own list of client-provided commands. But the server's API
    -- doesn't support *searching* commands; we can only ask for the
    -- full list. That means that, unlike the other completion modes
    -- where we want to ask the server repeatedly as the search string
    -- is refined, in this case we want to ask the server only once
    -- and avoid repeating the request for the same data as the user
    -- types more of the search string. To accomplish that, we use a
    -- special cache key -- the empty string, which normal user input
    -- processing will never use -- as the cache key for the "full" list
    -- of commands obtained by merging the server's list with our own.
    -- We populate that cache entry when completion starts and then
    -- subsequent completions consult *that* list instead of asking the
    -- server again. Subsequent completions then filter and match the
    -- cached list against the user's search string.
    let entry = HM.lookup serverResponseKey =<< mCache
        -- The special cache key to use to store the merged server and
        -- client command list, sorted but otherwise unfiltered except
        -- for eliminating deleted or hidden commands.
        serverResponseKey = ""
        lowerSearch = T.toLower searchString
        matches (CommandCompletion _ name _ desc) =
            lowerSearch `T.isInfixOf` (T.toLower name) ||
            lowerSearch `T.isInfixOf` (T.toLower desc)
        matches _ = False

    if (isNothing entry || (mActiveTy /= (Just ACCommands)))
       then doAsyncWith Preempt $ do
                let clientAlts = mkAlt <$> commandList
                    mkAlt (Cmd name desc args _) =
                        (Client, name, printArgSpec args, desc)

                serverCommands <- MM.mmListCommandsForTeam myTid False session
                let filteredServerCommands =
                        filter (\c -> not (hiddenCommand c || isDeletedCommand c)) $
                        F.toList serverCommands
                    serverAlts = mkTuple <$> filteredServerCommands
                    mkTuple cmd =
                        ( Server
                        , commandTrigger cmd
                        , commandAutoCompleteHint cmd
                        , commandAutoCompleteDesc cmd
                        )
                    mkCompletion (src, name, args, desc) =
                        CommandCompletion src name args desc
                    alts = fmap mkCompletion $
                           clientAlts <> serverAlts

                return $ Just $ do
                    -- Store the complete list of alterantives in the cache
                    setCompletionAlternatives myTid ctx serverResponseKey alts ty

                    -- Also store the list of alternatives specific to
                    -- this search string
                    let newAlts = sortBy (compareCommandAlts searchString) $
                                  filter matches alts
                    setCompletionAlternatives myTid ctx searchString newAlts ty

       else case entry of
           Just alts | mActiveTy == Just ACCommands ->
               let newAlts = sortBy (compareCommandAlts searchString) $
                             filter matches alts
               in setCompletionAlternatives myTid ctx searchString newAlts ty
           _ -> return ()

compareCommandAlts :: Text -> AutocompleteAlternative -> AutocompleteAlternative -> Ordering
compareCommandAlts s (CommandCompletion _ nameA _ _)
                     (CommandCompletion _ nameB _ _) =
    let isAPrefix = s `T.isPrefixOf` nameA
        isBPrefix = s `T.isPrefixOf` nameB
    in if isAPrefix == isBPrefix
       then compare nameA nameB
       else if isAPrefix
            then LT
            else GT
compareCommandAlts _ _ _ = LT

doUserAutoCompletion :: AutocompletionType -> AutocompleteContext -> Text -> MH ()
doUserAutoCompletion ty ctx searchString = do
    session <- getSession
    tId <- use csCurrentTeamId
    myUid <- gets myUserId
    cId <- use (csCurrentChannelId(tId))

    withCachedAutocompleteResults tId ctx ty searchString $
        doAsyncWith Preempt $ do
            ac <- MM.mmAutocompleteUsers (Just tId) (Just cId) searchString session

            let active = Seq.filter (\u -> userId u /= myUid && (not $ userDeleted u))
                alts = F.toList $
                       ((\u -> UserCompletion u True) <$> (active $ MM.userAutocompleteUsers ac)) <>
                       (maybe mempty (fmap (\u -> UserCompletion u False) . active) $
                              MM.userAutocompleteOutOfChannel ac)

                specials = [ MentionAll
                           , MentionChannel
                           ]
                extras = [ SpecialMention m | m <- specials
                         , (T.toLower searchString) `T.isPrefixOf` specialMentionName m
                         ]

            return $ Just $ setCompletionAlternatives tId ctx searchString (alts <> extras) ty

doChannelAutoCompletion :: AutocompletionType -> AutocompleteContext -> Text -> MH ()
doChannelAutoCompletion ty ctx searchString = do
    session <- getSession
    tId <- use csCurrentTeamId
    cs <- use csChannels

    withCachedAutocompleteResults tId ctx ty searchString $ do
        doAsyncWith Preempt $ do
            results <- MM.mmAutocompleteChannels tId searchString session
            let alts = F.toList $ (ChannelCompletion True <$> inChannels) <>
                                  (ChannelCompletion False <$> notInChannels)
                (inChannels, notInChannels) = Seq.partition isMember results
                isMember c = isJust $ findChannelById (channelId c) cs
            return $ Just $ setCompletionAlternatives tId ctx searchString alts ty

-- Utility functions

-- | Attempt to re-use a cached autocomplete alternative list for
-- a given search string. If the cache contains no such entry (keyed
-- on search string), run the specified action, which is assumed to be
-- responsible for fetching the completion results from the server.
withCachedAutocompleteResults :: TeamId
                              -> AutocompleteContext
                              -- ^ The autocomplete context
                              -> AutocompletionType
                              -- ^ The type of autocompletion we're
                              -- doing
                              -> Text
                              -- ^ The search string to look for in the
                              -- cache
                              -> MH ()
                              -- ^ The action to execute on a cache miss
                              -> MH ()
withCachedAutocompleteResults tId ctx ty searchString act = do
    mCache <- preuse (csTeam(tId).tsEditState.cedAutocomplete._Just.acCachedResponses)
    mActiveTy <- preuse (csTeam(tId).tsEditState.cedAutocomplete._Just.acType)

    case Just ty == mActiveTy of
        True ->
            -- Does the cache have results for this search string? If
            -- so, use them; otherwise invoke the specified action.
            case HM.lookup searchString =<< mCache of
                Just alts -> setCompletionAlternatives tId ctx searchString alts ty
                Nothing -> act
        False -> act

setCompletionAlternatives :: TeamId
                          -> AutocompleteContext
                          -> Text
                          -> [AutocompleteAlternative]
                          -> AutocompletionType
                          -> MH ()
setCompletionAlternatives tId ctx searchString alts ty = do
    let list = L.list (CompletionList tId) (V.fromList $ F.toList alts) 1
        state = AutocompleteState { _acPreviousSearchString = searchString
                                  , _acCompletionList =
                                      list & L.listSelectedL .~ Nothing
                                  , _acCachedResponses = HM.fromList [(searchString, alts)]
                                  , _acType = ty
                                  }

    pending <- use (csTeam(tId).tsEditState.cedAutocompletePending)
    case pending of
        Just val | val == searchString -> do

            -- If there is already state, update it, but also cache the
            -- search results.
            csTeam(tId).tsEditState.cedAutocomplete %= \prev ->
                let newState = case prev of
                        Nothing ->
                            state
                        Just oldState ->
                            state & acCachedResponses .~
                                HM.insert searchString alts (oldState^.acCachedResponses)
                in Just newState

            mh $ vScrollToBeginning $ viewportScroll $ CompletionList tId

            when (autocompleteFirstMatch ctx) $
                tabComplete Forwards
        _ ->
            -- Do not update the state if this result does not
            -- correspond to the search string we used most recently.
            -- This happens when the editor changes faster than the
            -- async completion responses arrive from the server. If we
            -- don't check this, we show completion results that are
            -- wrong for the editor state.
            return ()

wordAtColumn :: Int -> Text -> Maybe (Int, Text)
wordAtColumn i t =
    let tokens = T.groupBy (\a b -> isSpace a == isSpace b) t
        go _ j _ | j < 0 = Nothing
        go col j ts = case ts of
            [] -> Nothing
            (w:rest) | j <= T.length w && not (isSpace $ T.head w) -> Just (col, w)
                     | otherwise -> go (col + T.length w) (j - T.length w) rest
    in go 0 i tokens
