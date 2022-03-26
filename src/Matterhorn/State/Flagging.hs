module Matterhorn.State.Flagging
  ( loadFlaggedMessages
  , updateMessageFlag
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Data.Function ( on )
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HM
import           Lens.Micro.Platform

import           Network.Mattermost.Types

import           Matterhorn.State.Common
import           Matterhorn.Types


loadFlaggedMessages :: Seq FlaggedPost -> ChatState -> IO ()
loadFlaggedMessages prefs st = doAsyncWithIO Normal st $ do
  return $ Just $ do
      sequence_ [ updateMessageFlag (flaggedPostId fp) True
                | fp <- toList prefs
                , flaggedPostStatus fp
                ]


-- | Update the UI to reflect the flagged/unflagged state of a
-- message. This __does not__ talk to the Mattermost server, but
-- rather is the function we call when the Mattermost server notifies
-- us of flagged or unflagged messages.
updateMessageFlag :: PostId -> Bool -> MH ()
updateMessageFlag pId f = do
  if f
    then csResources.crFlaggedPosts %= Set.insert pId
    else csResources.crFlaggedPosts %= Set.delete pId
  msgMb <- use (csPostMap.at(pId))
  case msgMb of
    Just msg
      | Just cId <- msg^.mChannelId -> withServerChannel cId $ \chan -> do
      let isTargetMessage m = m^.mMessageId == Just (MessagePostId pId)
          h = ServerChannel cId
      csChannel(h).ccContents.cdMessages.traversed.filtered isTargetMessage.mFlagged .= f
      csPostMap.ix(pId).mFlagged .= f

      let mTId = chan^.ccInfo.cdTeamId
          updatePostOverlay :: TeamId -> MH ()
          updatePostOverlay tId = do
              -- We also want to update the post overlay if this happens
              -- while we're we're observing it
              mode <- use (csTeam tId.tsMode)
              case mode of
                PostListOverlay PostListFlagged
                  | f ->
                      csTeam tId.tsPostListOverlay.postListPosts %=
                        addMessage (msg & mFlagged .~ True)

                  -- deleting here is tricky, because it means that we
                  -- need to move the focus somewhere: we'll try moving
                  -- it _up_ unless we can't, in which case we'll try
                  -- moving it down.
                  | otherwise -> do
                      selId <- use (csTeam tId.tsPostListOverlay.postListSelected)
                      posts <- use (csTeam tId.tsPostListOverlay.postListPosts)
                      let nextId = case getNextPostId selId posts of
                            Nothing -> getPrevPostId selId posts
                            Just x  -> Just x
                      csTeam tId.tsPostListOverlay.postListSelected .= nextId
                      csTeam tId.tsPostListOverlay.postListPosts %=
                        filterMessages (((/=) `on` _mMessageId) msg)
                _ -> return ()

      case mTId of
          Nothing -> do
              ts <- use csTeams
              forM_ (HM.keys ts) updatePostOverlay
          Just tId -> updatePostOverlay tId

    _ -> return ()
