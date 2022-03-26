{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Matterhorn.Types.Common
  ( ChannelHandle(..)
  , channelHandleChannelId
  , sanitizeUserText
  , sanitizeUserText'
  , userIdForDMChannel
  )
where

import Prelude ()
import Matterhorn.Prelude

import qualified Data.Text as T
import Data.Char ( isPrint )
import           Data.Hashable ( Hashable )
import           GHC.Generics ( Generic )

import Network.Mattermost.Types ( UserText, unsafeUserText, UserId(..), Id(..), ChannelId )

data ChannelHandle =
    ServerChannel ChannelId
    deriving (Eq, Read, Show, Ord, Generic, Hashable)

channelHandleChannelId :: ChannelHandle -> Maybe ChannelId
channelHandleChannelId (ServerChannel cId) = Just cId

sanitizeUserText :: UserText -> T.Text
sanitizeUserText = sanitizeUserText' . unsafeUserText

sanitizeUserText' :: T.Text -> T.Text
sanitizeUserText' =
    T.filter (\c -> isPrint c || c == '\n') . -- remove non-printable
    T.replace "\ESC" "<ESC>" .
    T.replace "\t" " "

-- | Extract the corresponding other user from a direct channel name.
-- Returns Nothing if the string is not a direct channel name or if it
-- is but neither user ID in the name matches the current user's ID.
userIdForDMChannel :: UserId
                   -- ^ My user ID
                   -> Text
                   -- ^ The channel name
                   -> Maybe UserId
userIdForDMChannel me chanName =
    -- Direct channel names are of the form "UID__UID" where one of the
    -- UIDs is mine and the other is the other channel participant.
    let vals = T.splitOn "__" chanName
    in case vals of
        [u1, u2] -> if | (UI $ Id u1) == me  -> Just $ UI $ Id u2
                       | (UI $ Id u2) == me  -> Just $ UI $ Id u1
                       | otherwise        -> Nothing
        _ -> Nothing
