module Matterhorn.Events.ChannelSelect where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Widgets.Edit ( handleEditorEvent )
import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Events.Keybindings
import           Matterhorn.State.Channels
import           Matterhorn.State.ChannelSelect
import           Matterhorn.State.Editing ( editingKeybindings )
import           Matterhorn.Types
import qualified Matterhorn.Zipper as Z


onEventChannelSelect :: TeamId -> Vty.Event -> MH Bool
onEventChannelSelect tId =
  handleKeyboardEvent (channelSelectKeybindings tId) $ \e -> do
      handled <- handleKeyboardEvent (editingKeybindings tId (csTeam(tId).tsChannelSelectState.channelSelectInput)) (const $ return ()) e
      when (not handled) $
          mhHandleEventLensed (csTeam(tId).tsChannelSelectState.channelSelectInput) handleEditorEvent e

      updateChannelSelectMatches tId

channelSelectKeybindings :: TeamId -> KeyConfig -> KeyHandlerMap
channelSelectKeybindings tId = mkKeybindings (channelSelectKeyHandlers tId)

channelSelectKeyHandlers :: TeamId -> [KeyEventHandler]
channelSelectKeyHandlers tId =
    [ staticKb "Switch to selected channel"
         (Vty.EvKey Vty.KEnter []) $ do
             matches <- use (csTeam(tId).tsChannelSelectState.channelSelectMatches)
             case Z.focus matches of
                 Nothing -> return ()
                 Just match -> do
                     setMode tId Main
                     setFocus tId $ channelListEntryChannelHandle $ matchEntry match

    , mkKb CancelEvent "Cancel channel selection" $ setMode tId Main
    , mkKb NextChannelEvent "Select next match" $ channelSelectNext tId
    , mkKb PrevChannelEvent "Select previous match" $ channelSelectPrevious tId
    , mkKb NextChannelEventAlternate "Select next match (alternate binding)" $ channelSelectNext tId
    , mkKb PrevChannelEventAlternate "Select previous match (alternate binding)" $ channelSelectPrevious tId
    ]
