module Matterhorn.Events.ChannelSelect where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Widgets.Edit ( handleEditorEvent )
import qualified Graphics.Vty as Vty

import           Matterhorn.Events.Keybindings
import           Matterhorn.State.Channels
import           Matterhorn.State.ChannelSelect
import           Matterhorn.State.Editing ( editingKeybindings )
import           Matterhorn.Types
import qualified Matterhorn.Zipper as Z


onEventChannelSelect :: Vty.Event -> MH Bool
onEventChannelSelect =
  handleKeyboardEvent channelSelectKeybindings $ \e -> do
      handled <- handleKeyboardEvent (editingKeybindings (csCurrentTeam.tsChannelSelectState.channelSelectInput)) (const $ return ()) e
      when (not handled) $
          mhHandleEventLensed (csCurrentTeam.tsChannelSelectState.channelSelectInput) handleEditorEvent e

      updateChannelSelectMatches

channelSelectKeybindings :: KeyConfig -> KeyHandlerMap
channelSelectKeybindings = mkKeybindings channelSelectKeyHandlers

channelSelectKeyHandlers :: [KeyEventHandler]
channelSelectKeyHandlers =
    [ staticKb "Switch to selected channel"
         (Vty.EvKey Vty.KEnter []) $ do
             tId <- use csCurrentTeamId
             matches <- use (csCurrentTeam.tsChannelSelectState.channelSelectMatches)
             case Z.focus matches of
                 Nothing -> return ()
                 Just match -> do
                     setMode tId Main
                     setFocus $ channelListEntryChannelId $ matchEntry match

    , mkKb CancelEvent "Cancel channel selection" $ do
        tId <- use csCurrentTeamId
        setMode tId Main
    , mkKb NextChannelEvent "Select next match" channelSelectNext
    , mkKb PrevChannelEvent "Select previous match" channelSelectPrevious
    , mkKb NextChannelEventAlternate "Select next match (alternate binding)" channelSelectNext
    , mkKb PrevChannelEventAlternate "Select previous match (alternate binding)" channelSelectPrevious
    ]
