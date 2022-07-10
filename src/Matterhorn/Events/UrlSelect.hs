{-# LANGUAGE RankNTypes #-}
module Matterhorn.Events.UrlSelect where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Widgets.List
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( Lens' )

import           Matterhorn.State.UrlSelect
import           Matterhorn.State.SaveAttachmentWindow
import           Matterhorn.Types
import           Matterhorn.Types.KeyEvents


onEventUrlSelect :: Lens' ChatState (MessageInterface Name i) -> Vty.Event -> MH Bool
onEventUrlSelect which =
    handleEventWith [ mhHandleKeyboardEvent (urlSelectKeybindings which)
                    , \e -> mhHandleEventLensed (which.miUrlList.ulList) handleListEvent e >> return True
                    ]

urlSelectKeybindings :: Lens' ChatState (MessageInterface Name i) -> KeyConfig KeyEvent -> KeyHandlerMap KeyEvent MH
urlSelectKeybindings which = mkKeybindings (urlSelectKeyHandlers which)

urlSelectKeyHandlers :: Lens' ChatState (MessageInterface Name i) -> [MHKeyEventHandler]
urlSelectKeyHandlers which =
    [ onKey "Open the selected URL, if any"
         (Vty.EvKey Vty.KEnter []) $
             openSelectedURL which

    , onEvent SaveAttachmentEvent "Save the selected attachment" $
        openSaveAttachmentWindow which

    , onEvent CancelEvent "Cancel URL selection" $ stopUrlSelect which

    , onEvent SelectUpEvent "Move cursor up" $
        mhHandleEventLensed (which.miUrlList.ulList) handleListEvent (Vty.EvKey Vty.KUp [])

    , onEvent SelectDownEvent "Move cursor down" $
        mhHandleEventLensed (which.miUrlList.ulList) handleListEvent (Vty.EvKey Vty.KDown [])

    , onKey "Cancel URL selection"
         (Vty.EvKey (Vty.KChar 'q') []) $ stopUrlSelect which

    ]
