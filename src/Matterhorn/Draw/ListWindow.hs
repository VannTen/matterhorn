{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Matterhorn.Draw.ListWindow
  ( drawListWindow
  , WindowPosition(..)
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Brick.Widgets.Edit
import qualified Brick.Widgets.List as L
import           Control.Monad.Trans.Reader ( withReaderT )
import qualified Data.Foldable as F
import qualified Data.Text as T
import           Graphics.Vty ( imageWidth, translateX)
import           Lens.Micro.Platform ( (%~), (.~), to )

import           Matterhorn.Themes
import           Matterhorn.Types


hLimitWithPadding :: Int -> Widget n -> Widget n
hLimitWithPadding pad contents = Widget
  { hSize  = Fixed
  , vSize  = (vSize contents)
  , render =
      withReaderT (& availWidthL  %~ (\ n -> n - (2 * pad))) $ render $ cropToContext contents
  }

data WindowPosition =
    WindowCenter
    | WindowUpperRight
    deriving (Eq, Show)

-- | Draw a ListWindowState. This draws a bordered box with the
-- window's search input and results list inside the box. The provided
-- functions determine how to render the window in various states.
drawListWindow :: ListWindowState a b
               -- ^ The window state
               -> (b -> Widget Name)
               -- ^ The function to build the window title from the
               -- current search scope
               -> (b -> Widget Name)
               -- ^ The function to generate a message for the search
               -- scope indicating that no results were found
               -> (b -> Widget Name)
               -- ^ The function to generate the editor prompt for the
               -- search scope
               -> (Bool -> a -> Widget Name)
               -- ^ The function to render an item from the window's
               -- list
               -> Maybe (Widget Name)
               -- ^ The footer widget to render underneath the search
               -- results
               -> WindowPosition
               -- ^ How to position the window layer
               -> Int
               -- ^ The maximum window width in columns
               -> Widget Name
drawListWindow st scopeHeader scopeNoResults scopePrompt renderItem footer layerPos maxWinWidth =
  positionLayer $ hLimitWithPadding 10 $ vLimit 25 $
  hLimit maxWinWidth $
  borderWithLabel title body
  where
      title = withDefAttr clientEmphAttr $
              hBox [ scopeHeader scope
                   , case st^.listWindowRecordCount of
                         Nothing -> emptyWidget
                         Just c -> txt " (" <+> str (show c) <+> txt ")"
                   ]
      positionLayer = case layerPos of
          WindowCenter -> centerLayer
          WindowUpperRight -> upperRightLayer
      body = vBox [ (padRight (Pad 1) promptMsg) <+>
                    renderEditor (txt . T.unlines) True (st^.listWindowSearchInput)
                  , cursorPositionBorder
                  , showResults
                  , fromMaybe emptyWidget footer
                  ]
      plural 1 = ""
      plural _ = "s"
      cursorPositionBorder =
          if st^.listWindowSearching
          then hBorderWithLabel $ txt "[Searching...]"
          else case st^.listWindowSearchResults.L.listSelectedL of
              Nothing -> hBorder
              Just _ ->
                  let showingFirst = "Showing first " <> show numSearchResults <>
                                     " result" <> plural numSearchResults
                      showingAll = "Showing all " <> show numSearchResults <>
                                   " result" <> plural numSearchResults
                      showing = "Showing " <> show numSearchResults <>
                                " result" <> plural numSearchResults
                      msg = case getEditContents (st^.listWindowSearchInput) of
                          [""] ->
                              case st^.listWindowRecordCount of
                                  Nothing -> showing
                                  Just total -> if numSearchResults < total
                                                then showingFirst
                                                else showingAll
                          _ -> showing
                  in hBorderWithLabel $ str $ "[" <> msg <> "]"

      scope = st^.listWindowSearchScope
      promptMsg = scopePrompt scope

      showMessage = center . withDefAttr clientEmphAttr

      showResults
        | numSearchResults == 0 = showMessage $ scopeNoResults scope
        | otherwise = renderedUserList

      renderedUserList = L.renderList renderItem True (st^.listWindowSearchResults)
      numSearchResults = F.length $ st^.listWindowSearchResults.L.listElementsL

upperRightLayer :: Widget a -> Widget a
upperRightLayer w =
    Widget (hSize w) (vSize w) $ do
        result <- render w
        c <- getContext
        let rWidth = result^.imageL.to imageWidth
            leftPaddingAmount = max 0 $ c^.availWidthL - rWidth
            paddedImage = translateX leftPaddingAmount $ result^.imageL
            off = Location (leftPaddingAmount, 0)
        if leftPaddingAmount == 0 then
            return result else
            return $ addResultOffset off
                   $ result & imageL .~ paddedImage
