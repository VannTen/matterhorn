{-# LANGUAGE TemplateHaskell #-}
module Matterhorn.InputHistory
  ( InputHistory
  , newHistory
  , readHistory
  , writeHistory
  , addHistoryEntry
  , getHistoryEntry
  , removeChannelHistory
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Control.Monad.Trans.Except
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import           Lens.Micro.Platform ( (.~), (%~), at, ix, makeLenses )
import           System.Directory ( createDirectoryIfMissing )
import           System.FilePath ( dropFileName )
import qualified System.IO.Strict as S
import qualified System.Posix.Files as P
import qualified System.Posix.Types as P

import           Matterhorn.Types.Common ( ChannelHandle )
import           Matterhorn.FilePaths
import           Matterhorn.IOUtil


data InputHistory =
    InputHistory { _historyEntries :: HashMap ChannelHandle (V.Vector Text)
                 }
                 deriving (Show)

makeLenses ''InputHistory

newHistory :: InputHistory
newHistory = InputHistory mempty

removeChannelHistory :: ChannelHandle -> InputHistory -> InputHistory
removeChannelHistory h ih = ih & historyEntries.at h .~ Nothing

historyFileMode :: P.FileMode
historyFileMode = P.unionFileModes P.ownerReadMode P.ownerWriteMode

writeHistory :: InputHistory -> IO ()
writeHistory ih = do
    historyFile <- historyFilePath
    createDirectoryIfMissing True $ dropFileName historyFile
    let entries = (\(h, z) -> (h, V.toList z)) <$>
                  (HM.toList $ ih^.historyEntries)
    writeFile historyFile $ show entries
    P.setFileMode historyFile historyFileMode

readHistory :: IO (Either String InputHistory)
readHistory = runExceptT $ do
    contents <- convertIOException (S.readFile =<< historyFilePath)
    case reads contents of
        [(val, "")] -> do
            let entries = (\(h, es) -> (h, V.fromList es)) <$> val
            return $ InputHistory $ HM.fromList entries
        _ -> throwE "Failed to parse history file"

addHistoryEntry :: Text -> ChannelHandle -> InputHistory -> InputHistory
addHistoryEntry e h ih = ih & historyEntries.at h %~ insertEntry
    where
    insertEntry Nothing  = Just $ V.singleton e
    insertEntry (Just v) =
      Just $ V.cons e (V.filter (/= e) v)

getHistoryEntry :: ChannelHandle -> Int -> InputHistory -> Maybe Text
getHistoryEntry h i ih = do
    es <- ih^.historyEntries.at h
    es ^? ix i
