{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Main where
import System.Exit
--import System.Clock
import qualified Data.Text as T
import Graphics.Vty hiding (Button)
import Graphics.Vty.Widgets.All
import Control.Concurrent
import Control.Monad.Trans(liftIO)
import Git


createFileList items = do
  mainFocusGroup <- newFocusGroup
  textList <- newTextList items 1
  setSelectedUnfocusedAttr textList $ Just (green `on` blue)
  addToFocusGroup mainFocusGroup textList
  formattedTextList <- centered
                        -- =<< vLimit 10
                        -- =<< hLimit 50
                        =<< bordered textList
  return (formattedTextList, mainFocusGroup)

main, main2 :: IO ()
main = runInGitContext $ do
    _gitContext <- getContext
    stagedFiles <- getStagedFiles

    liftIO $ do
      mainCollection <- newCollection

      (ui2, mainFocusGroup) <- createFileList stagedFiles
      _ <- addToCollection mainCollection ui2 mainFocusGroup

      let keyHandler = \_focusGroup k mods ->
            case (k, mods) of
                (KChar 'n', [MCtrl]) -> return True
                (KEsc, []) -> exitSuccess
                _ -> return False

      mainFocusGroup `onKeyPressed` keyHandler

      runUi mainCollection $ defaultContext { focusAttr = black `on` yellow }

main2 = do
  e <- editWidget
  countRef <- newIORef (5::Int)
  fg <- newFocusGroup
  timeText <- plainText ""
  ui <-  centered e <--> centered timeText
  addToFocusGroup fg e
  forkIO $
    forever $ do
      schedule $ do
        modifyIORef countRef (+1) 
        t <- readIORef countRef
        setText timeText $ fromString $ show t
      threadDelay 100000
  c <- newCollection
  addToCollection c ui fg
  e `onActivate` \this ->
      getEditText this >>= (error . ("You entered: " ++))
  runUi c defaultContext

forever f = 
  do f
     forever f