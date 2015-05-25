{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Exit
-- import qualified Data.Text as T
import Graphics.Vty hiding (Button)
import Graphics.Vty.Widgets.All
import Control.Monad.Trans(liftIO)
import Git


createWindow items = do
  mainFocusGroup <- newFocusGroup
  textList <- newTextList items 1
  setSelectedUnfocusedAttr textList $ Just (green `on` blue)
  addToFocusGroup mainFocusGroup textList
  formattedTextList <- centered
                        -- =<< vLimit 10
                        -- =<< hLimit 50
                        =<< bordered textList
  return (formattedTextList, mainFocusGroup)

mainpoc :: IO ()
mainpoc = runInGitContext $ do
    _gitContext <- getContext
    stagedFiles <- getStagedFiles

    liftIO $ do
      mainCollection <- newCollection

      (ui2, mainFocusGroup) <- createWindow stagedFiles
      _ <- addToCollection mainCollection ui2 mainFocusGroup

      let keyHandler = \_focusGroup k mods ->
            case (k, mods) of
                (KChar 'n', [MCtrl]) -> return True
                (KEsc, []) -> exitSuccess
                _ -> return False

      mainFocusGroup `onKeyPressed` keyHandler

      runUi mainCollection $ defaultContext { focusAttr = black `on` yellow }

main :: IO ()
main = do
  e <- editWidget
  ui <- centered e
  fg <- newFocusGroup
  addToFocusGroup fg e
  c <- newCollection
  addToCollection c ui fg
  e `onActivate` \this ->
      getEditText this >>= (error . unpack . ("You entered: " ++))
  runUi c defaultContext