{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Exit
import qualified Data.Text as T
import Graphics.Vty hiding (Button)
import Graphics.Vty.Widgets.All
import Control.Monad.Trans(liftIO)
import Git

mkSecondUI items = do
  fg <- newFocusGroup
  lst <- newTextList items 1
  setSelectedUnfocusedAttr lst $ Just (green `on` blue)
  addToFocusGroup fg lst
  c <- centered
            -- =<< vLimit 10
            -- =<< hLimit 50
            =<< bordered lst
  return (c, fg)

main :: IO ()
main = runInGitContext $ do
    gitContext <- getContext
    stagedFiles <- getStagedFiles

    liftIO $ do
      coll <- newCollection

      (ui2, fg2) <- mkSecondUI stagedFiles
      switchToSecond <- addToCollection coll ui2 fg2

      let keyHandler = \something k mods ->
            case (k, mods) of
                (KChar 'n', [MCtrl]) -> return True
                (KEsc, []) -> exitSuccess
                _ -> return False

      fg2 `onKeyPressed` keyHandler

      runUi coll $ defaultContext { focusAttr = black `on` yellow }
