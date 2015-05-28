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


createFileList items = newTextList items 1

decorateList textList = do
  setSelectedUnfocusedAttr textList $ Just (green `on` blue)
  formattedTextList <- centered
                        -- =<< vLimit 10
                        -- =<< hLimit 50
                        =<< bordered textList
  return formattedTextList

main, main2 :: IO ()
main = runInGitContext $ do
    _gitContext <- getContext
    stagedFiles <- getStagedFiles
    unstagedFiles <- getStagedFiles
    let numOfStagedFiles = length stagedFiles
        numOfunstagedFiles = length unstagedFiles

    liftIO $ do
      mainCollection <- newCollection
      mainFocusGroup <- newFocusGroup

      stagedFilesList <- createFileList stagedFiles
      unstagedFilesList <- createFileList unstagedFiles
      
      stagedFilesList `onKeyPressed`  \_focusGroup k mods ->
            case (k, mods) of
                (KDown, []) -> selectHandler stagedFilesList unstagedFilesList (>=numOfStagedFiles)                               
                _ -> return False

      unstagedFilesList `onSelectionChange` \case
              SelectionOn k _ _ -> print k
              _ -> return ()

      ui <- (plainText "Staged Files" >>= centered)
            <--> decorateList unstagedFilesList
            <--> ((plainText "Unstaged Files" >>= centered)
            <--> decorateList stagedFilesList
            <--> ((plainText "Some" >>= centered >>= hLimit 60)
             <++> (plainText "Note" >>= centered >>= hLimit 60)))

      addToFocusGroup mainFocusGroup ui

      _ <- addToCollection mainCollection ui mainFocusGroup

      let keyHandler = \_focusGroup k mods ->
            case (k, mods) of
                (KChar 'n', [MCtrl]) -> return True
                (KEsc, []) -> exitSuccess
                _ -> return False

      mainFocusGroup `onKeyPressed` keyHandler

      runUi mainCollection $ defaultContext { focusAttr = black `on` yellow }

selectHandler::Widget (List a b) -> Widget (List a b) -> (Int -> Bool) -> IO Bool
selectHandler source destination predicate = do 
  selected <- getSelected source
  case selected of
    Nothing -> return False  
    Just ( i, _) -> if predicate i 
                      then do focus source
                              undefined
                              return True
                      else return False

main2 = do
  e <- editWidget
  countRef <- newIORef (5::Int)
  fg <- newFocusGroup
  timeText <- plainText ""
  ui <-  centered e <--> centered timeText
  addToFocusGroup fg e
  _ <- forkIO $
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