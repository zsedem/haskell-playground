{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Git(runInGitContext, getStagedFiles, GitContextRef, getContext, runInTheContext) where
import           Control.Monad.State
import           Data.IORef
import           Data.Text


newtype GitContext a = GitContext { gitContext :: StateT GitContextRef IO a } 
    deriving (Functor, Applicative, Monad, MonadIO)

newtype GitContextRef = GitContextRef ( IORef GitStatus )

data GitStatus = GitStatus { stagedFiles           :: [Text]
                           , unstagedFiles :: [Text]
                           } deriving(Show)


runInGitContext :: GitContext a -> IO a
runInGitContext GitContext {..} = initGitContext >>= evalStateT gitContext

initGitContext :: IO GitContextRef
initGitContext = do ioref <- newIORef $
                              GitStatus { stagedFiles=["some.py", "some2.py"]
                                        , unstagedFiles=["some.pyc", "etc/custom.conf"]
                                        }
                    return $ GitContextRef ioref

getStagedFiles :: GitContext [Text]
getStagedFiles = stagedFiles `liftM` getGitStatus

getContext :: GitContext GitContextRef
getContext = GitContext get

runInTheContext :: GitContextRef -> GitContext a -> IO a
runInTheContext status GitContext {..}  = evalStateT gitContext status

getGitStatus :: GitContext GitStatus
getGitStatus = GitContext $ do GitContextRef ioref <- get
                               liftIO $ readIORef ioref
