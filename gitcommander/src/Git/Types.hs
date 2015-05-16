module Git.Types where
import Data.Time(UTCTime(..))

data Commit = Commit
    { hashId :: Text
    , author :: Text
    , date :: UTCTime
    , commitMessage :: CommitMessage
    , fileDiffs :: [FileDiff]
    } deriving(Eq)
data CommitMessage = CommitMessage
    { messageHead :: Text
    , messageBody :: Text
    , signOffs :: [(Name, Email)]
    } deriving(Eq)


data FileDiff = FileDiff
    { oldFileName :: Text
    , newFileName :: Text
    , diff text
    }
type Name = Text
type Email = Text
