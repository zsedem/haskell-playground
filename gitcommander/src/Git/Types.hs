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


type FileDiff = Text
type Name = Text
type Email = Text
