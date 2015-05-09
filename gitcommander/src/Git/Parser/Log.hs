module Git.Parser.Log(
    parseGitLog
) where
import Data.Time(UTCTime(..))
import qualified Git.Types                     as Git
import           Text.ParserCombinators.Parsec
type GitLogParserMonad = GenParser Char ()

parseGitLog :: [Char] -> Either ParseError [Git.Commit]
parseGitLog = parse gitLogParser "gitLogParseError: "

gitLogParser :: GitLogParserMonad [Git.Commit]
gitLogParser = many commit

commit :: GitLogParserMonad Git.Commit
commit = do
    parsedHashId <- hashId
    parsedAuthor <- author
    parsedDate <- date
    parsedMessage <- message
    parsedDiffs <- many diff
    return $ Git.Commit parsedHashId parsedAuthor parsedDate parsedMessage parsedDiffs

hashId :: GitLogParserMonad Text
hashId = do _ <- string "commit "
            parsedHashId <- many anyChar
            _ <- newline
            return $ fromString parsedHashId

author :: GitLogParserMonad Text
author = do _ <- string "Author: "
            parsedAuthor <- many anyChar
            _ <- newline
            return $ fromString parsedAuthor

date :: GitLogParserMonad UTCTime
date = undefined

message :: GitLogParserMonad Git.CommitMessage
message = undefined

diff :: GitLogParserMonad Git.FileDiff
diff = undefined
