module Git.Parser.LogSpec where
import Git.Parser.Log
import Git.Types

spec = describe "parse" $
    it "should parse a simple git log" $
        parseGitLog [] `shouldBe` [Commit {}]
