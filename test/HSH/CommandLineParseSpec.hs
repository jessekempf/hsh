module HSH.CommandLineParseSpec (spec) where

import Test.Hspec

import HSH.CommandLineParse
import HSH.ShellState

spec :: Spec
spec = do
  let testEnv = setEnv "FOO" "BAR" defaultShellState

  describe "variable expander" $ do
    it "maps FOO to FOO" $
      expandVariables "FOO" defaultShellState `shouldBe` Just "FOO"
    it "maps ${FOO} to BAR" $
      expandVariables "${FOO}" testEnv `shouldBe` Just "BAR"
    it "maps quux${FOO}baz to quuxBARbaz" $
      expandVariables "quux${FOO}baz" testEnv `shouldBe` Just "quuxBARbaz"
    it "allows nesting variable references" $ do
      let nestedEnv = setEnv "BAZ" "FOO" testEnv
      expandVariables "${${BAZ}}" nestedEnv `shouldBe` Just "BAR"
    it "returns Nothing for an attempt to expand an undefined variable" $
      expandVariables "${UNDEFINED}" defaultShellState `shouldBe` Nothing

  describe "line expander" $ do
    it "returns nothing if the line is syntacitcally invalid" $
      expand (words "echo ${FOO}") defaultShellState `shouldBe` Nothing
    it "does environment variable substitution where possible" $
      expand (words "echo ${FOO}") testEnv `shouldBe` Just (words "echo BAR")

  describe "the parser" $ do
    let parseLine_ line = parseLine line defaultShellState

    it "parses basic builtin and non-builtin commands" $ do
      parseLine_ "setenv foo bar" `shouldBe` Just (SetEnv "foo" "bar")
      parseLine_ "getenv foo" `shouldBe` Just (GetEnv "foo")
      parseLine_ "some command" `shouldBe` Just (External Command{name="some", args=["command"], input = STDIN, output = STDOUT})
      parseLine_ "showstate" `shouldBe` Just DebugState
      parseLine_ "cd newdir" `shouldBe` Just (Chdir "newdir")
      parseLine_ "ls -l" `shouldBe` Just (External Command{name="ls", args=["-l"], input = STDIN, output = STDOUT})

    -- XXX: This violates the principle of least astonishment but at least it is a
    -- XXX: _defined_ behavior.
    it "treats incorrectly called builtins as external commands" $
      parseLine "setenv foo" defaultShellState `shouldBe` Just (External Command{name="setenv", args=["foo"], input = STDIN, output = STDOUT})

  describe "process specification parser" $ do
    it "parses a command without arguments" $
      commandFromString "cat" `shouldBe`
        Command{ name = "cat", args = [], input = STDIN, output = STDOUT }

    it "parses a command with arguments" $
      commandFromString "cat foo bar" `shouldBe`
        Command{ name = "cat", args = ["foo", "bar"], input = STDIN, output = STDOUT }

    it "parses a command that redirects stdin from a file" $ do
      commandFromString "cat < infile" `shouldBe`
        Command{ name = "cat", args = [], input = InputFile "infile", output = STDOUT }
      commandFromString "cat <infile" `shouldBe`
        Command{ name = "cat", args = [], input = InputFile "infile", output = STDOUT }

    it "parses a command that redirects stdout to a file" $ do
      commandFromString "cat > outfile" `shouldBe`
        Command{ name = "cat", args = [], input = STDIN, output = OutputFile "outfile" }
      commandFromString "cat >outfile" `shouldBe`
        Command{ name = "cat", args = [], input = STDIN, output = OutputFile "outfile" }

    it "parses a command that redirects stdin from a file and redirects stdout to a file" $ do
      commandFromString "cat < infile > outfile" `shouldBe`
        Command{ name = "cat", args = [], input = InputFile "infile", output = OutputFile "outfile" }
      commandFromString "cat > outfile < infile" `shouldBe`
        Command{ name = "cat", args = [], input = InputFile "infile", output = OutputFile "outfile" }

    it "parses a command that redirects stdout to another command" $ do
      commandFromString "cat foo | wc -l" `shouldBe`
        Command{
          name = "cat", args = ["foo"], input = STDIN,
          output = DownstreamProcess Command{name="wc", args = ["-l"], input = STDIN, output = STDOUT }
        }
      commandFromString "cat foo |wc -l" `shouldBe`
        Command{
          name = "cat", args = ["foo"], input = STDIN,
          output = DownstreamProcess Command{name="wc", args = ["-l"], input = STDIN, output = STDOUT }
        }
    it "parses a pipeline of the form 'cmd1 foo < infile | cmd2 bar | cmd3 > outfile'" $
      commandFromString "cmd1 foo < infile | cmd2 bar | cmd3 > outfile" `shouldBe`
        Command{
          name = "cmd1", args = ["foo"], input = InputFile "infile",
          output = DownstreamProcess Command{
            name="cmd2", args = ["bar"], input = STDIN,
            output = DownstreamProcess Command{
              name = "cmd3", args = [], input = STDIN, output = OutputFile "outfile"}
          }
        }
