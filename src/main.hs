import Types
import AuxFunctions
import Assembler
import Compiler
import Lexer
import Parser


testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)


-- Define a list of test cases and their expected results
testCases :: [(String, (String, String))]
testCases = [
    ("x := 5; x := x - 1;", ("", "x=4")),
    ("x := 0 - 2;", ("", "x=-2")),
    ("if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;", ("", "y=2")),
    ("x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);", ("", "x=1")),
    ("x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;", ("", "x=2")),
    ("x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;", ("", "x=2,z=4")),
    ("x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;", ("", "x=34,y=68")),
    ("x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;", ("", "x=34")),
    ("if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;", ("", "x=1")),
    ("if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;", ("", "x=2")),
    ("x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);", ("", "x=2,y=-10,z=6")),
    ("i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);", ("", "fact=3628800,i=1"))
    ]

-- Function to run and print all test cases
runAllTests :: [((String, (String, String)))] -> IO ()
runAllTests tests = mapM_ runTest tests
  where
    runTest (input, expected) = do
      let result = testParser input
      putStrLn $ "Test input: " ++ input
      putStrLn $ "Expected: " ++ show expected
      putStrLn $ "Result: " ++ show result
      putStrLn $ if result == expected then "Test Passed" else "Test Failed"
      putStrLn "----------------------------------------"

-- To execute all tests, call the runAllTests function in your main or GHCi
-- main = runAllTests testCases


--- PART 1 ---

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- test a branch
-- testAssembler [Fals,Branch [Push 1] [Push 2]] == ("1","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"


--- PART 2 ---


-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1") 
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
-- test while with only 1 statement in the do block
-- testParser "i := 10; while (not(i == 1)) do (i := i - 1;);" == ("","i=1")