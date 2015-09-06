import String

import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Element exposing (runDisplay)

import LispParser exposing (..)

singleCase : Test
singleCase = suite "LispParser parsing single-exp Test"
        [ test "parse lmabda"
          << assertEqual (Just (Lambda [VarName "x", VarName "y"] (Literal (IntValue 1))))
          <| read "(lambda (x y) 1)"

        , test "parse if"
          << assertEqual (Just (If (Literal (IntValue 1)) (Literal (IntValue 2)) (Literal (IntValue 3))))
          <| read "(if 1 2 3)"

        , test "parse set!"
          << assertEqual (Just (Set (VarName "v") (Literal (IntValue 1))))
          <| read "(set! v 1)"

        , test "parse apply0"
          << assertEqual (Just (Apply (VarRef (VarName "f")) []))
          <| read "(f)"

        , test "parse apply1"
          << assertEqual (Just (Apply (VarRef (VarName "f")) [Literal (IntValue 1)]))
          <| read "(f 1)"

        , test "parse opApply0"
          << assertEqual (Just (OpApply Add []))
          <| read "(+)"

        , test "parse opApply1"
          << assertEqual (Just (OpApply Add [Literal (IntValue 1)]))
          <| read "(+ 1)"
        ]

printTest : Test
printTest = suite "LispParser printing exp Test"
        [ test "print lmabda"
          << assertEqual "(lambda (x y) 1)"
          <| print (Lambda [VarName "x", VarName "y"] (Literal (IntValue 1)))

        , test "print if"
          << assertEqual "(if 1 2 3)"
          <| print (If (Literal (IntValue 1)) (Literal (IntValue 2)) (Literal (IntValue 3)))

        , test "print set!"
          << assertEqual "(set! v 1)"
          <| print (Set (VarName "v") (Literal (IntValue 1)))

        , test "print apply0"
          << assertEqual "(f)"
          <| print (Apply (VarRef (VarName "f")) [])

        , test "print apply1"
          << assertEqual "(f 1)"
          <| print (Apply (VarRef (VarName "f")) [Literal (IntValue 1)])

        , test "print opApply0"
          << assertEqual "(+)"
          <| print (OpApply Add [])

        , test "print opApply1"
          << assertEqual "(+ 1)"
          <| print (OpApply Add [Literal (IntValue 1)])
        ]

main = runDisplay <| suite "LispParser Test" [singleCase, printTest]
