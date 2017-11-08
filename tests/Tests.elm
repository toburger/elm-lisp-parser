module Tests exposing (..)

import LispParser exposing (..)
import Test exposing (..)
import Expect


singleCase : Test
singleCase =
    describe "LispParser parsing single-exp Test"
        [ test "parse lmabda" <|
            \_ ->
                Expect.equal (Just (Lambda [ VarName "x", VarName "y" ] (Literal (FloatValue 1)))) <|
                    read "(lambda (x y) 1)"
        , test "parse if" <|
            \_ ->
                Expect.equal (Just (If (Literal (FloatValue 1)) (Literal (FloatValue 2)) (Literal (FloatValue 3)))) <|
                    read "(if 1 2 3)"
        , test "parse set!" <|
            \_ ->
                Expect.equal (Just (Set (VarName "v") (Literal (FloatValue 1)))) <|
                    read "(set! v 1)"
        , test "parse apply0" <|
            \_ ->
                Expect.equal (Just (Apply (VarRef (VarName "f")) [])) <|
                    read "(f)"
        , test "parse apply1" <|
            \_ ->
                Expect.equal (Just (Apply (VarRef (VarName "f")) [ Literal (FloatValue 1) ])) <|
                    read "(f 1)"
        , test "parse opApply0" <|
            \_ ->
                Expect.equal (Just (OpApply Add [])) <|
                    read "(+)"
        , test "parse opApply1" <|
            \_ ->
                Expect.equal (Just (OpApply Add [ Literal (FloatValue 1) ])) <|
                    read "(+ 1)"
        , test "parse list" <|
            \_ ->
                Expect.equal (Just (List [ Literal (FloatValue 1), Literal (FloatValue 2), Literal (FloatValue 3) ])) <|
                    read "(1 2 3)"
        , test "parse empty list" <|
            \_ ->
                Expect.equal (Just (List [])) <|
                    read "()"
        ]


printTest : Test
printTest =
    describe "LispParser printing exp Test"
        [ test "print lmabda" <|
            \_ ->
                Expect.equal "(lambda (x y) 1)" <|
                    print (Lambda [ VarName "x", VarName "y" ] (Literal (FloatValue 1)))
        , test "print if" <|
            \_ ->
                Expect.equal "(if 1 2 3)" <|
                    print (If (Literal (FloatValue 1)) (Literal (FloatValue 2)) (Literal (FloatValue 3)))
        , test "print set!" <|
            \_ ->
                Expect.equal "(set! v 1)" <|
                    print (Set (VarName "v") (Literal (FloatValue 1)))
        , test "print apply0" <|
            \_ ->
                Expect.equal "(f)" <|
                    print (Apply (VarRef (VarName "f")) [])
        , test "print apply1" <|
            \_ ->
                Expect.equal "(f 1)" <|
                    print (Apply (VarRef (VarName "f")) [ Literal (FloatValue 1) ])
        , test "print opApply0" <|
            \_ ->
                Expect.equal "(+)" <|
                    print (OpApply Add [])
        , test "print opApply1" <|
            \_ ->
                Expect.equal "(+ 1)" <|
                    print (OpApply Add [ Literal (FloatValue 1) ])
        , test "print list" <|
            \_ ->
                Expect.equal "(1 2 3)" <|
                    print (List [ Literal (FloatValue 1), Literal (FloatValue 2), Literal (FloatValue 3) ])
        , test "print empty list" <|
            \_ ->
                Expect.equal "()" <|
                    print (List [])
        ]
