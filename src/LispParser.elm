module LispParser
    exposing
        ( read
        , print
        , Exp(..)
        , VarName(..)
        , Value(..)
        , Op(..)
        )

{-| This library provides parser of simple-lisp-syntax and its AST data-structure.
Converting AST to String of lisp-expression is also supported.

# Definition
@docs Exp, VarName, Value, Op

# Lisp-expression to AST
@docs read

# AST to Lisp-expression
@docs print

-}

import Combine exposing (..)
import Combine.Char as PChar
import Combine.Num as PNumber
import List as List
import String exposing (fromList, concat)


type alias Parser a =
    Combine.Parser () a


{-| Expression (AST) type
-}
type Exp
    = Lambda (List VarName) Exp
    | If Exp Exp Exp
    | Set VarName Exp
    | Apply Exp (List Exp)
    | OpApply Op (List Exp)
    | Literal Value
    | List (List Exp)
    | VarRef VarName


{-| Variable type
-}
type VarName
    = VarName String


{-| Value type
-}
type Value
    = FloatValue Float


{-| Operator type
-}
type Op
    = Add
    | Sub
    | Mul
    | Div
    | Max
    | Min


{-| Parse Lisp-expression
-}
read : String -> Maybe Exp
read inp =
    case parse exp inp of
        Ok ( _, _, e ) ->
            Just e

        otherwise ->
            Nothing


{-| Describe Lisp-expression
-}
print : Exp -> String
print exp =
    case exp of
        Lambda vs e ->
            "(lambda (" ++ (String.concat << (List.intersperse " ")) (List.map printVar vs) ++ ") " ++ print e ++ ")"

        If e0 e1 e2 ->
            "(if " ++ print e0 ++ " " ++ print e1 ++ " " ++ print e2 ++ ")"

        Set v e ->
            "(set! " ++ printVar v ++ " " ++ print e ++ ")"

        Apply e es ->
            "(" ++ (String.concat << (List.intersperse " ")) (print e :: List.map print es) ++ ")"

        OpApply o es ->
            "(" ++ (String.concat << (List.intersperse " ")) (printOp o :: List.map print es) ++ ")"

        Literal v ->
            printVal v

        List es ->
            "(" ++ (String.concat << (List.intersperse " ")) (List.map print es) ++ ")"

        VarRef v ->
            printVar v


printVar : VarName -> String
printVar (VarName s) =
    s


printVal : Value -> String
printVal (FloatValue i) =
    toString i


printOp : Op -> String
printOp op =
    case op of
        Add ->
            "+"

        Sub ->
            "-"

        Mul ->
            "*"

        Div ->
            "/"

        Max ->
            "max"

        Min ->
            "min"



-- Exp Parser


exp : Parser Exp
exp =
    lazy (\() -> choice [ lambda, if_, set, opApply, list, apply, literal, varRef ])


list : Parser Exp
list =
    many literal
        |> map List
        |> parens
        |> tokenize


lambda : Parser Exp
lambda =
    let
        paramList =
            many (tokenize varName) |> parens |> tokenize
    in
        (tokenize << string) "lambda"
            *> map Lambda paramList
            |> andMap (lazy (\() -> exp))
            |> parens
            |> tokenize


if_ : Parser Exp
if_ =
    (tokenize << string) "if"
        *> map If exp
        |> andMap exp
        |> andMap exp
        |> parens
        |> tokenize


set : Parser Exp
set =
    (tokenize << string) "set!"
        *> map Set (tokenize varName)
        |> andMap exp
        |> parens
        |> tokenize


apply : Parser Exp
apply =
    map Apply exp
        |> andMap (many exp)
        -- : Parser Exp
        |>
            parens
        |> tokenize


opApply : Parser Exp
opApply =
    map OpApply (tokenize op)
        |> andMap (lazy (\_ -> many exp))
        |> parens
        |> tokenize



-- succeed (OpApply Add [])


literal : Parser Exp
literal =
    map Literal intValue |> tokenize


varRef : Parser Exp
varRef =
    map VarRef varName |> tokenize



-- others Parser


op : Parser Op
op =
    let
        ops =
            [ Add, Sub, Mul, Div, Max, Min ]

        toks =
            [ "+", "-", "*", "/", "max", "min" ]

        p op tok =
            map (always op) (string tok)
    in
        List.map2 p ops toks |> List.foldl or (fail "")


intValue : Parser Value
intValue =
    map FloatValue (PNumber.float <|> map toFloat PNumber.int)


varName : Parser VarName
varName =
    let
        headChar =
            PChar.lower

        tailChar =
            or PChar.lower PChar.upper
    in
        map (::) headChar
            |> andMap (many tailChar)
            |> map (VarName << String.fromList)



-- utils of Parser


separator : Parser ()
separator =
    map (always ()) (or (PChar.char ' ') (PChar.char '\n'))


tokenize : Parser a -> Parser a
tokenize p =
    many separator *> p <* many separator
