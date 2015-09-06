module LispParser (
    read
  , print
  , Exp(..)
  , VarName(..)
  , Value(..)
  , Op(..)) where

{-| This library provides parser of simple-lisp-syntax and its AST data-structure.
Converting AST to String of lisp-expression is also supported.

# Definition
@docs Exp, VarName, Value, Op

# Lisp-expression to AST
@docs read

# AST to Lisp-expression
@docs print

-}

import Parser exposing (..)
import Parser.Char as PChar
import Parser.Number as PNumber
import List as List
import String exposing(fromList, concat)

{-| Expression (AST) type -}
type Exp = Lambda (List VarName) Exp
  | If Exp Exp Exp
  | Set VarName Exp
  | Apply Exp (List Exp)
  | OpApply Op (List Exp)
  | Literal Value
  | VarRef VarName

{-| Variable type -}
type VarName = VarName String

{-| Value type -}
type Value = IntValue Int

{-| Operator type -}
type Op = Add | Sub | Mul | Div

{-| Parse Lisp-expression -}
read : String -> Maybe Exp
read inp =
    case parse exp inp of
        Ok e -> Just e
        otherwise -> Nothing

{-| Describe Lisp-expression -}
print : Exp -> String
print exp = case exp of
    Lambda vs e  -> "(lambda (" ++ (String.concat << (List.intersperse " ")) (List.map printVar vs) ++ ") " ++ print e ++ ")"
    If e0 e1 e2  -> "(if " ++ print e0 ++ " " ++ print e1 ++ " " ++ print e2 ++ ")"
    Set v e      -> "(set! " ++ printVar v ++ " " ++ print e ++ ")"
    Apply e es   -> "(" ++ (String.concat << (List.intersperse " ")) (print e :: List.map print es) ++ ")"
    OpApply o es -> "(" ++ (String.concat << (List.intersperse " ")) (printOp o :: List.map print es) ++ ")"
    Literal v    -> printVal v
    VarRef v     -> printVar v

printVar : VarName -> String
printVar (VarName s) = s

printVal : Value -> String
printVal (IntValue i) = toString i

printOp : Op -> String
printOp op = case op of
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"

-- Exp Parser
exp : Parser Exp
exp = recursively (\() -> choice [lambda, if_, set, apply, opApply, literal, varRef])

lambda : Parser Exp
lambda =
    let paramList = many (tokenize varName) |> PChar.parenthesized |> tokenize
    in
        (tokenize << token) "lambda"
        *> map Lambda paramList
        |> andMap (recursively (\() -> exp))
        |> PChar.parenthesized
        |> tokenize

if_ : Parser Exp
if_ =
    (tokenize << token) "if"
    *> map If exp
    |> andMap exp
    |> andMap exp
    |> PChar.parenthesized
    |> tokenize

set : Parser Exp
set =
    (tokenize << token) "set!"
    *> map Set (tokenize varName)
    |> andMap exp
    |> PChar.parenthesized
    |> tokenize

apply : Parser Exp
apply =
    map Apply exp
    |> andMap (many exp) -- : Parser Exp
    |> PChar.parenthesized
    |> tokenize

opApply : Parser Exp
opApply =
    map OpApply (tokenize op) -- : Parser ([Exp] -> Exp)
    |> andMap (many exp) -- : Parser Exp
    |> PChar.parenthesized
    |> tokenize

literal : Parser Exp
literal = map Literal intValue |> tokenize

varRef : Parser Exp
varRef = map VarRef varName |> tokenize

-- others Parser
op : Parser Op
op =
    let ops  = [Add, Sub, Mul, Div]
        toks = ["+", "-", "*", "/"]
        p op tok = map (always op) (token tok)
    in
        List.map2 p ops toks |> List.foldl or empty

intValue : Parser Value
intValue = map IntValue PNumber.integer

varName : Parser VarName
varName =
     let headChar = PChar.lower
         tailChar = PChar.lower `or` PChar.upper
     in
         map (::) headChar
         |> andMap (many tailChar)
         |> map (VarName << String.fromList)

-- utils of Parser
separator : Parser ()
separator = map (always ()) (symbol ' ' `or` symbol '\n')

tokenize : Parser a -> Parser a
tokenize p = many separator *> p <* many separator
