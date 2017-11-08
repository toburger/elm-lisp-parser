module ListInterpreter exposing (..)

import LispParser exposing (read, print, Exp(..), Op(..), Value(..), VarName(..))
import Html exposing (text, ul, li, code, pre)
import Dict


type alias Environment =
    Dict.Dict String Exp


interpretOp : Op -> ( Float -> Float -> Float, Float )
interpretOp op =
    case op of
        Add ->
            ( (+), 0 )

        Sub ->
            ( (-), 0 )

        Mul ->
            ( (*), 1 )

        Div ->
            ( (/), 1 )

        Max ->
            ( max, -2147483648 )

        Min ->
            ( min, 2147483647 )


extractValues : List Exp -> List Float
extractValues exps =
    exps
        |> List.map (interpret env)
        |> List.filterMap
            (\x ->
                case x of
                    Literal (FloatValue v) ->
                        Just v

                    _ ->
                        Nothing
            )


floatLit : Float -> Exp
floatLit =
    Literal << FloatValue


interpret : Environment -> Exp -> Exp
interpret env exp =
    case exp of
        Lambda vars exp ->
            Debug.crash "not implemented: lambda"

        If cond t f ->
            let
                condition =
                    case interpret env cond of
                        Literal (FloatValue 0) ->
                            False

                        _ ->
                            True
            in
                if condition then
                    interpret env t
                else
                    interpret env f

        Set (VarName name) exp ->
            interpret
                (Dict.insert name exp env)
                (List [])

        Apply exp exps ->
            let
                xs =
                    List.map (interpret env) exps
            in
                -- TODO: xs
                interpret env exp

        OpApply op exps ->
            let
                ( opf, base ) =
                    interpretOp op

                values =
                    extractValues exps

                value =
                    List.foldr opf base values
            in
                floatLit value

        Literal (FloatValue x) ->
            floatLit x

        List exps ->
            List (List.map (interpret env) exps)

        VarRef (VarName name) ->
            case env |> Dict.get name of
                Just x ->
                    interpret env x

                Nothing ->
                    -- My LISP interpreter simply returns an empty list
                    -- if a binding references an inexistent value.
                    -- This may not be what you expect,
                    -- but it can be helpful in situations where
                    -- you have bindings in your lisp code, but your
                    -- environment doesn't provide all the referenced values.
                    List []


env : Environment
env =
    Dict.fromList [ ( "foo", floatLit 8 ) ]


samples : List String
samples =
    [ "(- 4 3)"
    , "(if (+ 1 0) 3 45)"
    , "(if (+ 0 0) 3 45)"
    , "(min 3 45 0)"
    , "(max 3 45)"
    , "(1 2 3 4 5)"
    , "foo"
    , "(+ (- 1 2) 3 foo)"
    , "bar"
    , "(+ foo bar)"
    ]


main : Html.Html msg
main =
    samples
        |> List.map
            (\x ->
                case read x of
                    Just ast ->
                        let
                            result =
                                interpret env ast
                        in
                            li []
                                [ pre []
                                    [ text
                                        (print ast
                                            ++ "\n"
                                            ++ toString ast
                                            ++ "\n"
                                            ++ toString result
                                            ++ "\n"
                                            ++ print result
                                        )
                                    ]
                                ]

                    Nothing ->
                        text "---"
            )
        |> ul []
