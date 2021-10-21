module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL




type UnaryOp
  = NegateOp
  | AddOne
  | SubtractOne
  
type BinaryOp
  = Add
  | Multiply

type Calculator 
  = Empty
  | Value Float
  | UnaryCalculation UnaryOp Calculator
  | BinaryCalculation Calculator UnaryOp Calculator



init : Calculator
init =
  Value 0

-- UPDATE


type Msg
  = Increment
  | Decrement
  | Negate
  | Solve


update : Msg -> Calculator -> Calculator
update msg calculator =
  case msg of
    Increment ->
      UnaryCalculation AddOne calculator

    Decrement ->
      UnaryCalculation SubtractOne calculator
      
    Negate ->
      UnaryCalculation NegateOp calculator
    
    Solve ->
      case solveCalculator calculator of
        Just f -> Value f
        Nothing -> Empty

      
solveCalculator : Calculator -> Maybe Float
solveCalculator calculator =
  case calculator of
    Empty -> Nothing
    Value f -> Just f
    UnaryCalculation AddOne innerCalc ->
      lift1 (\n -> n + 1) (solveCalculator innerCalc)
    UnaryCalculation SubtractOne innerCalc ->
      lift1 (\n -> n - 1) (solveCalculator innerCalc)
    UnaryCalculation NegateOp innerCalc ->
      lift1 (\n -> (-1)*n) (solveCalculator innerCalc)
    _ -> Nothing
        
    

calculatorToText : Calculator -> String
calculatorToText calculator =
  case calculator of
    Empty ->
      "Empty"
    Value f ->
      String.fromFloat f
    UnaryCalculation op innerCalc ->
      case op of
        AddOne -> (calculatorToText innerCalc) ++ " + 1"
        SubtractOne -> (calculatorToText innerCalc) ++ " - 1"
        NegateOp -> "(-1) * (" ++ (calculatorToText innerCalc) ++ ")" 
    BinaryCalculation calculator1 op calculator2 ->
      calculatorToText calculator1 ++ "binary" ++ calculatorToText calculator2
    

-- VIEW


view : Calculator -> Html Msg
view calculator =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (calculatorToText calculator) ]
    , button [ onClick Increment ] [ text "+" ]
    , button [ onClick Negate ] [ text "*(-1)"]
    , button [ onClick Solve ] [text "="]
    ]
    
    
    
-- Lift and Bind Float Functions into Maybe Float Monad        
        
lift1 : (Float -> Float) -> Maybe Float -> Maybe Float
lift1 function x =
  case x of
    Just f -> Just (function f)
    Nothing -> Nothing
    
lift2 : (Float -> Float -> Float) -> Maybe Float -> Maybe Float -> Maybe Float
lift2 function x y =
  case x of
    Just f -> lift1 (function f) y
    Nothing -> Nothing
    
bind1 : (Float -> Maybe Float) -> Maybe Float -> Maybe Float
bind1 function x =
  case x of
    Just f -> function f
    Nothing -> Nothing
    
bind2 : (Float -> Float -> Maybe Float) -> Maybe Float -> Maybe Float -> Maybe Float
bind2 function x y =
  case x of
    Just f -> bind1 (function f) y
    Nothing -> Nothing
