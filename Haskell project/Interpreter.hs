module Interpreter
  (
    -- * Types
    Prog,

    -- * Functions
    evalRaw,
    evalAdt,
  ) where
import qualified Data.Map as M
-------------------------------------------------------------------------------
--------------------------------- The Expr ADT  -------------------------------
-------------------------------------------------------------------------------

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
          | Equal Expr Expr
          | Smaller Expr Expr
          | Symbol String
          | Value Int deriving (Show, Read)

-- [Optional] TODO Implement a parser for the Expr ADT.
--

-------------------------------------------------------------------------------
---------------------------------- The Prog ADT -------------------------------
-------------------------------------------------------------------------------
data Prog = Eq String Expr
          | Seq Prog Prog
          | If Expr Prog Prog
          | While Expr Prog
          | Return Expr deriving (Show, Read)

myMap = M.empty
message::String
message2::String

--mesajele de roare posibile in evaluare unui program
message = "Uninitialized variable"
message2 = "Missing return"

--variabila folosita pentru a spune daca s-a intalnit return sau nu
ret_val::Int
ret_val = 0

--evaluation of expression
evaluateExpr myMap (Value x) = 
                      let crtMap = M.insert "x" (toInteger x) myMap in
                      case  (M.lookup "x" crtMap) of
                      	Nothing -> Left message
                      	Just value -> Right (ret_val, value, crtMap)

evaluateExpr myMap (Symbol var) = 
                      case M.lookup var myMap of
                        Nothing -> Left message
                        Just value -> Right (ret_val, value, myMap)

evaluateExpr myMap (Smaller expr1 expr2) = 
                      case (evaluateExpr myMap expr1) of
                        Left message -> Left message
                        Right (ret_val, value1, crtMap) -> 
                           case (evaluateExpr myMap expr2) of
                             Left message -> Left message
                             Right (ret_val, value2, crtMap) -> 
                                 if value1 < value2 then 
                                    Right (ret_val, 1, crtMap)
                                 else 
                                 	Right (ret_val, (-1), crtMap)

evaluateExpr myMap(Equal expr1 expr2) = 
                      case (evaluateExpr myMap expr1) of
                        Left message -> Left message
                        Right (ret_val, value1, crtMap) ->
                          case (evaluateExpr myMap expr2) of
                              Left message -> Left message
                              Right (ret_val, value2, crtMap) ->
                                if value1 == value2 then
                                   Right (ret_val, 1, crtMap)
                                else 
                                   Right (ret_val, (-1), crtMap)

evaluateExpr myMap (Mult expr1 expr2) = 
                        case (evaluateExpr myMap expr1) of
                          Left message -> Left message
                          Right (ret_val, value1, crtMap) ->
                             case (evaluateExpr myMap expr2) of
                              Left message -> Left message
                              Right (ret_val, value2, crtMap) -> 
                              	  Right (ret_val, (value1 * value2), crtMap)

evaluateExpr myMap (Sub expr1 expr2) = 
                        case (evaluateExpr myMap expr1) of
                          Left message -> Left message
                          Right (ret_val, value1, crtMap) ->
                            case (evaluateExpr myMap expr2) of
                              Left message -> Left message
                              Right (ret_val, value2, crtMap) -> 
                              	Right (ret_val, (value1 - value2), crtMap)

evaluateExpr myMap (Add expr1 expr2) = 
                        case (evaluateExpr myMap expr1) of
                          Left message -> Left message
                          Right (ret_val, value1, crtMap)->
                            case (evaluateExpr myMap expr2) of
                              Left message -> Left message
                              Right (ret_val, value2, crtMap) -> 
                              	Right (ret_val, (value1 + value2), crtMap)

--evaluation of a program
evaluateProg myMap (Eq var expr) = 
                        case (evaluateExpr myMap expr) of
                        Left message -> Left message
                        Right (ret_val, value, crtMap) -> 
                        	Right (ret_val, value, (M.insert var value myMap))

evaluateProg myMap (Seq progr1 progr2) = 
                        case (evaluateProg myMap progr1) of
                          Left message -> Left message
                          Right (ret_val, value1, crtMap) ->
                          	if ret_val == 0 then
                            	case (evaluateProg crtMap progr2) of
                              		Left message -> Left message
                             	 	Right (ret_val, value2, newMap) -> 
  										Right (ret_val, value2, newMap)
  							else
  								Right (ret_val, value1, crtMap)

evaluateProg myMap (If expr prog1 prog2)=
                         case (evaluateExpr myMap expr) of
                           Left message -> Left message
                           Right (ret_val, value, crtMap) ->
                              if value == 1 then 
                                case (evaluateProg myMap prog1) of
                                  Left message -> Left message
                                  Right (ret_val, value, crtMap) -> 
                                  	Right (ret_val, value, crtMap)
                              else
                                case (evaluateProg myMap prog2) of
                                  Left message -> Left message
                                  Right (ret_val, value, crtMap) -> 
                                  	Right (ret_val, value, crtMap)

evaluateProg myMap (While expr prog) = 
                            case (evaluateExpr myMap expr) of
                              Left message -> Left message
                              Right (ret_val, value, crtMap) ->
                                if value == 1 then
                                  case (evaluateProg myMap prog) of
                                    Left message -> Left message
                                    Right (ret_val, value, crtMap) ->
                                    	if ret_val == 0 then 
                                      		case (evaluateProg crtMap (While expr prog)) of
                                        		Left message -> Left message
                                        		Right (ret_val, value, crtMap) -> 
                                        			Right (ret_val, value, crtMap)
                                        else
                                        	Right (ret_val, value, crtMap)
                                else
                                  Right (ret_val, 0, crtMap)

evaluateProg myMap (Return expr) = 
                              case (evaluateExpr myMap expr) of
                                Left message -> Left message
                                Right (ret_val, value, crtMap) -> 
                                	Right (ret_val + 1, value, crtMap)

evalTest prog = 
              case (evaluateProg M.empty prog) of
                Left message -> Left message
                Right (ret_val, value, crtMap) -> 
                	if ret_val == 0 then
                		Left message2
                	else
                		Right (fromInteger value)
       
       
-- [Optional] TODO Implement a parser for the Prog ADT.
--

-- [Optional] TODO The *parse* function.  It receives a String - the program in
-- a "raw" format and it could return *Just* a program as an instance of the
-- *Prog* data type if no parsing errors are encountered, or Nothing if parsing
-- failed.
--
-- This is composed with evalAdt to yield the evalRaw function.
parse :: String -> Maybe Prog
parse = undefined

-------------------------------------------------------------------------------
-------------------------------- The Interpreter ------------------------------
-------------------------------------------------------------------------------

-- TODO The *evalAdt* function.  It receives the program as an instance of the
-- *Prog* data type and returns an instance of *Either String Int*; that is,
-- the result of interpreting the program.
--
-- The result of a correct program is always an Int.  This is a simplification
-- we make in order to ease the implementation.  However, we wrap this Int with
-- a *Either String* type constructor to handle errors.  The *Either* type
-- constructor is defined as:
--
-- data Either a b = Left a | Right b
--
-- and it is generally used for error handling.  That means that a value of
-- *Left a* - Left String in our case - wraps an error while a value of *Right
-- b* - Right Int in our case - wraps a correct result (notice that Right is a
-- synonym for "correct" in English).
-- 
-- For further information on Either, see the references in the statement of
-- the assignment.
--
evalAdt :: Prog -> Either String Int
evalAdt prog = evalTest prog

-- The *evalRaw* function is already implemented, but it relies on the *parse*
-- function which you have to implement.
--
-- Of couse, you can change this definition.  Only its name and type are
-- important.
evalRaw :: String -> Either String Int
evalRaw rawProg = case parse rawProg of
                    Just prog -> evalAdt prog
                    Nothing   -> Left "Syntax error"
