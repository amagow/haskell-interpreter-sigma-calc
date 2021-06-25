module Target where

import Parser
import Declare
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Fail
import Control.Monad.State
{-

The type Mem is a model of memory: it is used to store objects in memory.
For this language we have to have memory, just as in the interpreter
with mutable state, because objects are mutable. That is, the method
update operation can actually modify methods stored in objects.

Therefore the memory stores all the objects that are allocated in the program.
-}

type Object = [(Label, MethodClosure)]

type Mem = [Object]

{- The replace operation, updates a method for an object in memory -}

replace :: Int -> Label -> MethodClosure -> Mem -> Mem
replace i l closure mem =
    let (before, o : after) = splitAt i mem in
    before ++ [[if l == n then (n, closure) else (n,c) | (n,c) <- o]] ++ after

{- Evaluation takes the following arguments:

SigmaTerm: The expression to be evaluated
Env: The current environment
Mem: The current memory

and returns the following:

Value: The value that is computed
Mem: The updated memory (in case method update operations have been performed)

-}

unary :: UnaryOp -> Value -> Maybe Value
unary Not (VBool b) = Just (VBool (not b))
unary Neg (VInt i)  = Just (VInt (-i))
unary op v          = error ("Invalid unary" ++ show op ++ "operation: " ++ show v)

binary :: BinaryOp -> Value -> Value -> Maybe Value
binary Declare.Add (VInt a)  (VInt b)  = Just (VInt (a + b))
binary Declare.Sub (VInt a)  (VInt b)  = Just (VInt (a - b))
binary Declare.Mult (VInt a)  (VInt b) = Just (VInt (a * b))
binary Declare.Div (VInt a)  (VInt b)  = Just (VInt (a `div` b))
binary Declare.And (VBool a) (VBool b) = Just (VBool (a && b))
binary Declare.Or  (VBool a) (VBool b) = Just (VBool (a || b))
binary Declare.LT  (VInt a)  (VInt b)  = Just (VBool (a < b))
binary Declare.LE  (VInt a)  (VInt b)  = Just (VBool (a <= b))
binary Declare.GE  (VInt a)  (VInt b)  = Just (VBool (a >= b))
binary Declare.GT  (VInt a)  (VInt b)  = Just (VBool (a > b))
binary Declare.EQ  a         b         = Just (VBool (a == b))
binary         op  a         b         = error ("Invalid binary " ++ show op ++ " operation: " ++ show a ++ ", " ++ show b) 

evaluateS :: SigmaTerm -> Env -> Mem -> Maybe (Value, Mem)
evaluateS (SigmaVar v) e mem = case lookup v e of
  Just a -> Just (a, mem)
  Nothing -> error ("Variable " ++ show v ++ " is undefined!")
evaluateS (Object o) e mem =
  let mc = map (\(l,Method v t) -> (l,Closure e v t)) o
  in Just (ObjRef (length mem), mem ++ [mc])
evaluateS (Call a l) e mem = 
    case evaluateS a e mem of
      Just (ObjRef i,mem') ->
        let ms = mem' !! i
        in case lookup l ms of
              Just (Closure env v m) -> evaluateS m ((v,ObjRef i) : env) mem'
              _ -> error ("Method not found: The method " ++ show l
                          ++ " was not found in:\n" ++ show ms) 
      _ -> error ("Type error: The expression:\n "  ++
                     show a ++ "\n does not evaluate to an object!")
evaluateS (Let x a b) e mem =
  case evaluateS a e mem of
    Just (v, mem') ->
      let e' = (x, v): e
      in evaluateS b e' mem'
evaluateS (Clone a) e mem =
  case evaluateS a e mem of
    Just (ObjRef i, mem') ->
      let ms = mem' !! i
      in Just (ObjRef (length mem'), mem' ++ [ms]) 
    _ -> error "Expression is cannot be cloned"
evaluateS (Lit a) e  mem    = Just (VInt a, mem)
evaluateS (Boolean a) e mem = Just (VBool a, mem)                           
evaluateS (Update a l (Method v m)) e mem =
  case evaluateS a e mem of
    Just (ObjRef i, mem') ->
      Just (ObjRef i, replace i l (Closure e v m) mem')
    _ -> error "Expression cannot be updated"
evaluateS (Binary op a b) e mem =
  case evaluateS a e mem of
    Just (v1,mem1) ->
      case evaluateS b e mem1 of
        Just (v2,mem2) ->
          case binary op v1 v2 of
            Just result -> Just (result, mem2)
        _ -> error "Error in Binary operation"
    _ -> error "Error in Binary operation"
evaluateS (Unary op a) e mem = 
  case evaluateS a e mem of
    Just (v, mem') -> 
       case unary op v of
        Just result -> Just (result, mem')
    _ -> error "Error in Unary operation"
evaluateS (If a b c) e mem = 
  case evaluateS a e mem of
    Just (VBool test, mem') ->
      evaluateS (if test then b else c) e mem'
    _ -> error "Type error"
    

access :: Int -> Mem -> Object
access i mem = mem !! i


-- Question 4

newMemory   :: Object -> MaybeT (State Mem) Value
newMemory obj                 = do 
  mem <- lift get
  lift . put $ mem ++ [obj]
  return (ObjRef (length mem))

readMemory  :: Value -> MaybeT (State Mem) Object
readMemory (ObjRef i)                  = do
  mem <- lift get
  return (access i mem)

cloneMemory :: Value -> MaybeT (State Mem) Value
cloneMemory (ObjRef i)        = do
  mem <- lift get
  lift . put $ mem ++ [access i mem]
  return (ObjRef (length mem))

callMethod :: Value -> Label -> MaybeT (State Mem) Value
callMethod (ObjRef i) l  = do
  mem <- lift get
  let ms = access i mem
  case lookup l ms of
    Just (Closure env v m) -> evaluateSM m ((v,ObjRef i) : env)

updateObject :: Value -> Label -> MethodClosure -> MaybeT (State Mem) Value
updateObject (ObjRef i) l closure = do
    mem <- lift get
    let (before, o : after) = splitAt i mem
    lift . put $ before ++ [[if l == n then (n, closure) else (n,c) | (n,c) <- o]] ++ after
    return (ObjRef i) 

evaluateSM :: SigmaTerm -> Env -> MaybeT (State Mem) Value
evaluateSM (Lit v) e  = return (VInt v)
evaluateSM (Boolean v) e  = return (VBool v)
evaluateSM (SigmaVar v) e = 
  case lookup v e of
      Just a -> return a
      Nothing -> error ("Variable " ++ show v ++ " is undefined!")
evaluateSM (Binary op a b) e = do
  v1 <- evaluateSM a e
  v2 <- evaluateSM b e
  return (fromJust (binary op v1 v2))
evaluateSM (Unary op a) e = do
  v <- evaluateSM a e 
  return (fromJust (unary op v))
evaluateSM (If a b c) e = do
  VBool cond <- evaluateSM a e
  evaluateSM (if cond then b else c) e
evaluateSM (Let x a b) e = do
  v <- evaluateSM a e
  let e' = (x, v): e
  evaluateSM b e'
evaluateSM (Object o) e = do
  let mc = map (\(l,Method v t) -> (l,Closure e v t)) o
  newMemory mc
evaluateSM (Call a l) e = do
    ObjRef i <- evaluateSM a e
    callMethod (ObjRef i) l
evaluateSM (Clone a) e = do
  i <- evaluateSM a e 
  o <- readMemory i
  newMemory o
evaluateSM (Update a l (Method v m)) e = do
  ObjRef i <- evaluateSM a e
  updateObject (ObjRef i) l (Closure e v m)

executeM :: SigmaTerm -> SigmaTerm
executeM e = do
  let val = runState (runMaybeT (evaluateSM e [])) []
  case val of
    (Just (ObjRef i), ms) ->  revert (ms !! i)
    (Just (VInt i), ms) -> Lit i
    (Just (VBool i), ms) -> Boolean i
    _ -> error "Answer is not a value"

-- Question 4 over

execute :: SigmaTerm -> SigmaTerm
execute e = case evaluateS e [] [] of
  Just (ObjRef i, ms) ->  revert (ms !! i)
  Just (VInt i, ms) -> Lit i
  Just (VBool i, ms) -> Boolean i
  _ -> error "Answer is not a value"


revert :: Object -> SigmaTerm
revert [] = Object []
revert (x:xs) = let (l, cl) = x in
  case cl of
    Closure _ y s -> let mt = Method y s in
      case revert xs of
        Object ms -> Object ((l, mt):ms)
        _ -> error "Revert function fails"

-- | Tests for Target
--
-- >>> o1 = Object [(Label "l", Method (Var "x")  (Call (SigmaVar (Var "x")) (Label "l")))]
-- >>> o2 = Object [(Label "l", Method (Var "x")  (Call (SigmaVar (Var "x")) (Label "l")))]
-- >>> o3 = Object [(Label "l", Method (Var "y")  (Update (SigmaVar (Var "y")) (Label "l") (Method (Var "x") (SigmaVar (Var "x")))))]
-- >>> p1 = Call o1 (Label "l")
-- >>> p2 = Call o2 (Label "l")
-- >>> p3 = Call o3 (Label "l")
-- >>> a2 = execute p2
-- >>> a3 = execute p3
-- >>> a2 == o2
-- True
-- >>> a3 == o2
-- True
-- >>> vtrue = Object [(Label "if", Method (Var "x")  (Call (SigmaVar (Var "x")) (Label "then"))), (Label "then", Method (Var "x")  (Call (SigmaVar (Var "x")) (Label "then"))), (Label "else", Method (Var "x")  (Call (SigmaVar (Var "x")) (Label "else")))]
-- >>> vfalse = Object [(Label "if", Method (Var "x")  (Call (SigmaVar (Var "x")) (Label "else"))), (Label "then", Method (Var "x")  (Call (SigmaVar (Var "x")) (Label "then"))), (Label "else", Method (Var "x")  (Call (SigmaVar (Var "x")) (Label "else")))]
-- >>> cond b c d = Call (Update (Update b (Label "then") (Method (Var "_") c)) (Label "else") (Method (Var "_") d)) (Label "if")
-- >>> if1 = cond vtrue vfalse vtrue
-- >>> (execute if1) == vfalse
-- True
-- >>> if2 = cond vfalse vfalse vtrue
-- >>> (execute if2) == vtrue
-- True
