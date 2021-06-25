module Source where

import Parser
import Declare
import Target
import Data.List
import Data.Maybe

type TClass = [(Var, Exp)]

substitute :: SigmaTerm -> SigmaTerm -> SigmaTerm -> SigmaTerm
substitute (SigmaVar v) (SigmaVar vr) val   = if v == vr then val else SigmaVar v 
substitute (Object o) vr val                = Object [(l, Method v (substitute s vr val)) |(l, Method v s) <- o]
substitute (Call a l) vr val                = Call (substitute a vr val) l
substitute (Update a l (Method v b)) vr val = Update (substitute a vr val) l (Method v (substitute b vr val))
substitute (Let v a b) vr val               = Let v (substitute a vr val) (substitute b vr val)
substitute (Clone a) vr val                 = Clone (substitute a vr val)
substitute (Lit x) vr val                   = Lit x
substitute (Boolean x) vr val               = Boolean x
substitute (Binary op a b) vr val           = Binary op (substitute a vr val) (substitute b vr val)
substitute (Unary op a) vr val              = Unary op (substitute a vr val) 
substitute (If a b c) vr val                = If (substitute a vr val) (substitute b vr val) (substitute c vr val)

translate :: Exp -> TClass -> SigmaTerm
translate (SNew a) tc  = Call (translate (classGen a tc) tc) (Label "new")
translate (Lam v b) tc = 
    let val = substitute (translate b tc) (SigmaVar v) (Call (SigmaVar v) (Label "arg")) 
        in Object [( Label "arg", Method v (Call (SigmaVar v) (Label "arg"))), (Label "val", Method v val)]
translate (Apply b a) tc = 
    let f = Clone (translate b tc)
        y = translate a tc 
    in Call (Update f (Label "arg") (Method (Var "this") y)) (Label "val")
translate (SLit x) tc                    = Lit x
translate (SBool x) tc                   = Boolean x 
translate (SUnary op a) tc               = Unary op (translate a tc)
translate (SBin op a b) tc               = Binary op (translate a tc) (translate b tc)
translate (SIf a b c) tc                 = If (translate a tc) (translate b tc) (translate c tc)
translate (SVar v) tc              = 
    let super = Var "super"
    in if v == super 
        then case lookup super tc of
            Just (SVar v')    -> SigmaVar v'
            Just (Class pm e) -> translate (Class pm e) tc 
            _                 -> error "variable super not found"
        else SigmaVar v
translate (SClone a) tc                  = Clone (translate a tc)
translate (SObject o) tc                 = Object [(l, Method v (translate s tc)) |(l, SMethod v s) <- o]
translate (SCall a l) tc                 = Call (translate a tc) l
translate (SUpdate a l (SMethod v b)) tc = Update (translate a tc) l (Method v (translate b tc))
translate (SLet v (Class pm e) b) tc     = 
    let tc'  = (v, Class pm e): tc
        tc'' = 
            case e of 
                SVar v       -> (Var "super", SVar v): tc'
                Class pm' e' -> (Var "super", Class pm' e'): tc'
                Top          -> tc'
                _            -> error "Class is undefined"
    in Let v (translate (Class pm e) tc'') (translate b tc'')
translate (SLet v a b) tc                = Let v (translate a tc) (translate b tc)
translate (Class xr a) tc                = translate (classGen (Class xr a) tc) tc
-- translate Top tc                         = _


newConstructor pm = [(Label "new", SMethod (Var "this") (SObject (constructor pm (SVar (Var "this")))))]
    where constructor xs ref = [(l, SMethod (Var v) (Apply (SCall ref l) (SVar (Var v)))) |(l, SMethod (Var v) exp) <- xs]

classGen :: Exp -> TClass -> Exp
classGen (Class pm Top) tc = 
    let srm = [(l, SMethod v (Lam v exp))| (l, SMethod v exp) <- pm] 
    in SObject (srm ++ newConstructor pm)
classGen (Class pm (SVar v )) tc = 
    case lookup v tc of
        Just (Class pm' e) -> Class (pm ++ pm') Top
        _                  -> error "Couldn't find class definition"
classGen exp _   = exp