{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main where

import qualified GHC
import GHC (IdSupply, Literal, TyVar, TyCon, Coercion,
            stepIdSupply, splitIdSupply,
            tcDataCons, dcArity, dcTyCon,
            mkSymCo, mkTransCo)

import qualified Data.Map as M


type Var = String
type CoVar = String

data Lam = Lam [Var] [CoVar] Term
data Cont = Cont [Var] Term
data Term = Term [(Var, Lam)] [(CoVar, Cont)] Transfer
data Transfer = Call Arg [Arg] [CoArg] | Return CoArg [Arg]

data Castable a = Uncast a | Cast a Coercion

type Arg = Castable Arg'
data Arg' = Var Var | Lit Literal | Update
type CoArg = Castable CoArg'
data CoArg' = CoVar CoVar | Halt | Unreachable

data ArgType = Type Type | ForAll TyVar
data Boxness = Computy -- ^ Computational arrow: must enter code
             | Boxy    -- ^ Non-computational arrow: just pattern matcihng
data Type = FunTy [ArgType] Boxness [[ArgType]] | TyCon TyCon | TyApp Type Type

castCastable :: Castable a -> Coercion -> Castable a
castCastable (Uncast x)  co' = Cast x co'
castCastable (Cast x co) co' = Cast x (co `mkTransCo` co')

-- Maybe :: * -> *
-- Maybe a = <> -> {<>, <a>}

fromGHC :: GHC.Term -> Term
fromGHC e = build $ fromGHC' e (Uncast Halt)

fromGHC' :: GHC.Term -> CoArg -> TermBuilder
fromGHC' e {- :: t -} s {- :: t -} = case e of
    GHC.Var x
      | GHC.isUnLiftedType (GHC.varType x)
      -> transfer (Return s [Uncast (Var x)])
      | otherwise
      -> transfer (Call (Uncast (Var x)) [] [s])
    GHC.Lit l
      -> transfer (Return s [Uncast (Lit l)])
    GHC.App e1 e2
      -> var $ \x -> var $ \f -> coVar $ \o ->
            fromGHCBind x e2 $
                cont o ([f], transfer $ Call (Uncast (Var f)) [Uncast (Var x)] [s]) $
                    fromGHC' e1 (Uncast (CoVar o))
    GHC.Lam x e
      -> var $ \f -> coVar $ \o ->
            lam f ([x], [o], fromGHC' e (Uncast (CoVar o))) $ transfer (Return s [Uncast (Var f)])
    GHC.LetNonRec x e1 e2
      -> fromGHCBind x e1 $
            fromGHC' e2 s
    GHC.LetRec xes e
      -> foldr (uncurry fromGHCBind) (fromGHC' e s) xes
    GHC.Case e x alts
      -> var $ \y -> coVar $ \o0 -> coVar $ \o1 ->
            cont o0 ([y], lam x ([], [o1], transfer $ Return (Uncast (CoVar o1)) [Uncast (Var y)]) $
                                fromGHCAlts y alts s) $
                fromGHC' e (Uncast (CoVar o0))
    GHC.Cast e {- :: t' -} co {- :: t' ~ t -}
      -> fromGHC' e (s `castCastable` mkSymCo co)
    -- FIXME: explicit type application/abstraction

fromGHCAlts :: Var -> [(GHC.AltCon, [GHC.Var], GHC.Term)] -> CoArg -> TermBuilder
fromGHCAlts y alts s = go Nothing Nothing alts
  where
    go mb_def mb_ei_sort []
      = (maybe ($ Uncast Unreachable) (\e f -> coVar $ \o_def -> cont o_def ([], fromGHC' e s) $ f (Uncast (CoVar o_def))) mb_def) $ \s_def ->
          case mb_ei_sort of
            Nothing      -> transfer (Return s_def [])
            Just (Left lit_alts) -> error "fromGHCAlts: FIXME LitAlt" lit_alts
            Just (Right (dc_tc, dc_alts))
              | let dcs = expectJust "fromGHCAlts: must have sibling datacons" (tcDataCons dc_tc)
              -> compose [\k -> coVar $ \o -> k o $ case M.lookup dc dc_alts of
                                    Nothing            -> \mk -> vars (dcArity dc) $ \xs -> cont o (xs, transfer (Return s_def [])) mk
                                    Just (xs_dc, e_dc) -> cont o (xs_dc, fromGHC' e_dc s)
                         | dc <- dcs] $ \os ->
                    transfer $ Call (Uncast (Var y)) [] (map (Uncast . CoVar) os)
                                 
    go Nothing mb_ei_sort ((GHC.Default, [], e):alts)
      = go (Just e) mb_ei_sort alts
    go mb_def mb_ei_sort ((GHC.LitAlt l, [], e):alts)
      | Just lit_alts <- case mb_ei_sort of
          Nothing              -> Just []
          Just (Left lit_alts) -> Just lit_alts
          _                    -> Nothing
      = go mb_def (Just (Left ((l, e):lit_alts))) alts
    go mb_def mb_ei_sort ((GHC.DataAlt dc, xs, e):alts)
      | Just (dc_tc, dc_alts) <- case mb_ei_sort of
          Nothing                                     -> Just (tc,    M.empty)
          Just (Right (dc_tc, dc_alts)) | dc_tc == tc -> Just (dc_tc, dc_alts)
          _                                           -> Nothing
      = go mb_def (Just (Right (dc_tc, M.insert dc (xs, e) dc_alts))) alts
      where tc = dcTyCon dc
    go _ _ _ = error "fromGHCAlts: impossible"

compose :: [(CoVar -> (TermBuilder -> TermBuilder) -> TermBuilder) -> TermBuilder]
        -> ([CoVar] -> TermBuilder) -> TermBuilder
compose []     k' = k' []
compose (k:ks) k' = k $ \o mk_mk -> mk_mk $ compose ks $ \os -> k' (o:os)

{-
compose :: [DataCon]
        -> ([CoVar] -> TermBuilder -> TermBuilder)
        ->  [CoVar] -> TermBuilder -> TermBuilder
compose 
-}

fromGHCBind :: Var -> GHC.Term -> TermBuilder -> TermBuilder
fromGHCBind x e mk
  | GHC.isUnLiftedType (GHC.exprType e)
  = coVar $ \o ->
        cont o ([x], mk) $
            fromGHC' e (Uncast (CoVar o))
  | otherwise
  = var $ \x' -> coVar $ \o0 -> coVar $ \o1 ->
        lam x ([], [o0], cont o1 ([x'], transfer (Call (Uncast Update) [Uncast (Var x), Uncast (Var x')] [Uncast (CoVar o0)])) $
                            fromGHC' e (Uncast (CoVar o1)))
              mk

type TermBuilder = IdSupply -> [(Var, Lam)] -> [(CoVar, Cont)] -> Term

transfer :: Transfer -> TermBuilder
transfer t _ids fs ks = Term fs ks t

build :: TermBuilder -> Term
build mk = mk (error "FIXME") [] []

var :: (Var   -> TermBuilder) -> TermBuilder
var mk ids = mk x ids'
  where (ids', x) = stepIdSupply ids

vars :: Int -> ([Var] -> TermBuilder) -> TermBuilder
vars 0 mk = mk []
vars n mk = var (\x -> vars (n - 1) (\xs -> mk (x:xs)))

coVar :: (CoVar -> TermBuilder) -> TermBuilder
coVar = var

lam :: Var -> ([Var], [CoVar], TermBuilder) -> TermBuilder -> TermBuilder
lam x (xs, ks, mk1) mk2 = branch mk1 $ \e -> lam' x (Lam xs ks e) mk2

cont :: CoVar -> ([Var], TermBuilder) -> TermBuilder -> TermBuilder
cont o (xs, mk1) mk2 = branch mk1 $ \e -> cont' o (Cont xs e) mk2

lam' :: Var -> Lam -> TermBuilder -> TermBuilder
lam' x f mk ids fs ks = mk ids ((x, f):fs) ks

cont' :: CoVar -> Cont -> TermBuilder -> TermBuilder
cont' o k mk ids fs ks = mk ids fs ((o, k):ks)

branch :: TermBuilder -> (Term -> TermBuilder) -> TermBuilder
branch mk1 mk2 ids = mk2 (mk1 ids0 [] []) ids1
  where (ids0, ids1) = splitIdSupply ids


expectJust :: String -> Maybe a -> a
expectJust msg Nothing  = error msg
expectJust _   (Just x) = x