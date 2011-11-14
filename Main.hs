module Main where

import qualified GHC


type Var = String
type CoVar = String

data Lam = Lam [Var] [CoVar] Term
data Cont = Cont [Var] Term
data Term = Term [(Var, Lam)] [(CoVar, Cont)] Transfer
data Transfer = Call Arg [Arg] [CoArg] | Return CoArg [Arg]

data Arg = Var Var | Lit Literal | Update
data CoArg = CoVar CoVar | Halt

data Type = FunTy [Type] [[Type]] | TyCon TyCon | TyApp Type Type | ForAll TyVar Type

-- Maybe :: * -> *
-- Maybe a = <> -> {<>, <a>}

fromGHC :: GHC.Term -> Term
fromGHC e = fromGHC' M.empty e Halt

fromGHC' :: IdSupply -> [(Var, Lam)] -> [(CoVar, Cont)] -> Term -> CoArg -> Term
fromGHC' ids fs ks e r = case e of
    GHC.Var x -> Term fs ks (Return r [Var x]) -- D[a -> b] = <D[a]> -> {<D[b]>}
    GHC.Lit l -> Term fs ks (Return r [Lit l]) -- D[Int#] = Int#
    GHC.App e1 e2
      | GHC.isUnLiftedType (GHC.exprType e2)
      -> fromGHC' ids1 fs ((r0, Cont [x] (fromGHC ids2 [] [(r1, Cont [f] (Call (Var f) [Var x] [r]))] e1 r1)):ks) e2 r0
      | otherwise
      -> fromGHC' ids1 ((x, Lam [] [r1] (fromGHC' ids2 [] [] e2 r1)):fs) ((r0, Cont [f] (Term [] [] (Call (Var f) [Var x] [r]))):ks) e1 r0
      where (ids0, r0:r1:x:f:_) = stepIdSupply ids
            (ids1, ids2) = splitIdSupply ids0
