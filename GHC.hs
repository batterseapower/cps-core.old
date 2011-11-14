module GHC where

import Data.Function


type IdSupply = ()

stepIdSupply :: IdSupply -> (IdSupply, Var)
stepIdSupply = error "FIXME: stepIdSupply"

splitIdSupply :: IdSupply -> (IdSupply, IdSupply)
splitIdSupply = error "FIXME: splitIdSupply"

type Var = String

type Literal = Int

data DataCon = DC { dcName :: String, dcTyCon :: TyCon, dcArity :: Int }

instance Eq DataCon where
    (==) = (==) `on` dcName

instance Ord DataCon where
    compare = compare `on` dcName

data Term = Var Var | Lit Literal
          | App Term Term | Lam Var Term
          | TyApp Term Type | TyLam TyVar Term
          | LetNonRec Var Term Term
          | LetRec [(Var, Term)] Term
          | Case Term Var [(AltCon, [Var], Term)]
          | Cast Term Coercion

type Coercion = (Type, Type) -- Can't be arsed

data AltCon = LitAlt Literal | DataAlt DataCon | Default

type TyVar = String

data TyCon = TC { tcName :: String, tcDataCons :: Maybe [DataCon] }

instance Eq TyCon where
    (==) = (==) `on` tcName

data Type = TyVarTy TyVar | TyConTy TyCon | AppTy Type Type | ForAllTy TyVar Type

varType :: Var -> Type
varType = error "FIXME: varType"

isUnLiftedType :: Type -> Bool
isUnLiftedType = error "FIXME: isUnLiftedType"

exprType :: Term -> Type
exprType = error "FIXME: exprType"

mkTransCo :: Coercion -> Coercion -> Coercion
mkTransCo (ty1, _) (_, ty2) = (ty1, ty2)

mkSymCo :: Coercion -> Coercion
mkSymCo (ty1, ty2) = (ty2, ty1)
