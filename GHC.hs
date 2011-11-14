module GHC where

type Var = String

type Literal = Int

data DataCon = DC { dcName :: String, dcTyCon :: TyCon }

data Term = Var Var | Lit Literal
          | App Term Term | Lam Var Term
          | TyApp Term Type | TyLam TyVar Term
          | LetNonRec Var Term Term
          | LetRec [(Var, Term)] Term
          | Case Term Var [(AltCon, [Var], Term)]
          | Cast Term Coercion

type Coercion = () -- Can't be arsed

data AltCon = LitAlt Literal | DataAlt DataCon | Default

type TyVar = String

data TyCon = TC { tcName :: String, tcDataCons :: Maybe [DataCon] }

data Type = TyVar TyVar | TyCon TyCon | TyApp Type Type | ForAll TyVar Type
