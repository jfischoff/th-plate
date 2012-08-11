{-# LANGUAGE TupleSections, RankNTypes #-}
module Language.Haskell.TH.Plate.Folds where
import Language.Haskell.TH.Plate.Types
import Data.Generics.Multiplate
import Language.Haskell.TH.Syntax
import Control.Applicative
import Data.Traversable
import Data.Functor.Constant
import Data.Functor.Identity
import Data.List

childTypes :: Name -> Q [Type]
childTypes n = do 
      let go (ConT n	       ) = Constant <$> childTypes n
          go (VarT n	       ) = return $ Constant [] 
          go (ArrowT	       ) = return $ Constant []
          go (ListT	           ) = return $ Constant []
          go x                   = return $ Constant [x]   
          
      return $ 
          fmap nub $ foldFor infoPL (preorderFold purePlate { typPL = go } ) n


