{-# LANGUAGE TupleSections, RankNTypes, FlexibleInstances, DeriveFunctor #-}
module Language.Haskell.TH.Plate.Folds where
import Language.Haskell.TH.Plate.Types
import Data.Generics.Multiplate
import Language.Haskell.TH.Syntax
import Control.Applicative
import Data.Traversable
import Data.Functor.Constant
import Data.Functor.Identity
import Data.List
import Data.Monoid
import Control.Monad

newtype MQ a = MQ { unMQ :: a}
    deriving (Functor)
    
instance (Monad m, Monoid a) => Monoid (MQ (m a)) where
    mempty  = MQ $ return mempty
    mappend (MQ x) (MQ y) = MQ $ liftM2 mappend x y

childTypes :: Info -> MQ (Q [Type])
childTypes n = do 
      let go (ConT x ) = Constant $ MQ $ (unMQ . childTypes) =<< reify x
          go (VarT _ ) = Constant $ MQ $ return [] 
          go (ArrowT ) = Constant $ MQ $ return []
          go (ListT	 ) = Constant $ MQ $ return []
          go x         = Constant $ MQ $ return [x]   
          
 
      fmap nub <$> foldFor infoPL (preorderFold purePlate { typPL = go } ) n

--Perhaps an alternate way...or correct have tested the code yet.
--      roconnor: jfischoff: \proj plate -> foldFor proj (applyNaturalTransform eta plate)  where eta (Compose x) = Constant (getConstant `liftM` x)
--      [5:14pm] roconnor: jfischoff: I think that has the type you requested
--
--