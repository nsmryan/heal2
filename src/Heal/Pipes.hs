{-# LANGUAGE ScopedTypeVariables #-}
module Heal.Pipes 
  (
  ) where

--import Pipes
--import Pipes.Core
--import Pipes.Prelude
--
--import Data.Sequence as S
--
--import System.Random.MWC.Monad as MWC
--import System.Random.MWC
--
--import Control.Monad.Primitive.Class



{- Mutation -}
{-
pointMutationLociStd ::
  (MonadPrim m, Variate a) =>
  Double -> S.Seq a -> Rand m (S.Seq a)
pointMutationLociStd prob p = pointMutationLoci prob (const MWC.uniform) p

pointMutationLoci ::
  (MonadPrim m) =>
  Double -> (a -> Rand m a) -> S.Seq a -> Rand m (S.Seq a)
pointMutationLoci prob f p = runEffect $ passDown p >-> passUp 
  
  
passDown :: (Monad m) => S.Seq a -> Proxy a X a X m r
passDown p = do
  a <- request $ S.index p 0
  return $ S.update 0 a p

passUp :: (Monad m) => Proxy a X a X m r
passUp = do
  a <- await
  respond a
-}

