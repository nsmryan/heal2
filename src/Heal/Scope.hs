{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE BangPatterns #-}
module Heal.Scope
( Scope(..)
, composeScopes
, modify
, modifyM
, replace
, bitsIndexer
, seqIndexer
, listIndexer
, vecIndexer
, VecState(..)
, SeqState(..)
, vecPair
) where

import Debug.Trace

import System.Random.MWC.Monad as MWC
import System.Random.MWC
import System.Random.MWC.CondensedTable.Monad as MWC
import System.Random.MWC.Distributions.Monad as MWC

import Control.Lens.Iso
import Control.Lens.Type
import Control.Arrow

import qualified Data.Sequence as S
import Data.Sequence ((><))
import Data.Bits
import Data.DList as DL
import Data.DList (DList)
import Data.Monoid
import qualified Data.Vector as V


{- Scope Definition -}
--data Scope s t n a =
--  Scope { transport :: ReifiedIso' s t
--        , focus :: ReifiedLens' t a
--        , action :: n -> t -> t
--        }
--data Scope s t a =
--  forall t. Scope 
--            { transport :: ReifiedIso' s t
--            , focus :: ReifiedLens' t a
--            , action :: n -> t -> t
--            }

data Scope s t n a =
  Scope { into  :: s -> t
        , outOf :: t -> s
        , move  :: n -> t -> t
        , get   :: t -> a
        , set   :: a -> t -> t
        }


replace :: Scope s t n a -> a -> t -> t
replace scope a t = modify scope (const a) t

modify :: Scope s t n a -> (a -> a) -> t -> t
modify scope f t = set scope (f $ get scope t) t

modifyM :: (Monad m) => Scope s t n a -> (a -> m a) -> t -> m t
modifyM scope f t = 
  do ma <- f $ get scope t
     return $ set scope ma t

composeScopes ::
  Scope s t n a ->
  Scope a t' n' a' ->
  Scope s (t, t') (Maybe n, n') a'
composeScopes outerScope innerScope =
  Scope -- into
        (\s -> let t = into outerScope s in (t, into innerScope (get outerScope t)))
        -- outOf
        (\(t, t') -> outOf outerScope $ set outerScope (outOf innerScope t') t)
        -- move
        (\(maybeN, n') (t, t') ->
          case maybeN of
            Nothing ->
              (t, move innerScope n' t')

            Just n ->
              let newOuter = move outerScope n  $ set outerScope (outOf innerScope t') t
                  newInner = move innerScope n' $ into innerScope $ get outerScope newOuter
              in (newOuter, newInner))
        -- get
        (\(t, t') -> get innerScope t')
        -- set
        (\a' (t, t') -> (t, set innerScope a' t'))

              
--composeScopes ::
--  (m'' -> (m, m')) ->
--  (s -> Scope s t m a) ->
--  (a -> Scope a t' m' a') ->
--  s -> Scope s (t, t') m'' a'
--composeScopes eachMove mkOuter mkInner start =
--  let outerScope = mkOuter start
--      innerScope = mkInner $ ixGet outerScope
--   in Scope (internalState outerScope, internalState innerScope)
--            (\m'' (t, _) ->
--              let (m, m') = eachMove m''
--                  movedOuter = move outerScope m t
--                  newInner = internalState $ mkInner $ getItem outerScope movedOuter
--                  movedInner = move innerScope m' newInner
--               in (movedOuter, movedInner))
--            (\(t, t') -> extractResult outerScope t)
--            (\(t, t') -> getItem innerScope t')
--            (\a (t, t') -> (t, setItem innerScope a t'))

{- Sequence Scope -}
data SeqState a = SeqState (S.Seq a) a (S.Seq a) deriving (Show, Eq)

moveSeq :: Int -> SeqState a -> SeqState a
moveSeq n t@(SeqState front a back) =
  -- move by 0 does nothing
  if n == 0
    then t
    -- enough left in back to move into it
    else if S.length back >= n
         then
           SeqState (front >< S.singleton a >< S.take (n-1) back)
                    (S.index back (n-1))
                    (S.drop n back)
         -- back is empty
         else if S.length back == 0
              then t
              -- not enough locations- go to end of back
              else let nRest = S.length back - 1
                   in  SeqState (front >< (S.singleton a) >< (S.take nRest back))
                                (S.index back nRest)
                                S.empty

-- This indexer can only move forwards
seqIndexer :: Scope (S.Seq a) (SeqState a) Int a
seqIndexer =
        -- into
  Scope (\as -> SeqState S.empty (S.index as 0) (S.drop 1 as))
        -- outOf
        (\(SeqState as a as') -> as >< S.singleton a >< as')
        -- move[
        moveSeq
        -- get
        (\(SeqState _ a _) -> a)
        -- set
        (\a (SeqState as _ as') -> SeqState as a as')

data SeqCross a = SeqCross (S.Seq a) (S.Seq a) [S.Seq a] [S.Seq a]

seqCross :: Scope (S.Seq a, S.Seq a) (SeqCross a) Int ()
seqCross =
        -- into
  Scope (\(as, as') -> SeqCross as as' [] [])
        -- outOf
        (\(SeqCross as as' pairs pairs') ->
          ((mconcat $ reverse pairs) <> as, (mconcat $ reverse pairs') <> as'))
        -- move
        (\n (SeqCross as as' pairs pairs') -> 
          let (first,  rest)  = S.splitAt n as
              (first', rest') = S.splitAt n as'
          in SeqCross rest rest' (first : pairs) (first' : pairs'))
        -- get
        (\_ -> ())
        -- set
        (\() t -> t)

{- Bits Scope -}
-- TODO consider taking the number of bits as an input
bitsIndexer :: (FiniteBits b) => Scope b (b, Int) Int Bool
bitsIndexer =
  Scope (\b -> (b, 0))
        fst
        (\n (b, n') -> (b, max 0 (min (finiteBitSize b - 1) (n + n'))))
        (\(b, n) -> testBit b n)
        (\bool (b, n) ->
          if bool
            then (setBit   b n, n)
            else (clearBit b n, n))

{- List Scope -}
moveList :: Int -> (DList a, a, [a]) -> (DList a, a, [a])
moveList n t@(dlist, a, as) =
  case n of
    0 -> t
    otherwise ->
      case length as >= n of
        True ->
          (dlist <> (DL.singleton a) <> (DL.fromList . take (n-1) $ as)  
          , as !! (n-1)
          , drop n as
          )

        False ->
          case length as == 0 of
            True -> 
              t
           
            False ->
              let nRest = length as - 1
              in ( dlist <> (DL.singleton a) <> (DL.fromList . take nRest $ as)
                 , as !! nRest
                 , []
                 )
  

listIndexer :: Scope [a] (DList a, a, [a]) Int a
listIndexer =
  Scope (\as -> (DL.empty, as !! 0, drop 1 as))
        (\(dlist, a, as) -> DL.toList (dlist <> (DL.singleton a) <> DL.fromList as))
        moveList
        (\(_, a, _) -> a)
        (\a (dlist, _, as) -> (dlist, a, as))

{- Vector Scope -}
data VecState a = VecState !(V.Vector a) !Int [(Int, a)]
  deriving (Show, Eq)

vecIndexer :: Scope (V.Vector a) (VecState a) Int a
vecIndexer =
  Scope -- into
        (\v -> (VecState v 0 []))
        -- outOf
        (\(VecState v _ changes) ->
          v V.// changes)
        -- move
        (\n' (VecState v n changes) ->
          (VecState v (max 0 (min (V.length v) (n + n'))) changes))
        -- get
        (\(VecState v n changes) -> 
          case lookup n changes of
            Nothing ->
              v V.! n

            Just val ->
              val
        )
        -- set 
        (\a (VecState v n changes) -> 
          case changes of
            [] ->
              (VecState v n [(n, a)])

            ((ix, val):rest) ->
              (VecState v n ((n, a) : if ix == n then rest else changes))
        )

vecPair :: (Show a) => Scope (V.Vector a) (VecState (a, a)) Int (a, a)
vecPair = vecIndexer
  {
    into = (\v -> let len = V.length v
                      halfIx = len `div` 2
                      paired = V.zip (V.slice 0 halfIx v) (V.slice halfIx halfIx v)
                   in VecState paired 0 [])
  , outOf = (\(VecState v _ changes) -> let v' = v V.// changes
                                        in V.map fst v' <> V.map snd v')

  }

