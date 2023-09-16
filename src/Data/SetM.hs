module Data.SetM where

import Control.Monad.Freer
import Data.Set ( Set )
import qualified Data.Set as S

type SetM = Freer Set

runSetM :: Ord a => SetM a -> Set a
runSetM (Freer xs next) = let ss = S.foldr (\ x acc -> next x : acc) [] xs
                          in  S.unions (runSetM <$> ss)
runSetM (Purer x)       = S.singleton x
