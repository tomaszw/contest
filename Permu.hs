import qualified Data.Set as S

cycles :: (Ord a) => [a] -> [[a]]
cycles per =
    where
      ref = sort per
