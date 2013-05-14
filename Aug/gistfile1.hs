import Control.Monad.Logic
import Data.List

foo :: Logic Bool
foo = do [c2,c3,c4,c5,c6,c7,cJ,cA] <- sequence . replicate 8 $ return True `mplus` return False
         guard $ c3 `thinks` sane cA
         guard $ c4 `thinks` not (not (sane c2) && not (sane c3))
         guard $ c5 `thinks` sane cA == sane c4
         guard $ c6 `thinks` sane cA && sane c2
         guard $ c7 `thinks` not (sane c5)
         guard $ cJ `thinks` not (not (sane c6) && not (sane c7))
         return cJ
    where a `thinks` b = a == b
          sane a = a


answer = nub $ observeAll foo
