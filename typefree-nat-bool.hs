import Control.Monad (liftM, liftM3)
import Data.Maybe (isNothing)
data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
          deriving (Show)

-- define the subset value from Term
isNumVal :: Term -> Bool
isNumVal TmZero = True
isNumVal (TmSucc t) = isNumVal t
isNumVal _ = False

isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal t = isNumVal t

eval1 :: Term -> Maybe Term
eval1 (TmIf TmTrue t2 _) = Just t2
eval1 (TmIf TmFalse _ t3) = Just t3
eval1 (TmIf t1 t2 t3) = liftM3 TmIf (eval1 t1) (Just t2) (Just t3)
eval1 (TmPred TmZero) = Just TmZero
eval1 (TmPred (TmSucc nv)) | isNumVal nv = Just nv
eval1 (TmIsZero TmZero) = Just TmTrue
eval1 (TmIsZero (TmSucc nv)) | isNumVal nv = Just TmFalse
eval1 (TmSucc t) = liftM TmSucc (eval1 t)
eval1 (TmPred t) = liftM TmPred (eval1 t)
eval1 (TmIsZero t) = liftM TmIsZero (eval1 t)
eval1 _ = Nothing

eval :: Maybe Term -> Maybe Term
eval t =
    if isNothing t
       then Nothing
    else
        let val = t >>= eval1 in
            if isNothing val then
               if liftM isVal t == Just True
                  then t
               else Nothing
            else
               eval val

main :: IO ()
main = do
    print $ eval $ Just (TmIf TmTrue (TmSucc (TmPred (TmSucc TmZero))) TmZero)
    print $ eval $ Just (TmPred TmTrue)
