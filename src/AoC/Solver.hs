module AoC.Solver where

-- This approach was based on an idea by https://github.com/mstksg.

data a :->: b = MkSol
    { sParse :: String -> Maybe a
    , sSolve :: a -> Maybe b
    , sPrint :: b -> String
    }

runSolution :: (a :->: b) -> String -> Maybe String
runSolution (MkSol sParse sSolve sPrint) raw = do
    input <- sParse raw
    solution <- sSolve input
    pure $ sPrint solution
