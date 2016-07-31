module Lib
    ( someFunc
    ) where
import FRP.Elerea.Param

someFunc :: IO ()
someFunc = putStrLn "someFunc"

foo :: Signal Int
foo = undefined
