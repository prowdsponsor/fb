module Main where

import Facebook.Gen.Csv
import Facebook.Gen.Environment
import Data.Csv
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import Data.Either hiding (rights)
import Data.Text
import Data.Map.Strict

csvFiles = V.fromList ["data/adaccount.csv", "data/adcampaign.csv"] -- TODO

main :: IO ()
main = do
    inps <- V.mapM BS.readFile csvFiles
    let csvs = V.map (decode HasHeader) inps :: V.Vector (Either String (V.Vector CsvLine))
    --print $ V.head csvs
    let (Right (Env m)) = buildEnv $ rights csvs
    putStrLn $ showTree m
    --let env = buildEnv csvs
    --print $ V.head csvs

    -- parse CSV, build Env, type check/unify, gen code, save files, be awesome
    --
    --

isAllRight :: V.Vector (Either String a) -> Bool
isAllRight xs = V.length (V.filter isRight xs) == V.length xs

rights :: V.Vector (Either String a) -> V.Vector a
rights xs | isAllRight xs = V.map fct xs
          | otherwise = error "Found Left in csvs!"
    where
        fct (Right x) = x
        fct (Left _) = error "This is impossible!"
