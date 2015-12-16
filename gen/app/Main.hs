module Main where

import Data.Csv
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import Data.Either hiding (rights)
import Data.Text
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Facebook.Gen.Csv
import Facebook.Gen.Environment
import Facebook.Gen.Types
import Facebook.Gen.CodeGenStr

csvFiles = V.fromList ["data/adaccount.csv"] --, "data/adcampaign.csv", "data/insights.csv", "data/adset.csv"] -- TODO

main :: IO ()
main = do
    inps <- V.mapM BS.readFile csvFiles
    let csvs = V.map (decode HasHeader) inps :: V.Vector (Either String (V.Vector CsvLine))
    --print $ V.head csvs
    let (Right e@(Env m)) = buildEnv $ rights csvs
    --let out = (Map.!) ((Map.!) (genFiles e) (Entity "AdAccount")) $ InteractionMode "Reading"
    --writeFile "Test.hs" $ unpack $ V.foldl' append "" out
    --putStrLn $ Map.showTree m
    let (fp, out) = Prelude.head $ genFiles e
    writeFile "Test.hs" $ unpack out
    print $ genFiles e
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
