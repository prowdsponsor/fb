module Main where

import Data.Csv
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import Data.Either hiding (rights)
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as Map
import Data.Monoid

import Facebook.Gen.Csv
import Facebook.Gen.Environment
import Facebook.Gen.Types
import Facebook.Gen.CodeGenStr

csvFiles = V.fromList ["data/adaccount.csv", "data/adcampaign.csv", "data/adset.csv"] --, "data/insights.csv", ] -- TODO

main :: IO ()
main = do
    inps <- V.mapM BS.readFile csvFiles
    let csvs = V.map (decode HasHeader) inps :: V.Vector (Either String (V.Vector CsvLine))
    --print $ rights csvs
    let (Right e@(Env m)) = buildEnv $ rights csvs
    --let out = (Map.!) ((Map.!) (genFiles e) (Entity "AdAccount")) $ InteractionMode "Reading"
    --writeFile "Test.hs" $ unpack $ V.foldl' append "" out
    --putStrLn $ Map.showTree m
    let (fp, out) = V.head $ genFiles e
    writeFile "Test.hs" $ unpack out
    --print $ V.head $ V.tail $ genFiles e
    saveFiles $ genFiles e
    -- parse CSV, build Env, type check/unify, gen code, save files, be awesome

saveFiles :: V.Vector (FilePath, Text) -> IO ()
saveFiles = V.mapM_ save
    where
        save (path, data_) = T.writeFile ("../src/" <> path) data_

isAllRight :: V.Vector (Either String a) -> Bool
isAllRight xs = V.length (V.filter isRight xs) == V.length xs

rights :: Show a => V.Vector (Either String a) -> V.Vector a
rights xs | isAllRight xs = V.map fct xs
          | otherwise = error $ show xs -- "Found Left in csvs!"
    where
        fct (Right x) = x
        fct (Left _) = error "This is impossible!"
