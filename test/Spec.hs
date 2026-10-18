{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Prelude hiding ((<>))
import Test.QuickCheck
import Data.List (intersperse)
import Control.Monad (liftM, liftM2)
import System.Exit (exitFailure)

import SimpleJSON
import Prettify

instance Arbitrary Doc where
    arbitrary = oneof
        [ return Empty
        , liftM Char arbitrary
        , liftM Text arbitrary
        , return Line
        , liftM2 Concat arbitrary arbitrary
        , liftM2 Union arbitrary arbitrary
        ]
instance Arbitrary JValue where
    arbitrary = sized genJValue

genJValue :: Int -> Gen JValue
genJValue 0 = oneof
    [ liftM JString arbitrary
    , liftM JNumber arbitrary
    , liftM JBool arbitrary
    , return JNull
    ]
genJValue n = oneof
    [ liftM JString arbitrary
    , liftM JNumber arbitrary
    , liftM JBool arbitrary
    , return JNull
    , liftM JArray (resize (n `div` 2) (listOf (genJValue (n `div` 2))))
    , liftM JObject (resize (n `div` 2) (listOf (liftM2 (,) arbitrary (genJValue (n `div` 2)))))
    ]

prop_empty_id :: Doc -> Bool
prop_empty_id x = (empty <> x == x) && (x <> empty == x)

prop_text :: String -> Bool
prop_text s = text s == if null s then Empty else Text s

prop_hcat :: [Doc] -> Bool
prop_hcat xs = hcat xs == glue xs
    where
        glue []     = empty
        glue (d:ds) = d <> glue ds

prop_punctuate' :: Doc -> [Doc] -> Bool
prop_punctuate' s xs = punctuate s xs == combine (intersperse s xs)
    where
        combine []                = []
        combine [x]               = [x]
        combine (x:Empty:ys)      = x : combine ys
        combine (Empty:y:ys)      = y : combine ys
        combine (x:y:ys)          = x `Concat` y : combine ys

-- Propriedade 5: Corretude dos accessors getString e getInt
prop_accessors :: String -> Int -> Bool
prop_accessors s n =
    getString (JString s) == Just s &&
    getInt (JNumber (fromIntegral n)) == Just n

-- Propriedade 6: Predicado isNull estrito para JNull
prop_isNull :: JValue -> Bool
prop_isNull JNull = isNull JNull
prop_isNull v     = not (isNull v)

-- Propriedade 7: Renderização compacta de texto puro preserva a string (Capítulo 9)
prop_compact_text :: String -> Bool
prop_compact_text s = compact (text s) == s

-- Propriedade 8: pretty de texto sem softline independe da largura especificada (Capítulo 9)
prop_pretty_width_text :: Int -> String -> Property
prop_pretty_width_text w s =
    w > 0 ==> pretty w (text s) == s

-- Disparo automático via Template Haskell
return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
    putStrLn "=== Executando Bateria de Testes QuickCheck ==="
    passed <- runTests
    if passed
        then putStrLn "Passou em todos os testes."
        else do
            putStrLn "Alguns testes falharam."
            exitFailure