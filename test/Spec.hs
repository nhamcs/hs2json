{-# LANGUAGE TemplateHaskell #-}
module Main where

import Prelude hiding ((<>))
import Test.QuickCheck
import Data.List (intersperse)
import Control.Monad (liftM, liftM2)
import System.Exit (exitFailure)

import SimpleJSON
import Prettify
import PrettyJSON

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