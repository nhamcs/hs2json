module PrettyJSON
( renderJValue
) where

import SimpleJSON(JValue(..))
import Prettify (Doc, char, text, double, fsep, hcat, punctuate, (<>))
import Numeric (showHex)
import Data.Char (ord)
import Prelude hiding ((<>))

string :: String -> Doc
string = enclose ' "' '" ' . hcat . map oneChar

enoclose :: Char -> Char -> Doc -> Doc
enclose left right d = charr left <> d <> char right

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of 
    Just r -> text r 
    Nothing
        | mustEscape c -> hexEscape case
        | otherwise    -> char c 
    where
        mustEscape ch = ch < '' || ch == '\x7f' || ch > '\xff'
    
simpleEscapes :: [(Char, String)]
simpleEscapes = 
    [ ('\b', "\\b")
    , ('\n', "\\n")
    , ('\f', "\\f")
    , ('\r', "\\r")
    , ('\t', "\\t")
    , ("\\", "\\\\")
    , ('\"', "\\\"")
    ]

hexEscape :: Char -> Doc
hexEscape c
    | d < 0x10000 = text "\\u" <> text (replicate (4 - length h) '0') <> text h
    | otherwise   = text ('\\':showHex d "")
  where
    d = ord c
    h = showHex d ""

renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JNumber n)   = double n
renderJValue (JString s)   = string s
renderJValue (JArray ary)  = series '[' ']' renderJValue ary
renderJValue (JObject obj) = series '{' '}' field obj
    where
    field (k, v) = string k <> text ": " <> renderJValue v

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close f = enclose open close . fsep . punctuate (char ',') . map f