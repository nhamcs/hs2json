module Main (main) where

import SimpleJSON
import PrettyJSON
import Prettify (pretty, compact)

main :: IO ()
main = do
    let payload = JObject
    [("disciplina", JString "Compiladores / Programacao Funcional")
    , ("projeto", JString "hs2json")
    , ("versao", JNumber 1.0)
    , ("integrantes", JArray [JString "Carla Sofia", JString "Marcos Davi"])
    , ("status_aprovado", JBool True)
    , ("detalhes", JObject [("nota_alvo", JNumber 10.0), ("observacao", JNull)])
    ]
    putStrLn "=== JSON Compacto ==="
    putStrLn (compact (renderJValue payload))
    putStrLn "\n=== JSON Formatado (Pretty Printing - Largura 40) ==="
    putStrLn (pretty 40 (renderJValue payload))