module Engine.Parser (parseCommand) where

import Engine.Types
import Data.Char (toLower)

-- Función principal que parsea comandos en español
parseCommand :: String -> Maybe Command
parseCommand input = 
    let palabras = words (map toLower input)
    in case palabras of
        -- Comando para moverse
        ("ir":dir:[]) -> parseDirection dir >>= \d -> Just (Ir d)
        
        -- Comando para mirar la sala
        ["mirar"] -> Just Mirar
        
        -- Comando para tomar objetos (acepta "tomar" y "coger")
        ("tomar":resto) -> Just (Tomar (unwords resto))
        ("coger":resto) -> Just (Tomar (unwords resto))
        
        -- Comando para ver el inventario
        ["inventario"] -> Just Inventario
        ["inv"] -> Just Inventario
        
        -- Comando para salir del juego
        ["salir"] -> Just Salir
        
        -- Comando no reconocido
        _ -> Nothing

-- Parsea una dirección en español
parseDirection :: String -> Maybe Direction
parseDirection dir = case dir of
    "norte" -> Just Norte
    "sur"   -> Just Sur
    "este"  -> Just Este
    "oeste" -> Just Oeste
    _       -> Nothing