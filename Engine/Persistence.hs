module Engine.Persistence (loadWorldData) where

import Engine.Types
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (isPrefixOf)
import Data.Char (toLower)

-- Carga los datos del mundo desde un archivo
loadWorldData :: FilePath -> IO (Either String (Map RoomName Room, Map ItemName Item))
loadWorldData path = do
    contenido <- readFile path
    let bloques = separarBloques contenido
    return $ parsearBloques bloques

-- Separa el archivo en bloques usando "---" como delimitador
separarBloques :: String -> [String]
separarBloques contenido = 
    let lineas = lines contenido
        bloques = split "---" lineas
    in filter (not . null . unwords) bloques
  where
    split _ [] = [[]]
    split delim (l:ls)
        | delim `isPrefixOf` l = [] : split delim ls
        | otherwise = case split delim ls of
                        (x:xs) -> (l:x):xs
                        [] -> [[l]]

-- Parsea todos los bloques y construye los mapas
parsearBloques :: [String] -> Either String (Map RoomName Room, Map ItemName Item)
parsearBloques bloques = do
    let parseos = map parsearBloque bloques
    resultados <- sequence parseos
    let items = [item | ItemBloque item <- resultados]
        salas = [sala | SalaBloque sala <- resultados]
        itemsMap = Map.fromList [(itemName item, item) | item <- items]
        salasMap = Map.fromList [(roomName sala, sala) | sala <- salas]
    return (salasMap, itemsMap)

-- Tipo auxiliar para distinguir bloques
data Bloque = ItemBloque Item | SalaBloque Room

-- Parsea un bloque individual
parsearBloque :: [String] -> Either String Bloque
parsearBloque lineas = 
    let lineasLimpias = map trim $ filter (not . null) lineas
    in case lineasLimpias of
        (primera:resto) 
            | "ITEM:" `isPrefixOf` primera -> 
                parsearItem lineasLimpias >>= \item -> Right (ItemBloque item)
            | "SALA:" `isPrefixOf` primera -> 
                parsearSala lineasLimpias >>= \sala -> Right (SalaBloque sala)
            | otherwise -> Left $ "Bloque desconocido: " ++ primera
        [] -> Left "Bloque vacío"

-- Parsea un bloque de tipo ITEM
parsearItem :: [String] -> Either String Item
parsearItem lineas = do
    nombre <- extraerValor "ITEM:" lineas
    desc <- extraerValor "DESC:" lineas
    return $ Item nombre desc

-- Parsea un bloque de tipo SALA
parsearSala :: [String] -> Either String Room
parsearSala lineas = do
    nombre <- extraerValor "SALA:" lineas
    desc <- extraerValor "DESC:" lineas
    let salidas = parsearSalidas lineas
        objetos = parsearObjetos lineas
    return $ Room nombre desc salidas objetos

-- Extrae el valor de una línea con prefijo específico
extraerValor :: String -> [String] -> Either String String
extraerValor prefijo lineas =
    case filter (prefijo `isPrefixOf`) lineas of
        (l:_) -> Right $ trim $ drop (length prefijo) l
        [] -> Left $ "No se encontró: " ++ prefijo

-- Parsea todas las líneas SALIDA
parsearSalidas :: [String] -> Map Direction RoomName
parsearSalidas lineas =
    let lineasSalida = filter ("SALIDA:" `isPrefixOf`) lineas
        salidas = map parsearUnaSalida lineasSalida
    in Map.fromList [s | Just s <- salidas]

-- Parsea una línea SALIDA individual
parsearUnaSalida :: String -> Maybe (Direction, RoomName)
parsearUnaSalida linea =
    let contenido = trim $ drop 7 linea  -- Quita "SALIDA:"
        partes = words contenido
    in case partes of
        (dir:"->":resto) -> do
            direccion <- parsearDirString dir
            let destino = unwords resto
            return (direccion, destino)
        _ -> Nothing

-- Parsea una dirección desde string
parsearDirString :: String -> Maybe Direction
parsearDirString s = case map toLower s of
    "norte" -> Just Norte
    "sur" -> Just Sur
    "este" -> Just Este
    "oeste" -> Just Oeste
    _ -> Nothing

-- Parsea todas las líneas OBJETO
parsearObjetos :: [String] -> [ItemName]
parsearObjetos lineas =
    let lineasObj = filter ("OBJETO:" `isPrefixOf`) lineas
    in map (\l -> trim $ drop 7 l) lineasObj

-- Elimina espacios al inicio y final de un string
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile esEspacio
         esEspacio c = c == ' ' || c == '\t' || c == '\n' || c == '\r'