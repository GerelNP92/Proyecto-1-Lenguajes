module Main where

import Engine.Types
import Engine.Parser
import Engine.Core
import Engine.Persistence
import qualified Data.Map as Map
import System.IO (hFlush, stdout)
import System.Exit (exitSuccess)
import Data.Char (toLower)
import Data.List (intercalate)

main :: IO ()
main = do
    putStrLn "=== Motor de Aventura de Texto ==="
    putStrLn "Cargando mundo..."
    
    resultado <- loadWorldData "mundo.txt"
    
    case resultado of
        Left error -> do
            putStrLn $ "Error al cargar el mundo: " ++ error
            putStrLn "Asegúrate de que el archivo mundo.txt existe y está bien formado."
        
        Right (salas, items) -> do
            -- Verificar que hay al menos una sala para comenzar
            if Map.null salas
            then putStrLn "Error: el mundo no tiene ninguna sala definida."
            else do
                -- Tomar la primera sala como sala inicial
                let primeraSala = fst $ head $ Map.toList salas
                    estadoInicial = GameState {
                        currentRoom = primeraSala,
                        playerInventory = [],
                        worldRooms = salas,
                        worldItems = items
                    }
                
                putStrLn "\n¡Mundo cargado exitosamente!"
                putStrLn "Escribe 'ayuda' para ver los comandos disponibles.\n"
                
                -- Mostrar la sala inicial
                putStrLn $ describirSalaActual estadoInicial
                putStrLn ""
                
                -- Iniciar el bucle del juego
                gameLoop estadoInicial

-- Función auxiliar para describir la sala
describirSalaActual :: GameState -> String
describirSalaActual state =
    case Map.lookup (currentRoom state) (worldRooms state) of
        Nothing -> "Error: no se puede encontrar la sala actual."
        Just sala -> 
            let desc = roomDesc sala
                objetos = if null (roomObjects sala)
                         then "\nNo hay objetos visibles aquí."
                         else "\nObjetos: " ++ intercalate ", " (roomObjects sala)
                salidas = "\nSalidas: " ++ mostrarSalidasMain (roomExits sala)
            in roomName sala ++ "\n" ++ desc ++ objetos ++ salidas

-- Bucle principal del juego
gameLoop :: GameState -> IO ()
gameLoop estado = do
    putStr "> "
    hFlush stdout
    entrada <- getLine
    
    -- Manejo especial para el comando de ayuda
    if map toLower entrada `elem` ["ayuda", "help"]
    then do
        mostrarAyuda
        gameLoop estado
    else
        case parseCommand entrada of
            Nothing -> do
                putStrLn "No entiendo ese comando. Escribe 'ayuda' para ver los comandos."
                gameLoop estado
            
            Just Salir -> do
                putStrLn "¡Gracias por jugar! Hasta pronto."
                exitSuccess
            
            Just comando -> do
                let (mensaje, nuevoEstado) = processCommand comando estado
                putStrLn mensaje
                putStrLn ""
                gameLoop nuevoEstado

-- Muestra la ayuda con todos los comandos
mostrarAyuda :: IO ()
mostrarAyuda = do
    putStrLn "\n=== Comandos Disponibles ==="
    putStrLn "  ir <dirección>     - Muévete a otra sala (norte, sur, este, oeste)"
    putStrLn "  mirar              - Examina la sala actual"
    putStrLn "  tomar <objeto>     - Toma un objeto de la sala"
    putStrLn "  coger <objeto>     - Sinónimo de tomar"
    putStrLn "  inventario (inv)   - Muestra tu inventario"
    putStrLn "  salir              - Termina el juego"
    putStrLn "  ayuda              - Muestra este mensaje"
    putStrLn ""

-- Muestra las salidas de una sala
mostrarSalidasMain :: Map.Map Direction RoomName -> String
mostrarSalidasMain salidas = 
    if Map.null salidas
    then "ninguna"
    else intercalate ", " [mostrarDir dir | (dir, _) <- Map.toList salidas]
  where
    mostrarDir Norte = "norte"
    mostrarDir Sur = "sur"
    mostrarDir Este = "este"
    mostrarDir Oeste = "oeste"