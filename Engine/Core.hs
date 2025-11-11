module Engine.Core (processCommand) where

import Engine.Types
import qualified Data.Map as Map
import Data.List (intercalate)

-- Función pura que procesa comandos y actualiza el estado
processCommand :: Command -> GameState -> (String, GameState)
processCommand cmd state = case cmd of
    Ir dir -> moverJugador dir state
    Mirar -> (describirSalaActual state, state)
    Tomar obj -> tomarObjeto obj state
    Inventario -> (mostrarInventario state, state)
    Salir -> ("¡Hasta luego! Gracias por jugar.", state)

-- Mueve al jugador en una dirección
moverJugador :: Direction -> GameState -> (String, GameState)
moverJugador dir state =
    case Map.lookup (currentRoom state) (worldRooms state) of
        Nothing -> ("Error: sala actual no encontrada.", state)
        Just sala -> 
            case Map.lookup dir (roomExits sala) of
                Nothing -> ("No puedes ir en esa dirección.", state)
                Just nuevaSala -> 
                    let nuevoEstado = state { currentRoom = nuevaSala }
                        mensaje = "Te mueves hacia " ++ mostrarDireccion dir ++ ".\n\n" 
                                ++ describirSalaActual nuevoEstado
                    in (mensaje, nuevoEstado)

-- Describe la sala actual con sus objetos y salidas
describirSalaActual :: GameState -> String
describirSalaActual state =
    case Map.lookup (currentRoom state) (worldRooms state) of
        Nothing -> "Error: no se puede encontrar la sala actual."
        Just sala -> 
            let desc = roomDesc sala
                objetos = if null (roomObjects sala)
                         then "\nNo hay objetos visibles aquí."
                         else "\nObjetos visibles: " ++ intercalate ", " (roomObjects sala)
                salidas = if Map.null (roomExits sala)
                         then "\nNo hay salidas."
                         else "\nSalidas: " ++ mostrarSalidas (roomExits sala)
            in roomName sala ++ "\n" ++ desc ++ objetos ++ salidas

-- Toma un objeto de la sala y lo añade al inventario
tomarObjeto :: String -> GameState -> (String, GameState)
tomarObjeto nombreObj state =
    case Map.lookup (currentRoom state) (worldRooms state) of
        Nothing -> ("Error: sala actual no encontrada.", state)
        Just sala ->
            if nombreObj `elem` roomObjects sala
            then
                let nuevaSala = sala { roomObjects = filter (/= nombreObj) (roomObjects sala) }
                    nuevasRooms = Map.insert (currentRoom state) nuevaSala (worldRooms state)
                    nuevoInv = nombreObj : playerInventory state
                    nuevoEstado = state { 
                        playerInventory = nuevoInv,
                        worldRooms = nuevasRooms
                    }
                in ("Has tomado: " ++ nombreObj, nuevoEstado)
            else
                ("No hay ningún objeto llamado '" ++ nombreObj ++ "' aquí.", state)

-- Muestra el contenido del inventario
mostrarInventario :: GameState -> String
mostrarInventario state =
    if null (playerInventory state)
    then "Tu inventario está vacío."
    else "Inventario:\n" ++ unlines (map ("  - " ++) (playerInventory state))

-- Convierte dirección a texto legible
mostrarDireccion :: Direction -> String
mostrarDireccion Norte = "el norte"
mostrarDireccion Sur = "el sur"
mostrarDireccion Este = "el este"
mostrarDireccion Oeste = "el oeste"

-- Muestra las salidas disponibles
mostrarSalidas :: Map.Map Direction RoomName -> String
mostrarSalidas salidas = 
    intercalate ", " [mostrarDireccion dir ++ " (" ++ sala ++ ")" 
                     | (dir, sala) <- Map.toList salidas]
