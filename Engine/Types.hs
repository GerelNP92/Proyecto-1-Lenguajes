module Engine.Types where

import qualified Data.Map as Map
import Data.Map (Map)

-- Tipos básicos para identificadores
type RoomName = String
type ItemName = String

-- Direcciones posibles en el juego
data Direction = Norte | Sur | Este | Oeste
    deriving (Show, Eq, Ord)

-- Comandos que el jugador puede ejecutar
data Command 
    = Ir Direction
    | Mirar
    | Tomar String
    | Inventario
    | Salir
    deriving (Show, Eq)

-- Definición de un objeto del juego
data Item = Item
    { itemName :: ItemName
    , itemDesc :: String
    } deriving (Show, Eq)

-- Definición de una sala
data Room = Room
    { roomName :: RoomName
    , roomDesc :: String
    , roomExits :: Map Direction RoomName  -- Salidas disponibles
    , roomObjects :: [ItemName]            -- Objetos en la sala
    } deriving (Show, Eq)

-- Estado completo del juego
data GameState = GameState
    { currentRoom :: RoomName              -- Sala donde está el jugador
    , playerInventory :: [ItemName]        -- Inventario del jugador
    , worldRooms :: Map RoomName Room      -- Todas las salas del mundo
    , worldItems :: Map ItemName Item      -- Todos los objetos definidos
    } deriving (Show, Eq)