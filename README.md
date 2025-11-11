# Motor de Aventura de Texto

**Proyecto 1 - Laboratorio de Lenguajes de Programación I**

## Información del Proyecto

- **Nombre:** Gerel Negreira Peruzzi
- **Carnet:** 09-11163


## Descripción

Este proyecto implementa un motor reutilizable para juegos de aventura de texto en Haskell. El motor está completamente desacoplado del contenido del juego, permitiendo cargar diferentes mundos desde archivos de configuración.

## Compilación y Ejecución

### Usando Stack (Recomendado)
```bash
# Compilar el proyecto
stack build

# Ejecutar el juego
stack run
```

### Usando Cabal
```bash
# Compilar
cabal build

# Ejecutar
cabal run aventura-texto
```

## Estructura del Proyecto

├── Main.hs                  # Punto de entrada y bucle principal
├── Engine/
│   ├── Types.hs            # Definición de tipos de datos
│   ├── Parser.hs           # Parseo de comandos
│   ├── Core.hs             # Lógica pura del juego
│   └── Persistence.hs      # Carga del mundo desde archivo
├── mundo.txt               # Archivo de ejemplo del mundo
├── aventura-texto.cabal    # Archivo de configuración Cabal
├── stack.yaml              # Archivo de configuración Stack
└── README.md               # Este archivo

## Comandos Disponibles

- `ir <dirección>` - Muévete a otra sala (norte, sur, este, oeste)
- `mirar` - Examina la sala actual
- `tomar <objeto>` o `coger <objeto>` - Toma un objeto
- `inventario` o `inv` - Muestra tu inventario
- `salir` - Termina el juego
- `ayuda` - Muestra la lista de comandos

## Justificación de Diseño

### Estructuras de Datos

**1. Map para el Mundo y las Salidas**

Elegí usar `Data.Map` para almacenar las salas del mundo, los objetos y las salidas de cada sala por las siguientes razones:

- **Búsqueda eficiente**: `Map` ofrece búsqueda en tiempo O(log n), lo cual es ideal para encontrar salas por nombre o direcciones por clave.
- **Nombres únicos**: Los mapas garantizan claves únicas, lo que previene duplicados de salas u objetos.
- **Semántica clara**: Usar `Map RoomName Room` expresa claramente la relación entre el nombre de una sala y sus datos.

**2. Listas para Inventarios y Objetos en Salas**

Opté por listas simples `[ItemName]` para:

- **Simplicidad**: El inventario típicamente tiene pocos elementos, por lo que la búsqueda lineal es aceptable.
- **Orden**: Las listas mantienen el orden de adquisición, lo cual puede ser útil para mostrar el inventario.
- **Facilidad de manipulación**: Agregar y remover elementos es directo con operaciones como `(:)` y `filter`.

### Separación entre Lógica Pura e Impura

El diseño logra una clara separación entre código puro e impuro:

**Módulos Puros (Sin IO):**

- **Engine.Types**: Solo definiciones de tipos, completamente puro.
- **Engine.Parser**: Función `parseCommand :: String -> Maybe Command` es pura, solo transforma strings.
- **Engine.Core**: La función clave `processCommand :: Command -> GameState -> (String, GameState)` es completamente pura. Recibe estado y comando, retorna nuevo estado y mensaje. No hay efectos secundarios.

**Módulos con IO:**

- **Engine.Persistence**: `loadWorldData` usa `IO` para leer archivos, pero retorna `Either` para manejar errores de forma explícita.
- **Main.hs**: Contiene toda la lógica de I/O: leer entrada del usuario, imprimir mensajes, y el bucle principal del juego.

**Beneficios de esta Separación:**

1. **Testabilidad**: La lógica pura en `Engine.Core` puede probarse sin mocks de I/O.
2. **Razonamiento**: Las funciones puras son más fáciles de entender y verificar.
3. **Reutilización**: El motor puede usarse en diferentes contextos (CLI, GUI, web) cambiando solo el módulo de I/O.
4. **Modularidad**: Cada módulo tiene una responsabilidad clara y bien definida.

## Formato del Archivo mundo.txt

El archivo define objetos y salas usando bloques separados por `---`:

ITEM: nombre_objeto
DESC: Descripción del objeto
SALA: nombre_sala
DESC: Descripción de la sala
SALIDA: Dirección -> Nombre_Sala_Destino
OBJETO: nombre_objeto

## Ejemplo de Uso

mirar
Sala de Estar
Una acogedora sala de estar con muebles antiguos...
Objetos: llaves, mapa
Salidas: norte, este


tomar llaves
Has tomado: llaves


inventario
Inventario:


llaves


ir norte
Te mueves hacia el norte.

Cocina
Una cocina desordenada con platos sucios...

## Notas Técnicas

- El proyecto usa solo bibliotecas estándar de Haskell (`base` y `containers`)
- Todos los tipos de datos están definidos usando ADTs con `data`
- El estado del juego es inmutable y se pasa funcionalmente
- Los errores se manejan usando `Maybe` y `Either`