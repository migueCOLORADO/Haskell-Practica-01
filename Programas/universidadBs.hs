-- Importar librerias
import Data.Time.Clock      -- Importar libreria para manejo de tiempo
import Data.List            -- Importar libreria para manejo de listas
import System.IO            -- Importar libreria para manejo de archivos
import Control.Exception () -- Importar libreria para manejo de excepciones | Sirve para evitar que el programa se cierre si ocurre un error
import Control.DeepSeq (deepseq) -- Importar libreria para el uso de DeepSeq, una Funcion de evaluacion profunda | Fuerza la lectura de cada caracter en un archivo
import Data.Maybe (isJust)      -- Importar libreria para el uso de Maybe | Permite encapsular un valor opcional (Just: con valor, Nothing: sin valor)

-- Definicion de los tipos de datos que representan la informacion de un estudiante
data Estudiante = Estudiante {
    nombre :: String,       -- Tipo de dato | Nombre del estudiante
    identificacion :: Int,              -- Tipo de dato | Identificacion del estudiante      
    fechaEntrada :: UTCTime,        -- Tipo de dato | Fecha de entrada del estudiante a la universidad
    fechaSalida :: Maybe UTCTime  -- Usamos Maybe para representar que el estudiante aún hace parte del universidad o ya salió
} deriving (Show, Read)

-- Función para registrar la entrada de un estudiante a la universidad  
registrarEntrada :: String -> Int ->  UTCTime -> [Estudiante] -> [Estudiante]       -- Declaracion de la funcion
registrarEntrada nombreEstudiante idEstudiante tiempo universidad =
    Estudiante nombreEstudiante idEstudiante tiempo Nothing : universidad    -- Retorno de la funcion | Agrega un nuevo estudiante al final de la lista

-- Función para registrar la salida de un estudiante de la universidad
registrarSalida :: Int -> UTCTime -> [Estudiante] -> [Estudiante]       -- Declaracion de la funcion
registrarSalida idEstudiante tiempo universidad =
    map (\e -> if idEstudiante == identificacion e then e { fechaSalida = Just tiempo } else e) universidad    -- Retorno de la funcion | Actualiza la fecha de salida de un estudiante

-- Funcion para buscar un estudiante por su id registrado en la universidad
buscarEstudiante :: Int -> [Estudiante] -> Maybe Estudiante       -- Declaracion de la funcion
buscarEstudiante idEstudiante universidad =
    find (\e -> idEstudiante == identificacion e) universidad    -- Retorno de la funcion | Busca un estudiante por su id
    

-- Función para calcular el tiempo que un estudiante permanecio en la universidad hasta su salida
tiempoEnLaU :: Estudiante -> IO NominalDiffTime
tiempoEnLaU estudiante = do
    case fechaSalida estudiante of
        Just tiempoSalida -> return $ diffUTCTime tiempoSalida (fechaEntrada estudiante)
        Nothing -> do
            tiempoActual <- getCurrentTime      -- Obtenemos la hora actual
            return $ diffUTCTime tiempoActual (fechaEntrada estudiante)     -- Retorno de la funcion | Devolver la diferencia entre la hora actual y la hora de entrada del estudiante

-- Función para guardar la información de los estudiantes en un archivo de texto
guardarLaU :: [Estudiante] -> IO ()      -- Definicion de la funcion
guardarLaU universidad = do         -- Declaracion de la funcion
    withFile "university.txt" WriteMode $ \h -> do     -- Abrir el archivo university.txt en modo escritura
        hPutStr h (unlines (map mostrarEstudiante universidad))     -- Guardar la informacion de los estudiantes en el archivo university.txt
    putStrLn "La informacion de la universidad ha sido guardada exitosamente"   -- Retorno de la funcion | Mensaje de confirmacion
    putStr "Esta informacion la encontraras en el archivo university.txt."      -- Retorno de la funcion | Mensaje de confirmacion

-- Función para cargar la información de los vehículos desde un archivo de texto
cargarLaU :: IO [Estudiante]        -- Definicion de la funcion
cargarLaU = do      -- Declaracion de la funcion
    contenido <- withFile "university.txt" ReadMode $ \h -> do    -- Abrir el archivo university.txt en modo lectura
        contenido <- hGetContents h     -- Leer el contenido del archivo
        contenido `deepseq` return contenido        -- Retorno de la funcion | Devolver el contenido del archivo | ¿Como lo hace? | deepseq evalua el contenido del archivo y lo devuelve como resultado de la funcion | ¿Que es deepseq? | deepseq es una funcion que evalua el contenido de un archivo de manera profunda
    let lineas = lines contenido    -- Separar el contenido del archivo en lineas
    return (map leerEstudiante lineas)      
    where       -- Lee la informacion de cada estudiante en el archivo
        leerEstudiante linea = read linea :: Estudiante         -- Retorno de la funcion | Devolver la informacion de un estudiante

-- Función para mostrar la información de un estudiante como cadena de texto
mostrarEstudiante :: Estudiante -> String
mostrarEstudiante (Estudiante nombre identificacion fechaEntrada fechaSalida) =
    "Estudiante | {Nombre : \"" ++ nombre ++ ", Id : " ++ show identificacion ++ ", Entrada = " ++ show fechaEntrada ++ ", salida = " ++ maybe "Nothing" show fechaSalida ++ "}"

-- Función para listar los Estudiantes en la Universidad
listarEstudiantes :: [Estudiante] -> IO ()
listarEstudiantes [] = putStrLn "No hay Estudiantes en la Universidad."     -- Mensaje por defecto si no hay estudiantes en la universidad
listarEstudiantes estudiante = do
    putStrLn "Estudiante en la Universidad:" 
    mapM_ (putStrLn . mostrarEstudiante) estudiante 
    -- Mostrar la informacion de los estudiantes en la universidad | mapM_ aplica la funcion mostrarEstudiante a cada estudiante en la lista

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar la universidad desde el archivo de texto
    university <- cargarLaU         -- Carga la informacion del archivo a la lista de estudiantes
    putStrLn "¡Bienvenido al Sistema de Gestión de la Universidad!"
    -- Ciclo principal del programa
    cicloPrincipal university  -- Llama a la funcion cicloPrincipal y carga la lista de estudiantes 

-- Función para el ciclo principal del programa
cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal universidad = do
    -- Mostrar opciones del menú
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada del Estudiante"
    putStrLn "2. Registrar salida del Estudiante"
    putStrLn "3. Buscar Estudiante por Identificacion"
    putStrLn "4. Lista de Estudiantes"
    putStrLn "5. Tiempo de Permanencia de un Estudiante en la Universidad"
    putStrLn "6. Salir"

    opcion <- getLine -- Obtiene la ocpion seleccionada por el usuario y la asigna como valor a opcion
    case opcion of --- Evalua la opcion seleccionada por el usuario con la estructura de control "case"
        "1" -> do       -- 1er caso | El usuario quiere registrar la entrada de un estudiante
            putStrLn "Ingrese el Nombre del Estudiante:"
            nombreEstudiante <- getLine
            putStrLn "Ingrese la Identificacion del Estudiante:"
            idEstudianteStr <- getLine
            let idEstudiante = read idEstudianteStr :: Int  -- Lee y cambia el tipo de dato de idEstudianteStr a Int y este valor se asigna a idEstudiante
            tiempoActual <- getCurrentTime      -- Obtiene la hora actual    
            let universityActualizada = registrarEntrada nombreEstudiante idEstudiante tiempoActual universidad -- Actualiza la lista de estudiantes con el nuevo estudiante
            putStrLn $ "Estudiante con Identificacion " ++ idEstudianteStr ++ " ingresado a la universidad."
            guardarLaU universityActualizada        -- Actualiza el archivo con la nueva lista de estudiantes

            cicloPrincipal universityActualizada        -- Ejecuta el ciclo principal con la nueva lista de estudiantes

        "2" -> do       -- 2do caso | El usuario quiere registrar la salida de un estudiante
            putStrLn "Ingrese la Identificacion del Estudiante a Salir:"
            idEstudianteStr <- getLine
            let idEstudiante = read idEstudianteStr :: Int      -- Lee y cambia el tipo de dato de idEstudianteStr a Int y este valor se asigna a idEstudiante
            tiempoActual <- getCurrentTime
            let universidadActualizada = registrarSalida idEstudiante tiempoActual universidad      -- Actualiza la lista de estudiantes con la salida del estudiante
            putStrLn $ "Estudiante con id " ++ idEstudianteStr ++ " salio de la universidad."
            guardarLaU universidadActualizada
            cicloPrincipal universidadActualizada   -- Ejecuta el ciclo principal con la nueva lista de estudiantes

        "3" -> do       -- 3er caso | El usuario quiere buscar un estudiante por su id
            putStrLn "Ingrese la Identificacion del Estudiante a buscar:"
            idEstudianteStr <- getLine
            let idEstudiante = read idEstudianteStr :: Int       -- Lee y cambia el tipo de dato de idEstudianteStr a Int y este valor se asigna a idEstudiante
            case buscarEstudiante idEstudiante universidad of       -- Necesito que me expliques las lineas 119 a la 133 por favor 
                Just estudiante -> do
                    tiempoTotal <- tiempoEnLaU estudiante
                    if isJust (fechaSalida estudiante) then do
                        putStrLn $ "El Estudiante con id " ++ idEstudianteStr ++ " ya salio de la universidad."
                        putStrLn $ "Tiempo que estuvo en la universidad: " ++ show tiempoTotal ++ " segundos."
                    else do
                        putStrLn $ "El Estudiante con id " ++ idEstudianteStr ++ " se encuentra en la universidad."
                        putStrLn $ "Tiempo en la universidad: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Estudiante no encontrado en la universidad."
            cicloPrincipal universidad      -- Ejecuta el ciclo principal con la nueva lista de estudiantes

        "4" -> do       -- 4to caso | El usuario quiere ver la lista de estudiantes en la universidad
            listarEstudiantes universidad      -- Muestra la lista de estudiantes en la universidad
            cicloPrincipal universidad      -- Ejecuta el ciclo principal

        "5" -> do       -- 5to caso | El usuario quiere saber el tiempo de permanencia de un estudiante en la universidads
            putStrLn "Ingrese la Identificacion del Estudiante:"
            idEstudianteStr <- getLine
            let idEstudiante = read idEstudianteStr :: Int
            case buscarEstudiante idEstudiante universidad of
                Just est -> do
                    case fechaSalida est of
                        Just _ -> do
                            tiempoTotal <- tiempoEnLaU est
                            putStrLn $ "Tiempo que el estudiante " ++ nombre est ++ " con id: " ++ idEstudianteStr ++ " permaneció en la universidad: " ++ show tiempoTotal ++ " segundos."
                        Nothing -> do
                            putStrLn $ "El Estudiante " ++ nombre est ++ " con id: " ++ idEstudianteStr ++ " aun esta en la universidad."
                            tiempoTotal <- tiempoEnLaU est
                            putStrLn $ "Tiempo que lleva en la universidad: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "El Estudiante no se encontro registrado en la universidad"
            cicloPrincipal universidad      -- Ejecuta el ciclo principal 
        
        "6" -> putStrLn "¡Hasta luego!"

        _ -> do     -- Caso por defecto | Si la opcion seleccionada no es valida
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal universidad      -- Ejecuta el ciclo principal






