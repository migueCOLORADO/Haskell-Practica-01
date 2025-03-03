-- Importar librerias
import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception ()
import Control.DeepSeq (deepseq)
import Data.Maybe (isJust)

-- Definicion del tipo de datos para representar la informacion de un estudiante
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
    where
        leerEstudiante linea = read linea :: Estudiante         -- Retorno de la funcion | Devolver la informacion de un estudiante

-- Función para mostrar la información de un estudiante como cadena de texto
mostrarEstudiante :: Estudiante -> String
mostrarEstudiante (Estudiante nombre identificacion fechaEntrada fechaSalida) =
    "Estudiante | {Nombre : \"" ++ nombre ++ ", Id : " ++ show identificacion ++ ", Entrada = " ++ show fechaEntrada ++ ", salida = " ++ maybe "Nothing" show fechaSalida ++ "}"

-- Función para listar los Estudiantes en la Universidad
listarEstudiantes :: [Estudiante] -> IO ()
listarEstudiantes [] = putStrLn "No hay Estudiantes en la Universidad."
listarEstudiantes estudiante = do
    putStrLn "Estudiante en la Universidad:"
    mapM_ (putStrLn . mostrarEstudiante) estudiante

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar la universidad desde el archivo de texto
    university <- cargarLaU
    putStrLn "¡Bienvenido al Sistema de Gestión de la Universidad!"

    -- Ciclo principal del programa
    cicloPrincipal university

-- Función para el ciclo principal del programa
cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal universidad = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada del Estudiante"
    putStrLn "2. Registrar salida del Estudiante"
    putStrLn "3. Buscar Estudiante por Identificacion"
    putStrLn "4. Lista de Estudiantes"
    putStrLn "5. Tiempo de Permanencia de un Estudiante en la Universidad"
    putStrLn "6. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el Nombre del Estudiante:"
            nombreEstudiante <- getLine
            putStrLn "Ingrese la Identificacion del Estudiante:"
            idEstudianteStr <- getLine
            let idEstudiante = read idEstudianteStr :: Int
            tiempoActual <- getCurrentTime
            let universityActualizada = registrarEntrada nombreEstudiante idEstudiante tiempoActual universidad
            putStrLn $ "Estudiante con Identificacion " ++ idEstudianteStr ++ " ingresado a la universidad."
            guardarLaU universityActualizada

            cicloPrincipal universityActualizada

        "2" -> do
            putStrLn "Ingrese la Identificacion del Estudiante a Salir:"
            idEstudianteStr <- getLine
            let idEstudiante = read idEstudianteStr :: Int
            tiempoActual <- getCurrentTime
            let universidadActualizada = registrarSalida idEstudiante tiempoActual universidad
            putStrLn $ "Estudiante con id " ++ idEstudianteStr ++ " salio de la universidad."
            guardarLaU universidadActualizada
            cicloPrincipal universidadActualizada

        "3" -> do
            putStrLn "Ingrese la Identificacion del Estudiante a buscar:"
            idEstudianteStr <- getLine
            let idEstudiante = read idEstudianteStr :: Int
            case buscarEstudiante idEstudiante universidad of
                Just estudiante -> do
                    tiempoTotal <- tiempoEnLaU estudiante
                    if isJust (fechaSalida estudiante) then do
                        putStrLn $ "El Estudiante con id " ++ idEstudianteStr ++ " ya salio de la universidad."
                        putStrLn $ "Tiempo que estuvo en la universidad: " ++ show tiempoTotal ++ " segundos."
                    else do
                        putStrLn $ "El Estudiante con id " ++ idEstudianteStr ++ " se encuentra en la universidad."
                        putStrLn $ "Tiempo en la universidad: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Estudiante no encontrado en la universidad."
            cicloPrincipal universidad

        "4" -> do
            listarEstudiantes universidad
            cicloPrincipal universidad

        "5" -> do
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
            cicloPrincipal universidad
        
        "6" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal universidad






