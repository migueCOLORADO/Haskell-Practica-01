
-- Importar librerias
import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.DeepSeq (deepseq)

-- Definicion del tipo de datos para representar la informacion de un estudiante
data Estudiante = Estudiante {
    nombre :: String,       -- Tipo de dato | Nombre del estudiante
    identificacion :: Int,              -- Tipo de dato | Identificacion del estudiante
    edad :: Int,            -- Tipo de dato | Edad del estudiante
    carrera :: String,      -- Tipo de dato | Carrera del estudiante
    semestre :: Int,        -- Tipo de dato | Semestre del estudiante
    fechaEntrada :: UTCTime,        -- Tipo de dato | Fecha de entrada del estudiante a la universidad
    fechaSalida :: Maybe UTCTime  -- Usamos Maybe para representar que el estudiante aún hace parte del universidad o ya salió
} deriving (Show, Read)

-- Función para registrar la entrada de un estudiante a la universidad  
registrarEntrada :: String -> Int -> Int -> String -> Int -> UTCTime -> [Estudiante] -> [Estudiante]       -- Declaracion de la funcion
registrarEntrada nombreEstudiante idEstudiante edadEstudiante carreraEstudiante semestreEstudiante tiempo universidad =
    Estudiante nombreEstudiante idEstudiante edadEstudiante carreraEstudiante semestreEstudiante tiempo Nothing : universidad    -- Retorno de la funcion | Agrega un nuevo estudiante al final de la lista

-- Función para registrar la salida de un estudiante de la universidad
registrarSalida :: Int -> UTCTime -> [Estudiante] -> [Estudiante]       -- Declaracion de la funcion
registrarSalida idEstudiante tiempo universidad =
    map (\e -> if idEstudiante == identificacion e then e { fechaSalida = Just tiempo } else e) universidad    -- Retorno de la funcion | Actualiza la fecha de salida de un estudiante

-- Funcion para buscar un estudiante por su id registrado en la universidad
buscarEstudiante :: Int -> [Estudiante] -> Maybe Estudiante       -- Declaracion de la funcion
buscarEstudiante idEstudiante universidad =
    find (\e -> idEstudiante == identificacion e && isNothing (fechaSalida e)) universidad    -- Retorno de la funcion | Busca un estudiante por su id
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo que un estudiante permanecio en la universidad hasta su salida
tiempoEnLaU :: Estudiante -> IO NominalDiffTime
tiempoEnLaU estudiante = do
    tiempoActual <- getCurrentTime      -- Obtenemos la hora actual
    return $ diffUTCTime tiempoActual (fechaEntrada estudiante)     -- Retorno de la funcion | Devolver la diferencia entre la hora actual y la hora de entrada del estudiante

-- Función para guardar la información de los vehículos en un archivo de texto
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
mostrarEstudiante (Estudiante nombre identificacion edad carrera semestre fechaEntrada fechaSalida) =
    "Estudiante | {Nombre : \"" ++ nombre ++ ", Id : " ++ show identificacion ++ ", Edad : " ++ maybe "Nothing" show salida ++ "}"



