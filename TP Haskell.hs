import Control.Exception
import Data.Char
import Data.List
import Data.Function

main :: IO ()
main = do { 
           str <- leerArchivoEntrada;
           esp <- ingresarCantEspacios;
           -- putStrLn ((show esp) ++ "   espacios");
           arbol <- transformar str; --IO [[String]]
           pathSalida <- obtenerRutaSalida;
           generarArchivoSalida pathSalida esp arbol;
           continuar <- consultarFinDatos;
           if continuar
           then main
           else putStrLn "Fin del procesamiento."
           }

{-----------------------------------------
Validacion del Archivo de entrada
Comprueba que el archivo de entrada contenga
solamente caracteres AlphaNum o Dash
Elimina las comas que separan los marcadores
de nodos en cada linea.
Validaciones: 
        -> Caracteres Solo "1...9" || "a...z" || "A...Z"
        -> ultimo elemento de cada fila tiene el formato "X-N" donde N es un entero, X es un String y estan separados por 1 dash
        -> No existen 2 filas iguales. 
Deja una lista, donde cada elemento es una 
lista de String, donde cada string es el 
valor de cada nodo.
------------------------------------------}


{- 
Revisar que el dash solo aparezca en el ultimo elemento 
-}

validarArchivo :: String -> IO ([[String]]) 
validarArchivo str = do
                        let archivo = map separarEnNodos (lines str)
                        if checkCharacters archivo && checkDash archivo && checkValueIsInt archivo && checkFilasNoRepetidos archivo -- && checkOrder archivo
                        then return archivo
                        else error "Error de validacion!"

isOtherPunctuation :: Char -> Bool
isOtherPunctuation c = (generalCategory c) == OtherPunctuation

isDashPunctuation :: Char -> Bool
isDashPunctuation c = (generalCategory c) == DashPunctuation 

separarEnNodos :: String -> [String]
separarEnNodos str = filter (not . any isOtherPunctuation) . groupBy ((==) `on` isOtherPunctuation) $ str


checkValueIsInt :: [[String]] -> Bool
checkValueIsInt archivo = all (==True) (map checkValueIsIntLine archivo)

checkValueIsIntLine :: [String] -> Bool
checkValueIsIntLine line = checkValueIsIntString $ last line

checkValueIsIntString :: String -> Bool
checkValueIsIntString str = length (words num) >= 1 && (all isDigit num) && (read num ::Int) >= 0 where num = (drop 2 str)


checkDash :: [[String]] -> Bool
checkDash archivo = all (==True) (map checkDashLine archivo)

checkDashLine :: [String] -> Bool
checkDashLine line =  checkDashString $ last line

checkDashString :: String -> Bool
checkDashString str = isDashPunctuation $ str !! 1

checkCharacters :: [[String]] -> Bool
checkCharacters archivo = all (==True) (concat (map checkCharactersLine archivo))

checkCharactersLine :: [String] -> [Bool]
checkCharactersLine line = concat (map checkCharactersString line)

checkCharactersString :: String -> [Bool]
checkCharactersString str =  zipWith (||) (map isDashPunctuation str) (map isAlphaNum str)

isSorted :: (Ord a) => [a] -> Bool
isSorted []       = True
isSorted [x]      = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

filasConcat :: [[String]] -> [String]
--filasConcat archivo = map (\str -> takeWhile (/= '-') (concat str)) archivo
filasConcat archivo = map concat archivo

checkFilasNoRepetidos :: [[String]] -> Bool
checkFilasNoRepetidos lines = length (filasConcat lines) == length (nub (filasConcat lines))
        --(isSorted $ map head filas) && (checkFilasOrden (map tail filas))

{-
-----------------------------------------------------------------------------------
Manejo de IO.
Validaciones pertinentes para evitar errores de input humano.
Manejo de "repeat", mas de un procesamiento por ejecucion. 
-----------------------------------------------------------------------------------
-}

obtenerRutaSalida :: IO String
obtenerRutaSalida = do{
                       putStrLn "Ingrese la ruta del archivo de salida, o el nombre del archivo en la carpeta actual.";
                       str <- getLine;
                       ruta <- catch (writeFile str "" >> return str) manejarRutaInvalida;
                       return ruta
                      }

manejarRutaInvalida :: SomeException -> IO String
manejarRutaInvalida e = do{
                           putStrLn("La ruta ingresada no es válida, excepción: " ++ (show e));
                           obtenerRutaSalida
                          }


leerArchivoEntrada :: IO [[String]]
leerArchivoEntrada = do{
                        putStrLn "Ingresar la ruta del archivo de entrada.";
                        str <- capturarLineaYleer;
                        result <- catch (validarArchivo str) manejarErrorArchivo2;
                        return result
                       }

capturarLineaYleer :: IO String
capturarLineaYleer = do{
                        path <- getLine;
                        str <- catch ((readFile) path) manejarErrorArchivo;
                        --result <- catch (validarArchivo str) manejarErrorArchivo;
                        return str
                        }

manejarErrorArchivo :: SomeException -> IO String
manejarErrorArchivo e = do{
                           putStrLn ("Archivo de entrada inválido, Excepción: " ++ (show e));
                           putStrLn "Ingresar la ruta del archivo de entrada.";
                           capturarLineaYleer
                          }

manejarErrorArchivo2 :: SomeException -> IO [[String]]
manejarErrorArchivo2 e = do{
                           putStrLn ("Archivo de entrada inválido, Excepción: " ++ (show e));
                           leerArchivoEntrada
                          }


ingresarCantEspacios :: IO Int
ingresarCantEspacios = do{
                          putStrLn "Ingresar la cantidad de espacios de indentación para el archivo de salida.";
                          esp <- getLine;  
                          if validarCantEspacios esp
                          then return (read esp ::Int)
                          else do{
                                  putStrLn "El valor ingresado no es un entero mayor que cero.";
                                  ingresarCantEspacios
                                 }
                         }

validarCantEspacios :: String -> Bool
validarCantEspacios esp = length (words esp) == 1 && (all isDigit esp) && (read esp ::Int) > 0


consultarFinDatos :: IO Bool
--Devuelve True si se desea continuar ingresando archivos.
consultarFinDatos = do{
                        putStrLn "Desea procesar otro archivo de entrada? Ingrese S/N.";
                        resp <- getLine;
                        if esSNValido resp
                        then return (resp == "S" || resp == "s")
                        else do{
                                putStrLn "Ingreso inválido, debe ser de la forma S/N.";
                                consultarFinDatos
                               }
                      }


esSNValido :: String -> Bool
esSNValido str = str == "S" || str == "N" || str == "s" || str == "n"

{-----------------------------
-- Arbol y transformacion ----
------------------------------}
{-
[["1","2","1","A-7"],["1","2","1","B-2"],["1","2","1","C-3"],["1","2","1","2","A-1"],["1","2","1","2","Z-5"],["1","2","1","3-7"],["1","2","2","1-0"],["1","2","2","2-3"],["1","2","5","1-1"],["2","1","1","1","W-5"],["2","1","1","1","Z-3"],["2","1","2","1-4"],["A","1","2","1","C-2"],["A","1","2","1","D-1"],["A","1","2","2","S-1"],["A","1","2","2","T-1"],["A","4","1","1","G-3"],["A","4","1","1","H-3"],["A","4","1","2","M-1"],["A","4","1","2","N-9"],["A","4","3","1-2"]]
-}
data ArbN a = Nodo a [ArbN a] Int deriving Show

etiqueta :: ArbN a -> a
etiqueta (Nodo e hs x) = e

existeHijo :: Eq a => a -> ArbN a -> Bool
existeHijo hijo (Nodo e [] x) = False
existeHijo hijo (Nodo e (h:hs) x) = hijo == (etiqueta h) || (existeHijo hijo (Nodo e hs x))

agregarHijo :: ArbN a -> ArbN a -> ArbN a
--Se agrega el 2do arbol a la lista de hijos del 1ro.
agregarHijo (Nodo e hs x) hijo = Nodo e (hijo:hs) x

reemplazarHijo :: Eq a => ArbN a -> ArbN a -> ArbN a
--Se reemplaza el 2do arbol por el hijo del 1ro que tenía la misma etiqueta. Antes debe usarse existeHijo.
reemplazarHijo (Nodo e hs x) arb = Nodo e (reempHijoL hs arb) x

reempHijoL :: Eq a => [ArbN a] -> ArbN a -> [ArbN a]
reempHijoL (h:hs) arb = if (etiqueta h) == (etiqueta arb) then arb:hs else h:(reempHijoL hs arb)

getHijo :: Eq a => a -> ArbN a -> ArbN a
--Devuelve el hijo del árbol que contiene la etiqueta del 1er parámetro. Antes debe usarse existeHijo.
getHijo n (Nodo e (h:hs) x) = if n == (etiqueta h) then h else getHijo n (Nodo e hs x)



transformar :: [[String]] -> IO (ArbN String)
transformar xss = return (agregarFilas xss (Nodo "" [] 0))

agregarFilas :: [[String]] -> ArbN String -> ArbN String
agregarFilas [] arb = arb
agregarFilas (f:fs) arb = cargarFila f (agregarFilas fs arb)

cargarFila :: [String] -> ArbN String -> ArbN String
cargarFila (n:ns) arb = if elem '-' n then agregarHijo arb (Nodo (takeWhile (/= '-') n) [] (read (tail(dropWhile (/= '-') n)) :: Int)) 
                                      else if existeHijo n arb then reemplazarHijo arb (cargarFila ns (getHijo n arb))
                                                               else agregarHijo arb (cargarFila ns (Nodo n [] 0))




obtenerTotalNodo :: ArbN a -> Int
obtenerTotalNodo (Nodo e hs x) = if null hs then x else sum(map obtenerTotalNodo hs)


generarArchivoSalida :: String -> Int -> ArbN String -> IO ()
generarArchivoSalida ruta esp arb =do
        writeFile ruta (arbToString esp 0 arb (esp + longMaxima arb esp) (longMayorTotal arb));
        putStrLn "Archivo Procesado con exito!"


arbToString :: Int -> Int -> ArbN String -> Int -> Int -> String

arbToString esp baseEsp (Nodo e hs x) longMax mayorTotal = if null hs

                                                           then e ++ 
                                                                (concat(replicate (longMax - baseEsp - (length e)) " ")) ++ 
                                                                "Valor: " ++ 
                                                                concat(replicate (mayorTotal - (length (show x))) " ") ++ 
                                                                (show x)

                                                           else e ++ 
                                                                (arbToStringL esp baseEsp hs longMax mayorTotal) ++ 
                                                                "\n" ++ 
                                                                concat(replicate baseEsp " ") ++ 
                                                                "Total " ++ e ++ ":" ++ 
                                                                (concat(replicate (longMax - baseEsp - (length e)) " ")) ++ 
                                                                concat(replicate (mayorTotal - (length (show (obtenerTotalNodo(Nodo e hs x))))) " ") ++ 
                                                                show (obtenerTotalNodo(Nodo e hs x))


arbToStringL :: Int -> Int -> [ArbN String] -> Int -> Int -> String

arbToStringL esp baseEsp [] longMax mayorTotal = ""
arbToStringL esp baseEsp (h:hs) longMax mayorTotal = "\n" ++ concat(replicate (baseEsp + esp) " ") ++ (arbToString esp (esp + baseEsp) h longMax mayorTotal) ++ (arbToStringL esp baseEsp hs longMax mayorTotal)


longMaxima :: ArbN String -> Int -> Int
--Devuelve la máxima longitud en caracteres de un renglón del árbol.

longMaxima (Nodo e [] x) esp = 7 + length e    -- Se suma 7 debido a la long de "Total: "
longMaxima (Nodo e (h:hs) x) esp = length e + esp + maximum (map (\arb -> longMaxima arb esp) (h:hs))


longMayorTotal :: ArbN a -> Int
--Devuelve la longitud del valor de nodo más largo (en cantidad de caracteres)

longMayorTotal (Nodo e [] x) = length (show x)
longMayorTotal (Nodo e (h:hs) x) = max (length(show(obtenerTotalNodo (Nodo e (h:hs) x)))) (maximum (map (\arb -> length (show (obtenerTotalNodo arb))) (h:hs)))



--(Nodo "" [Nodo "1" [Nodo "2" [Nodo "1" [Nodo "A" [] 7, Nodo "B" [] 2, Nodo "C" [] 3, Nodo "2" [Nodo "A" [] 1, Nodo "Z" [] 5] 0, Nodo "3" [] 7] 0, Nodo "2" [Nodo "1" [] 0, Nodo "2" [] 3] 0, Nodo "5" [Nodo "1" [] 1] 0] 0] 0, Nodo "2" [Nodo "1" [Nodo "1" [Nodo "1" [Nodo "W" [] 5, Nodo "Z" [] 3] 0] 0, Nodo "2" [Nodo "1" [] 4] 0] 0] 0, Nodo "A" [Nodo "1" [Nodo "2" [Nodo "1" [Nodo "C" [] 2, Nodo "D" [] 1] 0, Nodo "2" [Nodo "S" [] 1, Nodo "T" [] 1] 0] 0] 0, Nodo "4" [Nodo "1" [Nodo "1" [Nodo "G" [] 3, Nodo "H" [] 3] 0, Nodo "2" [Nodo "M" [] 1, Nodo "N" [] 9] 0] 0, Nodo "3" [Nodo "1" [] 2] 0] 0] 0] 0)