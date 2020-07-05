import Control.Exception
import Data.Char
import Data.List
import Data.Function


main :: IO ()
main = do { 
           str <- leerArchivoEntrada;
           listoParaArbol <- validarArchivo str;
           esp <- ingresarCantEspacios;
           putStrLn ((show esp) ++ "   espacios");
           arbol <- transformar listoParaArbol;
           pathSalida <- obtenerRutaSalida;

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
Deja una lista, donde cada elemento es una 
lista de String, donde cada string es el 
valor de cada nodo.
------------------------------------------}
validarArchivo :: String -> IO ([[String]]) 
validarArchivo str = do
                        let archivo = map separarEnNodos (lines str)
                        if check archivo
                        then return archivo
                        else error "Error de validacion!"

isOtherPunctuation :: Char -> Bool
isOtherPunctuation c = (generalCategory c) == OtherPunctuation

isDashPunctuation :: Char -> Bool
isDashPunctuation c = (generalCategory c) == DashPunctuation 

separarEnNodos :: String -> [String]
separarEnNodos str = filter (not . any isOtherPunctuation) . groupBy ((==) `on` isOtherPunctuation) $ str

check :: [[String]] -> Bool
check archivo = all (==True) (concat (map checkLine archivo))

checkLine :: [String] -> [Bool]
checkLine line = concat (map checkString line)

checkString :: String -> [Bool]
checkString str =  zipWith (||) (map isDashPunctuation str) (map isAlphaNum str)

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


leerArchivoEntrada :: IO String
leerArchivoEntrada = do{
                        putStrLn "Ingresar la ruta del archivo de entrada.";
                        path <- getLine;
                        str <- catch ((readFile) path) manejarErrorArchivo;
                        return str
                       }


manejarErrorArchivo :: SomeException -> IO String
manejarErrorArchivo e = do{
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

------------------------------
-- Arbol y transformacion ----
------------------------------

data ArbN a = Nodo a [ArbN a] Int deriving Show

transformar :: [[String]] -> IO (ArbN String)
transformar (xs:xss) = do
                        let primerCabezas = nub (map head (xs:xss))
                        let restoCabezas = map tail (xs:xss) 
                        --if null xs
                        --then return (Nodo '$' (primerCabeza:restoCabezas) 0)
                        --else 
                        return (cargarHijos primerCabezas (Nodo "$" [] 0))        
                        
cargarHijos :: [String] -> ArbN String -> ArbN String
cargarHijos [] arb = arb
cargarHijos (x:xs) (Nodo str ys i) =  cargarHijos xs (Nodo str ((Nodo x [] 0):ys) i)

encontrarHijos :: ArbN String -> [[String]] -> [[String]]
encontrarHijos arb [] = []
encontrarHijos (Nodo str ys i) (xs:xss) = if str == head xs then (tail xs):(encontrarHijos (Nodo str ys i) xss) else encontrarHijos (Nodo str ys i) xss

transformarHijos :: ArbN String -> [[String]] -> ArbN String
transformarHijos arb xss = cargarHijos (nub (map head xss)) arb

------------------------------
-- /Arbol y transformacion ---
------------------------------


--ASUMIENDO QUE ES VALIDO EL ARBOL
strToArbol :: [Char] -> [Char] -> ArbN Char
strToArbol (x:(z:(w:[]))) ys = if z == '-' then (Nodo x [] (digitToInt w)) else error "Error en string de entrada"
strToArbol (x:(z:xs)) ys = if x == ',' then strToArbol xs ys --[',','2']
                                else 
                                     if z /= '-' then (Nodo x ([(strToArbol (z:xs) (x:ys))]) 0) -- ['1','2',..]
                                     else Nodo x [compararRamas (tail xs) (reverse(head xs:ys))] (digitToInt (head xs))


compararRamas :: [Char] -> [Char] -> ArbN Char 
compararRamas (x:xs) (y:ys) = if x == ',' then 
                                        if head xs == y then compararRamas xs ys else strToArbol (x:xs) (y:ys) 
                                else 
                                        if x == y then compararRamas xs ys else strToArbol (x:xs) (y:ys)
                                        
--strToArbol ['1','2','1','A','-','3','1','2','1','2','A','-','2'] []

obtenerTotalNodo :: ArbN a -> Int
obtenerTotalNodo (Nodo e hs x) = if null hs then x else sum(map obtenerTotalNodo hs)


generarArchivoSalida :: Show a => String -> Int -> ArbN a -> IO ()
generarArchivoSalida ruta esp arb = writeFile ruta (arbToString esp 0 arb (esp + longMaxima arb esp) (longMayorTotal arb))


arbToString :: Show a => Int -> Int -> ArbN a -> Int -> Int -> String

arbToString esp baseEsp (Nodo e hs x) longMax mayorTotal = if null hs

                                                           then (show e) ++ 
                                                                (concat(replicate (longMax - baseEsp - (length (show e))) " ")) ++ 
                                                                "Valor: " ++ 
                                                                concat(replicate (mayorTotal - (length (show x))) " ") ++ 
                                                                (show x)

                                                           else (show e) ++ 
                                                                (arbToStringL esp baseEsp hs longMax mayorTotal) ++ 
                                                                "\n" ++ 
                                                                concat(replicate baseEsp " ") ++ 
                                                                "Total " ++ (show e) ++ ":" ++ 
                                                                (concat(replicate (longMax - baseEsp - (length (show e))) " ")) ++ 
                                                                concat(replicate (mayorTotal - (length (show (obtenerTotalNodo(Nodo e hs x))))) " ") ++ 
                                                                show (obtenerTotalNodo(Nodo e hs x))


arbToStringL :: Show a => Int -> Int -> [ArbN a] -> Int -> Int -> String

arbToStringL esp baseEsp [] longMax mayorTotal = ""
arbToStringL esp baseEsp (h:hs) longMax mayorTotal = "\n" ++ concat(replicate (baseEsp + esp) " ") ++ (arbToString esp (esp + baseEsp) h longMax mayorTotal) ++ (arbToStringL esp baseEsp hs longMax mayorTotal)


longMaxima :: Show a => ArbN a -> Int -> Int
--Devuelve la máxima longitud en caracteres de un renglón del árbol.

longMaxima (Nodo e [] x) esp = 7 + length (show e)    -- Se suma 7 debido a la long de "Total: "
longMaxima (Nodo e (h:hs) x) esp = length (show e) + esp + maximum (map (\arb -> longMaxima arb esp) (h:hs))


longMayorTotal :: ArbN a -> Int
--Devuelve la longitud del valor de nodo más largo (en cantidad de caracteres)

longMayorTotal (Nodo e [] x) = length (show x)
longMayorTotal (Nodo e (h:hs) x) = max (length(show(obtenerTotalNodo (Nodo e (h:hs) x)))) (maximum (map (\arb -> length (show (obtenerTotalNodo arb))) (h:hs)))



--(Nodo "" [Nodo "1" [Nodo "2" [Nodo "1" [Nodo "A" [] 7, Nodo "B" [] 2, Nodo "C" [] 3, Nodo "2" [Nodo "A" [] 1, Nodo "Z" [] 5] 0, Nodo "3" [] 7] 0, Nodo "2" [Nodo "1" [] 0, Nodo "2" [] 3] 0, Nodo "5" [Nodo "1" [] 1] 0] 0] 0, Nodo "2" [Nodo "1" [Nodo "1" [Nodo "1" [Nodo "W" [] 5, Nodo "Z" [] 3] 0] 0, Nodo "2" [Nodo "1" [] 4] 0] 0] 0, Nodo "A" [Nodo "1" [Nodo "2" [Nodo "1" [Nodo "C" [] 2, Nodo "D" [] 1] 0, Nodo "2" [Nodo "S" [] 1, Nodo "T" [] 1] 0] 0] 0, Nodo "4" [Nodo "1" [Nodo "1" [Nodo "G" [] 3, Nodo "H" [] 3] 0, Nodo "2" [Nodo "M" [] 1, Nodo "N" [] 9] 0] 0, Nodo "3" [Nodo "1" [] 2] 0] 0] 0] 0)