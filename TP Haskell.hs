import Control.Exception
import Data.Char


main :: IO ()
main = do { 
           str <- leerArchivoEntrada;
           putStrLn str;
           esp <- ingresarCantEspacios;
           putStrLn ((show esp) ++ "   espacios")
          }


manejarErrorArchivo :: SomeException -> IO String
manejarErrorArchivo e = do{
                           putStrLn ("Archivo de entrada inválido, Excepción: " ++ (show e));
                           leerArchivoEntrada
                          }


leerArchivoEntrada :: IO String
leerArchivoEntrada = do{
                        putStrLn "Ingresar la ruta del archivo de entrada.";
                        path <- getLine;
                        str <- catch (readFile path) manejarErrorArchivo;
                        return str
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




-- Opcion 1 (hojas con valor, nodos sin)

data ArbNH a = NodoNH a [ArbNH a] | Hoja a Int deriving Show

-- Validar: - Arbol vacio
--			- Que no venga lista vacia en un nodo.



---------------------------------------



-- Opcion 2 (nodo == hoja)

data ArbN a = Nodo a [ArbN a] Int deriving Show

-- Validar: - Arbol vacio
--			- Llenar los no hoja con ceros.




obtenerTotalNodo :: ArbN a -> Int

obtenerTotalNodo (Nodo e hs x) = if null hs then x else sum(map obtenerTotalNodo hs)


-- (Nodo 1 [Nodo 2 [Nodo 4 [] 5] 0, Nodo 3 [Nodo 5 [Nodo 8 [] 1, Nodo 9 [] 1] 0 , Nodo 6 [] 1 , Nodo 7 [] 1] 0] 0)


arbToString :: Show a => Int -> Int -> ArbN a -> String  -- Genera el output que va al archivo (con los totales, version final). Primer parámetro: Cantidad de espacios

arbToString esp baseEsp (Nodo e hs x) = if null hs
                                        then (show e) ++ "   " ++ "Valor: " ++ (show x)
                                        else (show e) ++ (arbToStringL esp baseEsp hs) ++ "\n" ++ concat(replicate baseEsp " ") ++ "Total " ++ (show e) ++ ": " ++ show (obtenerTotalNodo(Nodo e hs x))


arbToStringL :: Show a => Int -> Int -> [ArbN a] -> String

arbToStringL esp baseEsp [] = ""
arbToStringL esp baseEsp (h:hs) = "\n" ++ concat(replicate (baseEsp + esp) " ") ++ (arbToString esp (esp + baseEsp) h) ++ (arbToStringL esp baseEsp hs)











