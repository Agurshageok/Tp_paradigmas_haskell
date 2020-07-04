Primer cuatrimestre de 2020
Paradigmas de Programación (Programación Funcional)
Trabajo Práctico 1a: Totalizador

	Se pide programar en Haskell una aplicación que permita ingresar información estructurada jerárquicamente, donde cada item asociado a la menor jerarquía ingresada tendrá asociado una determinado valor. Esta información jerárquica deberá existir en un archivo de texto de entrada en forma de “tabla” (incluyendo cada valor asociado a cada item de la menor jerarquía) fila por fila, para finalmente visualizar por pantalla la información en forma jerárquica con totalizadores de los valores asociados en cada nivel del la jerarquía.

	En el archivo de texto de entrada la información se indica por filas, donde cada fila sontiene una sucesión de valores alfanuméricos. Cada fila consistirá en un string con un cierto formato, conteniendo una lista de strings separados por comas, finalizando con un valor numérico asociado final que lo separará de los datos anteriores con otro separador distinto de la coma (puede ser el caracter guión) y eventuales caracteres separadores. Cada una de las filas deberá poseer el mismo formato finalizando también con el valor numérico final, aunque la cantidad de valores string de la lista puede llegar a ser distinta. Sí deberá cumplirse que las listas de strings de cada fila deberán formar un orden lexicográfico de menor a mayor respecto a las listas de strings del resto de las filas (como si cada string de la lista de strings de cada fila conformara una letra de una palabra, comparada el resto de las palabras de las filas restantes). No podrán haber filas idénticas en relación a la lista de strings.

Un ejemplo de contenido de varias filas en el archivo puede ser el siguiente:

'''
1,2,1,A-7
1,2,1,B-2
1,2,1,C-3
1,2,1,2,A-1
1,2,1,2,Z-5
1,2,1,3-7
1,2,2,1-0
1,2,2,2-3
1,2,5,1-1
2,1,1,1,W-5
2,1,1,1,Z-3
2,1,2,1-4
A,1,2,1,C-2
A,1,2,1,D-1
A,1,2,2,S-1
A,1,2,2,T-1
A,4,1,1,G-3
A,4,1,1,H-3
A,4,1,2,M-1
A,4,1,2,N-9
A,4,3,1-2
'''

luego se procederá a visualizar la información ingresada, pero estructurada jerárquicamente. Se deberá visualizar por niveles, indicando al fin de cada nivel la suma de todos los valores adicionales contenidos.
La forma de generar el resultado como salida deberá ser de la siguiente forma (en función de los datos ingresados antes):

   1
      2
         1
            A          Valor:   7
            B          Valor:   2
            C          Valor:   3
            2
               A       Valor:   1
               Z       Valor:   5
            Total 2:            6
            3          Valor:   7
         Total 1:              25
         2
            1          Valor:   0
            2          Valor:   3
         Total 2:               3
         5
            1          Valor:   1
         Total 5:               1
      Total 2:                 29
   Total 1:                    29
   2
      1
         1
            1
               W       Valor:   5
               Z       Valor:   3
            Total 1:            8
         Total 1:               8
         2
            1          Valor:   4
         Total 2:               4
      Total 1:                 12
   Total 2:                    12
   A
      1
         2
            1
               C       Valor:   2
               D       Valor:   1
            Total 1:            3
            2          Valor:   1
         Total 2:               4
      Total 1:                  4
      4
         1
            1
              G        Valor:   3
              H        Valor:   3
            Total 1:            6
            2
               M       Valor:   1
               N       Valor:   9
            Total 2:           10
         Total 1:              16
         3
            1          Valor:   2
         Total 3:               2
      Total 4:                 18
   Total A:                    22
Total:                         63

El formato del ingreso es el siguiente:
Primero se ingresa por teclado el nombre de archivo de entrada. Si hay algún error en el archvo (si es inexisente o existe algún error de formato en los datos), se informa el error y se reingresa el archivo. Si no se produjera error, luego se ingresa la cantidad de caracteres espacio de indentación en el archivo resultado (en el ejemplo esta cantidad fue 3), y luego se ingresa el nombre de archivo de salida donde se dejará el resultado según lo indicado (si el nombre del archivo de salida es erróneo, se informa el error y se deberá reingresar el nombre). Luego del procesamiento exitoso, el usuario ingresará si quiere realizar otro procesamiento (opción “S” o “N”). En el caso de ingresar “S”, se repite el proceso principal; en el caso de ingresar “N”, el proceso principal finaliza.


Características del programa:

	-	Se deberán utilizar las funciones o mecanismos ya existentes en el lenguaje si son adecuados, y no crear funciones para un uso específico.
	-	Se apreciará el uso de tipos de datos, funciones de orden superior, listas por compresión y mónadas.
	-	El reductor principal deberá ser main, y su tipo deberá ser IO ().

