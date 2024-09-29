#lang eopl

#|----------EQUIPO DE TRABAJO - TALLER III-------------- 

Ervin Caravali  Ibarra
ervin.caravali@correounivalle.edu.co
Código: 1925648

Brayan Camilo Urrea Jurado
urrea.brayan@correounivalle.edu.co
Código: 2410023

Enlace al repositorio: https://github.com/bcuj2/Taller3FLP.gits
---------------------------------------------------------

************************************* PUNTOS A EVALAUAR *****************************************
Punto a).
propósito: Escriba un programa en su lenguaje de programación que contenga un procedimiento @sumarDigitos
que le permita sumar los dígitos de un número entero positivo. evaluar @sumarDigitos(147) finEval deberá retornar 12


Punto b).
propósito: Escriba un programa en su lenguaje de programación que contenga un procedimiento que permita calcular el
factorial de un número n. Como la gramática para funciones recursivas debe ser propuesta por el grupo, incluya dos ejemplos
de uso para el factorial de 5 y el factorial de 10.


Punto c).
propósito: Escriba un programa en su lenguaje de programación que contenga un procedimiento que permita calcular una
potencia de forma recursiva @potencia(base, exponente). Si no se evidencia el uso de recursión, el ejercicio no será
valido. Incluya un llamado a la función recursiva: "evaluar @potencia (4, 2) finEval " que retornaría 16.


Punto d).
propósito: Escriba un programa que sume los números en un rango de valores positivos [a,b], donde siempre se cumple 
en la invocación a < b:  Por ejemplo "evaluar @sumaRango (2, 5) finEval  "  retornaría 14.


Punto E).
propósito: Se Definieron tres funciones y se está realizando una serie de evaluaciones.
La función 'integrantes' retorna una cadena de texto con los nombres "Ervin_y_Brayan".
La función 'saludar' toma una función 'm' como argumento y retorna una cadena de texto concatenada con el resultado de evaluar 'm'.
La función 'decorate' evalúa la función 'saludar' con 'integrantes' como argumento y retorna el resultado.

Luego, se evalúa 'decorate' y se espera que devuelva "Hola:Ervin_y_Brayan".

letrec
@integrantes() = "Ervin_y_Brayan"
@saludar(@m) = ("Hola:" concat evaluar @m() finEval) 
@decorate() = evaluar @saludar(@integrantes) finEval
{evaluar @decorate() finEval}


Punto F).
propósito: Se Definen tres funciones y se está realizando una serie de evaluaciones.
La función 'integrantes' retorna una cadena de texto con los nombres "Ervin_y_Brayan".
La función 'saludar' toma una función 'm' como argumento y retorna una cadena de texto concatenada con el resultado de evaluar 'm'.
La función 'decorate' toma un argumento 'm' y retorna la concatenación del resultado de evaluar 'saludar' con 'integrantes' y 'm'.

Luego, se evalúa 'decorate' con el argumento "_EstudiantesFLP" y se espera que devuelva "Hola:Ervin_y_Brayan_EstudiantesFLP".

letrec
@integrantes() = "Ervin_y_Brayan"
@saludar(@m) = ("Hola:" concat evaluar @m() finEval)
@decorate(@m) = (evaluar @saludar(@integrantes) finEval concat @m)
{evaluar @decorate("_EstudiantesFLP") finEval}
//Retorna "Hola:Ervin_y_Brayan_EstudiantesFLP".


--------------------INTERPRETADOR SIMPLE--------------------------

Definición de la gramática BNF para las expresiones del lenguaje:

  <programa>         ::= <expression>
                         <un-programa (exp)>
  <expression>       ::= <numero>
                         <numero-lit (num)>
                     ::= "\""<texto>"\""
                         <texto-lit (txt)>
                     ::= <identificador>
                         <var-exp (id)>
                     ::= (<expresion> <primitiva-binaria> <expresion>)
                         <primapp-bin-exp (exp1 prim-binaria exp2)>
                     ::= <primitiva-unaria> (<expresion>)
                         <primapp-un-exp (prim-unaria exp)>
                     
                     ::= Si <expresion> "{" <expresion>  "}" "sino" "{" <expresion> "}"
                         <condicional-exp (test-exp true-exp false-exp)>
                     
                     ::= declarar ({<identificador> = <expresion> ';' }*)) {<expresion>}
                         <variableLocal-exp (ids exps cuerpo)>
                     
                     ::= procedimiento (<identificador>*(',') ) "{" <expresion> "}"
                         <procedimiento-exp (ids cuero)>

                     ::= "evaluar" expresion (expresion *(",") ) finEval
                         <app-exp (exp exps)>

                    
                     
                     ::= letrec {<identificador> ({<identificador}*(,)) = <expresion>}* {<expresion>}
                         <letrec-exp (proc-names idss bodies letrec-body)

 <primitiva-binaria> ::= + (primitiva-suma)
                     ::= ~ (primitiva-resta)
                     ::= / (primitiva-resta)
                     ::= * (primitiva-div)
                     ::= concat( primitiva-concat)
                     ::= > (primitiva-mayor)
                     ::= < (primitiva-menor)
                     ::= >= (primitiva-mayor-igual)
                     ::= <= (primitiva-menor-igual)
                     ::= != (primitiva-diferente)
                     ::= == (primitiva-comparador-igual)

 <primitiva-unaria>  ::= longitud (primitiva-longitud)
                     ::= add1 (primitiva-add1)
                     ::= sub1 (primitiva-sub1)
                     ::= neg (primitiva-negacion-booleana)
|#