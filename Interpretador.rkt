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

;Especificación léxica
(define scanner-spec-simple-interpreter
  '((white-sp ;Espacios en blanco
     (whitespace) skip) 
    (comentario ;Comentarios
     ("//" (arbno (not #\newline))) skip);
    (texto
     ((or letter "_") (arbno (or letter digit "-" ":"))) string);Esta regla define cómo reconocer y manejar cadenas de texto.
                                                                ;Puede comenzar con una letra o un guión bajo("_") y luego puede contener letras, dígitos, guiones y dos puntos.
    (identificador ;Identificadores
     ("@" (arbno letter)) symbol)
    (numero ;Número entero positivo
     (digit (arbno digit)) number)
    (numero ;Número entero negativo
     ("-" digit (arbno digit)) number)
    (numero
     (digit (arbno digit) "." digit (arbno digit)) number) ;Número decimal positivo
    (numero ;Número decimal negativo
     ("-" digit (arbno digit) "." digit (arbno digit)) number)
    ))


;Especificación Sintáctica (Gramática)
(define grammar-simple-interpreter
  '(;Programa
    (programa (expresion) un-programa)

    ;Expresiones

    ;Una expresión que es un número literal
    (expresion (numero) numero-lit)

    ;Una expresión que es un texto literal entre comillas dobles
    (expresion ("\""texto"\"") texto-lit)

    ;Una expresión que es un identificador
    (expresion (identificador) var-exp)

    ;Expresión que contiene una operación binaria
    (expresion
     ("("expresion primitiva-binaria expresion")") primapp-bin-exp)
    ;Expresión que es una operación unaria entre paréntesis
    (expresion (primitiva-unaria "("expresion")") primapp-un-exp)
    
    ;Condicional
    (expresion ("Si" expresion "{" expresion "}" "sino" "{" expresion "}") condicional-exp)
    
    ;Variables Locales
    (expresion ("declarar" "(" (arbno identificador "=" expresion ";") ")" "{" expresion "}") variableLocal-exp)

    ;Procedimientos
    (expresion ("procedimiento" "(" (separated-list identificador ",") ")" "{" expresion "}") procedimiento-exp)
    (expresion ("evaluar" expresion "(" (separated-list expresion ",") ")" "finEval") app-exp)
    
    ;Recursividad
    (expresion ("letrec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "{" expresion "}") 
                letrec-exp)
     
     
    ;Primitivas-binarias
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)
    (primitiva-binaria (">") primitiva-mayor)
    (primitiva-binaria ("<") primitiva-menor)
    (primitiva-binaria (">=") primitiva-mayor-igual)
    (primitiva-binaria ("<=") primitiva-menor-igual) 
    (primitiva-binaria ("!=") primitiva-diferente)               
    (primitiva-binaria ("==") primitiva-comparador-igual)
                    
    ;Primitivas-unarias
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    (primitiva-unaria ("neg") primitiva-negacion-booleana)
    
    ))

;Construyendo datos automáticamente
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

;Definición de la función show-the-datatypes
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

; El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)
(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

; El Analizador Léxico (Scanner)
(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

; El Interpretador (FrontEnd + Evaluación + Señal para lectura)
(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (eval-program  pgm))
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

 
 ;*******************************************************************************************

;El Interpretador

;eval-program: <programa> -> numero
;Función que evalúa un programa teniendo en cuenta un ambiente dado (Se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    ; Se utiliza el macro/casos 'cases' para hacer coincidencia de patrones sobre 'pgm'
    (cases programa pgm
      ; Si 'pgm' coincide con el patrón '(un-programa (body))', entonces:
      (un-programa (body)
                   ; Se evalúa la expresión 'body' en el entorno inicial
                   (eval-expresion body (init-env))))))

; Función que inicializa un ambiente con símbolos y valores definidos.
(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e)     ;lista de símbolos
     '(1 2 3 "hola" "FLP") ;Lista de valores correspondientes a cada símbolo
     (empty-env))))        ;Un ambiente vacío


;eval-expresion: <expresion> <environment> -> numero
;Evalua la expresion en el ambiente de entrada
(define eval-expresion
  (lambda (exp env)
    (cases expresion exp
      ; Caso: Número literal. Retorna el propio número.
      (numero-lit (num) num)
      
      ; Caso: Texto literal. Retorna el propio texto.
      (texto-lit (txt) txt)
      
      ; Caso: Expresión de variable. Busca la variable en el entorno y la retorna.
      (var-exp (id) (buscar-variable env id))
      
      ; Caso: Aplicación de operador binario. Evalúa los operandos y aplica la operación.
      (primapp-bin-exp (exp1 prim-binaria exp2)
                       (let (
                             (args1 (eval-rand exp1 env))
                             (args2 (eval-rand exp2 env))
                             )
                         (apply-primitiva-binaria args1 prim-binaria args2)))
      
      ; Caso: Aplicación de operador unario. Evalúa el operando y aplica la operación.
      (primapp-un-exp (prim-unaria exp)
                      (let (
                            (args (eval-rand exp env))
                            )
                        (apply-primitiva-unaria prim-unaria args)))
      
      ; Caso: Expresión condicional. Evalúa la condición y retorna la rama correspondiente.
      (condicional-exp (test-exp true-exp false-exp)
                       (if (valor-verdad? (eval-expresion test-exp env))
                           (eval-expresion true-exp env)
                           (eval-expresion false-exp env)))
      
      ; Caso: Expresión de variable local. Evalúa los valores y extiende el entorno.
      (variableLocal-exp (ids exps cuerpo)
                          (let ((args (eval-rands exps env)))
                           (eval-expresion cuerpo (extend-env ids args env))))
                           
      ; Caso: Expresión de procedimiento. Crea una cerradura (closure).
      (procedimiento-exp (ids cuerpo)
        (cerradura ids cuerpo env))
      
      ; Caso: Aplicación de procedimiento. Evalúa el procedimiento y aplica los argumentos.
      (app-exp (rator rands)
               (let ((proc (eval-expresion rator env))
                     (args (eval-rands rands env)))
                 (if (procVal? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expresion
                                 "Intento de aplicar un no-procedimiento ~s" proc))))
      
      ; Caso: Expresión letrec. Evalúa el cuerpo con entorno extendido de forma recursiva.
      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expresion letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))))

  )

;Función auxiliar para aplicar eval-rand a cada elemento dentro de exp1 exp2
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

;Función auxiliar para evaluar un "rand".
(define eval-rand
  (lambda (rand env)
    (eval-expresion rand env)))

;apply-primitiva-binaria: <expresion> <primitiva> <expresion> -> numero or string
(define apply-primitiva-binaria
  (lambda (exp1 prim exp2)
    (cases primitiva-binaria prim
      (primitiva-suma () (+ exp1 exp2))                          
      (primitiva-resta () (- exp1 exp2))                        
      (primitiva-multi () (* exp1 exp2))                        
      (primitiva-div () (/ exp1 exp2))                          
      (primitiva-concat () (string-append exp1 exp2))          
      (primitiva-mayor () (> exp1 exp2))                       
      (primitiva-menor () (< exp1 exp2))                        
      (primitiva-mayor-igual () (>= exp1 exp2))                
      (primitiva-menor-igual () (<= exp1 exp2))                
      (primitiva-diferente () (not (= exp1 exp2)))             
      (primitiva-comparador-igual () (= exp1 exp2))           
      )))  

;apply-primitiva-unaria <primitiva> <expresion> -> numero
(define apply-primitiva-unaria
  (lambda (prim exp)
    (cases primitiva-unaria prim
      (primitiva-longitud () (string-length exp))                 
      (primitiva-add1 () (+ exp 1))                               
      (primitiva-sub1 () (- exp 1))                               
      (primitiva-negacion-booleana () (not exp))                
      )))     
      

