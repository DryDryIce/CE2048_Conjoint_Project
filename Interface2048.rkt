#lang racket

(require 2htdp/universe)
(require 2htdp/image)
(require "Logic2048.rkt")

(provide iniciar-2048)

;----------------------------------------------------------
; Constantes visuales
;----------------------------------------------------------

(define TAM-CELDA 80)
(define ESPACIO 8)
(define COLOR-FONDO-TABLERO "lightgray")
(define COLOR_TEXTO "black")
(define ANCHO-VENTANA 1100)
(define ALTO-VENTANA 1000)
(define FONDO-VENTANA
  (rectangle ANCHO-VENTANA ALTO-VENTANA "solid" "white"))

(define (centrar-en-ventana imagen)
  (place-image imagen
               (/ ANCHO-VENTANA 2)
               (/ ALTO-VENTANA 2)
               FONDO-VENTANA))

;----------------------------------------------------------
; Estado general
; Se representa como:
; (modo entrada tablero puntaje)
; modo = 'menu o 'juego
; entrada = string
; tablero = lista
; puntaje = numero
;----------------------------------------------------------

(define (crear-estado modo entrada tablero puntaje)
  (cons modo
        (cons entrada
              (cons tablero
                    (cons puntaje '())))))

(define (estado-modo estado)
  (car estado))

(define (estado-entrada estado)
  (car (cdr estado)))

(define (estado-tablero estado)
  (car (cdr (cdr estado))))

(define (estado-puntaje estado)
  (car (cdr (cdr (cdr estado)))))

;----------------------------------------------------------
; Crear estado inicial en menu
;----------------------------------------------------------

(define (crear-estado-menu)
  (crear-estado 'menu "" '() 0))

;----------------------------------------------------------
; Crear estado de juego
;----------------------------------------------------------

(define (crear-estado-juego filas columnas)
  (crear-estado 'juego
                ""
                (crear-tablero-inicial filas columnas)
                0))

;----------------------------------------------------------
; Resultado de jugada -> nuevo estado
;----------------------------------------------------------

(define (actualizar-estado-con-resultado estado resultado)
  (crear-estado
   'juego
   ""
   (resultado-tablero resultado)
   (+ (estado-puntaje estado)
      (resultado-puntaje resultado))))

;----------------------------------------------------------
; Colores de baldosas
;----------------------------------------------------------

(define (color-baldosa valor)
  (cond
    ((= valor 0) "white")
    ((= valor 2) "lightyellow")
    ((= valor 4) "lightgoldenrod")
    ((= valor 8) "orange")
    ((= valor 16) "darkorange")
    ((= valor 32) "tomato")
    ((= valor 64) "red")
    ((= valor 128) "yellowgreen")
    ((= valor 256) "green")
    ((= valor 512) "lightblue")
    ((= valor 1024) "deepskyblue")
    ((= valor 2048) "gold")
    (else "plum")))

;----------------------------------------------------------
; Dibujar baldosa
;----------------------------------------------------------

(define (dibujar-baldosa valor)
  (cond
    ((= valor 0)
     (overlay
      (rectangle TAM-CELDA TAM-CELDA "outline" "gray")
      (rectangle TAM-CELDA TAM-CELDA "solid" (color-baldosa valor))))
    (else
     (overlay
      (text (number->string valor) 20 COLOR_TEXTO)
      (overlay
       (rectangle TAM-CELDA TAM-CELDA "outline" "gray")
       (rectangle TAM-CELDA TAM-CELDA "solid" (color-baldosa valor)))))))

;----------------------------------------------------------
; Espacios visuales
;----------------------------------------------------------

(define (espacio-horizontal)
  (rectangle ESPACIO TAM-CELDA "solid" COLOR-FONDO-TABLERO))

(define (espacio-vertical ancho)
  (rectangle ancho ESPACIO "solid" COLOR-FONDO-TABLERO))

;----------------------------------------------------------
; Dibujar fila
;----------------------------------------------------------

(define (dibujar-fila fila)
  (cond
    ((null? fila) empty-image)
    ((null? (cdr fila))
     (dibujar-baldosa (car fila)))
    (else
     (beside
      (dibujar-baldosa (car fila))
      (espacio-horizontal)
      (dibujar-fila (cdr fila))))))

;----------------------------------------------------------
; Ancho visual de fila
;----------------------------------------------------------

(define (ancho-fila n)
  (cond
    ((= n 0) 0)
    ((= n 1) TAM-CELDA)
    (else (+ TAM-CELDA ESPACIO (ancho-fila (- n 1))))))

;----------------------------------------------------------
; Dibujar tablero
;----------------------------------------------------------

(define (dibujar-tablero tablero)
  (cond
    ((null? tablero) empty-image)
    ((null? (cdr tablero))
     (dibujar-fila (car tablero)))
    (else
     (above
      (dibujar-fila (car tablero))
      (espacio-vertical (ancho-fila (largo-lista (car tablero))))
      (dibujar-tablero (cdr tablero))))))

;----------------------------------------------------------
; Fondo del tablero
;----------------------------------------------------------

(define (envolver-tablero tablero)
  (overlay
   (dibujar-tablero tablero)
   (rectangle
    (+ (ancho-fila (largo-lista (car tablero))) 20)
    (+ (* (largo-lista tablero) TAM-CELDA)
       (* (- (largo-lista tablero) 1) ESPACIO)
       20)
    "solid"
    COLOR-FONDO-TABLERO)))

;----------------------------------------------------------
; Mensaje de estado
;----------------------------------------------------------

(define (mensaje-estado tablero)
  (cond
    ((equal? (estado-juego tablero) 'ganado)
     (text "¡Ganaste!" 24 "darkgreen"))
    ((equal? (estado-juego tablero) 'perdido)
     (text "Juego terminado: perdiste" 24 "red"))
    (else
     (text "Usa las flechas del teclado" 18 "black"))))

;----------------------------------------------------------
; Dibujar pantalla de menu
;----------------------------------------------------------

(define (dibujar-menu estado)
  (above
   (text "2048" 36 "black")
   (text "Ingrese el tamaño del tablero: filas columnas" 18 "black")
   (text "Ejemplo: 4 6" 18 "gray")
   (overlay
    (text (estado-entrada estado) 24 "blue")
    (rectangle 220 40 "outline" "black"))
   (text "Minimo 4x4 y maximo 10x10" 16 "gray")
   (text "Presione Enter para iniciar" 16 "gray")))

;----------------------------------------------------------
; Dibujar pantalla de juego
;----------------------------------------------------------

(define (dibujar-juego estado)
  (above
   (text "2048" 32 "black")
   (text
    (string-append "Puntaje: " (number->string (estado-puntaje estado)))
    20
    "black")
   (mensaje-estado (estado-tablero estado))
   (envolver-tablero (estado-tablero estado))
   (text "Presiona r para reiniciar" 16 "gray")))

;----------------------------------------------------------
; Dibujar segun modo
;----------------------------------------------------------

(define (dibujar-estado estado)
  (cond
    ((equal? (estado-modo estado) 'menu)
     (centrar-en-ventana (dibujar-menu estado)))
    (else
     (centrar-en-ventana (dibujar-juego estado)))))

;----------------------------------------------------------
; Caracter valido para entrada
;----------------------------------------------------------

(define (tecla-valida-menu? tecla)
  (cond
    ((string=? tecla "0") #t)
    ((string=? tecla "1") #t)
    ((string=? tecla "2") #t)
    ((string=? tecla "3") #t)
    ((string=? tecla "4") #t)
    ((string=? tecla "5") #t)
    ((string=? tecla "6") #t)
    ((string=? tecla "7") #t)
    ((string=? tecla "8") #t)
    ((string=? tecla "9") #t)
    ((string=? tecla " ") #t)
    (else #f)))

;----------------------------------------------------------
; Agregar caracter a la entrada
;----------------------------------------------------------

(define (agregar-a-entrada estado tecla)
  (crear-estado
   'menu
   (string-append (estado-entrada estado) tecla)
   '()
   0))

;----------------------------------------------------------
; Borrar ultimo caracter
;----------------------------------------------------------

(define (borrar-ultimo-caracter texto)
  (cond
    ((= (string-length texto) 0) "")
    (else (substring texto 0 (- (string-length texto) 1)))))

(define (borrar-entrada estado)
  (crear-estado
   'menu
   (borrar-ultimo-caracter (estado-entrada estado))
   '()
   0))

;----------------------------------------------------------
; Separar texto en filas y columnas
; Se espera formato "m n"
;----------------------------------------------------------

(define (intentar-crear-juego texto)
  (crear-juego-desde-lista
   (string-split texto)))

(define (crear-juego-desde-lista partes)
  (cond
    ((not (= (length partes) 2)) #f)
    ((not (string->number (car partes))) #f)
    ((not (string->number (car (cdr partes)))) #f)
    ((dimension-valida? (string->number (car partes))
                        (string->number (car (cdr partes))))
     (crear-estado-juego
      (string->number (car partes))
      (string->number (car (cdr partes)))))
    (else #f)))

;----------------------------------------------------------
; Manejo de teclas en menu
;----------------------------------------------------------

(define (manejar-tecla-menu estado tecla)
  (cond
    ((string=? tecla "\b")
     (borrar-entrada estado))
    ((string=? tecla "\r")
     (cond
       ((intentar-crear-juego (estado-entrada estado))
        (intentar-crear-juego (estado-entrada estado)))
       (else estado)))
    ((tecla-valida-menu? tecla)
     (agregar-a-entrada estado tecla))
    (else estado)))

;----------------------------------------------------------
; Reiniciar juego con mismo tamaño
;----------------------------------------------------------

(define (reiniciar-estado estado)
  (crear-estado
   'juego
   ""
   (crear-tablero-inicial
    (largo-lista (estado-tablero estado))
    (largo-lista (car (estado-tablero estado))))
   0))

;----------------------------------------------------------
; Manejo de teclas en juego
;----------------------------------------------------------

(define (manejar-tecla-juego estado tecla)
  (cond
    ((string=? tecla "r")
     (reiniciar-estado estado))
    ((juego-finalizado? (estado-tablero estado))
     estado)
    ((string=? tecla "left")
     (actualizar-estado-con-resultado
      estado
      (jugar-izquierda-con-puntaje (estado-tablero estado))))
    ((string=? tecla "right")
     (actualizar-estado-con-resultado
      estado
      (jugar-derecha-con-puntaje (estado-tablero estado))))
    ((string=? tecla "up")
     (actualizar-estado-con-resultado
      estado
      (jugar-arriba-con-puntaje (estado-tablero estado))))
    ((string=? tecla "down")
     (actualizar-estado-con-resultado
      estado
      (jugar-abajo-con-puntaje (estado-tablero estado))))
    (else estado)))

;----------------------------------------------------------
; Manejo general de teclado
;----------------------------------------------------------

(define (manejar-tecla estado tecla)
  (cond
    ((equal? (estado-modo estado) 'menu)
     (manejar-tecla-menu estado tecla))
    (else
     (manejar-tecla-juego estado tecla))))

;----------------------------------------------------------
; Iniciar programa
;----------------------------------------------------------

(define (iniciar-2048)
  (big-bang
   (crear-estado-menu)
   (on-key manejar-tecla)
   (to-draw dibujar-estado)))

(iniciar-2048)
