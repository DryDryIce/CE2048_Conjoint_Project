#lang racket

(provide dimension-valida?
         crear-fila
         crear-tablero-vacio
         largo-lista
         obtener-elemento
         reemplazar-elemento
         obtener-celda
         colocar-celda
         contar-ceros-fila
         contar-vacios-tablero
         existe-valor?
         tablero-ganador?
         quitar-ceros-fila
         combinar-fila-izquierda
         agregar-ceros-derecha
         mover-fila-izquierda
         mover-tablero-izquierda
         invertir-lista
         mover-fila-derecha
         mover-tablero-derecha
         primera-columna
         quitar-primera-columna)

;----------------------------------------------------------
; Validar dimensión
;----------------------------------------------------------

(define (dimension-valida? m n)
  (and (integer? m)
       (integer? n)
       (>= m 4)
       (<= m 10)
       (>= n 4)
       (<= n 10)))

;----------------------------------------------------------
; Largo de una lista
;----------------------------------------------------------

(define (largo-lista lista)
  (cond
    ((null? lista) 0)
    (else (+ 1 (largo-lista (cdr lista))))))

;----------------------------------------------------------
; Crear una fila de n ceros
; Ejemplo: (crear-fila 4) => '(0 0 0 0)
;----------------------------------------------------------

(define (crear-fila n)
  (cond
    ((<= n 0) '())
    (else (cons 0 (crear-fila (- n 1))))))

;----------------------------------------------------------
; Crear tablero vacío de m filas y n columnas
; Ejemplo: (crear-tablero-vacio 4 4)
;----------------------------------------------------------

(define (crear-tablero-vacio m n)
  (cond
    ((not (dimension-valida? m n))
     '())
    ((<= m 0) '())
    (else (cons (crear-fila n)
                (crear-tablero-vacio (- m 1) n)))))

;----------------------------------------------------------
; Obtener el elemento en posición índice de una lista
; índice empieza en 0
; Ejemplo: (obtener-elemento '(10 20 30) 1) => 20
;----------------------------------------------------------

(define (obtener-elemento lista indice)
  (cond
    ((null? lista) '())
    ((= indice 0) (car lista))
    (else (obtener-elemento (cdr lista) (- indice 1)))))

;----------------------------------------------------------
; Reemplazar el elemento en posición índice de una lista
; índice empieza en 0
; Ejemplo: (reemplazar-elemento '(1 2 3) 1 9) => '(1 9 3)
;----------------------------------------------------------

(define (reemplazar-elemento lista indice valor)
  (cond
    ((null? lista) '())
    ((= indice 0) (cons valor (cdr lista)))
    (else (cons (car lista)
                (reemplazar-elemento (cdr lista) (- indice 1) valor)))))

;----------------------------------------------------------
; Obtener una celda del tablero
; fila y columna empiezan en 0
;----------------------------------------------------------

(define (obtener-celda tablero fila columna)
  (obtener-elemento (obtener-elemento tablero fila) columna))

;----------------------------------------------------------
; Colocar un valor en una celda del tablero
; fila y columna empiezan en 0
;----------------------------------------------------------

(define (colocar-celda tablero fila columna valor)
  (reemplazar-elemento
   tablero
   fila
   (reemplazar-elemento
    (obtener-elemento tablero fila)
    columna
    valor)))

;----------------------------------------------------------
; Contar ceros en una fila
;----------------------------------------------------------

(define (contar-ceros-fila fila)
  (cond
    ((null? fila) 0)
    ((= (car fila) 0) (+ 1 (contar-ceros-fila (cdr fila))))
    (else (contar-ceros-fila (cdr fila)))))

;----------------------------------------------------------
; Contar espacios vacíos en todo el tablero
;----------------------------------------------------------

(define (contar-vacios-tablero tablero)
  (cond
    ((null? tablero) 0)
    (else (+ (contar-ceros-fila (car tablero))
             (contar-vacios-tablero (cdr tablero))))))

;----------------------------------------------------------
; Verificar si un valor existe en una fila
;----------------------------------------------------------

(define (existe-valor-en-fila? fila valor)
  (cond
    ((null? fila) #f)
    ((= (car fila) valor) #t)
    (else (existe-valor-en-fila? (cdr fila) valor))))

;----------------------------------------------------------
; Verificar si un valor existe en todo el tablero
;----------------------------------------------------------

(define (existe-valor? tablero valor)
  (cond
    ((null? tablero) #f)
    ((existe-valor-en-fila? (car tablero) valor) #t)
    (else (existe-valor? (cdr tablero) valor))))

;----------------------------------------------------------
; Detectar si ya se ganó
;----------------------------------------------------------

(define (tablero-ganador? tablero)
  (existe-valor? tablero 2048))

;----------------------------------------------------------
; Quitar ceros de una fila
;----------------------------------------------------------

(define (quitar-ceros-fila fila)
  (cond
    ((null? fila) '())
    ((= (car fila) 0)
     (quitar-ceros-fila (cdr fila)))
    (else
     (cons (car fila)
           (quitar-ceros-fila (cdr fila))))))

;----------------------------------------------------------
; Combinar una fila ya sin ceros hacia la izquierda
;----------------------------------------------------------

(define (combinar-fila-izquierda fila)
  (cond
    ((null? fila) '())
    ((null? (cdr fila)) fila)
    ((= (car fila) (car (cdr fila)))
     (cons (+ (car fila) (car (cdr fila)))
           (combinar-fila-izquierda (cdr (cdr fila)))))
    (else
     (cons (car fila)
           (combinar-fila-izquierda (cdr fila))))))


;----------------------------------------------------------
; Agregar ceros a la derecha hasta alcanzar largo-final
;----------------------------------------------------------

(define (agregar-ceros-derecha fila largo-final)
  (cond
    ((= (largo-lista fila) largo-final) fila)
    (else (agregar-ceros-derecha (append fila '(0)) largo-final))))

;----------------------------------------------------------
; Mover una fila hacia la izquierda
;----------------------------------------------------------

(define (mover-fila-izquierda fila)
  (agregar-ceros-derecha
   (combinar-fila-izquierda
    (quitar-ceros-fila fila))
   (largo-lista fila)))

;----------------------------------------------------------
; Aplicar movimiento a todo el tablero hacia la izquierda
;----------------------------------------------------------

(define (mover-tablero-izquierda tablero)
  (cond
    ((null? tablero) '())
    (else
     (cons (mover-fila-izquierda (car tablero))
           (mover-tablero-izquierda (cdr tablero))))))

;----------------------------------------------------------
; Invertir una lista
;----------------------------------------------------------

(define (invertir-lista lista)
  (cond
    ((null? lista) '())
    (else (append (invertir-lista (cdr lista))
                  (cons (car lista) '())))))

;----------------------------------------------------------
; Mover una fila hacia la derecha
;----------------------------------------------------------

(define (mover-fila-derecha fila)
  (invertir-lista
   (mover-fila-izquierda
    (invertir-lista fila))))

;----------------------------------------------------------
; Aplicar movimiento a todo el tablero hacia la derecha
;----------------------------------------------------------

(define (mover-tablero-derecha tablero)
  (cond
    ((null? tablero) '())
    (else
     (cons (mover-fila-derecha (car tablero))
           (mover-tablero-derecha (cdr tablero))))))

;----------------------------------------------------------
; Obtener la primera columna del tablero
;----------------------------------------------------------

(define (primera-columna tablero)
  (cond
    ((null? tablero) '())
    (else
     (cons (car (car tablero))
           (primera-columna (cdr tablero))))))

;----------------------------------------------------------
; Quitar la primera columna del tablero
;----------------------------------------------------------

(define (quitar-primera-columna tablero)
  (cond
    ((null? tablero) '())
    (else
     (cons (cdr (car tablero))
           (quitar-primera-columna (cdr tablero))))))
