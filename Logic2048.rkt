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
         quitar-primera-columna
         transponer-tablero
         mover-tablero-arriba
         mover-tablero-abajo
         listas-iguales?
         tableros-iguales?
         tablero-cambio?
         crear-posicion
         posicion-fila
         posicion-columna
         posiciones-vacias-fila
         posiciones-vacias-tablero
         seleccionar-posicion-aleatoria
         generar-baldosa-aleatoria
         insertar-baldosa-en-posicion
         insertar-baldosa-aleatoria
         actualizar-tablero-despues-movimiento
         jugar-izquierda
         jugar-derecha
         jugar-arriba
         jugar-abajo
         crear-tablero-inicial
         insertar-dos-iniciales
         contar-valor-en-fila
         contar-valor-en-tablero
         hay-movimiento-izquierda?
         hay-movimiento-derecha?
         hay-movimiento-arriba?
         hay-movimiento-abajo?
         hay-movimientos-posibles?
         juego-ganado?
         juego-perdido?
         juego-finalizado?
         estado-juego
         puntaje-fila-izquierda
         puntaje-tablero-izquierda
         puntaje-tablero-derecha
         puntaje-tablero-arriba
         puntaje-tablero-abajo
         crear-resultado-jugada
         resultado-tablero
         resultado-puntaje
         jugar-izquierda-con-puntaje
         jugar-derecha-con-puntaje
         jugar-arriba-con-puntaje
         jugar-abajo-con-puntaje)

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
;----------------------------------------------------------

(define (crear-fila n)
  (cond
    ((= n 0) '())
    (else (cons 0 (crear-fila (- n 1))))))

;----------------------------------------------------------
; Crear tablero vacío
;----------------------------------------------------------

(define (crear-tablero-vacio m n)
  (cond
    ((not (dimension-valida? m n)) '())
    (else (crear-tablero-aux m n))))

(define (crear-tablero-aux m n)
  (cond
    ((= m 0) '())
    (else
     (cons (crear-fila n)
           (crear-tablero-aux (- m 1) n)))))

;----------------------------------------------------------
; Obtener elemento por índice
;----------------------------------------------------------

(define (obtener-elemento lista indice)
  (cond
    ((null? lista) '())
    ((= indice 0) (car lista))
    (else (obtener-elemento (cdr lista) (- indice 1)))))

;----------------------------------------------------------
; Reemplazar elemento por índice
;----------------------------------------------------------

(define (reemplazar-elemento lista indice valor)
  (cond
    ((null? lista) '())
    ((= indice 0) (cons valor (cdr lista)))
    (else (cons (car lista)
                (reemplazar-elemento (cdr lista) (- indice 1) valor)))))

;----------------------------------------------------------
; Obtener celda
;----------------------------------------------------------

(define (obtener-celda tablero fila columna)
  (obtener-elemento (obtener-elemento tablero fila) columna))

;----------------------------------------------------------
; Colocar celda
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
; Contar vacíos en el tablero
;----------------------------------------------------------

(define (contar-vacios-tablero tablero)
  (cond
    ((null? tablero) 0)
    (else (+ (contar-ceros-fila (car tablero))
             (contar-vacios-tablero (cdr tablero))))))

;----------------------------------------------------------
; Buscar valor en fila
;----------------------------------------------------------

(define (existe-valor-en-fila? fila valor)
  (cond
    ((null? fila) #f)
    ((= (car fila) valor) #t)
    (else (existe-valor-en-fila? (cdr fila) valor))))

;----------------------------------------------------------
; Buscar valor en tablero
;----------------------------------------------------------

(define (existe-valor? tablero valor)
  (cond
    ((null? tablero) #f)
    ((existe-valor-en-fila? (car tablero) valor) #t)
    (else (existe-valor? (cdr tablero) valor))))

;----------------------------------------------------------
; Detectar victoria
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

;----------------------------------------------------------
; Transponer tablero
;----------------------------------------------------------

(define (transponer-tablero tablero)
  (cond
    ((null? tablero) '())
    ((null? (car tablero)) '())
    (else
     (cons (primera-columna tablero)
           (transponer-tablero (quitar-primera-columna tablero))))))

;----------------------------------------------------------
; Mover tablero hacia arriba
;----------------------------------------------------------

(define (mover-tablero-arriba tablero)
  (transponer-tablero
   (mover-tablero-izquierda
    (transponer-tablero tablero))))

;----------------------------------------------------------
; Mover tablero hacia abajo
;----------------------------------------------------------

(define (mover-tablero-abajo tablero)
  (transponer-tablero
   (mover-tablero-derecha
    (transponer-tablero tablero))))

;----------------------------------------------------------
; Verificar si dos listas son iguales elemento por elemento
;----------------------------------------------------------

(define (listas-iguales? lista1 lista2)
  (cond
    ((and (null? lista1) (null? lista2)) #t)
    ((null? lista1) #f)
    ((null? lista2) #f)
    ((equal? (car lista1) (car lista2))
     (listas-iguales? (cdr lista1) (cdr lista2)))
    (else #f)))

;----------------------------------------------------------
; Verificar si dos tableros son iguales fila por fila
;----------------------------------------------------------

(define (tableros-iguales? tablero1 tablero2)
  (cond
    ((and (null? tablero1) (null? tablero2)) #t)
    ((null? tablero1) #f)
    ((null? tablero2) #f)
    ((listas-iguales? (car tablero1) (car tablero2))
     (tableros-iguales? (cdr tablero1) (cdr tablero2)))
    (else #f)))

;----------------------------------------------------------
; Verificar si el tablero cambió
;----------------------------------------------------------

(define (tablero-cambio? tablero-original tablero-nuevo)
  (cond
    ((tableros-iguales? tablero-original tablero-nuevo) #f)
    (else #t)))

;----------------------------------------------------------
; Crear una posición como lista de dos elementos
;----------------------------------------------------------

(define (crear-posicion fila columna)
  (cons fila (cons columna '())))

(define (posicion-fila posicion)
  (car posicion))

(define (posicion-columna posicion)
  (car (cdr posicion)))

;----------------------------------------------------------
; Obtener posiciones vacías de una fila
;----------------------------------------------------------

(define (posiciones-vacias-fila fila indice-fila indice-columna)
  (cond
    ((null? fila) '())
    ((= (car fila) 0)
     (cons (crear-posicion indice-fila indice-columna)
           (posiciones-vacias-fila (cdr fila)
                                   indice-fila
                                   (+ indice-columna 1))))
    (else
     (posiciones-vacias-fila (cdr fila)
                             indice-fila
                             (+ indice-columna 1)))))

;----------------------------------------------------------
; Obtener todas las posiciones vacías del tablero
;----------------------------------------------------------

(define (posiciones-vacias-tablero-aux tablero indice-fila)
  (cond
    ((null? tablero) '())
    (else
     (append
      (posiciones-vacias-fila (car tablero) indice-fila 0)
      (posiciones-vacias-tablero-aux (cdr tablero) (+ indice-fila 1))))))

(define (posiciones-vacias-tablero tablero)
  (posiciones-vacias-tablero-aux tablero 0))

;----------------------------------------------------------
; Seleccionar una posición vacía aleatoria
;----------------------------------------------------------

(define (seleccionar-posicion-aleatoria posiciones)
  (obtener-elemento posiciones
                    (random (largo-lista posiciones))))

;----------------------------------------------------------
; Generar una baldosa aleatoria: 2 o 4
;----------------------------------------------------------

(define (generar-baldosa-aleatoria)
  (cond
    ((= (random 2) 0) 2)
    (else 4)))

;----------------------------------------------------------
; Insertar una baldosa en una posición específica
;----------------------------------------------------------

(define (insertar-baldosa-en-posicion tablero posicion valor)
  (colocar-celda tablero
                 (posicion-fila posicion)
                 (posicion-columna posicion)
                 valor))

;----------------------------------------------------------
; Insertar una baldosa aleatoria en un espacio vacío
;----------------------------------------------------------

(define (insertar-baldosa-aleatoria tablero)
  (cond
    ((= (contar-vacios-tablero tablero) 0) tablero)
    (else
     (insertar-baldosa-en-posicion
      tablero
      (seleccionar-posicion-aleatoria
       (posiciones-vacias-tablero tablero))
      (generar-baldosa-aleatoria)))))

;----------------------------------------------------------
; Actualizar el tablero después de un movimiento
; Solo inserta nueva baldosa si el tablero cambió
;----------------------------------------------------------

(define (actualizar-tablero-despues-movimiento tablero-original tablero-movido)
  (cond
    ((tablero-cambio? tablero-original tablero-movido)
     (insertar-baldosa-aleatoria tablero-movido))
    (else tablero-original)))

;----------------------------------------------------------
; Jugadas completas con inserción de nueva baldosa
;----------------------------------------------------------

(define (jugar-izquierda tablero)
  (actualizar-tablero-despues-movimiento
   tablero
   (mover-tablero-izquierda tablero)))

(define (jugar-derecha tablero)
  (actualizar-tablero-despues-movimiento
   tablero
   (mover-tablero-derecha tablero)))

(define (jugar-arriba tablero)
  (actualizar-tablero-despues-movimiento
   tablero
   (mover-tablero-arriba tablero)))

(define (jugar-abajo tablero)
  (actualizar-tablero-despues-movimiento
   tablero
   (mover-tablero-abajo tablero)))

;----------------------------------------------------------
; Insertar una baldosa de valor específico en posición aleatoria
;----------------------------------------------------------

(define (insertar-valor-aleatorio tablero valor)
  (cond
    ((= (contar-vacios-tablero tablero) 0) tablero)
    (else
     (insertar-baldosa-en-posicion
      tablero
      (seleccionar-posicion-aleatoria
       (posiciones-vacias-tablero tablero))
      valor))))

;----------------------------------------------------------
; Insertar las dos baldosas iniciales con valor 2
;----------------------------------------------------------

(define (insertar-dos-iniciales tablero)
  (insertar-valor-aleatorio
   (insertar-valor-aleatorio tablero 2)
   2))

;----------------------------------------------------------
; Crear tablero inicial del juego
;----------------------------------------------------------

(define (crear-tablero-inicial m n)
  (cond
    ((not (dimension-valida? m n)) '())
    (else
     (insertar-dos-iniciales
      (crear-tablero-vacio m n)))))

;----------------------------------------------------------
; Contar cuántas veces aparece un valor en una fila
;----------------------------------------------------------

(define (contar-valor-en-fila fila valor)
  (cond
    ((null? fila) 0)
    ((= (car fila) valor)
     (+ 1 (contar-valor-en-fila (cdr fila) valor)))
    (else
     (contar-valor-en-fila (cdr fila) valor))))

;----------------------------------------------------------
; Contar cuántas veces aparece un valor en todo el tablero
;----------------------------------------------------------

(define (contar-valor-en-tablero tablero valor)
  (cond
    ((null? tablero) 0)
    (else
     (+ (contar-valor-en-fila (car tablero) valor)
        (contar-valor-en-tablero (cdr tablero) valor)))))

;----------------------------------------------------------
; Verificar si un movimiento en una dirección cambia el tablero
;----------------------------------------------------------

(define (hay-movimiento-izquierda? tablero)
  (tablero-cambio? tablero
                   (mover-tablero-izquierda tablero)))

(define (hay-movimiento-derecha? tablero)
  (tablero-cambio? tablero
                   (mover-tablero-derecha tablero)))

(define (hay-movimiento-arriba? tablero)
  (tablero-cambio? tablero
                   (mover-tablero-arriba tablero)))

(define (hay-movimiento-abajo? tablero)
  (tablero-cambio? tablero
                   (mover-tablero-abajo tablero)))

;----------------------------------------------------------
; Verificar si existe al menos un movimiento posible
;----------------------------------------------------------

(define (hay-movimientos-posibles? tablero)
  (cond
    ((hay-movimiento-izquierda? tablero) #t)
    ((hay-movimiento-derecha? tablero) #t)
    ((hay-movimiento-arriba? tablero) #t)
    ((hay-movimiento-abajo? tablero) #t)
    (else #f)))

;----------------------------------------------------------
; Verificar si el juego fue ganado
;----------------------------------------------------------

(define (juego-ganado? tablero)
  (tablero-ganador? tablero))

;----------------------------------------------------------
; Verificar si el juego fue perdido
; Se pierde si no hay 2048 y no hay movimientos posibles
;----------------------------------------------------------

(define (juego-perdido? tablero)
  (cond
    ((juego-ganado? tablero) #f)
    ((hay-movimientos-posibles? tablero) #f)
    (else #t)))

;----------------------------------------------------------
; Verificar si el juego finalizó
;----------------------------------------------------------

(define (juego-finalizado? tablero)
  (cond
    ((juego-ganado? tablero) #t)
    ((juego-perdido? tablero) #t)
    (else #f)))

;----------------------------------------------------------
; Estado general del juego
;----------------------------------------------------------

(define (estado-juego tablero)
  (cond
    ((juego-ganado? tablero) 'ganado)
    ((juego-perdido? tablero) 'perdido)
    (else 'en-juego)))

;----------------------------------------------------------
; Calcular el puntaje que genera una fila al moverse a la izquierda
;----------------------------------------------------------

(define (puntaje-fila-izquierda fila)
  (puntaje-fila-sin-ceros-izquierda
   (quitar-ceros-fila fila)))

(define (puntaje-fila-sin-ceros-izquierda fila)
  (cond
    ((null? fila) 0)
    ((null? (cdr fila)) 0)
    ((= (car fila) (car (cdr fila)))
     (+ (+ (car fila) (car (cdr fila)))
        (puntaje-fila-sin-ceros-izquierda (cdr (cdr fila)))))
    (else
     (puntaje-fila-sin-ceros-izquierda (cdr fila)))))

;----------------------------------------------------------
; Calcular puntaje total de un tablero al moverse a la izquierda
;----------------------------------------------------------

(define (puntaje-tablero-izquierda tablero)
  (cond
    ((null? tablero) 0)
    (else
     (+ (puntaje-fila-izquierda (car tablero))
        (puntaje-tablero-izquierda (cdr tablero))))))

;----------------------------------------------------------
; Calcular puntaje total de un tablero al moverse a la derecha
;----------------------------------------------------------

(define (puntaje-tablero-derecha tablero)
  (cond
    ((null? tablero) 0)
    (else
     (+ (puntaje-fila-izquierda (invertir-lista (car tablero)))
        (puntaje-tablero-derecha (cdr tablero))))))

;----------------------------------------------------------
; Calcular puntaje total de un tablero al moverse hacia arriba
;----------------------------------------------------------

(define (puntaje-tablero-arriba tablero)
  (puntaje-tablero-izquierda
   (transponer-tablero tablero)))

;----------------------------------------------------------
; Calcular puntaje total de un tablero al moverse hacia abajo
;----------------------------------------------------------

(define (puntaje-tablero-abajo tablero)
  (puntaje-tablero-derecha
   (transponer-tablero tablero)))

;----------------------------------------------------------
; Crear resultado de jugada como lista:
; (tablero puntaje)
;----------------------------------------------------------

(define (crear-resultado-jugada tablero puntaje)
  (cons tablero (cons puntaje '())))

(define (resultado-tablero resultado)
  (car resultado))

(define (resultado-puntaje resultado)
  (car (cdr resultado)))

;----------------------------------------------------------
; Jugada completa a la izquierda con puntaje
; Si el tablero no cambia, el puntaje ganado es 0
;----------------------------------------------------------

(define (jugar-izquierda-con-puntaje tablero)
  (cond
    ((tablero-cambio? tablero (mover-tablero-izquierda tablero))
     (crear-resultado-jugada
      (actualizar-tablero-despues-movimiento
       tablero
       (mover-tablero-izquierda tablero))
      (puntaje-tablero-izquierda tablero)))
    (else
     (crear-resultado-jugada tablero 0))))

;----------------------------------------------------------
; Jugada completa a la derecha con puntaje
;----------------------------------------------------------

(define (jugar-derecha-con-puntaje tablero)
  (cond
    ((tablero-cambio? tablero (mover-tablero-derecha tablero))
     (crear-resultado-jugada
      (actualizar-tablero-despues-movimiento
       tablero
       (mover-tablero-derecha tablero))
      (puntaje-tablero-derecha tablero)))
    (else
     (crear-resultado-jugada tablero 0))))

;----------------------------------------------------------
; Jugada completa hacia arriba con puntaje
;----------------------------------------------------------

(define (jugar-arriba-con-puntaje tablero)
  (cond
    ((tablero-cambio? tablero (mover-tablero-arriba tablero))
     (crear-resultado-jugada
      (actualizar-tablero-despues-movimiento
       tablero
       (mover-tablero-arriba tablero))
      (puntaje-tablero-arriba tablero)))
    (else
     (crear-resultado-jugada tablero 0))))

;----------------------------------------------------------
; Jugada completa hacia abajo con puntaje
;----------------------------------------------------------

(define (jugar-abajo-con-puntaje tablero)
  (cond
    ((tablero-cambio? tablero (mover-tablero-abajo tablero))
     (crear-resultado-jugada
      (actualizar-tablero-despues-movimiento
       tablero
       (mover-tablero-abajo tablero))
      (puntaje-tablero-abajo tablero)))
    (else
     (crear-resultado-jugada tablero 0))))
