## ENGLISH
## Put comments here that give an overall description of what your
## functions do

## SPANISH
## Escribe comentarios que den una descripción global de lo que tu
## función hace

## Normalmente el obtener la inversa de una matriz es un proceso computacional
## costoso.

## La siguiente función creará una matriz especial que almacena en el caché
## su Inversa

makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  set <- function(y) {
    x <<- y
    inversa <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) mi <<- inverse
  getinv <- function() mi
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Ahora la siguiente funcion obtiene la matriz especial creada por
## la funcion makeCacheMatrix (La funcion creada arriba)
## Entonces si la inversa de la matriz fue calculada, suponiendo que
## no hay cambios en la matriz, entonces debería mostrarnos la inversa
## que tiene guardada en el caché.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mi <- x$getinv()
  if (!is.null(mi)) {
    message("Obteniendo datos en caché")  ##Si hay datos en el cache mostrará este mensaje
    return(mi) ## Nos dará el resultado.
  }
  mi <- solve(x$get(), ...)
  x$setinv(mi)
  mi ##Nos devuelve el valor "inversa" que es el resultado final.
}

##PRUEBAS REALIZADAS

#> source("cachematrix.R")

#> matriz <- makeCacheMatrix(matrix(rnorm(4), 2, 2))

#> matriz$get()
#           [,1]      [,2]
#[1,]  0.5178547 0.6422707
#[2,] -1.1762520 0.1879743

#> matriz$getinv()
#NULL

#> cacheSolve(matriz)
#           [,1]      [,2]
#[1,] 0.2204161 -0.7531179  ??? Por ahora no hay datos en caché
#[2,] 1.3792572  0.6072294

#> cacheSolve(matriz)
#Obteniendo datos en caché
#           [,1]      [,2]
#[1,] 0.2204161 -0.7531179 ??? Al haber datos en el caché se muestra el mensaje y
#[2,] 1.3792572  0.6072294   se muestra automáticamente el resultado.