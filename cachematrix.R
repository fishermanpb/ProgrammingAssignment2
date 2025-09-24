makeCacheMatrix <- function(x = matrix()) {
 # Inicializar la inversa como NULL
    inv <- NULL
    
    # Función para establecer la matriz
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Resetear la caché cuando la matriz cambia
    }
    
    # Función para obtener la matriz
    get <- function() x
    
    # Función para establecer la inversa
    setinverse <- function(inverse) inv <<- inverse
    
    # Función para obtener la inversa
    getinverse <- function() inv
    
    # Retornar lista de funciones
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}




cacheSolve <- function(x, ...) {
 # Intentar obtener la inversa de la caché
    inv <- x$getinverse()
    
    # Si la inversa está en caché, retornarla
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    
    # Si no está en caché, calcular la inversa
    data <- x$get()           # Obtener la matriz
    inv <- solve(data, ...)   # Calcular la inversa
    x$setinverse(inv)         # Almacenar en caché
    
    # Retornar la inversa calculada
    inv
 }
