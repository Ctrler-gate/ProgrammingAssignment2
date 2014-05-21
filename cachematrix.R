## This function creates a special matrix that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
    matrix_i <- NULL
    set <- function(y){
        matri <<- y
        matrix_i <<- NULL
    }
    get <- function() matri
    set_inverse <- function(inverse) matrix_i <<- inverse
    get_inverse <- function() matrix_i
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function computes the inverse of a square matrix
## If the inverse has already computed, it simply load from the cache

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$get_inverse()
    if (!is.null(inverse)){
        message ("Loading cache data...!")
        return (inverse)
    }
    mat <- x$get()
    if(dim(mat)[1] != dim(mat)[2]){
        matrix_i <<- c("Does not exist!!")
        return (matrix_i)
    }
    mat_i <- solve(mat)
    x$set_inverse(mat_i)
    mat_i
}
