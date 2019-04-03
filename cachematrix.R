## The first makeCacheMatrix function takes a matrix array and outputs a
## custom object (list) that includes the matrix and it's inverse that can 
## be inputted into the cacheSolve function.
## The cacheSolve function checks if the inverse of the matrix has been
## cached. If not, it recalculates the inverse of the original matrix.


## makeCacheMatrix takes an input of a matrix array and returns a list
## of the matrix and the inverse of the matrix, cached. It also includes a
## backup of the matrix, assuming makeCacheMatrix$matrix can be edited.
## This allows the backup (assumed not to be edited) to serve as a checking
## mechanism to verify if the inverse needs to be recalculated.

makeCacheMatrix <- function(x = matrix()) {
        mat <- x
        inv <- solve(x)
        backup <- mat
        matt <- list(matrix = mat, inverse = inv, backup = backup)
}


## cacheSolve takes the output of makeCacheMatrix and returns the inverse
## of the given matrix. If the x$matrix and x$backup are identical and 
## x$inverse is not null, then it retrieves the cached inverse. If x$inverse
## is null or if x$matrix has been altered to differ from x$backup, the
## function recalculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
        inv <- x$inverse
        if(is.null(inv) || x$matrix != x$backup){
                print("no cached inverse, calculating inverse...")
                inverse <- solve(x$matrix)
                print(inverse)
        }else if(!is.null(inv) && x$matrix == x$backup){
                print("retrieving cached inverse")
                inverse <- inv
                print(inverse)
        }
}
