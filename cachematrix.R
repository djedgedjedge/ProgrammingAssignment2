## To create a matrix 
## functions do

## Function that defines accessors for a special matrix.  
## For the "set" function, it will check whether both matrices (x and y) are similar.
## It will update x and its inversed only if the matrices are different

makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL
        set <- function(y) {
                if (!(is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y))){
                        print(is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y))
                        print(y)
                        x <<- y
                        inv_matrix <<- NULL
                }
        }
        get <- function() x
        setInv <- function(inv_mat) inv_matrix <<- inv_mat
        getInv <- function() inv_matrix
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Check whether the special matrix has an inversed matrix already computed
## if yes, it will get directly the inversed matric from cache
## if no, it will calculate the inversed matrix and put it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_matrix <- x$getInv()
        if(!is.null(inv_matrix)) {
                message("getting cached data")
                return(inv_matrix)
        }
        data <- x$get()
        inv_matrix <- solve(data, ...)
        x$setInv(inv_matrix)
        inv_matrix
}
