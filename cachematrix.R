## The makeCacheMatrix function creates an extended matrix object that allows to retrieve its inversion.
## The inversion of the matrix is only computed the first time it is requested by a client and is cached afterwards. The caching
## functionality is encapsulated in the makeCacheMatrix object. Clients only request the inverted value of the cacheMatrix, the
## process of caching is transparent for them.

## cacheSolve acts as a client to the makeCacheMatrix object. It does not actually know that the inversion is cached, therefore
## the implementation is rather short.

#' This method creates a matrix that allows to also retrieve the inverted matrix. The inverted matrix will be lazily computed
#' the first time the inverted value is requested and will be cached on subsequent requests
#' @param original any invertible matrix
#' @return caching enabled matrix
makeCacheMatrix <- function(original = matrix()) {
    ## initialize the inverted matrix as NULL
    inversion <- NULL
    
    ## Reset the value of the matrix and empty the cache
    set <- function(y) {
        original <<- y
        ## clear cache
        inversion <<- NULL
        
    }
    
    ## retrieve the matrix value
    get <- function() original
    
    ## Does the inversion and returns the inverted matrix. First call will calculate and cache the value.
    invert <- function(){
        ## the inversion value is NULL on the first call. -> Calculate and store in cache
        if(is.null(inversion)) {
            message("calculating the inverted matrix")
            inversion <<- solve(original)  
        }
        return(inversion)
    }
    
    ## create function signature
    list(set = set, get = get, invert = invert)
}


#' This function will return the inversion of any invertible matrix. The function will only 
#' compute the inverted matrix the first time the function is called for any matrix and will cache the inverted matrix.
#' Any subsequent calls will return the cached value.
#' @param x any caching enable matrix created by makeCacheMatrix
#' @return the inverted value of the cacheMatrix
cacheSolve <- function(x, ...) {
    # loading inverted matrix
    x$invert()
}