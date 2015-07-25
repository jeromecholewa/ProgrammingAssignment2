## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# function creating an object (list of functions)
# associated with the matrix x (argument)
makeCacheMatrix <- function(x = matrix()) {

        # initializes the inverse variable inv to NULL
        inv <- NULL

        # sets the cached matrix x to a new matrix y and clear the memory of
        # the inverse of x, if it had been calculated before
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }

        # this method return x, that is the initial matrix
        get <- function() x

        #sets the inverse variable inv to "inverse" (will be used in the
        # cacheSolve function)
        setInverse <- function(inverse) inv <<- inverse

        #gets the value currently stored in "inv"
        getInverse <- function() inv


        # the object returned by that function is a list of the
        # functions above
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


# That function cacheSolve checks whether the object x (cached
# copy of the matrix x) already had its inverse calculated
# before and stored. If yes, it gives that value, without recalculating
# if not, it calculates the inverse and then stores the value
# in the cache.
cacheSolve <- function(x, ...) {

        inv <- x$getInverse()

        # checks if x already had its inverse calculated
        if(!is.null(inv)) {
                message("getting cached data")
                #if so, it just returns the value and we exit the function
                return(inv)
        }

        data <- x$get()  #x[[2]]()  would be equivalent

        ## Return a matrix that is the inverse of 'x'
        #THAT IS WHERE ALL THE CALCULATION HAPPENS
        inv <- solve(data, ...)

        #stores the inverse (inv) into the "inv" variable of
        # the cached copy of x.
        x$setInverse(inv) # x[[3]](inv) would be equivalent

        #returns the newly calculated inverse of the matrix x
        inv
}
