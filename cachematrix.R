## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Initially, we created the constructor function "makeCacheMatrix" that caches calculated values or inputs. 
#Then we create the variable inv and set it to NULL by default.In addition, we created an object with 4 
#objects attached to it, the value of the array being accessible with the get function of the same object, 
#defining a function that can change the cached value of the array, obtaining the inverse function to obtain 
#the inverse in the array's cache, which sets its value to NULL every time we assign a new array and set the 
#inverse to gain access to the object via the cacheSolve function. Finally, the << - operator in both defined 
#functions attached to the object, is used to assign values in the parent frame outside the function environment
#created by the makeCacheMatrix function.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        get <- function() x
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getinv <- function() inv
        setinv <- function(inverse) {
                inv <<- inverse
        }
        return(list(
                set = set,
                get = get,
                getinverse = getinv,
                setinverse = setinv
        ))
}


## Write a short comment describing this function
#With respect to the cacheSolve function, obtain access to the object's inverse matrix with the operator
#<- which "searches in the function environment and then searches one level up in the parent environment"
#and returns the cached inverse matrix without computing again, of course, either computes the inverse
#otherwise for a first input matrix or if the matrix was changed with the makeCacheMatrix $ set function.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
                return(inverse)
        }
        m <- solve(x$get())
        x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'
}
