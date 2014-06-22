##These functions are based on Programming assignment 2 for the 
##Coursera R programming course.
##
##The original file was forked from the github repository at  
##  https://github.com/rdpeng/ProgrammingAssignment2 on 6/192014
##The coded below is based on the example give in the readme.md 
##for caching and returning the mean.

## This file contains 2 functions
##makeCacheMatrix - which, has getters and setters for a matrix and its inverse 
##                  (Note: the matrix is assumed to be invertable)
##cacheSolve - function that checks to see if a matrix has been set and the 
##            value of its incerse has been calculated and cached, 
##            if it has been cached return it, other wise calculate and store it
##example:
## > my_matrix<-diag(4) #set identity matrix
## > my_matrix[1,3]<-5 # set at least one value not identity so can test inverse 
##                      is returned
## > mcm<-makeCacheMatrix(my_matrix)
## > cacheSolve(mcm)
## [,1] [,2] [,3] [,4]
## [1,]    1    0   -5    0
## [2,]    0    1    0    0
## [3,]    0    0    1    0
## [4,]    0    0    0    1


## makeCache is used to get and set the values of an invertible matrix
## make cache is used to cache (store) the value of the inverse matrix and 
## return it
makeCacheMatrix <- function(x = matrix()) {
   
          m <- NULL
          set <- function(y) {
               x <<- y
               m <<- NULL
          }
          get <- function() x
          setsolve <- function(solve) m <<- solve
          getsolve <- function() m
          list(set = set, get = get,
               setsolve = setsolve,
               getsolve = getsolve)
}


##cacheSolve-returns the inverse of the matrix set with makeCacheMatrix
##Note first run makeCacheMtrix to generate the matrix with the attached getters 
##and setters
##use the pro

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of the matrix set with 
     ##makeCacheMatrix
    
          #check to see if inverse has been calculated and stored "cached"
          m <- x$getsolve()
          if(!is.null(m)) {
               message("getting cached data")
               return(m)
          }
          #if the inverse (solve) value is not cached, calculate and store it for future use
          data <- x$get()
          m <- solve(data, ...)
          x$setsolve(m)
          m
}
