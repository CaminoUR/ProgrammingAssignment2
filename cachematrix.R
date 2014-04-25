
## PROGRAMMING ASSIGNMENT 2: CACHING THE INVERSE OF A MATRIX

## We define the 'MakeCacheMatrix' function as a function of 'x', where 'x' is a square invertible matrix. 
## MakeCacheMatrix returns a list of 4 components. This components are the following 4 functions: set, get, setsolve and getsolve.

MakeCacheMatrix <- function(x = matrix()) 
	
	{
  
	## We initialize 's' in the current environment.

		s <- NULL


	## SET FUNCTION:

	## We define the 'set' function to store the value of 'x' in the cache. 
	##	How we do it? -> We use the '<<-' operator to assign a value to an object in an environment that is different from the current environment.
	## 	Why we do it? -> If the contents of the matrix are not changing, we can get the inverse of it (if we have already calculated and stored it) from the cache rather than recompute.
	## Whenever we call this function:
	## 	1. We will assign the entering value to 'x' and set it in the cache.
	## 	2. We will assign the null value to 's' and set it in the cache.
      
		set <- function(y) 
		{
            		x <<- y
            		s <<- NULL
        	}
      

	## GET FUNCTION:

	## We define the 'get' function to get the value of 'x' from the cache.
	## 	If 'x' has not been stored in the cache before, this function will return the NULL value.

		get <- function() x


	## SETSOLVE FUNCTION:

	## We define the 'setsolve' function to calculate the inverse of 'x', and set it in the cache.
	## Whenever we call this function:
	## 	1. We calculate the inverse of 'x' (where 'x' is a square invertible matrix) with the 'solve' function of R. 
	##		If 'x' is not a invertible matrix, R will return this error: "Error in solve.default(x) : Lapack routine dgesv: system is exactly singular: U[,] = 0".
	##		If 'x' is not a square matrix, R will return thir error: "Error in solve.default(x) : 'a' (r x c) must be square".
	## 	2. We will assign the value of the inverse of 'x' to 's' and stored it in the cache.

      		setsolve <- function(x)
		{
			s <<- solve(x)
		}


	## GETSOLVE FUNCTION:	

	## We define the 'getsolve' function, to get the value of the inverse of 'x' from the cache.
	## 	If 's' has not been stored in the cache before, this function will return the NULL value.

      		getsolve <- function() s

	
	## MakeCacheMatrix returns a list of 4 components. This components are the following 4 functions: set, get, setsolve and getsolve

      		list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
	
	}


## We assign the result of the function 'MakeCacheMatrix' to 'MCM', for an specific value 'x'. So 'MCM' is a list of 4 components (functions).

MCM<-MakeCacheMatrix(x)


## We define the 'CacheSolve' function as a function of 'MCM'. 
## This function will return the inverse of the matrix 'x': R will get it from the cache, in case it has been calculated and stored before. Otherwise, R will calculate and store it in the cache.

CacheSolve <- function(MCM) 

	{
        
	## We first check if the inverse has already been calculated. 

		s <- MCM$getsolve()

	
	## If 's' is not null (meaning: the inverse has already been calculated and stored in the cache), we get cached data and skips the computation with the 'return' function.
      
		if (!is.null(s)) 
		{
            		message("Getting cached data")
            	return (s) 
        	}

        
	## Otherwise, we get 'x' from the cache, calculate the inverse of it, and set it in the cache.

		x<-MCM$get()
	      	s<-MCM$setsolve(x)

      
	## Print results

		s
	
	}

