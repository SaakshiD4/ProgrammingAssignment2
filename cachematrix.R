# makeCacheMatrix: Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the cached inverse as NULL
  m <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y      # Store the matrix in the parent environment
    m <<- NULL   # Clear cached inverse when matrix is updated
  }
  
  # Function to get the matrix
  get <- function() {
    x            # Return the matrix
  }
  
  # Function to set the cached inverse
  setinverse <- function(inverse) {
    m <<- inverse  # Store the inverse in the parent environment
  }
  
  # Function to get the cached inverse
  getinverse <- function() {
    m             # Return the cached inverse
  }
  
  # Return a list of functions to interact with the matrix and its cached inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve: Computes the inverse of the special matrix created by makeCacheMatrix
cacheSolve <- function(x, ...) {
  # Retrieve the cached inverse
  m <- x$getinverse()
  
  # If the inverse is already cached, return it
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # If the inverse is not cached, compute it
  data <- x$get()             # Get the matrix
  m <- solve(data, ...)       # Compute the inverse
  x$setinverse(m)             # Cache the computed inverse
  
  # Return the computed inverse
  m
}

# Example usage of makeCacheMatrix and cacheSolve

# Create a matrix
matrix_data <- matrix(c(4, 7, 2, 6), nrow = 2, ncol = 2)

# Create the special "matrix" object that can cache its inverse
cached_matrix <- makeCacheMatrix(matrix_data)

# Compute and cache the inverse
inverse_matrix_1 <- cacheSolve(cached_matrix)

# Print the inverse
print(inverse_matrix_1)

# Now call cacheSolve again to demonstrate caching
inverse_matrix_2 <- cacheSolve(cached_matrix)

# Print the inverse again
print(inverse_matrix_2)
