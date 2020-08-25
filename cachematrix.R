##makeCacheMatrix creates a function to reset the cache when new inputs are entered, and it creates a list of functions to be called by cacheSolve
makeCacheMatrix <- function(input = matrix()) { 
        inverse <- NULL ## initialize inverse
        mutateinput <- function(newinput){ ##this function resets inverse when input is changed
                input <<- newinput
                inverse <<- NULL
        }
        accessinput <- function() input
        mutateinverse <- function(solve) inverse <<- solve
        accessinverse <- function() inverse
        list(mutateinput = mutateinput, accessinput = accessinput,
             mutateinverse = mutateinverse, accessinverse = accessinverse) 
        ##Makes a nice list to call functions by name using $
        
}

##cacheSolve takes the output of makeCacheMatrix as an input, checks if inverse is cached, and returns it. 
## Example: cacheSolve(makeCacheMatrix(testmatrix))
cacheSolve <- function(input, ...) {
        inverse <- input$accessinverse() #call from our nice list
        if(!is.null(inverse)) {
                message("Getting cached inverse. I have a great memory!")
                return(inverse) 
        } else {
                message("No cached data") 
                dm <- input$accessinput() ##grabs the new data matrix
                newinverse <- solve(dm) 
                input$mutateinverse(newinverse) ##updates the cached inverse
                newinverse
        }
        
}
