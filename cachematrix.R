
makeCacheMatrix <- function(m = matrix()){
    inverse <- NULL#set up variable inverse to null
    
    set_matrix <- function(n){
        #function sets matrix
        m <<- n
        inverse <<- NULL
    }
    
    get_matrix <- function(){
        #function return matrix
        m
    }
    
    set_inverse <- function(m){
        #function set inverse of matrix
        inverse <<- m
        
    }
    get_inverse <- function(){
        #function return inverse of matrix
        inverse
    }
    list(set_matrix = set_matrix, get_matrix = get_matrix, 
         set_inverse = set_inverse, get_inverse = get_inverse)  
    #all is saved in one list 
}

#this function "cacheSolve" check if inverse was already calculated. If it was return it
#if it was not than calculate it and set it in list
cacheSolve <- function(m,...){
    s <- m$get_inverse()#call funtion set_matrix
    if(!is.null(s)){#if is not null
        message("getting cached matrix inverse")#print message
        return(s)#return inverse of matrix matrix
    }
    data <- m$get_matrix()#if is null
    s <- solve(data, ...)#calculate inverse
    m$set_inverse(s)#set inverse
    s#return inverse    
}

