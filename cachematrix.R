makeCacheMatrix=function(x=matrix()){
        cachedMatrix = NULL  #the original cachedMatrix variable is assigned to be Null
        
        set= function(y){       #
                x<<-y
                cachedMatrix<<-NULL
        }
        get= function(){
                x
        }
        setCacheMatrix=function(inverse){
                cachedMatrix<<- inverse
        }
        getInverse= function() {   #gets the inverse matrix
                cachedMatrix
        }
        
        list(set=set,get=get, setCacheMatrix=setCacheMatrix,getInverse=getInverse)
}





cacheSolve= function(x,...){
        cachedMatrixVal= x$getInverse()  #collects the inverse value from the list
        if(!is.null(cachedMatrixVal)){
                message("getting cached Matrix")
                return(cachedMatrixVal)
        }   #if the inverse has already been calculated, spit it out from the cache and return the val
        #ends the program
        data=x$get() #the inverse was not cached so it needs to be calculated
        cachedMatrixVal=solve(data,...) #calculates the inverse and assigns it to the cachedMatrixVal var
        x$setCacheMatrix(cachedMatrixVal)  #runs the setCacheMatrix function in the list and saves the inverse
        cachedMatrixVal  #returns the inverse matrix
        
}