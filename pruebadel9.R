## Small function to check the results of cachematrix.R functions
##
##    nf: number of rows and columns of the matrix used to test the functions

pruebadel9<-function(nf=10){
      
      ## Fill the matrix with rnorm
      xMatrix<-matrix(rnorm(nf*nf),nrow=nf,ncol=nf)
      
      ## Initialize xCache
      xCache<-makeCacheMatrix(xMatrix)
      
      ## First call to cacheSolve. Compute the inverse.
      message("\nfirst call to cacheSolve")
      print(cacheSolve(xCache))
      
      ## Second call to cacheSolve. Gets the inverse from the cache.
      message("\nsecond call to cacheSolve")
      print(cacheSolve(xCache))
      
      ## function to "clean" the matrix of very small numbers using sapply
      ff<-function(x){
            if(abs(x)<1.e-6) 
                  0 
            else 
                  x
      }
      
      ## the product of the matrix with its inverse should result the unity matrix 
      ## clean the resulting matrix for readability
      message("\nunity matrix")
      matrix(sapply(xCache$get()%*%xCache$getinv(),ff),nrow=nf,ncol=nf)
      
}