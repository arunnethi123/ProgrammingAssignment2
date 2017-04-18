#Creating function to 1)set matrix 2)get matrix which returns matrix 3)setInv to store the inverse 4) getInv to 
#return inverse matrix.
makeCacheMatrix <- function(mat_data=matrix())
{
  inv<-NULL
  set<-function(y)
  {
    mat_data<<-y
    inv<<-NULL
    mat_data
  }
  get<-function() mat_data
  
  setInv<-function(i)
  {
    inv<<-i
  }
  getInv<-function() inv
  
  list(set=set,get=get,setInv=setInv,getInv=getInv)
  
}


#Creating function to calculate inverse and return it if it is not created else returns the existing inverse 
#matrix with a message.
cacheSolve<- function(mcm,...)
{
  inv<-mcm$getInv()
  if(!is.null(inv))
  {
    message("getting inverse matrix data")
    return(inv)
  }
  mat<-mcm$get()
  inv=solve(mat,...)
  mcm$setInv(inv)
  return(inv)
  
}


#Example Input with result: l=makeCacheMatrix(matrix(c(1,2,3,4),2,2))
# cacheSolve(l)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
# cacheSolve(l)
#getting inverse matrix data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

