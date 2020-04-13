this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("augcoeff.r") 

findRow <- function(maxValue, i, a){
  n = nrow(a)
  for (j in 1:n){
    if (abs(a[j,i])==maxValue){
      return (list(index = j,row = a[j,]))
    }
  }
}

backwardElimination <- function(a){
  b=a[,"RHS"]
  n = nrow(a)
  x=NULL

  for (i in n:1){
    x[i] = (b[i] - sum(a[i, (i+1):n] * x[(i+1):n])) / a[i,i]
  }
  
  return (x)
}

Gaussian <- function(augCoeffMatrix){
  a = augCoeffMatrix$augcoeffmatrix
  n = length(augCoeffMatrix$variables)
  xvalues = NULL

  for (i in 1:(n-1)){
    maxValue = (max(abs(a[i:n,i])))
    pivotRow = findRow(maxValue,i,a)
    #swap
    a[pivotRow$index,]=a[i,]
    a[i,]=pivotRow$row
    for (j in (i+1):n){
      pivotEl = a[i,i]
      multiplier = a[j,i]/pivotEl
      nr = a[i,] * multiplier
      a[j,] = a[j,] - nr
    }
  }
  a[is.nan(a)] = 0
  x = backwardElimination(a)
  return (list(solutionSet = x, matrix = a, variables = augCoeffMatrix$variables ))
}

GaussJordan <- function(augCoeffMatrix){
  #print(augCoeffMatrix)
  a = augCoeffMatrix$augcoeffmatrix
  #print(a)
  n = length(augCoeffMatrix$variables)
  for (i in 1:n){
    if (i!=n){
      maxValue = (max(abs(a[i:n,i])))
      pivotRow = findRow(maxValue,i,a)
      
      #swap
      a[pivotRow$index,]=a[i,]
      a[i,]=pivotRow$row
    }
    a[i,] = a[i,]/a[i,i]
    
     for (j in 1:n){
      if (i==j){
        next
      }
      nr = a[j,i]*a[i,]
      a[j,]= a[j,] - nr
     }
  }
  x = backwardElimination(a)
  return (list(solutionSet = x, matrix = a, variables = augCoeffMatrix$variables ))
}

E1 <- function (x1, x2, x3) 0.3 * x1 + -0.2 * x2 + 0 * x3 + -71.4;
E2 <- function (x1, x2, x3) 3 * x1 + -0.1 * x2 + 0 * x3 + -7.85;
E3 <- function (x1, x2, x3) 0.1 * x1 + 7 * x2 + -0.3 * x3 + 19.3;
system <- c(E2, E3, E1);
#print(system)
#result <- AugCoeffMatrix(system)
#print(result)

#gaussianElim = Gaussian(result)
#gaussJordanElim = GaussJordan(result)
#print("gaussian elimination")
#print(gaussianElim)
#print("gauss jordan")
#print(gaussJordanElim)