this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("gaussian_gaussjordan.r")

PolynomialRegression <- function(x, y, degree){
  if (is_Valid(x,y,degree)){
    new = (sum(x**0))
    rownames = NULL
    colnames = NULL
    for (i in 1:(degree+1)){
      rownames = c(rownames,i)
      colnames = c(colnames,paste("x",sep="",i))
    }
    colnames = c(colnames,'RHS')
    augcoeff = matrix(dimnames = list(rownames,colnames),nrow = degree + 1, ncol = degree + 2)
    for (i in 1:(2*degree)){
      new = c(new,sum(x**i))
    }
    for (i in 1:(degree + 1)){
      augcoeff[i,] = c(new[i:(i+degree)],1)
    }
    for (i in 1: degree){
      augcoeff[i+1, degree+2] = sum(x**i * y)
    }
    augcoeff[1,degree+2] = sum(x**0 * y)
    m = list(augcoeffmatrix = augcoeff, variables = colnames[1:(length(colnames)-1)])
    result = GaussJordan(m)
    
    s = "f <- function (x)"
    for (i in 1:(degree+1)){
      coef = result$solutionSet[i]
      term = paste("x",sep=" ** ",(i-1))
      if (i != (degree+1)){
        s = paste(s, sep=" ", coef, "*" ,term, "+")
      }
      else {
        s = paste(s,coef, "*" ,term, sep = " ")
      }
    }
    s = eval(parse(text = s))
    print(list(func = s, coefficient = result$solutionSet))
    return(list(func = s, coefficient = result$solutionSet))
  }
}

is_Valid <- function(x,y,degree) {
  print(x)
  if (degree < 1){
    return (FALSE)
  }
  if (length(x) != length(y)){
    return (FALSE)
  }
  
  return (TRUE)
  
}

#temperature = c(50,50,50,70,70,70,80,80,80,90,90,90,100,100,100)
#yield = c(3.3,2.8,2.9,2.3,2.6,2.1,2.5,2.9,2.4,3.0,3.1,2.8,3.3,3.5,3.0)
#temperature = c(1,2,3)
#yield = c(2,3,4)
#print(typeof(temperature))
#polynomial = PolynomialRegression(temperature,yield,3)
#print(polynomial$func(10))
#print(polynomial$coefficient)
#print(polynomial)
