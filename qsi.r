this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("gaussian_gaussjordan.r")

eqToFunc <- function(header,eq_list){
  n = length(eq_list)
  eq = c()
  for (i in 1:n){
    eq_list[i] = (paste(sep="","E", i," <- ",header,eq_list[i]," }"))
    eq = c(eq,eval(parse(text = eq_list[i])))
  }
  
  return(eq)
  
}
header <- function(interval){
  f = "function("
  
  #for a variables
  for (i in 1:interval){
    var = paste(sep="","a",i)
    f = paste(f,var,",")
  }
  
  #for b variables
  for (i in 1:interval){
    var = paste(sep="","b",i)
    f = paste(f,var,",")
  }
  
  #for c variables
  for (i in 1:interval){
    var = paste(sep="","c",i)
    f = paste(f,var)
    if (i != interval){
      f = paste(f,",")
    }
  }
  f = paste(f,"){")
  return (f)
}

add_equation1 <- function(n,x,subscript,fx){
  x2 = x**2
  f = paste(sep="",x2, " * ", "a", subscript, " + ", x, " * ", "b", subscript, " + ", "c", subscript, " - ",y_data[fx])
  for (i in 1:n){
    if (i!=subscript){
      f = paste(f, sep="", " + ",0,"* a",i, " + ",0,"* b",i, " + ",0,"* c",i)
    }
  }
  
  return (f)
}

add_equation2 <- function(n,x,sub_rhs,sub_lhs){
  x2 = 2*x
  f = paste(sep="",x, " * ", "a", sub_rhs, " + ", "b", sub_rhs," - ", x, " * ", "a",sub_lhs, " - ","b",sub_lhs )
  for (i in 1:n){
    if (i!=sub_rhs || i!=sub_rhs){
      f = paste(f,sep="", " + ",0,"* a",i, " + ",0,"* b",i, " + ",0,"* c",i)
    }
  }
  return(f)
}

getColumn <- function(n){
  col = c()
  for (i in 1:n){
    col = c(col,paste(sep="","a",i))
    col = c(col,paste(sep="","b",i))
    col = c(col,paste(sep="","c",i))
  }
  col = c(col,paste(sep="","RHS"))
  return (col)
}
qsi <- function(est,x_data,y_data){
  interval = length(x_data) - 1
  eq = c()
  
  header = header(interval)
  
  colnames = getColumn(interval)
  
  rownames = c(1:(interval*3))
  m = matrix(nrow=length(rownames),ncol=length(colnames),dimnames = list(rownames,colnames))
  row = 1
    
    for (i in 2:interval){
      x = x_data[i]
      y = y_data[i]
      x2 = x**2
      
      a1 = paste(sep="","a",(i-1))
      a2 = paste(sep="","a",i)
      b1 = paste(sep="","b",(i-1))
      b2 = paste(sep="","b",i)
      c1 = paste(sep="","c",(i-1))
      c2 = paste(sep="","c",i)

      m[row,a1] =  x2
      m[row,b1] =  x
      m[row,c1] =  1
      m[row,"RHS"] = y
      
      row = row + 1
      
      m[row,a2] =  x2
      m[row,b2] =  x
      m[row,c2] =  1
      
      m[row,"RHS"] = y
      
      row = row + 1      
      
    }      
    
    x = x_data[1]
    y = y_data[1]
    x2 = x**2
    
    a1 = paste(sep="","a",1)
    b1 = paste(sep="","b",1)
    c1 = paste(sep="","c",1)
    
    m[row,a1] = x2
    m[row,b1] = x
    m[row,c1] = 1
    m[row,"RHS"] = y
    
    row = row + 1
    n = interval+1

    x = x_data[(interval+1)]
    y = y_data[(interval+1)]
    x2 = x**2
    
    a1 = paste(sep="","a",interval)
    b1 = paste(sep="","b",interval)
    c1 = paste(sep="","c",interval)
    
    m[row,a1] = x2
    m[row,b1] = x
    m[row,c1] = 1
    m[row,"RHS"] = y
    
    row = row + 1
    

    for (i in 2:interval){
      x = x_data[i]
      x2 = x*2
      
      a1 = paste(sep="","a",(i-1))
      a2 = paste(sep="","a",i)
      b1 = paste(sep="","b",(i-1))
      b2 = paste(sep="","b",i)
      
      m[row,a1] = x2
      m[row,b1] = 1
      m[row,a2] = -x2
      m[row,b2] = -1
      
      row = row + 1
      
    }
    m[row,"a1"] = 0
    
    m[is.na(m)] = 0
    
    m <- (m[1:(interval*3-1),-1])
    
    var_length = length(colnames)-1
    variables = colnames[2:var_length]
    
    augcoeff = (list(augcoeffmatrix = m,variables = variables))
    
    result = Gaussian(augcoeff)
    ss = result$solutionSet
    if (sum(is.nan(ss))){
      result = GaussJordan(augcoeff)
    }
    
    result$solutionSet = c(0,result$solutionSet)
    result$variables = c("a1", result$variables)
    
    intervalFunctions = c()
    for (j in 0:(interval-1)){
      terms = paste(sep="",result$solutionSet, " * x**",j)
      fi = "function(x)"
      for(i in 1:interval*3){
        fi = paste(fi,terms[i])
        if (i!=interval*3)
          fi = paste(fi,"+")
      }
      intervalFunctions = c(intervalFunctions,fi)
    }
      
    for (i in 1:interval){
      if(x_data[i]<=est && x_data[i+1]){
        f = intervalFunctions[i]
        intrvl = i
      }
    }
    
    print(f)
    f = eval(parse(text=f)) 
    return(list(intervalFunctions=intervalFunctions,f=f,interval=intrvl))
    
}



#x_data = c(3,4.5,7,9)
#y_data = c(2.5,1,2.5,0.5)

#x_data = c(1.6,2,2.5)  
#y_data = c(2,8,14)
