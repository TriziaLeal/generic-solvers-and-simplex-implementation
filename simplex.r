getColNames<-function(){
  col = c("x1","x2","x3","x4","x5","y1","y2","y3","y4","y5","z1","z2","z3","z4","z5","S1","S2","S3","S4","S5","S6","S7","S8","Z","RHS")
  return(col)
}
getRowNames<-function(){
  row = c("S1","S2","S3","S4","S5","S6","S7","S8","Z")
  return(row)
}

fillmatrix <- function(costsMatrix,demand,supply){
  colnames = getColNames()
  rownames = getRowNames()
  m = matrix(0,nrow=9,ncol=25,dimnames = list(rownames,colnames))
  
  m[1,1:5]=1
  m[1,"S1"] = 1
  m[2,6:10]=1
  m[2,"S2"] = 1
  m[3,11:15]=1
  m[3,"S3"] = 1
  
  m[1:3,"RHS"] = supply
  
  m[4,"x1"]=1 
  m[4,"y1"]=1
  m[4,"z1"]=1
  m[4,"S4"] = 1
  
  m[5,"x2"]=1
  m[5,"y2"]=1
  m[5,"z2"]=1
  m[5,"S5"] = 1
  
  m[6,"x3"]=1
  m[6,"y3"]=1
  m[6,"z3"]=1
  m[6,"S6"] = 1
  
  m[7,"x4"]=1
  m[7,"y4"]=1
  m[7,"z4"]=1
  m[7,"S7"] = 1
  
  m[8,"x5"]=1
  m[8,"y5"]=1
  m[8,"z5"]=1
  m[8,"S8"] = 1
  
  m[4:8,"RHS"] = demand*-1
  m[4:8,1:15] = m[4:8,1:15]*-1
  m[9,1:5]=costsMatrix[1,]
  m[9,6:10]=costsMatrix[2,]
  m[9,11:15]=costsMatrix[3,]
  m[9,"Z"]=1  
  return(m)
}

pivotRow <- function(rhs){
  min = min(rhs[1:8])
  rowIndex = which(rhs == min)
  return(rowIndex[1])
  
}
getPivotColumn <- function(row){
  pivotElement = min(row[1:24])
  colIndex = match(pivotElement,row)
  return(list(pivotElement = pivotElement,colIndex = colIndex[1]))
}

phase1 <- function(tableau,m){
  while(min(m[1:8,"RHS"])<0){
    rowIndex = pivotRow(m[,"RHS"])
    rhs = m[rowIndex,"RHS"]
    pivotRow = m[rowIndex,]
    
    pivotColumn = getPivotColumn(pivotRow)
    tableau = gaussjordan(tableau,m,rowIndex,pivotRow,pivotColumn)
    m=tableau[[length(tableau)]]
  }
  
  return(tableau)
}

phase2 <- function(tableau,m){
  while(min(m["Z",1:24])<0){
    pivotColumn = getPivotColumn(m["Z",])
    pCIndex = pivotColumn$colIndex
    pC = m[,pCIndex]
    
    tr = m[,"RHS"]/pC
    
    minPos = min(tr[tr>0])
    rowIndex = which(tr == minPos)
  
    pivotColumn$pivotElement = m[rowIndex,pCIndex]
    
    pivotRow = m[rowIndex,]
    tableau = gaussjordan(tableau,m,rowIndex,pivotRow,pivotColumn)
    m = tableau[[length(tableau)]]
  }
  return (tableau)
}

gaussjordan <- function(tableau,m,rowIndex,pR,pivotColumn){
  pE = pivotColumn$pivotElement
  pCIndex = pivotColumn$colIndex
  m[rowIndex,] = pR/pE
    for (i in 1:nrow(m)){
      if (i == rowIndex){
        next
      }
      temp = m[i,pCIndex] * m[rowIndex,]
       m[i,]=m[i,]-temp    
    }
    tableau[[length(tableau)+1]] = m
  return(tableau)
}
getNumShip <- function(m){
  denverNumShip=c()
  phoenixNumShip=c()
  dallasNumShip=c()
 for (i in 1:5){
    index = which(m[,i]==1)
    if (length(index)==1){
      denverNumShip[i] = m[index,"RHS"]
    }
    else if (length(which(m[,1]>1))){
      denverNumShip[i] = 0
    }
 }
  for (j in 6:10){
    index = which(m[,j]==1)
    if (length(index)==1){
      phoenixNumShip[j] = m[index,"RHS"]
    }
    else if (length(which(m[,1]>1))){
      phoenixNumShip[j] = 0
    }
  }
  phoenixNumShip = phoenixNumShip[6:10]
  for (k in 11:15){
    index = which(m[,k]==1)
    if (length(index)==1){
      dallasNumShip[k] = m[index,"RHS"]
    }
    else if (length(which(m[,1]>1))){
      dallasNumShip[k] = 0
    }
  }
  dallasNumShip = dallasNumShip[11:15]
  shipMatrix = matrix(nrow=3,ncol = 6)
  shipMatrix[1,2:6]=denverNumShip
  shipMatrix[2,2:6]=phoenixNumShip
  shipMatrix[3,2:6]=dallasNumShip
  shipMatrix[1,1]=sum(denverNumShip)
  shipMatrix[2,1]=sum(phoenixNumShip)
  shipMatrix[3,1]=sum(dallasNumShip)
  return(shipMatrix)
}


simplex <- function(costsMatrix,demand,supply){
  tableau = list()
  m = fillmatrix(costsMatrix,demand, supply)
  tableau[[(length(tableau)+1)]] = m
  tableau = phase1(tableau,m)
  m=tableau[[length(tableau)]]
  tableau = phase2(tableau,m)
  m=tableau[[length(tableau)]]
  numShip = getNumShip(m)
  return(list(tableau=tableau,shipMatrix = numShip))
}

