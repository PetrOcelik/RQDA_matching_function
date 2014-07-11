############################
####### Jiri Fejfar 7-7-2014
############################

#----------function matches overlapping segments of coded text (for details see readme)----------

sortAnnotations <- function(matrixA, matrixB)
{
  colnames(matrixA) <- c("start", "end", "cid")
  colnames(matrixB) <- c("start", "end", "cid")
  
  sortMatrixA <- matrixA[order(matrixA[,1]),]
  sortMatrixA$id <- seq_len(nrow(sortMatrixA))
  sortMatrixB <- matrixB[order(matrixB[,1]),]
  sortMatrixB$id <- seq_len(nrow(sortMatrixB))

  sortMatrixA$matched_c2c=FALSE
  sortMatrixB$matched_c2c=FALSE
  
  sortMatrixA$matched_u2u=FALSE
  sortMatrixB$matched_u2u=FALSE
  
  sortMatrixA$matched_u2c=FALSE
  sortMatrixB$matched_u2c=FALSE
  
  #----------find CORRECTLY matched (CORRECT TO CORRECT)----------
  #Cartesian product
  M <- merge(sortMatrixA, sortMatrixB, by.x=NULL, by.y=NULL, all.y=TRUE)
  
  #Match conditions:
  #position
  a1 <- (M["start.x"] >= M["start.y"]) & (M["end.x"] <= M["end.y"])
  a2 <- (M["start.x"] <= M["start.y"]) & (M["end.x"] >= M["end.y"])
  
  a3 <- (M["start.x"] < M["start.y"] & M["end.x"] > M["start.y"] & M["end.x"] <  M["end.y"] )
  a4 <- (M["start.y"] < M["start.x"] & M["end.y"] > M["start.x"] & M["end.y"] <  M["end.x"] )
  
  #label
  a5 <- M["cid.x"] == M["cid.y"]
  
  #create logical index vector
  a_c2c <- ( a1 | a2 | a3 | a4 ) & a5
  
  #set column matched_c2c to TRUE
  sortMatrixA[M[a_c2c, "id.x"], "matched_c2c"] = TRUE
  sortMatrixB[M[a_c2c, "id.y"], "matched_c2c"] = TRUE
  
  #----------find UNCORRECTLY matched 1 (UNCORRECT TO UNCORRECT)----------
  #Cartesian product with new values
  M <- merge(sortMatrixA, sortMatrixB, by.x=NULL, by.y=NULL, all.y=TRUE)
  
  #are not correctly matched
  a6 <- (!M["matched_c2c.x"] & !M["matched_c2c.y"])
  
  #create logical index vector
  a_u2u_orig <- ( a1 | a2 | a3 | a4 ) & a6
  a_u2u <- a_u2u_orig
  
  #remove "cross" pairs
  for (m in 1:(length(a_u2u)-1)){ 
    if(a_u2u[m]==TRUE){
      idx <- M[m,"cid.x"]
      idy <- M[m,"cid.y"]
      startx <- M[m,"start.x"]
      starty <- M[m,"start.y"]
      endx <- M[m,"end.x"]
      endy <- M[m,"end.y"]
      for (n in (m+1):length(a_u2u)){
        if(a_u2u[n]==TRUE){
          if(M[n,"start.x"]==startx & M[n,"start.y"]==starty & M[n,"end.x"]==endx & M[n,"end.y"]==endy){
            if(M[n,"cid.x"]==idx || M[n,"cid.y"]==idy){
              a_u2u[n]=FALSE
            }  
          }
        }
      }
    }
  }
    
  #set column matched_u2u to TRUE
  sortMatrixA[M[a_u2u, "id.x"], "matched_u2u"] = TRUE
  sortMatrixB[M[a_u2u, "id.y"], "matched_u2u"] = TRUE
  
  #add "cross" pairs if they have no other pair
  for (k in 1:(length(a_u2u))){
    if (a_u2u_orig[k] == TRUE & a_u2u[k] == FALSE){
      if (sortMatrixA[M[k, "id.x"], "matched_u2u"] == FALSE || sortMatrixB[M[k, "id.y"], "matched_u2u"] == FALSE ){
        a_u2u[k] = TRUE
        sortMatrixA[M[k, "id.x"], "matched_u2u"] = TRUE
        sortMatrixB[M[k, "id.y"], "matched_u2u"] = TRUE
      }  
    }
  }
  
  #----------find UNCORRECTLY matched 2 (UNCORRECT TO CORRECT)----------
  #Cartesian product with new values
  M <- merge(sortMatrixA, sortMatrixB, by.x=NULL, by.y=NULL, all.y=TRUE)
  
  #are not correctly matched
  a7a <- (!M["matched_c2c.x"] & !M["matched_u2u.x"])
  a7b <- (!M["matched_c2c.y"] & !M["matched_u2u.y"])
  a7 <- a7a + a7b
  
  #create logical index vector
  a_u2c_orig <- ( a1 | a2 | a3 | a4 ) & a7
  a_u2c <- a_u2c_orig

  #remove "cross" pairs
  for (m in 1:(length(a_u2c)-1)){ 
    if(a_u2c[m]==TRUE){
      idx <- M[m,"cid.x"]
      idy <- M[m,"cid.y"]
      startx <- M[m,"start.x"]
      starty <- M[m,"start.y"]
      endx <- M[m,"end.x"]
      endy <- M[m,"end.y"]
      for (n in (m+1):length(a_u2c)){
        if(a_u2c[n]==TRUE){
          if(M[n,"start.x"]==startx & M[n,"start.y"]==starty & M[n,"end.x"]==endx & M[n,"end.y"]==endy){
            if(M[n,"cid.x"]==idx || M[n,"cid.y"]==idy){
              a_u2c[n]=FALSE
            }  
          }
        }
      }
    }
  }
  
  #set column matched_u2u to TRUE
  sortMatrixA[M[a_u2c, "id.x"], "matched_u2c"] = TRUE
  sortMatrixB[M[a_u2c, "id.y"], "matched_u2c"] = TRUE

  #----------find UNMATCHED----------
  M <- merge(sortMatrixA, sortMatrixB, by.x=NULL, by.y=NULL, all.y=TRUE)
  
  #create NA dataframe
  NADataFrame=data.frame(0, 0, 0, 0, FALSE, FALSE, FALSE)
  colnames(NADataFrame) <- c("start", "end", "cid", "id", "matched_c2c", "matched_u2u", "matched_u2c")
  
  #create combined NA results
  naa <- sortMatrixA[sortMatrixA$matched_c2c==FALSE & sortMatrixA$matched_u2u==FALSE & sortMatrixA$matched_u2c==FALSE,]
  nab <- sortMatrixB[sortMatrixB$matched_c2c==FALSE & sortMatrixB$matched_u2u==FALSE & sortMatrixB$matched_u2c==FALSE,]
  NAA <- merge(naa, NADataFrame, by.x=NULL, by.y=NULL, all.y=TRUE)
  NAB <- merge(NADataFrame, nab, by.x=NULL, by.y=NULL, all.y=TRUE)
  
  #----------bind all together----------
  result <- rbind(M[a_c2c,], M[a_u2u,], M[a_u2c,], NAA, NAB)
  return(result[,c(1,2,3,8,9,10)])
}

setwd("working_directory")

table1 <- read.csv("data")
table2 <- read.csv("data")

sortedTable <- sortAnnotations(table1, table2)
