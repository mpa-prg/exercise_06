install.packages("combn")

library(combn) 

# declaring a list 
Xa <- c(2,3,5,10) 
Xb <- c(3,7,10)
Xab <- c(1,2,2,5,5,5)
permXa <- c()
permXb <- c()
permXa <- permn(Xa)
permXb <- permn(Xb)

position_map <- function(numbers){
  map <- c(0)
  for (i in 1:length(numbers)){
    map <- append(map, map[i]+numbers[i])
  }
  return(map)
}
for (i in 1:length(permXa)){
  for (j in 1:length(permXb)){
    posXa <- position_map(permXa[[i]])
    posXb <- position_map(permXb[[j]])
    combined <- c(posXa, posXb)
    combined <- sort(unique(combined))
    diff <- c()
    for (x in 1:(length(combined)-1)){
      diff <- append(diff, combined[x+1]-combined[x])
    }
    diff <- sort(diff)
    if (all(diff == Xab) == TRUE){
      print(diff)
      print(permXa[[i]])
      print(permXb[[j]])
    }
  }
}

deltaX <- c(2,2,3,3,4,5,6,7,8,10)

PartialDigestProblem <- function(deltaX) {
  width <- max(deltaX)
  deltaX <- deltaX[deltaX != width]
  X <- c(0, width)
  Place(deltaX, X, width)
}

Place <- function(deltaX, X, width) {
  if (length(deltaX) == 0) {
    print(X)
    return
  }
  
  y <- max(deltaX)
  
  if (all(Remove(y, X) %in% deltaX)) {
    X <- c(X, y)
    lengths_to_remove <- Remove(y, X)
    deltaX <- Remove(deltaX, lengths_to_remove)
    Place(deltaX, X, width)
    X <- setdiff(X, y)
    deltaX <- c(deltaX, lengths_to_remove)
  }
  
  if (all(Remove(width - y, X) %in% deltaX)) {
    X <- c(X, width - y)
    lengths_to_remove <- Remove(width - y, X)
    deltaX <- Remove(deltaX, lengths_to_remove)
    Place(deltaX, X, width)
    X <- setdiff(X, width - y)
    deltaX <- c(deltaX, lengths_to_remove)
  }
}

Remove <- function(lengths, X) {
  lengths_to_remove <- lengths[lengths %in% X]
  return(lengths_to_remove)
}

PartialDigestProblem(deltaX)








permXa[[6]]
permXb[[1]]
posXa <- position_map(permXa[[6]])
posXb <- position_map(permXb[[1]])
posXa
posXb
combined <- c(posXa, posXb)
combined <- sort(unique(combined))

diff <- c()
for (i in 1:(length(combined)-1)){
  diff <- append(diff, combined[i+1]-combined[i])
  
}
diff <- sort(diff)

