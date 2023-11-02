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

