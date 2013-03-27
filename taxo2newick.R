newick <- function(x){
  base <- x[1]
  if(length(x) > 1){
    for(i in 2:length(x)){
      base <- paste(base, x[i], sep = ", ")
    }
    return(paste("(", base, ")", sep = ""))
  } else {
    return(base)
  }
}
paste("(", base, ")", sep = "")

taxo2newick <-function(x){
  base <- newick(unique(x$Class))
  for(i in 1:length(unique(x$Class))){
    temp <- subset(x, Class == unique(x$Class)[i])
    temp2 <- newick(as.vector(unique(temp$Order)))
    base <- gsub(unique(x$Class)[i], temp2, base)
    for(j in 1:length(unique(temp$Order))){
      lowertemp <- subset(temp, Order == unique(temp$Order)[j])
      lowertemp2 <- newick(as.vector(unique(lowertemp$Family)))
      base <- gsub(unique(temp$Order)[j], lowertemp2, base)
    }
  }
  return(paste(base, ";", ""))
}