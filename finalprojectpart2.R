########################Part 3#############################
##Nearest Neighbor
#names: Arif Ali, Matt Iannone 
winningparty = function(Dem, GOP){
  ifelse(GOP>Dem, "Rep",
         ifelse(GOP!=Dem, "Dem", 
                sample(c("Rep", "Dem"), 1)))
}
###determines which party won, which county based on 
FinalDF$winner2004 = winningparty(FinalDF$kerryVote ,FinalDF$bushVote)
FinalDF$winner2012 = winningparty(FinalDF$obama.votes ,FinalDF$romney.votes)
library(class)
k2004 = function(){
  DFpart2 = na.omit(FinalDF
                    [ , c("Gr1-8.Enroll", "HS.Enroll",
                                 "College.Enroll","Total.Pop", "longitude",
                                 "latitude", "winner2004", "bushVote",  "kerryVote")]
                    )
  cl = DFpart2$winner2004
  test = DFpart2[ , c("Gr1-8.Enroll", "HS.Enroll",
                      "College.Enroll","Total.Pop", "longitude",
                      "latitude", "bushVote",  "kerryVote")]
  set.seed(222222222)
  k2004 = knn(test, test, cl, k = 5, prob = TRUE)
  
  return(k2004)
  ###k2004 is where na.omit version of FinalDF is used for the knn function.
  ###cl is the winning party
}
k2004 = k2004()
###issues noticed are the mismatching of winning parties to counties, 
###not sure if that's suppose to happen.
########################Part 4#############################

#layout(matrix(c(1,2)), width = 1, height = c(2,2))

#barplot(c(sum(k2004$Dem)/(10^6),sum(k2004$Rep)/(10^6)), col= c("#010440", "#A61103"),
#        ylab = c("Number Votes in the by Million"),
#       main = c("The Elections Predictions of 2004 based on Nearest Neighbor"))
plot(k2004, col= c("#010440", "#A61103"), ylab = c("Number of Counties"))
############################################################