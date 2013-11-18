winningparty = function(Dem, GOP){
  
  ## GOP is a vector of Republican vote counts
  ## Dem is a vector of Democrat vote counts
  ## This function determines which party won by comparing vote counts
  ## In case of a tie, it randomly selects a winner
  
  ifelse(GOP>Dem, "Rep",
         ifelse(GOP!=Dem, "Dem", 
                sample(c("Rep", "Dem"), 1)))
}


FinalDF$winner2004 = winningparty(FinalDF$kerryVote, FinalDF$bushVote)
FinalDF$winner2012 = winningparty(FinalDF$obama.votes, FinalDF$romney.votes)


source("http://bioconductor.org/biocLite.R")
biocLite("preprocessCore")
library("preprocessCore")
library(class)


knnElec = function(k, var, winner){
  
  ### k is the desired amount of neighbors. var is a vector containing
  ### the names of variables to use in the nearest neighbor analysis.
  ### winner is the name of the vector in FinalDF containing the
  ### winner of the election to be used in the test set
  ### Function performs k-nearest neighbor analysis on the election
  
  DFpart2 = na.omit(FinalDF
                    [ , c("bushVote","kerryVote", winner, "longitude",
                          "latitude", var)]
  )
  
  DFpart2[,4:(5+length(var))] = normalize.quantiles(as.matrix(DFpart2[,4:(5+length(var))]))
  
  test = DFpart2[ , c("longitude",
                      "latitude", "bushVote",  "kerryVote", var)]
  set.seed(222222222)
  k2004 = knn(test, test, DFpart2$winner, k, prob = TRUE)
  
  return(k2004)
}

knnVars = "Attain.BachAndUp"
knn2004 = knnElec(k=3, var=knnVars, "actualwinner2004")



########################Part 4#############################

#layout(matrix(c(1,2)), width = 1, height = c(2,2))

#barplot(c(sum(k2004$Dem)/(10^6),sum(k2004$Rep)/(10^6)), col= c("#010440", "#A61103"),
#        ylab = c("Number Votes in the by Million"),
#       main = c("The Elections Predictions of 2004 based on Nearest Neighbor"))
barplot(c(sum(as.character(knn2004)=="Dem"),
       sum(as.character(knn2004)=="Rep")),
        width = c(sum(na.omit(FinalDF)$kerryVote[knn2004=="Dem"]*
          attr(knn2004, "prob")[knn2004=="Dem"]/3),
                  sum(na.omit(FinalDF)$bushVote[knn2004=="Rep"]*
                    attr(knn2004, "prob")[knn2004=="Rep"]/3)),
        col= c("#010440", "#A61103"),
      xlab = c("Counties"),
      ylab = c("Number of votes"))
############################################################

#sum(knnElec(k=3, var = "NaturalizedCitizen", winner ="winner2004") == na.omit(FinalDF)$winner2012)