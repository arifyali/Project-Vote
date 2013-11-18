# Classification Tree with rpart
library(rpart)

## Takes vectors containing number of votes for republicans and democrats
## If either value is missing, it returns NA for that row
## If the values are equivalent, it returns NA for that row
winners = function(rep, dem) {
  return(ifelse(is.na(rep) & is.na(dem) | rep == dem, NA,
         ifelse(dem > rep, "Dem", "Rep")))
}

## Measures the percentage of correct predictions
predAccuracy = function(predictions, actual) {
  return(sum(ifelse(predictions == actual, 1, 0),
         na.rm = TRUE) / length(actual[!is.na(actual)]))
}

getWinner = function(winners) {
  totalDem = sum(ifelse(winners == "Dem", 1, 0), na.rm = TRUE)
  if(totalDem / length(totalDem[!is.na(totalDem)]) > 0.50) {
    return("Dem")
  } else {
    return("Rep")
  }
}

for(i in 5:39) {
  FinalDF[,c(i)] = as.numeric(as.character(FinalDF[,c(i)]))
}
FinalDF$Winner2004 = winners(FinalDF$bushVote, FinalDF$kerryVote)
FinalDF$Winner2012 = winners(FinalDF$obama.votes, FinalDF$romney.votes)

# grow tree
names(FinalDF) = gsub("[-+]", "", names(FinalDF))
formulaList = paste("Winner2004 ~",
                    paste(names(FinalDF[,c(5:6, 9:39)]), collapse=" + "), collapse="")
fit = rpart(as.formula(formulaList),
             method="class", data=FinalDF, control = rpart.control(cp = 0.03))

# plot tree 
par(mfrow=c(1,1), xpd=NA)

prune(fit, fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

plot(fit, uniform=TRUE, main="Classification Tree for Winning Party")
text(fit, use.n=TRUE, all=TRUE, cex=.8, digits=4)

# Get predictions
FinalDF$Predictions2004 = as.character(predict(fit, FinalDF[,c(5:6, 9:39)], type="class"))

# Guage accuracy
#percCorrect = predAccuracy(FinalDF$Predictions2012, FinalDF$Winner2012)

# Get winner
#print(getWinner(FinalDF$Predictions2012))