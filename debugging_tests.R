### debugging code :[

createPredMap = function(predictions, winners, file, type, desc) {
  ### creates a map that demonstrates how well the predicted results did.
  ### predictions and winners are vectors of predicted vs actual winner ("Rep" or "Dem")
  ### file is the file name in which to save the plot
  ### type is a string describing the method (e.g. "Recursive Partitioning")
  ### desc is the caption for the plot.
  library(maps)
  color2 = predColor(predictions, winners)
  
  pdf(file=file, width = 9.9, height = 7.7)
  map("county", fill=TRUE, col="#BDBDBD44", border="white")
  title(main = paste("Prediction Accuracy using", type),
        sub = paste("Red and Blue represent correctly-predicted Republican and",
                    "Democrat, respectively.\nBeige corresponds to Republican wins",
                    "that were incorrectly predicted as Democrat.\nGreen corresponds",
                    "to Democrat wins that were incorrectly predicted as Republican.",
                    "\n", desc))
  legend("bottomleft", c("Rep", "Dem", "Rep (predicted Dem)", "Dem (predicted Rep)"),
         fill = c("#E31A1C", "#1F78B4", "#FE9929", "#4DAF4A"))
  points(calDF$longitude/1000000, calDF$latitude/1000000, col=color2, pch=20)
  dev.off()  
}
  

knnVars = c("Attain.BachAndUp", "Total.Pop")
calDF = na.omit(FinalDF[, c("state", "county", "bushVote","kerryVote", "winner2012", 
                              "longitude", "latitude", knnVars)])
calDF = calDF[calDF$state == "texas",]

createPredMap(knn2004$knnpredict2004, calDF$winner2012, 
              "RAWRknnPredMap.pdf",
              "K-Nearest Neighbor",
              paste("Our KNN predictor tended to incorrectly predict:",
                    "Democrat wins in counties where Republicans won."))


