### Project 4 - The Averagers ###

# Names: Matthew Iannone, Arif Ali, Teresa Tenfelder, Sean Linehan

#### WORK ####
## Part 2
# Matt - GML and Elec04 data, most of merging work
# Arif - Census data, helped with merging
# Teresa - 2012 Elec data, helped with merging
# Sean - Helped with merging, helped standardizing code format

## Part 3
# Matt - Helped with KNN
# Arif - Did much of the work on KNN
# Teresa - Helped with Recursive Partitioning
# Sean - Did most of the work on Recursive Partitioning

## Part 4
# Matt - Helped on map and plots
# Arif - Helped on plots and prediction
# Teresa - Did bulk of work on map and plots
# Sean - Helped on plots and prediction


## Group meetings: Tuesday 11/27 (M/A/S/T); Thursday 11/29 (M/A/T);
## Saturday 12/1 (M/A/S/T); Sunday 12/2 (M/A/S); Monday 12/3 (M/A);
## Tuesday 12/4 (M/A); Wednesday 12/5 (M/A/S)

##########################################################


LoadRawGeo = function(URL){  
  # URL is the URL to the geographic data
  # Function loads the data and formats it from XML into a list
  # where each entry is a state
  
  library("XML")
  rawGeoXML = xmlParse(URL)
  StateNodes = getNodeSet(rawGeoXML, "//state")
  
  rawGeoDat = lapply(1:length(StateNodes), function(z){
                    xmlValue(StateNodes[[z]])
                  })
  
  names(rawGeoDat) = sapply(1:length(rawGeoDat), function(z){
                              xmlValue(StateNodes[[z]][[1]])
                            })
  names(rawGeoDat) = tolower(gsub("([\n])|(^[[:space:]]*)|([[:space:]]*$)",
                                  "", names(rawGeoDat)))
  return(rawGeoDat)
}


ProcessRawGeo = function(rawGeo){
  # rawGeo is the list generated from LoadRawGeo
  # This function processes the list of character vectors into
  # a list of data frames, one per state
  
  states = names(rawGeo)
  rawGeo = gsub("[[:upper:]]{2,}", "", rawGeo) 
  rawGeo = strsplit(rawGeo, "(\n[[:space:]]*\n[[:space:]]*)|(\n[[:space:]]*)")
  
  for (i in 1:length(rawGeo)){
    countycount = (length(rawGeo[[i]]) - 1) / 3
    rawGeo[[i]] = data.frame(
      rep(states[i], countycount),
      rawGeo[[i]][seq(2, length=countycount,by=3)],
      rawGeo[[i]][seq(3, length=countycount,by=3)],
      rawGeo[[i]][seq(4, length=countycount,by=3)]     )
    colnames(rawGeo[[i]]) = c("state", "county", "longitude" ,"latitude")
  }
  
  return(rawGeo)
}


elimCounty = function(geoDat){
  
  ## geoDat is the output from ProcessRawGeo
  ## This function helps standardize names (lowercase, no "county", no ".")
  ## This function also cleans up some other county names
  
  
  for(i in 1:length(geoDat)){
    geoDat[[i]]$county = tolower(geoDat[[i]]$county)
    geoDat[[i]]$county = gsub(
      "([[:space:]]county)|([[:space:]]parish)", "", geoDat[[i]]$county)
    geoDat[[i]]$county = gsub(
      "([[:space:]]census[[:space:]]area)|([[:space:]]borough)|[.]",
      "",geoDat[[i]]$county)
    geoDat[[i]]$county = gsub(
      "\\<de[[:space:]]", "de", geoDat[[i]]$county)
    geoDat[[i]]$county = gsub(
      "\\<la[[:space:]]", "la", geoDat[[i]]$county)
    geoDat[[i]]$county = gsub(
      "\\<le[[:space:]]", "le", geoDat[[i]]$county)
    geoDat[[i]]$county = gsub(
      "\\<jo[[:space:]]", "jo", geoDat[[i]]$county)
    geoDat[[i]]$county = gsub(
      "\\<mc[[:space:]]", "mc", geoDat[[i]]$county)
    geoDat[[i]]$county = gsub(
      "'", "", geoDat[[i]]$county)
    geoDat[[i]]$county = gsub(
      "mi-da", "mida", geoDat[[i]]$county)
    geoDat[[i]]$county = gsub(
      "jeff davis", "jefferson davis", geoDat[[i]]$county)
  }
  geoDat[[47]]$county = gsub(" city", "%", geoDat[[47]]$county)
  geoDat[[47]]$county = gsub("anklin%", "anklin city", geoDat[[47]]$county)
  geoDat[[47]]$county = gsub("%", "", geoDat[[47]]$county)
  
  return(geoDat)
}


ToDF = function(Geolist){
  
  # This function takes a list of data frames (Geolist) and merges them
  # It also does a last minute patch to fix numeric factors
  
  DF = merge(Geolist[[1]], Geolist[[2]], all=TRUE)
  for (i in 1:(length(Geolist)-2)){
    DF=merge(DF, Geolist[[i+2]], all=TRUE)
  }
  DF$longitude = as.numeric(as.character(DF$longitude))
  DF$latitude = as.numeric(as.character(DF$latitude))
  return(DF)
}


CountyFix = function(DF, oldcount, newcount, state){
  
  # This function can be applied to fix single county names in a DF
  # DF is the data frame to be operated on, oldcount is current county name
  # newcount is desired county name, state is county state
  
  DF$county[DF$county==oldcount & DF$state==state] = newcount
  return(DF)
}


AveragerID = function(DF){
  
  # This function creates a standardized ID column
  # DF is any id with a "county" and "state column
  
  DF$ID = paste(DF$state, DF$county)
  return(DF)
}


GeoDat = ToDF(elimCounty(ProcessRawGeo(LoadRawGeo(
  "http://www.stat.berkeley.edu/users/nolan/data/Project2012/counties.gml" ))))
GeoDat = AveragerID(GeoDat)


###########################################################################################

### This script first sets up the modular functions, which are used by the
### parse2012results() function.  Call parse2012results() to get the data
### frame containing state, county, and vote numbers.

getCountyName = function(node) {
  ### NODE is of the form "<tbody id="county1XXX">...</tbody>" and contains all of the
  ### politico information for that county.
  ### Returns the county name.
  countyNode = node[[1]][[1]]
  countyName = xmlValue(countyNode, recursive=FALSE)
  countyName = sapply(countyName, function(str) {
    sub(" $", "", countyName)
  })
}

getObamaVotes = function(node) {
  ### NODE is of the form "<tbody id="county1XXX">" and contains all of the
  ### politico information for that county.
  ### Returns the number of votes for Obama/Democrat.
  myNode = node[[1]]
  if (!isObamaNode(myNode)) {
    myNode = node[[2]]
  }
  return(getVotes(myNode))
}

getRomneyVotes = function(node) {
  ### NODE is of the form "<tbody id="county1XXX">" and contains all of the
  ### politico information for that county.
  ### Returns the number of votes for Romney/Republican.
  myNode = node[[1]]
  if (isObamaNode(myNode)) {
    myNode = node[[2]]
  }
  return(getVotes(myNode))
}

isObamaNode = function(node) {
  ### NODE is of the form <tr class="party-XXX[ race-winner]">...</tr> where XXX
  ### indicates republican or democrat. " race-winner" is optional.
  ### returns true if XXX=democrat; false if XXX=republican.
  party = xmlGetAttr(node, "class")
  containsDem = grep("party-democrat", party)
  return(length(containsDem) > 0)
}

getVotes = function(node) {
  ### NODE is of the form <tr class="party-XXX...">...</tr> where XXX
  ### indicates republican or democrat and the node includes information
  ### about that candidate and how many votes they received.
  ### returns the number of votes that candidate received.
  voteNode = getNodeSet(node, "./td[@class='results-popular']")
  voteNode = voteNode[[1]]
  return(xmlValue(voteNode))
}

cleanVoteValues = function(string) {
  ### This function takes the character vector returned by getVotes,
  ### and cleans them by removing commas and spaces.  Returns the numbers
  ### in a numeric vector.
  string = gsub("[ ,]+", "", string)
  return(as.numeric(string))
}

cleanCountyNames = function(string) {
  ### takes a string vector of county names and cleans it according to our standards
  ### to make it easier to merge with everyone else's data.
  s = tolower(string)
  s = gsub("\\.| county", "", s)
  s = gsub("\\<saint\\>", "st", s)
  s = gsub("\\<de ", "de", s)
  s = gsub("-", "", s)
  s = gsub("\\<la ", "la", s)
  s = gsub("\\<le ", "le", s)
  s = gsub("jeff davis", "jefferson davis", s)
  s = gsub("'","",s)
  s = gsub("\\<jo ", "jo", s)
  s = gsub("[^a-zA-Z[:punct:] -']", "n", s)
}

parse2012results = function() {
  ### This function calls the above functions and parses the 2012 results
  ### returns a data frame with state.names, county.names, as character vectors,
  ### and obama.votes, romney.votes as numeric vectors.
  
  library(XML)
  
  state.names = county.names = character(0)
  obama.votes = romney.votes = numeric(0)
  
  urlBegin = "http://www.stat.berkeley.edu/users/nolan/data/Project2012/countyVotes2012/"
  stateNames = readLines(paste(urlBegin, "stateNames.txt", sep = ""))
  stateNames = gsub("\"", "", stateNames)
  stateNames = stateNames[-c(1, 3)]
  
  for (stateName in stateNames) {
    cat(stateName, "\n")
    file = xmlParse(paste(urlBegin, stateName, ".xml", sep = ""))
    root = xmlRoot(file)
    
    countyNames = xpathSApply(doc = root, path = "/table/tbody", getCountyName)
    obamaVotes = cleanVoteValues(xpathSApply(root, "/table/tbody", getObamaVotes))
    romneyVotes = cleanVoteValues(xpathSApply(root, "/table/tbody", getRomneyVotes))
    
    state.names = append(state.names, rep(stateName, length(countyNames)))
    county.names = append(county.names, countyNames)
    obama.votes = append(obama.votes, obamaVotes)
    romney.votes = append(romney.votes, romneyVotes)
    
  }
  
  state.names = gsub("-", " ", state.names)
  county.names = cleanCountyNames(county.names)
  county.names[state.names == "virginia" & county.names != "franklin city"] = 
    gsub(" city", "", county.names[state.names == "virginia" 
                                   & county.names != "franklin city"])
  county.names[county.names == "staten island"] = "richmond"
  county.names[county.names == "manhattan"] = "new york"
  county.names[county.names == "brooklyn"] = "kings"
  
  return(data.frame(state = state.names, county = county.names, 
                    obama.votes, romney.votes,
                    stringsAsFactors = FALSE))
}

results2012 = parse2012results()
results2012 = AveragerID(results2012)

################################################################################

LoadRaw2004 = function(URL){
  
  # URL is the URL to the 2004 election data
  # Function performs initial processing of data into
  # a reader-friendly dataframe
  
  raw2004dat = read.table(url(URL), stringsAsFactors = FALSE)
  names(raw2004dat) = t(raw2004dat)[, 1]
  raw2004dat = raw2004dat[-1, ]
  for (i in 2:ncol(raw2004dat)) {
    raw2004dat[, i]=as.numeric(raw2004dat[, i])
  }
  rownames(raw2004dat) = 1:nrow(raw2004dat)
  return(raw2004dat)
}

ProcessRaw2004 = function(raw2004){
  
  # raw2004 is the output from LoadRaw2004
  # This function further processes the data into a more useful form
  
  countysplit = strsplit(raw2004$countyName, ",")
  raw2004$county = sapply(1:length(countysplit), function(z){
                              countysplit[[z]][2]
                          })
  raw2004$state = sapply(1:length(countysplit), function(z){
                            countysplit[[z]][1]
                         })
  raw2004 = raw2004[, -1]
  
  return(raw2004)
}


CountyFix04 = function(elec04){
  
  # This function helps facilitate the merging
  # elec04  is the output from ProcessRaw2004
  elec04$county[elec04$county == "dade" & elec04$state == "florida"] = "miamidade"
  elec04$county = gsub("\\<de[[:space:]]", "de", elec04$county)
  elec04$county = gsub("\\<la[[:space:]]", "la", elec04$county)
  elec04$county = gsub("\\<le[[:space:]]", "le", elec04$county)
  elec04$county = gsub("jeff davis", "jefferson davis", elec04$county)
  elec04$county = gsub("\\<du[[:space:]]", "du", elec04$county)
  elec04$county = gsub("\\<jo[[:space:]]", "jo", elec04$county)
  elec04$county = gsub(" dulac", " du lac", elec04$county)
  return(elec04)
}


Elec04 = CountyFix04(ProcessRaw2004(LoadRaw2004(
  "http://www.stat.berkeley.edu/users/nolan/data/Project2012/countyVotes2004.txt" )))
Elec04 = CountyFix(Elec04, "washington", "district of columbia", "district of columbia")
Elec04 = AveragerID(Elec04)


############################################################################

Standst= function(data){
  
  ## Fixes issues with "St." in county names for data (the Census Data)
  
  gsub("[Ss]te?[.]", "st", data$Geography)
}

order.data = function(data, column){
  
  ## Orders a certain column of data: data is a data frame, column
  ## is a vector to order within the data frame
                      
  data[order(column), ]
}

County = function(data){
  
  ## data is the Census data frame
  ## This removes the "county" and "parish designations from the county data
  
  tolower(gsub(
    "(County|Parish|Municipio|Municipality|Municip)*,.*$", "",
    data$Geography))
}

States = function(data){
  
  ## This fixes an issue with West Virginia's irregular state name
  ## data is the Census data frame
  
  data$State = gsub(" westvirginia",
                    " west virginia", 
                    gsub(".+,+[:blank:]*", "",
                         tolower(gsub(".*, ","", data$Geography))))
}

DP = function(){
  
  # This function reads in the data for DP02 and DP03, processes
  # a bit, and then merges.
  
  DP02 = read.csv(
    "http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/DP02.csv")
  names(DP02) = readLines(
    "http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/DP02_metadata.txt")
  names(DP02)[3] = "Geography"
  DP02$Geography = Standst(DP02)
  DP02 = order.data(DP02, DP02$Geography)
  
  DP03 = read.csv(
    "http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/DP03.csv")
  names(DP03) = readLines(
    "http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/DP03_metadata.txt")
  names(DP03)[3] = "Geography"
  DP03$Geography = Standst(DP03)
  DP03 = order.data(DP03, DP03$Geography)
  
  DP = merge(DP03, DP02, by = "Geography", all.x=TRUE)
  return(DP)
}

population = function(){
  
  # Loads B01003, processes and sorts population data
  
  B01003 = read.csv(
    "http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/B01003.csv")
  names(B01003) = readLines(
    "http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/B01_metadata.txt")
  names(B01003)[3] = "Geography"
  
  Total.population = B01003[B01003$POPGROUP.id == 1, c(3,6,7)]
  names(Total.population) = c("Geography", "Total.Pop", "Pop.Margin.of.error")
  White.alone = B01003[B01003$POPGROUP.id == 2, c(3,6,7)]
  names(White.alone) = c("Geography", "White.alone.pop",
                          "White.alone.Pop.Margin.of.error")
  Black.or.African.American.alone = B01003[B01003$POPGROUP.id == 4, c(3,6,7)]
  names(Black.or.African.American.alone) = 
    c("Geography", "Black.or.African.American.alone.pop",
      "Black.or.African.American.alone.Pop.Margin.of.error")
  
  MergeOne = merge(Total.population,White.alone, by = "Geography", all=TRUE)
  population = merge(MergeOne, Black.or.African.American.alone, by = "Geography", all=TRUE)
  population$Geography= Standst(population)
  population = order.data(population, population$Geography)
}

Census2010data = function(){
  
  ## Combines DP with population and does some last minute processing
  
  Census2010data = merge(DP(), population(), by = "Geography")
  Census2010data$county = County(Census2010data)
  Census2010data$state = States(Census2010data)
  
  Census2010data[Census2010data$state == "virginia" &
    Census2010data$county != "franklin city", "county"] = 
    gsub(" city", "",
         Census2010data[Census2010data$state == "virginia" &
           Census2010data$county != "franklin city", "county"])
  
  Census2010data$county = gsub(" $", "", Census2010data$county)
  return(Census2010data)
}


Census2010data = Census2010data()
Census2010data$county = cleanCountyNames(Census2010data$county)
Census2010data = CountyFix(Census2010data, "st genevieve", "ste genevieve", "missouri")
Census2010data = AveragerID(Census2010data)[
      c(1:4,length(Census2010data),5:(length(Census2010data)-1))]


##########################################################################
# It's mergin' time!

MasterMerge = function(list){
  # This function takes a list of the data frames created earlier and merges
  
  FinalDF = list[[1]]
  for (i in 2:length(list))   { 
    FinalDF = merge(FinalDF, list[[i]], all = TRUE)  } 
  FinalDF = FinalDF[order(FinalDF$ID), ]
  FinalDF$ID = 1:nrow(FinalDF)
  rownames(FinalDF) = NULL
  return(FinalDF)
}

DataToss = function(states, DF, keep){
  
  # This function takes a vector of state names and tosses out data
  # for those states from data frame DF. It also tosses out many
  # excessive variables - "keep" is a vector containing the columns
  # that are kept.
  
  DF = DF[, keep]
  for(i in 1:length(states)){
    ElimState = states[i]
    DF = DF[DF$state != ElimState, ]
  }
  return(DF)
}


FinalDF = MasterMerge(list(Elec04, results2012, GeoDat, Census2010data))
EducationNums = seq(grep("HC03_VC78,Percent; SCHO", names(FinalDF)),
                      grep("HC03_VC94,Percent; EDUCA", names(FinalDF)), by=4)
FertNums = seq(grep("HC03_VC51,Percent; FERT", names(FinalDF)),
                    grep("HC03_VC52,.Percent; FERT", names(FinalDF)), by=4)
CitizenNums = seq(grep("HC03_VC139,Percent; U.S", names(FinalDF)),
               grep("HC03_VC150,Percent; YEAR OF EN", names(FinalDF)), by=4)
VarsToKeep = c(3:1, 4:9, EducationNums, FertNums, CitizenNums, 21, 41, 73, 85, 159,
               163, 212, 249, 259, 415, 767)
FinalDF = DataToss(c("alaska","puerto rico", "hawaii"), FinalDF, VarsToKeep)
names(FinalDF)[10:38] = c("Gr1-8.Enroll", "HS.Enroll", "College.Enroll",
                          "Adult.Enroll", "Attain.Gr1-8", "Attain.Gr9-12","Attain.HSDip",
                          "Attain.SomeCol", "Attain.Associate", "Attain.Bachelor",
                          "Attain.GradOrProf", "Attain.HSDipAndUp", "Attain.BachAndUp",
                          "BirthLastYearAges15-50", "UnmarriedWomen",
                          "NaturalizedCitizen","NotCitizen","ImmigrateBefore2000",
                          "ForeignBorn","Labor.Force", "Unemployment", "BuisSciArt.Occup",
                          "AgroForestFishMine.Occup", "Median.HouseIncome",
                          "Mean.HouseIncome", "Income10k-", "Income.200k+",
                          "IncomePerCapita", "TotalHouseholds")


#####################END OF PART 2#######################
###knn Part####
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


knnElec = function(k, var, winner, candidates){
  
  ### k is the desired amount of neighbors. var is a vector containing
  ### the names of variables to use in the nearest neighbor analysis.
  ### winner is the name of the vector in FinalDF containing the
  ### winner of the election to be used in the test set
  ###candidates are the number of votes that each candidate got.
  ### Function performs k-nearest neighbor analysis on the election
  
  DFpart2 = na.omit(FinalDF
                    [ , c(candidates, winner, "longitude",
                          "latitude", var)]
  )
  
  DFpart2[,6:(5+length(var))] = normalize.quantiles(as.matrix(DFpart2[,6:(5+length(var))]))
  
  test = DFpart2[ , c("longitude",
                      "latitude", candidates, var)]
  set.seed(222222222)
  kwinner = knn(test, test, DFpart2$winner, k, prob = TRUE)
  return(kwinner)
}
Predicting2004outcome = function(){
  candidates = c("bushVote",  "kerryVote")
  knnVars = c("NaturalizedCitizen", "Attain.HSDipAndUp")
  knnpredict2004 = knnElec(k=3, var=knnVars, "winner2004", candidates)
  knn2004 = data.frame(na.omit(FinalDF)[, c("state", "county", "Total.Pop")], 
                       knnpredict2004,
                                        weighofk = attr(knnpredict2004, "prob"))
  return(knn2004)
  }

knn2004 = Predicting2004outcome()
###plot 1(Part 4)####
barplot(c(sum(as.character(knn2004$knnpredict2004)=="Dem"),
          sum(as.character(knn2004$knnpredict2004)=="Rep")),
        width = c(sum(
          knn2004[knn2004$knnpredict2004=="Dem","Total.Pop"]*
            knn2004[knn2004$knnpredict2004=="Dem","weighofk"],
          (knn2004[knn2004$knnpredict2004=="Rep","Total.Pop"]*
            (1-knn2004[knn2004$knnpredict2004=="Rep","weighofk"]))),
                  sum(
                    knn2004[knn2004$knnpredict2004=="Rep","Total.Pop"]*
                    knn2004[knn2004$knnpredict2004=="Rep","weighofk"],
                    (knn2004[knn2004$knnpredict2004=="Dem","Total.Pop"]*
                    (1-knn2004[knn2004$knnpredict2004=="Dem","weighofk"])))
                  ),
        col= c("#010440", "#A61103"),
        xlab = c("Counties"),
        ylab = c("Number of votes"),
        sub = c("A comparision of the number of counties won by Democrats and Republicans based on variables regarding citizenship and  attainment of at least an undergraduate degree, 
                with the width corresponding to population of the counties multiplied by the k value. 
                This was done in order to see by how much of the population was won by each party."), cex.sub=0.5)


###Plot 2(Part 4)####
Predicting04outcome = function(){
  candidates = c("bushVote",  "kerryVote")
  knnVars = c("ForeignBorn", "BuisSciArt.Occup",
              "AgroForestFishMine.Occup")
  knnpredict2004 = knnElec(k=3, var=knnVars, "winner2004", candidates)
  knn2004 = data.frame(na.omit(FinalDF)[, c("state", "county", "Total.Pop")], 
                       knnpredict2004,
                       weighofk = attr(knnpredict2004, "prob"))
  return(knn2004)
}
knn04 = Predicting04outcome()
barplot(c(sum(as.character(knn04$knnpredict2004)=="Dem"),
          sum(as.character(knn04$knnpredict2004)=="Rep")),
        width = c(sum(
          knn04[knn04$knnpredict2004=="Dem","Total.Pop"]*
            knn04[knn04$knnpredict2004=="Dem","weighofk"],
          (knn04[knn04$knnpredict2004=="Rep","Total.Pop"]*
            (1-knn04[knn04$knnpredict2004=="Rep","weighofk"]))),
                  sum(
                    knn04[knn04$knnpredict2004=="Rep","Total.Pop"]*
                      knn04[knn04$knnpredict2004=="Rep","weighofk"],
                    (knn04[knn04$knnpredict2004=="Dem","Total.Pop"]*
                      (1-knn04[knn04$knnpredict2004=="Dem","weighofk"])))
        ),
        col= c("#010440", "#A61103"),
        xlab = c("Counties"),
        ylab = c("Number of votes"),
        sub = c("Comparing meployment and Foreign Born"))
###End of knn Part and Part 4 plots####

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
dev.off()
# Get predictions
FinalDF$Predictions2004 = as.character(predict(fit, FinalDF[,c(5:6, 9:39)], type="class"))

#####################END OF PART 3######################

### PLOT 1 ###
### Accuracy of RPART predictor ###

correctColor = function(party) {
  ### Takes a character vector with party name "Rep" or "Dem".
  ### Returns blue for Dem, red for Rep. uses Brewer qualitative Hex Codes.
  ifelse(party == "Rep", "#E31A1C", "#1F78B4")
}

incorrectColor = function(party) {
  ### Takes a character vector with party name "Rep" or "Dem".
  ### Returns green if Dem, and purple if Rep. Uses Brewer Hex Codes.
  ifelse(party == "Rep", "#FE9929", "#4DAF4A")
}

predColor = function(predicted, actual) {
  ### predicted is a character vector of the predicted winner ("Rep" or "Dem")
  ### actual is a character vector of the actual 2012 winner ("Rep" or "Dem")
  ### Returns the appropriate hex color code for that county: 
  ###    Blue if correctly predict Dem.  Red if correctly predict Rep.
  ###    Green if predict Rep but Dem won. Beige if predict Dem but Rep won.
  
  color = mapply(function(pred, act) {
    if (is.na(pred) | is.na(act)) {
      "black"
    }
    ifelse(pred == act,
           correctColor(pred),
           incorrectColor(pred))
  }, predicted, actual)
  
  return(color)
}

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
  legend("bottomleft", c("Dem", "Rep", "Rep (predicted Dem)", "Dem (predicted Rep)"),
         fill = c("#E31A1C", "#1F78B4", "#FE9929", "#4DAF4A"))
  points(FinalDF$longitude/1000000, FinalDF$latitude/1000000, col=color2, pch=20)
  dev.off()  
}

## Create pred map for recursive partitioning
createPredMap(FinalDF$Predictions2004, FinalDF$winner2012, 
              "rpartPredMap.pdf",
              "Recursive Partitioning",
              paste("Our Recursive Partitioning predictor tended to incorrectly predict:",
                    "Democrat wins in counties where Republicans won."))

## Create pred map for knn
knnVars = c("ForeignBorn", "BuisSciArt.Occup", "AgroForestFishMine.Occup", "Total.Pop")

createPredMap(knn04$knnpredict2004, na.omit(FinalDF[, c("bushVote","kerryVote", 
                                                        "winner2012", "longitude", 
                                                        "latitude", knnVars)])$winner2012, 
              "knnPredMap.pdf",
              "K-Nearest Neighbor",
              paste("Our KNN predictor tended to incorrectly predict:",
                    "Democrat wins in counties where Republicans won."))


### ARROW PLOT ###

makeArrowMap = function () {
  ### creates the arrow map. Code to plot-to-PDF is commented-out; replace with a 
  ### valid pathname and uncomment to see the PDF.
  
  library(maps)
  
  mapsDF = FinalDF[, c("state", "county", "bushVote", "kerryVote", "obama.votes", 
                       "romney.votes", "latitude", "longitude")]
  mapsDF = na.omit(mapsDF, "bushVote", "longitude")
  mapsDF$longitude = mapsDF$longitude/1000000
  mapsDF$latitude = mapsDF$latitude/1000000
  
  getArrowLength = function(rep2004, dem2004, rep2012, dem2012) {
    ## Should be passed bushVote, kerryVote, romney.votes, obama.votes.
    ## Returns the length of the arrow in the x-direction (normalized).
    repProportion2004 = rep2004 / (rep2004 + dem2004)
    repProportion2012 = rep2012 / (rep2012 + dem2012)
    return(repProportion2012 - repProportion2004)
  }
  
  mapsDF$arrowLength = 5 * with(mapsDF, getArrowLength(bushVote, kerryVote, 
                                                       romney.votes, obama.votes))
  mapsDF$arrowLength[abs(mapsDF$arrowLength) < 0.01] = 0.01
  
#  pdf(file="~/Dropbox/Fall Semester 2012/Stat 133/Project_Final/arrowMap.pdf",
#      width = 9.9, height = 7.65)
  
  map("county", fill=TRUE, col="#BDBDBD44", border="white")
  title(main = "Vote Shift from 2004 to 2012", 
        sub = paste("Arrow length corresponds to proportion of change. Notice that", 
                    "although there are\nlong red arrows indicating a strong shift",
                    "toward voting Republican in those areas, \nthe blue arrows occur in",
                    "critical 'swing states' which play an important role electorally.",
                    "\nVirginia data were unavailable."))
  map("state", fill=TRUE, col="#BDBDBD44", add=TRUE, border="white", lwd=2)
  
  invisible(mapply(function(lon, lat, arrLen) {
    arrows(lon, lat, lon+arrLen, lat+abs(arrLen), length=0.025, angle=45,
           col=ifelse(arrLen>0, "red", "blue"))
  }, mapsDF$longitude, mapsDF$latitude, mapsDF$arrowLength))
  
#  dev.off()
}
#makeArrowMap()

#####################END OF PART 4#######################