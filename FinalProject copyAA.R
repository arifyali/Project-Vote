### Project 4 - The Averagers ###

# Names: Matthew Iannone, Arif Ali, Teresa Tenfelder, Sean Linehan



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
      "jeff davis", "jefferson davis", geoDat[[i]]$county)
  }
  geoDat[[47]]$county = gsub(" city", "%", geoDat[[47]]$county)
  geoDat[[47]]$county = gsub("anklin%", "anklin city", geoDat[[47]]$county)
  geoDat[[47]]$county = gsub("%", "", geoDat[[47]]$county)
  
  return(geoDat)
}


ToDF = function(Geolist){
  
  # This function takes a list of data frames (Geolist) and merges them
  
  DF = merge(Geolist[[1]], Geolist[[2]], all=TRUE)
  for (i in 1:(length(Geolist)-2)){
    DF=merge(DF, Geolist[[i+2]], all=TRUE)
  }
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
  elec04$county[elec04$county == "dade" & elec04$state == "florida"] = "miami-dade"
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
HouseholdNums = seq(grep("HC03_VC51,Percent; FERT", names(FinalDF)),
                    grep("HC03_VC52,.Percent; FERT", names(FinalDF)), by=4)
RelationNums
VarsToKeep = c(3:1, 4:9, EducationNums, HouseholdNums, 21, 41, 73, 85, 159, 163,
               249, 259, 415, 767)
FinalDF = DataToss(c("alaska","puerto rico", "hawaii"), FinalDF, VarsToKeep)
names(FinalDF)[10:30] = c("Gr1-8.Enroll", "HS.Enroll", "College.Enroll",
                          "Adult.Enroll", "Attain.Gr1-8", "Attain.Gr9-12","Attain.HSDip",
                          "Attain.SomeCol", "Attain.Associate", "Attain.Bachelor",
                          "Attain.GradOrProf", "Attain.HSDipAndUp", "Attain.BachAndUp",
                          "BirthLastYearAges15-50","UnmarriedWomen"
                          "Labor.Force", "Unemployment", "BuissSciArt.Occup",
                          "AgroForestFishMine.Occup", "Median.HouseIncome",
                          "Mean.HouseIncome", "Income.200k+", "IncomePerCapita")


#####################END OF PART 1#######################
winningparty = function(Dem, GOP){
  ifelse(GOP>Dem, "Rep",
         ifelse(GOP!=Dem, "Dem", NA))
}
FinalDF$winner2004 = winningparty(FinalDF$kerryVote ,FinalDF$bushVote)
FinalDF$winner2012 = winningparty(FinalDF$obama.votes ,FinalDF$romney.votes)

test = na.omit(FinalDF[,-c(1:7,32,33)])

train=test
cl = FinalDF$winner2004
knn(train, test, cl, k = , l = 0, prob = FALSE, use.all = TRUE)
