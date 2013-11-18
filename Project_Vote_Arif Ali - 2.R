###The Averagers
###begining of Census 2010 data


Geography= function(data){
  gsub("(St.|st.|st)", "st", data$Geography)
}

order.data = function(data, column){
  data[order(column), ]
}

County = function(data){
tolower(gsub(
  "(County|Parish|Municipio|Municipality|Municip)*,.*$", "",
                 data$Geography))
}

States = function(data){
  data$State = gsub(" westvirginia", " west virginia", 
                    gsub(".+,+[:blank]*", "", tolower(gsub(".*, ","", data$Geography))))
}

DP = function(){
  ## Read in the data
  DP02 = read.csv("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/DP02.csv")
  ## Set the names for the data
  names(DP02) = readLines("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/DP02_metadata.txt")
  ## Make the Greography column have a friendlier name
  names(DP02)[3] = "Geography"
  DP02$Geography = Geography(DP02)
  DP02 = order.data(DP02, DP02$Geography)
  
  DP03 = read.csv("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/DP03.csv")
  names(DP03) = readLines("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/DP03_metadata.txt")
  names(DP03)[3] = "Geography"
  DP03$Geography = Geography(DP03)
  DP03 = order.data(DP03, DP03$Geography)
  
  DP = merge(DP03, DP02, by = "Geography", all.x=TRUE)
  return(DP)
}

population = function(){
  B01003 = read.csv("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/B01003.csv")
  names(B01003)=gsub(".+,+[:alnum:]*", "",
                     readLines(
                       "http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/B01_metadata.txt")
                     )
  Total.population = data.frame(B01003[B01003$POPGROUP== "Total population", c(3,6,7)])
  names(Total.population) = c("Geography", "Total.Pop", "Pop.Margin.of.error")
  White.alone = data.frame(B01003[B01003$POPGROUP== "White alone", c(3,6,7)])
  names(White.alone) = c("Geography", "White.alone.pop","White.alone.Pop.Margin.of.error")
  Black.or.African.American.alone = data.frame(B01003[B01003$POPGROUP ==
    "Black or African American alone", c(3,6,7)])
  names(Black.or.African.American.alone) = 
    c("Geography", "Black.or.African.American.alone.pop",
      "Black.or.African.American.alone.Pop.Margin.of.error")
  population = merge(
    merge(Total.population,
                         White.alone,
                         by = "Geography", all=TRUE),
                         Black.or.African.American.alone,
                         by = "Geography", all=TRUE)
  population$Geography= Geography(population)
  population = order.data(population, population$Geography)
}

Census2010data = function(){
  Census2010data = merge(DP(), population(), by = "Geography")
  Census2010data$County = County(Census2010data)
  Census2010data$States = States(Census2010data)
  return(Census2010data)
}
###end of Census 2010 Data