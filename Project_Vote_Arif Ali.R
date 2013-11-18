    Standst= function(data){
      gsub("(St.|st.|st)\\>", "st", data$Geography)
      ###resolved the issue of the period in saint which allowed for consistency 
    }
    
    order.data = function(data, column){
      data[order(column), ]
      ###just an easy helper function that helps organize the data
  }
  
  County = function(data){
  tolower(gsub(
    "(County|Parish|Municipio|Municipality|Municip)*,.*$", "",
                   data$Geography))
  ###strips the name down to just the unique name and lower case for a uniformity
  }
  
  States = function(data){
    data$State = gsub(" westvirginia",
                          " west virginia", 
                          gsub(".+,+[:blank:]*", "",
                               tolower(gsub(".*, ","", data$Geography))))
  ###Weird issue regarding West Virginia compared to the WeSTVirginia as well as getting the state's name
    }
  
    DP = function(){
      ## Read in the data
      DP02 = read.csv("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/DP02.csv")
      ## Set the names for the data
      names(DP02) = readLines("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/DP02_metadata.txt")
      ## Make the Geography column have a friendlier name
      names(DP02)[3] = "Geography"
      DP02$Geography = Standst(DP02)
      DP02 = order.data(DP02, DP02$Geography)
      ## helped with the merging
      DP03 = read.csv("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/DP03.csv")
      names(DP03) = readLines("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/DP03_metadata.txt")
      names(DP03)[3] = "Geography"
      DP03$Geography = Standst(DP03)
      DP03 = order.data(DP03, DP03$Geography)
      ## helped with the merging
      DP = merge(DP03, DP02, by = "Geography", all.x=TRUE)
      return(DP)
    }
  
  population = function(){
    B01003 = read.csv("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/B01003.csv")
    names(B01003)=readLines("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/B01_metadata.txt")
    names(B01003)[3] = "Geography"
    ###helped with merging the date with DP
    Total.population = B01003[B01003$POPGROUP.id == 1, c(3,6,7)]
    names(Total.population) = c("Geography", "Total.Pop", "Pop.Margin.of.error")
    ###the data was organized badly so it was changed so that the rows wouldn't repeat
    White.alone = B01003[B01003$POPGROUP.id == 2, c(3,6,7)]
    names(White.alone) = c("Geography", "White.alone.pop","White.alone.Pop.Margin.of.error")
    ###same as Total. population
    Black.or.African.American.alone = B01003[B01003$POPGROUP.id == 4, c(3,6,7)]
    names(Black.or.African.American.alone) = 
      c("Geography", "Black.or.African.American.alone.pop",
        "Black.or.African.American.alone.Pop.Margin.of.error")
    ###same as Total. population
    MergeOne = merge(Total.population,White.alone,by = "Geography", all=TRUE)
    population = merge(MergeOne, Black.or.African.American.alone, by = "Geography", all=TRUE)
    population$Geography= Standst(population)
    population = order.data(population, population$Geography)
    ###had to remake it into a data frame so it could be merged with DP
  }
  
  Census2010data = function(){
    Census2010data = merge(DP(), population(), by = "Geography")
    ###merges all the data
    Census2010data$county = County(Census2010data)
    ###adds county id
    Census2010data$state = States(Census2010data)
    ###adds state id
    Census2010data[Census2010data$state=="virginia" & Census2010data$county != "franklin city", "county"] = 
    gsub(" city", "",
         Census2010data[Census2010data$state=="virginia" & Census2010data$county!="franklin city", "county"])
    ###issue with 'city' in virginia compared to rest of data so word was remove with exception of franklin city
    Census2010data$county = gsub(" $", "", Census2010data$county)
    ###spaces at end, removed.
    return(Census2010data)
  }
    
  Census2010data = Census2010data()
  AveragerID(Census2010data)