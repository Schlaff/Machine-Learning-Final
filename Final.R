#clear work environment
rm(list=ls())

#set working directory 
setwd("C:/Users/brian.schlaff/OneDrive - Apex Companies, LLC/DT_Desktop/Final")

#Load Building Data and examine it
Building <- read.csv("Existing_Buildings_Energy___Water_Efficiency__EBEWE__Program.csv",header=T, na.strings=c("","NA"))
names(Building)
dim(Building)

sum(is.na(Building$Tree_Canopy_in_Urban_and_Non-Urban_LA_County__2014_)) # determine the amount of cells to omit

#remove NA 
Building <- na.omit(Building)
dim(Building)

#Change all Not Classified strings to zero in Energy Star score column
library(plyr)
Building$ENERGY.STAR.SCORE <- mapvalues(Building$ENERGY.STAR.SCORE,
                     from = "Not Available",
                     to = 0)

#Change all of the addresses to Block Group and add it to an empty list
library(censusr)
zip <- read.csv("ZIP_Codes_and_Postal_Cities.csv")
BlockID=list()

for(i in 16610:length(Building$ï..BUILDING.ADDRESS)){
  x=toString(Building$ï..BUILDING.ADDRESS[i])
  for(j in 1: length(zip$ZIP.Code)){
    if(zip$ZIP.Code[j]==Building$POSTAL.CODE[i]){
      y=toString(zip$Postal.City.1[j])
    }
  }
  BlockID[[i]] = as.numeric(call_geolocator(street = x , city = y, state = "CA"))
}

sum(is.na(BlockID))

#remove last four digits from blockid to convert from block group to census tract
CensusTract <- substr(BlockID,1,11)
CensusTract<-as.numeric(CensusTract)

#append Census Tract ID to the Building dataframe
buildings <-cbind(Building,CensusTract)

#omit the NA values
buildings <- na.omit(buildings)

#export to csv to save
write.csv(buildings, "buildings.csv", row.names = FALSE)

buildings<-read.csv("buildings.csv")

#Load Tree Data
Tree <- read.csv("Tree_Canopy_in_Urban_and_Non-Urban_LA_County__2014_.csv")
names(Tree)
dim(Tree)

sum(is.na(Tree))

#Remove NA values from Tree
Tree<-na.omit(Tree)
dim(Tree)


# Loop through the census tract column of each datasets, link them, and append to empty dataframe
buildings[,"Can_P"] <- NA


total = as.numeric(length(Tree$GEOID10))*(length(buildings$CensusTract))
count = 0

for(ii in 1:length(buildings$CensusTract)){
  B = buildings$CensusTract[ii]
  for(jj in 1:length(Tree$GEOID10)){
    C = Tree$GEOID10[jj]
    
    count = count + 1
    percent = 100*(count/(total))
    print(percent)
    
    if(B==C){
      perc=Tree$Can_P[jj]
      buildings$Can_P[ii]=perc
    }
  }
}

#remove NA values from unmatched GEOID areas
buildings<- na.omit(buildings)

#Export the Final Combined dataframe
write.csv(buildings,"buildings.csv",row.names = FALSE)

### EDA


