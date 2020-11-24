#Final Project EDA Part

setwd("~/Desktop/Machine Learning/Final Project")

#Import the Data
#-------------------------------------------------------------------------------------
data <- read.csv("buildings.csv")
summary(data)


#Create a new dataset with the independence and dependence variables for EDA
data.eda <- data.frame(data$CARBON.DIOXIDE.EMISSIONS..Metric.Ton.CO2e., 
                       data$ENERGY.STAR.SCORE, 
                       data$Can_P, data$YEAR.BUILT, 
                       data$SITE.ENERGY.USE.INTENSITY..EUI...kBtu.ftÂ..,
                       data$Source.EUI..kBtu.ftÂ.., 
                       data$OCCUPANCY, 
                       data$POSTAL.CODE, 
                       data$NUMBER.OF.BUILDINGS, 
                       data$GROSS.BUILDING.FLOOR.AREA..ftÂ..,
                       data$PROPERTY.TYPE)
                    
names(data.new)

#Correct the names
names(data.eda) <- c("CO2.emission", 
                     "energy.star.score",
                     "Can.P",
                     "year.built",
                     "site.energy.usuage", 
                     "source.energy.usage", 
                     "occupancy",
                     "postal.code",
                     "number.of.buildings",
                     "gross.building.floor.area",
                     "property.type")

str(data.eda)
summary(data.eda)


#EDA
#---------------------------------------------------------------------------------------

#Histogram 

#Transform factor variables to numeric
CO2.emission<- as.numeric(data.eda$CO2.emission)
energy.star.score<-as.numeric(data.eda$energy.star.score)
canopy<- data.eda$Can.P
year.built<-as.numeric(data.eda$year.built)
site.enery.usage <-as.numeric(data.eda$site.energy.usuage)
source.energy.usage <- as.numeric(data.eda$source.energy.usage)
occupancy <- as.numeric(data.eda$occupancy)
postal.code <-as.numeric(data.eda$postal.code)
number.of.buildings<-as.numeric(data.eda$number.of.buildings)
gross.building.floor.area <- data.eda$gross.building.floor.area

data.num<- data.frame(CO2.emission, energy.star.score, canopy,year.built,
                       site.enery.usage, source.energy.usage, occupancy,
                       postal.code, number.of.buildings, 
                      gross.building.floor.area)

hist(data.num$CO2.emission, freq=TRUE, col="blue")
hist(data.num$energy.star.score, freq=TRUE, col="blue")
hist(data.num$canopy, freq=TRUE, col="blue")
hist(data.num$year.built, freq=TRUE, col="blue")
hist(data.num$site.enery.usage, freq=TRUE, col="blue")
hist(data.num$source.energy.usage, freq=TRUE, col="blue")
hist(data.num$occupancy, freq=TRUE, col="blue")
hist(data.num$postal.code, freq=TRUE, col="blue")
hist(data.num$number.of.buildings, freq=TRUE, col="blue")
hist(data.num$gross.building.floor.area, freq=TRUE, col="blue")


#Correlation Plot
#correlation of numeric variables

#install.packages("corrplot")
library(corrplot)
n<- cor(data.num)
corrplot(n, method="number")
#didn't include property type because factor doesn't make sense. 

#Scatterplots 
#Note: pls not to run the following code together, it takes really long time

plot(data.eda$CO2.emission, data.eda$energy.star.score, pch=19, col="grey")
plot(data.eda$CO2.emission, data.eda$Can.P,pch = 19, col= "grey")
plot(data.eda$CO2.emission, data.eda$year.built, pch = 19, col = "grey")
plot(data.eda$CO2.emission, data.eda$site.energy.usuage, pch = 19, col = "grey")
plot(data.eda$CO2.emission, data.eda$source.energy.usage, pch = 19, col = "grey")
plot(data.eda$CO2.emission, data.eda$occupancy, pch = 19, col = "grey")
plot(data.eda$CO2.emission, data.eda$postal.code, pch = 19, col = "grey")
plot(data.eda$CO2.emission, data.eda$number.of.buildings, pch = 19, col = "grey")
plot(data.eda$CO2.emission, data.eda$gross.building.floor.area, pch = 19, col = "grey")
plot(data.eda$CO2.emission, data.eda$property.type, pch = 19, col = "grey")

#scatterplots don't show clear relationship between each independent variables to dependent
#variables, maybe we can get rid of the scatterplots


#Box Plots 

boxplot(data.num$CO2.emission,
        main = "Mean CO2 Emission",col = "grey",notch = TRUE)

boxplot(data.num$energy.star.score,
        main = "Mean Energy Star Score",col = "grey",notch = TRUE)

boxplot(data.num$canopy,
        main = "Mean Percentage of Canopy Area ",col = "grey",notch = TRUE)
boxplot(data.num$year.built,
        main = "Mean Year Built",col = "grey",notch = TRUE)

boxplot(data.num$site.enery.usage,
        main = "Mean Site Energy Usage",col = "grey",notch = TRUE)

boxplot(data.num$source.energy.usage,
        main = "Mean Source Energy Usage",col = "grey",notch = TRUE)

boxplot(data.num$postal.code,
        main = "Postal Code",col = "grey", notch = TRUE)
boxplot(data.num$number.of.buildings,
        main = "Mean Number of Buildings",col = "grey",notch = TRUE)

boxplot(data.num$gross.building.floor.area,
        main = "Mean Gross Building Floor Area",col = "grey",notch = TRUE)

#factor seems not working in boxplot
#boxplot(data.new$property.type,
#       main = "Property Type",col = "grey",notch = TRUE)

#tired to compare the two boxplot side by side but failed. 
#boxplot(data.num$CO2.emission ~ data.num$data.new.gross.building.floor.area,
#               main = "Mean Gross Building Floor Area",col = "grey",notch = TRUE)


