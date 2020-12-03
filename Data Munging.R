# Data Munging

data <- read.csv("buildings.csv")
summary(data)


#Create a new dataset with the independence and dependence variables
data1 <- data.frame(data$CARBON.DIOXIDE.EMISSIONS.Metric.Ton.CO2e., 
                       data$ENERGY.STAR.SCORE, 
                       data$Can_P, 
                       data$YEAR.BUILT, 
                       data$SITE.ENERGY.USE.INTENSITY..EUI...kBtu.ftÂ..,
                       data$Source.EUI..kBtu.ftÂ.., 
                       data$OCCUPANCY, 
                       data$CensusTract, 
                       data$NUMBER.OF.BUILDINGS, 
                       data$GROSS.BUILDING.FLOOR.AREA..ftÂ..,
                       data$PROPERTY.TYPE)


#Correct the names
names(data1) <- c("CO2.emission", 
                     "energy.star.score",
                     "Can.P",
                     "year.built",
                     "site.energy.usuage", 
                     "source.energy.usage", 
                     "occupancy",
                     "census.tract",
                     "number.of.buildings",
                     "gross.building.floor.area",
                     "property.type")

summary(data1)
str(data1)
#Create binary data

# Save the levels from ocean_proximity  factor variable
property_type_level <- levels(data1$property.type)
property_type_level

#build a logical map of proximity variable based on passed value
binaryType <- function(c) {return(data1$property.type == c)}


#use sapply to loop through all levels
newVars <- sapply(property_type_level, binaryType)
newVars

