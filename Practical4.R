library("ggplot2")
library("plyr") # for replace() and other functions (optional)
library("rgdal") # for readOGR and other vector maps and spatial objects
library("maptools") # for fortify
library("gpclib")


#2. We’ll read the file with data expectancy information (whodata.csv) and we will
#assign it to a variable “mydata”. Note that the file has a second row with
#description that has to be skipped.
mydata = read.csv("/Users/Sabrina/Downloads/data_practical_4/whodata.csv",header= T)[-1,]
#3. We’ll check that everything is ok in “mydata”, using head(), names() or
#summary()
head(mydata) 
summary(mydata)
#4. In “mydata”, we’ll rename the variable “Country” (or X if you didn’t skip the
#dummy row) as “sovereignt” with the objective of being able to execute, in
#step 8 below, a join between the health data and the geographical data.
colnames(mydata)[1] <- "sovereignt"
head(mydata)
#5. We read the file with the map and assign it to the variable “myworld”, using
#the instruction readOGR(). Use the as source the current directory: "." and as
#the layer the name without extension: "ne_110m_admin_0_countries".
myworld = readOGR("/Users/Sabrina/Downloads/data_practical_4/")
#https://mxd.codes/articles/what-is-a-shapefile-shp-dbf-and-shx
#we just have to type in the directory where the shapefiles reside

#6. We give format to the variable “myworld” to make it understandable by
#ggplot()

#a. First, we make it a data frame with name “myworld.f” from the variable
#“myworld” using the function fortify() and setting the “region”
#parameter equal to “sov_a3”.

myworld.f = fortify(myworld,region = "sov_a3")
#got error : Error: isTRUE(gpclibPermitStatus()) is not TRUE
#https://stackoverflow.com/questions/38776102/issue-fortifying-shapefile


