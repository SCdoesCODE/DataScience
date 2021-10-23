#1 - Inspection of data
titanic <- read.csv(file = "/Users/Sabrina/Downloads/titanic.csv",header=TRUE, sep =",")
titanic<-subset(titanic,select=-X)
head(titanic)
summary(titanic)
plot(titanic)
#Which variables are quantitative and which variables are categorical?
#How can we know it?

#Answer - we look at the summary and see that categorical variables have a length
#,class and mode meanwhile the quantitative variable Freq has some stats on e.g
#min, median, mean etc. 

#2 - Working with basic graphics
#Download the file “cars.csv” from poliformat. This file contains 
#information about the speed and stopping distances of cars.

cars <- read.csv(file = "/Users/Sabrina/Downloads/cars.csv",header=TRUE, sep =",")

#2.1 Make a plot of the distance field in terms of the speed field (use
#the $ syntax).

distance <- cars$dist

speed <- cars$speed

plot(distance ,speed)

#2.2 Make a histogram of the distance variable.

hist(distance)

#2.3 Make a histogram of the speed variable.

hist(speed)

#2.4 Modify the previous plots to show the name of the variables
#(“speed” or “distance”) as the title of the axis. Change the title
#of the three graphics, and also use colours for the histograms and
#titles. Save the new graphics as pdf files.

pdf(file="dist_speed_plot.pdf")
plot(distance ,speed, main = " ", col = "Magenta", xlab="Distance", ylab="Speed")
title("Distance vs Speed", col.main = "Magenta")
dev.off()

pdf(file="dist_hist.pdf")
hist(distance,main =" " ,col = "Green",xlab="Distance")
title("Distance", col.main = "green")
dev.off()

pdf(file="speed_hist.pdf")
hist(speed,main = " ",col = "Blue", xlab="Speed")
title("Speed", col.main = "Blue")
dev.off()

#3 Transformations of variables and datasets.
#Remove the first column of the cars data frame. 
cars
cars[,1] <- NULL
cars
#Now, assume that data from two more cars are made available:
#3.1 Construct a new data frame with the above data.
speed <- c(21,38)
dist <- c(47,87)
df <- data.frame(speed, dist)
df
#3.2 Add the constructed data frame to the cars data frame.
cars <- rbind(cars,df)
cars
#3.3 Sort the data in the resulting dataset by column speed (ascending). 
#There is two ways to do it: using the order() command or
#combining the with and the order() commands. (Suggestion:
#to search on the internet “how to sort a data frame by columns”).
ordered_idx <- order(cars$speed)
ordered_idx
cars[ordered_idx,]
#Exercise 4: Data manipulation. Download the file “airquality.csv” from
#poliformat . This dataset contains some New York air quality mea-
#surements.

#1 Extract the first 2 rows of the data frame and print them to the
#console. What does the output look like?
aq <- read.csv(file = "/Users/Sabrina/Downloads/airquality.csv",header=TRUE, sep =",")
aq[1:2,]
#2. How many observations (i.e., rows) there are in this data frame?
nrow(aq)
#3. What is the value of Ozone in the 40th row?
aq$Ozone[40]
#4. How many missing values there are in the Ozone column of this
#data frame?

#both of these work
sum(is.na(aq$Ozone))
nrow(subset(aq,is.na(Ozone)))

#5. What is the mean of the Ozone column in this dataset? Exclude
#missing values (coded as NA) from this calculation.

non_na_ozone <- aq$Ozone[!is.na(aq$Ozone)]
mean(non_na_ozone)

#6. Extract the subset of rows of the data frame where Ozone values
#are above 31 and Temp values are above 90. What is the mean
#of Solar.R in this subset?
sub <- subset(aq,Ozone > 31 & Temp > 90)
mean(sub$Solar.R)

#Exercise 5: Data transformation (2).

#1. Discretise the Ozone column into five bins (‘bin1’, ‘bin2’, ...) of
#equal width and a sixth bin (‘binNA’) for NA.
mini <- min(non_na_ozone)
maxi <- max(non_na_ozone)
width <- maxi/5
non_na_ozone[non_na_ozone<width]
non_na_ozone <- sort(non_na_ozone)
for (idx in 1:4){
  bin <- non_na_ozone[non_na_ozone>width*idx & non_na_ozone<width*(idx+1)]
  print("----------")
  print(bin)
} 

bin_NA <- aq$Ozone[is.na(aq$Ozone)]
bin_NA

#2. Discretise the Solar column into four bins of equal size and a fifth
#bin for NA.

bin_size <- length(non_na_ozone)/4
non_na_ozone[1:bin_size]
for (idx in 1:3){
  bin <- non_na_ozone[(bin_size*idx) : (bin_size*(idx+1))]
  print("----------")
  print(bin)
} 

bin_NA

#3. Create a new column AbsDay from the columns Month and Day
#such that counts the number of days passed from Month=5 and
#Day=1. - so days between given day and the 1st of May

#the airquality table seems to be covering every single day so we
#can just make a vector with values between 1 and length(aq$Day)

aq$AbsDay <- c(1:length(aq$Day))
aq


#Exercise 6: Data transformation (3).

#1. Numerise the class column, where Crew=4, 1st=3, 2nd=2 and
#3rd=1.
titanic$Class[titanic$Class=="Crew"] <- 4
titanic$Class[titanic$Class=="1st"] <- 3
titanic$Class[titanic$Class=="2nd"] <- 2
titanic$Class[titanic$Class=="3rd"] <- 1
titanic
#2. Transform the titanic data frame into a new data frame (titanic2)
#with as many examples as passengers using the Freq column. In
#other words, there should be no rows for those for which Freq=0
#and there should be 35 replicated rows for those with Freq=35.

#rep(values, n_times)
#seq generates a sequence from a certain nr to another nr (Default from 1)
titanic2 <- data.frame(titanic[rep(seq(nrow(titanic)), titanic$Freq),])
#omit the last frequency column
titanic2[,1:5]

#3. Compare the plots of the original titanic data frame with the new
#one.
plot(titanic)
plot(titanic2)
#no difference

#Exercise 7: Data selection.

#1. Calculate a correlation matrix for the air dataset. Do you see a
#pair of attributes that are redundant?
cor(airquality, use="complete.obs")
#the deault method is the pearson correlation coefficient which measures 
#the linear dependencies between two variables 
#so when we see a large value it means that the two attributes are highly
#correlated meaning that one attribute can determine the other
#this means we can drop one of the attributes without information loss
#meaning that one of the attributes are redundant
#this is the case for pairs of identical columns

#seems like there are no redundant variables as the only high values
#in absolute terms (so -1 or 1) are in the diagonal


#Calculate a correlation matrix for the cars dataset. Do you see a
#pair of attributes that are redundant?
cor(cars, use="complete.obs")
#both attributes seem to be highly correlated, but none seem to be redundant

#3. Using the data frame ‘air’, perform a simple random sampling of
#50 examples.
airquality[sample(nrow(airquality), 50), ]

#4. Using the data frame ‘air’, perform a stratified random sampling
#of 5 examples of each month.

#stratified random sampling - a technique to to help us create a statistically
#significant sample from a large dataset. 

#first extract the monthly data in separate dataframes
mon5 <- airquality[airquality$Month == 5,]
mon6 <- airquality[airquality$Month == 6,]
mon7 <- airquality[airquality$Month == 7,]
mon8 <- airquality[airquality$Month == 8,]
mon9 <- airquality[airquality$Month == 9,]
#lists can hold dataframes
months <- list(mon5,mon6,mon7,mon8,mon9)

#do random sampling on each separate dataframe
for (mon_idx in 1:length(unique(airquality$Month))){
  mon <- data.frame(months[mon_idx])
  sample <- mon[sample(nrow(mon), 5), ]
  print(sample)
} 
  
  


