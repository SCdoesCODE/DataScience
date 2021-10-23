#Generate the numbers 1, 2,. . ., 12, and store the result in the vector x.
x <- 1:12
x
#Generate four repetitions of the sequence of numbers (6, 2, 4).
gl (3 ,4 , labels = c (6,2,4))
#Generate the sequence consisting of six 9s, then five 2s, and finally four 
#5s. Store the numbers in a 5 by 3 matrix (populating it columnwise).
matrix(data = c(9,9,9,9,9,9,2,2,2,2,2,5,5,5,5), nrow = 5, ncol = 3, byrow = TRUE)
#Generate a vector consisting of 20 numbers generated randomly from
#a normal distribution. Use the value 100 as seed
set.seed(100)
normv <- rnorm(20, mean=0, sd=1)
#Then, calculate the following statistics about the generated vector:
#mean, median, variance and the standard deviation.
#Repeat the generation of the vector and the statistics with and without
#changing the seed and observe what happens.
mean(normv);
median(normv);
var(normv);
sd(normv);
#Answer - the same seed generates the same results

#From the resources folder at poliformat, download the file “data1.txt”
#that contains information about students.

#Read the data into an R object named students (data is in a
#space-delimited text file and there is no header row).
students <- data.frame(read.table("/Users/Sabrina/Downloads/data1.txt",header = TRUE))
students
#Print the header names only.
head(students, 0)
#Print the column height.
students$height

#What is the gender distribution (how many observations are in
#each group) and the distribution of sampling sites (column population)?
gender <- students[,3]
table(gender)
table(students[,4])
#Show the distributions in the above item at the same time by
#using a contingency table.
genfactor <- factor(students[,3])
popfactor <- factor(students[,4])
table(genfactor,popfactor)
#Make two subsets of your dataset by splitting it according to gen-
#der. Use data frame operations first and then do the same using
#the function subset. Use the help to understand how subset works.
?subset
split(students, students$gender)
subset(students, gender == "female")
subset(students, gender == "male")
#Make two subsets containing individuals below and above the
#median height. Use data frame operations first and then do the
#same using the function subset.
med <- median(students[,1])
students[students$height<med]
students[students$height>=med]
subset(students, height < median(students[,1]))
subset(students, height >= median(students[,1]))
#Change height from centimetres to metres for all rows in the
#data frame. Do this using in three different ways: with basic
#primitives, a loop using for and the function apply.
students$height <- students$height/100
students

for (h in 1:nrow(students)){
  students[h,1] <- students[h,1]/100
} 
students

students[,1] <- students[,1]/100
students
#Plot height against shoesize, using blue circles for males and magenta 
#crosses for females. Add a legend.

females <- subset(students, gender == "female")
males <- subset(students, gender == "male")

heightfem <- females$height
heightmale <- males$height

shoesizefem <- females$shoesize
shoesizemale <- males$shoesize

minshoesize <- min(students$shoesize)
minheight <- min(students$height)
maxshoesize <- max(students$shoesize)
maxheight <- max(students$height)

shoesizerange <- c(minshoesize,maxshoesize)
heightrange <- c(minheight,maxheight)

plot(heightfem ,shoesizefem,pch = 4, col="magenta",ylab="",xlab="",xlim = heightrange,ylim=shoesizerange)
points(heightmale ,shoesizemale,pch = 1, col="blue",ylab="",xlab="",xlim = heightrange,ylim=shoesizerange)
legend(x = "bottomright", legend = c("female","male"),col = c("magenta","blue"),pch = c(4,1))
title(main="Height vs Shoesize",xlab="height", ylab="shoesize")



