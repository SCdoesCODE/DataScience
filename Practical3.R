#install.packages("ggplot2")

QC <- read.csv(file = "/Users/Sabrina/Downloads/Queratocono.csv",header=TRUE, sep =",")
QC
summary(QC)
library("ggplot2")

#1. Study the relation between K1 and K2 with smoother (by default and using
#linear regression). The plots should look like those shown in Figure 1.

ggplot(QC, aes(x = K1, y = K2)) + 
  geom_point() +
  stat_smooth(col = "blue")

ggplot(QC, aes(x = K1, y = K2)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "blue")

#2. Study the relation between K1 and K2 distinguishing by factor na,
#according to what is shown in Figure 2 (note the title).

ggplot(QC, aes(x = K1, y = K2)) + 
  ggtitle("Relation between K1 and K2") + 
  geom_point(aes(col=factor(na))) + #change color based on column na
  stat_smooth(method = "lm",aes(col=factor(na)))

#3. Study the relation between K1 and K1.salida (see Figure 3).

ggplot(QC, aes(x = K1, y = K1.salida)) + geom_point() 

#4. Build a histogram in terms of grosor (note that grosor should be taken as a
#factor) of the inserted ring (see Figure 4).

qplot(QC$grosor, geom="histogram",binwidth = 15, fill = factor(QC$na))

#5. Build a scatter plot of the relation between K1 and K2 with “faceting” in
#terms of the parameters diam and na, by assigning different colours to the
#points according to the thickness (grosor) of the ring. In order to visualise
#all points correctly use a transparency of value 1/3 (see Figure 5).

ggplot(QC, aes(x = K1, y = K2)) + 
  geom_point(aes(col=factor(grosor)),alpha = 1/3) +
  facet_grid(vars(QC$diam), vars(QC$na)) #vars supplies variables from the dataset

#6. Create two boxplots that show a summary of the distributions of K1 and K2
#(separately) with respect to the thickness (grosor) as shown Figure 6.

ggplot(QC,aes(x=factor(grosor), y=K1)) + 
  geom_boxplot()

ggplot(QC,aes(x=factor(grosor), y=K2)) + 
  geom_boxplot()

