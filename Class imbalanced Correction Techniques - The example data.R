#======================================================================================================================
# THE EXAMPLE DATA
#======================================================================================================================

#The Example data is created from the popular 'iris' data.
#From the 'iris' data we have considered only teo Species: 'virginica' and 'versicolor'
#Only two variables are considered (So that we can visualize the workings in 2D)
#The variables considered are - Petal Length and Sepal Length
#Some of the observations of 'virginica' Species were deliberately removed to make it a minority class
#Some duplicate observations of the minority classes were removed (because it was interfaring with the animation feel)
#Some observatioins of the minority classes were deliberately introduced.

data = iris
data = data[data$Species!='setosa',]

#Deliberately introducing some observations
data2 <- data[data$Petal.Length < 5.2, c(1,3,5)]
a=data.frame(Sepal.Length=c(5.35, 5.87, 6.15, 6.31, 6.50),
             Petal.Length=c(4.50, 4.75, 4.50, 4.75, 4.65),
             Species=rep('virginica',5))


#Deleting some observations
data2 = rbind(data2,a)
data2 = data2[!(data2$Sepal.Length==6.0 & data2$Species=='virginica'),]
data2 = data2[!(data2$Sepal.Length==6.3 & data2$Species=='virginica'),]

#Visualize the Data
library(ggplot2)
ggplot(data2, aes(Sepal.Length, Petal.Length)) + geom_point(aes(col=Species))

#Converting the Species to binary
data2$Y = ifelse(data2$Species == 'virginica', 1, 0)
data2$Species = NULL

#Re-naming rows and columns
names(data2) = c('X1', 'X2', 'Y')
rownames(data2) = 1:nrow(data2)

#Removing some duplicate observations
data2 = data2[-c(51,60), ]
rownames(data2) = 1:nrow(data2)


write.csv(data2, "Imbalanced Technique Demo.csv")
View(data2)



plot(data2$X1, data2$X2, pch=20, col = ifelse(data2$Y==1, "black", "magenta")) 
