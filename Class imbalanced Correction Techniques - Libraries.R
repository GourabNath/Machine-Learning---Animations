#===========================================================================================
#Data
#===========================================================================================

data2 = read.csv("C:\\Users\\Gourab\\Documents\\Imbalanced Technique Demo.csv")
data2 = data2[-1]
round(prop.table(table(data2$Y))*100,2)

plot(data2[,1], data2[,2], pch=20, 
     col=ifelse(data2[,3]==1, 'black','magenta'),
     xlab = 'Variable 1', ylab = 'Variable 2',
     main = 'Data')

#===========================================================================================
#Random Under-Sampling
#===========================================================================================




#============================================================================================
#Random Over-Samling:
#============================================================================================




#============================================================================================
#Tomek:
#============================================================================================

library(unbalanced)

#ENN using the function ubTomek fron the unbalanced library
data2.tomek = ubTomek(X = data2[,1:2], Y = data2$Y)

#Index of the deleted observations
attributes(data2.tomek)
deleted.tomek = data2.tomek$id.rm 

#Visualize the Deleted Observations
{
plot(data2$X1, data2$X2, pch=20, col=ifelse(data2$Y==1, 'black','magenta'),
     xlab = 'Variable 1', ylab = 'Variable 2',
     main = 'Undersampling using Tomek')

points(data2$X1[deleted.tomek], data2$X2[deleted.tomek], pch=1, cex=2, col='red')
points(data2$X1[deleted.tomek], data2$X2[deleted.tomek], pch=4, cex=2, col='red')

}


#============================================================================================
#ENN
#============================================================================================

#ENN using the function ubENN fron the unbalanced library
enn = ubENN(X = data2[,1:2], Y = data2$Y, k = 2)

#Index of the deleted observations
deleted.enn = enn$id.rm


#Visualize the Deleted Observations
{
plot(data2$X1, data2$X2, pch=20, col = ifelse(data2$Y==1, "black", "magenta"),
     xlab = 'Variable 1', ylab = 'Variable 2',
     main = 'Undersampling using ENN')

points(data2[deleted.enn,]$X1, data2[deleted.enn,]$X2, pch=1, cex=2, col='red')
points(data2[deleted.enn,]$X1, data2[deleted.enn,]$X2, pch=4, cex=2, col='red')
}


#=============================================================================================
#SMOTE
#=============================================================================================

install.packages('smotefamily')
library(smotefamily)

#ENN using the function ubENN fron the unbalanced library
data2.smote = SMOTE(data2[-c(3,4)],       #feature values
                 data2$Y,                 #class labels
                 K = 3, dup_size = 1)     #function parameters


#The synthetically generated data
View(data2.smote$syn_data)


#Visualizing the newly generated observations
{
plot(data2$X1, data2$X2, pch=20, col = ifelse(data2$Y==1, "black", "magenta"),
     xlab = 'Variable 1', ylab = 'Variable 2',
     main = 'Synthetic Data Generation using SMOTE')

points(data2.smote$syn_data$X1, data2.smote$syn_data$X2, pch=20, col='red')
points(data2.smote$syn_data$X1, data2.smote$syn_data$X2, pch=1, cex=2, col='red')
}



#Reference for Smote:
#http://rikunert.com/SMOTE_explained

