#===========================================================================================
#PLAYGROUND
#===========================================================================================

#SMOTE ANIMATION - IDEA GENERATION:
#------------------------------------

k = 1

#Subset with only the minority class 
data_1 = data2[data2$Y == 1, ]


#Consider an obs from the minority class
x = data[62, 1:2]   #52 51 52 56 62 63


#Plot the entire data
plot(data[,1], data[,2], pch=20, cex = 1, 
     col = ifelse(data[,3] ==1, "maroon", "orange"),
     main = paste('Observation No.',i),
     xlab = 'Variable1', ylab = 'Variable2') 


#plot the considered minority point
points(x[1],x[2], pch=1, cex=3, col='red')


#Find and circle the nearest minority neigbout of the considered point
k = k_NN(data_1[,1:2], x, k=1)


#Circle the nearest neighbour
points(k$KNN[1],k$KNN[2], pch=1, cex=3, col='blue')


#Draw a line segment connecting the two points
segments(as.numeric(x[1]),as.numeric(x[2]),
         as.numeric(k$KNN[1]),as.numeric(k$KNN[2]),lty=2, col='red')



#---------------------------------------------------------------------------------------
#Function to get coordinates using section formula
x1 = as.numeric(x[1])
y1 = as.numeric(x[2])
x2 = as.numeric(k$KNN[1])
y2 = as.numeric(k$KNN[2])

k$Dist
d1 = k$Dist*0.6
d2 = k$Dist*0.4

x = (d1*x1+d2*x2)/(d1+d2)
y = (d1*y1+d2*y2)/(d1+d2)

points(x,y,pch=19,col='black')

#Creating the function
section_form <- function(x,y,d1,d2)
{
  a = (d1*x[1]+d2*y[1])/(d1+d2)
  b = (d1*x[2]+d2*y[2])/(d1+d2)
  return(c(a,b))
}

x=c(x1,y1)
y=c(x2,y2)
section_form(x,y,d1,d2)

#---------------------------------------------------------------------------------------

#Plot a point at the distance d*rand, which is collinear to the first point
d = k$Dist
d1 = runif(1)*d
d2 = d - d1

p = section_form(as.numeric(k$KNN), x, d1, d2)
p

points(p[1], p[2], pch=19)


#----------------------------------------------------------------------------------------



#SMOTE for k=2

k = 2

#Subset with only the minority class 
data_1 = data2[data2$Y == 1, ]


#Consider an obs from the minority class
x = data_1[data_1$X1<5, 1:2]


#Plot the entire data
plot(data[,1], data[,2], pch=20, cex = 1, 
     col = ifelse(data[,3] ==1, "maroon", "orange"),
     main = paste('Observation No.',i),
     xlab = 'Variable1', ylab = 'Variable2') 


#plot the considered minority point
points(x[1],x[2], pch=1, cex=3, col='red')


#Find and circle the nearest minority neigbout of the considered point
k = k_NN(data_1[,1:2], x, k=2)


#Circle the nearest neighbour
points(k$KNN[1,1],k$KNN[1,2], pch=1, cex=3, col='blue')
points(k$KNN[2,1],k$KNN[2,2], pch=1, cex=3, col='blue')


#Draw a line segment connecting the two points
segments(as.numeric(x[1]),as.numeric(x[2]),
         as.numeric(k$KNN[1,1]),as.numeric(k$KNN[1,2]),lty=2, col='red')


#Plot a point at the distance d*rand, which is collinear to the first point
d = k$Dist[1]
d1 = runif(1)*d
d2 = d - d1

p = section_form(as.numeric(k$KNN[1,]), x, d1, d2)
p

points(p[1], p[2], pch=19)



#Draw a line segment connecting the two points
segments(as.numeric(x[1]),as.numeric(x[2]),
         as.numeric(k$KNN[2,1]),as.numeric(k$KNN[2,2]),lty=2, col='red')


#Plot a point at the distance d*rand, which is collinear to the first point
d = k$Dist[2]
d1 = runif(1)*d
d2 = d - d1

p = section_form(as.numeric(k$KNN[2,]), x, d1, d2)
p

points(p[1], p[2], pch=19)




#-----------------------------------------------------------------------------------
#The below function works only for k=1



Plot_SMOTE <- function(data, K, obs=which(data$Y == 1), ntimes=1)
{
  syn_obs <- data.frame(X1=NA, X2=NA)
  #Subset with only the minority class 
  data_1 = data[data$Y == 1, ]
  
  #function for section formula
  section_form <- function(x,y,d1,d2)
  {
    a = (d1*x[1]+d2*y[1])/(d1+d2)
    b = (d1*x[2]+d2*y[2])/(d1+d2)
    return(c(a,b))
  }
  
  for(j in 1:ntimes){
    for(i in obs)
    {
      #Consider an obs from the minority class
      x = data[i, 1:2]
      #Find the nearest minority neigbout of the considered point
      k = k_NN(data_1[,1:2], x, k=K)
      
      #Syn_obs
      d = k$Dist
      d1 = runif(1)*d
      d2 = d - d1
      p=section_form(x,k$KNN[1,],d1,d2)
      
      #Adding it to the synthetic obs data frame
      syn_obs = rbind(syn_obs,p)
      
      #Plot the entire data
      plot(data[,1], data[,2], pch=20, cex = 1, 
           col = ifelse(data[,3] ==1, "black", "magenta"),
           main = paste('Observation No.',i),
           xlab = 'Variable1', ylab = 'Variable2') 
      
      #Plotting the synthetic obs
      points(syn_obs[-nrow(syn_obs),1],syn_obs[-nrow(syn_obs),2],cex=2,pch=13,col='black')
      
      #plot the considered minority point
      points(x[1],x[2], pch=1, cex=3, col='red')
      
      
      Sys.sleep(1)
      #Circle the NN
      points(k$KNN[1],k$KNN[2], pch=1, cex=3, col='blue')
      #Draw a line segment connecting the two points
      segments(as.numeric(x[1]),as.numeric(x[2]),
               as.numeric(k$KNN[1]),as.numeric(k$KNN[2]),lty=2, col='seagreen')
      
      Sys.sleep(1)
      points(p[1],p[2],pch=13,cex=2, col='orange')
      Sys.sleep(1)
      
      
      
    }
    
  }
  #Plot the entire data
  plot(data[,1], data[,2], pch=20, cex = 1, 
       col = ifelse(data[,3] ==1, "black", "magenta"),
       main = paste('Observation No.',i),
       xlab = 'Variable1', ylab = 'Variable2') 
  
  #Plotting the synthetic obs
  points(syn_obs[-nrow(syn_obs),1],syn_obs[-nrow(syn_obs),2],cex=2,pch=13,col='black')
  
  syn_obs=syn_obs[complete.cases(syn_obs),]
  return(syn_obs)
  
  
}


Plot_SMOTE(data2,K=1,ntimes = 2)

