#===========================================================================================
#CLASS IMBALANCED TECHNIQUES - ANIMATIONS
#===========================================================================================


#Data
#===========================================================================================

data2 = read.csv("C:\\Users\\Gourab\\Documents\\Imbalanced Technique Demo.csv")
data2 = data2[-1]
View(data2)



#Algorithm to identify the Nearest Neighbours:
#============================================================================================

#Formula for Eulidean distance:
euclid_dist <- function(x,y)
{
  if(length(x)!=length(y))
    stop('The vetors must have same length.')
  
  return(sum((x-y)^2))
  
}


#Function to get the k nearest neighbours:
k_NN <- function(data, x, class, k)
{
  distance = c()
  for(i in 1:nrow(data))
  {
    #calculate distance of each obs from x
    distance[i] = euclid_dist(data[i,],x)
  }
  
  dist_obs = data.frame('Obs'=1:nrow(data), 'Dist'=distance)
  dist_obs_ord = dist_obs[order(dist_obs$Dist), ]
  KNN = data[dist_obs_ord$Obs[2:(k+1)], ]
  return(list(KNN=KNN, Obs=dist_obs_ord$Obs[2:(k+1)], 
              Dist=sqrt(dist_obs_ord$Dist[2:(k+1)])))
}


#Plotting the NNs:

##Plot the original data
plot(data2$X1, data2$X2, pch=20, col = ifelse(data2$Y==1, "black", "magenta")) 

#Plot the observation
points(data2[1,1:2]$X1, data2[1,1:2]$X2, pch=1, cex=3, col='red')

#Plot the k-Nearest Neighbours of the observation
knn = k_NN(data2[1:2], data2[1,1:2], data2$Y, k=3) 
knn$KNN

points(knn$KNN$X1, knn$KNN$X2, pch=1, cex=3, col='blue')



#Plotting the NNs for all the observations

Plot_KNN<- function(data, K, obs=1:nrow(data))
{
  for(i in obs)
  {
    x = data[i, 1:2]
    knn = k_NN(data[,1:2], x, data2$Y, k=K)$KNN
    
    plot(data[,1], data[,2], pch=20, col = ifelse(data[,3] ==1, "black", "magenta"),
         main = paste("Observation No.",i),
         xlab = 'Variable 1', ylab = 'Variable 2')
    
    points(data[i, 1], data[i, 2], pch=1, cex=3, col='red')
    
    Sys.sleep(1.5)
    
    points(knn[,1], knn[,2], pch=1, cex=3, col='blue')
    
    Sys.sleep(0.75)
  }
  
}


Plot_KNN(data2, K=3, obs=which(data2$Y == 1))




#============================================================================================
# TOMEK - DEMONSTRATION
#============================================================================================

Plot_Tomek <- function(data, K, obs=1:nrow(data))
{
  
  tomek = data.frame(X1=NA, X2=NA, Y=NA)
  for(i in obs)
  {
    x = data[i, 1:2]
    knn = k_NN(data[,1:2], x, data2$Y, k=K)
    
    #Checking for Tomek Links
    obs = knn$Obs
    if(data[i,3] != data[obs,3])
      tomek = rbind(tomek,data[c(i,obs), ])
    
    plot(data[,1], data[,2], pch=20, cex = 1, 
         col = ifelse(data[,3] ==1, "black", "magenta"),
         main = paste("No. of Tomek Links Found:", (nrow(tomek)-1)/2),
         xlab = 'Variable1', ylab = 'Variable2') 
    
    points(tomek[, 1], tomek[, 2], pch=1, cex=3, 
           col=ifelse(tomek[,3] ==1, "blue", "red"))
    
    points(data[i, 1], data[i, 2], pch=1, cex=3, col='red')
    points(knn$KNN[,1], knn$KNN[,2], pch=1, cex=3, col='blue')
    
    Sys.sleep(2)
    
  }
  
  
  tomek = tomek[-1,]
  tomek_0 = tomek[tomek$Y==0, ]
  

  text(6.0,3.25, 'Now Remove the Majority Classes \namong the Tomek Links', col='red')
  points(tomek_0[, 1], tomek_0[, 2], pch=4, cex=2, col='red')
  
  #Sys.sleep(3)
  #del_rows = as.numeric(rownames(tomek_0))
  #plot(data[-del_rows,1], data[-del_rows,2], pch=20, cex = 1, 
  #       col = ifelse(data[,3] ==1, "maroon", "orange"),
  #       main = paste("No. of Tomek Links Found:", (nrow(tomek))/2),
  #     xlab = 'Variable1', ylab = 'Variable2') 
  
  
  #points(tomek[tomek$Y==1, 1], tomek[tomek$Y==1, 2], pch=1, cex=3, col='blue')
  
  return(tomek)
  
}




tmk = Plot_Tomek(data2, K=1, obs=which(data2$Y == 1))




#============================================================================================
#ENN - Demonstration
#============================================================================================

Plot_ENN <- function(data, K, obs=which(data$Y == 0))
{
  
  enn_del = data.frame(X1=NA, X2=NA)
  del_obsNo = c()
  
  for(i in obs)
  {
    x = data[i, 1:2]
    knn = k_NN(data[,1:2], x, data2$Y, k=K)
    
    #Checking ENN
    pred_Y = ifelse(sum(data$Y[knn$Obs]) >= length(data$Y[knn$Obs])/2, 1, 0)
    
    #If the obs belongs to the minority class & gets misclassified
    if(data$Y[i] == 0 & data$Y[i] != pred_Y)
    {
      #Then that obs must be deleted
      enn_del <- rbind(enn_del, x)
      del_obsNo <- c(del_obsNo, i)
      
      #Remove NA
      enn_del <- enn_del[complete.cases(enn_del),]
  
    }
    
    plot(data[,1], data[,2], pch=20, cex = 1, 
         col = ifelse(data[,3] ==1, "black", "magenta"),
         main = paste('Observation No.',i),
         xlab = 'Variable1', ylab = 'Variable2') 
    
    #Plotting the deleted observations
    points(enn_del[,1], enn_del[,2], pch=1, cex=3, col='red')
    points(enn_del[,1], enn_del[,2], pch=4, cex=3, col='red')
    text(enn_del[,1]+0.1, enn_del[,2], del_obsNo, cex=0.75)
    
   
    points(data[i, 1], data[i, 2], pch=1, cex=3, col='red')
    
    Sys.sleep(0.6)
    points(knn$KNN[,1], knn$KNN[,2], pch=1, cex=3, col='blue')
    
    Sys.sleep(0.6)
    
  }
  
  
  plot(data[,1], data[,2], pch=20, cex = 1, 
       col = ifelse(data[,3] ==1, "black", "magenta"),
       main = paste('Observation No.',i),
       xlab = 'Variable1', ylab = 'Variable2') 
  
  #Plotting the deleted observations
  points(enn_del[,1], enn_del[,2], pch=1, cex=3, col='red')
  points(enn_del[,1], enn_del[,2], pch=4, cex=3, col='red')
  text(enn_del[,1]+0.1, enn_del[,2], del_obsNo, cex=0.75)
  
  
  
  return(data[-del_obsNo, ])
  
}



enn = Plot_ENN(data2, K=3, obs=which(data$Y == 0))



data2[55,]
data=data2
i = 7 #52 51 52 54 55 56 60 61 62 63
K=1


#============================================================================================
# SMOTE DEMONSTRATION
#============================================================================================

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
      
      for(l in 1:nrow(k$KNN))
      {
        #Syn_obs
        d = k$Dist
        d1 = runif(1)*d
        d2 = d - d1
        p=section_form(x,k$KNN[l,],d1,d2)
        
        #Adding it to the synthetic obs data frame
        syn_obs = rbind(syn_obs,p)
        
        #Plot the entire data
        plot(data[,1], data[,2], pch=20, cex = 1.25, 
             col = ifelse(data[,3] ==1, "black", "magenta"),
             main = paste('Observation No.',i ,", Neighbour", l),
             xlab = 'Variable1', ylab = 'Variable2') 
        
        #Plotting the synthetic obs
        points(syn_obs[-nrow(syn_obs),1],syn_obs[-nrow(syn_obs),2],cex=2,pch=13,col='orange')
        
        #plot the considered minority point
        points(x[1],x[2], pch=1, cex=3, col='red')
        
        
        Sys.sleep(1)
        #Circle the NN
        points(k$KNN[l,1],k$KNN[l,2], pch=1, cex=3, col='blue')
        #Draw a line segment connecting the two points
        segments(as.numeric(x[1]),as.numeric(x[2]),
                 as.numeric(k$KNN[l,1]),as.numeric(k$KNN[l,2]),lty=2, col='seagreen')
        
        Sys.sleep(1)
        points(p[1],p[2],pch=13,cex=2, col='black')
        Sys.sleep(1)
        
      }
      
    }
    
  }
  #Plot the entire data
  plot(data[,1], data[,2], pch=20, cex = 1.25, 
       col = ifelse(data[,3] ==1, "black", "magenta"),
       main = paste('Actual Observations + Synthetic Observations'),
       xlab = 'Variable1', ylab = 'Variable2') 
  
  #Plotting the synthetic obs
  points(syn_obs[-nrow(syn_obs),1],syn_obs[-nrow(syn_obs),2],cex=2,pch=13,col='black')
  
  syn_obs=syn_obs[complete.cases(syn_obs),]
  return(syn_obs)
  
  
}


Plot_SMOTE(data2,K=3,ntimes = 1)
