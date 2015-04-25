require(ggplot2)
require(cluster)
#4/17/2015. asdf

#import stacked dataset
data <- read.table('D:/COMPLEXITY_MUELLER/IV_MAPPS_RawData/StackedRAW_NO1006.txt', sep='\t', skip=2, header=T)
fix <- read.table('D:/COMPLEXITY_MUELLER/SEClean_FIX.txt', sep='\t', header=T)
fix.red <- subset(fix, select=c("FixationIndex", "CentroidX", "CentroidY", "Duration")) 
write.csv(fix.red, file="FixationDurations_Full.csv", row.names=FALSE)

#Create a reduced raw data set "data.red" systematically sampling that large set
data$index <- seq(1:dim(data)[1]) #create column from 1 to the end of the dataset
data$include <- ifelse(data$index %% 100 == 1, 1, 0) #Delete all but every 100th obs
data.red <- subset(data, include == 1 
                   & GazeX > 0 
                   & GazeY > 0 
                   & GazeY < 1, 
                   select=c("GazeX", "GazeY")) 
                   #In that subset, take out all error terms for GazeX and 
                   #GazeY, and delete all other columns.
write.csv(data.red, file="RawGaze_Reduced_25Drivers_mod100.csv", row.names=FALSE)


# K-Means Cluster Analysis
wss <- (nrow(data.red)-1)*sum(apply(data.red,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data.red, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

fit <- kmeans(data.red, 7) # 7 cluster solution, arbitrarily chosen.
aggregate(data.red,by=list(fit$cluster),FUN=mean) # get cluster means 
data.red <- data.frame(data.red, fit$cluster) # append cluster assignment
data.red$fit.cluster <- as.factor(data.red$fit.cluster) 
ggplot(data.red, aes(x=GazeX, y=GazeY, color=fit.cluster)) + 
  geom_point(alpha=0.05) + ggtitle("K-Means Clustering: 7 Clusters")



#HClust Cluster Analysis




