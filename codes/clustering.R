# This script is for trader clusters
## all features will be used.

# Clear your workspace by removing all objects returned by ls():
rm(list = ls()) 

getwd()
setwd("C:/Users/CEYDA/Desktop/emprical_research")

Sys.timezone() 

 install.packages("ggplot2")
 install.packages("cluster")
 install.packages("factoextra")
 install.packages("gridExtra")
 install.packages("NbClust")
 
library(NbClust)
library(cluster)
library(factoextra)
library(gridExtra)
library(ggplot2)
 

# You already all features file as RData file.

load("~/Desktop/emprical_research/data/all_features.RData")

mycluster <- all_features[, c(1,2,14,15,30)]
mycluster[is.na(mycluster)] <- 0
head(mycluster)
str(mycluster)

scaled.cluster <- scale(mycluster[,2:6])

kmeans3 <- kmeans(mycluster[,2:6], centers = 3, nstart=30)
plot1<- fviz_cluster(kmeans3, data = scaled.cluster)
plot1

kmeans6 <- kmeans(mycluster[,2:6], centers = 6, nstart =30)
plot2<- fviz_cluster(kmeans6, data = scaled.cluster)
plot2


#####################

secondcluster <- trade_stats[,c(28,29)]
# secondcluster$avgtpm <- trade_features$avgtpm
# secondcluster$avgopm <- all_features$avgopm
# secondcluster$avgov <- all_features$avgov

secondcluster[is.na(secondcluster)] <- 0

kmeans3_ <- kmeans(secondcluster, centers = 6, nstart=30)
plot3<- fviz_cluster(kmeans3_, data = secondcluster,geom = "point",
                     stand = FALSE, choose.vars = c("avgxaxis", "avgyaxis"), ellipse.type  = "norm") +
                    theme_bw()
plot3


kmeans3_$cluster


