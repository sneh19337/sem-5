library(mclust)
data=read.csv('IRIS.csv',header=FALSE);
myMclust=Mclust(data[,1:8],4)
mySummary=summary( myMclust$BIC, data=data[,1:8] )
CLUST=myMclust$classification
print(myMclust);
print(mySummary);
print(CLUST);
plot(myMclust)

# EM Cluster was quite fast and more accurate than Kmeans