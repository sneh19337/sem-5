mydata<-read.csv('IRIS.csv',header=FALSE)
W=c();
for(i in 2:12){
x=kmeans(mydata[,1:8], i);
W=append(W,(x$tot.withinss));
}

print(W);
print(order(W)+1);

plot(W,xlab='Cluster',ylab='SSE');
