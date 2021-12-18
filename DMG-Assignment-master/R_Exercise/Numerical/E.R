library('data.table')
data<-fread('http://www.dataminingbook.info/pmwiki.php/Main/NumericDataAnalysis?action=download&upname=magic04.txt')
data<-data[,V11:=NULL]

h<-data.matrix(data);
y <- dnorm(h[,1],mean(h[,1]),sqrt(var(h[,1])));
x <- h[,1];
plot(x,y);