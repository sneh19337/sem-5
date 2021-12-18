library('data.table')
data<-fread('http://www.dataminingbook.info/pmwiki.php/Main/NumericDataAnalysis?action=download&upname=magic04.txt')
data<-data[,V11:=NULL]

 h=data.matrix(m);
b<-matrix(, nrow = 1, ncol = 10)
for(i in 1:10)
{
b[1,i]<-var(h[,i])
 }
print(b);
 v<-order(b[1,],decreasing=TRUE)
print(v);
