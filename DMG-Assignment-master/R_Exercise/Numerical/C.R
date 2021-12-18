library('data.table')
data<-fread('http://www.dataminingbook.info/pmwiki.php/Main/NumericDataAnalysis?action=download&upname=magic04.txt')
data<-data[,V11:=NULL]

h<-data.matrix(data);

a<-c();

for(i in 1:ncol(h))
{ 
a<-append(a,sum(h[,i])/nrow(h));
}

print(a);
 