library('data.table')
data<-fread('http://www.dataminingbook.info/pmwiki.php/Main/NumericDataAnalysis?action=download&upname=magic04.txt')
data<-data[,V11:=NULL]

f<-cov(data)

a<-c();
for(i in 1:ncol(f))
{
 a<-append(a,f[i,i]);
}
print(a);
print(max(a));