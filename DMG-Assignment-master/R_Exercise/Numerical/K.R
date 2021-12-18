library('data.table')
data<-fread('http://www.dataminingbook.info/pmwiki.php/Main/NumericDataAnalysis?action=download&upname=magic04.txt')
data[,V11:=NULL]
h<-data.matrix(data);
max<-0
a<--1
b<--1

for(i in 1:ncol(h))
{
c1<-h[,i]
for(j in 1:ncol(h))
{
if(i!=j)
{
c2<-h[,j]
c3<-c1-c2
n<-sqrt(sum(c3 * c3))
if(max<n)
{
max=n
a<-i
b<-j
}
}
}
}
print(max)
print(a)
print(b)

#L2 Norm