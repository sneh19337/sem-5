library('data.table')
data<-fread('http://www.dataminingbook.info/pmwiki.php/Main/NumericDataAnalysis?action=download&upname=magic04.txt')
data[,V11:=NULL]
h<-data.matrix(data);
min<-9999
a<--1
b<--1

min2<-9999
p<--1
q<--1

for(i in 1:ncol(h))
{
c1<-h[,i]
for(j in 1:ncol(h))
{
if(i!=j)
{
c2<-h[,j]
n<-crossprod(c1, c2)/sqrt(crossprod(c1) * crossprod(c2))
if(min>n)
{
min=n
a<-i
b<-j
}
if(min<n && min2>n)
{
min2=n
p<-i
q<-j
}
}
}
}
print('First Pair');
print(min);
print(a);
print(b);

print('Second Pair');
print(min2);
print(p);
print(q);


