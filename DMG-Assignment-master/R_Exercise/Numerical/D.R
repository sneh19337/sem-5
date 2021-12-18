MyData<-read.table("http://www.dataminingbook.info/pmwiki.php/Main/NumericDataAnalysis?action=download&upname=magic04.txt",sep=',');

max=-9999;
max2=-9999;
f1=-1;
f2=-1
s1=-1;
s2=-1;
n=length(MyData);
i=1;
j=1;

print(n);

while(i<=n-1){

j=1;
while(j<=n-1){
if(i!=j){
a=cor(MyData[,i],MyData[,j]);
if(max < a)
{
max=a;
f1=i;
f2=j;
}
if(a<max && a>max2)
{
max2=a;
s1=i;
s2=j;
}
}
j=j+1;
}
i=i+1;
}
print('First Pair');
print(paste('Attribute1: ',f1,''));
print(paste('Attribute2: ',f2,''));
print(max);
print('Second Pair')
print(paste('Attribute1: ',s1,''));
print(paste('Attribute2: ',s2,''));
print(max2);
