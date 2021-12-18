MyData <- read.table("http://www.dataminingbook.info/pmwiki.php/Main/ContingencyTableAnalysis?action=download&upname=adult.data.txt");
max=-1;
min=99999999;
f1=-1;
f2=-1;
s1=-1;
s2=-1;
for(i in 1:ncol(MyData))
{
for(j in 1:ncol(MyData))
{
if(i!=j){
tbl = table(MyData[,i],MyData[,j]);
a<-(chisq.test(tbl)[1]$statistic);
if(max<a)
{
max=a;
f1=i;
f2=j;
}else if(min>a){
min=a;
s1=i;
s2=j;
}
}
}
}
print('Maximum');
print(paste('Attribute1: ',f1,''));
print(paste('Attribute2: ',f2,''));
print(max);

print('Minimum');
print(paste('Attribute1: ',s1,''));
print(paste('Attribute2: ',s2,''));
print(min);


