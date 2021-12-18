
library('data.table')

data<-fread('http://www.dataminingbook.info/pmwiki.php/Main/NumericDataAnalysis?action=download&upname=magic04.txt')

data[,V11:=NULL]

h<-data.matrix(data);
flag=-1;
a<-c()

for(i in 1:ncol(h))
{
 	c1<-h[,i]
 	for(j in i:ncol(h))
 	{
 	c2<-h[,j]
 	if(sum(c1*c2)==0)
 		{
 		a<-append(a,i)
 		a<-append(a,j)
		flag=0;
 		break
		}
	}
}

if(flag==-1){
print('No Pair is Orthogonal');
}else{
print('The oryhogonal Pair is:-');
print(a)
}

