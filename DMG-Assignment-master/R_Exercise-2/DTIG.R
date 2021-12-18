mydata=read.csv("Titanic.csv");


table_attribute <- function(X)
{
r=c();
for(i in 2:4)
{
D=unique(factor(X[,i]));
C=levels(droplevels(D));
for(j in 1:length(C))
{
r=append(r,paste(C[j],i,sep='-'));
}
}
return (r);
}



data_entropy<- function(mydata)
{
y=0;
n=0;
total=sum(mydata[,6]);
for(i in 1:nrow(mydata))
{
if(mydata[i,5]=="Yes")
{
y=y+mydata[i,6];
}else{
n=n+mydata[i,6];
}
}
y=y/total;
n=n/total;
entropy= -(y*log(y)+n*log(n));
print(entropy);
return(entropy);
}
 


gain<-function(data_entropy,attribute_entropy)
{
return (data_entropy-attribute_entropy);
}



attribute_entropt<-function(P,X,R,F)
{
	yes_prob1=0;
	no_prob1=0;
	yes_prob2=0;
	no_prob2=0;
	total=sum(F);
	sum1=0;
	sum2=0;
	
		for(i in 1:length(X))
		{
			if((X[i]==P))
			{
				
				if(R[i]=='Yes')
				{
					yes_prob1=yes_prob1+F[i];	
				}else
				{
					no_prob1=no_prob1+F[i];
				}
				sum1=sum1+F[i];			
			}else{

				if(R[i]=='Yes')
				{
					yes_prob2=yes_prob2+F[i];	
				}else
				{
					no_prob2=no_prob2+F[i];
				}
				sum2=sum2+F[i];
				
			}
		}				
	
	yes_prob1=yes_prob1/total;
	no_prob1=no_prob1/total;
	
	yes_prob2=yes_prob2/total;
	no_prob2=no_prob2/total;
	
	entropy1= -(yes_prob1*log(yes_prob1)+no_prob1*log(no_prob1));
	entropy2= -(yes_prob2*log(yes_prob2)+no_prob2*log(no_prob2));
	
	sum1=sum1/total;
	sum2=sum2/total;
	
	final_out=sum1*entropy1+sum2*entropy2;

	return(final_out);
}




Desicion_Tree<-function(data,index)
{
a = table(data[,5]);
if((a[names(a)=='Yes']==nrow(data) || a[names(a)=='No']==nrow(data)))
{
	print('Pure Condition');
	
}


scale=-1;
max=-1;
x=data_entropy(data);


for(i in 1:length(index))
{
	
	input=index[i];
	process=strsplit(input, "-")[[1]];
	rf=strtoi(process[2]);
	value=attribute_entropt(process[1],data[,rf],data[,5],data[,6]);
	
	out_value=gain(x,value);
	
	
	if(!is.nan(out_value)){
	if(max < out_value)
	{
		max=out_value;
		scale=i;	
	}
	}
}

max_gain_attri=index[scale];
index=index[-which(index==index[scale])];

D_yes=c();	
D_no=c();
process=strsplit(max_gain_attri, "-")[[1]];	

rf=strtoi(process[2])

for(i in 1:nrow(data))
{
	
		if((data[i,rf]==process[1]))
		{
			D_yes=rbind(D_yes,data[i,]);			
		}else{
			D_no=rbind(D_no,data[i,]);	
		}
	
}

print('Data Yes');
print(D_yes);
print('Data No');
print(D_no);

Desicion_Tree(D_yes,index);
Desicion_Tree(D_no,index);
}


index=table_attribute(mydata);
Desicion_Tree(mydata,index);

