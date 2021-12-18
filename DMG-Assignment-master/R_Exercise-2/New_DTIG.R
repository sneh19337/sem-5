mydata=read.csv("Titanic.csv");
print(mydata);
index=c(2,3,4);

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

	return(entropy);
}



value_entropy<-function(P,X,R,F)
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
	
	if(yes_prob1==0)
		yes_prob1=0.000001;
	if(yes_prob2==0)
		yes_prob2=0.000001;
	if(no_prob1==0)
		no_prob1=0.000001;
	if(no_prob2==0)
		no_prob2=0.000001;

	entropy1= -(yes_prob1*log(yes_prob1)+no_prob1*log(no_prob1));
	entropy2= -(yes_prob2*log(yes_prob2)+no_prob2*log(no_prob2));
	
	sum1=sum1/total;
	sum2=sum2/total;
	
	final_out=sum1*entropy1+sum2*entropy2;

	return(final_out);
}


value_count<-function(P,X,F)
{
	sum=0;
	for(i in 1:length(X))
	{
		if(X[i]==P)
		{
			sum=sum+F[i];
		}
	}
	return(sum);
}



attri_entropy<-function(A,R,F)
{
	D=unique(factor(A));
	C=levels(droplevels(D));
	a=c();
	b=c();
	total=sum(F);

	for(i in 1:length(C))
	{
		a=append(a,value_entropy(C[i],A,R,F));		
		b=append(b,(value_count(C[i],A,F)/total));
	}
	sum=0;
	for(i in 1:length(C))
	{
		sum=sum+a[i]*b[i];
	}
	return(sum);
}



DecisionTree<-function(mydata,index)
{
	a = table(mydata[,5]);
	if(a[names(a)=='Yes']==nrow(mydata))
	{
		print('Yes');
		return();
	}else if(a[names(a)=='No']==nrow(mydata))
	{
		print('No');
		return();
	}else if(length(index)==0)
	{
		if(a[1]>=a[2]){
			print(a[1]);
		}else{
			print(a[2]);
		}
		return();
	}	

	scale=-1;
	max=-9999;

	x=data_entropy(mydata);
	
	for(i in 1:length(index))
	{
			
		value=attri_entropy(mydata[,index[i]],mydata[,5],mydata[,6]);	
		value=gain(x,value);
		if(max < value)
		{
			max=value;
			scale=i;	
		}
	}
	
	max_attri=index[scale];
	names=colnames(mydata)
	index=index[-which(index==index[scale])];
	
	print(names[max_attri]);
		
	Div=unique(factor(mydata[,max_attri]));
	Call=levels(droplevels(Div));
	
	for(i in 1:length(Call))
	{
		div_data=c();
		for(j in 1:nrow(mydata))
		{
			if(mydata[j,max_attri]==Call[i])
			{
				div_data=rbind(div_data,mydata[j,]);
			}
		}
		print(Call[i]);
		DecisionTree(div_data,index);
	}
}


DecisionTree(mydata,index);

