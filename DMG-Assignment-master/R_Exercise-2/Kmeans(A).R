mydata=read.csv('IRIS1.csv',header=FALSE);

k1=mydata[1,];
k2=mydata[nrow(mydata),];
k3=mydata[nrow(mydata)/2,];
k4=mydata[nrow(mydata)/2+1,];

C1=c();
C2=c();
C3=c();
C4=c();

p1=mydata[1,];
p2=mydata[nrow(mydata),];
p3=mydata[nrow(mydata)/2,];
p4=mydata[nrow(mydata)/2+1,];




find_mean<-function(mydata)
{
	C=c();
	for(i in 1:ncol(mydata))
	{
		C=append(C,mean(mydata[,i]))
	}
	
	return(C);
}

find<-function(X)
{
	return(sum(X)/length(X));
}

distance<-function(X,Y)
{
	sum=0;
	for(i in 1:length(X))
	{
		sum=sum+(X[i]-Y[i])^2;	
	}
	return(sum^(1/2));
}




Kmeans<-function(mydata,p1,p2,p3,p4,k1,k2,k3,k4,C1,C2,C3,C4)
{

	for(i in 1:nrow(mydata))
	{

		g=mydata[i,];		
		D1=distance(g,k1);
		D2=distance(g,k2);
		D3=distance(g,k3);
		D4=distance(g,k4);
		
		Min1=min(D1,D2,D3,D4);
		
		
		if(Min1==D1){
			C1=rbind(C1,mydata[i,]);
		}else if(Min1==D2){
			C2=rbind(C2,mydata[i,]);
		}else if(Min1==D3){
			C3=rbind(C3,mydata[i,]);
		}else{
			C4=rbind(C4,mydata[i,]);
		}
		
	}

	print(C1);
	print(C2);
	print(C3);
	print(C4);
	p1=k1;
	p2=k2;
	p3=k3;
	p4=k4;
	
	k1=find_mean(C1);
	k2=find_mean(C2);
	k3=find_mean(C3);
	k4=find_mean(C4);	
	
	f1=(find(p1)-find(k1))^2;
	f2=(find(p2)-find(k2))^2;
	f3=(find(p3)-find(k3))^2;
	f4=(find(p4)-find(k4))^2;
	
	

	convergence=f1+f2+f3+f4;
	
	
		
	if(convergence<=0.001)
	{
		print('We ARE DONE !!');
		
		print('FIRST BUCKET');
		print(C1);
		
		print('SECOND BUCKET');
		print(C2);
		
		print('THIRD BUCKET');
		print(C3);
		
		print('FORTH BUCKET');
		print(C4);
		
		
	}else{
		C1=c();
		C2=c();
		C3=c();
		C4=c();			
		Kmeans(mydata,p1,p2,p3,p4,k1,k2,k3,k4,C1,C2,C3,C4);
	}
}
	

Kmeans(mydata,p1,p2,p3,p4,k1,k2,k3,k4,C1,C2,C3,C4);


