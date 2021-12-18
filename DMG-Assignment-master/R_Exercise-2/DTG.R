mydata=read.csv("Titanic.csv");

#function that give unique attribute in a table

table_attribute <- function(X)
{
D<-unique(factor(X));
C<-levels(droplevels(D));
return (C);
}

#This function that return Gini Value

gini_value<- function(mydata)
{
y<-0;
n<-0;
total<-sum(mydata[,6]);
for(i in 1:nrow(mydata))
{
if(mydata[i,5]=="Yes")
{
y<-y+mydata[i,6];
}else{
n<-n+mydata[i,6];
}
}
y<-y/total;
n<-n/total;
gini<-y*n;
return (gini);
}
 
#This function return the gain

gain<-function(gini_value,attribute_gini)
{
return (gini_value-attribute_gini);
}

#This function give the attributr gini

gini_attribute<-function(X,R,F)
{
total<-sum(F);
sum<-0;
unique_attribute=table_attribute(X);
for(i in 1:length(unique_attribute))
{
attri<-unique_attribute[i];
attri_prob<-0;
for(i in 1:length(X))
{
if(X[i]==attri)
attri_prob<-attri_prob+F[i];
}
attri_prob<-attri_prob/total;
attri_yes_prob<-0;
attri_no_prob<-0;
for(i in 1:length(R))
{
if(X[i]==attri && R[i]=='Yes'){
attri_yes_prob<-attri_yes_prob+F[i];
}else if(X[i]==attri && R[i]=='No'){
attri_no_prob<-attri_no_prob+F[i];
}
}
attri_yes_prob<-attri_yes_prob/total;
attri_no_prob<-attri_no_prob/total;
sum<-sum+(attri_no_prob*attri_yes_prob*attri_prob);
}
return (sum);
}

index<-c(2,3,4);

Desicion_tree<-function(data,index)
{
	a <- table(data[,5]);
	if(a[names(a)=='Yes']==nrow(data) || a[names(a)=='No']==nrow(data))
	{
	print('Pure Condition');
	return(NULL);
	}
	gini<-gini_value(data);
	attri_gini<-0;
	max<--1;
	scale<--1;	
	for(i in 2:4)
	{
		if(is.element(i,index)){
			attri_gini<-gain(gini,gini_attribute(data[,i],data[,5],data[,6]));	
			if(max<attri_gini)
			{
				scale<-i;
				max<-attri_gini;
			}		
		}
	}
	if(scale!=-1){
	index<-index[-which(index==scale)]
	print("Max Gain Attribute");
	print(scale);
	division_attri<-table_attribute(data[,scale]);
	print("Division");
	print(division_attri);
	for(j in 1:length(division_attri))
	{
		new_data<-c();
		for(i in 1:nrow(data))
		{
			if(data[i,scale]==division_attri[j])
			{
				new_data<-rbind(new_data,data[i,]);
			}
		}
		print("Data");
		print(new_data);
			
		Desicion_tree(new_data,index);		
	}
	}else
	{
		print('all over');
	}

}

Desicion_tree(mydata,index);


#Problem
#-> need to see recursion(stopping condition)








