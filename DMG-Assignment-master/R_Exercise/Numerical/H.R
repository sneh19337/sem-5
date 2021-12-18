library('data.table')
data<-fread('http://www.dataminingbook.info/pmwiki.php/Main/NumericDataAnalysis?action=download&upname=magic04.txt')
h<-data.matrix(data);

for(i in 1:5){
	for(j in i+1:5){
		if(i<=5 && j<=5){
		dev.new();
		plot(h[,i],h[,j],xlab=paste(i, "Attribute", sep="-"),ylab=paste(j, "Attribute", sep="-"),col=c('black','green'));
		}
	}
}

#Relation b/w 1-2 : As their values incresese they spread out,for low value the plot is comapact 
#Relation b/w 1-3 : As their values incresese they spread out ,for low value they coincide and the spread of plot is less
#Relation b/w 1-4 : same as in case of 1-5
#Relation b/w 1-5 : same as 2-5 but the number of values are high
#Relation b/w 2-3 : as x increases the plot spread out and the number of values become less
#Relation b/w 2-4 : same as in case of 2-5
#Relation b/w 2-5 : there is a high intensity of values for x<50 as we go along the x axis spread of plot decreases
#Relation b/w 3-4 :same as in case of 3-5
#Relation b/w 3-5 : As x axis increases the frequency of value decreases,as seen from the plot it has more points in the lower x axis region
#Relation b/w 4-5 :The plot remain compact ,both the attribute are spread in the same fashion that's why the green and black region overlap