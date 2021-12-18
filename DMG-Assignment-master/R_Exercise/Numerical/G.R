library('data.table')
data<-fread('http://www.dataminingbook.info/pmwiki.php/Main/NumericDataAnalysis?action=download&upname=magic04.txt')
data<-data[,V11:=NULL]


a<-c()
b<-c()
h=data.matrix(data);

getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
 }

for(i in 1:10)
 {
 a[i]<-median(h[,i])
 b[i]<-getmode(h[,i])
 }
print('median -> ');
print(a);
print('mode -> ');
print(b);
