MyData <- read.table("http://www.dataminingbook.info/pmwiki.php/Main/ContingencyTableAnalysis?action=download&upname=adult.data.txt");

a<-c();

for(i in 1:ncol(MyData))
{
tbl = table(MyData[,i],MyData[,15]);
a<-append(a,(chisq.test(tbl)[1]$statistic));
}
v<-order(a,decreasing=TRUE);
print(a);
print(v);