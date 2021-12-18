data <- read.table("http://www.dataminingbook.info/pmwiki.php/Main/NumericDataAnalysis?action=download&upname=magic04.txt",sep=',');
tlb<-(table(cut(data[,1],quantile(data[,1]),include.lowest=T)));
barplot(tlb,main="Equidepth Histogram",ylim=c(0,5000));
