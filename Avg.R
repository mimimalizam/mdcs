files<-list.files(path = "D:/AirportData")


# calculating :
# 1. Average ArrDelay, first remove rows with missing ArrDelay data from starting data
# 2. Average DepDelay, first remove rows with missing DepDelay data from starting data


sumaAD<-0;nAD<-0; sumaDD<-0; nDD<-0;

for (i in 1:(length(files)-2)){
	MyData <- read.csv(file=files[i], header=TRUE, sep=",") 
	a<-subset(subset(MyData, ArrDelay!='Na'),select=c(ArrDelay))	
	sumaAD<-sum(a$ArrDelay)+sumaAD		
	nAD<-nrow(a)+nAD
	b<-subset(subset(MyData, DepDelay!='Na'),select=c(DepDelay))	
	sumaDD<-sum(b$DepDelay)+sumaDD		
	nDD<-nrow(b)+nDD
}
print('Average ArrDelay is: ');print(sumaAD/nAD);
print('Average DepDelay is: ');print(sumaDD/nDD);


