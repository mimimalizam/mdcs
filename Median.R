files<-list.files(path = "D:/AirportData")


# calculating :
# 1. Average ArrDelay, first remove rows with missing ArrDelay data from starting data
# 2. Average DepDelay, first remove rows with missing DepDelay data from starting data
vmina<-vector()
vmaxa<-vector()
vmind<-vector()
vmaxd<-vector()


for (i in 1:60){
	MyData <- read.csv(file=files[i], header=TRUE, sep=",") 
	a<-subset(MyData, ArrDelay!='Na')
	vmina[i]<-max(a$ArrDelay)
	vmaxa[i]<-min(a$ArrDelay)	
	}
Month<-1:60;
MinA<-vmaxa;#accidently changed min for max in vector reading
MaxA<-vmina;
MinMax<-data.frame(Month,MinA, MaxA)
write.csv(MinMax, "MinMax.csv")

Min<-min(MinA)
Max<-max(MaxA)
interval<-seq(from=-1302, to=2598, by=100)

broj<-vector();
b<-0;
for (i in 1:1){
	for(j in 1:39){
	MyData <- read.csv(file=files[i], header=TRUE, sep=",")
	a<-nrow(subset(subset(MyData,ArrDelay!='Na'), ArrDelay>interval[j] & ArrDelay<interval[j+1]))
	b<-a+b 
	broj[i]<-b}
	}


people$Height.Cat<-cut(people$Height, c(150, 160, 180, 200), c("Short", "Medium", "Tall"))