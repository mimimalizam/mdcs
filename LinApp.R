files<-list.files(path = "D:/AirportData")
data_names <- gsub("[.]csv", "", files)
dn<-data_names[1:60]
carriers<-read.csv("carriers.csv",header=T)
vc<-carriers$Code

#vadm vector number of ArrDelay considerable flights per month
#vddm vector number of DepDelay considerable flights per month
#vadm15 vector number of ArrDelay considerable flights per month with delay >= 15min
#vddm15 vector number of DepDelay considerable flights per month with delay >= 15min
#vnfm vector number of flights per month
#vnfcm vector number of flights cancelled per month
#vnfdm vector number of flights deverted per month
#vc vector codes of carriers
#vcnla vector number of late flight arr per month for carrier
#vcsla vector sum of late arr delays of flights per month for carrier
#vcnld vector number of late flight dep per month for carrier
#vcsld vector sum of late flight dep per month for carrier

vadm<-vector();vddm<-vector();vadm15<-vector();vddm15<-vector();
vnfm<-vector();vnfcm<-vector();vnfdm<-vector();
vcnla<-vector();vcsla<-vector();vcnld<-vector();vcsld<-vector();

for (i in 1:60){
	MyData <- read.csv(file=files[i], header=TRUE, sep=",")			
	vadm[i]<-nrow(subset(MyData, ArrDelay!='Na'))			
	vddm[i]<-nrow(subset(MyData, DepDelay!='Na'))
	vadm15[i]<-nrow(subset(MyData, ArrDelay!='Na' & ArrDelay>=15))
	vddm15[i]<-nrow(subset(MyData, DepDelay!='Na' & DepDelay>=15))
	vnfm[i]<-nrow(MyData)
	vnfcm[i]<-nrow(subset(MyData, Cancelled==1))
	vnfdm[i]<-nrow(subset(MyData, Diverted==1))	
}
freqpmonth<-data.frame(vadm, vddm, vadm15,vddm15,vnfm,vnfcm,vnfdm)
write.csv(freqpmonth, "FreqMonth.csv")


FlightsMonth<-data.frame(vnfm,vnfcm,vnfdm)
names(FlightsMonth)[c(1,2,3)]<-c("NumFMonth", "CancelledFMonth", "DevertedFMonth")
write.csv(FlightsMonth,"FlightsMonth.csv")

Year<-c(2003,2004,2005,2006,2007);
NumFY<-vector();CancelledFY<-vector();DevertedFY<-vector();
for (i in 0:4){
	a<-0
	b<-0
	c<-0
	for (j in 1:12){ a<-a+vnfm[i*12+j];b<-b+vnfcm[i*12+j];c<-c+vnfdm[i*12+j]}
	NumFY[i+1]<-a 
	CancelledFY[i+1]<-b
	DevertedFY[i+1]<-c
}
FlightsYear<-data.frame(Year,NumFY,CancelledFY,DevertedFY)
write.csv(FlightsYear,"FlightsYear.csv")
plot(Year, NumFY)
abline(coef(lm(Year, NumFY)))

lm(formula=NumFY ~ Year, data=FlightsYear)
treemodel<-lm(formula=NumFY ~ Year, data=FlightsYear)
summary(treemodel)

Month<-1:60
FlightsMonth1<-data.frame(Month,vnfm)
lm(formula=vnfm ~ Month, data=FlightsMonth1)
treemodel<-lm(formula=vnfm ~ Month, data=FlightsMonth1)
summary(treemodel)
plot(Month, vnfm)



