SampleData=read.csv("DPSD.csv",stringsAsFactors = FALSE)
data=SampleData
len = length(data$ln)
data$gn<-as.factor(data$gn)

for (i in 1:len){
data$ln[i]<-unlist(strsplit(data$ln,split=" ")[i])[1]
data$fn[i]<-unlist(strsplit(data$fn,split=" ")[i])[1]
data$fullname[i]<-paste(data$ln[i],data$fn[i])
data$day[i]<-as.integer(unlist(strsplit(data$dob,split="/")[i])[1])
data$month[i]<-as.integer(unlist(strsplit(data$dob,split="/")[i])[2])
data$year[i]<-as.integer(unlist(strsplit(data$dob,split="/")[i])[3])

}
a<-c(1,2,4)
df=data[,-a]

uniqueids<-unlist(labels(unique( df[ , 1:5 ] ))[1])

FinalList<-SampleData[uniqueids,]
