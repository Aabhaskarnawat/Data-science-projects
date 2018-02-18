SampleData=read.csv("DPSD.csv",stringsAsFactors = FALSE)
data=SampleData
len = length(data$ln)
data$gn<-as.numeric(as.factor(data$gn))


for (i in 1:len){
  data$fullname[i]<-paste(data$ln[i],data$fn[i])}
a<-c(1,4)
df=data[,-a]


#FUNCTION TO MATCH WORDS

mySecFun<-function(df,len)
{
  #install.packages("qualV")
  #library(qualV)
  #install.packages("stringr", dependencies = TRUE)
  #library(stringr)
  Threshold = 0.95
  del = 0 
  
    for(i in 1:len)
    {
        for(j in (i+1):len)
        {			
          a<-character(0)
          b<-character(0)
          
          a<- df$fullname[i]
          a<-unlist(strsplit(a,split = ""))
          b<-df$fullname[j]
          b<-unlist(strsplit(b,split = ""))
          q<-LCS(a,b)
          ratio <- length(q$LCS)/(min(length(q$a),length(q$b)))
          if(ratio >=Threshold)
          {
            del<-c(del,j)
          }	      
        }
      
      if(length(del)!=1){
        del<-del[-1]
        df[del,]<-c(0,0,0,0,0,0) } 
      
    }
  return(df)
}


#EMPTY DATAFRAME

datanew <- data.frame( ID=integer(),ln=character(),dob=character(),gn=factor(),fn=character(),fullname=character(),day=integer(),month=integer(),year=integer(),stringsAsFactors=FALSE)

df1<-subset(data,data$gn==1)
df2<-subset(data,data$gn==2)
li1<-table(df1$dob)
li2<-table(df2$dob)

#FEMALE
for(i in 1:length(table(df1$dob)))
{k1<-c(which(df1$dob==names(li1[i])))
df<-df1[k1,]
len = length(df$fullname)
dfn<-mySecFun(df,len)
datanew<-rbind(datanew,dfn)
}

#MALE
for(i in 1:length(table(df2$dob)))
{k2<-c(which(df2$dob==names(li2[i])))
df<-df2[k2,]
len = length(df$fullname)
dfn<-mySecFun(df,len)
datanew<-rbind(datanew,dfn)
}

t<-which(datanew$gn==0)

FinalList<-datanew[-t,]

Output <-SampleData[FinalList$ID,]
Output<-Output[,2:5]