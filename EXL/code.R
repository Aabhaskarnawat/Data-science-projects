Demographics <- read.csv("Demographics.csv")
str(Demographics)
library(caret)
Demographics$Gender <- as.numeric(Demographics$Gender)
Demographics$Marital_Status <- as.numeric(Demographics$Marital_Status)
Demographics$Owns_Home <- as.numeric(Demographics$Owns_Home)
preproc=preProcess(Demographics[2:6])
Demographics=predict(preproc,Demographics)
distances <- dist(Demographics[2:6],method = "euclidean")
clusterDemographics <- hclust(distances,method = "ward.D")
##plot(clusterDemographics)
clusterGroups <- cutree(clusterDemographics,k=8)
cluster1 <- subset(Demographics,clusterGroups==1)
cluster2 <- subset(Demographics,clusterGroups==2)
cluster3 <- subset(Demographics,clusterGroups==3)
cluster4 <- subset(Demographics,clusterGroups==4)
cluster5 <- subset(Demographics,clusterGroups==5)
cluster6 <- subset(Demographics,clusterGroups==6)
cluster7 <- subset(Demographics,clusterGroups==7)
cluster8 <- subset(Demographics,clusterGroups==8)

test <- read.csv("Ping_Information.csv")
newtest<-test[which(test$lat>37.783),]
newtest<-newtest[which(newtest$lng<(-122.4)),]

train <- read.csv("Store_Mapping.csv")

train$lat<-train$latitude
train$lng<-train$longitude

model<-lm(x ~ lat+lng,data=newtest)
train$x<-predict(model,newdata=train)

model<-lm(y ~ lat+lng,data=newtest)
train$y<-predict(model,newdata=train)

############################ 

CLUSTERTEST1<-NA
for(i in 1:103){CLUSTERTEST1<-rbind(CLUSTERTEST1,subset(newtest,(newtest$ID==cluster1$Id[i])))}
CLUSTERTEST1<-CLUSTERTEST1[-1,]

CLUSTERTEST2<-NA
for(i in 1:50){CLUSTERTEST2<-rbind(CLUSTERTEST2,subset(newtest,(newtest$ID==cluster2$Id[i])))}
CLUSTERTEST2<-CLUSTERTEST2[-1,]

CLUSTERTEST3<-NA
for(i in 1:142){CLUSTERTEST3<-rbind(CLUSTERTEST3,subset(newtest,(newtest$ID==cluster3$Id[i])))}
CLUSTERTEST3<-CLUSTERTEST3[-1,]

CLUSTERTEST4<-NA
for(i in 1:73){CLUSTERTEST4<-rbind(CLUSTERTEST4,subset(newtest,(newtest$ID==cluster4$Id[i])))}
CLUSTERTEST4<-CLUSTERTEST4[-1,]

CLUSTERTEST5<-NA
for(i in 1:72){CLUSTERTEST5<-rbind(CLUSTERTEST5,subset(newtest,(newtest$ID==cluster5$Id[i])))}
CLUSTERTEST5<-CLUSTERTEST5[-1,]

CLUSTERTEST6<-NA
for(i in 1:48){CLUSTERTEST6<-rbind(CLUSTERTEST6,subset(newtest,(newtest$ID==cluster6$Id[i])))}
CLUSTERTEST6<-CLUSTERTEST6[-1,]

CLUSTERTEST7<-NA
for(i in 1:51){CLUSTERTEST7<-rbind(CLUSTERTEST7,subset(newtest,(newtest$ID==cluster7$Id[i])))}
CLUSTERTEST7<-CLUSTERTEST7[-1,]

CLUSTERTEST8<-NA
for(i in 1:28){CLUSTERTEST8<-rbind(CLUSTERTEST8,subset(newtest,(newtest$ID==cluster8$Id[i])))}
CLUSTERTEST8<-CLUSTERTEST8[-1,]

#############################################################################

preproc=preProcess(CLUSTERTEST1[8:10])
CLUSTERTEST1=predict(preproc,CLUSTERTEST1)
distances <- dist(CLUSTERTEST1[8:10],method = "euclidean")

clusterDemographics <- hclust(distances,method = "ward.D")
##plot(clusterDemographics)
clusterGroups <- cutree(clusterDemographics,k=6)
finalcluster1 <- subset(CLUSTERTEST1,clusterGroups==1)
finalcluster2 <- subset(CLUSTERTEST1,clusterGroups==2)
finalcluster3 <- subset(CLUSTERTEST1,clusterGroups==3)
finalcluster4 <- subset(CLUSTERTEST1,clusterGroups==4)
finalcluster5 <- subset(CLUSTERTEST1,clusterGroups==5)
finalcluster6 <- subset(CLUSTERTEST1,clusterGroups==6)

##########################################################
#############################################################################

preproc=preProcess(CLUSTERTEST2[8:10])
CLUSTERTEST2=predict(preproc,CLUSTERTEST2)
distances <- dist(CLUSTERTEST2[8:10],method = "euclidean")

clusterDemographics <- hclust(distances,method = "ward.D")
##plot(clusterDemographics)
clusterGroups <- cutree(clusterDemographics,k=5)
finalcluster7 <- subset(CLUSTERTEST2,clusterGroups==1)
finalcluster8 <- subset(CLUSTERTEST2,clusterGroups==2)
finalcluster9 <- subset(CLUSTERTEST2,clusterGroups==3)
finalcluster10 <- subset(CLUSTERTEST2,clusterGroups==4)
finalcluster11<- subset(CLUSTERTEST2,clusterGroups==5)

##########################################################

#############################################################################

preproc=preProcess(CLUSTERTEST3[8:10])
CLUSTERTEST3=predict(preproc,CLUSTERTEST3)
distances <- dist(CLUSTERTEST3[8:10],method = "euclidean")

clusterDemographics <- hclust(distances,method = "ward.D")
##plot(clusterDemographics)
clusterGroups <- cutree(clusterDemographics,k=5)
finalcluster12 <- subset(CLUSTERTEST3,clusterGroups==1)
finalcluster13<- subset(CLUSTERTEST3,clusterGroups==2)
finalcluster14<- subset(CLUSTERTEST3,clusterGroups==3)
finalcluster15 <- subset(CLUSTERTEST3,clusterGroups==4)
finalcluster16<- subset(CLUSTERTEST3,clusterGroups==5)

##########################################################
#############################################################################

preproc=preProcess(CLUSTERTEST4[8:10])
CLUSTERTEST4=predict(preproc,CLUSTERTEST4)
distances <- dist(CLUSTERTEST4[8:10],method = "euclidean")

clusterDemographics <- hclust(distances,method = "ward.D")
##plot(clusterDemographics)
clusterGroups <- cutree(clusterDemographics,k=4)
finalcluster17 <- subset(CLUSTERTEST4,clusterGroups==1)
finalcluster18 <- subset(CLUSTERTEST4,clusterGroups==2)
finalcluster19 <- subset(CLUSTERTEST4,clusterGroups==3)
finalcluster20 <- subset(CLUSTERTEST4,clusterGroups==4)

##########################################################
#############################################################################

preproc=preProcess(CLUSTERTEST5[8:10])
CLUSTERTEST5=predict(preproc,CLUSTERTEST5)
distances <- dist(CLUSTERTEST5[8:10],method = "euclidean")

clusterDemographics <- hclust(distances,method = "ward.D")
##plot(clusterDemographics)
clusterGroups <- cutree(clusterDemographics,k=6)
finalcluster21 <- subset(CLUSTERTEST5,clusterGroups==1)
finalcluster22 <- subset(CLUSTERTEST5,clusterGroups==2)
finalcluster23 <- subset(CLUSTERTEST5,clusterGroups==3)
finalcluster24 <- subset(CLUSTERTEST5,clusterGroups==4)
finalcluster25<- subset(CLUSTERTEST5,clusterGroups==5)
finalcluster26<- subset(CLUSTERTEST5,clusterGroups==6)

##########################################################
#############################################################################

preproc=preProcess(CLUSTERTEST6[8:10])
CLUSTERTEST6=predict(preproc,CLUSTERTEST6)
distances <- dist(CLUSTERTEST6[8:10],method = "euclidean")

clusterDemographics <- hclust(distances,method = "ward.D")
##plot(clusterDemographics)
clusterGroups <- cutree(clusterDemographics,k=4)
finalcluster27 <- subset(CLUSTERTEST6,clusterGroups==1)
finalcluster28 <- subset(CLUSTERTEST6,clusterGroups==2)
finalcluster29 <- subset(CLUSTERTEST6,clusterGroups==3)
finalcluster30 <- subset(CLUSTERTEST6,clusterGroups==4)


##########################################################
#############################################################################

preproc=preProcess(CLUSTERTEST7[8:10])
CLUSTERTEST7=predict(preproc,CLUSTERTEST7)
distances <- dist(CLUSTERTEST7[8:10],method = "euclidean")

clusterDemographics <- hclust(distances,method = "ward.D")
##plot(clusterDemographics)
clusterGroups <- cutree(clusterDemographics,k=4)
finalcluster31 <- subset(CLUSTERTEST7,clusterGroups==1)
finalcluster32 <- subset(CLUSTERTEST7,clusterGroups==2)
finalcluster33 <- subset(CLUSTERTEST7,clusterGroups==3)
finalcluster34 <- subset(CLUSTERTEST7,clusterGroups==4)


##########################################################
#############################################################################

preproc=preProcess(CLUSTERTEST8[8:10])
CLUSTERTEST8=predict(preproc,CLUSTERTEST8)
distances <- dist(CLUSTERTEST8[8:10],method = "euclidean")

clusterDemographics <- hclust(distances,method = "ward.D")
##plot(clusterDemographics)
clusterGroups <- cutree(clusterDemographics,k=5)
finalcluster35 <- subset(CLUSTERTEST8,clusterGroups==1)
finalcluster36 <- subset(CLUSTERTEST8,clusterGroups==2)
finalcluster37 <- subset(CLUSTERTEST8,clusterGroups==3)
finalcluster38 <- subset(CLUSTERTEST8,clusterGroups==4)
finalcluster39 <- subset(CLUSTERTEST8,clusterGroups==5)

##########################################################

library(randomForest)

model<-randomForest(Store_Name~lat+lng+x+y+Floor_Index , data=train,ntree=200)

finalcluster1$Store_Name<-predict(model,finalcluster1)
finalcluster2$Store_Name<-predict(model,finalcluster2)
finalcluster3$Store_Name<-predict(model,finalcluster3)
finalcluster4$Store_Name<-predict(model,finalcluster4)
finalcluster5$Store_Name<-predict(model,finalcluster5)
finalcluster6$Store_Name<-predict(model,finalcluster6)
finalcluster7$Store_Name<-predict(model,finalcluster7)
finalcluster8$Store_Name<-predict(model,finalcluster8)
finalcluster9$Store_Name<-predict(model,finalcluster9)
finalcluster10$Store_Name<-predict(model,finalcluster10)
finalcluster11$Store_Name<-predict(model,finalcluster11)
finalcluster12$Store_Name<-predict(model,finalcluster12)
finalcluster13$Store_Name<-predict(model,finalcluster13)
finalcluster14$Store_Name<-predict(model,finalcluster14)
finalcluster15$Store_Name<-predict(model,finalcluster15)
finalcluster16$Store_Name<-predict(model,finalcluster16)
finalcluster17$Store_Name<-predict(model,finalcluster17)
finalcluster18$Store_Name<-predict(model,finalcluster18)
finalcluster19$Store_Name<-predict(model,finalcluster19)
finalcluster20$Store_Name<-predict(model,finalcluster20)
finalcluster21$Store_Name<-predict(model,finalcluster21)
finalcluster22$Store_Name<-predict(model,finalcluster22)
finalcluster23$Store_Name<-predict(model,finalcluster23)
finalcluster24$Store_Name<-predict(model,finalcluster24)
finalcluster25$Store_Name<-predict(model,finalcluster25)
finalcluster26$Store_Name<-predict(model,finalcluster26)
finalcluster27$Store_Name<-predict(model,finalcluster27)
finalcluster28$Store_Name<-predict(model,finalcluster28)
finalcluster29$Store_Name<-predict(model,finalcluster29)
finalcluster30$Store_Name<-predict(model,finalcluster30)
finalcluster31$Store_Name<-predict(model,finalcluster31)
finalcluster32$Store_Name<-predict(model,finalcluster32)
finalcluster33$Store_Name<-predict(model,finalcluster33)
finalcluster34$Store_Name<-predict(model,finalcluster34)
finalcluster35$Store_Name<-predict(model,finalcluster35)
finalcluster36$Store_Name<-predict(model,finalcluster36)
finalcluster37$Store_Name<-predict(model,finalcluster37)
finalcluster38$Store_Name<-predict(model,finalcluster38)
finalcluster39$Store_Name<-predict(model,finalcluster39)

FINAL<-rbind(finalcluster1,finalcluster2,finalcluster3,finalcluster4,finalcluster5,finalcluster6,finalcluster7,finalcluster8,finalcluster9,finalcluster10,finalcluster11,finalcluster12,finalcluster13,finalcluster14,finalcluster15,finalcluster16,finalcluster17,finalcluster18,finalcluster19,finalcluster20,finalcluster21,finalcluster22,finalcluster23,finalcluster24,finalcluster25,finalcluster26,finalcluster27,finalcluster28,finalcluster29,finalcluster30,finalcluster31,finalcluster32,finalcluster33,finalcluster34,finalcluster35,finalcluster36,finalcluster37,finalcluster38,finalcluster39)
###write.csv(FINAL, "C:/Users/A.Karnawat/Downloads/EXL/test2.csv")
###timedata <- read.csv("test2.csv")

r<-data.frame(matrix(NA,nrow=567,ncol=2))
FINAL$Store_Name<-as.character(FINAL$Store_Name)


if(FALSE){
  ############### Most store visited if taken unique per day#####################
  
  for(i in 1:567)
  {
    a<-timedata[which(timedata$ID == i),]
    b<-sort(unique(a$Month))
    f<-0
    for(j in 1:length(b))
    {
      c<-a[which(a$Month==b[j]),]
      d<-sort(unique(c$Date))
      for(k in 1:length(d))
      {
        e<-a[which(c$Date==d[k]),]
        f<-c(f,sort(unique(e$Store_Name)))
        
      }
    }
    
    r[i,2]<-names(which.max(table(f)))
  }
  
  
}









###################### Most store visited Overall ####################

for(i in 1:567)
{
  a<-FINAL[which(FINAL$ID == i),]
  
  f<-sort(a$Store_Name)
  
  r[i,1]<-names(which.max(table(f)))
}

##### TO GET CATEGORY #####
category<-read.csv("category.csv")
FINAL$CAT<-NA
category$Fine_Category<-as.character(category$Fine_Category)
for(i in 1:154){FINAL$CAT[which(FINAL$Store_Name==category$Store_Name[i])]<-category$Fine_Category[i]}


########



for(i in 1:567)
{
  a<-FINAL[which(FINAL$ID == i),]
  
  f<-sort(a$CAT)
  
  r[i,2]<-names(which.max(table(f)))
}

write.csv(r, "C:/Users/A.Karnawat/Downloads/EXL/FINALIZE.csv")