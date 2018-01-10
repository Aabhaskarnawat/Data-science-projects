#SUBMISSION PART 1
# GETTING DATA
data<-read.csv("sample_data_test.csv")

# INPUT "NULL" VALUES THROUGH KNN TECHNIQUE
install.packages("VIM")
library(VIM)

data$minor[which(data$minor=="NULL")]<-NA
final<-kNN(data,variable = "minor")
final$marital_status[which(final$marital_status=="NULL")]<-NA
final<-kNN(final,variable = "marital_status")
final$sex[which(final$sex=="NULL")]<-NA
final<-kNN(final,variable = "sex")
final$educational_status[which(final$educational_status=="NULL")]<-NA
final<-kNN(final,variable = "educational_status")
final$income[which(final$income=="NULL")]<-NA
final<-kNN(final,variable = "income")
final$date_of_birth[which(final$date_of_birth=="NULL")]<-NA
final<-kNN(final,variable = "date_of_birth")
final<-final[,-which(names(final)=="date_of_birth")]


# GETTING SIGNIFICANT VARIABLES
install.packages("Boruta")
library(Boruta)
set.seed(123)
boruta.train <- Boruta(dependents~., data = final, doTrace = 2)
final.boruta <- TentativeRoughFix(boruta.train)
boruta.df <- attStats(final.boruta)
print(boruta.df)

#SEPERATING SIGNIFICANT VARIABLES ONLY :-
DeleteNames<-rownames(boruta.df)[which(boruta.df$decision == "Rejected")]
listD<-0
lenA<-length(names(final))
lenB<-length(DeleteNames)
for(i in 1:lenA)
{
  for(j in 1:lenB)
  {
    if(names(final)[i] == DeleteNames [j])
    {  listD<-c(listD,i)  }
  }
}

final<-final[,-listD]
# GROUPING THROUGH CLUSTER FORMATION
install.packages("cluster")
library(cluster)
pamClus <- pam(final, 5)

# SEPERATING CLUSTERS
listA<-c(which(pamClus$clustering==1))
listB<-c(which(pamClus$clustering==2))
listC<-c(which(pamClus$clustering==3))
listD<-c(which(pamClus$clustering==4))
listE<-c(which(pamClus$clustering==5))

IDSET1<-final[listA,]
IDSET2<-final[listB,]
IDSET3<-final[listC,]
IDSET4<-final[listD,]
IDSET5<-final[listE,]

GROUP1 <- IDSET1$cid_no
GROUP2 <- IDSET2$cid_no
GROUP3 <- IDSET3$cid_no
GROUP4 <- IDSET4$cid_no
GROUP5 <- IDSET5$cid_no

