#RCODE

# First get the training and testing data set from folder
train<-read.csv("investmentsData.csv")
test <- read.csv("samplesubmission.csv")
test$Date <- 7
library(rpart)

#Normalize the variables that you think are varying too much and also separate the outliers.
train$Adv <- (train$Advisor_Id - mean(train$Advisor_Id)) / sd(train$Advisor_Id)

test$Adv <- (test$Advisor_Id - mean(train$Advisor_Id)) / sd(train$Advisor_Id)

test$Morningstar.Category <- test$Transaction_Type 
train$Morningstar.Category <- as.integer(as.factor(train$Morningstar.Category))
test$Morningstar.Category <- as.integer(as.factor(test$Morningstar.Category))
train1 <- subset(train,train$Morningstar.Category == 1)
train2 <- subset(train,train$Morningstar.Category != 1)
test2 <- subset(test,test$Morningstar.Category != 1)
test1 <- subset(test,test$Morningstar.Category == 1)

test1$zVar1 <- (test1$Investment_Id - mean(train1$Investment_Id)) / sd(train1$Investment_Id)
train1$zVar1 <- (train1$Investment_Id - mean(train1$Investment_Id)) / sd(train1$Investment_Id)

test2$zVar2 <- (test2$Investment_Id -  mean(train2$Investment_Id)) / sd(train2$Investment_Id)
train2$zVar2 <- (train2$Investment_Id - mean(train2$Investment_Id)) / sd(train2$Investment_Id)

#Start predicting the things which are not given in test dataset on basis of data of training dataset
model <- glm(Rating ~ Adv+Morningstar.Category + zVar2 + Date,data = train2)
pred <- predict(model,newdata = test2)
test2$Rating <- round(pred)
test1$Rating <- " "



model1 <- glm(X1.Yr...Rank ~ Adv+zVar2+Rating + Morningstar.Category + Date, data = train2)
pred <- predict(model1,newdata = test2)
test2$X1.Yr...Rank<-round(pred)



model1 <- glm(X3.Yr...Rank ~Adv+zVar2+X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train2)
pred <- predict(model1,newdata = test2)
test2$X3.Yr...Rank<-round(pred)

model1 <- glm(X5.Yr...Rank ~ Adv+zVar2+X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train2)
pred <- predict(model1,newdata = test2)
test2$X5.Yr...Rank<-round(pred)
############################################################


train3 <- subset(train2,is.na(train2$X10.Yr...Rank))
train4 <- subset(train2,!is.na(train2$X10.Yr...Rank))

a<-c(intersect(test2$Investment_Id,train3$Investment_Id))
a<-as.vector(a)
test3<-subset(test2,test2$Investment_Id == 106023 | test2$Investment_Id ==106364 | test2$Investment_Id ==106366 |test2$Investment_Id == 111277 | test2$Investment_Id ==120816 | test2$Investment_Id ==120819 | test2$Investment_Id ==120821 |test2$Investment_Id == 120823 | test2$Investment_Id ==122057 )

b<-c(intersect(test2$Investment_Id,train4$Investment_Id))
b<-as.vector(b)
test4<-subset(test2,test2$Investment_Id == 105561 |test2$Investment_Id == 105564 |test2$Investment_Id == 106020 )


test3$X10.Yr...Rank<- " "


model4 <- glm(X10.Yr...Rank ~ Adv+zVar2+X5.Yr...Rank + X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train4)
pred <- predict(model4,newdata = test4)
test4$X10.Yr...Rank<-pred




model5 <- glm( X1.Yr.Return ~ Adv+zVar2+ X10.Yr...Rank + X5.Yr...Rank + X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train4)
pred <- predict(model5,newdata = test4)
test4$X1.Yr.Return<-pred

model5 <- glm( X3.Yr.Return ~ Adv+zVar2 + X1.Yr.Return + X10.Yr...Rank + X5.Yr...Rank + X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train4)
pred <- predict(model5,newdata = test4)
test4$X3.Yr.Return <-pred

model5 <- glm( X5.Yr.Return ~ Adv+zVar2+ X3.Yr.Return + X1.Yr.Return + X10.Yr...Rank + X5.Yr...Rank + X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train4)
pred <- predict(model5,newdata = test4)
test4$X5.Yr.Return <-pred


model5 <- glm( X10.Yr.Return ~Adv+ zVar2 + X5.Yr.Return + X3.Yr.Return + X1.Yr.Return + X10.Yr...Rank + X5.Yr...Rank + X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train4)
pred <- predict(model5,newdata = test4)
test4$X10.Yr.Return  <-pred




model5 <- glm( X1.Yr.Return ~ Adv+zVar2  + X5.Yr...Rank + X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train3)
pred <- predict(model5,newdata = test3)
test3$X1.Yr.Return<-pred

model5 <- glm( X3.Yr.Return ~ Adv+zVar2 + X1.Yr.Return + X5.Yr...Rank + X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train3)
pred <- predict(model5,newdata = test3)
test3$X3.Yr.Return <-pred

model5 <- glm( X5.Yr.Return ~ Adv+zVar2 + X3.Yr.Return + X1.Yr.Return  + X5.Yr...Rank + X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train3)
pred <- predict(model5,newdata = test3)
test3$X5.Yr.Return <-pred

test3$X10.Yr.Return <- " "












######################################
train4$zVar3 <- (train4$Net.Flows - mean(train4$Net.Flows)) / sd(train4$Net.Flows)
train3$zVar4 <- (train3$Net.Flows - mean(train3$Net.Flows)) / sd(train3$Net.Flows)





model5 <- glm( zVar3 ~ X10.Yr.Return +Adv+ zVar2 + X5.Yr.Return + X3.Yr.Return + X1.Yr.Return + X10.Yr...Rank + X5.Yr...Rank + X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train4)
pred <- predict(model5,newdata = test4)
test4$zVar3<-pred



model5 <- glm( zVar4 ~ X5.Yr.Return +Adv+ zVar2 + X3.Yr.Return + X1.Yr.Return  + X5.Yr...Rank + X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train3)
pred <- predict(model5,newdata = test3)
test3$zVar4 <-pred

















####################################
model5 <- lm( Code_1 ~ zVar3 +Adv+ zVar2+ X10.Yr.Return  + X5.Yr.Return + X3.Yr.Return + X1.Yr.Return + X10.Yr...Rank + X5.Yr...Rank + X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train4)
pred <- predict(model5,newdata = test4)
test4$Code_1 <-round(pred)



model5 <- lm( Code_1 ~ zVar4+ zVar2 +Adv+ X5.Yr.Return + X3.Yr.Return + X1.Yr.Return  + X5.Yr...Rank + X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train3)
pred <- predict(model5,newdata = test3)
test3$Code_1 <-round(pred)

####################################3
model5 <- lm( Code_2 ~ Code_1 + zVar3 +Adv+ zVar2 + X10.Yr.Return  + X5.Yr.Return + X3.Yr.Return + X1.Yr.Return + X10.Yr...Rank + X5.Yr...Rank + X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train4)
pred <- predict(model5,newdata = test4)
test4$Code_2 <-round(pred)



model5 <- lm(Code_2 ~ Code_1 + zVar4 +Adv+ zVar2+ X5.Yr.Return  + X3.Yr.Return + X1.Yr.Return  + X5.Yr...Rank + X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train3)
pred <- predict(model5,newdata = test3)
test3$Code_2 <-round(pred)

###############################
model5 <- lm(Code_3 ~ Code_2 + Code_1 +Adv+ zVar3 + zVar2 + X10.Yr.Return + X5.Yr.Return + X3.Yr.Return + X1.Yr.Return + X10.Yr...Rank + X5.Yr...Rank + X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train4)
pred <- predict(model5,newdata = test4)
test4$Code_3 <-round(pred)



model5 <- lm(Code_3 ~ Code_2 + Code_1 +Adv+ zVar4 + zVar2 + X5.Yr.Return  + X3.Yr.Return + X1.Yr.Return  + X5.Yr...Rank + X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train3)
pred <- predict(model5,newdata = test3)
test3$Code_3 <-round(pred)

#####################################
model5 <- lm(Code_4  ~ Code_3 + Code_2 +Adv+ Code_1 + zVar3 + zVar2 + X10.Yr.Return  + X5.Yr.Return + X3.Yr.Return + X1.Yr.Return + X10.Yr...Rank + X5.Yr...Rank + X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train4)
pred <- predict(model5,newdata = test4)
test4$Code_4 <-round(pred)



model5 <- lm(Code_4  ~ Code_3 + Code_2 +Adv+ Code_1 + zVar4 + zVar2 + X5.Yr.Return  + X3.Yr.Return + X1.Yr.Return  + X5.Yr...Rank + X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train3)
pred <- predict(model5,newdata = test3)
test3$Code_4 <-round(pred)

#####################################
model5 <- lm(Code_5 ~ Code_4 + Code_3 +Adv+ Code_2 + Code_1 + zVar3 + zVar2 + X10.Yr.Return + X5.Yr.Return + X3.Yr.Return + X1.Yr.Return + X10.Yr...Rank + X5.Yr...Rank + X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train4)
pred <- predict(model5,newdata = test4)
test4$Code_5 <-round(pred)



model5 <- lm(Code_5 ~ Code_4  + Code_3 + Code_2 +Adv+ Code_1 + zVar4 + zVar2 + X5.Yr.Return  + X3.Yr.Return + X1.Yr.Return  + X5.Yr...Rank + X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train3)
pred <- predict(model5,newdata = test3)
test3$Code_5 <-round(pred)







model5 <- lm(Amount ~ Code_5 + Code_4 + Adv+Code_3 + Code_2 + Code_1 + zVar3 + zVar2 + X10.Yr.Return + X5.Yr.Return + X3.Yr.Return + X1.Yr.Return + X10.Yr...Rank + X5.Yr...Rank + X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train4)
pred <- predict(model5,newdata = test4)
test4$Amount <-pred


model5 <- lm(Amount ~ Code_5 + Code_4  +Adv+ Code_3 + Code_2 + Code_1 + zVar4 + zVar2 + X5.Yr.Return  + X3.Yr.Return + X1.Yr.Return  + X5.Yr...Rank + X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train3)
pred <- predict(model5,newdata = test3)
test3$Amount<-pred


train4$Amt <- (train4$Amount - mean(train4$Amount)) / sd(train4$Amount)

test4$Amt <- (test4$Amount - mean(train4$Amount)) / sd(train4$Amount)


train3$Amt <- (train3$Amount - mean(train3$Amount)) / sd(train3$Amount)

test3$Amt <- (test3$Amount - mean(train3$Amount)) / sd(train3$Amount)


model5 <- glm(Transaction_Type ~ Amt+ Code_5 + Code_4 + Adv+Code_3 + Code_2 + Code_1 + zVar3 + zVar2 + X10.Yr.Return + X5.Yr.Return + X3.Yr.Return + X1.Yr.Return + X10.Yr...Rank + X5.Yr...Rank + X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train4,family = binomial)
pred <- predict(model5,newdata = test4,type="response")
test4$Transaction_Type1 <-pred



model5 <- glm(Transaction_Type ~ Amt+Code_5 + Code_4  +Adv+ Code_3 + Code_2 + Code_1 + zVar4 + zVar2 + X5.Yr.Return  + X3.Yr.Return + X1.Yr.Return  + X5.Yr...Rank + X3.Yr...Rank + X1.Yr...Rank+ Rating + Morningstar.Category + Date, data = train3,family = binomial)
pred <- predict(model5,newdata = test3,type="response")
test3$Transaction_Type1 <-pred

test3$Transaction_Type1[which(test3$Transaction_Type1<0.5)]<-0
test4$Transaction_Type1[which(test4$Transaction_Type1<0.5)]<-0

test3$Transaction_Type1[which(test3$Transaction_Type1>=0.5)]<-1
test4$Transaction_Type1[which(test4$Transaction_Type1>=0.5)]<-1

######################################################
model5 <- lm( Code_1 ~ Adv+ zVar1+Morningstar.Category + Date, data = train1)
pred <- predict(model5,newdata = test1)
test1$Code_1 <-round(pred)

########################################################

model5 <- lm( Code_2 ~ Code_1 + Adv+ zVar1+Morningstar.Category + Date, data = train1)
pred <- predict(model5,newdata = test1)
test1$Code_2 <-round(pred)
#################################################

model5 <- lm( Code_3 ~ Code_2 + Code_1 + Adv+ zVar1+Morningstar.Category + Date, data = train1)
pred <- predict(model5,newdata = test1)
test1$Code_3 <-round(pred)
##################################################

model5 <- lm(Code_4  ~ Code_3 + Code_2 + Code_1 + Adv+ zVar1+Morningstar.Category + Date, data = train1)
pred <- predict(model5,newdata = test1)
test1$Code_4 <-round(pred)

#############################################


model5 <- lm(Code_5 ~ Code_4 + Code_3 + Code_2 + Code_1 + Adv+ zVar1+Morningstar.Category + Date, data = train1)
pred <- predict(model5,newdata = test1)
test1$Code_5 <-round(pred)




library(randomForest)

train1$Amount <- (train1$Amount - mean(train1$Amount)) / sd(train1$Amount)


model5 <- randomForest(Amount ~ Code_5 + Code_4 + Code_3 + Code_2 + Code_1 + Adv+ zVar1+Morningstar.Category + Date, data = train1)
pred <- predict(model5,newdata = test1)
test1$Amount <-round(pred)


train1$Transaction_Type<-as.factor(train1$Transaction_Type)
model5 <- randomForest(Transaction_Type ~ Amount + Code_5 + Code_4 + Code_3 + Code_2 + Code_1 + Adv+ zVar1+Morningstar.Category + Date, data = train1)
pred <- predict(model5,newdata = test1)
test1$Transaction_Type <-pred
#############################################
tst1<-test1[,c(1,2,3)]
tst3<-test3[,c(1,2,23)]
tst4<-test4[,c(1,2,23)]

tst1$Transaction_Type<-as.integer(tst1$Transaction_Type)
tst1$Transaction_Type[which(as.integer(tst1$Transaction_Type)==1)]<-0
tst1$Transaction_Type[which(as.integer(tst1$Transaction_Type)==2)]<-1

tst3$Amount[which(tst3$Amount>0)]<-0
tst3$Amount[which(tst3$Amount<0)]<-1

tst4$Amount[which(tst4$Amount>0)]<-0
tst4$Amount[which(tst4$Amount<0)]<-1

tst4$Transaction_Type<-tst4$Amount
tst3$Transaction_Type<-tst3$Amount


#Finally when all transaction_type is predicted combeine all final dataset and get the final submission ".csv" file.


final<-rbind(tst1,subset(tst3[,c(1,2,4)]),subset(tst4[,c(1,2,4)]))
