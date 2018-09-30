
#Reading Testing And Training Data
train<-read.csv("train.csv",stringsAsFactors = F)
test<-read.csv("test.csv",stringsAsFactors = F)

# created New Variables Such As-
#Age:-Age of pessenger
#DaysLeft:-Days left for travel
#Distributed age as below 15,15-30,30-50 and above 50
#FlighHour:- At which Hour of day flight is taking off.
#Gender:-Male Or Female
#doc:- If person is doctor or not
#L1 & L2 :- Derived from observing name column

train$Date.of.Birth <- as.Date(train$Date.of.Birth)
df <- data.frame(date = train$Date.of.Birth,
                 year = as.numeric(format(train$Date.of.Birth, format = "%Y")),
                 month = as.numeric(format(train$Date.of.Birth, format = "%m")),
                 day = as.numeric(format(train$Date.of.Birth, format = "%d")))

train$Flight.Date <- as.Date(train$Flight.Date)
df1 <- data.frame(date = train$Flight.Date,
                  year = as.numeric(format(train$Flight.Date, format = "%Y")),
                  month = as.numeric(format(train$Flight.Date, format = "%m")),
                  day = as.numeric(format(train$Flight.Date, format = "%d")))

train$Booking.Date <- as.Date(train$Booking.Date)
df2 <- data.frame(date = train$Booking.Date,
                  year = as.numeric(format(train$Booking.Date, format = "%Y")),
                  month = as.numeric(format(train$Booking.Date, format = "%m")),
                  day = as.numeric(format(train$Booking.Date, format = "%d")))

df1$DAYSLEFT<-(df1$year-df2$year)*365 + (df1$month-df2$month)*30 +(df1$day-df2$day)

train$daysleft<-df1$DAYSLEFT



df$age<-(max(df2$year)-df$year)
df$age55<-0
df$age55[c(which(df$age<=15))]<-1
df$age55[c(which(df$age>15 & df$age<=30))]<-2
df$age55[c(which(df$age>30 & df$age<=50))]<-3
df$age55[c(which(df$age>50))]<-4

train$age<-df$age
train$age55<-df$age55






train$FlighHour<-as.integer(c(sapply(strsplit(train$Flight.Time, ":"),`[`,1)))

train$L1<-as.integer(as.factor(c(sapply(strsplit(train$Name," "),`[`,2))))
train$L2<-as.integer(as.factor(c(sapply(strsplit(train$Name," "),`[`,3))))

train$From<-as.integer(as.factor(train$From))
train$To<-as.integer(as.factor(train$To))
train$Class<-as.integer(as.factor(train$Class))



library(stringr)

train$Gender[c(which(str_detect(train$Name, "Mrs")))]<-1
train$Gender[c(which(str_detect(train$Name, "Dr. F")))]<-1
train$Gender[c(which(str_detect(train$Name, "Miss")))]<-1
train$Gender[c(which(str_detect(train$Name, "Mr.")))]<-0
train$Gender[c(which(str_detect(train$Name, "Dr. M")))]<-0


train$doc<-0
train$doc[c(which(str_detect(test$Name, "Dr")))]<-1



###########################


test$Date.of.Birth <- as.Date(test$Date.of.Birth)
df <- data.frame(date = test$Date.of.Birth,
                 year = as.numeric(format(test$Date.of.Birth, format = "%Y")),
                 month = as.numeric(format(test$Date.of.Birth, format = "%m")),
                 day = as.numeric(format(test$Date.of.Birth, format = "%d")))



test$Flight.Date <- as.Date(test$Flight.Date)
df1 <- data.frame(date = test$Flight.Date,
                  year = as.numeric(format(test$Flight.Date, format = "%Y")),
                  month = as.numeric(format(test$Flight.Date, format = "%m")),
                  day = as.numeric(format(test$Flight.Date, format = "%d")))

test$Booking.Date <- as.Date(test$Booking.Date)
df2 <- data.frame(date = test$Booking.Date,
                  year = as.numeric(format(test$Booking.Date, format = "%Y")),
                  month = as.numeric(format(test$Booking.Date, format = "%m")),
                  day = as.numeric(format(test$Booking.Date, format = "%d")))

df1$DAYSLEFT<-(df1$year-df2$year)*365 + (df1$month-df2$month)*30 +(df1$day-df2$day)

test$daysleft<-df1$DAYSLEFT


df$age<-(max(df2$year)-df$year)
df$age55<-0
df$age55[c(which(df$age<=15))]<-1
df$age55[c(which(df$age>15 & df$age<=30))]<-2
df$age55[c(which(df$age>30 & df$age<=50))]<-3
df$age55[c(which(df$age>50))]<-4

test$age<-df$age
test$age55<-df$age55

test$L1<-as.integer(as.factor(c(sapply(strsplit(test$Name," "),`[`,2))))
test$L2<-as.integer(as.factor(c(sapply(strsplit(test$Name," "),`[`,3))))




test$FlighHour<-as.integer(c(sapply(strsplit(test$Flight.Time, ":"),`[`,1)))





test$From<-as.integer(as.factor(test$From))
test$To<-as.integer(as.factor(test$To))
test$Class<-as.integer(as.factor(test$Class))


test$Gender[c(which(str_detect(test$Name, "Dr. F")))]<-1
test$Gender[c(which(str_detect(test$Name, "Miss")))]<-1
test$Gender[c(which(str_detect(test$Name, "Mr.")))]<-0
test$Gender[c(which(str_detect(test$Name, "Dr. M")))]<-0
test$Gender[c(which(str_detect(test$Name, "Mrs")))]<-1

test$doc<-0
test$doc[c(which(str_detect(test$Name, "Dr")))]<-1








#################

## removed Name Column
train <- train[ ,-1]



###normalization Of Data

mdl<-mean(train$daysleft)
sdl<-sd(train$daysleft)

mfl<-mean(train$FlighHour)
sfl<-sd(train$FlighHour)


train$daysleft <- (train$daysleft - mdl) / sdl
train$FlighHour <- (train$FlighHour - mfl) / sfl
test$daysleft <- (test$daysleft - mdl) / sdl
test$FlighHour <- (test$FlighHour - mfl) / sfl

am<-mean(train$age)
ad<-sd(train$age)
train$age <- (train$age - am) / ad
test$age <- (test$age - am) / ad




q1<-max(train$L2)
train$L2 <- (train$L2)/q1
test$L2 <- (test$L2)/q1


q2<-max(train$L1)
train$L1 <- (train$L1)/q2
test$L1 <- (test$L1)/q2

q3<-max(train$From)
train$From <- (train$From)/q3
test$From <- (test$From)/q3


q4<-max(train$To)
train$To <- (train$To)/q4
test$To <- (test$To)/q4


### created some new combinations of variables by looking at the correlations. 
train$c2<-train$Class*train$age
train$d2<-train$Class*train$daysleft
train$e2<-train$Class*train$daysleft*train$FlighHour
train$f2<-train$From*train$To

test$c2<-test$Class*test$age
test$d2<-test$Class*test$daysleft
test$e2<-test$Class*test$daysleft*test$FlighHour
test$f2<-test$From*test$To




## Used Boruta package to determine the relations among variables
install.packages("Boruta")
library(Boruta)
set.seed(123)
boruta.train <- Boruta(Fare~c2+d2+e2+f2+g2+h2+age+daysleft+Class+From+To, data = train, doTrace = 2)
print(boruta.train)
final.boruta <- TentativeRoughFix(boruta.train)
boruta.df <- attStats(final.boruta)
print(boruta.df)






#fit <- rpart(log(Fare+1)~c2+d2+e2+f2+age+age55+daysleft+Class+From+To,method="anova", data=train,control=rpart.control(minsplit=25,cp=0))
#Prune the tree
#treeOptimal <- prune(fit,cp=fit$cptable[which.min(fit$cptable[,4]),1])
#Prediction
#test$Fare<-exp(predict(treeOptimal,newdata=test,method = "anova"))-1
#model2<-lm(Fare~age*Class+Class^2+daysleft^2+daysleft*Class+daysleft*Class*FlighHour+From*To+age*doc+L1^2+L2^2,data=train)


#Applied Random Forest Model with the existing and new variables that were created.
model<-randomForest(Fare~c2+d2+e2+f2+age+age55+daysleft+Class+From+To,train,ntree=200)
test$Fare<-predict(model,test)
ans<-test$Fare
write.csv(ans,"C:/Users/A.Karnawat/Desktop/JPMC/lRF2.csv")


