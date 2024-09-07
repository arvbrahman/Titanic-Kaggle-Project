#Loading the data 
Titanic.test <- read.csv("test.csv",stringsAsFactors = F)
Titanic.train <- read.csv("train.csv",stringsAsFactors = F)


#Merging into One
Titanic.train$isTrain <- T
Titanic.test$isTrain <- F
Titanic.test$Survived <- NA
Titanic.full <- rbind(Titanic.train,Titanic.test)


#Cleaning the data , Handling the missing values, Removing Unnecessary columns
Age.median <- median(Titanic.full$Age,na.rm = T)
Titanic.full[is.na(Titanic.full$Age),"Age"] <- Age.median
Fare.median <- median(Titanic.full$Fare,na.rm = T)
Titanic.full[is.na(Titanic.full$Fare),"Fare"] <- Fare.median

keeps <- c("PassengerId","Survived","Pclass","Age","SibSp","Parch","Fare","isTrain")
Titanic.full <- Titanic.full[keeps]


#Splitting back and scaling
Titanic.train <- Titanic.full[Titanic.full$isTrain==T,]
Titanic.test <- Titanic.full[Titanic.full$isTrain==F,]

PassengerId <- Titanic.test$PassengerId
PI<- as.data.frame(PassengerId)

keeps <- c("Survived","Pclass","Age","SibSp","Parch","Fare")
Titanic.train <- Titanic.train[keeps]
Titanic.test <- Titanic.test[keeps]

Train_scaled <- scale(Titanic.train[-1])
Test_scaled <- scale(Titanic.test[-1])


#Prdeiction
model <- knn(Train_scaled,
             Test_scaled,
             Titanic.train$Survived,
             k=10)

#CSV file for submission
PI$Survived<- model 
write.csv(PI,"Titanic_submission_knn.csv",row.names = F)
