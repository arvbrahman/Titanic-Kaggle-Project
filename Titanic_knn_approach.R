#Loading the original data sets
T_train <- read.csv(file = "train.csv",stringsAsFactors = F)
T_test <- read.csv(file = "test.csv", stringsAsFactors = F)

#Merging the train and test into one
T_train$isTrain <- T
T_test$isTrain <- F  
T_test$Survived <- NA
T_full <- rbind(T_train,T_test)

#Cleaning data
T_full[T_full$Embarked=="","Embarked"] <- "U"

Age.mean <- mean(T_full$Age,na.rm = T)
T_full[is.na(T_full$Age),"Age"] <- Age.mean
Fare.mean <- mean(T_full$Fare,na.rm = T)
T_full[is.na(T_full$Fare),"Fare"] <- Fare.mean

#