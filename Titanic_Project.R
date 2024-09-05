#Loading the Data
Titanic.train <- read.csv(file = "train.csv", stringsAsFactors = F)
Titanic.test <- read.csv(file = "test.csv", stringsAsFactors = F)

#Merging the data into one 
Titanic.train$isTrain <- T
Titanic.test$isTrain <- F

Titanic.test$Survived <- NA

Titanic.full <- rbind(Titanic.train,Titanic.test)

#Cleaning the data , Handling the missing values
Titanic.full[Titanic.full$Embarked=="","Embarked"] <- "U"

Age.median <- median(Titanic.full$Age,na.rm = T)
Titanic.full[is.na(Titanic.full$Age),"Age"] <- Age.median

Fare.median <- median(Titanic.full$Fare,na.rm = T)
Titanic.full[is.na(Titanic.full$Fare),"Fare"] <- Fare.median

#Converting categorical values to factor
Titanic.full$Pclass<- as.factor(Titanic.full$Pclass)
Titanic.full$Sex<- as.factor(Titanic.full$Sex)
Titanic.full$Embarked<- as.factor(Titanic.full$Embarked)

#Splitting the data back into train and test 
Titanic.train <- Titanic.full[Titanic.full$isTrain==T,]
Titanic.test <- Titanic.full[Titanic.full$isTrain==F,]

#Converting the predicting value into factor
Titanic.train$Survived <- as.factor(Titanic.train$Survived)


#Making a formula (separating dependent and independent variables)
Survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived.formula<- as.formula(Survived.equation)

#Using randomForest to make the model 
install.packages("randomForest")
library(randomForest)
Titanic.model <- randomForest(Survived.formula,Titanic.train,ntree = 500, mtry = 5, nodesize = 0.01*nrow(Titanic.test) )

#Testing the model on test dataset
Survived <- predict(Titanic.model,Titanic.test)

#Creating a csv file for submission
PassengerId <- Titanic.test$PassengerId
df <- data.frame(PassengerId)
df$Survived <- Survived
write.csv(df,file = "Titanic_submission.csv",row.names = F)