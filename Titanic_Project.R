#
Titanic.train <- read.csv(file = "train.csv", stringsAsFactors = F)
Titanic.test <- read.csv(file = "test.csv", stringsAsFactors = F)

#
Titanic.train$isTrain <- T
Titanic.test$isTrain <- F

Titanic.test$Survived <- NA

Titanic.full <- rbind(Titanic.train,Titanic.test)

#
Titanic.full[Titanic.full$Embarked=="","Embarked"] <- "U"

Age.median <- median(Titanic.full$Age,na.rm = T)
Titanic.full[is.na(Titanic.full$Age),"Age"] <- Age.median

Fare.median <- median(Titanic.full$Fare,na.rm = T)
Titanic.full[is.na(Titanic.full$Fare),"Fare"] <- Fare.median

#
Titanic.full$Pclass<- as.factor(Titanic.full$Pclass)
Titanic.full$Sex<- as.factor(Titanic.full$Sex)
Titanic.full$Embarked<- as.factor(Titanic.full$Embarked)

#
Titanic.train <- Titanic.full[Titanic.full$isTrain==T,]
Titanic.test <- Titanic.full[Titanic.full$isTrain==F,]

#
Titanic.train$Survived <- as.factor(Titanic.train$Survived)


#
install.packages("randomForest")
library(randomForest)
Survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived.formula<- as.formula(Survived.equation)

Titanic.model <- randomForest(Survived.formula,Titanic.train,ntree = 500, mtry = 5, nodesize = 0.01*nrow(Titanic.test) )

Survived <- predict(Titanic.model,Titanic.test)

PassengerId <- Titanic.test$PassengerId
df <- data.frame(PassengerId)
df$Survived <- Survived

write.csv(df,file = "Titanic_submission.csv",row.names = F)
