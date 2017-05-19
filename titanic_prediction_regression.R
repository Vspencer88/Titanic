library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(randomForest)

# Read datasets
train <- read.csv('~/Progamming/titanic train.csv', stringsAsFactors = F)
test  <- read.csv('~/Progamming/titanic test.csv', stringsAsFactors = F)


## Test for columns with NA's
colnames(train)[apply(train,2,anyNA)]
colnames(test)[apply(test,2,anyNA)]

### Code training df's NA age values with average age of sample
avg_train_age<-mean(train$Age,na.rm = T)
train$Age<-ifelse(is.na(train$Age),avg_train_age,train$Age)
avg_test_age<-mean(test$Age,na.rm = T)
test$Age<-ifelse(is.na(test$Age),avg_test_age,test$Age)

## Impute the gender of NA values in test set through names
train$Title <- gsub('(.*, )|(\\..*)', '', train$Name)
table(train$Sex,train$Title)
test$Title <- gsub('(.*, )|(\\..*)', '', test$Name)
table(test$Sex, test$Title)

### Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

### Reassign mlle, ms, and mme accordingly
train$Title[train$Title == 'Mlle']        <- 'Miss' 
train$Title[train$Title == 'Ms']          <- 'Miss'
train$Title[train$Title == 'Mme']         <- 'Mrs' 
train$Title[train$Title %in% rare_title]  <- 'Rare Title'
test$Title[test$Title == 'Mlle']        <- 'Miss' 
test$Title[test$Title == 'Ms']          <- 'Miss'
test$Title[test$Title == 'Mme']         <- 'Mrs' 
test$Title[test$Title %in% rare_title]  <- 'Rare Title'

### Show title counts by sex again
table(test$Sex, test$Title)


## Remove NA's from test data's fare column   
avg_fare<-mean(test$Fare,na.rm=T)
test$Fare<-ifelse(is.na(test$Fare),avg_fare,test$Fare)


### Calculate number of total family members
train$Family<-train$SibSp+train$Parch
test$Family<-test$SibSp+test$Parch


### Run regression model
reg <- lm(Survived ~ Pclass + Sex + Age + Family + Fare + Title, data = train)
reg_pred <- predict(reg,test)

## Create data frame of predictions and export to CSV
solution <- data.frame(PassengerId = test$PassengerId,Survived = forest_pred)
write.csv(solution,file = "C:/Users/vspen/Documents/Programming/R/regression titanic solution.csv",row.names = FALSE)