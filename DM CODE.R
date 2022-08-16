
rm(list=ls()) 

library(stringr)
library(e1071) 

t_traindata <- read.csv("train.csv") 
t_testdata <- read.csv("test.csv") 


############################


##### Train Data Cleaning #####

head(t_traindata)

## Checking for NULL data
table(is.na(t_traindata))

## Removing ticket variable as data seems of no use
t_traindata <- subset( t_traindata, select = -Ticket)

## Cabin
table(t_traindata$Cabin == "")

## Since cabin has majority of blank values, removing cabin
t_traindata <- subset( t_traindata, select = -Cabin) 

## Name
table(t_traindata$Name== "")

## Modifying Name variable
t_traindata$Name<-as.character(t_traindata$Name)
Name1<-sapply(strsplit(t_traindata$Name,','),`[[`,2)
t_traindata$Name<- sapply(strsplit(Name1, ". ", fixed=TRUE),`[[`,1)
head(t_traindata$Name)

## Age
table(is.na(t_traindata$Age))

## Filling the Null Age data
a1 <- subset(t_traindata, t_traindata$Name == " Master")
b1<-a1[!is.na(as.numeric(a1$Age)), ]
a1$Age[is.na(a1$Age)] <-median(b1$Age)    ## Since little boys have master in their name

a2 <- subset(t_traindata, t_traindata$Name == " Miss" & t_traindata$Parch >=1 )
b2 <- a2[!is.na(as.numeric(a2$Age)), ]
a2$Age[is.na(a2$Age)] <- median(b2$Age)   ## Little girls have miss in their name and have at least 1 parent

a3 <- subset(t_traindata, t_traindata$Name == " Miss" & t_traindata$Parch == 0 )
b3 <- a3[!is.na(as.numeric(a3$Age)), ]
a3$Age[is.na(a3$Age)] <- median(b3$Age   )## Single women with no parent/child

a4 <- subset(t_traindata, t_traindata$Name != " Master" & t_traindata$Name != " Miss" )
b4 <- a4[!is.na(as.numeric(a4$Age)), ]
a4$Age[is.na(a4$Age)] <- median(b4$Age)   ## Rest of the data


## Combining all the subsetted datasets
t_traindata1 <- rbind(a1, a2, a3, a4)
nrow(t_traindata1) ## This should be equal to 891 rows
table(is.na(t_traindata1))

## Fare
table(is.na(t_traindata1$Fare)) 

## Checking Skewness
skewness(t_traindata1$Fare)
t_traindata1$Fare <- log(t_traindata1$Fare + 1) ## Taking natural log to reduce skewness
skewness(t_traindata1$Fare) ## Closer to 0 is better

## Creating Alone_NotAlone, 0 > Alone, 1 > NotAlone
t_traindata1$Alone_NotAlone <- ifelse(t_traindata1$SibSp > 0 | t_traindata1$Parch > 0, 1, 0)

## Removing Name variable
t_traindata1 <- subset( t_traindata1, select = -Name)

## Embarked
table(t_traindata1$Embarked == "")
table(t_traindata1$Embarked)

## Since Embarked has majority of S, filling blank rows with S.
t_traindata1$Embarked[t_traindata1$Embarked == ""] <- "S"

## All NULL data has been filled

table(is.na(t_traindata1))
head(t_traindata1)


#####################################################################################################################


##### Test Data Cleaning #####


head(t_testdata)

## Checking for NULL data
table(is.na(t_testdata))


## Removing ticket variable as data seems of no use
t_testdata <- subset( t_testdata, select = -Ticket)


## Cabin
table(t_testdata$Cabin == "")

## Since cabin has majority of blank values, removing cabin
t_testdata <- subset( t_testdata, select = -Cabin) 


## Modifying the Name variable
t_testdata$Name<-as.character(t_testdata$Name)
table(is.na(t_testdata$Name))
Name2<-sapply(strsplit(t_testdata$Name,','),`[[`,2)
t_testdata$Name<- sapply(strsplit(Name2, ". ", fixed=TRUE),`[[`,1)
head(t_testdata$Name)


##Age
table(is.na(t_testdata$Age))

## Filling the Null Age data
a5 <- subset(t_testdata, t_testdata$Name == " Master")
b5<-a5[!is.na(as.numeric(a5$Age)), ]
a5$Age[is.na(a5$Age)] <- median(b5$Age)    ## Since little boys have master in their name

a6 <- subset(t_testdata, t_testdata$Name == " Miss" & t_testdata$Parch >=1 )
b6<-a6[!is.na(as.numeric(a6$Age)), ]
a6$Age[is.na(a6$Age)] <- median(b6$Age)    ## Little girls have miss in their name and have atleast 1 parent

a7 <- subset(t_testdata, t_testdata$Name == " Miss" & t_testdata$Parch == 0 )
b7 <- a7[!is.na(as.numeric(a7$Age)), ]
a7$Age[is.na(a7$Age)] <- median(b7$Age)    ## Single women with no parent/child

a8 <- subset(t_testdata, t_testdata$Name != " Master" & t_testdata$Name != " Miss" )
b8 <- a8[!is.na(as.numeric(a8$Age)), ]
a8$Age[is.na(a8$Age)] <- median(b8$Age)    ## Rest of the data

## Combining all the subsetted datasets
t_testdata1 <- rbind(a5, a6, a7, a8)
nrow(t_testdata1) ## This should be equal to 418 rows
table(is.na(t_testdata1))

## Fare
table(is.na(t_testdata1$Fare))
## 1 value from Fare is NULL

aa9<-t_testdata1[!is.na(as.numeric(t_testdata1$Fare)), ]
t_testdata1$Fare[is.na(t_testdata1$Fare)] <- median(aa9$Fare)
table(is.na(t_testdata1$Fare))

## Checking Skewness
skewness(t_testdata1$Fare)
t_testdata1$Fare <- log(t_testdata1$Fare + 1) ## Taking natural log to reduce skewness
skewness(t_testdata1$Fare) ## Closer to 0 is better


## Creating Alone_NotAlone, 0 > Alone, 1 > NotAlone
t_testdata1$Alone_NotAlone <- ifelse(t_testdata1$SibSp > 0 | t_testdata1$Parch > 0, 1, 0)


## Removing Name variable
t_testdata1 <- subset( t_testdata1, select = -Name)


## Embarked
table(t_testdata1$Embarked == "")


## All NULL data has been filled

table(is.na(t_testdata1))
head(t_testdata1)

############################


##### Predicting Survival Rate #####


## Running GLM function on the clean training dataset
set.seed(4444)
glm.obj <- glm(Survived ~.,  data = t_traindata1)
summary(glm.obj)


## Choosing only statistically significant variables
set.seed(4444)
glm1.obj <- glm(Survived ~ Sex + Fare + Parch + SibSp + Age + Pclass + Alone_NotAlone, data = t_traindata1)
summary(glm1.obj)


## Error rate function
error.rate <- function(mat) {(mat[2,1] + mat[1,2]) / sum(mat)}


## Optimal Threshold funciton
set.seed(4444)
optimal.value<- function(n=48){
  Error.mat <- matrix(NA,n,2)
  for(i in 1:n) {
    glm.obj <- glm(Survived ~ Sex + Fare + Parch + SibSp + Age + Pclass + Alone_NotAlone, data = t_traindata1)
    fittedvalues2   <- glm.obj$fitted.values > (i * 0.02)
    Classif.Table1 <- table(fittedvalues2, t_traindata1$Survived)
    error.rate <- (Classif.Table1[2,1] + Classif.Table1[1,2]) / sum(Classif.Table1)
    Error.mat[i,1] <- error.rate
    Error.mat[i,2] <- i*0.02
    
  }
  Error.mat
}

x <- optimal.value() ## Storing optimal.value function values into x

x.df<- as.data.frame(x)
min.error <- min(x.df$V1) # Minimum error value
optimal.threshold<-x.df[x.df$V1==min.error,] # Optimal error value based on the threshold


## Plotting the graph
par(mar = c(4.5,4.5,0,1)+2)
matplot(x[,2], x[,1], type = "l", main="Error Rate vs Probability", xlab='Threshold', 
        ylab="Error Rate")
abline(v = optimal.threshold[,2])
abline(h = min(x[,1]))
paste("The minimum error rate is", min.error, "with an optimal threshold value of", optimal.threshold[,2])


## Misclassification Table for optimal threshold
prediction <- glm1.obj$fitted.values > optimal.threshold[,2]
Classif.table2  <- table(prediction, t_traindata1$Survived)
Classif.table2 
error.rate(Classif.table2)


## Predicting using test data
testpredict <- predict(glm1.obj, newdata=t_testdata1, type = "response") ## Predicting using test data
pred.test <- ifelse(testpredict > optimal.threshold[,2],1,0)
testpredict<-data.frame(PassengerID=t_testdata1$PassengerId,Survived=pred.test)
write.csv(testpredict,file = "Titanic_Prediction_final.csv",row.names = FALSE)

