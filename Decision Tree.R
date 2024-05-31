
#(C)TREE BASED METHODS

#Train the model
df1=df_original   

df1$Revenue<- as.factor(df1$Revenue)
df1$Weekend<- as.factor(df1$Weekend)
df1$SpecialDay <- as.factor(df1$SpecialDay)
df1$Month <- as.factor(df1$Month)
df1$OperatingSystems <- as.factor(df1$OperatingSystems)
df1$Browser <- as.factor(df1$Browser)
df1$Region <- as.factor(df1$Region)
df1$TrafficType <- as.factor(df1$TrafficType)
df1$VisitorType <- as.factor(df1$VisitorType)


set.seed(435)
train <- sample(1:nrow(df1),nrow(df1)*(7/10))

df1.train <- df1[train,]
df1.test  <- df1[-train,]

#install.packages("rpart")
library(rpart)
fit <- rpart(Revenue~.,
             data=df1.train,
             method="class",
             control=rpart.control(xval = 10,minsplit=50),
             parms=list(split="gini"))
fit

library(rpart.plot)
rpart.plot(fit,
           type=1,
           extra = 1,
           main="Group 9 - Decision Tree")


#Decision Tree Confusion Matrix
df1.predict <- predict(fit, df1.test, type="class")
df1.actual  <- df1.test$Revenue

confusion.matrix <- table(df1.predict, df1.actual)
confusion.matrix

proptable <- prop.table(confusion.matrix)
proptable

decisiontree_accuracy
decisiontree_accuracy = proptable[1,1] + proptable[2,2]; decisiontree_accuracy
