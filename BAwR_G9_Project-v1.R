setwd("E:/UTD/OneDrive - The University of Texas at Dallas/2023/Spring 2023/Business Analytics with R/Group Project/Group 9")
rm(list=ls())

#import libraries
install.packages("ggplot2")
install.packages("gridExtra") # for random forrest
install.packages("ggdensity")
install.packages("ggcorr")  #install.packages("GGally")
install.packages("dummies") #removed from R
install.packages("C50")     
install.packages("forecast")
install.packages("GGally")  #added new

library(ggplot)    #no package called GGPLOT
library(ggplot2)
library(ggdensity)
library(gridExtra)
library(GGally)
library(caret)
library(data.table)
library(ggpubr)
library(ROSE)
library(class)
library(tree)
library(dtree)
library(randomForest)
library(mltools)
library(rsample)
library(e1071)
library(pheatmap)
library(keras)
library(dummies)  #error
library(mlbench)
library(reticulate)
library(dplyr)
library(infotheo)
library(praznik)
library(ggpubr)
library(corrgram)
library(ggcorr)    #error
library(klaR)
library(caret)
library(tidyverse)
library(data.table)

install.packages("parsnip")
library(tidymodels)

library(partykit)
library(rpart)
library(rpart.plot)
library(e1071)
library(C50)
library(forecast)
require(randomForest)
library(RWeka)

#import csv file
df <- read.csv(file = "online_shoppers_intention.csv")
View(df)

#descriptive statistics
str(df)
head(df)
summary(df)

#removing duplicates
df_duplicate <- nrow(df[duplicated(df),])
df <- df[!duplicated(df),]
str(df)

#identification of missing values
which(is.na(df))

#Renaming June to Jun for convenience of plotting
df$Month <- as.character(df$Month)
df$Month[df$Month == "June"] <- "Jun"
df$Month <- as.factor(df$Month)
df$Month = factor(df$Month, levels = month.abb)

#*******************************************************************************
#EXPLORATORY DATA ANALYSIS

install.packages("ggplot2")
library(ggplot2)
#Administrative pages: number of pages visited
plot1 <- ggplot(df, aes(x=1, y=Administrative)) + geom_violin() + geom_violin(trim=FALSE, fill='#E69F00', color='gray') + coord_flip() + labs(x = " ") + labs(y = "Number of Administrative pages visited") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
#Administrative pages: total time spent
plot2 <- ggplot(df, aes(x=1, y=Administrative_Duration)) + geom_violin() + geom_violin(trim=FALSE, fill='#E69F00', color='gray') + coord_flip() + labs(x = " ") + labs(y = "Total time spent in Administrative pages") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())

#Informational pages: number of pages visited
plot3 <- ggplot(df, aes(x=1, y=Informational)) + geom_violin() + geom_violin(trim=FALSE, fill='#56B4E9', color='gray') + coord_flip() + labs(x = " ") + labs(y = "Number of Informational pages visited") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
#Informational pages: total time spent
plot4 <- ggplot(df, aes(x=1, y=Informational_Duration)) + geom_violin() +  geom_violin(trim=FALSE, fill='#56B4E9', color='gray') + coord_flip() + labs(x = " ") + labs(y = "Total time spent in Informational pages") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())

#Product related pages: number of pages visited
plot5 <- ggplot(df, aes(x=1, y=ProductRelated)) + geom_violin() + geom_violin(trim=FALSE, fill='#FF9999', color='gray') + coord_flip() + labs(x = " ") + labs(y = "Number of ProductRelated pages visited") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
#Product related: total time spent
plot6 <- ggplot(df, aes(x=1, y=ProductRelated_Duration)) + geom_violin() + geom_violin(trim=FALSE, fill='#FF9999', color='gray') + coord_flip() + labs(x = " ") + labs(y = "Total time spent in ProductRelated pages") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())

install.packages("gridExtra")
library(gridExtra)
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow = 3, ncol = 2)

#Side-by-side comparison charts
plot1 <- ggplot(df, aes(x=Revenue, y=Administrative)) + geom_violin() + geom_violin(trim=FALSE, fill='#E69F00', color='gray') + labs(x = "Administrative") + labs(y = " ") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
plot4 <- ggplot(df, aes(x=Revenue, y=Administrative_Duration)) + geom_violin() + geom_violin(trim=FALSE, fill='#E69F00', color='gray') + labs(x = "Administrative_Duration") + labs(y = " ") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
plot2 <- ggplot(df, aes(x=Revenue, y=Informational)) + geom_violin() + geom_violin(trim=FALSE, fill='#56B4E9', color='gray') + labs(x = "Informational") + labs(y = " ") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
plot5 <- ggplot(df, aes(x=Revenue, y=Informational_Duration)) + geom_violin() +  geom_violin(trim=FALSE, fill='#56B4E9', color='gray') + labs(x = "Informational_Duration") + labs(y = " ") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
plot3 <- ggplot(df, aes(x=Revenue, y=ProductRelated)) + geom_violin() + geom_violin(trim=FALSE, fill='#FF9999', color='gray') + labs(x = "ProductRelated") + labs(y = " ") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
plot6 <- ggplot(df, aes(x=Revenue, y=ProductRelated_Duration)) + geom_violin() + geom_violin(trim=FALSE, fill='#FF9999', color='gray') + labs(x = "ProductRelated_Duration") + labs(y = " ") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow = 2, ncol = 3)

install.packages("ggpubr")
library(ggpubr)

#BounceRates, ExitRates and PageValues
plot1 <- ggdensity(df, x = "BounceRates", fill = "thistle2", color = "thistle2", add = "median", rug = TRUE) + labs(y = " ")
plot2 <- ggdensity(df, x = "ExitRates", fill = "skyblue1", color = "skyblue1", add = "median", rug = TRUE) + labs(y = " ")
plot3 <- ggdensity(df, x = "PageValues", fill = "sienna3", color = "sienna3", add = "median", rug = TRUE) + labs(y = " ")
grid.arrange(plot1, plot2, plot3, nrow = 3)


plot1 <- ggplot(df, aes(x=BounceRates, fill=Revenue)) + geom_density(alpha=0.4) + labs(y = " ")
plot2 <- ggplot(df, aes(x=ExitRates, fill=Revenue)) + geom_density(alpha=0.4) + labs(y = " ")
plot3 <- ggplot(df, aes(x=PageValues, fill=Revenue)) + geom_density(alpha=0.4) + labs(y = " ")
grid.arrange(plot1, plot2, plot3, nrow = 3)

#Special and non-special days
plot1 <- ggplot(df, aes(x = factor(1), y = SpecialDay)) + geom_boxplot(width = 0.4, fill = "white") + geom_jitter(color = "deepskyblue4", width = 0.1, size = 1, alpha=0.4) + labs(x = "Special Day") + labs(y = "Closeness") + theme(axis.text.x = element_blank(), axis.ticks = element_blank())
plot2 <- ggplot(df, aes(x = Revenue, y = SpecialDay)) + geom_boxplot(width = 0.4, fill = "white") + geom_jitter(color = "deepskyblue4", width = 0.2, size = 1, alpha=0.4) + labs(x = "Special Day") + labs(y = " ") + theme(axis.ticks = element_blank())
grid.arrange(plot1, plot2, ncol = 2)

#Month-wise distribution
plot <- ggplot(data.frame(df), aes(Month, fill=Revenue)) + geom_bar() + labs(x = "Month") + labs(y = " ")
plot

#Categorization based upon OS, browser, region, traffic type, weekend and visitor type
plot1 <- ggplot(data.frame(df), aes(OperatingSystems, fill=Revenue)) + geom_bar() + labs(x = "Operating Systems") + labs(y = " ") + scale_x_continuous(breaks = 1:8)
plot2 <- ggplot(data.frame(df), aes(Browser, fill=Revenue)) + geom_bar() + labs(x = "Browser") + labs(y = " ") + scale_x_continuous(breaks = 1:13)
plot3 <- ggplot(data.frame(df), aes(Region, fill=Revenue)) + geom_bar() + labs(x = "Region") + labs(y = " ") + scale_x_continuous(breaks = 1:9)
plot4 <- ggplot(data.frame(df), aes(TrafficType, fill=Revenue)) + geom_bar() + labs(x = "Traffic Type") + labs(y = " ")
plot5 <- ggplot(data.frame(df), aes(Weekend, fill=Revenue)) + geom_bar() + labs(x = "Weekend") + labs(y = " ")
plot6 <- ggplot(data.frame(df), aes(VisitorType, fill=Revenue)) + geom_bar() + labs(x = "Visitor Type") + labs(y = " ") + scale_x_discrete(labels = c("New_Visitor" = "New", "Other" = "Other", "Returning_Visitor" = "Return"))
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow = 3, ncol = 2)

#Target feature distribution
plot <- ggplot(data.frame(df$Revenue), aes(x=df$Revenue)) + geom_bar() + labs(x = "Target Feature Distribution")
plot

install.packages("ggcorrplot")
library(ggcorrplot)

#Correlation between different sets of variables  -------not working
corr_map <- ggcorrplot(df[, 1:10], method=c("everything", "pearson"), label=TRUE, hjust = .90, size = 3, layout.exp = 2)
corr_map 

#*******************************************************************************

#DATA PRE-PROCESSING

#Transforming categorical attributes into factor types
library(magrittr)
library(dplyr)
df <- df %>% 
  mutate(OperatingSystems = as.factor(OperatingSystems),
         Browser = as.factor(Browser),
         Region = as.factor(Region),
         TrafficType = as.factor(TrafficType),
         VisitorType = as.factor(VisitorType),
         Weekend = as.integer(Weekend),
         Revenue = as.integer(Revenue)
  )

#One-hot encoding to save original copy
df_new <- df
df$Revenue <- as.factor(df$Revenue)
print("Original dataset")

print(str(df_new))

#Peforming one hot encoding on all columns except Revenue
revenueData <- df[,18]; revenueData

install.packages("recipes")
library(recipes)
encoded_df <- one_hot(as.data.table(df[,-18]))
df <- cbind(encoded_df, df[,18]); 
colnames(df)[colnames(df)=="V2"] = "Revenue"
#df$Revenue <- as.factor(df$Revenue)

print("After one-hot encoding")

print(str(df))

#Split training and testing data.

split_df_new <- initial_split(df_new, prop = .7, strata = "Revenue")
train_df_new <- training(split_df_new)
test_df_new <- testing(split_df_new)

print("Original dataset")

table(train_df_new$Revenue) %>% prop.table()
table(test_df_new$Revenue) %>% prop.table()

split <- initial_split(df, prop = .7, strata = "Revenue")
train_data <- training(split)
test_data <- testing(split)

print("After one-hot encoding")

table(train_data$Revenue) %>% prop.table()
table(test_data$Revenue) %>% prop.table()


#Preprocess the continuous attributes by splitting from categorical ones and binding
train_numerical <- train_data[,1:10] 
train_categorical <- train_data[,11:77]
test_numerical <- test_data[,1:10] 
test_categorical = test_data[,11:77]

#Utilization of scaling function
train_scaled = scale(train_numerical)
test_scaled = scale(test_numerical, center=attr(train_scaled, "scaled:center"), scale=attr(train_scaled, "scaled:scale"))

#Column binding
train_data <- cbind(train_scaled, train_categorical)
test_data <- cbind(test_scaled, test_categorical)

#Oversampling to overcome imbalance in dataset
N_df_new = 2*length(which(train_df_new$Revenue == 0))
df_new_over <- ovun.sample(Revenue~.,data = train_df_new, method= 'over', N = N_df_new, seed = 2020)$data

N = 2*length(which(train_data$Revenue == 0))
df_over <- ovun.sample(Revenue~.,data = train_data, method= 'over', N = N, seed = 2020)$data

#Splitting features and target
features_df_new <- setdiff(names(train_df_new), "Revenue")
features <- setdiff(names(train_data), "Revenue")

#*******************************************************************************

#DATA MODELING

#PREDICTION USING DIFFERENT ALGORITHMS
#(A) NAIVE BAYES CLASSFIER

x_df_new <- train_df_new[, features_df_new]
y_df_new <- train_df_new$Revenue
x <- train_data[, ..features]
y <- train_data$Revenue

train_control <- trainControl(
  method = "cv",
  number = 10
)

nb.ml_df_new <- caret::train(
  x = x_df_new,
  y = as.factor(y_df_new),
  method = "nb",
  trControl = train_control
)

nb.ml <- caret::train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control
)

print("Without one-hot encoding")
print(confusionMatrix(nb.ml_df_new))
#Accuracy
a1 <- 0.8404

print("With one-hot encoding")
print(confusionMatrix(nb.ml))
#Accuracy
a2 <- 0.8436 

#*******************************************************************************

#(B) K-NEAREST NEIGHBOUR

#Train the model and predict
knn_model <- knn(df_over[, 1:76], test_data[, 1:76], df_over$Revenue)

#Confusion Matrix and Metrics
print("Default k-NN")

CM_knn_default <- confusionMatrix(knn_model, factor(test_data$Revenue))
print(CM_knn_default)

#Visualize accuracies of different k
knn_model <- NULL
errors <- NULL

for (i in 1:30) {
  knn_model <- knn(df_over[, 1:76], test_data[, 1:76], df_over$Revenue, k = i)
  errors[i] <- mean(knn_model != test_data$Revenue)
}

knn.error <- as.data.frame(cbind(k=1:30, errors))

ggplot(knn.error, aes(k, errors)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 1:30) +
  theme_bw() +
  xlab("Value of K") +
  ylab("Error")

#Result of the best-performance model
k_nn <- knn(df_over[, 1:76], test_data[, 1:76], df_over$Revenue, k=1)
print("Best-performance k-NN")

CM_knn_best <- confusionMatrix(k_nn, factor(test_data$Revenue))
print(CM_knn_best)
#Accuracy
a3 <- 0.8392

#*******************************************************************************

#(C)TREE BASED METHODS

#Train the model

set.seed(100)

df1=df   

df1$Revenue<- as.factor(df1$Revenue)
df1$Weekend<- as.factor(df1$Weekend)

index <- createDataPartition(df1$Revenue, p=0.75, list=FALSE)
train <-df1[ index,]
test <- df1[-index,]

#Decision Tree Model
decision_tree <- rpart(Revenue ~ . , method='class', data= train)
prp(decision_tree)
rpart.plot(decision_tree)

### compute the predictive accuracy in the test set

pred_new <- predict(decision_tree, test, type = "class")
mean(pred_new == test$Revenue)

#Accuracy
a4 <- 0.8970

# C4.5 Decision trees

fit<-J48(Revenue ~.,data=train)
summary(fit)

p_tree<-predict(fit,test[,1:17])
confusionMatrix(p_tree,test$Revenue)

#Accuracy
a5 <- 0.8925

# C5.0 Boosted trees

dtree<-C5.0(train,train$Revenue)
plot(dtree)

p_dtree<-predict(dtree,test)
confusionMatrix(table(p_dtree,test$Revenue))

#Accuracy
a6 <- 1

#Random Forest
#Train the model
n<-length(names(df_new_over)) 
m = ceiling(log2(n))
rf.fit <- randomForest(factor(Revenue)~., data = train, mtry = m)

rf.pred <- predict(rf.fit, test)
confusionMatrix(table(rf.pred,test$Revenue))

head(rf.pred)

#Compute the prediction accuracy in the testing set
mean(rf.pred == test$Revenue)

#Accuracy
a7 <- 0.8918 
  
#Compute the prediction accuracy in the training set

rf.pred2 <- predict(rf.fit, train)

mean(rf.pred2 == train$Revenue)

#*******************************************************************************

#(D) SVM Algorithm

#Linear SVM

#Train the model
svm_fit = svm(as.factor(Revenue)~., data=df_over, kernel = "linear", scale = FALSE)

#Predict
pred <- predict(svm_fit, newdata = test_data)

#Confusion Matrix and Metrics of Linear SVM

print("Linear SVM")

linear_SVM <- confusionMatrix(pred, factor(test_data$Revenue))
print(linear_SVM)

#Accuracy
a8 <- 0.8761

#Radial SVM

#Train the model
svm_fit2 = svm(as.factor(Revenue)~., data=df_over, kernel = "radial", scale = FALSE)

#Predict
pred_radial <- predict(svm_fit2, newdata = test_data)

#Confusion Matrix and Metrics of RBF SVM
print("Radial SVM")

radial_SVM <- confusionMatrix(pred_radial, factor(test_data$Revenue))
print(radial_SVM)

#Accuracy
a9 <- 0.8736

#*******************************************************************************
#Target Feature Selection

varImpPlot(rf.fit, sort = TRUE, n.var = 25, main = 'Features Importance by Random Forest')

#*******************************************************************************

#Comparison of accuracies between different models

X<-c("NB-Without One Hot Encoding","NB-With One Hot Encoding","KNN","Classification","C4.5-Tree","C5.0-Tree","Random Forest","Linear SVM","Radial SVM")
Y<-round(c(a1,a2,a3,a4,a5,a6,a7,a8,a9),2)

X_name <- "model"
Y_name <- "accuracy"

df <- data.frame(X,Y)
names(df) <- c(X_name,Y_name)

ggplot(df,aes(x=model,y=accuracy,fill=model))+geom_bar(stat = "identity") + geom_text(aes(label=accuracy),position=position_dodge(width=0.9), vjust=-0.25)

#*******************************************************************************
#OPTIMIZATION

#Binning the continuous variables for use of Mutual Information
continous_cols <- df_new_over %>%
  dplyr::select(Administrative_Duration, Informational_Duration, ProductRelated_Duration, BounceRates, ExitRates, PageValues, SpecialDay)

#Scaling the continuous columns
standardized_cols = as.data.frame(scale(continous_cols))

col_names = names(standardized_cols)
binned_cols <- standardized_cols %>% mutate(
  Administrative_Duration=cut(Administrative_Duration, breaks = c(-Inf,-3.5,-2.5,-1.5,-0.5,0.5,1.5,2.5,3.5,Inf), labels = c(-4,-3,-2,-1,0,1,2,3,4)),
  Informational_Duration=cut(Informational_Duration, breaks = c(-Inf,-3.5,-2.5,-1.5,-0.5,0.5,1.5,2.5,3.5,Inf), labels = c(-4,-3,-2,-1,0,1,2,3,4)),
  ProductRelated_Duration=cut(ProductRelated_Duration,breaks = c(-Inf,-3.5,-2.5,-1.5,-0.5,0.5,1.5,2.5,3.5,Inf), labels = c(-4,-3,-2,-1,0,1,2,3,4)),
  BounceRates=cut(BounceRates,breaks = c(-Inf,-3.5,-2.5,-1.5,-0.5,0.5,1.5,2.5,3.5,Inf), labels = c(-4,-3,-2,-1,0,1,2,3,4)),
  ExitRates=cut(ExitRates,breaks = c(-Inf,-3.5,-2.5,-1.5,-0.5,0.5,1.5,2.5,3.5,Inf), labels = c(-4,-3,-2,-1,0,1,2,3,4)),
  PageValues=cut(PageValues,breaks = c(-Inf,-3.5,-2.5,-1.5,-0.5,0.5,1.5,2.5,3.5,Inf), labels = c(-4,-3,-2,-1,0,1,2,3,4)),
  SpecialDay=cut(SpecialDay,breaks = c(-Inf,-3.5,-2.5,-1.5,-0.5,0.5,1.5,2.5,3.5,Inf), labels = c(-4,-3,-2,-1,0,1,2,3,4))
)

binned <- df_new_over %>% mutate(
  Administrative_Duration = binned_cols$Administrative_Duration,
  Informational_Duration = binned_cols$Informational_Duration,
  ProductRelated_Duration = binned_cols$ProductRelated_Duration,
  BounceRates = binned_cols$BounceRates,
  ExitRates = binned_cols$ExitRates,
  PageValues = binned_cols$PageValues,
  SpecialDay = binned_cols$SpecialDay
)

#Mutual Information measures
#MI filter
MI = vector()
for (i in 1:17){
  MI <- c(MI, mutinformation(binned$Revenue, binned[,i]))
}
MI = data.frame(names(binned)[1:17],MI)
MI <- MI[with(MI, order(-MI)), ]

MI

#Minimum redundancy feature selection
#mMRM filter
score = MRMR(binned[1:17],binned$Revenue,17)$score
score = as.data.frame(score)
score

#RF- after feature selection
features_selected = c("Administrative_Duration", "Informational_Duration", "ProductRelated_Duration", "ExitRates", "PageValues", "Month", "OperatingSystems", "Browser", "Region", "TrafficType", "VisitorType", "Weekend")
df_new_over <- df_new_over[, c(features_selected, "Revenue")]
test_df_new <- test_df_new[, c(features_selected, "Revenue")]

n<-length(names(df_new_over)) 
m = ceiling(log2(n))
rf_train<-randomForest(as.factor(df_new_over$Revenue)~.,data=df_new_over,mtry=m ,ntree=100,importance=TRUE,proximity=TRUE)

#Predict
pred_2<-predict(rf_train,newdata=test_df_new)

#Confusion Matrix and Metrics
print("After feature selection")

#After feature selection
Optimized_RF <- confusionMatrix(pred_2, factor(test_df_new$Revenue))
print(Optimized_RF)





