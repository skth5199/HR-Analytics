library(dplyr)
library(ggplot2)
library(caret)
library(purrr)
library(devtools)
library(eply)

#train <- read.csv("train.csv")
train= read.csv("C:/Users/SRIKANTH/Desktop/hranalytics/train.csv")
head(train) # view first 5 rows
# Dimensions of the Dataset
dim(train)
# What are the types of our features ?
str(train)
colSums(is.na(train))
# Plotting the categorical variables
cat_features <- c("education", "department", "region", "gender", "recruitment_channel")
for (f in cat_features){
  counts <- table(train[f])
  barplot(counts, las=2)
}
### Fixing the missing values

# Education
train$education[train$education==""] <- "Bachelor's"
# Prev year rating
train$previous_year_rating[is.na(train$previous_year_rating)] <- mean(train$previous_year_rating, na.rm = TRUE)
### Dividing the features into the various types of features
x=map(train,class)

categ=c()
cont=c()

for (i in colnames(train)){
  if(x[[i]]=="factor")
    categ[length(categ)+1]=i
  else
    cont[length(cont)+1]=i
}
cat("Categorical Features: \n",categ,fill=TRUE)
cat("Non-categorical Features: \n", cont,fill=TRUE)
### Count Plots for Categorical Features
cat_plotter<-function(x){
  val=table(train[[x]])
  values=c()
  for(i in val)
    values[length(values)+1]=i
  
  df=data.frame(names(val),values)
  
  ggplot(df,aes(x = names(val), y = values, fill = names(val))) +
    geom_bar(stat = "identity") +
    theme_classic() +
    labs(
      x = x,
      y = "Count",
      title = paste(x,  "wise count"
      )
    ) + 
    theme(axis.text.x = element_blank())
}

cat_plotter("department")
cat_plotter("region")
cat_plotter("education")
cat_plotter("gender")
cat_plotter("recruitment_channel")

### Density Plots for Continuous Features

#cont_plotter<-function(x){
#  d <- density(train[[x]]) # returns the density data
#  plot(d,col="dodgerblue",xlab=x,main="",lwd=2,yaxs="i")
#  legend("topright",legend=x, col="dodgerblue", lty=1:2, cex=0.8,inset = .02,lwd=2)
#}

#cont_plotter( "no_of_trainings")
#cont_plotter("age")
#cont_plotter("previous_year_rating")
#cont_plotter("length_of_service")
#cont_plotter("KPIs_met..80.")
#cont_plotter("awards_won.")
#cont_plotter("avg_training_score")
#cont_plotter("is_promoted")
### Ordinal Encode "education" feature
o <- c("Below Secondary", "Master's & above", "Bachelor's")
n <- factor(train$education, levels=o)
train$education <- as.numeric(n)

### Label Encode "gender" feature
train$gender <- as.numeric(factor(train$gender))

### Ordinal Encode "recruitment_channel" feature
o <- c("other", "sourcing", "referred")
n <- factor(train$recruitment_channel, levels=o)
train$recruitment_channel <- as.numeric(n)

### One hot encode "department" and "region" features
dmy <- dummyVars(" ~ .", data = train)
df <- data.frame(predict(dmy, newdata = train))

### Final DF
head(df)

### Creating a Validation set

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(df$is_promoted, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- df[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- df[validation_index,]

### Binning Age

### Binning age
breaks <- c(20,30,40,50,60)
tags <- c(1,2,3,4)
# train
group_bins <- cut(dataset$age, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)
dataset$age <- as.numeric(group_bins)
# test
group_bins <- cut(validation$age, 
                  breaks=breaks, VA
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)
validation$age <- as.numeric(group_bins)

### Dimensionality Reduction

library(caret)
library(mlbench)

### Remove emp id
dataset <- dataset[ , !(names(dataset) %in% c("employee_id"))]
validation <- validation[ , !(names(validation) %in% c("employee_id"))]

### Dealing with Multi-collinearity
correlation <- cor(dataset)
## None of the features have a correlation of greater than 75%

### Ranking Features by importance
# fit_glm = glm(is_promoted~.,dataset,family = "binomial")
# varImp(fit_glm)

### Evaluation


dataset$is_promoted[dataset$is_promoted==0] <- "No"
dataset$is_promoted[dataset$is_promoted==1] <- "Yes"

validation$is_promoted[validation$is_promoted==0] <- "No"
validation$is_promoted[validation$is_promoted==1] <- "Yes"

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# KNN
#set.seed(10)
#fit.knn <- train(is_promoted~., data=dataset, method="knn", metric=metric, trControl=control)

# SVM
#set.seed(10)
#fit.svm <- train(is_promoted~., data=dataset, method="svmRadial", metric=metric, trControl=control)

# Random Forest
#set.seed(10)
#fit.rf <- train(is_promoted~., data=dataset, method="rf", metric=metric, trControl=control)

# Summarize accuracy of models
# results <- resamples(list(knn=fit.knn, svm=fit.svm, rf=fit.rf))
# summary(results)

#print(fit.knn)
#print(fit.svm)
#knnPredict <- predict(fit.knn,newdata = validation$is_promoted )
#x=confusionMatrix(knnPredict,as.factor(validation$is_promoted),mode = "sens_spec")
#for (i in x)
#  print(i)

count=0
#for(i in 1:length(knnPredict))
#{
#  if(knnPredict[i]==validation$is_promoted[i]) 
#  {  count=count+1
#      }
#}
#score=count/length(knnPredict)
#print(score)
#F1_Score(validation$is_promoted, knnPredict, positive = NULL)

library(xgboost)
y=dataset$is_promoted
y[dataset$is_promoted=="No"]=0
y[dataset$is_promoted=="Yes"]=1

y=as.numeric(y)
train_matrix <- xgb.DMatrix(data = as.matrix(dataset[,-54]), label = y)
y=validation$is_promoted
y[validation$is_promoted=="No"]=0
y[validation$is_promoted=="Yes"]=1

y=as.numeric(y)
test_matrix <- xgb.DMatrix(data = as.matrix(validation[,-54]), label = y)
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "merror",
                   "num_class" = 2)
cv.nfold=10
xgb <- xgb.cv(train_matrix, 
              
               params = xgb_params,
    
               nfold=cv.nfold,
               
               data = train_matrix, 
               nrounds = 50,
               
               verbose = FALSE,
               prediction = TRUE
)

bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = 50)
test_pred <- predict(bst_model, newdata = test_matrix)
test_prediction <- matrix(test_pred, nrow = 2,
                          ncol=length(test_pred)/2) %>%
                          t() %>%
                         data.frame() %>%
                           mutate(label = y + 1,
                           max_prob = max.col(., "last"))
## Accuracy for 20 split in original training dataset
confusionMatrix(factor(test_prediction$max_prob),
                factor(test_prediction$label),
                positive = '2',
                mode = "everything")
#-------------------------------------------------------------------
breaks <- c(20,30,40,50,60)
tags <- c(1,2,3,4)
group_bins <- cut(df$age, 
                              breaks=breaks, 
                                     include.lowest=TRUE, 
                                     right=FALSE, 
                                     labels=tags)
df$age= as.numeric(group_bins)
df <- df[ , !(names(df) %in% c("employee_id"))]
correlation <- cor(df)


library(xgboost)
df$is_promoted[df$is_promoted=="No"]=0
df$is_promoted[df$is_promoted=="Yes"]=1
label_train=df$is_promoted
label_train=as.numeric(label_train)
train_matrix2 <- xgb.DMatrix(data = as.matrix(df[,-54]), label = label_train)
            #-----------------
test= read.csv("C:/Users/SRIKANTH/Desktop/hranalytics/test.csv")
colSums(is.na(train))
cat_features <- c("education", "department", "region", "gender", "recruitment_channel")
for (f in cat_features){
       counts <- table(train[f])
     barplot(counts, las=2)
 }
test$education[test$education==""] <- "Bachelor's"
test$previous_year_rating[is.na(test$previous_year_rating)] <- mean(test$previous_year_rating, na.rm = TRUE)
x=map(test,class)

### Ordinal Encode "education" feature
o <- c("Below Secondary", "Master's & above", "Bachelor's")
n <- factor(test$education, levels=o)
test$education <- as.numeric(n)
 ### Label Encode "gender" feature
   test$gender <- as.numeric(factor(test$gender))

### Ordinal Encode "recruitment_channel" feature
o <- c("other", "sourcing", "referred")
n <- factor(test$recruitment_channel, levels=o)
test$recruitment_channel <- as.numeric(n)

### One hot encode "department" and "region" features
dmy2 <- dummyVars(" ~ .", data = test)
df2 <- data.frame(predict(dmy2, newdata = test))

group_bins <- cut(df2$age, 
                                     breaks=breaks, 
                                     include.lowest=TRUE, 
                                     right=FALSE, 
                                     labels=tags)
df2$age= as.numeric(group_bins)
df2 <- df2[ , !(names(df2) %in% c("employee_id"))]
correlation <- cor(df2)

df2$age=as.numeric(df2$age)


xgb_params <- list("objective" = "multi:softmax",
                   "eval_metric" = "mlogloss",
                   
                   "num_class" = 2)
cv.nfold=10

dtrain <- xgb.DMatrix(as.matrix(df[,-54]), label=as.numeric(df$is_promoted))
xgb.DMatrix.save(dtrain, 'xgb.DMatrix.data')
df$is_promoted=as.numeric(df$is_promoted)
xgboost.model <- xgb.cv( 
              
              params = xgb_params,
              
              nfold=cv.nfold,
              
              data = dtrain, 
              nrounds = 50,
              
              verbose = FALSE,
              prediction = TRUE
)

bst_model <- xgb.train(params = xgb_params,
                       data =  dtrain,
                       label=as.numeric(df$is_promoted),
                       nrounds = 50)


testcase_pred2=predict(bst_model,as.matrix(df2))
#print(head(testcase_pred2))
length(testcase_pred2)
#print(summary(testcase_pred2==testcase_pred))
#write.csv(testcase_pred2, file = "xyz.csv",row.names=FALSE)