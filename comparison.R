library(data.table)
library(e1071)
library(cvTools)
library(caret)
polls1 <- fread('presidential_polls.csv', stringsAsFactors = TRUE, select = c("type", "state", "enddate", "pollster", "grade", "samplesize", "population", "poll_wt", "rawpoll_clinton", "rawpoll_trump", "adjpoll_clinton", "adjpoll_trump", "multiversions", "poll_id"), showProgress = TRUE)
polls1$grade <- sub("^$","F",polls1$grade)
polls1$grade <- factor(polls1$grade, ordered = TRUE, levels = c("F","D","C-","C","C+","B-","B","B+","A-","A","A+"))
polls1$enddate <- as.Date(polls1$enddate, format = "%m/%d/%Y")
polls1 <- na.omit(polls1)
for (i in 1:nrow(polls1)) {
  if(polls1$adjpoll_clinton[i]<polls1$adjpoll_trump[i]) {
    polls1[i,'labels'] <- as.factor('trump')
  }
  else {
    polls1[i,'labels'] <- as.factor('clinton')
  }
}

folds <- cvFolds(nrow(polls1), K=10, type = "random")
index <- 1:round(nrow(folds$subsets)*0.9)


control<-trainControl(method = "repeatedcv",number = 10,repeats = 3)
# CART
set.seed(7)
fit.cart <- train(labels~., data=polls1[-index,-1], method="rpart", trControl=control)

# SVM
set.seed(7)
fit.svm <- train(labels~adjpoll_clinton - adjpoll_trump, data=polls1[-index,-1], method="svmRadial", trControl=control)
# naive_bayes
set.seed(7)
fit.nb <- train(labels~adjpoll_clinton - adjpoll_trump, data=polls1[-index,-1], method="nb", trControl=control)

#confusion matrix of svm and naive bayes
predicted_svm <- predict(fit.svm, polls1[index,-1])
cm_svm <- confusionMatrix(predicted_svm, polls1$labels[index])
cm_svm

predicted_nb <- predict(fit.nb, polls1[index,-1])
cm_nb<- confusionMatrix(predicted_nb,polls1$labels[index])
cm_nb

#results
results <- resamples(list(svm_model=fit.svm, naive_bayes=fit.nb))

#summary
summary(results)

#boxplot
bwplot(results)

# density plots of accuracy
scales <- list(x=list(relation="free"), y=list(relation="free"))
densityplot(results, scales=scales, pch = "|")


#scatter plots
splom(results)
