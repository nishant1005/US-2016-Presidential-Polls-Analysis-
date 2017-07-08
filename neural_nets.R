library(neuralnet)
library(data.table)

#creat vector of column max and min values
maxs <- apply(polls[,1:2], 2, max)
mins <- apply(polls[,1:2], 2, min)

# Use scale() and convert the resulting matrix to a data frame
scaled.data <- as.data.frame(scale(polls[,1:2],center = mins, scale = maxs - mins))

# Check out results
print(head(scaled.data,2))

# Convert Private column from Yes/No to 1/0
winner = as.numeric(polls$winning)-1
data_neos = cbind(winner,scaled.data)

data_reduced<-head(data_neos,2000)
data_reduced1

library(caTools)
set.seed(101)


# Create Split (any column is fine)
split2<-sample.split(data_reduced1$winner,SplitRatio = 0.70)

# Split based off of split Boolean Vector
train_set1<-subset(data_reduced1,split=TRUE)
test_set1<-subset(data_reduced1,split=FALSE)
train_set1

feats<-names(scaled.data)

# Concatenate strings and Convert to formula
func_neurons<-paste(feats1,collapse = '-')
func_neurons<-paste('winner ~',func_neurons)
func_neurons<-as.formula(func_neurons)
func_neurons

#train neural net
neuronsZ1<-neuralnet(func_neurons,train_set1,hidden = c(3),linear.output = FALSE,stepmax = 1e9)
plot(neuronsZ1)

#predict results
predict2<-compute(neuronsZ,test_set1[,1:2])
#print results
print(head(predict2$net.result))
#confusion matrix
table(test_set1$winner,predict2$net.result)

