# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")

# Read Data Source
read.data = function(file=file){
  read.csv(file,header = TRUE,stringsAsFactors = FALSE)
}
df = read.data('RedWhiteWine.csv')

# Remove quality attribute, not needed for this assignment
df = subset(df,select=-quality)

# Add a more descriptive label for Wine Type Red=1, White=2
df$r.or.w = ifelse(df$Class == 1,'R','W')
df$r.or.w = as.factor(df$r.or.w)

#Explore Data
str(df)
summary(subset(df,select=-Class))
lapply(subset(df,select=-Class),sd)
head(df,n=10)
tail(df)
if(!require(reshape2)){install.packages("reshape2")}
head(melt(df),n=20)
if(!require(ggplot2)){install.packages("ggplot2")}
ggplot(melt(subset(df,select=-Class)),mapping = aes(x=value)) +
  geom_histogram(bins=10) +
  facet_wrap(~variable,scales = 'free_x')

#Function to Partition Data Set into training, testing, and validation sets
ds.partition = function(ds){
  ds.randoms = runif(nrow(ds))
  ds.split.mark = quantile(ds.randoms,.5)
  ds.train.flg = ds.randoms <= ds.split.mark
  ds.training = ds[ds.train.flg,]
  
  ds.randoms.t = runif(nrow(ds.training))
  ds.split.mark.t = quantile(ds.randoms.t,.5)
  ds.test.flg.t = ds.randoms.t <= ds.split.mark.t
  ds.testing = ds.training[ds.test.flg.t,]
  ds.validate = ds.training[!ds.test.flg.t,]
  
  ds.split = list(data.training=ds.training,data.testing=ds.testing,data.validating=ds.validate)
  return(ds.split)
}

#set seed
set.seed(4)
#Partition Data Sets
ds.partitions = ds.partition(df)

build.tree = function(tree.formula,dataset){
  if(!require(rpart)){install.packages("rpart")}
  
  tree = rpart(tree.formula,data=dataset,method='class')
  return(tree)
}
#Plot a fancy tree
plot.tree.fancy = function(tree){
  if(!require(rpart.plot)){install.packages("rpart.plot")}
  if(!require(rattle)){install.packages("rattle")}
  if(!require(RColorBrewer)){install.packages("RColorBrewer")}
  
  fancyRpartPlot(tree)
}

#Prediciton Function
get.wine.prediction = function(tree,dataset,threshold=.7){
  #Predict
  predictions = predict(tree,dataset)
  actuals = ifelse(dataset$r.or.w=='R',"R","W")
  #Use a threshold to turn prediction probability into a prediction
  #Convert the predicted probabilites to predictions using a threshold
  predicted = ifelse(predictions[,1] > threshold, "R", "W")
  
  #confusion matrices
  cm = table(predicted,actuals,dnn = c("Predicted","Actual"))
  print(cm)
  
  #Calculate accuracy is defined as the fraction of predictions that are correct
  red.count.actual = nrow(dataset[dataset$r.or.w == 'R',])
  white.count.actual = nrow(dataset[dataset$r.or.w == 'W',])
  red.count.predicted = cm[1]
  white.count.predicted = cm[2,2]
  total.tests = nrow(dataset)
  total.correct.predictions = red.count.predicted + white.count.predicted
  
  accuracy = total.correct.predictions/total.tests
  print(paste0("Accuracy: ","%",round(accuracy*100,1)))
  return(predictions)
}

#Get AUC
wine.prediction.auc = function(response,predictor){
  if(!require(pROC)){install.packages("pROC")}
  return(auc(response,predictor))
}

#Build a Decision tree based on all Attributes and plot the results
tree.all = build.tree(r.or.w~.-Class,ds.partitions$data.training)
plot.tree.fancy(tree.all)
get.wine.prediction(tree.all,ds.partitions$data.testing)



