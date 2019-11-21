# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")

# Read Data Source
read.data = function(file=file){
  read.csv(file,header = TRUE,stringsAsFactors = FALSE)
}
bank.def = read.data('BankData.csv')

#Inspect the data
# Clean or format Data
str(bank.def)
if(!require(dplyr)){install.packages("dplyr")}
head(sample_frac(bank.def,0.5),10)
table(bank.def$pep)/nrow(bank.def)

bank.def$pep = as.factor(bank.def$pep)
bank.def$sex = as.factor(bank.def$sex)
bank.def$married = as.factor(bank.def$married)
bank.def$car = as.factor(bank.def$car)
bank.def$save_act = as.factor(bank.def$save_act)
bank.def$current_act = as.factor(bank.def$current_act)
bank.def$mortgage = as.factor(bank.def$mortgage)
bank.def$region = as.factor(bank.def$region)
str(bank.def)
unique(bank.def$pep)


#Visualize the Data
if(!require(reshape2)){install.packages("reshape2")}
head(melt(bank.def),n=5)
if(!require(ggplot2)){install.packages("ggplot2")}
ggplot(melt(subset(bank.def,select=-pep)),mapping = aes(x=value)) +
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


#Pre-process - Partition data set
#set seed
set.seed(4)
#Partition Data Sets
ds.partitions = ds.partition(bank.def)
training.data = ds.partitions$data.training
testing.data = ds.partitions$data.testing
validating.data = ds.partitions$data.validating
# What is the proportion of PEP in each data set
print("Training Data Set: PEP Preportion 0=NO, 1=YES");table(training.data$pep)/nrow(training.data)
print("Testing Data Set: PEP Preportion 0=NO, 1=YES");table(testing.data$pep)/nrow(testing.data)
print("Validation Data Set: PEP Preportion 0=NO, 1=YES");table(validating.data$pep)/nrow(validating.data)


#Build Modeling Formula
build.formula = function(toBeModeled,modeledByList){
  attributesPlus = paste(attributes,collapse="+")
  formula = as.formula(paste(toBeModeled,attributesPlus,sep="~"))
  return(formula)
}

#Build Classification Tree (Recursive Partitioning and Regression Trees)
build.tree = function(tree.formula,dataset){
  if(!require(rpart)){install.packages("rpart")}
  
  tree = rpart(tree.formula,data=dataset,method='class')
  return(tree)
}

#Plot tree function - fancy...
plot.tree.fancy = function(tree){
  if(!require(rpart.plot)){install.packages("rpart.plot")}
  if(!require(rattle)){install.packages("rattle")}
  if(!require(RColorBrewer)){install.packages("RColorBrewer")}
  
  fancyRpartPlot(tree)
}

#Prediciton Function
get.pep.predictions = function(tree,dataset,threshold=.7){
  #Predict
  predictions = predict(tree,dataset)
  actuals = ifelse(dataset$pep=='YES',"YES","NO")
  #Use a threshold to turn prediction probability into a prediction
  #Convert the predicted probabilites to predictions using a threshold
  #threshold = .7
  predicted = ifelse(predictions[,2] > threshold, "YES", "NO")
  
  #confusion matrices
  cm = table(predicted,actuals,dnn = c("Predicted","Actual"))
  print(cm)
  
  #Calculate accuracy is defined as the fraction of predictions that are correct
  yes.count.actual = nrow(dataset[dataset$pep == 'YES',])
  no.count.actual = nrow(dataset[dataset$pep == 'NO',])
  yes.count.predicted = cm[1]
  no.count.predicted = cm[2,2]
  total.tests = nrow(dataset)
  total.correct.predictions = yes.count.predicted + no.count.predicted
  
  accuracy = total.correct.predictions/total.tests
  print(paste0("Accuracy: ","%",round(accuracy*100,1)))
  return(predictions)
}
#Get AUC
pep.prediction.auc = function(response,predictions){
  if(!require(pROC)){install.packages("pROC")}
  roc.obj = roc(response,predictions)
  return(auc(roc.obj))
}
#Plot AUC
plot.pep.auc = function(response,predictions){
  if(!require(pROC)){install.packages("pROC")}
  roc.obj = roc(response,predictions)
  scores.rounded = round(predictions,digits=1)
  roc.rounded = roc(response,scores.rounded)
  plot(roc.obj,print.auc=TRUE)
  lines(roc.rounded,col='red',type='b')
  text(0.4,0.43,labels=sprintf("AUC:%0.3f",auc(roc.rounded)),col='red')
}

#Build a Decision tree based on all Attributes and plot the results
attributes = names(training.data)
attributes = attributes[!attributes %in% c("pep")]
f = build.formula('pep',attributes)
tree.all = build.tree(f,training.data)
plot.tree.fancy(tree.all)

#Print the summary results of our training model
summary(tree.all)

#Calculate Accuracy of training model predictions to test data set and the AUC, the area under the ROC Curve
pep.predictions = get.pep.predictions(tree.all,testing.data)
pep.auc = pep.prediction.auc(testing.data$pep,pep.predictions[,2])
paste0("AUC: ",pep.auc)
plot.pep.auc(testing.data$pep,pep.predictions[,2])

# Experiment with Pruneing
prune.tree.all = prune(tree.all,cp=tree.all$cptable[which.min(tree.all$cptable[,"xerror"]),'CP'])

#plot pruned tree
plot(prune.tree.all,uniform=TRUE,main="Pruned Classification Tree for Bank Data")
text(prune.tree.all,use.n=TRUE,cex=.8)
post(prune.tree.all,file="prune.tree.bank.data.ps", title="Pruned Classification Tree for Bank Data")


if(!require(caret)){install.packages("caret")}
if(!require(klaR)){install.packages("klaR")}
#Automatic grid search
#determin training control
t.control = trainControl(method='cv', number=10, classProbs = TRUE)
#fix the parameters of the algorithm
grid = expand.grid(.fL=c(0),.usekernel=c(FALSE))
#train the model
t.model = train(f,data=bank.def,method='rpart',trControl=t.control,tuneLength=10, tuneGrid=grid)
#summarize results
print(t.model)

#Print the final model from the parameter sweep process
finalModel = t.model$finalModel
print(finalModel)

plot.tree.fancy(finalModel)
finalModel$variable.importance
finalModel$call$data
finalModel$xNames

#AUC
ctrl = trainControl(method="repeatedcv", number=10, repeats = 300, savePredictions = TRUE, classProbs = TRUE)
mdl = train(f, data=bank.def, method = "rpart", trControl = ctrl)
pred = predict(mdl, newdata = bank.def, type="prob")
roc.1 = roc(bank.def$pep, pred$control)
roc.2 = roc(mdl$pred$obs,mdl$pred$control)
roc.3 = roc(as.numeric(mdl$trainingData$.outcome=='case'),aggregate(case~rowIndex,mdl$pred,mean)[,'case'])


##Create binary class labels
#bank.def$is.married = ifelse(bank.def$married == 'YES',1,0)
#bank.def$has.car = ifelse(bank.def$car == 'YES',1,0)
#bank.def$has.save_act = ifelse(bank.def$save_act == 'YES',1,0)
#bank.def$is.current_act = ifelse(bank.def$current_act == 'YES',1,0)
#bank.def$has.mortgage = ifelse(bank.def$mortgage == 'YES',1,0)
#bank.def$offer.pep.class = ifelse(bank.def$pep == 'YES',1,0)
#str(bank.def)
#bank.def.class = subset(bank.def, select= -c(married,car,save_act,current_act,mortgage,pep))
#str(bank.def.class)
#head(sample_frac(bank.def.class,0.5),10)

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

#Pre-process - Partition data set
#set seed
set.seed(4)
#Partition Data Sets
ds.partitions = ds.partition(bank.def)
#ds.partitions
table(ds.partitions$data.training$pep)/nrow(ds.partitions$data.training)
table(ds.partitions$data.testing$pep)/nrow(ds.partitions$data.testing)
table(ds.partitions$data.validating$pep)/nrow(ds.partitions$data.validating)

#function to build a list of training sets
#build.training.sets = function(training.set,n.samples,size.sample){
  set.seed(908)   
  trainings = list()
  for(i in 1:n.samples){
    def.sample = sample_frac(training.set,size.sample,replace=TRUE)
    trainings[[i]] = def.sample
  } 
  return(trainings)
}

#function learn tree Ti from Di
#learn.tree = function(training.sets,testing.sets,num.tests){
  
  
  
  return(tree)
}


#Construct Attributes list formula for Random Forest Input
attributes = names(ds.partitions$data.training)
attributes = attributes[!attributes %in% c("pep")]
attributesPlus = paste(attributes,collapse="+")
r.forest.formula = as.formula(paste("pep",attributesPlus,sep="~"))
r.forest.formula

#Build Random Forest
if(!require(randomForest)){install.packages("randomForest")}
target.mark.rf = randomForest(r.forest.formula,ds.partitions$data.training,ntree=500,importance=T)
plot(target.mark.rf)


#Variable Importance Table
attribute.importance = data.frame(importance(target.mark.rf,type=2))
attribute.importance$Attributes = row.names(attribute.importance)
attribute.importance[order(attribute.importance$MeanDecreaseGini,decreasing=T),]

#Predict the response
ds.partitions$data.training$prediction = predict(target.mark.rf,ds.partitions$data.training)

#Confusion Matrix
if(!require(e1071)){install.packages("e1071")}
if(!require(caret)){install.packages("caret")}
confusionMatrix(data=ds.partitions$data.training$prediction,
                reference=ds.partitions$data.training$pep,
                positive='YES')

if(!require(party)){install.packages("party")}
plot(target.mark.rf,type='simple')

options(repos='http://cran.rstudio.org')
have.packages <- installed.packages()
cran.packages <- c('devtools','plotrix','randomForest','tree')
to.install <- setdiff(cran.packages, have.packages[,1])
if(length(to.install)>0) install.packages(to.install)

library(devtools)
if(!('reprtree' %in% installed.packages())){
  install_github('araastat/reprtree')
}
for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))


library(randomForest)
library(reprtree)
reprtree:::plot.getTree(target.mark.rf)

#Get a tree from the Random Forest Model
library(randomForest)
library(reprtree)
tree <- getTree(target.mark.rf, k=1, labelVar=TRUE)
realtree <- reprtree:::as.tree(tree, target.mark.rf)
plot(tree,y=NULL,type=c("proportional","uniform"))

#function to build a list of training sets
build.training.sets = function(training.set,n.samples,size.sample){
  set.seed(908)   
  trainings = list()
  for(i in 1:n.samples){
    def.sample = sample_frac(training.set,size.sample,replace=TRUE)
    trainings[[i]] = def.sample
  } 
  return(trainings)
}

#classification Tree (Recursive Partitioning and Regression Trees)
build.tree = function(tree.formula,dataset){
  if(!require(rpart)){install.packages("rpart")}
  
  tree = rpart(tree.formula,data=dataset,method='class')
  return(tree)
}

#Plot tree function - fancy...
plot.tree.fancy = function(tree){
  if(!require(rpart.plot)){install.packages("rpart.plot")}
  if(!require(rattle)){install.packages("rattle")}
  if(!require(RColorBrewer)){install.packages("RColorBrewer")}
  
  fancyRpartPlot(tree)
}

#function learn tree Ti from Di
learn.tree = function(training.sets,testing.sets,num.tests){
  
  
  
  return(tree)
}
#Prediciton Function
get.pep.prediction = function(tree,dataset){
  #Predict
  predictions = predict(tree,dataset)
  actuals = ifelse(dataset$pep=='YES',"YES","NO")
  #Use a threshold to turn prediction probability into a prediction
  #Convert the predicted probabilites to predictions using a threshold
  threshold = .7
  predicted = ifelse(predictions[,1] > threshold, "YES", "NO")
  
  #confusion matrices
  cm = table(predicted,actuals,dnn = c("Predicted","Actual"))
  print(cm)
  
  #Calculate accuracy is defined as the fraction of predictions that are correct
  yes.count.actual = nrow(dataset[dataset$pep == 'YES',])
  no.count.actual = nrow(dataset[dataset$pep == 'NO',])
  yes.count.predicted = cm[1]
  no.count.predicted = cm[2,2]
  total.tests = nrow(dataset)
  total.correct.predictions = yes.count.predicted + no.count.predicted
  
  accuracy = total.correct.predictions/total.tests
  print(paste0("Accuracy: ","%",round(accuracy*100,1)))
  return(predictions)
}
#Get AUC
pep.prediction.auc = function(response,predictions){
  if(!require(pROC)){install.packages("pROC")}
  roc.obj = roc(response,predictions)
  return(auc(roc.obj))
}
#Plot AUC
plot.pep.auc = function(response,predictions){
  if(!require(pROC)){install.packages("pROC")}
  roc.obj = roc(response,predictions)
  scores.rounded = round(predictions,digits=1)
  roc.rounded = roc(response,scores.rounded)
  plot(roc.obj,print.auc=TRUE)
  lines(roc.rounded,col='red',type='b')
  text(0.4,0.43,labels=sprintf("AUC:%0.3f",auc(roc.rounded)),col='red')
}

#Build a Decision tree based on all Attributes and plot the results
tree.all = build.tree(r.forest.formula,ds.partitions$data.training)
plot.tree.fancy(tree.all)

#Predict the response on the test data
ds.partitions$data.training$prediction = predict(tree.all,ds.partitions$data.training)
pred = predict(tree.all,ds.partitions$data.training)
pred[,1]
head(ds.partitions$data.training)
if(!require(e1071)){install.packages("e1071")}
if(!require(caret)){install.packages("caret")}
confusionMatrix(data=ds.partitions$data.training$prediction,
  reference=ds.partitions$data.training$pep,
  positive='YES')


#Random Forest - D=Training set
training.sets = NULL
training.sample.size = .2
training.n.samples = 5
testing.sets = NULL
k.forest.trees = 1
n.tests = 100

for(i in 1:k.forest.trees){
  #build data set Di by sampling with replacement from D
  training.sets = build.training.sets(ds.partitions$data.training,training.n.samples,training.sample.size)
  #learn tree Ti from Di
}
length(training.sets)
head(training.sets[[i]])

#explore
if(!require(rpart)){install.packages("rpart")}
fit = rpart(offer.pep.class ~ ., data=training.sets[[1]],method="class")
printcp(fit)
plotcp(fit)
summary(fit)


#Random Forest
if(!require(randomForest)){install.packages("randomForest")}
r.forest = randomForest(offer.pep.class ~ ., data=ds.partitions$data.training)

attributes = list()
attributes = names(ds.partitions$data.training)
attributes = attributes[1:10]
attributes
sample.attributes = sample_frac(as.data.frame.character(attributes),.5)
sample.attributes
