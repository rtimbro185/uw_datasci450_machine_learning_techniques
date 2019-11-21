# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")
setwd('C:/workspaces/uw_data_science/ds_450/R/assignment_5')



## Support Vector Machine  ########################################################################################################################################################################################
## For this exercise, we will use the veh-prime.arff file, and support vector machines for classification.
## Notes
##  .	As a first step you will need to modify the .arff file so that the car, noncar classes are re-placed with 1 and -1 respectively.
##  .	Consider use 10 fold cross-validation (this should come up as the default)
##  Try various parameters, and explain what you observe
#######################################################################################################

## Install packages
##########################################################################################################################################################################################
if(!require(e1071)){install.packages("e1071")}
if(!require(rattle)){install.packages("rattle")}
#if(!require(RGtk2Extras)){install.packages("RGtk2Extras")}
if(!require(GGally)){install.packages("GGally")}
#if(!require(rggobi)){install.packages("rggobi")}
#if(!require(fBasics)){install.packages("fBasics")}
#if(!require(caret)){install.packages("caret")}
if(!require(clusterSim)){install.packages("clusterSim")}
if(!require(Hmisc)){install.packages("Hmisc")}



########################################################################################


# Read Data Source / Initial Data Investigation
################################################################################################################
read.data = function(file=file){
  read.csv(file,header = TRUE,stringsAsFactors = FALSE)
}

vehPrime.df <- read.data('veh-prime.csv')

names(vehPrime.df)
nrow(vehPrime.df)
str(vehPrime.df)

head(vehPrime.df)
unique(vehPrime.df$CLASS)
describe(vehPrime.df)

vehPrime.inputs = colnames(vehPrime.df[,-37])
vehPrime.target = colnames(vehPrime.df[37])
##################################################################################################

# EXPLORE THE DATA
###################
if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(gridExtra)){install.packages("gridExtra")}


# replace car, noncar with binary classification and scale feature values
# Credit: https://stackoverflow.com/questions/15215457/standardize-data-columns-in-r
##################################################################################
vehPrime.df1 = vehPrime.df
vehPrime.df1$CLASS = as.integer(ifelse(vehPrime.df1$CLASS == 'car',1,0))

# n5 - normalization in range <-1,1> ((x-mean)/max(abs(x-mean)))
vehPrime.df1 = data.Normalization (vehPrime.df1,type="n5", normalization='column')

str(vehPrime.df1)
describe(vehPrime.df1)

write.csv(vehPrime.df1, file = "veh-prime-trans.csv", row.names=FALSE)
##################################################################################


# Normalization Functions
##############################################################################################################################
minMaxNorm = function(v){
  n = round((v - min(v)) / (max(v) - min(v)),5)
  return(n)
}
# Z-Score (Standaridization) Normalization
zScoreNorm = function(v){
  n = (v-mean(v))/sd(v)
}

# Main Flow
#########################################################################################

set.seed(3456)
# Create training / validation / test data
nobs <- nrow(vehPrime.df1) # 846 observations 
sample <- train <- sample(nrow(vehPrime.df1), 0.7*nobs) # 592 observations
validate <- sample(setdiff(seq_len(nrow(vehPrime.df1)), train), 0.15*nobs) # 126 observations
test <- setdiff(setdiff(seq_len(nrow(vehPrime.df1)), train), validate) # 128 observations




if(!require(ROCR)){install.packages("ROCR")}
if(!require(gplot2)){install.packages("gplot2")}
newData = vehPrime.df1[test,c(vehPrime.inputs,vehPrime.target)]
rOp = kernlab::predict(rbfSVM, newdata=newData, type="probabilities")[,2]

no.miss = na.omit(na.omit(newData)$CLASS)
miss.list = attr(no.miss, "na.action")
attributes(no.miss) = NULL

if (length(miss.list))
{
 pred = prediction(rOp[-miss.list], no.miss)
} else
{
 pred = prediction(rOp, no.miss)
}

pe = performance(pred, "tpr", "fpr")
au = performance(pred, "auc")@y.values[[1]]
pd = data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p = ggplot(pd, aes(x=fpr, y=tpr))
p = p + geom_line(colour="red")
p = p + xlab("False Positive Rate") + ylab("True Positive Rate")
p = p + ggtitle("ROC Curve SVM veh-prime-trans.csv [test] CLASS")
p = p + theme(plot.title=element_text(size=10))
p = p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p = p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.
# Remove observations with missing target.
no.miss = na.omit(na.omit(newData)$CLASS)
miss.list = attr(no.miss, "na.action")
attributes(no.miss) = NULL
if (length(miss.list))
{
 pred <- prediction(rOp[-miss.list], no.miss)
} else
{
 pred <- prediction(rOp, no.miss)
}
performance(pred, "auc")

