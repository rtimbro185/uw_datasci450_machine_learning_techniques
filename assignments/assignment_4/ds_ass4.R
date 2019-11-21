# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")
setwd('C:/workspaces/uw_data_science/ds_450/R/assignment_4')

# Read Data Source
read.data = function(file=file){
  read.csv(file,header = TRUE,stringsAsFactors = FALSE)
}

video.df0 = read.data('Video_Store.csv')

names(video.df0)
nrow(video.df0)
str(video.df0)

head(video.df0)


## Data Preparation  ########################################################################################################################################################################################
## Task A - Use smoothing by bin means to smooth the values of the Age attribute. Use a bin depth of 4
## Example: Bin 1: 4, 8, 9, 15 - Bin 2: 21, 21, 24, 25 - Smoothing by bin means: - Bin 1: 9, 9, 9, 9 - Bin 2: 23, 23, 23, 23

if(!require(taRifx)){install.packages("taRifx")}
if(!require(plyr)){install.packages("plyr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(gridExtra)){install.packages("gridExtra")}

video.df0 = sort(video.df0, f = ~ Age)
video.df0
n = 4
nr = nrow(video.df0)
video.df0.s1 = split(video.df0, rep(1:ceiling(nr/n), each=n, length.out=nr))

for(i in 1:length(video.df0.s1)){
  video.df0.s1[[i]]$Age = round(mean(video.df0.s1[[i]]$Age),0)
}
video.df1 = ldply(video.df0.s1, data.frame)
video.df1 = select(video.df1,-.id)
  
options(repr.plot.width=8, repr.plot.height=8)
bw1 = (max(video.df0$Age) - min(video.df0$Age))/20
bw2 = (max(video.df1$Age) - min(video.df1$Age))/30
p1 = ggplot(video.df0, aes(Age)) + geom_histogram(binwidth = bw1) +
  ggtitle('Histogram of Video Store Customer Ages')
p2 = ggplot(video.df1, aes(Age)) + geom_histogram(binwidth = bw2) +
  ggtitle('Histogram of Video Store Customer Ages Smoothed')
grid.arrange(p1,p2,nrow=2)

#############################################################################################################################################################################################################

## Data Preparation  ########################################################################################################################################################################################
## Task B - Use min-max normalization to transform the values of the Income attribute onto the range [0.0-1.0].
v = video.df0$Income
income.min.max.norm = round((v - min(v)) / (max(v) - min(v)),1)
income.min.max.norm
video.df1$Income = income.min.max.norm



#############################################################################################################################################################################################################
## Data Preparation  ########################################################################################################################################################################################
## Task C - Use z-score normalization to standardize the values of the Rentals attribute
v = video.df0$Rentals
rentals.z_score.norm = (v - mean(v)) / sd(v)
rentals.z_score.norm
video.df1$Rentals = rentals.z_score.norm

#############################################################################################################################################################################################################

#############################################################################################################################################################################################################
## Data Preparation  ########################################################################################################################################################################################
## Task D - Discretize the (original) Income attribute based on the following categories: High = 60K+; Mid = 25K-59K; Low = less than $25K
video.df1$Income.DISC[video.df0$Income >= 60000] = 'High'
video.df1$Income.DISC[video.df0$Income >= 25000 & video.df0$Income <= 59000] = 'Mid'
video.df1$Income.DISC[video.df0$Income < 25000] = 'Low'
print("Frequence Table Showing The Grouping of Income Levels by Category")
table(video.df0$Income,video.df1$Income.DISC)

# Prepare Data Further
video.df1$Age.Desc[video.df0$Age >= 70 & video.df0$Age <= 87] = 'SilentG'
video.df1$Age.Desc[video.df0$Age >= 51 & video.df0$Age <= 69] = 'BabyBoomer'
video.df1$Age.Desc[video.df0$Age >= 35 & video.df0$Age <= 50] = 'GenX'
video.df1$Age.Desc[video.df0$Age >= 18 & video.df0$Age <= 34] = 'Millenial'
video.df1$Age.Desc[video.df0$Age < 18] = 'Child'
table(video.df0$Age, video.df1$Age.Desc)

#############################################################################################################################################################################################################

#############################################################################################################################################################################################################
## Data Preparation  ########################################################################################################################################################################################
## Task E - Convert the original data (not the results of parts a-d) into the standard spreadsheet format
## (note that this requires that you create, for every categorical attribute, additional attributes corresponding to values of that categorical attribute; numerical attributes in the original data remain unchanged).
print("Structer of Original Data Set")
str(video.df0)
if(!require(dummies)){install.packages("dummies")}

video.df2 = video.df0
video.df3 = video.df2
# Convert Gender categorical attribute into a numeric value

print("Gender Attribute Unique Values")
unique(video.df2$Gender)
video.df2$Gender = as.integer(ifelse(video.df2$Gender=='M',1,0))

# Convert Incedentals categorical attribute into a numeric value
print("Incidentals Attribute Unique Values")
unique(video.df2$Incidentals)
video.df2$Incidentals = as.integer(ifelse(video.df2$Incidentals=='Yes',1,0))

# Convert Genre categorical attributes into numeric representations using the dummies package
print("Genre Attribute Unique Values")
unique(video.df2$Genre)
video.df2$Genre = as.factor(video.df2$Genre)
video.df2 = dummy.data.frame(video.df2, names = c('Genre'), sep="_")
video.df2 = select(video.df2,-Cust.ID)
print("Structer of Original Data Set after converting non-numeric attributes into the spreadsheet format")
str(video.df2)
# Create a data frame using the dummies package on all categorical attributes
video.df3 = dummy.data.frame(video.df3, names = c('Gender','Incidentals','Genre'), sep="_")
print("Structer of Original Data Set after converting non-numeric attributes into the spreadsheet format using the dummies package")
video.df3 = select(video.df3,-Cust.ID)
str(video.df3)


write.csv(video.df3, file = "ds_ass4_task_E.csv", row.names=FALSE)


#############################################################################################################################################################################################################

#############################################################################################################################################################################################################
## Data Preparation  ########################################################################################################################################################################################
## Task F - Using the standardized data set (from part e), perform basic correlation analysis among the attributes.¶
##          Discuss your results by indicating any strong correlations (positive or negative) among pairs of attributes. 
##          You need to construct a complete Correlation Matrix (Please read the brief document Basic Correlation Analysis (see course website) for more detail).
##          Question: Can you observe any "significant" patterns among groups of two or more variables? Explain
if(!require(corrplot)){install.packages("corrplot")}
if(!require(caret)){install.packages("caret")}
if(!require(mlbench)){install.packages("mlbench")}


options(repr.plot.height = 6, repr.plot.width = 8)
c.plot = cor(video.df2, method = 'pearson')
print(c.plot)
corrplot(c.plot, method="number", type='lower')
c.high = findCorrelation(c.plot,cutoff=0.5)
print(c.high)
colnames(video.df2[c.high])

if(!require(corrplot)){install.packages("corrplot")}
options(repr.plot.height = 6, repr.plot.width = 8)
c.plot = cor(video.df3, method = 'pearson')
print(c.plot)
corrplot(c.plot, method="number", type='lower')

c.high = findCorrelation(c.plot,cutoff=0.5)
print(c.high)
colnames(video.df3[c.high])


options(repr.plot.height = 6, repr.plot.width = 8)
video.df2.sub1 = select(video.df2, Age, Income, Gender)
c.plot = cor(video.df2.sub1, method = 'pearson')
print(c.plot)
corrplot(c.plot, method="number", type='lower')


#############################################################################################################################################################################################################

#############################################################################################################################################################################################################
## Data Preparation  ########################################################################################################################################################################################
## Task G - Perform a cross-tabulation of the two "gender" variables versus the three "genre" variables.
##          Show this as a 2 x 3 table with entries representing the total counts.
##          Then, use a graph or chart that provides the best visualization of the relationships between these sets of variables.
##          Question: Can you draw any significant conclusions?

# 
xtabs(~Gender + Genre, data = video.df0)
summary(xtabs(~Gender + Genre, data = video.df0))
plot(xtabs(~Gender + Genre, data = video.df0))


video.df2.sub2 = select(video.df2,Gender,Genre_Action,Genre_Comedy,Genre_Drama)
# Correlation Plot complete data set
if(!require(corrplot)){install.packages("corrplot")}
options(repr.plot.height = 6, repr.plot.width = 8)
c.plot = cor(video.df2.sub2, method = 'pearson')
print(c.plot)
corrplot(c.plot, method="number", type='lower')

# Use dummies package on all categorical variables, see how that effects things
video.df3.sub1 = select(video.df3,Gender_F,Gender_M,Genre_Action,Genre_Comedy,Genre_Drama)
options(repr.plot.height = 6, repr.plot.width = 8)
c.plot = cor(video.df3.sub1, method = 'pearson')
print(c.plot)
corrplot(c.plot, method="number", type='lower')

#############################################################################################################################################################################################################

#############################################################################################################################################################################################################
## Data Preparation  ########################################################################################################################################################################################
## Task H - Select all "good" customers with a high value for the Rentals attribute
##          (a "good customer is defined as one with a Rentals value of greater than or equal to 30).
##          Then, create a summary (e.g., using means, medians, and/or other statistics) of the selected data with respect to all other attributes.
##          Note: To know whether your observed patterns in the target group are significant, you need to compare them with the general population using the same metrics. 

if(!require(dplyr)){install.packages("dplyr")}
video.df4 = video.df3
video.df3.good.custs = filter(video.df3,Rentals >= 30)
print("#### Good Customers Head data and Summary Statistics")
head(video.df3.good.custs)
summary(video.df3.good.custs)
print("#### Bad Customers Head data and Summary Statistics")
video.df3.bad.custs = filter(video.df3,Rentals < 30)
head(video.df3.bad.custs)
summary(video.df3.bad.custs)

video.df4$Is_Good_Cust = as.integer(ifelse(video.df4$Rentals >= 30,1,0))
video.df4$Age = video.df1$Age.Desc
video.df4$Rentals = video.df1$Rentals
video.df4$Income = video.df1$Income.DISC
video.df4 = dummy.data.frame(video.df4, names = c('Income','Age'), sep="_")
head(video.df4)
video.df4.sub1 = select(video.df4,-Rentals, - Avg.Per.Visit)

print("#### Plot Correlationg Matrix for Good Customers ####")
options(repr.plot.height = 10, repr.plot.width = 10)
good.custs = filter(video.df4.sub1,Is_Good_Cust == 1)
nrow(good.custs)
c.plot1 = cor(good.custs, method = 'pearson', use = 'complete.obs')
c.plot1[is.na(c.plot1)] = as.numeric(0.0001)
print(c.plot1)
c.high1 = findCorrelation(c.plot1,cutoff=0.5)
print(c.high1)
colnames(video.df4.sub1[c.high1])
corrplot(c.plot1, method="number", type='lower')


print("#### Plot Correlationg Matrix for Bad Customers ####")
options(repr.plot.height = 10, repr.plot.width = 10)
bad.custs = filter(video.df4.sub1,Is_Good_Cust == 0)
nrow(bad.custs)
c.plot2 = cor(bad.custs, method = 'pearson', use = 'complete.obs')
c.plot2[is.na(c.plot2)] = as.numeric(0.0001)
#print(c.plot2)
c.high2 = findCorrelation(c.plot2,cutoff=0.5)
print(c.high2)
colnames(video.df4.sub1[c.high2])
corrplot(c.plot2, method="number", type='lower')





# Perform K-Means Clustering
if(!require(factoextra)){install.packages("factoextra")}
#Determine number of clusters
wss = (nrow(video.df4)-1)*sum(apply(video.df4,2,var))
for(i in 2:15) wss[i] = sum(kmeans(video.df4,centers=i)$withinss)
plot(1:15,wss,type='b',xlab="Number of Clusters", ylab="Within groups sum of squares")

# Scale good customer data frame
video.df4.scaled = scale(video.df4)

max.k = 8 #Max number of clusters
df = video.df4.scaled
wss = sapply(1:max.k, function(k){kmeans(video.df4.scaled, k, nstart=10)$tot.withinss})
plot(1:max.k, wss, type="b", pch=19, frame=FALSE, xlab="Number of clusters K", ylab="Total within-clusters sum of squares")
abline(v=3,lty=2)

if(!require(NbClust)){install.packages("NbClust")}
set.seed(766)
nb.clust = NbClust(video.df4, distance='euclidean', min.nc=2, max.nc=10, method="complete", index="gap")
nb.clust

set.seed(777)
km.scalled = kmeans(video.df4.scaled,3,nstart=25)
km.scalled$cluster
if(!require(cluster)){install.packages("cluster")}
#plot cluster
clusplot(video.df4.scaled,km.scalled$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)

#get cluster means
aggregate(video.df4,by=list(km.scalled$cluster),FUN=mean)
#append cluster assignment
video.df4 = data.frame(video.df4,km.scalled$cluster)
head(video.df4)

df.cluster1 = video.df4[which(video.df4$km.scalled.cluster == 1),]
nrow(df.cluster1)
#ftable(xtabs(~Incidentals_Yes + Gender_M + Gender_F + Income_High + Income_Mid, data = df.cluster1))
df.c1.summary = summarise(df.cluster1, count = n(), Good_Customers = sum(Is_Good_Cust), Bad_Customers = n()-sum(Is_Good_Cust), Males = sum(Gender_M), Females = sum(Gender_F), High_Incomes = sum(Income_High), Mid_Incomes = sum(Income_Mid), Low_Incomes = sum(Income_Low), BabyBoomers = sum(Age_BabyBoomer), 
          Children = sum(Age_Child), GenXs = sum(Age_GenX), Millenials = sum(Age_Millenial), Rentals_Mean = mean(Rentals), Avg_Per_Visit = mean(Avg.Per.Visit), Incidentals_Yes = sum(Incidentals_Yes),
          Incidentals_No = sum(Incidentals_No), Genre_Actions = sum(Genre_Action), Genre_Comedys = sum(Genre_Comedy), Genre_Dramas = sum(Genre_Drama))
df.c1.summary
df.cluster2 = video.df4[which(video.df4$km.scalled.cluster == 2),]
nrow(df.cluster2)
df.c2.summary = summarise(df.cluster2, count = n(), Good_Customers = sum(Is_Good_Cust), Bad_Customers = n()-sum(Is_Good_Cust), Males = sum(Gender_M), Females = sum(Gender_F), High_Incomes = sum(Income_High), Mid_Incomes = sum(Income_Mid), Low_Incomes = sum(Income_Low), BabyBoomers = sum(Age_BabyBoomer), 
          Children = sum(Age_Child), GenXs = sum(Age_GenX), Millenials = sum(Age_Millenial), Rentals_Mean = mean(Rentals), Avg_Per_Visit = mean(Avg.Per.Visit), Incidentals_Yes = sum(Incidentals_Yes),
          Incidentals_No = sum(Incidentals_No), Genre_Actions = sum(Genre_Action), Genre_Comedys = sum(Genre_Comedy), Genre_Dramas = sum(Genre_Drama))
df.c2.summary
df.cluster3 = video.df4[which(video.df4$km.scalled.cluster == 3),]
nrow(df.cluster3)
df.c3.summary = summarise(df.cluster3, count = n(), Good_Customers = sum(Is_Good_Cust), Bad_Customers = n()-sum(Is_Good_Cust), Males = sum(Gender_M), Females = sum(Gender_F), High_Incomes = sum(Income_High), Mid_Incomes = sum(Income_Mid), Low_Incomes = sum(Income_Low), BabyBoomers = sum(Age_BabyBoomer), 
          Children = sum(Age_Child), GenXs = sum(Age_GenX), Millenials = sum(Age_Millenial), Rentals_Mean = mean(Rentals), Avg_Per_Visit = mean(Avg.Per.Visit), Incidentals_Yes = sum(Incidentals_Yes),
          Incidentals_No = sum(Incidentals_No), Genre_Actions = sum(Genre_Action), Genre_Comedys = sum(Genre_Comedy), Genre_Dramas = sum(Genre_Drama))
df.c3.summary

summary.cluster.df = rbind.fill(df.c1.summary,df.c2.summary,df.c3.summary)
row.names(summary.cluster.df) = c('Cluster1', 'Cluster2', 'Cluster3')
summary.cluster.df
write.csv(summary.cluster.df, file = "ds_ass4_cluster_analysis.csv", row.names=FALSE)


#############################################################################################################################################################################################################

#############################################################################################################################################################################################################
## Data Preparation  ########################################################################################################################################################################################
## Task I - Suppose that because of the high profit margin, the store would like to increase the sales of incidentals.
##          Based on your observations in previous parts discuss how this could be accomplished. Explain your answer based on your analysis of the data.
##          Question: Should customers with specific characteristics be targeted?
##          Question: Should certain types of movies be preferred?¶
#Plot tree function - fancy...
plot.tree.fancy = function(tree){
  if(!require(rpart.plot)){install.packages("rpart.plot")}
  if(!require(rattle)){install.packages("rattle")}
  if(!require(RColorBrewer)){install.packages("RColorBrewer")}
  
  fancyRpartPlot(tree)
}

df.incidentals = video.df1
df.incidentals = select(df.incidentals, -Cust.ID, -Income, -Age)
df.incidentals$Gender = as.factor(df.incidentals$Gender)
df.incidentals$Incidentals = as.factor(df.incidentals$Incidentals)
df.incidentals$Genre = as.factor(df.incidentals$Genre)
df.incidentals$Income.DISC = as.factor(df.incidentals$Income.DISC)
df.incidentals$Age.Desc = as.factor(df.incidentals$Age.Desc)

# Define train contrpl
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# Training the model
incidentals.model <- train(Incidentals~., data=df.incidentals, method="rpart", preProcess="scale", trControl=control)
print(incidentals.model$finalModel)

plot.tree.fancy(incidentals.model$finalModel)
incidentals.model$finalModel$variable.importance


bank.rForest = randomForest( pep ~ ., data = train.set, ntree = 10000, importance = T)
bank.rForest
plot(bank.rForest, main = "Error Rate of Random Forest Using All Features for Sweeping")
varImpPlot(bank.rForest, sort = T, main = "Variable Importance of Bank Data")
prediction.PEP = predict(bank.rForest, test.set[,colnames(Bank.Data)], type = "class")
c.matrix = table(prediction.PEP, test.set$pep)
confusionMatrix(c.matrix)

roc = roc(response = as.numeric(test.set$pep), predictor = as.numeric(prediction.PEP), plot = T, auc = T)
roc


normalize <- function(x) { 
  x <- as.matrix(x)
  minAttr=apply(x, 2, min)
  maxAttr=apply(x, 2, max)
  x <- sweep(x, 2, minAttr, FUN="-") 
  x=sweep(x, 2,  maxAttr-minAttr, "/") 
  attr(x, 'normalized:min') = minAttr
  attr(x, 'normalized:max') = maxAttr
  return (x)
}

out.video.df4 = video.df4
n = normalize(out.video.df4$Rentals)
out.video.df4$Rentals = n[0:50,]

n = normalize(out.video.df4$Avg.Per.Visit)
out.video.df4$Avg.Per.Visit = n[0:50,]
out.video.df4 = select(out.video.df4,-Rental)
# Write CSV in R
write.csv(out.video.df4, file = "ds_ass4_all_numeric.csv",row.names=FALSE)

#parts a - d 
video.df0.plus = video.df0
video.df0.plus$Age.Smooth = video.df1$Age
video.df0.plus$Income.Norm = video.df1$Income
video.df0.plus$Rentals.Z_Score = video.df1$Rentals
video.df0.plus$Income.DISC = video.df1$Income.DISC


write.csv(video.df0.plus, file = "ds_ass4_aTod.csv", row.names=FALSE)
#############################################################################################################################################################################################################


