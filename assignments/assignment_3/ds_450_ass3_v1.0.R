# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")

# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")

# Read Data Source
read.data = function(file=file){
  read.csv(file,header = TRUE,stringsAsFactors = FALSE)
}
sessions.df = read.data('Sessions.csv')

if(!require(factoextra)){install.packages("factoextra")}
names(sessions.df)
nrow(sessions.df)
str(sessions.df)

head(sessions.df)

x2=sessions.df[,c('Home','Search','Prod_B','Products','Prod_C')]
x2.cluster = kmeans(x2,4)
x2.cluster

#Determine number of clusters
wss = (nrow(sessions.df)-1)*sum(apply(sessions.df,2,var))

for(i in 2:15) wss[i] = sum(kmeans(sessions.df,centers=i)$withinss)
plot(1:15,wss,type='b',xlab="Number of Clusters", ylab="Within groups sum of squares")

clusters4 = kmeans(sessions.df,4) # 4 clusters
clusters4
#get cluster means
aggregate(sessions.df,by=list(clusters4$cluster),FUN=mean)
#append cluster assignment
sessions.df = data.frame(sessions.df,clusters4$cluster)
head(sessions.df)

if(!require(cluster)){install.packages("cluster")}
#plot cluster
clusplot(sessions.df,clusters4$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)

#Finding K
n = 200
g = 6
set.seed(g)
k.find.df = data.frame(x = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))),
                       y = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))))
head(k.find.df)
nrow(k.find.df)
plot(k.find.df)

sessions.df.scaled = scale(sessions.df)
set.seed(777)
km.scalled = kmeans(sessions.df.scaled,4,nstart=25)
km.scalled$cluster


#Visualize k-means clusters
fviz_cluster(km.scalled,data=sessions.df.scaled,geom='point',stand=FALSE,frame.type = 'norm')

elbow.df = k.find.df
#Determine number of clusters
wss = (nrow(elbow.df)-1)*sum(apply(elbow.df,2,var))

for(i in 2:15) wss[i] = sum(kmeans(elbow.df,centers=i)$withinss)
plot(1:15,wss,type='b',xlab="Number of Clusters", ylab="Within groups sum of squares")

max.k = 8 #Max number of clusters
df = sessions.df.scaled
wss = sapply(1:max.k, function(k){kmeans(df, k, nstart=10)$tot.withinss})
plot(1:max.k, wss, type="b", pch=19, frame=FALSE, xlab="Number of clusters K", ylab="Total within-clusters sum of squares")
abline(v=3,lty=2)

if(!require(fpc)){install.packages("fpc")}
best = pamk(k.find.df)
cat("Silhouette Cluster Estimation: ",best$nc,"\n")
plot(pam(k.find.df,best$nc))


