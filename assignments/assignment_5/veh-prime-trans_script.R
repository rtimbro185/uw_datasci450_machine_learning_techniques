# Rattle is Copyright (c) 2006-2015 Togaware Pty Ltd.

#============================================================
# Rattle timestamp: 2017-05-26 18:15:33 x86_64-w64-mingw32 

# Rattle version 4.1.0 user 'RTimbro1'

# This log file captures all Rattle interactions as R commands. 

Export this log to a file using the Export button or the Tools 
# menu to save a log of all your activity. This facilitates repeatability. For example, exporting 
# to a file called 'myrf01.R' will allow you to type in the R Console 
# the command source('myrf01.R') and so repeat all actions automatically. 
# Generally, you will want to edit the file to suit your needs. You can also directly 
# edit this current log in place to record additional information before exporting. 
 
# Saving and loading projects also retains this log.

# We begin by loading the required libraries.

library(rattle)   # To access the weather dataset and utility commands.
library(magrittr) # For the %>% and %<>% operators.

# This log generally records the process of building a model. However, with very 
# little effort the log can be used to score a new dataset. The logical variable 
# 'building' is used to toggle between generating transformations, as when building 
# a model, and simply using the transformations, as when scoring a dataset.

building <- TRUE
scoring  <- ! building


# A pre-defined value is used to reset the random seed so that results are repeatable.

crv$seed <- 42 

#============================================================
# Rattle timestamp: 2017-05-26 18:19:08 x86_64-w64-mingw32 

# Load the data.

veh.dfdataset <- read.csv("file:///C:/workspaces/uw_data_science/ds_450/R/assignment_5/veh-prime.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

#============================================================
# Rattle timestamp: 2017-05-26 18:19:08 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
veh.dfnobs <- nrow(veh.dfdataset) # 846 observations 
veh.dfsample <- veh.dftrain <- sample(nrow(veh.dfdataset), 0.7*veh.dfnobs) # 592 observations
veh.dfvalidate <- sample(setdiff(seq_len(nrow(veh.dfdataset)), veh.dftrain), 0.15*veh.dfnobs) # 126 observations
veh.dftest <- setdiff(setdiff(seq_len(nrow(veh.dfdataset)), veh.dftrain), veh.dfvalidate) # 128 observations

# The following variable selections have been noted.

veh.dfinput <- c("f0", "f1", "f2", "f3",
     "f4", "f5", "f6", "f7",
     "f8", "f9", "f10", "f11",
     "f12", "f13", "f14", "f15",
     "f16", "f17", "f18", "f19",
     "f20", "f21", "f22", "f23",
     "f24", "f25", "f26", "f27",
     "f28", "f29", "f30", "f31",
     "f32", "f33", "f34", "f35")

veh.dfnumeric <- c("f0", "f1", "f2", "f3",
     "f4", "f5", "f6", "f7",
     "f8", "f9", "f10", "f11",
     "f12", "f13", "f14", "f15",
     "f16", "f17", "f18", "f19",
     "f20", "f21", "f22", "f23",
     "f24", "f25", "f26", "f27",
     "f28", "f29", "f30", "f31",
     "f32", "f33", "f34", "f35")

veh.dfcategoric <- NULL

veh.dftarget  <- "CLASS"
veh.dfrisk    <- NULL
veh.dfident   <- NULL
veh.dfignore  <- NULL
veh.dfweights <- NULL

#============================================================
# Rattle timestamp: 2017-05-26 18:19:26 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

#============================================================
# Rattle timestamp: 2017-05-26 18:19:38 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

#============================================================
# Rattle timestamp: 2017-05-26 18:19:56 x86_64-w64-mingw32 

# Display a pairs plot for the selected variables. 

# Use GGally's ggpairs() to do the hard work.

library(GGally) # Provides ggpairs().
library(ggplot2) # Provides theme(), element_blank().

ggpairs(veh.dfdataset[veh.dfsample,],
        columns=c(1,2,3),
        colour="CLASS",
        diag=list(continuous="density",
                  discrete="bar"),
        upper=list(continuous="cor",
                   combo="box",
                   discrete="ratio"),
        lower=list(continuous="points",
                   combo="denstrip",
                   discrete="facetbar")) +
  theme(panel.grid.major=element_blank())

#============================================================
# Rattle timestamp: 2017-05-26 18:20:11 x86_64-w64-mingw32 

# Display a pairs plot for the selected variables. 

# Use GGally's ggpairs() to do the hard work.

library(GGally) # Provides ggpairs().
library(ggplot2) # Provides theme(), element_blank().

ggpairs(veh.dfdataset[veh.dfsample,],
        columns=c(1,2,3),
        colour="CLASS",
        diag=list(continuous="density",
                  discrete="bar"),
        upper=list(continuous="cor",
                   combo="box",
                   discrete="ratio"),
        lower=list(continuous="points",
                   combo="denstrip",
                   discrete="facetbar")) +
  theme(panel.grid.major=element_blank())

#============================================================
# Rattle timestamp: 2017-05-26 18:20:17 x86_64-w64-mingw32 

# Display a pairs plot for the selected variables. 

# Use GGally's ggpairs() to do the hard work.

library(GGally) # Provides ggpairs().
library(ggplot2) # Provides theme(), element_blank().

ggpairs(veh.dfdataset[veh.dfsample,],
        columns=c(1,2,3,37),
        colour="CLASS",
        diag=list(continuous="density",
                  discrete="bar"),
        upper=list(continuous="cor",
                   combo="box",
                   discrete="ratio"),
        lower=list(continuous="points",
                   combo="denstrip",
                   discrete="facetbar")) +
  theme(panel.grid.major=element_blank())

#============================================================
# Rattle timestamp: 2017-05-26 18:20:34 x86_64-w64-mingw32 

# The 'gplots' package provides the 'barplot2' function.

library(gplots, quietly=TRUE)

#============================================================
# Rattle timestamp: 2017-05-26 18:20:34 x86_64-w64-mingw32 

# Bar Plot 

# Generate the summary data for plotting.

ds <- rbind(summary(na.omit(veh.dfdataset[veh.dfsample,]$CLASS)),
    summary(na.omit(veh.dfdataset[veh.dfsample,][veh.dfdataset[veh.dfsample,]$CLASS=="car",]$CLASS)),
    summary(na.omit(veh.dfdataset[veh.dfsample,][veh.dfdataset[veh.dfsample,]$CLASS=="noncar",]$CLASS)))

#============================================================
# Rattle timestamp: 2017-05-26 18:21:08 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
veh.dfnobs <- nrow(veh.dfdataset) # 846 observations 
veh.dfsample <- veh.dftrain <- sample(nrow(veh.dfdataset), 0.7*veh.dfnobs) # 592 observations
veh.dfvalidate <- sample(setdiff(seq_len(nrow(veh.dfdataset)), veh.dftrain), 0.15*veh.dfnobs) # 126 observations
veh.dftest <- setdiff(setdiff(seq_len(nrow(veh.dfdataset)), veh.dftrain), veh.dfvalidate) # 128 observations

# The following variable selections have been noted.

veh.dfinput <- c("f0", "f1", "f2", "f3",
     "f4", "f5", "f6", "f7",
     "f8", "f9", "f10", "f11",
     "f12", "f13", "f14", "f15",
     "f16", "f17", "f18", "f19",
     "f20", "f21", "f22", "f23",
     "f24", "f25", "f26", "f27",
     "f28", "f29", "f30", "f31",
     "f32", "f33", "f34", "f35")

veh.dfnumeric <- c("f0", "f1", "f2", "f3",
     "f4", "f5", "f6", "f7",
     "f8", "f9", "f10", "f11",
     "f12", "f13", "f14", "f15",
     "f16", "f17", "f18", "f19",
     "f20", "f21", "f22", "f23",
     "f24", "f25", "f26", "f27",
     "f28", "f29", "f30", "f31",
     "f32", "f33", "f34", "f35")

veh.dfcategoric <- NULL

veh.dftarget  <- "CLASS"
veh.dfrisk    <- NULL
veh.dfident   <- NULL
veh.dfignore  <- NULL
veh.dfweights <- NULL

#============================================================
# Rattle timestamp: 2017-05-26 18:21:24 x86_64-w64-mingw32 

# The 'Hmisc' package provides the 'contents' function.

library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(veh.dfdataset[veh.dfsample, c(veh.dfinput, veh.dfrisk, veh.dftarget)])
summary(veh.dfdataset[veh.dfsample, c(veh.dfinput, veh.dfrisk, veh.dftarget)])

#============================================================
# Rattle timestamp: 2017-05-26 18:21:40 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

#============================================================
# Rattle timestamp: 2017-05-26 18:21:51 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for f0

library(dplyr) # Provides select().
library(ggplot2) # Provides ggplot(), aes(), geom_density(), xlab(), ggtitle(), labs().

# Generate the plot.

p01 <- crs %>%
  with(dataset[sample,]) %>%
  select(f0, CLASS) %>%
  ggplot(aes(x=f0)) +
  geom_density(lty=3) +
  geom_density(aes(fill=CLASS, colour=CLASS), alpha=0.55) +
  xlab("f0\n\nRattle 2017-May-26 18:21:51 RTimbro1") +
  ggtitle("Distribution of f0 (sample)\nby CLASS") +
  labs(fill="CLASS", y="Density")

# Use ggplot2 to generate histogram plot for f1

# Generate the plot.

p02 <- crs %>%
  with(dataset[sample,]) %>%
  select(f1, CLASS) %>%
  ggplot(aes(x=f1)) +
  geom_density(lty=3) +
  geom_density(aes(fill=CLASS, colour=CLASS), alpha=0.55) +
  xlab("f1\n\nRattle 2017-May-26 18:21:51 RTimbro1") +
  ggtitle("Distribution of f1 (sample)\nby CLASS") +
  labs(fill="CLASS", y="Density")

# Display the plots.

library(gridExtra) # Provides grid.arrange().

grid.arrange(p01, p02)

#============================================================
# Rattle timestamp: 2017-05-26 18:33:28 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for f0

library(dplyr) # Provides select().
library(ggplot2) # Provides ggplot(), aes(), geom_density(), xlab(), ggtitle(), labs().

# Generate the plot.

p01 <- crs %>%
  with(dataset[sample,]) %>%
  select(f0, CLASS) %>%
  ggplot(aes(x=f0)) +
  geom_density(lty=3) +
  geom_density(aes(fill=CLASS, colour=CLASS), alpha=0.55) +
  xlab("f0\n\nRattle 2017-May-26 18:33:28 RTimbro1") +
  ggtitle("Distribution of f0 (sample)\nby CLASS") +
  labs(fill="CLASS", y="Density")

# Use ggplot2 to generate histogram plot for f1

# Generate the plot.

p02 <- crs %>%
  with(dataset[sample,]) %>%
  select(f1, CLASS) %>%
  ggplot(aes(x=f1)) +
  geom_density(lty=3) +
  geom_density(aes(fill=CLASS, colour=CLASS), alpha=0.55) +
  xlab("f1\n\nRattle 2017-May-26 18:33:28 RTimbro1") +
  ggtitle("Distribution of f1 (sample)\nby CLASS") +
  labs(fill="CLASS", y="Density")

# Use ggplot2 to generate histogram plot for f2

# Generate the plot.

p03 <- crs %>%
  with(dataset[sample,]) %>%
  select(f2, CLASS) %>%
  ggplot(aes(x=f2)) +
  geom_density(lty=3) +
  geom_density(aes(fill=CLASS, colour=CLASS), alpha=0.55) +
  xlab("f2\n\nRattle 2017-May-26 18:33:28 RTimbro1") +
  ggtitle("Distribution of f2 (sample)\nby CLASS") +
  labs(fill="CLASS", y="Density")

# Use ggplot2 to generate histogram plot for f3

# Generate the plot.

p04 <- crs %>%
  with(dataset[sample,]) %>%
  select(f3, CLASS) %>%
  ggplot(aes(x=f3)) +
  geom_density(lty=3) +
  geom_density(aes(fill=CLASS, colour=CLASS), alpha=0.55) +
  xlab("f3\n\nRattle 2017-May-26 18:33:28 RTimbro1") +
  ggtitle("Distribution of f3 (sample)\nby CLASS") +
  labs(fill="CLASS", y="Density")

# Display the plots.

library(gridExtra) # Provides grid.arrange().

grid.arrange(p01, p02, p03, p04)

#============================================================
# Rattle timestamp: 2017-05-26 18:47:53 x86_64-w64-mingw32 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

veh.dfcor <- cor(veh.dfdataset[veh.dfsample, veh.dfnumeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

veh.dford <- order(veh.dfcor[1,])
veh.dfcor <- veh.dfcor[veh.dford, veh.dford]

# Display the actual correlations.

print(veh.dfcor)

# Graphically display the correlations.

corrplot(veh.dfcor, mar=c(0,0,1,0))
title(main="Correlation veh-prime.csv using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle timestamp: 2017-05-26 18:48:13 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for f0

library(dplyr) # Provides select().
library(ggplot2) # Provides ggplot(), aes(), geom_density(), xlab(), ggtitle(), labs().

# Generate the plot.

p01 <- crs %>%
  with(dataset[sample,]) %>%
  select(f0, CLASS) %>%
  ggplot(aes(x=f0)) +
  geom_density(lty=3) +
  geom_density(aes(fill=CLASS, colour=CLASS), alpha=0.55) +
  xlab("f0\n\nRattle 2017-May-26 18:48:13 RTimbro1") +
  ggtitle("Distribution of f0 (sample)\nby CLASS") +
  labs(fill="CLASS", y="Density")

# Use ggplot2 to generate histogram plot for f1

# Generate the plot.

p02 <- crs %>%
  with(dataset[sample,]) %>%
  select(f1, CLASS) %>%
  ggplot(aes(x=f1)) +
  geom_density(lty=3) +
  geom_density(aes(fill=CLASS, colour=CLASS), alpha=0.55) +
  xlab("f1\n\nRattle 2017-May-26 18:48:13 RTimbro1") +
  ggtitle("Distribution of f1 (sample)\nby CLASS") +
  labs(fill="CLASS", y="Density")

# Use ggplot2 to generate histogram plot for f2

# Generate the plot.

p03 <- crs %>%
  with(dataset[sample,]) %>%
  select(f2, CLASS) %>%
  ggplot(aes(x=f2)) +
  geom_density(lty=3) +
  geom_density(aes(fill=CLASS, colour=CLASS), alpha=0.55) +
  xlab("f2\n\nRattle 2017-May-26 18:48:13 RTimbro1") +
  ggtitle("Distribution of f2 (sample)\nby CLASS") +
  labs(fill="CLASS", y="Density")

# Use ggplot2 to generate histogram plot for f3

# Generate the plot.

p04 <- crs %>%
  with(dataset[sample,]) %>%
  select(f3, CLASS) %>%
  ggplot(aes(x=f3)) +
  geom_density(lty=3) +
  geom_density(aes(fill=CLASS, colour=CLASS), alpha=0.55) +
  xlab("f3\n\nRattle 2017-May-26 18:48:13 RTimbro1") +
  ggtitle("Distribution of f3 (sample)\nby CLASS") +
  labs(fill="CLASS", y="Density")

# Display the plots.

library(gridExtra) # Provides grid.arrange().

grid.arrange(p01, p02, p03, p04)

#============================================================
# Rattle timestamp: 2017-05-26 18:48:17 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for f0

library(dplyr) # Provides select().
library(ggplot2) # Provides ggplot(), aes(), geom_density(), xlab(), ggtitle(), labs().

# Generate the plot.

p01 <- crs %>%
  with(dataset[sample,]) %>%
  select(f0, CLASS) %>%
  ggplot(aes(x=f0)) +
  geom_density(lty=3) +
  geom_density(aes(fill=CLASS, colour=CLASS), alpha=0.55) +
  xlab("f0\n\nRattle 2017-May-26 18:48:17 RTimbro1") +
  ggtitle("Distribution of f0 (sample)\nby CLASS") +
  labs(fill="CLASS", y="Density")

# Use ggplot2 to generate histogram plot for f1

# Generate the plot.

p02 <- crs %>%
  with(dataset[sample,]) %>%
  select(f1, CLASS) %>%
  ggplot(aes(x=f1)) +
  geom_density(lty=3) +
  geom_density(aes(fill=CLASS, colour=CLASS), alpha=0.55) +
  xlab("f1\n\nRattle 2017-May-26 18:48:17 RTimbro1") +
  ggtitle("Distribution of f1 (sample)\nby CLASS") +
  labs(fill="CLASS", y="Density")

# Use ggplot2 to generate histogram plot for f2

# Generate the plot.

p03 <- crs %>%
  with(dataset[sample,]) %>%
  select(f2, CLASS) %>%
  ggplot(aes(x=f2)) +
  geom_density(lty=3) +
  geom_density(aes(fill=CLASS, colour=CLASS), alpha=0.55) +
  xlab("f2\n\nRattle 2017-May-26 18:48:18 RTimbro1") +
  ggtitle("Distribution of f2 (sample)\nby CLASS") +
  labs(fill="CLASS", y="Density")

# Use ggplot2 to generate histogram plot for f3

# Generate the plot.

p04 <- crs %>%
  with(dataset[sample,]) %>%
  select(f3, CLASS) %>%
  ggplot(aes(x=f3)) +
  geom_density(lty=3) +
  geom_density(aes(fill=CLASS, colour=CLASS), alpha=0.55) +
  xlab("f3\n\nRattle 2017-May-26 18:48:18 RTimbro1") +
  ggtitle("Distribution of f3 (sample)\nby CLASS") +
  labs(fill="CLASS", y="Density")

# Display the plots.

library(gridExtra) # Provides grid.arrange().

grid.arrange(p01, p02, p03, p04)

#============================================================
# Rattle timestamp: 2017-05-26 18:49:08 x86_64-w64-mingw32 

# Load the data.

veh.dfdataset <- read.csv("file:///C:/workspaces/uw_data_science/ds_450/R/assignment_5/veh-prime-trans.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

#============================================================
# Rattle timestamp: 2017-05-26 18:49:08 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
veh.dfnobs <- nrow(veh.dfdataset) # 846 observations 
veh.dfsample <- veh.dftrain <- sample(nrow(veh.dfdataset), 0.7*veh.dfnobs) # 592 observations
veh.dfvalidate <- sample(setdiff(seq_len(nrow(veh.dfdataset)), veh.dftrain), 0.15*veh.dfnobs) # 126 observations
veh.dftest <- setdiff(setdiff(seq_len(nrow(veh.dfdataset)), veh.dftrain), veh.dfvalidate) # 128 observations

# The following variable selections have been noted.

veh.dfinput <- c("f0", "f1", "f2", "f3",
     "f4", "f5", "f6", "f7",
     "f8", "f9", "f10", "f11",
     "f12", "f13", "f14", "f15",
     "f16", "f17", "f18", "f19",
     "f20", "f21", "f22", "f23",
     "f24", "f25", "f26", "f27",
     "f28", "f29", "f30", "f31",
     "f32", "f33", "f34", "f35")

veh.dfnumeric <- c("f0", "f1", "f2", "f3",
     "f4", "f5", "f6", "f7",
     "f8", "f9", "f10", "f11",
     "f12", "f13", "f14", "f15",
     "f16", "f17", "f18", "f19",
     "f20", "f21", "f22", "f23",
     "f24", "f25", "f26", "f27",
     "f28", "f29", "f30", "f31",
     "f32", "f33", "f34", "f35")

veh.dfcategoric <- NULL

veh.dftarget  <- "CLASS"
veh.dfrisk    <- NULL
veh.dfident   <- NULL
veh.dfignore  <- NULL
veh.dfweights <- NULL

#============================================================
# Rattle timestamp: 2017-05-26 18:49:34 x86_64-w64-mingw32 

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

veh.dfrpart <- rpart(CLASS ~ .,
    data=veh.dfdataset[veh.dftrain, c(veh.dfinput, veh.dftarget)],
    method="class",
    parms=list(split="information"),
    control=rpart.control(usesurrogate=0, 
        maxsurrogate=0))

# Generate a textual view of the Decision Tree model.

print(veh.dfrpart)
printcp(veh.dfrpart)
cat("\n")

# Time taken: 0.09 secs

#============================================================
# Rattle timestamp: 2017-05-26 18:50:25 x86_64-w64-mingw32 

# Plot the resulting Decision Tree. 

# We use the rpart.plot package.

fancyRpartPlot(veh.dfrpart, main="Decision Tree veh-prime-trans.csv $ CLASS")

#============================================================
# Rattle timestamp: 2017-05-26 18:52:11 x86_64-w64-mingw32 

# Random Forest 

# The 'randomForest' package provides the 'randomForest' function.

library(randomForest, quietly=TRUE)

# Build the Random Forest model.

set.seed(crv$seed)
veh.dfrf <- randomForest::randomForest(as.factor(CLASS) ~ .,
      data=veh.dfdataset[veh.dfsample,c(veh.dfinput, veh.dftarget)], 
      ntree=500,
      mtry=6,
      importance=TRUE,
      na.action=randomForest::na.roughfix,
      replace=FALSE)

# Generate textual output of 'Random Forest' model.

veh.dfrf

# The `pROC' package implements various AUC functions.

# Calculate the Area Under the Curve (AUC).

pROC::roc(veh.dfrf$y, as.numeric(veh.dfrf$predicted))

# Calculate the AUC Confidence Interval.

pROC::ci.auc(veh.dfrf$y, as.numeric(veh.dfrf$predicted))

# List the importance of the variables.

rn <- round(randomForest::importance(veh.dfrf), 2)
rn[order(rn[,3], decreasing=TRUE),]

# Time taken: 0.90 secs

#============================================================
# Rattle timestamp: 2017-05-26 18:53:00 x86_64-w64-mingw32 

# Ada Boost 

# The `ada' package implements the boost algorithm.

# Build the Ada Boost model.

set.seed(crv$seed)
veh.dfada <- ada::ada(CLASS ~ .,
                    data=veh.dfdataset[veh.dftrain,c(veh.dfinput, veh.dftarget)],
                    control=rpart::rpart.control(maxdepth=30,
                                                 cp=0.010000,
                                                 minsplit=20,
                                                 xval=10),
                    iter=50)

# Print the results of the modelling.

print(veh.dfada)
round(veh.dfada$model$errs[veh.dfada$iter,], 2)
cat('Variables actually used in tree construction:\n')
print(sort(names(listAdaVarsUsed(veh.dfada))))
cat('\nFrequency of variables actually used:\n')
print(listAdaVarsUsed(veh.dfada))

# Time taken: 1.53 secs

#============================================================
# Rattle timestamp: 2017-05-26 18:54:45 x86_64-w64-mingw32 

# Support vector machine. 

# The 'kernlab' package provides the 'ksvm' function.

library(kernlab, quietly=TRUE)

# Build a Support Vector Machine model.

set.seed(crv$seed)
veh.dfksvm <- ksvm(as.factor(CLASS) ~ .,
      data=veh.dfdataset[veh.dftrain,c(veh.dfinput, veh.dftarget)],
      kernel="rbfdot",
      prob.model=TRUE)

# Generate a textual view of the SVM model.

veh.dfksvm

# Time taken: 0.16 secs

#============================================================
# Rattle timestamp: 2017-05-26 18:55:16 x86_64-w64-mingw32 

# Evaluate model performance. 

# Generate an Error Matrix for the Decision Tree model.

# Obtain the response from the Decision Tree model.

veh.dfpr <- predict(veh.dfrpart, newdata=veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)], type="class")

# Generate the confusion matrix showing counts.

table(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)]$CLASS, veh.dfpr,
        useNA="ifany",
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)]$CLASS, veh.dfpr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

# Generate an Error Matrix for the Ada Boost model.

# Obtain the response from the Ada Boost model.

veh.dfpr <- predict(veh.dfada, newdata=veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])

# Generate the confusion matrix showing counts.

table(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)]$CLASS, veh.dfpr,
        useNA="ifany",
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)]$CLASS, veh.dfpr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

# Generate an Error Matrix for the Random Forest model.

# Obtain the response from the Random Forest model.

veh.dfpr <- predict(veh.dfrf, newdata=na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)]))

# Generate the confusion matrix showing counts.

table(na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS, veh.dfpr,
        useNA="ifany",
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS, veh.dfpr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)]))

# Generate the confusion matrix showing counts.

table(na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS, veh.dfpr,
        useNA="ifany",
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS, veh.dfpr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

#============================================================
# Rattle timestamp: 2017-05-26 18:56:19 x86_64-w64-mingw32 

# Evaluate model performance. 

# Risk Chart: requires the ggplot2 package.

library(ggplot2)

# Generate a risk chart.

# Rattle provides evaluateRisk() and riskchart().

veh.dfpr <- predict(veh.dfrpart, newdata=veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])[,2]
veh.dfeval <- evaluateRisk(veh.dfpr, veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)]$CLASS)
print(riskchart(veh.dfpr, 
                veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)]$CLASS, 
                title="Performance Chart Decision Tree veh-prime-trans.csv [validate] ", show.lift=TRUE, show.precision=TRUE, legend.horiz=FALSE))


# Risk Chart: requires the ggplot2 package.

library(ggplot2)

# Generate a risk chart.

# Rattle provides evaluateRisk() and riskchart().

veh.dfpr <- predict(veh.dfada, newdata=veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)], type="prob")[,2]
veh.dfeval <- evaluateRisk(veh.dfpr, veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)]$CLASS)
print(riskchart(veh.dfpr, 
                veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)]$CLASS, 
                title="Performance Chart Ada Boost veh-prime-trans.csv [validate] ", show.lift=TRUE, show.precision=TRUE, legend.horiz=FALSE))


# Risk Chart: requires the ggplot2 package.

library(ggplot2)

# Generate a risk chart.

# Rattle provides evaluateRisk() and riskchart().

veh.dfpr <- predict(veh.dfrf, newdata=na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)]), type="prob")[,2]
veh.dfeval <- evaluateRisk(veh.dfpr, na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS)
print(riskchart(veh.dfpr, 
                na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS, 
                title="Performance Chart Random Forest veh-prime-trans.csv [validate] ", show.lift=TRUE, show.precision=TRUE, legend.horiz=FALSE))


# Risk Chart: requires the ggplot2 package.

library(ggplot2)

# Generate a risk chart.

# Rattle provides evaluateRisk() and riskchart().

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)]), type="probabilities")[,2]
veh.dfeval <- evaluateRisk(veh.dfpr, na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS)
print(riskchart(veh.dfpr, 
                na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS, 
                title="Performance Chart SVM veh-prime-trans.csv [validate] ", show.lift=TRUE, show.precision=TRUE, legend.horiz=FALSE))


#============================================================
# Rattle timestamp: 2017-05-26 18:58:40 x86_64-w64-mingw32 

# Evaluate model performance. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the rpart model on veh-prime-trans.csv [test].

veh.dfpr <- predict(veh.dfrpart, newdata=veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Decision Tree veh-prime-trans.csv [test] CLASS")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
performance(pred, "auc")

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the ada model on veh-prime-trans.csv [test].

veh.dfpr <- predict(veh.dfada, newdata=veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Ada Boost veh-prime-trans.csv [test] CLASS")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
performance(pred, "auc")

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the rf model on veh-prime-trans.csv [test].

veh.dfpr <- predict(veh.dfrf, newdata=na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]), type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Random Forest veh-prime-trans.csv [test] CLASS")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
performance(pred, "auc")

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the ksvm model on veh-prime-trans.csv [test].

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]), type="probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve SVM veh-prime-trans.csv [test] CLASS")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
performance(pred, "auc")

#============================================================
# Rattle timestamp: 2017-05-26 18:59:05 x86_64-w64-mingw32 

# Evaluate model performance. 

# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the rpart model on veh-prime-trans.csv [test].

veh.dfpr <- predict(veh.dfrpart, newdata=veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#CC0000FF", lty=1, add=FALSE)


# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the ada model on veh-prime-trans.csv [test].

veh.dfpr <- predict(veh.dfada, newdata=veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#66CC00FF", lty=2, add=TRUE)


# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the rf model on veh-prime-trans.csv [test].

veh.dfpr <- predict(veh.dfrf, newdata=na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]), type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#00CCCCFF", lty=3, add=TRUE)


# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the ksvm model on veh-prime-trans.csv [test].

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]), type="probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#6600CCFF", lty=4, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("rpart","ada","rf","ksvm"), col=rainbow(4, 1, .8), lty=1:4, title="Models", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Precision/Recall Plot  veh-prime-trans.csv [test]",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#============================================================
# Rattle timestamp: 2017-05-26 18:59:25 x86_64-w64-mingw32 

# Evaluate model performance. 

# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for rpart model on veh-prime-trans.csv [test].

veh.dfpr <- predict(veh.dfrpart, newdata=veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#CC0000FF", lty=1, add=FALSE)


# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for ada model on veh-prime-trans.csv [test].

veh.dfpr <- predict(veh.dfada, newdata=veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#66CC00FF", lty=2, add=TRUE)


# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for rf model on veh-prime-trans.csv [test].

veh.dfpr <- predict(veh.dfrf, newdata=na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]), type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#00CCCCFF", lty=3, add=TRUE)


# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for ksvm model on veh-prime-trans.csv [test].

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]), type="probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#6600CCFF", lty=4, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("rpart","ada","rf","ksvm"), col=rainbow(4, 1, .8), lty=1:4, title="Models", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Sensitivity/Specificity (tpr/tnr)  veh-prime-trans.csv [test]",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#============================================================
# Rattle timestamp: 2017-05-26 18:59:52 x86_64-w64-mingw32 

# Evaluate model performance. 

# Lift Chart: requires the ROCR package.

library(ROCR)

# Obtain predictions for the rpart model on veh-prime-trans.csv [test].

veh.dfpr <- predict(veh.dfrpart, newdata=veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}

# Convert rate of positive predictions to percentage.

per <- performance(pred, "lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Plot the lift chart.
ROCR::plot(per, col="#CC0000FF", lty=1, xlab="Caseload (%)", add=FALSE)

# Lift Chart: requires the ROCR package.

library(ROCR)

# Obtain predictions for the ada model on veh-prime-trans.csv [test].

veh.dfpr <- predict(veh.dfada, newdata=veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}

# Convert rate of positive predictions to percentage.

per <- performance(pred, "lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Plot the lift chart.
ROCR::plot(per, col="#66CC00FF", lty=2, xlab="Caseload (%)", add=TRUE)

# Lift Chart: requires the ROCR package.

library(ROCR)

# Obtain predictions for the rf model on veh-prime-trans.csv [test].

veh.dfpr <- predict(veh.dfrf, newdata=na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]), type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}

# Convert rate of positive predictions to percentage.

per <- performance(pred, "lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Plot the lift chart.
ROCR::plot(per, col="#00CCCCFF", lty=3, xlab="Caseload (%)", add=TRUE)

# Lift Chart: requires the ROCR package.

library(ROCR)

# Obtain predictions for the ksvm model on veh-prime-trans.csv [test].

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]), type="probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}

# Convert rate of positive predictions to percentage.

per <- performance(pred, "lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Plot the lift chart.
ROCR::plot(per, col="#6600CCFF", lty=4, xlab="Caseload (%)", add=TRUE)

# Add a legend to the plot.

legend("topright", c("rpart","ada","rf","ksvm"), col=rainbow(4, 1, .8), lty=1:4, title="Models", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Lift Chart  veh-prime-trans.csv [test]",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#============================================================
# Rattle timestamp: 2017-05-26 19:00:02 x86_64-w64-mingw32 

# Evaluate model performance. 

# Cost Curve: requires the ROCR package.

library(ROCR)

# Generate a Cost Curve for the Decision Tree model on veh-prime-trans.csv [test].

veh.dfpr <- predict(veh.dfrpart, newdata=veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])[,2]
plot(0, 0, xlim=c(0, 1), ylim=c(0, 1), xlab="Probability cost function", ylab="Normalized expected cost")
lines(c(0,1),c(0,1))
lines(c(0,1),c(1,0))

# Remove observations with missing target.

no.miss   <- na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
perf1 <- performance(pred, "fpr", "fnr")
for (i in seq_along(perf1@x.values))
{
	for (j in seq_along(perf1@x.values[[i]]))
	{
		lines(c(0,1),c(perf1@y.values[[i]][j],
				perf1@x.values[[i]][j]),
				col=terrain.colors(10)[i],lty=3)
	}
}
perf<-performance(pred, "ecost")

# Bug in ROCR 1.0-3 does not obey the add command.
# Calling the function directly does work.

.plot.performance(perf, lwd=1.5, xlim=c(0,1), ylim=c(0,1), add=T)
op <- par(xpd=TRUE)
text(0, 1.07, "FPR")
text(1, 1.07, "FNR")
par(op)
text(0.12, 1, "Predict +ve")
text(0.88, 1, "Predict -ve")
title(main="Cost Curve Decision Tree veh-prime-trans.csv [test]",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

# Cost Curve: requires the ROCR package.

library(ROCR)

# Generate a Cost Curve for the Ada Boost model on veh-prime-trans.csv [test].

veh.dfpr <- predict(veh.dfada, newdata=veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)], type="prob")[,2]
plot(0, 0, xlim=c(0, 1), ylim=c(0, 1), xlab="Probability cost function", ylab="Normalized expected cost")
lines(c(0,1),c(0,1))
lines(c(0,1),c(1,0))

# Remove observations with missing target.

no.miss   <- na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
perf1 <- performance(pred, "fpr", "fnr")
for (i in seq_along(perf1@x.values))
{
	for (j in seq_along(perf1@x.values[[i]]))
	{
		lines(c(0,1),c(perf1@y.values[[i]][j],
				perf1@x.values[[i]][j]),
				col=terrain.colors(10)[i],lty=3)
	}
}
perf<-performance(pred, "ecost")

# Bug in ROCR 1.0-3 does not obey the add command.
# Calling the function directly does work.

.plot.performance(perf, lwd=1.5, xlim=c(0,1), ylim=c(0,1), add=T)
op <- par(xpd=TRUE)
text(0, 1.07, "FPR")
text(1, 1.07, "FNR")
par(op)
text(0.12, 1, "Predict +ve")
text(0.88, 1, "Predict -ve")
title(main="Cost Curve Ada Boost veh-prime-trans.csv [test]",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

# Cost Curve: requires the ROCR package.

library(ROCR)

# Generate a Cost Curve for the Random Forest model on veh-prime-trans.csv [test].

veh.dfpr <- predict(veh.dfrf, newdata=na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]), type="prob")[,2]
plot(0, 0, xlim=c(0, 1), ylim=c(0, 1), xlab="Probability cost function", ylab="Normalized expected cost")
lines(c(0,1),c(0,1))
lines(c(0,1),c(1,0))

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
perf1 <- performance(pred, "fpr", "fnr")
for (i in seq_along(perf1@x.values))
{
	for (j in seq_along(perf1@x.values[[i]]))
	{
		lines(c(0,1),c(perf1@y.values[[i]][j],
				perf1@x.values[[i]][j]),
				col=terrain.colors(10)[i],lty=3)
	}
}
perf<-performance(pred, "ecost")

# Bug in ROCR 1.0-3 does not obey the add command.
# Calling the function directly does work.

.plot.performance(perf, lwd=1.5, xlim=c(0,1), ylim=c(0,1), add=T)
op <- par(xpd=TRUE)
text(0, 1.07, "FPR")
text(1, 1.07, "FNR")
par(op)
text(0.12, 1, "Predict +ve")
text(0.88, 1, "Predict -ve")
title(main="Cost Curve Random Forest veh-prime-trans.csv [test]",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

# Cost Curve: requires the ROCR package.

library(ROCR)

# Generate a Cost Curve for the SVM model on veh-prime-trans.csv [test].

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]), type="probabilities")[,2]
plot(0, 0, xlim=c(0, 1), ylim=c(0, 1), xlab="Probability cost function", ylab="Normalized expected cost")
lines(c(0,1),c(0,1))
lines(c(0,1),c(1,0))

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
perf1 <- performance(pred, "fpr", "fnr")
for (i in seq_along(perf1@x.values))
{
	for (j in seq_along(perf1@x.values[[i]]))
	{
		lines(c(0,1),c(perf1@y.values[[i]][j],
				perf1@x.values[[i]][j]),
				col=terrain.colors(10)[i],lty=3)
	}
}
perf<-performance(pred, "ecost")

# Bug in ROCR 1.0-3 does not obey the add command.
# Calling the function directly does work.

.plot.performance(perf, lwd=1.5, xlim=c(0,1), ylim=c(0,1), add=T)
op <- par(xpd=TRUE)
text(0, 1.07, "FPR")
text(1, 1.07, "FNR")
par(op)
text(0.12, 1, "Predict +ve")
text(0.88, 1, "Predict -ve")
title(main="Cost Curve SVM veh-prime-trans.csv [test]",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle timestamp: 2017-05-26 19:00:21 x86_64-w64-mingw32 

# Evaluate model performance. 

# Cost Curve: requires the ROCR package.

library(ROCR)

# Generate a Cost Curve for the SVM model on veh-prime-trans.csv [validate].

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)]), type="probabilities")[,2]
plot(0, 0, xlim=c(0, 1), ylim=c(0, 1), xlab="Probability cost function", ylab="Normalized expected cost")
lines(c(0,1),c(0,1))
lines(c(0,1),c(1,0))

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
perf1 <- performance(pred, "fpr", "fnr")
for (i in seq_along(perf1@x.values))
{
	for (j in seq_along(perf1@x.values[[i]]))
	{
		lines(c(0,1),c(perf1@y.values[[i]][j],
				perf1@x.values[[i]][j]),
				col=terrain.colors(10)[i],lty=3)
	}
}
perf<-performance(pred, "ecost")

# Bug in ROCR 1.0-3 does not obey the add command.
# Calling the function directly does work.

.plot.performance(perf, lwd=1.5, xlim=c(0,1), ylim=c(0,1), add=T)
op <- par(xpd=TRUE)
text(0, 1.07, "FPR")
text(1, 1.07, "FNR")
par(op)
text(0.12, 1, "Predict +ve")
text(0.88, 1, "Predict -ve")
title(main="Cost Curve SVM veh-prime-trans.csv [validate]",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle timestamp: 2017-05-26 19:00:28 x86_64-w64-mingw32 

# Evaluate model performance. 

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)]))

# Generate the confusion matrix showing counts.

table(na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS, veh.dfpr,
        useNA="ifany",
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS, veh.dfpr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

#============================================================
# Rattle timestamp: 2017-05-26 19:00:44 x86_64-w64-mingw32 

# Score a dataset. 

#============================================================
# Rattle timestamp: 2017-05-26 19:02:18 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
veh.dfnobs <- nrow(veh.dfdataset) # 846 observations 
veh.dfsample <- veh.dftrain <- sample(nrow(veh.dfdataset), 0.7*veh.dfnobs) # 592 observations
veh.dfvalidate <- sample(setdiff(seq_len(nrow(veh.dfdataset)), veh.dftrain), 0.15*veh.dfnobs) # 126 observations
veh.dftest <- setdiff(setdiff(seq_len(nrow(veh.dfdataset)), veh.dftrain), veh.dfvalidate) # 128 observations

# The following variable selections have been noted.

veh.dfinput <- c("f0", "f1", "f2", "f3",
     "f4", "f5", "f6", "f7",
     "f8", "f9", "f10", "f11",
     "f12", "f13", "f14", "f15",
     "f16", "f17", "f18", "f19",
     "f20", "f21", "f22", "f23",
     "f24", "f25", "f26", "f27",
     "f28", "f29", "f30", "f31",
     "f32", "f33", "f34", "f35")

veh.dfnumeric <- c("f0", "f1", "f2", "f3",
     "f4", "f5", "f6", "f7",
     "f8", "f9", "f10", "f11",
     "f12", "f13", "f14", "f15",
     "f16", "f17", "f18", "f19",
     "f20", "f21", "f22", "f23",
     "f24", "f25", "f26", "f27",
     "f28", "f29", "f30", "f31",
     "f32", "f33", "f34", "f35")

veh.dfcategoric <- NULL

veh.dftarget  <- "CLASS"
veh.dfrisk    <- NULL
veh.dfident   <- NULL
veh.dfignore  <- NULL
veh.dfweights <- NULL

#============================================================
# Rattle timestamp: 2017-05-26 19:02:39 x86_64-w64-mingw32 

# Support vector machine. 

# The 'kernlab' package provides the 'ksvm' function.

library(kernlab, quietly=TRUE)

# Build a Support Vector Machine model.

set.seed(crv$seed)
veh.dfksvm <- ksvm(as.factor(CLASS) ~ .,
      data=veh.dfdataset[veh.dftrain,c(veh.dfinput, veh.dftarget)],
      kernel="rbfdot",
      prob.model=TRUE)

# Generate a textual view of the SVM model.

veh.dfksvm

# Time taken: 0.18 secs

#============================================================
# Rattle timestamp: 2017-05-26 19:05:23 x86_64-w64-mingw32 

# Evaluate model performance. 

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)]))

# Generate the confusion matrix showing counts.

table(na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS, veh.dfpr,
        useNA="ifany",
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS, veh.dfpr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

#============================================================
# Rattle timestamp: 2017-05-26 19:05:49 x86_64-w64-mingw32 

# Evaluate model performance. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the ksvm model on veh-prime-trans.csv [validate].

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)]), type="probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve SVM veh-prime-trans.csv [validate] CLASS")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
performance(pred, "auc")

#============================================================
# Rattle timestamp: 2017-05-26 19:06:24 x86_64-w64-mingw32 

# Evaluate model performance. 

# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the ksvm model on veh-prime-trans.csv [validate].

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)]), type="probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#CC0000FF", lty=1, add=FALSE)


# Add a legend to the plot.

legend("bottomleft", c("ksvm"), col=rainbow(1, 1, .8), lty=1:1, title="Models", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Precision/Recall Plot  veh-prime-trans.csv [validate]",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#============================================================
# Rattle timestamp: 2017-05-26 19:06:44 x86_64-w64-mingw32 

# Evaluate model performance. 

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]))

# Generate the confusion matrix showing counts.

table(na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])$CLASS, veh.dfpr,
        useNA="ifany",
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])$CLASS, veh.dfpr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

#============================================================
# Rattle timestamp: 2017-05-26 19:06:53 x86_64-w64-mingw32 

# Evaluate model performance. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the ksvm model on veh-prime-trans.csv [test].

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]), type="probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve SVM veh-prime-trans.csv [test] CLASS")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
performance(pred, "auc")

#============================================================
# Rattle timestamp: 2017-05-26 19:07:01 x86_64-w64-mingw32 

# Evaluate model performance. 

# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the ksvm model on veh-prime-trans.csv [test].

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]), type="probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#CC0000FF", lty=1, add=FALSE)


# Generate a Precision/Recall Plot for the ksvm model on veh-prime-trans.csv [train].

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]), type="probabilities")[,2]

# In ROCR (1.0-3) plot does not obey the add command.
# Calling the function directly (.plot.performance) does work.

.plot.performance(performance(prediction(veh.dfpr, na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])$CLASS),"prec", "rec"), col="#00CCCCFF", lty=2, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="ksvm", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Precision/Recall Plot  veh-prime-trans.csv ",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#============================================================
# Rattle timestamp: 2017-05-26 19:07:46 x86_64-w64-mingw32 

# Support vector machine. 

# The 'kernlab' package provides the 'ksvm' function.

library(kernlab, quietly=TRUE)

# Build a Support Vector Machine model.

set.seed(crv$seed)
veh.dfksvm <- ksvm(as.factor(CLASS) ~ .,
      data=veh.dfdataset[veh.dftrain,c(veh.dfinput, veh.dftarget)],
      kernel="polydot",
      kpar=list("degree"=1),
      prob.model=TRUE)

# Generate a textual view of the SVM model.

veh.dfksvm

# Time taken: 0.12 secs

#============================================================
# Rattle timestamp: 2017-05-26 19:08:00 x86_64-w64-mingw32 

# Evaluate model performance. 

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)]))

# Generate the confusion matrix showing counts.

table(na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS, veh.dfpr,
        useNA="ifany",
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS, veh.dfpr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

#============================================================
# Rattle timestamp: 2017-05-26 19:08:16 x86_64-w64-mingw32 

# Evaluate model performance. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the ksvm model on veh-prime-trans.csv [validate].

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)]), type="probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve SVM veh-prime-trans.csv [validate] CLASS")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
performance(pred, "auc")

#============================================================
# Rattle timestamp: 2017-05-26 19:08:22 x86_64-w64-mingw32 

# Evaluate model performance. 

# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the ksvm model on veh-prime-trans.csv [validate].

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)]), type="probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#CC0000FF", lty=1, add=FALSE)


# Add a legend to the plot.

legend("bottomleft", c("ksvm"), col=rainbow(1, 1, .8), lty=1:1, title="Models", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Precision/Recall Plot  veh-prime-trans.csv [validate]",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#============================================================
# Rattle timestamp: 2017-05-26 19:09:10 x86_64-w64-mingw32 

# Support vector machine. 

# The 'kernlab' package provides the 'ksvm' function.

library(kernlab, quietly=TRUE)

# Build a Support Vector Machine model.

set.seed(crv$seed)
veh.dfksvm <- ksvm(as.factor(CLASS) ~ .,
      data=veh.dfdataset[veh.dftrain,c(veh.dfinput, veh.dftarget)],
      kernel="anovadot",
      prob.model=TRUE)

# Generate a textual view of the SVM model.

veh.dfksvm

# Time taken: 1.51 secs

#============================================================
# Rattle timestamp: 2017-05-26 19:09:20 x86_64-w64-mingw32 

# Evaluate model performance. 

# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the ksvm model on veh-prime-trans.csv [validate].

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)]), type="probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#CC0000FF", lty=1, add=FALSE)


# Add a legend to the plot.

legend("bottomleft", c("ksvm"), col=rainbow(1, 1, .8), lty=1:1, title="Models", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Precision/Recall Plot  veh-prime-trans.csv [validate]",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#============================================================
# Rattle timestamp: 2017-05-26 19:09:29 x86_64-w64-mingw32 

# Evaluate model performance. 

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)]))

# Generate the confusion matrix showing counts.

table(na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS, veh.dfpr,
        useNA="ifany",
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS, veh.dfpr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

#============================================================
# Rattle timestamp: 2017-05-26 19:09:36 x86_64-w64-mingw32 

# Evaluate model performance. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the ksvm model on veh-prime-trans.csv [validate].

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)]), type="probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve SVM veh-prime-trans.csv [validate] CLASS")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dfvalidate, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
performance(pred, "auc")

#============================================================
# Rattle timestamp: 2017-05-26 19:09:59 x86_64-w64-mingw32 

# Evaluate model performance. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the ksvm model on veh-prime-trans.csv [test].

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]), type="probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve SVM veh-prime-trans.csv [test] CLASS")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
performance(pred, "auc")

#============================================================
# Rattle timestamp: 2017-05-26 19:10:04 x86_64-w64-mingw32 

# Evaluate model performance. 

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]))

# Generate the confusion matrix showing counts.

table(na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])$CLASS, veh.dfpr,
        useNA="ifany",
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])$CLASS, veh.dfpr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

#============================================================
# Rattle timestamp: 2017-05-26 19:10:10 x86_64-w64-mingw32 

# Evaluate model performance. 

# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the ksvm model on veh-prime-trans.csv [test].

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]), type="probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])$CLASS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(veh.dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(veh.dfpr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#CC0000FF", lty=1, add=FALSE)


# Generate a Precision/Recall Plot for the ksvm model on veh-prime-trans.csv [train].

veh.dfpr <- kernlab::predict(veh.dfksvm, newdata=na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)]), type="probabilities")[,2]

# In ROCR (1.0-3) plot does not obey the add command.
# Calling the function directly (.plot.performance) does work.

.plot.performance(performance(prediction(veh.dfpr, na.omit(veh.dfdataset[veh.dftest, c(veh.dfinput, veh.dftarget)])$CLASS),"prec", "rec"), col="#00CCCCFF", lty=2, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="ksvm", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Precision/Recall Plot  veh-prime-trans.csv ",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#============================================================
# Rattle timestamp: 2017-05-26 19:10:49 x86_64-w64-mingw32 

# Principal Components Analysis (on numerics only).

pc <- prcomp(na.omit(veh.dfdataset[veh.dfsample, veh.dfnumeric]), scale=TRUE, center=TRUE, tol=0)

# Show the output of the analysis.

pc

# Summarise the importance of the components found.

summary(pc)

# Display a plot showing the relative importance of the components.

plot(pc, main="")
title(main="Principal Components Importance veh-prime-trans.csv",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
axis(1, at=seq(0.7, ncol(pc$rotation)*1.2, 1.2), labels=colnames(pc$rotation), lty=0)

# Display a plot showing the two most principal components.

biplot(pc, main="")
title(main="Principal Components veh-prime-trans.csv",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle timestamp: 2017-05-26 19:11:11 x86_64-w64-mingw32 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

veh.dfcor <- cor(veh.dfdataset[veh.dfsample, veh.dfnumeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

veh.dford <- order(veh.dfcor[1,])
veh.dfcor <- veh.dfcor[veh.dford, veh.dford]

# Display the actual correlations.

print(veh.dfcor)

# Graphically display the correlations.

opar <- par(cex=0.5)
corrplot(veh.dfcor, mar=c(0,0,1,0))
title(main="Correlation veh-prime-trans.csv using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
par(opar)

#============================================================
# Rattle timestamp: 2017-05-26 19:11:57 x86_64-w64-mingw32 

# Save the project data (variable crs) to file.

save(crs, file="C:\workspaces\uw_data_science\ds_450\R\assignment_5\veh-prime-trans.rattle", compress=TRUE)
