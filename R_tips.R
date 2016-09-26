#To do:
#------------------------------------
# get rid of NA values (we need to figure out the best way to do this)
# 		-maybe we use the average of all the values from the electrodes on that row? or the mode.. I guess we'll have to try a bunch to see what 
# remove stimulus type from training data
# normalize stimulus type to 0 (faces) or 1 (houses)
# missmap() will show where I am missing data
# use k-fold cross validation to split data into training and test data
# try logarithmic regression
# feature selection !!!!!!
# 		http://machinelearningmastery.com/feature-selection-with-the-caret-r-package/
# 		This is the most important thing we can do. We need to find the features (variables) that give us the most accurate results
# 		A lot of the best features are hidden in the data so we have to figure out what those are and then create a new feature based on that data that we can then use in our model
# get feature importance (which variables affect our model the most)



#TIPS
#-------------------------------------
# I would use rstudio for everything because of how integrated it is for R programming
# after you set a variable (eg data) and you perform an function on it (data <- read_csv()) type data to look at that variable
# by typing ? before any function it will tell you the parameters needed and everythign else about a function (?read_csv)
# ?? will search all of R for a function (useful if it's not working or you forgot what library to load to use the fuction)

# Websites for reference
#------------------------------------
#https://rpubs.com/flyingdisc/practical-machine-learning-xgboost
#https://github.com/dmlc/xgboost/tree/master/demo#features-walkthrough
#https://github.com/dmlc/xgboost/blob/master/R-package/demo/basic_walkthrough.R
#http://www.r-bloggers.com/an-introduction-to-xgboost-r-package/
#http://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
#https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf


# Packages needed so far
#-----------------------------------
# install all of these using install.packages("x") where x would be the name of the package below ie "readr"
# make sure you have the latest build of R https://cran.cnr.berkeley.edu/


require(readr) #import
require(caret) #preprocessing for machine learning use this to install correctly: install.packages("caret", dependencies = c("Depends", "Suggests"))
require(corrplot)
require(Rtsne)
require(xgboost) #machine learning package
require(stats)
require(knitr)
require(e1071) # more machine learning
require(dplyr) #this is great for removing columns, selecting and adding rows or columns too. Google dplyr cheat sheet for a great guide
require(ggplot2) #plotting
require(Amelia) #use missmap() to see missing values in data
knitr::opts_chunk$set(cache=TRUE)

#read data in using the fast readr package
raw_data <- read_csv(file = "~/Desktop/mlcomp/ecog_train_with_labels.csv", col_names = TRUE)
#look at overall data in raw data to see types and ranges
#we will need to convert or remove all non-numeric data becuase a lot of machine learning algorithms only allow numeric values
str(raw_data)
#look at first couple of rows
head(raw_data)
#look at last couple of rows
tail(raw_data)

#check for variance in the data (looks at each column to see its variance)
zero.var = nearZeroVar(raw_data, saveMetrics=TRUE)
print(zero.var)
#correlation matrix plot only accepts all numeric values
cplot <- corrplot.mixed(cor(raw_data), lower = "circle", upper = "color", tl.pos = "lt", diag = "n", order = "hclust", hclust.method = "complete")
cplot

corrMatrix <- cor(raw_data[,2:65])
# summarize the correlation matrix
print(corrMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorr <- findCorrelation(corrMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorr)
# t-Distributed Stochastic Neighbor Embedding helps us see any clustering patterns in the data to see if we can separate it that way
tsne <- Rtsne(as.matrix(raw_data), check_duplicates = FALSE, pca = TRUE, perplexity = 30, theta = 0.5, dims = 2)
tsne

# how to split dataset
#######
set.seed(1)
# Shuffle the dataset, call the result shuffled
n <- nrow(titanic)
shuffled <- titanic[sample(n), ]
# Split the data in train and test
train_indices <- 1:round(.7 * n)
train <- shuffled[train_indices, ]
test_indices <- (round(.7 * n) + 1):n
test <- shuffled[test_indices, ]
#########


#create new variable that will be our training data
training_data <- raw_data
#turn training data into dataframe
training_data <- as.data.frame(training_data)
#randomly extracts x numer of rows from the raw_data to be used to test our model
test_data <- sample_n(raw_data, 30, replace = TRUE)
#turn test data into dataframe
test_data <- as.data.frame(test_data)



#unused...
col_names <- colnames(data)
training_portion <- 3/4 # Taking the first 75% stimulus presentation cycles as the training data, for each patient
unique_patients <- unique(data[,1]) # get the list of unique patient ID
num_patients <- length(unique_patients) # get the number of unique patients
num_cols <- ncol(data)
dataset_train <- NULL
dataset_valid <- NULL

