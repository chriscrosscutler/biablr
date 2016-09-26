library(readr)
library(ggplot2)
library(dplyr)

#Change to directory where the BIABL_scripts.r and your data is
directory <- "~/Desktop"
functionfile <- paste(directory, "/BIABL_scripts.r", sep="")

source(functionfile)
setwd(directory)

#Import data
raw_data <- read_csv("~/Desktop/Repeatability_Dehydration_data.csv", col_names= TRUE)
raw_data <- na.omit(raw_data)

# Compute Intracranial Volume from all the ROI volumes - CSF Volume
raw_data$icv <- rowSums(raw_data[9:102])
# Add column with average adult brain volume
raw_data$Avg_Volume = 1500000

# Normalize all the data by the average adult brain volume
# volume at each ROI / icv * Avg_Volume

raw_data[,9:102] <- (raw_data[, 9:102] / raw_data$icv) * raw_data$Avg_Volume

#convert several columns from numeric to factors
raw_data$time_of_day <- as.factor(raw_data$time_of_day)
raw_data$Male0_Female1 <- as.factor(raw_data$Male0_Female1)
raw_data$Hyd1_Dehydra0 <- as.factor(raw_data$Hyd1_Dehydra0) 
raw_data$HighEstrogen1_Low0_Male2 <- as.factor(raw_data$HighEstrogen1_Low0_Male2)

#LINEAR REGRESSION
########################
#ROIs vs Time of Day

x = 0
pdf("ROIs_vs_AcquisitionTime_lm(MF).pdf")
attach(raw_data)
for (i in names(raw_data)){
  if(x > 9){
    fit <- lm(as.formula(paste(i, "~ AcquisitionTimes")), data = raw_data)
    print(ggplotRegression(fit))
  }
  x = x + 1
}
dev.off()

#ANOVAS
#Both Males and Females
###############################
#ROIs vs Time of Day
#loop through all ROIs of brain and output anova results to out dataframe
x = 0
pdf("ROIs vs Time of Day(MF).pdf")
for (i in names(raw_data)){
  if(x > 9){
    test <- aov(as.formula(paste(i, "~ time_of_day")), data = raw_data)
    print(ggplotANOVA(test))
  }
  x = x+1
}
dev.off()

#Hydrated Vs Dehydrated
x = 0
pdf("ROIs vs Hydration(MF).pdf")
for (i in names(raw_data)){
  if(x > 9){
    test <- aov(as.formula(paste(i, "~ Hyd1_Dehydra0")), data = raw_data)
    print(ggplotANOVA(test))
  }
  x = x+1
}
dev.off()
detach()


#Males
############################
males <- filter(raw_data, Male0_Female1 == 0)

#Linear Regression
x = 0
pdf("ROIs_vs_AcquisitionTime_lm(M).pdf")
attach(males)
for (i in names(males)){
  if(x > 9){
    fit <- lm(as.formula(paste(i, "~ AcquisitionTimes")), data = males)
    print(ggplotRegression(fit))
  }
  x = x + 1
}
dev.off()

#Hydrated Vs Dehydrated
x = 0
pdf("ROIs vs Hydration(M).pdf")
for (i in names(males)){
  if(x > 9){
    test <- aov(as.formula(paste(i, "~ Hyd1_Dehydra0")), data = males)
    print(ggplotANOVA(test))
  }
  x = x+1
}
dev.off()

#ROIs vs Time of Day
x = 0
pdf("ROIs vs Time of Day(M).pdf")
for (i in names(males)){
  if(x > 9){
    test <- aov(as.formula(paste(i, "~ time_of_day")), data = males)
    print(ggplotANOVA(test))
  }
  x = x+1
}
dev.off()
detach()


#Females
############################
females <- filter(raw_data, Male0_Female1 == 1)

#Linear Regression
x = 0
pdf("ROIs_vs_AcquisitionTime_lm(F).pdf")
attach(females)
for (i in names(females)){
  if(x > 9){
    fit <- lm(as.formula(paste(i, "~ AcquisitionTimes")), data = females)
    print(ggplotRegression(fit))
  }
  x = x + 1
}
dev.off()

#Lh vs Non-Lh
x = 0
pdf("ROIs vs Lh(F).pdf")
for (i in names(females)){
  if(x > 9){
    test <- aov(as.formula(paste(i, "~ HighEstrogen1_Low0_Male2")), data = females)
    print(ggplotANOVA(test))
  }
  x = x+1
}
dev.off()

#Hydrated Vs Dehydrated
x = 0
pdf("ROIs vs Hydration(F).pdf")
for (i in names(females)){
  if(x > 9){
    test <- aov(as.formula(paste(i, "~ Hyd1_Dehydra0")), data = females)
    print(ggplotANOVA(test))
  }
  x = x+1
}
dev.off()

#ROIs vs Time of Day
x = 0
pdf("ROIs vs Time of Day(F).pdf")
for (i in names(females)){
  if(x > 9){
    test <- aov(as.formula(paste(i, "~ time_of_day")), data = females)
    print(ggplotANOVA(test))
  }
  x = x+1
}
dev.off()
detach()

#Males vs Females
############################

#Variability
############################

  #Within Females
  ############################
  sub3f <- filter(raw_data, subj == 3)

  sub8f <- filter(raw_data, subj == 8)

  sub9f <- filter(raw_data, subj == 9)

  #Within Males
  ############################
  sub1m <- filter(raw_data, subj == 1)

  sub2m <- filter(raw_data, subj == 2)

  sub4m <- filter(raw_data, subj == 4)

  sub5m <- filter(raw_data, subj == 5)

  sub6m <- filter(raw_data, subj == 6)

  sub7m <- filter(raw_data, subj == 7)





#Generate dataframe with summary statistics for each ROI
sumdata <- t(as.matrix(summary(raw_data))) # convert to matrix
write.csv(t(as.matrix(summary(raw_data))), file="summary_raw_data.csv") # write to csv
raw_sum_data <- read_csv("~/Desktop/summary_raw_data.csv", col_names= FALSE) # import csv
raw_sum_data <- na.omit(raw_sum_data) # omit NA values
colnames(raw_sum_data) <- c("ROI", "Min", "First_Qu", "Median", "Mean", "Third_Qu", "Max") # assign column names
raw_sum_data <- raw_sum_data[4:99, ] # remove extra columns 
# remove strings from each cell in dataframe
raw_sum_data$Min <- gsub("Min.   : ", '', raw_sum_data$Min)
raw_sum_data$Min <- gsub("Min.   :", '', raw_sum_data$Min)
raw_sum_data$First_Qu <- gsub("1st Qu.: ", '', raw_sum_data$First_Qu)
raw_sum_data$First_Qu <- gsub("1st Qu.:", '', raw_sum_data$First_Qu)
raw_sum_data$Median <- gsub("Median :", '', raw_sum_data$Median)
raw_sum_data$Mean <- gsub("Mean   :", '', raw_sum_data$Mean)
raw_sum_data$Third_Qu <- gsub("3rd Qu.:", '', raw_sum_data$Third_Qu)
raw_sum_data$Max <- gsub("Max.   :", '', raw_sum_data$Max)

#convert string to Numbers
raw_sum_data$Min <- as.numeric(raw_sum_data$Min)
raw_sum_data$First_Qu <- as.numeric(raw_sum_data$First_Qu)
raw_sum_data$Median <- as.numeric(raw_sum_data$Median)
raw_sum_data$Mean <- as.numeric(raw_sum_data$Mean)
raw_sum_data$Third_Qu <- as.numeric(raw_sum_data$Third_Qu)
raw_sum_data$Max <- as.numeric(raw_sum_data$Max)

#add new empty columns to dataframe
raw_sum_data$Diff <- NA
raw_sum_data$Percent_Diff <- NA
raw_sum_data$Confidence_Interval_left<- NA
raw_sum_data$Confidence_Interval_right<- NA


# calculate diff, percent_diff, and Confience Intervals for each ROI in dataframe
#Diff
raw_sum_data$Diff <- raw_sum_data$Max - raw_sum_data$Min

#%Diff
raw_sum_data$Percent_Diff <- (raw_sum_data$Diff / raw_sum_data$Min) * 100 

#Confidence Intervals
x = 0
for (i in names(raw_data)){
  if(x > 7){
      print(i)
      error <- qt(0.975, df = length(raw_data[[i]]) - 1) * sd(raw_data[[i]]) / sqrt(length(raw_data[[i]]))
      left <- mean(raw_data[[i]]) - error
      right <- mean(raw_data[[i]]) + error
      raw_sum_data[i, "Confidence_Interval_left"] <- left
      raw_sum_data[i, "Confidence_Interval_right"] <- right
  }
  x <- x + 1
}

#save dataframe to csv
write.csv(raw_sum_data, file="summary_stats_all_subjects.csv") 

# read new dataframe
raw_sum_data_fixed <- read_csv("~/Desktop/summary_stats_all_subjects.csv", col_names= FALSE) # import csv

#plotting the confience intervals
F <- seq(1:1000000)

ggplot(raw_sum_data_fixed, aes(x = raw_sum_data_fixed$ROI, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = raw_sum_data_fixed$Confidence_Interval_right, ymin = raw_sum_data_fixed$Confidence_Interval_left)) +
  ylab("Volume") + 
  xlab("ROI") 







