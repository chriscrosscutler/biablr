
# Useful functions for the Brain Imaging and Behavior Lab at BYU
# Professor: Dr. Erin Bigler


# Normalize all the data by the average adult brain volume
# volume at each ROI / icv * Avg_Volume
# Variables:
# arg1- ROI
# arg2- icv 
# ex:
# normalize(right_hippocampus, 876590)

normalize <- function(arg1, arg2) {
   x <- arg1 / arg2
   y <- x * 1500000.00
}

# Pulls the data out of a linear regression
# Returns important values (R-squares, slope, intercept and P value) 
# At the top of a ggplot graph with the regression line.
# Variables:
# fit- a Linear Regression model
# ex:
# fit <- lm(Sepal.Length ~ Petal.Width, data = iris)
# ggplotRegression(fit)

ggplotRegression <- function (fit) {

require(ggplot2)

ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  ggtitle(paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                     "Intercept =",signif(fit$coef[[1]],5 ),
                     " Slope =",signif(fit$coef[[2]], 5),
                     " P =",signif(summary(fit)$coef[2,4], 5)))

}

# Pulls the data out of a ANOVA
# Returns important values (P value, F value, Sum Squares, Mean Squares) 
# At the top of a ggplot boxplot
# Variables:
# fit- an ANOVA model
# ex:
# fit <- aov(Sepal.Length ~ Petal.Width, data = iris)
# ggplotANOVA(fit)


ggplotANOVA <- function (fit) {
  
print("function ggplotANOVA")
print(fit)
require(ggplot2)

ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
  geom_boxplot() +
  ggtitle(paste("P Value = ", summary(fit)[[1]][["Pr(>F)"]][1], 
  				      "F Value =", summary(fit)[[1]][["F value"]][1],
                "Mean Sq =",signif(summary(fit)[[1]][,3][1], 5),
                "Sum Sq =",signif(summary(fit)[[1]][,2][1], 5)))
}
 


# Converts specific columns of your dataframe to factors to be used in ANOVA
# Returns the dataframe
# Variables:
# dataframe- your dataframe
# change- a vector of columns that need to be changed to factors
# ex: 
# to_change <- c(1, 4, 5)
# raw_data <- factorize(raw_data, to_change)

factorize <- function(dataframe, change){

  for (i in change){
    dataframe[[i]] <- as.factor(dataframe[[i]])
  }
  return dataframe
}



# Performs ANOVA or Linear Regression over all columns of a dataframe against a chosen varible. 
# Saves plots to pdf with key stats about each plot (see ggplotANOVA or Regression)
# Variables:
# dataframe- your data
# var- the variable you are looking at
# type- 1= ANOVA, 2= Linear Regression
# ex:
# multiplot(my_data, rainfall, 1)


multiplot <- function(dataframe, var, type){
  x = 0
  pdf(paste(var,".pdf", sep=""))
  if (type == 1){ # ANOVA
    for (i in names(dataframe)){
      if(x > 9){
        test <- aov(paste(i, "~", var), data = dataframe)
      }
      x = x+1
    }
  }
  else if (type == 2){ # Linear Regression
    for (i in names(dataframe)){
      if(x > 9){
        fit <- lm(paste(i, "~", var), data = dataframe)
        print(ggplotRegression(fit))
      }
      x = x + 1
    }
  }
  dev.off()
}
 



sapply(X, function(my) {
  f <- as.formula(paste("dat~contrasts*",my,"+Error(ID/(contrasts))"))
  summary(aov(f, data=set))
}, simplify=FALSE)






