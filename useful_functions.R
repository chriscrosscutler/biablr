sort()
print()
identical()
seq(start, end, by= ) #generate sequence
rep(object, times =) #replicate sequence  if times = 2 then:43214321 if each then 44332211
is.*() #(eg is.li(x) = TRUE if x is a list)

unlist(x) # turns list into vector

append() #adds to a datastructure
rev() # reverses a datastructure

grepl(pattern = <regex>, x = <string>) # use to find paterns in string data returns a vector of TRUE or FALSE for matches

^ #start of x in regexn eg ^a would search for everything that starts with an a
$ # end of a line eg a$ would find all words that end with an a
| # or in regex
grep() # returns vector of ints of locations where the pattern was matched
which(x) #returns vector of indicies where conditions x was met eg which(grepl(pattern = "^a", x = animals))

sub(pattern = <regex>, replacement = <string> x = <string data>) # only replaces first instance of pattern in string eg impala would become impola if you subbed o for a
gsub() # subs all matches in data

#you can use these match finders to subset your data to just include the columns where grep() returned true

library(tidyr)

str_trim() #removes all leading and trailing whitespace

str_detect(data, <string>) # find a string in your data
str_replace(data, <string find>, <replace>) # find and replace it

library(mice) #helps replace missing values in data
#see this post for more info:
#http://www.r-bloggers.com/imputing-missing-data-with-r-mice-package/

library(dplyr)
select(df, columns) # creates a new dataframe structure with only the columns you want

names(data) <- sub(" ", "_", names(data)) # changes column names in a data frame from "Column One" to "Column_One"
										  # must incert twice in code if column names have more than one space "Avg Student Age" to convert the second space to a underscore

# Linear Regression
fit <- lm(y ~ x1 + x2 + x3, data=mydata)
summary(fit) # show results
confint(fit, level=0.95) # Confidence Intervals for model parameters 

#check the length of each column to make sure they are the same length
sapply(df, function(x) sum(complete.cases(x)))

#parallel processing
library(multicore)


###########################################
#extracting values from ANOVA Summary
#pvalue 
pval <- summary(test)[[1]][,5][1]
#or
pv <- s[[1]][,5][1]
#or
summary(test)[[1]][["Pr(>F)"]][1]

#f value
fval <- summary(test)[[1]][,4][1]
#or 
summary(test)[[1]][["F value"]][1]

#Mean sq
meanval <- summary(test)[[1]][,3][1]
#Sum sq
sumval <- summary(test)[[1]][,2][1]


#auto update R on Mac
require(devtools)
install_github('andreacirilloac/updateR')
library(updateR)
updateR(admin_password = "password")

#turns string into formula
as.formula("y ~ x1 + x2") # = y ~ x1 + x2

#save a PDF to your working directory
write.csv(raw_data, file="test.csv") 




