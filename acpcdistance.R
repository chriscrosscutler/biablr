library(readr)
library(ggplot2)
dat11 <- read_csv("~/Desktop/subj11.csv", col_names= TRUE)
dat11 <- na.omit(dat11)
attach(dat11)
plot11 <- ggplot(dat11, aes(x=time_of_day, y=acpc_distance)) + geom_point(shape=1) + geom_smooth(method=lm,  se=FALSE) + ggtitle("Time of Day and ACPC Distance for First Scan of Subject 1") + ylab("AC PC Distance (mm)") + xlab("Time of Day")
lm <- lm(acpc_distance ~ time_of_day)
plot11
summary(lm)

detach(dat11)
dat12 <- read_csv("~/Desktop/subj12.csv", col_names= TRUE)
dat12 <- na.omit(dat12)
attach(dat12)
plot12 <- ggplot(dat12, aes(x=time_of_day, y=acpc_distance)) + geom_point(shape=1) + geom_smooth(method=lm,  se=FALSE) + ggtitle("Time of Day and ACPC Distance for Second Scan of Subject 1") + ylab("AC PC Distance (mm)") + xlab("Time of Day")
lm <- lm(acpc_distance ~ time_of_day)
plot12
summary(lm)
detach(dat12)

dat13 <- read_csv("~/Desktop/subj13.csv", col_names= TRUE)
dat13 <- na.omit(dat13)
attach(dat13)
plot13 <- ggplot(dat13, aes(x=time_of_day, y=acpc_distance)) + geom_point(shape=1) + geom_smooth(method=lm,  se=FALSE) + ggtitle("Time of Day and ACPC Distance for Third scan of Subject 1") + ylab("AC PC Distance (mm)") + xlab("Time of Day")
lm <- lm(acpc_distance ~ time_of_day)
plot13
summary(lm)
detach(dat13)


dat21 <- read_csv("~/Desktop/subj21.csv", col_names= TRUE)
dat21 <- na.omit(dat21)
attach(dat21)
plot21 <- ggplot(dat21, aes(x=time_of_day, y=acpc_distance)) + geom_point(shape=1) + geom_smooth(method=lm,  se=FALSE) + ggtitle("Time of Day and ACPC Distance for First Scan of Subject 2") + ylab("AC PC Distance (mm)") + xlab("Time of Day")
lm <- lm(acpc_distance ~ time_of_day)
plot21
summary(lm)

detach(dat21)
dat22 <- read_csv("~/Desktop/subj22.csv", col_names= TRUE)
dat22 <- na.omit(dat22)
attach(dat22)
plot22 <- ggplot(dat22, aes(x=time_of_day, y=acpc_distance)) + geom_point(shape=1) + geom_smooth(method=lm,  se=FALSE) + ggtitle("Time of Day and ACPC Distance for Second Scan of Subject 2") + ylab("AC PC Distance (mm)") + xlab("Time of Day")
lm <- lm(acpc_distance ~ time_of_day)
plot22
summary(lm)

detach(dat22)

dat23 <- read_csv("~/Desktop/subj23.csv", col_names= TRUE)
dat13 <- na.omit(dat23)
attach(dat23)
plot23 <- ggplot(dat23, aes(x=time_of_day, y=acpc_distance)) + geom_point(shape=1) + geom_smooth(method=lm,  se=FALSE) + ggtitle("Time of Day and ACPC Distance for Third scan of Subject 2") + ylab("AC PC Distance (mm)") + xlab("Time of Day")
lm <- lm(acpc_distance ~ time_of_day)
plot23
summary(lm)
detach(dat23)




library(readr)
library(ggplot2)
dat1 <- read_csv("~/Desktop/both1.csv", col_names= TRUE)
dat1 <- na.omit(dat1)
attach(dat1)
plot1 <- ggplot(dat1, aes(x=time_of_day, y=acpc_distance)) + geom_point(shape=1) + geom_smooth(method=lm,  se=FALSE) + ggtitle("Time of Day and ACPC Distance for First Scan of Both Subjects") + ylab("AC PC Distance (mm)") + xlab("Time of Day")
lm <- lm(acpc_distance ~ time_of_day)
plot1
summary(lm)

library(readr)
library(ggplot2)
dat2 <- read_csv("~/Desktop/both2.csv", col_names= TRUE)
dat2 <- na.omit(dat2)
attach(dat2)
plot2 <- ggplot(dat2, aes(x=time_of_day, y=acpc_distance)) + geom_point(shape=1) + geom_smooth(method=lm,  se=FALSE) + ggtitle("Time of Day and ACPC Distance for Second Scan of Both Subjects") + ylab("AC PC Distance (mm)") + xlab("Time of Day")
lm <- lm(acpc_distance ~ time_of_day)
plot2
summary(lm)


library(readr)
library(ggplot2)
dat3 <- read_csv("~/Desktop/both3.csv", col_names= TRUE)
dat3 <- na.omit(dat3)
attach(dat3)
plot3 <- ggplot(dat3, aes(x=time_of_day, y=acpc_distance)) + geom_point(shape=1) + geom_smooth(method=lm,  se=FALSE) + ggtitle("Time of Day and ACPC Distance for Third scan of Both Subjects") + ylab("AC PC Distance (mm)") + xlab("Time of Day")
lm <- lm(acpc_distance ~ time_of_day)
plot3
summary(lm)
detach(dat3)
