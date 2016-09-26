library(dplyr)
library(ggplot2)

create_file <- function(name){
  path   <- paste(getwd(),"/",name,".png",sep = '') %>% 
            file.path() %>% png(,width=960,height=480)
}

#this is the template: change the theme (and the name argument) to produce the other plots

create_file(name = "theme_bw")
ggplot(data=diamonds, aes(carat,price ))+
  geom_point(aes(colour= color))+
  theme_bw()
dev.off()
