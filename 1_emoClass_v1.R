library("readxl")
library(dplyr)
library (rtweet)
library(tidytext)
library(ggplot2)
library(syuzhet)
library(acs)
library(tidyverse)
library(plyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#read tweets
dat <- read_excel("Tweet_2019_11_3149.xlsx")

# clean tweet text
dat$text <-  gsub("https\\S*", "", dat$text)
dat$text <-  gsub("@\\S*", "", dat$text) 
dat$text  <-  gsub("amp", "", dat$text) 
dat$text  <-  gsub("[\r\n]", "", dat$text)
dat$text  <-  gsub("[[:punct:]]", "", dat$text)
      
#get emotions 
emo <- get_nrc_sentiment((dat$text))

#adverse emo yes/no
emo$adverse <- ifelse(rowSums(emo[ , c(1,3,4,6)], na.rm=TRUE) > 0, 1, 0)

#attach emotions to dat
dat <- cbind(dat, emo$adverse)

#rename column
names(dat)[names(dat)=="emo$adverse"] <- "adverse"

#drop columns
dat <- subset(dat, select = c(YMD,x,y,adverse) )

#safe dat a frame
save(dat,file="dat.Rdata")

