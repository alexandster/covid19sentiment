#libraries
#-------------------------
library(sf)
library(tmap)
library(dplyr)
library(stringr)
library(ggplot2)
library(hrbrthemes)
library(BBmisc)
library(zoo)
library(tidycensus)
library(reshape2)

#workspace
#-------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#read master table
#-------------------------
df <- read.csv("merged_tweet_data.csv")  %>%
  subset(., select = c(FIPS, Date, anger, anticipation, disgust, fear, joy, sadness, surprise, 
  trust, Adverse, Total_tw))

#preceeding zeroes
df$FIPS <- str_pad(df$FIPS, 5, pad="0") %>%
  as.character()

#space flattening
#-------------------------
df_t <- aggregate(df[,3:12], by=list(Category=df$Date), FUN=sum)
df_t$Category <- as.Date(df_t$Category)
names(df_t)[names(df_t) == 'Category'] <- 'date'

#ratio
df_t$ratio <- df_t$Adverse / (df_t$Total_tw +1)
df_t$ratio <- rollmean(df_t$ratio, 7, fill = "extend")
df_t$Total_tw <- rollmean(df_t$Total_tw, 7, fill = "extend")

ylim.prim <- c(min(df_t$Total_tw), max(df_t$Total_tw))   # ratio
ylim.sec <- c(min(df_t$ratio), max(df_t$ratio))    
b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1] 

#plot it!
p <- ggplot(df_t, aes(date, Total_tw)) +
  geom_line(color = "blue") +
  geom_line(aes(y = a + ratio*b), color = "red") +
  scale_y_continuous("Total tweets", 
                     sec.axis = sec_axis(~ (. - a)/b, name = "% adverse")) +
  scale_x_date(name = "", date_breaks = "2 month", date_labels = " %b '%y") +
  theme_light() +
  theme(
    axis.title.y = element_text(color = "blue", size=13),
    axis.title.y.right = element_text(color = "red", size=13))

p

ggsave("timeseries.png", p)
