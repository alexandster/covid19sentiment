#load libraries
require(caTools)
library(tidycensus)
library(tidyverse)
library(dplyr)
library(stringr)
library(usmap)
library(readxl)
library(sf)
library(reshape2)
library(cdlTools)
library(tigris)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#study period
#------------------------------------------------------------------------------------------------------------------------
startDate <- "2020-01-22"
endDate <- "2020-09-15"

#complete list of counties
#------------------------------------------------------------------------------------------------------------------------

data("fips_codes")
fips_codes$GEOID <- paste0(fips_codes$state_code, fips_codes$county_code) 
fips_codes<-subset(fips_codes[!(fips_codes$state=="AK" | 
                           fips_codes$state=="HI" |
                           fips_codes$state=="AS" |
                           fips_codes$state=="GU" |
                           fips_codes$state=="MP" |
                           fips_codes$state=="PR" |
                           fips_codes$state=="UM" |
                           fips_codes$state=="VI"),], select = c("GEOID"))


#census stuff
#------------------------------------------------------------------------------------------------------------------------

#get your own key: https://api.census.gov/data/key_signup.html
census_api_key("[get your own key]")

#get variable lookup table
v18 <- load_variables(2018, "acs5", cache = TRUE)
#------------------------------------------------------------------------------------------------------------------------



#####################
##county-level data##
#####################

#population data - county-level static (clsPop)
#------------------------------------------------------------------------------------------------------------------------
#get census data
Tot_pop <- subset(get_acs(geography = "county", 
                          variables = c(var = "B01003_001"), 
                          year = 2018),
                  select = -c(NAME,variable, moe))

#44% of 18-24 year olds use Twitter
Pop_18_34 <- subset(get_acs(geography = "county", 
                            variables = c(var = "B09021_008"), 
                            year = 2018),
                    select = -c(NAME,variable, moe))
Pop_18_34$estimate <- Pop_18_34$estimate / Tot_pop$estimate

Pop_o_60 <- subset(get_acs(geography = "county", 
                           variables = c(var = "S0101_C01_028E"), 
                           year = 2018),
                   select = -c(NAME,variable, moe))
Pop_o_60$estimate <- Pop_o_60$estimate / Tot_pop$estimate

Pop_m <- subset(get_acs(geography = "county", 
                        variables = c(var = "B01001_002"), 
                        year = 2018),
                select = -c(NAME,variable, moe))
Pop_m$estimate <- Pop_m$estimate / Tot_pop$estimate


#race, ethnicity variables
Pop_white <- subset(get_acs(geography = "county", 
                            variables = c(var = "B01001A_001"), 
                            year = 2018),
                    select = -c(NAME,variable, moe))
Pop_white$estimate <- Pop_white$estimate / Tot_pop$estimate

Pop_black <- subset(get_acs(geography = "county", 
                            variables = c(var = "B01001B_001"), 
                            year = 2018),
                    select = -c(NAME,variable, moe))
Pop_black$estimate <- Pop_black$estimate / Tot_pop$estimate

Pop_AmIndAlNat <- subset(get_acs(geography = "county", 
                                 variables = c(var = "B01001C_001"), 
                                 year = 2018),
                         select = -c(NAME,variable, moe))
Pop_AmIndAlNat$estimate <- Pop_AmIndAlNat$estimate / Tot_pop$estimate

Pop_asia <- subset(get_acs(geography = "county", 
                           variables = c(var = "B01001D_001"), 
                           year = 2018),
                   select = -c(NAME,variable, moe))
Pop_asia$estimate <- Pop_asia$estimate / Tot_pop$estimate

Pop_NaHaPaIs <- subset(get_acs(geography = "county", 
                               variables = c(var = "B01001E_001"), 
                               year = 2018),
                       select = -c(NAME,variable, moe))
Pop_NaHaPaIs$estimate <- Pop_NaHaPaIs$estimate / Tot_pop$estimate

#hispanics: DP05_0071E
Pop_Hisp <- subset(get_acs(geography = "county", 
                           variables = c(var = "DP05_0071E"), 
                           year = 2018),
                   select = -c(NAME,variable, moe))
Pop_Hisp$estimate <- Pop_Hisp$estimate / Tot_pop$estimate

#Ethnic heterogeneity (ETHHET)
ETHHET <- as.data.frame(cbind(Pop_NaHaPaIs$GEOID, 1-rowSums(cbind(Pop_Hisp$estimate^2, Pop_AmIndAlNat$estimate^2, Pop_asia$estimate^2, Pop_black$estimate^2, Pop_NaHaPaIs$estimate^2, Pop_white$estimate^2))))
colnames(ETHHET) <- c("FIPS", "ETHHET")
ETHHET$ETHHET <- as.numeric(ETHHET$ETHHET)

Income <- subset(get_acs(geography = "county", 
                         variables = c(var = "S1901_C01_012E"), 
                         year = 2018),
                 select = -c(NAME,variable,moe))
#impute NM mean for missing value
Income[is.na(Income$estimate),]$estimate <- mean((Income[(substr(Income$GEOID,1,2)=="35"),]$estimate), na.rm = TRUE)

Bachelor <- subset(get_acs(geography = "county",
                           variables = c(var = "B16010_041"),
                           year = 2018),
                   select = -c(NAME,variable,moe))
#impute NM mean for missing value
Bachelor[is.na(Bachelor$estimate),]$estimate <- mean((Bachelor[(substr(Bachelor$GEOID,1,2)=="35"),]$estimate), na.rm = TRUE)
Bachelor$estimate <- Bachelor$estimate / Tot_pop$estimate

Disabled <- subset(get_acs(geography = "county", 
                           variables = c(var = "S1810_C02_001E"), 
                           year = 2018),
                   select = -c(NAME,variable,moe))
Disabled$estimate <- Disabled$estimate / Tot_pop$estimate

English <- subset(get_acs(geography = "county", 
                          variables = c(var = "DP02_0113E"), 
                          year = 2018),
                  select = -c(NAME,variable,moe))
English$estimate <- English$estimate / Tot_pop$estimate

Unemployed <- subset(get_acs(geography = "county", 
                             variables = c(var = "B23025_005"), 
                             year = 2018),
                     select = -c(NAME,variable,moe))
Unemployed[is.na(Unemployed$estimate),]$estimate <- mean((Unemployed[(substr(Unemployed$GEOID,1,2)=="35"),]$estimate), na.rm = TRUE)
Unemployed$estimate <- Unemployed$estimate / Tot_pop$estimate

Uninsured <- subset(get_acs(geography = "county", 
                            variables = c(var = "S2701_C05_001E"), 
                            year = 2018),
                    select = -c(NAME,variable,moe)) 
Uninsured$estimate <- Uninsured$estimate / Tot_pop$estimate

Poverty <- subset(get_acs(geography = "county", 
                          variables = c(var = "B17010_002"), 
                          year = 2018),
                  select = -c(NAME,variable,moe))
Poverty[is.na(Poverty$estimate),]$estimate <- mean((Poverty[(substr(Poverty$GEOID,1,2)=="35"),]$estimate), na.rm = TRUE)
Poverty$estimate <- Poverty$estimate / Tot_pop$estimate

Production <- subset(get_acs(geography = "county", 
                          variables = c(var = "S2406_C01_006E"), 
                          year = 2018),
                  select = -c(NAME,variable,moe))
Production[is.na(Production$estimate),]$estimate <- mean((Production[(substr(Production$GEOID,1,2)=="35"),]$estimate), na.rm = TRUE)
Production$estimate <- Production$estimate / Tot_pop$estimate

#RUCC
RUCC <- subset(read_xls("../predictor_data/ruralurbancodes2013.xls"), select = c(FIPS, RUCC_2013))
RUCC$RUCC_2013 <- ifelse(RUCC$RUCC_2013 < 4, 0, 1)  #binary classification - 0: metropolitan, 1: non-metropolitan
  
#deal with Shannon County -> Ogala COunty: : replace FIPS code 46102 with the old code 46113
RUCC$FIPS[RUCC$FIPS == 46113] <- 46102

vulner <- read.csv("../predictor_data/SVI2018_US_COUNTY.csv") %>%
  subset(., select = c(FIPS, RPL_THEME1, RPL_THEME2, RPL_THEME3, RPL_THEME4, RPL_THEMES))
vulner$FIPS <- as.character(vulner$FIPS)
vulner$FIPS  <- str_pad(vulner$FIPS, 5, pad="0")
#impute NM mean for missing value
NM1 <- vulner[(substr(vulner$FIPS,1,2)=="35"),]$RPL_THEME1
NMA <- vulner[(substr(vulner$FIPS,1,2)=="35"),]$RPL_THEMES
NM1mean <- mean(NM1[-1])
NMAmean <- mean(NMA[-1])
vulner[vulner$FIPS==35039,]$RPL_THEME1 <- NM1mean
vulner[vulner$FIPS==35039,]$RPL_THEMES <- NMAmean


#population density
url <- "https://opendata.arcgis.com/datasets/fab7849b55d54f0f8f246605f6ee9306_12.csv"
popDens <- read.csv(url(url)) %>%
  subset(., select = c(ID,POPDENS_CY))
colnames(popDens) <- c("FIPS", "POPDENS")
popDens$FIPS <- as.character(str_pad(popDens$FIPS, 5, pad="0"))
popDens$FIPS[popDens$FIPS == 46113] <- 46102  

#primary care physicians, nursing homes, universities and colleges - county-level static (clsHnu)
#------------------------------------------------------------------------------------------------------------------------

#primary care physicians
library(datasets)
states <- state.name
states <- states[c(-1,-2,-11, -34, -48, -50)]
base <- "https://www.countyhealthrankings.org/sites/default/files/media/document/2020%20County%20Health%20Rankings%20"
statesURLs <- paste0(base, states,  "%20Data%20-%20v1_0.xlsx")
specialStates <- c("Alabama%20Data%20-%20v1_1.xlsx", "North%20Dakota%20Data%20-%20v1_1.xlsx", "West%20Virginia%20Data%20-%20v1.xlsx", "Wyoming%20Data%20-%20v1.xlsx")
specialStatesURLs <- paste0(base, specialStates)
allStates <- c(statesURLs, specialStatesURLs)
datalist = list()
for (i in 1:length(allStates)) {
  temp = tempfile(fileext = ".xlsx")
  download.file(allStates[i], destfile=temp, mode='wb')
  dat <- readxl::read_excel(temp, sheet =4)
  table <- cbind(dat[,1], dat$`Primary care physicians`)#[-1,]
  table <- table[-1:-2,]
  colnames(table) <- c("FIPS","PCP") #column names
  
  table$PCP <- as.numeric(table$PCP) #convert to numeric
  
  table$PCP[is.na(table$PCP)] <- mean(table$PCP, na.rm = TRUE)  #impute mean for missing values
  #dat$i <- i  # maybe you want to keep track of which iteration produced it?
  datalist[[i]] <- table # add it to your list
  print(allStates[i])
}
primaryCare = do.call(rbind, datalist)
primaryCare$PCP <- as.numeric(primaryCare$PCP)
dc<-data.frame("11001",1684)
names(dc)<-c("FIPS","PCP")
primaryCare <- rbind(primaryCare, dc)

#universities&colleges
univ <- read.csv("../../covid19test/data/Colleges_and_Universities.csv")
univFreq <- as.data.frame(table(univ$COUNTYFIPS))
univFreq$Var1 <- as.character(univFreq$Var1)

#add FIPS
univFreq$Var1 <- str_pad(univFreq$Var1, 5, pad="0")

#missing values
univFreq$Freq[is.na(univFreq$Freq)] <- 0

#combine
clsHnu <- left_join(primaryCare, univFreq, by = c("FIPS" = "Var1")) %>%
  left_join(., Tot_pop, by = c("FIPS" = "GEOID"))

colnames(clsHnu) <- c("GEOID", "PCP", "UNI", "POP")

#missing values
clsHnu$UNI[is.na(clsHnu$UNI)] <- 0

clsHnu$PCP <- (clsHnu$PCP / clsHnu$POP)   # compute ratio
clsHnu$UNI <- (clsHnu$UNI / clsHnu$POP)   # compute ratio

clsHnu <- subset(clsHnu, select = -c(POP)) #drop columns


#election results 2020
#------------------------------------------------------------------------------------------------------------------------
elec <- read_excel("Vote2020.xlsx") %>%
  subset(., select=c(FIPS, Biden))

elec$FIPS <- str_pad(elec$FIPS, 5, pad="0")

#------------------------------------------------------------------------------------------------------------------------



# COVID-19 data - county-level dynamic (cldCo)
#------------------------------------------------------------------------------------------------------------------------
#cases
url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
cTable <- read.csv(url(url))

cTable<-cTable[!(cTable$Province_State=="Diamond Princess" | cTable$Province_State=="Grand Princess" | cTable$Province_State=="Puerto Rico" |           #drop rows
                   cTable$Province_State=="Northern Mariana Islands" | cTable$Province_State=="Virgin Islands" | cTable$Province_State=="American Samoa" |
                   cTable$Province_State=="Recovered" | cTable$Province_State=="Alaska" | cTable$Province_State=="Hawaii"| cTable$Province_State=="Guam" | 
                   is.na(cTable$FIPS) | cTable$FIPS>80000),]
cTable <- subset(cTable, select = -c(UID, iso2, iso3, code3, Admin2, Province_State, Country_Region, Lat, Long_, Combined_Key)) #drop columns
colnames(cTable) <- c("FIPS", as.numeric(seq(as.Date("2020/1/22"), as.Date(Sys.Date()-2), by = "day"))) #column names
countyCase <- melt(cTable, id.var = "FIPS")    #convert table to long format
colnames(countyCase) <- c("FIPS","date", "cases") #column names
countyCase$date <- as.numeric(countyCase$date)  #convert date to numeric

#from aggregated counts to actual counts
countyCase  <- countyCase [order(countyCase $FIPS, countyCase $date), ]
diff2 <- function(x) diff(c(0, x))
countyCase $actual <- c(unlist(t(aggregate(cases~FIPS, countyCase , diff2)[, -1])))
countyCase [order(as.numeric(rownames(countyCase ))),]
countyCase$actual <- ifelse(countyCase$actual < 0, 0, countyCase$actual) #eliminate negatives (6 in total so far due to bad case reporting)
countyCase$date <- as.Date(countyCase$date, origin = "2020-01-21")  #convert date to date type
colnames(countyCase) <- c("FIPS","date", "caseCum", "caseNew") #column names

#days since first case (temporal distance to day of COVID-19 onset)
zeroes <- rowSums(cTable == 0) #number of days since onset for each county
firstCaseTable <- cTable  #initialize table
firstCaseTable$zeroes <- zeroes   
for (i in 2:(length(firstCaseTable)-1)) {
  firstCaseTable[,i] <- i-2 - firstCaseTable$zeroes
}
firstCaseTable <- firstCaseTable[1:(length(firstCaseTable)-1)]
colnames(firstCaseTable) <- c("FIPS", as.numeric(seq(as.Date("2020/1/22"), as.Date(Sys.Date()-2), by = "day"))) #column names
firstCaseTable <- melt(firstCaseTable, id.var = "FIPS")    #convert table to long format
colnames(firstCaseTable) <- c("FIPS","date", "daysSinceC") #column names
firstCaseTable$date <- as.numeric(firstCaseTable$date)  #convert date to numeric
firstCaseTable$date <- as.Date(firstCaseTable$date, origin = "2020-01-21")  #convert date to date type
countyCase <- inner_join(countyCase, firstCaseTable) #combine
countyCase <- countyCase[countyCase$date >= startDate & countyCase$date <= endDate,]  #date range = study period

#deaths
url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
dTable <- read.csv(url(url))
dTable<-dTable[!(dTable$Province_State=="Diamond Princess" | dTable$Province_State=="Grand Princess" | dTable$Province_State=="Puerto Rico" |           #drop rows
                   dTable$Province_State=="Northern Mariana Islands" | dTable$Province_State=="Virgin Islands" | dTable$Province_State=="American Samoa" |
                   dTable$Province_State=="Recovered" | dTable$Province_State=="Alaska" | dTable$Province_State=="Hawaii"| dTable$Province_State=="Guam" |
                   is.na(dTable$FIPS) | dTable$FIPS>80000),]
dTable <- subset(dTable, select = -c(UID, Population, iso2, iso3, code3, Admin2, Province_State, Country_Region, Lat, Long_, Combined_Key)) #drop columns

colnames(dTable) <- c("FIPS", as.numeric(seq(as.Date("2020/1/22"), as.Date(Sys.Date()-2), by = "day"))) #column names
countyDeath <- melt(dTable, id.var = "FIPS")    #convert table to long format
colnames(countyDeath) <- c("FIPS","date", "deaths") #column names
countyDeath$date <- as.numeric(countyDeath$date)  #convert date to numeric

#from aggregated counts to actual counts
countyDeath  <- countyDeath [order(countyDeath$FIPS, countyDeath$date), ]
diff2 <- function(x) diff(c(0, x))
countyDeath $actual <- c(unlist(t(aggregate(deaths~FIPS, countyDeath , diff2)[, -1])))
countyDeath [order(as.numeric(rownames(countyDeath ))),]

countyDeath$actual <- ifelse(countyDeath$actual < 0, 0, countyDeath$actual) #eliminate negatives (6 in total so far due to bad case reporting)
countyDeath$date <- as.Date(countyDeath$date, origin = "2020-01-21")  #convert date to date type
colnames(countyDeath) <- c("FIPS","date", "deathCum", "deathNew") #column names

#days since first death (temporal distance to day of COVID-19 onset)
zeroes <- rowSums(dTable == 0) #number of days since onset for each county
firstDeathTable <- dTable  #initialize table
firstDeathTable$zeroes <- zeroes   
for (i in 2:(length(firstDeathTable)-1)) {
  firstDeathTable[,i] <- i-2 - firstDeathTable$zeroes
}
firstDeathTable <- firstDeathTable[1:(length(firstDeathTable)-1)]
colnames(firstDeathTable) <- c("FIPS", as.numeric(seq(as.Date("2020/1/22"), as.Date(Sys.Date()-2), by = "day"))) #column names
firstDeathTable <- melt(firstDeathTable, id.var = "FIPS")    #convert table to long format
colnames(firstDeathTable) <- c("FIPS","date", "daysSinceD") #column names
firstDeathTable$date <- as.numeric(firstDeathTable$date)  #convert date to numeric
firstDeathTable$date <- as.Date(firstDeathTable$date, origin = "2020-01-21")  #convert date to date type

countyDeath <- inner_join(countyDeath, firstDeathTable) #combine
countyDeath <- countyDeath[countyDeath$date >= startDate & countyDeath$date <= endDate,]  #date range = study period

#combine case and death data
cldCo <- full_join(countyCase, countyDeath, by = c("FIPS", "date"))
cldCo <- cldCo %>%  mutate(FIPS = as.character(FIPS))
cldCo$FIPS <- str_pad(cldCo$FIPS, 5, pad="0")
cldCo <- subset(cldCo, select = -c(caseCum, deathCum)) #drop columns

# deal with NA values
cldCo$deathNew[is.na(cldCo$deathNew)] <- 0        #new deaths

#------------------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------------
#bind together
df <- left_join(cldCo, Tot_pop, by = c("FIPS" = "GEOID")) %>%
  left_join(., popDens, by = c("FIPS")) %>%
  left_join(., Pop_18_34, by = c("FIPS" = "GEOID")) %>%
  left_join(., Pop_o_60, by = c("FIPS" = "GEOID")) %>%
  left_join(., Pop_m, by = c("FIPS" = "GEOID")) %>%
  left_join(., ETHHET, by = "FIPS") %>%
  left_join(., Income, by = c("FIPS" = "GEOID")) %>%
  left_join(., Bachelor, by = c("FIPS" = "GEOID")) %>%
  left_join(., Disabled, by = c("FIPS" = "GEOID")) %>%
  left_join(., English, by = c("FIPS" = "GEOID")) %>%
  left_join(., Unemployed, by = c("FIPS" = "GEOID")) %>%
  left_join(., Uninsured, by = c("FIPS" = "GEOID")) %>%
  left_join(., Poverty, by = c("FIPS" = "GEOID")) %>%
  left_join(., Production, by = c("FIPS" = "GEOID")) %>%
  left_join(., RUCC, by = "FIPS") %>%
  left_join(., elec, by = "FIPS") %>%
  left_join(., vulner, by = "FIPS") %>%
  left_join(., clsHnu, by = c("FIPS" = "GEOID"))

colnames(df) <- c("FIPS", "date", "caseNew", "daysSinceC", "deathNew", "daysSinceD", "Tot_pop", "popDens", 
                  "Pop_18_34", "Pop_o_60","Pop_m", "ETHHET", "Income", "Bachelor", "Disabled", "English", 
                  "Unemployed", "Uninsured", "Poverty", "Production", "RUCC", "BIDEN", "SVI1", "SVI2", "SVI3", "SVI4", "SVI", "PCP","universities")  



#diagnostics
any(is.na(df))

#date to character
df$date <- as.character(df$date)  #convert date to numeric

#------------------------------------------------------------------------------------------------------------------------
# response variable

resp <- read.csv("merged_tweet_data.csv") %>%
  subset(., select = c(FIPS, Date, Adverse, Total_tw)) %>%
  rename(date=Date)

#satscan
#----------------------------------------------------------------
resp2 <- subset(resp, select = c(FIPS, date, Adverse, nonAdverse))
resp2$FIPS <- str_pad(resp2$FIPS, 5, pad="0")
resp2 <- left_join(resp2, Tot_pop, by = c("FIPS" = "GEOID"))
resp2$FIPS <- as.numeric(resp2$FIPS)
any(is.na(resp2))
write.csv(resp2, "cases.csv", row.names = FALSE)
#----------------------------------------------------------------

resp$date <- as.Date(resp$date)
resp <- resp[resp$date >= startDate & resp$date <= endDate,]  #date range = study period
resp$FIPS <- str_pad(resp$FIPS, 5, pad="0")
resp$date <- as.character(resp$date)  #convert date to character

#consistency checks
length(unique(resp$FIPS))
length(unique(df$FIPS))
length(unique(resp$date))
length(unique(df$date))

df2 <- left_join(resp, df, by=c("FIPS", "date"))

#write
write.csv(df2,"master_table.csv", row.names = FALSE)




