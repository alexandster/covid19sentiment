library(dplyr)
library(sf)
library(ggplot2)
library(tmap)
library(sp)
library(spdep)
library(stringr)
library(car)
library(INLA)
library(ggpubr)
library(sparr)
library(raster)
library(RColorBrewer)
library(gridExtra)
library(ape)
library(AER)
library(psych)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#data wrangle
df <- read.csv("../predictor_data/master_table.csv") 
df$FIPS <- str_pad(df$FIPS, 5, pad="0")

#data checks
length(unique(df$FIPS))
length(unique(df$date))
min(unique(df$date))
max(unique(df$date))

#time flattening
covid_cases <- aggregate((df$caseNew/df$Tot_pop) * 100000, by=list(Category=df$FIPS), FUN=sum)
colnames(covid_cases) <- c("FIPS", "covid_cases")

firstcase <- subset(df, date == "2020-09-15", select = c(FIPS, daysSinceC))
firstdeath <- subset(df, date == "2020-09-15", select = c(FIPS, daysSinceD))
covid_deaths <- aggregate(df$deathNew, by=list(Category=df$FIPS), FUN=sum)
colnames(covid_deaths) <- c("FIPS", "covid_deaths")

Total_tw <- aggregate(df$Total_tw, by=list(Category=df$FIPS), FUN=sum)
colnames(Total_tw) <- c("FIPS", "Total_tw")
Adv_tw <- aggregate(df$Adverse, by=list(Category=df$FIPS), FUN=sum)
colnames(Adv_tw) <- c("FIPS", "Adv_tw")

#response: percentage
df$ratio <- df$Adverse / (.01 + df$Total_tw)
df$percent <- df$ratio *100

any(is.na(df))

#variables
vars <- c("popDens", "Pop_18_34","Pop_o_60", "Pop_m","ETHHET", "Income", "Bachelor", "English", "Unemployed", "Uninsured", "Poverty", "Production",
          "RUCC", "BIDEN", "SVI1", "SVI2", "SVI3", "SVI4", "SVI", "PCP","universities", "percent", "ratio")

#time flattening & joining
df_sp<-df %>%
  group_by(FIPS) %>%
  summarise_at(.vars = vars, .funs = mean) %>%
  left_join(.,covid_cases,by=c("FIPS")) %>%
  left_join(.,covid_deaths,by=c("FIPS")) %>%
  left_join(.,firstcase,by=c("FIPS")) %>%
  left_join(.,firstdeath,by=c("FIPS")) %>%
  left_join(.,Adv_tw,by=c("FIPS")) %>%
  left_join(.,Total_tw,by=c("FIPS")) 

#factor variable
df_sp$RUCC <- factor(df_sp$RUCC) 

#PCP
df_sp$PCP <- df_sp$PCP * 100000 

#save
save(df_sp,file="sf_sp.Rda")

#--------------------------------------------------------------------------
#standardize covariates (mean = 0, stdev = 1)
vars2 <- c("popDens", "Pop_18_34","Pop_o_60", "Pop_m","ETHHET", "Income", "Bachelor", "English", "Unemployed", "Uninsured", "Poverty", 
           "Production", "BIDEN", "SVI", "PCP","universities",  "covid_cases", "covid_deaths")

df_sp <- df_sp %>% mutate_at(vars2, ~(scale(.) %>% as.vector))


#--------------------------------------------------------------------------
#multicollinearity
formula1 <- Adv_tw/Total_tw ~ popDens+Pop_o_60+Pop_m+ETHHET+English+Unemployed+Uninsured+Poverty+Production+
  BIDEN+PCP+universities+covid_cases+covid_deaths
A=lm(formula1, data=df_sp)
vif(A)
#--------------------multiple regression------------------------------------

#read geometries
geom <- st_read("CONUS_counties.shp") %>%
  subset(., select = c("GEOID", "CENTROID_X", "CENTROID_Y")) 

#eliminate zeros
df_sp <- df_sp[df_sp["Total_tw"] > 0, ]
df_sp <- left_join(df_sp, geom, by = c("FIPS" = "GEOID")) %>%
  st_as_sf(.)

#distance matrix 
k1 <- knn2nb(knearneigh(st_centroid(df_sp$geometry)))
k1dists <- unlist(nbdists(k1, st_centroid(df_sp$geometry)))
summary(k1dists)
wm2 <- dnearneigh(st_centroid(df_sp$geometry), 0 , 181)
wm2_mat <-  nb2mat(wm2, style='W')
nb2INLA("map3.adj", wm2)
g3 <- inla.read.graph(filename = "map3.adj")

# spatial effects
df_sp$idareau <- as.numeric(as.factor(df_sp$FIPS))

### Priors
## From Moraga - gives better estimate of Phi
u <- .5/.31
alpha <- .01
phi.u <- .5
phi.alpha <- 2/3

prior_bym2 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(u, alpha)),
  phi = list(
    prior = "pc",
    param = c(phi.u, phi.alpha))
)

#Model 1: iid
f1 <- Adv_tw ~ 1 + f(idareau, model = "iid")
res1 <- inla(f1,
             family = "binomial",
             data = df_sp,
             Ntrials = Total_tw,
             control.predictor=list(compute=TRUE),
             control.compute=list(dic=TRUE),
             verbose=TRUE
)
summary(res1)

#Moran's I of random intercept
Moran.I(res1[["summary.random"]][["idareau"]][["mean"]], wm2_mat)   #significant positive spatial autocorrelation

#Model 2: spatial model (Riebler, A., Sørbye, S. H., Simpson, D., & Rue, H. (2016). An intuitive Bayesian spatial model for disease mapping that accounts for scaling. Statistical methods in medical research, 25(4), 1145-1165.)
f2 <- Adv_tw ~ Pop_o_60+Pop_m+ETHHET+RUCC+Unemployed+Poverty+BIDEN+PCP+covid_cases + f(idareau,
                model = "bym2", 
                graph = g3, 
                hyper = prior_bym2,
                constr=TRUE,
                scale.model = TRUE
)

res2 = inla(f2, 
            family = "binomial", 
            data = df_sp,
            Ntrials=Total_tw,
            control.predictor=list(compute=TRUE),
            control.compute=list(dic=TRUE),
            verbose=TRUE

)
summary(res2)

#get coefficients on natural scale
for (x in 1:10) {
  print(c(inla.emarginal(exp, res2$marginals.fixed[[x]]), inla.qmarginal(c(0.025, 0.975),
  inla.tmarginal(exp, res2$marginals.fixed[[x]]))))
} 

#compare model fit
res1$dic$family.dic
res2$dic$family.dic

#attach b, u, v
df_sp$b <- exp(res2$summary.random$idareau$mean[1:2388])
df_sp$u <- exp(res2$summary.random$idareau$mean[2389:4776])
df_sp$v <- exp((res2$summary.random$idareau$mean[1:2388]*85.54773^(1/2)-0.04540^(1/2)*res2$summary.random$idareau$mean[2389:4776])/(1-0.04540)^(1/2))

# Moran's I
Moran.I(df_sp$b, wm2_mat)   #b - significant positive spatial autocorrelation
Moran.I(df_sp$u, wm2_mat)   #u - significant positive spatial autocorrelation
Moran.I(df_sp$v, wm2_mat)   #v - no autocorrelation

#write to shapefile for mapping
# st_write(df_sp, "../outputs/INLA.shp", append = FALSE)

#check mixing parameter
ggplot() + geom_line(data = as.data.frame(res2[["marginals.hyperpar"]][["Phi for idareau"]]), 
                     aes(x = x, y = y)) + theme_bw() + 
  ggtitle("Posterior of sd of the mixing parameter") 

#how much of the variation is explained by covariates?
sd_before <- 1/res1$summary.hyperpar$`0.5quant`[1]
sd_after <- 1/res2$summary.hyperpar$`0.5quant`[1]
(sd_before -  sd_after)/sd_before   #0.1484909


