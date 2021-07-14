#libraries
library(dplyr)
library(gamlss)
library(ggplot2)
library(BBmisc)
library(psych)
library(car)
library(sf)
library(spatstat)
library(sparr)
library(raster)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(scipen=999)

#data wrangle
df <- read.csv("master_table.csv") 

#data checks
length(unique(df$FIPS))
length(unique(df$date))
min(unique(df$date))
max(unique(df$date))

#covid rate
df$case_rate <- (df$caseNew/df$Tot_pop) * 100000
df$death_rate <- (df$deathNew/df$Tot_pop) * 100000

#ETHHET
df$ETHHET <- normalize(df$ETHHET, method = "range", range = c(0,1))

#time flattening
case_rate <- aggregate(df$case_rate, by=list(Category=df$FIPS), FUN=sum)
colnames(case_rate) <- c("FIPS", "case_rate")
firstcase <- subset(df, date == "2020-09-15", select = c(FIPS, daysSinceC))
firstdeath <- subset(df, date == "2020-09-15", select = c(FIPS, daysSinceD))
death_rate <- aggregate(df$death_rate, by=list(Category=df$FIPS), FUN=sum)
colnames(death_rate) <- c("FIPS", "death_rate")
Total_tw <- aggregate(df$Total_tw, by=list(Category=df$FIPS), FUN=sum)
colnames(Total_tw) <- c("FIPS", "Total_tw")
Adv_tw <- aggregate(df$Adverse, by=list(Category=df$FIPS), FUN=sum)
colnames(Adv_tw) <- c("FIPS", "Adv_tw")

#response: percentage
df$ratio <- df$Adverse / (.01 + df$Total_tw)
df$percent <- df$ratio *100

any(is.na(df))

df_sp<-df %>%
  group_by(FIPS) %>%
  summarise_at(.vars = c("popDens", "Pop_18_34","Pop_m","ETHHET", "Income", "Unemployed", "Uninsured", "Poverty", "Production","RUCC", "BIDEN", "SVI1", "SVI2", "SVI3", "SVI4", "SVI", "PCP","universities", "ratio", "percent"), .funs = mean) %>%
  left_join(.,case_rate,by=c("FIPS")) %>%
  left_join(.,death_rate,by=c("FIPS")) %>%
  left_join(.,firstcase,by=c("FIPS")) %>%
  left_join(.,firstdeath,by=c("FIPS")) %>%
  left_join(.,Total_tw,by=c("FIPS")) %>%
  left_join(.,Adv_tw,by=c("FIPS"))

#factor variable
df_sp$RUCC <- factor(df_sp$RUCC) 

#--------------------univariate regressions------------------------------------

vars <- c("popDens","Pop_18_34","Pop_m","ETHHET","Uninsured","Production","RUCC","BIDEN","SVI1",
          "SVI2","SVI3","SVI4","SVI","PCP","universities","case_rate","death_rate","daysSinceC","daysSinceD")

datalist = list()
for (i in 1:length(vars)) {
  
  form <- paste0("ratio ~ ", vars[i])
  mod <- lm(formula= form, data= df_sp)
  
  ## store table
  ctable <- coef(summary(mod))
  varEst <- summary(mod)$coefficients[2]  #coefficient estimate
  varPrt <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)[1] * 2 #p-value
  
  datalist[[i]] <- c(vars[i], varEst, varPrt)
}
coeff = do.call(rbind, datalist)
colnames(coeff) <- c("variableName", "varEst", "varPrt")

write.csv(coeff, "univariate_models.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------
#correlation matrix

df_subset <- df_sp %>% 
  subset(., select = c("Pop_18_34","ETHHET","BIDEN","SVI2","SVI3","SVI4","PCP","case_rate","daysSinceD")) %>%
  rename(., "Ethnic" = "ETHHET") %>%
  rename(., "Biden" = "BIDEN") %>%
  rename(., "Covid-19" = "case_rate") %>%
  rename(., "1st death" = "daysSinceD") 

corr <- cor(df_subset, method = c("pearson", "kendall", "spearman"))
round(corr, 2)


pairs.panels(df_subset,
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


# Model 1: OLS with percentage response -----------------------------------
mod1 <- lm(formula=ratio~ETHHET+RUCC+BIDEN+SVI2+SVI3+SVI4+PCP+case_rate+daysSinceD, data = df_sp)
summary(mod1)
plot(mod1)
vif(mod1)
AIC(mod1)

#plots
df_mod1 <- as.data.frame(cbind(mod1$residuals, mod1$fitted.values))

ggplot(df_mod1, aes(x=V2, y=V1)) +    # residuals vs. fitted
  geom_point() + 
  ggtitle("A.") +
  xlab("Fitted values") + ylab("Quantile residuals") + 
  theme_bw()
ggsave("Figure5A.png")

ggplot() + aes(sample = mod1$fitted.values) + stat_qq(distribution = qnorm) + #QQ plot
  stat_qq_line(line.p = c(0.25, 0.75), col = "grey80") + ylab("Height") + 
  ggtitle("B.") +
  xlab("Theoretical quantiles") + ylab("y") + 
  theme_bw()
ggsave("Figure5B.png")

# Model 2: zero-inflated beta regression -----------------------------------
mod2 <- gamlss(formula=ratio~ETHHET+RUCC+BIDEN+SVI2+SVI3+SVI4+PCP+case_rate+daysSinceD, data = df_sp, family=BEZI())
summary(mod2)
layout(1) # optional 4 graphs/page
plot(mod2)

#plots
df_mod2 <- as.data.frame(cbind(mod2$residuals, mod2$mu.fv))

ggplot(df_mod2, aes(x=V2, y=V1)) +    # residuals vs. fitted
  geom_point() + 
  ggtitle("C.") +
  xlab("Fitted values") + ylab("Quantile residuals") + 
  theme_bw()
ggsave("Figure5C.png")

ggplot() + aes(sample = mod2$mu.fv) + stat_qq(distribution = qnorm) + #QQ plot
  stat_qq_line(line.p = c(0.25, 0.75), col = "grey80") + ylab("Height") + 
  ggtitle("D.") +
  xlab("Theoretical quantiles") + ylab("y") + 
  theme_bw()
ggsave("Figure5D.png")

#sparr
#---------------------------------------------------------------------------------
#read geometries
geom <- st_read("CONUS_counties.shp") %>%
  subset(., select = c("GEOID", "CENTROID_X", "CENTROID_Y"))
geom$GEOID <- as.integer(as.character(geom$GEOID))

#join
df_sparr <- left_join(df_sp, geom, by = c("FIPS" = "GEOID"))

#adverse and total tweets
df_adverse <- df_sparr %>% subset(., select = c(FIPS, Adv_tw, CENTROID_X, CENTROID_Y))
df_totaltw <- df_sparr %>% subset(., select = c(FIPS, Total_tw, CENTROID_X, CENTROID_Y))

#ppp
tw_adv <- ppp(df_adverse$CENTROID_X, df_adverse$CENTROID_Y, marks = df_adverse$Adv_tw, window = owin(c(min(df_sparr$CENTROID_X) - 0.5,max(df_sparr$CENTROID_X) + 0.5),c(min(df_sparr$CENTROID_Y) - 0.5,max(df_sparr$CENTROID_Y) + 0.5)))

tw_tot <- ppp(df_totaltw$CENTROID_X, df_totaltw$CENTROID_Y, marks = df_totaltw$Total_tw, window =
                owin(c(min(df_sparr$CENTROID_X) - 0.5,max(df_sparr$CENTROID_X) + 0.5),c(min(df_sparr$CENTROID_Y) - 0.5,max(df_sparr$CENTROID_Y) + 0.5)))

parallel::detectCores()

#kde adverse tweets
f_breve <- bivariate.density(pp=tw_adv, h0=OS(tw_adv)/2, adapt=FALSE, resolution=100, weights=tw_adv$marks, verbose=TRUE, parallelise = 7)

#kde total tweets
g_tilde <- bivariate.density(pp=tw_tot, h0=OS(tw_adv)/2, adapt=FALSE, resolution=100, weights=tw_tot$marks, verbose=TRUE)

#risk
f <- risk(f_breve, g_tilde)

#extract values
r <- raster(f$rr)
crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

#rasValue <- extract(r, geom, fun=mean)
rasValue=extract(r, cbind(df_sparr$CENTROID_X, df_sparr$CENTROID_Y)) 
df2=cbind(df_sparr,rasValue) %>%
  as.data.frame()

plot(hist(rasValue))


#variable maps
#------------------------------------------------------------------------------------------------
df <- st_transform(df, crs=2163)

variable_map <- tm_shape(st_transform(st_as_sf(df), crs=2163)) +
  tm_fill(c("ETHHET","RUCC","BIDEN","SVI2", "SVI3", "SVI4", "PCP", "covid_cases", "daysSinceD"),
          style="fisher",
          palette=list("Reds", "Oranges", "Greens", "Blues", "Purples", "Greys", "BuGn", "RdPu", "YlOrRd"),
          title=c("Ethnic","RUCC","Biden","SVI 2", "SVI 3", "SVI 4", "PCP", "Covid-19", "1st death")) +
  tm_borders() +
  tm_layout(legend.position = c("left", "bottom"), frame = FALSE)
tmap_save(variable_map,filename = "variable_map.png")


# Model 4: OLS -----------------------------------
#selected model
mod4 <- lm(formula=rasValue~ETHHET+RUCC+BIDEN+SVI2+SVI3+SVI4+PCP+case_rate+daysSinceD, data = df2)
summary(mod4)
vif(mod4)
plot(mod4)
AIC(mod4)

#plots
df_mod4 <- as.data.frame(cbind(mod4$residuals, mod4$fitted.values))

ggplot(df_mod4, aes(x=V2, y=V1)) +    # residuals vs. fitted
  geom_point() + 
  ggtitle("A.") +
  xlab("Fitted values") + ylab("Quantile residuals") + 
  theme_bw()
ggsave("Figure7A.png")

ggplot() + aes(sample = mod4$fitted.values) + stat_qq(distribution = qnorm) + #QQ plot
  stat_qq_line(line.p = c(0.25, 0.75), col = "grey80") + ylab("Height") + 
  ggtitle("B.") +
  xlab("Theoretical quantiles") + ylab("y") + 
  theme_bw()
ggsave("Figure7B.png")


#Spatial Analysis of Residuals
#-------------------------------------------------------------------------------------------------------------

#spatial neighbors
w <- poly2nb(geom, queen=TRUE)
summary(w)
wm <- nb2mat(w, style='W')
rwm <- mat2listw(wm)
lm.morantest(mod1.select, rwm, alternative="two.sided")

#map residuals
geom <- geom %>%
  cbind(., mod1.select$residuals) %>%
  cbind(., mod1.select$fitted.values)

names(geom)[names(geom) == "mod1.select.residuals"] <- "mod1res"
names(geom)[names(geom) == "mod1.select.fitted.values"] <- "mod1fit"

#residual map
pal <- brewer.pal(7, "OrRd") # we select 7 colors from the palette
class(pal)

#tmap
res_map <- tm_shape(geom) +
  tm_fill("mod1res",
          style="fisher", 
          #palette=list("Reds"),
          title="model residuals")
res_map

#export to shapefile
geom2 <- geom %>%
  subset(., select = c(GEOID)) %>%
  left_join(., subset(df2, select = c(FIPS, rasValue)), by = c("GEOID"="FIPS"))
st_write(geom2, "geom2.shp", append = FALSE)

#-----------------------------------------------------------------------------------------



