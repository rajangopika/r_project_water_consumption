install.packages("sjstats")

# PROJECT  ----------------------------------------------------------------


# TOPIC : Analysis of factors affecting water consumption pattern ---------


# Submitted by Gopika Rajan (501098097) -----------------------------------

# Install packges and Load library
install.packages("lmerTest")
 library(lme4) # for the analysis
 library(haven) # to load the SPSS .sav file
 library(tidyverse) # needed for data manipulation.
 library(RColorBrewer) # needed for some extra colours in one of the graphs
 library(lmerTest)# to get p-value estimations that are not part of the standard lme4 packages
 library(sjstats)
 install.packages("MASS")
 library("ggplot2")
 library(naniar)
 install.packages("naniar")
 library(MASS)
 library(car)
 library(lubridate)
  
#Load data
  water <- read.csv(file = 'hourly_no_na_edit.csv') # Load the hourly data 
  hist((water$Reading)) # Plot histogram
  length(water$Reading)
  daily_water <- read.csv(file = 'df_test1.csv') # Load the daily data
  daily_water$Date <- as.Date(daily_water$Date,origin = lubridate::origin)
# Check NA
  water[is.na(water$Reading) | water=="Inf"] = NA
  water
  sum<- sum(is.na(water$Reading))
  sum  
#Standardise
  water$Reading <- scale(water$Reading, center = TRUE, scale = TRUE)
  water$Temp<- scale(water$Temp, center = TRUE, scale = TRUE)
  water$Precipitation<- scale(water$Precipitation, center = TRUE, scale = TRUE)
  water$Humidity<- scale(water$Humidity, center = TRUE, scale = TRUE)
  
  newdata$Reading<-scale(newdata$Reading, center = TRUE, scale = TRUE)
  newdata$Temp<- scale(newdata$Temp, center = TRUE, scale = TRUE)
  newdata$Precipitation<- scale(newdata$Precipitation, center = TRUE, scale = TRUE)
  newdata$Humidity<- scale(newdata$Humidity, center = TRUE, scale = TRUE)
  
# Plot the reading v/s hour
  daily_plot_1 <- ggplot(water, aes(x = Hour, y = Reading, color=covid))+
  geom_point(aes(color=factor(Covid)))
  daily_plot_1
  daily_plot <- ggplot(mnf, aes(x = Timestamp , y = Reading))+
  geom_point()
  daily_plot
# Check the distributions
  qqp(daily_water$Reading, "norm") # Normal
  qqp(daily_water$Reading, "lnorm") # Log Normal
  install.packages("fitdistrplus")
  library(fitdistrplus)
  nbinom <- fitdistr(water$Reading, "Negative Binomial")
  qqp(water$Reading, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
  
  gamma <- fitdistr(daily_water$Reading, "gamma") # Gamma
  qqp(water$Reading, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])
  
# Daily Model
  DL_1 <- lmer(Reading ~ Mean.Temp + Total.Rain + Total.Snow + (1|work12), data = daily_water)
  summary(DL_1)
  DL_2 <- lmer(Reading ~ Mean.Temp + Total.Rain + Total.Snow + (1|Covid/work12), data = daily_water)
  summary(DL_2)
  DL_3 <- lm(Reading ~ Mean.Temp + Total.Rain + Total.Snow, data = daily_water)
  summary(DL_3)
  
  anova(DL_1,DL_2,DL_3)
  
# Plot the reading v/s hour
  hour_plot <- ggplot(water, aes(x = Hour, y = Reading,color=Covid))+
  geom_point(aes(colour = factor(Covid)))
  hour_plot
  # Box Plot
  boxplot(Reading ~ Hour, data = water,outcol=1:length(water$Month)) 


# Basic LM for Before Covid only
basic.lm <- lm(Reading~ Hour, data = water)
summary(basic.lm)
  #Plot
    prelim_plot <- ggplot(water, aes(x = Hour, y = Reading)) +
    geom_point() +
    geom_smooth(method = "lm")
    plot(basic.lm, which = 2)
    
#Houlry model GLMM LMER package
    ML_1 <- glmer(Reading~ Hour + Temp + Humidity_1 + Precipitation +
                        (Covid |Week), data = water,family = gaussian(link = "log"),na.action = na.exclude)
    summary(ML_1)
    ML_2 <- glmer(Reading~ Hour + Temp + Humidity_1 + Precipitation +
                    (1|Covid), data = water,family = gaussian(link = "log"),na.action = na.exclude)
    summary(ML_1)
    summary(ML_2)
    ML_3 <- glmer(Reading~ Hour + Temp + Humidity_1 + Precipitation +
                    (1|Week), data = water,family = gaussian(link = "log"),na.action = na.exclude)
    
    summary(ML_3)
    ML_4 <- glm(Reading~Hour,data=water,family=gaussian(link="log"))
    summary(ML_4)
#Testing significance of random effects   
    anova(ML_1,ML_2,ML_3)
    
# Random effects Model
    
    PQL_4 <- glmmPQL(Reading ~ Hour, ~Hour |Covid/Week, family = gaussian(link = "log"),
                     data = water, verbose = FALSE)
    summary(PQL_4)
    
    
    ML_5 <- glmer(Reading~ Hour + Temp + Humidity + Precipitation +
                    (1+Hour+Humidity|Covid/Week), data = water,family = gaussian(link = "log"),na.action = na.exclude)
    
    summary(ML_5)
    
    
    
    (mm_plot <- ggplot(water, aes(x = Hour, y = Reading,colour = Week)) +
        facet_wrap(~Covid, nrow=2) +   # a panel for each mountain range
        geom_point(alpha = 0.5) +
        theme_classic() +
        geom_line(data = cbind(water, pred = predict(PQL_1)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
        theme(legend.position = "none",
              panel.spacing = unit(2, "lines"))  # adding space between panels
    )
#Checking
 #Homogeinity of variance
    plot(ML_1$Residuals ~ fitted(M8),
         xlab = 'Predicted values',
         ylab = 'Normalized residuals')
    abline(h = 0, lty = 2)
    
#Mixed Linear model
mixed.lmer <- lmer(Reading ~ Hour + Temp + Humidity+Precipitation + (1|Covid/Week), data = water,REML=FALSE)
summary(mixed.lmer)
mixed.lmer2<- lmer(Reading ~ Hour + Temp + Humidity+Precipitation + (1|Covid), data = water)
mixed.lmer3<- lmer(Reading ~ Hour + Temp + Humidity+Precipitation + (1|Week), data = water)

anova(mixed.lmer,mixed.lmer2,mixed.lmer3)

nlas_1 <- 
# Plots
(prelim_plot <- ggplot(water, aes(x = Mean.Temp, y = Reading)) +
    geom_point() +
    geom_smooth(method = "mixed.lmer"))
prelim_plot
plot(ML_1,1 )
plot(resid(ML_1) ~ fitted(ML_1),
     xlab = 'Predicted values',
     ylab = 'Normalized residuals')
abline(h = 0, lty = 2)

plot(resid(ML_1) ~ water$Precipitation,
     xlab = 'Water',
     ylab = 'Residual')
    abline(h = 0, lty = 2)

# Prediction
    predict(ML_1, newdata)
    newdata$p_reading1 <- predict(ML_3,newdata,re.form=NA)
    head(newdata)
    newdata
    
    plot(newdata$Reading,newdata$p_reading)
    plot(newdata$Reading,exp(newdata$p_reading1))
    plot(newdata$Reading,10^(newdata$p_reading1))
    RMSE<- sqrt(mean(newdata$p_reading-newdata$Reading)^2)
    RMSE
    newdata$exp_p<-exp(newdata$p_reading1)
    newdata$anti_log<-10^(newdata$p_reading1)
    newdata
    
qqnorm(resid(ML_1))
qqline(resid(ML_1))

acf(mixed.lmer$residuals)


(mm_plot <- ggplot(water, aes(x = Mean.Temp, y = Reading,colour = Work)) +
    facet_wrap(~Covid, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(water, pred = predict(mixed.lmer)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)



(week_plot <- ggplot(water, aes(x = Timestamp, y = Reading,colour = work12)) +
   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)
