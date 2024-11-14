# Attach the libraries
library(tidyverse)
library(naniar)
library(gtExtras)
library(dplyr)
library(readxl)

# Read the clinical data
cli <- read_csv("full_cohort_data.csv")
cli <- as.data.frame(cli)
View(cli)

# Summary
summary(cli)

# Histograms 
attach(cli)

## bmi histogram
hist(log(bmi),
     col = "green", border = "black")

### weight_first histogram
hist(log(weight_first),
     col = "steelblue", border = "black")


# Imputation in the weight_first and bmi
install.packages("mice")
library(mice)

# Predictive mean matching (Imputation method)
imputed_cli <- mice(cli, method = "pmm", m = 5)
df_cli<- complete(imputed_cli)

# Scatter plot
install.packages("ggplot2")
library(ggplot2)

df_cli %>%
  ggplot(aes(x = log(bmi), y = log(weight_first), col = service_unit))+ 
  geom_point(alpha = 0.3)+
  stat_smooth(method = lm, col = "#C42126", size = 0.5)+ 
  facet_wrap(~ service_unit)

# creating a normal plot
attach(df_cli)
plot(weight_first ~ bmi)
plot(bmi ~ weight_first)

# summarizing missing values
vis_miss(df_cli)

# compute mode
install.packages("modeest")
library(modeest)

# service_num
mod_num = mfv(df_cli$service_num)
print(mod_num)

# service_unit
mod_unit = mfv(cli$service_unit)
print(mod_unit)

mean(wbc_first, na.rm = T)


# Visualization for the bmi and weight_first
install.packages("ggplot2")
library(ggplot2)

df_cli %>%
  ggplot(aes(x = log(weight_first), y = log(bmi), col= gender_num, size = creatinine_first))+
  geom_point(alpha =0.3)+
  geom_smooth(method = lm)+
  facet_wrap(~ service_unit)

# linear regression model 
lm(bmi ~ weight_first)  
summary(lm(bmi ~ weight_first))
