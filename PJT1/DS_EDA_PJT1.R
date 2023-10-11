---
title: "PJT1_EDA"
author: "Brian Kim"
date: "`r Sys.Date()`"
output:
  html_document:
    code_folding: hide
    number_sections: false
    toc: yes
    toc_depth: 3
    toc_float: yes
  pdf_document:
    toc: yes
    toc_depth: '3'
---


```{r setup, include=FALSE}
# Some of common RMD options (and the defaults) are: 
# include=T, eval=T, echo=T, results='hide'/'asis'/'markup',..., collapse=F, warning=T, message=T, error=T, cache=T, fig.width=6, fig.height=4, fig.dim=c(6,4) #inches, fig.align='left'/'center','right', 
knitr::opts_chunk$set(warning = F, message = F)
# Can globally set option for number display format.
options(scientific=T, digits = 3) 
# options(scipen=9, digits = 3) 
```


df <- read.csv("Regional_harshrate global_1.csv")
dim(df)
str(df)


# monthly_hashrate boxplot
boxplot(df$monthly_absolute_hashrate_EH.S)


# data grouping by country
library(dplyr)

df %>%
  group_by(country) %>%
  summarise(count = n(),
            count_unique = n_distinct(country)) %>%
  View()

# Data extract
library(dplyr)
df_us <- df %>%
  filter(country == 'United States') %>%
df_rs <- df %>%
  filter(country == 'Russian Federation') %>%
df_ca <- df %>%
  filter(country == 'Canada') %>%
df_ga <- df %>%
  filter(country == 'Germany *') %>%
df_ir <- df %>%
  filter(country == 'Iran, Islamic Rep.') %>%
df_il <- df %>%
  filter(country == 'Ireland *') %>%
df_ka <- df %>%
  filter(country == 'Kazakhstan') %>%
df_ch <- df %>%
  filter(country == 'Mainland China') %>%
df_ma <- df %>%
  filter(country == 'Malaysia') %>%
df_ot <- df %>%
  filter(country == 'Other') %>%
  


# histogram
library(ggplot2)

ggplot(data = df, aes(x = monthly_absolute_hashrate_EH.S)) + 
  geom_histogram()

# histogram of each country
min(df$monthly_absolute_hashrate_EH.S)
max(df$monthly_absolute_hashrate_EH.S)

# data merge
df1 <- read.csv("bitcoin_mining.csv")
str(df1)

names(df1)[names(df1)=="Date.and.Time"] <-"date"
names(df1)[names(df1)=="Estimated..MtCO2e"] <-"MtCO2e"
names(df1)[names(df1)=="annualised.consumption.GUESS..TWh"] <-"annual_consumption"
df1$date <- gsub('T00:00:00', '', df1$date)

df <- merge(df,df1,by="date")

# histogram

# s.w.norm = rnorm(10000, mean(df$annual_consumption), sd(df$annual_consumption))
hist(df$annual_consumption, xlab="Power Consumption", ylab="Bitcoin Harshrate",
    freq=F, main="Power consumption and Bitcoin hash rate", nclass=24,
    col=rainbow(100)[45:100],
    boarder=NA)

lines(density(df$annual_consumption), col="#FF5050", lwd=2, lty=2)
lines(density(df$MtCO2e), col="yellow", lwd=2, lty=2)
# lines(density(s.w.norm), col="#5050F", lwd=2, lty=4)
    
    
    