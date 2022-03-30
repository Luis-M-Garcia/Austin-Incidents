library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(readxl)
library(lubridate)
install.packages('zoo')
library(zoo)

exp_data <- read_csv('EMS.csv')
exp_data <- as.data.frame(exp_data)
head(exp_data)
nrow(exp_data)
names(exp_data)

#keeping only incident, incident level columns, and month-year column
incidents <- exp_data %>% select(c(2:17))
exp_data$`Month-Year`
#doing comparison of months over the years, first jan 
jan_inc <- incidents %>% filter(str_detect(exp_data$`Month-Year`,'Jan'))
jan_inc$`Month-Year` <- str_sub(jan_inc$`Month-Year`,5,8)
jan_inc$`Month-Year`<- ymd(jan_inc$`Month-Year`,truncated = 2L)
ggplot(data = jan_inc) + geom_line(mapping = aes(x = `Month-Year`,y = `Total Incidents`))

feb_inc <- incidents %>% filter(str_detect(exp_data$`Month-Year`,'Feb'))
feb_inc$`Month-Year` <- str_sub(feb_inc$`Month-Year`,5,8)
feb_inc$`Month-Year`<- ymd(feb_inc$`Month-Year`,truncated = 2L)
ggplot(data = feb_inc) + geom_line(mapping = aes(x = `Month-Year`,y = `Total Incidents`))
#ctr f, find jan, replace with feb, saves time!

mar_inc <- incidents %>% filter(str_detect(exp_data$`Month-Year`,'Mar'))
mar_inc$`Month-Year` <- str_sub(mar_inc$`Month-Year`,5,8)
mar_inc$`Month-Year`<- ymd(mar_inc$`Month-Year`,truncated = 2L)
ggplot(data = mar_inc) + geom_point(mapping = aes(x = `Month-Year`,y = `Total Incidents`),color = 'red')+geom_line(mapping = aes(x = `Month-Year`,y = `Total Incidents`),color = 'blue')


apr_inc <- incidents %>% filter(str_detect(exp_data$`Month-Year`,'Apr'))
apr_inc$`Month-Year` <- str_sub(apr_inc$`Month-Year`,5,8)
apr_inc$`Month-Year`<- ymd(apr_inc$`Month-Year`,truncated = 2L)
ggplot(data = apr_inc) + geom_point(mapping = aes(x = `Month-Year`,y = `Total Incidents`),color = 'red')+geom_line(mapping = aes(x = `Month-Year`,y = `Total Incidents`),color = 'blue')


may_inc <- incidents %>% filter(str_detect(exp_data$`Month-Year`,'May'))
may_inc$`Month-Year` <- str_sub(may_inc$`Month-Year`,5,8)
may_inc$`Month-Year`<- ymd(may_inc$`Month-Year`,truncated = 2L)
ggplot(data = may_inc) + geom_point(mapping = aes(x = `Month-Year`,y = `Total Incidents`),color = 'red')+geom_line(mapping = aes(x = `Month-Year`,y = `Total Incidents`),color = 'blue')


jun_inc <- incidents %>% filter(str_detect(exp_data$`Month-Year`,'Jun'))
jun_inc$`Month-Year` <- str_sub(jun_inc$`Month-Year`,5,8)
jun_inc$`Month-Year`<- ymd(jun_inc$`Month-Year`,truncated = 2L)
ggplot(data = jun_inc) + geom_point(mapping = aes(x = `Month-Year`,y = `Total Incidents`),color = 'red')+geom_line(mapping = aes(x = `Month-Year`,y = `Total Incidents`),color = 'blue')


jul_inc <- incidents %>% filter(str_detect(exp_data$`Month-Year`,'Jul'))
jul_inc$`Month-Year` <- str_sub(jul_inc$`Month-Year`,5,8)
jul_inc$`Month-Year`<- ymd(jul_inc$`Month-Year`,truncated = 2L)
ggplot(data = jul_inc) + geom_point(mapping = aes(x = `Month-Year`,y = `Total Incidents`),color = 'red')+geom_line(mapping = aes(x = `Month-Year`,y = `Total Incidents`),color = 'blue')


aug_inc <- incidents %>% filter(str_detect(exp_data$`Month-Year`,'Aug'))
aug_inc$`Month-Year` <- str_sub(aug_inc$`Month-Year`,5,8)
aug_inc$`Month-Year`<- ymd(aug_inc$`Month-Year`,truncated = 2L)
ggplot(data = aug_inc) + geom_point(mapping = aes(x = `Month-Year`,y = `Total Incidents`),color = 'red')+geom_line(mapping = aes(x = `Month-Year`,y = `Total Incidents`),color = 'blue')


sep_inc <- incidents %>% filter(str_detect(exp_data$`Month-Year`,'Sep'))
sep_inc$`Month-Year` <- str_sub(sep_inc$`Month-Year`,5,8)
sep_inc$`Month-Year`<- ymd(sep_inc$`Month-Year`,truncated = 2L)
ggplot(data = sep_inc) + geom_point(mapping = aes(x = `Month-Year`,y = `Total Incidents`),color = 'red')+geom_line(mapping = aes(x = `Month-Year`,y = `Total Incidents`),color = 'blue')


oct_inc <- incidents %>% filter(str_detect(exp_data$`Month-Year`,'Oct'))
oct_inc$`Month-Year` <- str_sub(oct_inc$`Month-Year`,5,8)
oct_inc$`Month-Year`<- ymd(oct_inc$`Month-Year`,truncated = 2L)
ggplot(data = oct_inc) + geom_point(mapping = aes(x = `Month-Year`,y = `Total Incidents`),color = 'red')+geom_line(mapping = aes(x = `Month-Year`,y = `Total Incidents`),color = 'blue')


nov_inc <- incidents %>% filter(str_detect(exp_data$`Month-Year`,'Nov'))
nov_inc$`Month-Year` <- str_sub(nov_inc$`Month-Year`,5,8)
nov_inc$`Month-Year`<- ymd(nov_inc$`Month-Year`,truncated = 2L)
ggplot(data = nov_inc) + geom_point(mapping = aes(x = `Month-Year`,y = `Total Incidents`),color = 'red')+geom_line(mapping = aes(x = `Month-Year`,y = `Total Incidents`),color = 'blue')


dec_inc <- incidents %>% filter(str_detect(exp_data$`Month-Year`,'Dec'))
dec_inc$`Month-Year` <- str_sub(dec_inc$`Month-Year`,5,8)
dec_inc$`Month-Year`<- ymd(dec_inc$`Month-Year`,truncated = 2L)
ggplot(data = dec_inc) + geom_point(mapping = aes(x = `Month-Year`,y = `Total Incidents`),color = 'red')+geom_line(mapping = aes(x = `Month-Year`,y = `Total Incidents`),color = 'blue')

#I will now group months into their seasons to see which season has highest incidents
#creating a date datatype
seasons <- c('spring','summer','winter','fall')
season_check <- exp_data
season_check <- season_check %>% mutate(date = as.Date(as.yearmon(exp_data$`Month-Year`)))
season_check <- season_check %>% select(-c(`Month Key`,`Month-Year`))

#can extract years or months for further analysis
#year(season_check$date)
#month(season_check$date)
season_check[,'seasons'] <- 0
names(season_check)

season_check$seasons[month(season_check$date)%in%c(3,4,5)] <- 'spring'
season_check$seasons[month(season_check$date)%in%c(6,7,8)] <- 'summer'
season_check$seasons[month(season_check$date)%in%c(9,10,11)] <- 'fall'
season_check$seasons[month(season_check$date)%in%c(12,1,2)] <- 'winter'

#now I will see how the seasons compare
#seems as though winters have lowest amount of total incidents
#expected highest
season_check %>% group_by(seasons) %>% summarize(mean_incidents = mean(`Total Incidents`))

season_check %>% ggplot(mapping = aes(date,`Total Incidents`,color = seasons)) + geom_smooth(se=F)
 
