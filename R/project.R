library(tidyverse)
library(lme4)
library(ggthemes)

# Use a linear mixed model
# Experiemnt is a crossectional study, to know prevalence
# Could build a spagetti plot and a linear mixed model


#### Utility ####

data_file <- function(){
  setwd('C:\\Users\\Juan\\Desktop\\project0\\data')
}

r_file <- function(){
  setwd('C:\\Users\\Juan\\Desktop\\project0\\R')
}

python_file <- function(){
  setwd('C:\\Users\\Juan\\Desktop\\project0\\python')
}



data_file()
data = read_csv('dental_data.csv')

# Should we use an imputer

data = data %>% drop_na()



#### Data Attributes ####
# See structure of the data


str(data)

# Change the atritubes of variables

data$trtgroup = factor(data$trtgroup, levels = c(1:5))
data$sex = factor(data$sex, levels= c(1:5))
data$race = factor(data$race, levels = c(1:5))
data$smoker = factor(data$smoker, levels = c(0:1))
data$sites = factor(data$sites)


str(data)

#### Data Analisys ####

data %>%
  group_by(trtgroup) %>%
    summarize('count' = n(),
              'Mean Attach Base' = mean(attachbase),
              'Sd Attach Base' = sd(attachbase),
              'Mean Attach 1 year' = mean(attach1year),
              'Sd Attach 1 year' = sd(attach1year))

#### Plots #####
data %>%
  ggplot(aes(attachbase, fill = trtgroup)) + 
    geom_histogram(color = 'black')


data %>%
  ggplot(aes(attachbase, fill= trtgroup)) +
    geom_histogram(color = 'black') +
      facet_wrap(~trtgroup)


data %>%
  ggplot() +
    geom_histogram(aes(attachbase , fill = 'Base'), alpha =0.35) + 
    geom_histogram(aes(attach1year, fill = 'After 1 year'), alpha = 0.35) +
      ggtitle('Attachment lenght histogram. Base and After 1 year')


#

# Paired t test



t.test()



