setwd('D:/Project/R Scripts/Exploratory-Data-Analysis/Udacity+Facebook')
library(ggplot2)
library(dplyr)
library(tidyverse)
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')

# Chaining function
pf.fc_by_age_gender <- pf %>%
  #filters all na genders
  filter(!is.na(gender)) %>%
  group_by(age, gender) %>%
  summarise(
            mean_friend_count = mean(friend_count),
            median_friend_count = median(as.numeric(friend_count)),
            n=n()) %>%
  ungroup() %>%
  #we need to ungroup to remove age layer to access gender
  arrange(age)
head(pf.fc_by_age_gender)

# Create a line graph showing the
# median friend count over the ages
# for each gender.
ggplot(aes(x = age, y = median_friend_count),
       data = pf.fc_by_age_gender) +
  geom_line(aes(color=gender))

install.packages('reshape2')
library(reshape2)

pf.fc_by_age_gender.wide <- dcast(pf.fc_by_age_gender, 
                                  age ~ gender,
                                  value.var = 'median_friend_count')
head(pf.fc_by_age_gender.wide)

# Plot the ratio of the female to male median
# friend counts using the data frame
# pf.fc_by_age_gender.wide.
ggplot(aes(x=age, y=female/male), data=pf.fc_by_age_gender.wide) +
  geom_hline(yintercept=1,linetype=2) +geom_line()

# Create a variable called year_joined
# in the pf data frame using the variable
pf$year_joined <- floor(2014-pf$tenure/365)
# Using cut to bucket to custom(?) groups
pf$year_joined.bucket <- cut(pf$year_joined, breaks = c(2004,2009,2011,2012,2014))
table(year_joined.bucket)

#Draw seperate line graph for each bucket years plus grand mean
ggplot(aes(x = age, y = friend_count),
       data = subset(pf, !is.na(gender), !is.na(year_joined))) +
  geom_line(stat = 'summary', fun.y=mean, linetype= 2) +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = mean)

#Subset another dataframe to get friend_count/tenure to get friend rate
with(subset(pf, tenure>=1), summary(friend_count/tenure))

ggplot(aes(x = tenure, y = friendships_initiated/tenure),
       data = subset(pf, tenure>= 1, !is.na(friendships_initiated))) +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = mean)

ggplot(aes(x = 7 * round(tenure / 7), y = friendships_initiated / tenure),
       data = subset(pf, tenure > 0)) +
  geom_smooth(aes(color = year_joined.bucket))

#YOGURT DATASET
yo <- read.csv('yogurt.csv')
qplot(data=yo, x=price, binwidth=2)

# Create a new variable called all.purchases,
# which gives the total counts of yogurt for
# each observation or household.
yo <- transform(yo, all.purchases = strawberry+blueberry+pina.colada+plain+mixed.berry)
qplot(data=yo, x=all.purchases, binwidth = 1)

#Creat better vis
ggplot(aes(x=time,y=price),data=yo)+
  geom_jitter(alpha=1/4, shape=21)

# Create seperate linegraph for households
set.seed(42)
sample.ids <- sample(yo$id, 16)
ggplot(aes(x=time, y=price), data=subset(yo,id %in% sample.ids)) +
         facet_wrap(~id)+
         geom_line()+
         geom_point(aes(size=all.purchases), pch = 1)
       