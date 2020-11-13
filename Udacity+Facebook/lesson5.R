setwd('D:/Project/R Scripts/Exploratory-Data-Analysis/Udacity+Facebook')
library(ggplot2)
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')

qplot(x = age, y = friend_count, data = pf)

# scatterplot with points with transparency for problems with clusters
ggplot(aes(x = age, y = friend_count), data = pf) + 
  geom_point(alpha=1/20, position = position_jitter(h=0)) + 
    xlim(13,90) + coord_trans(y = "sqrt")

ggplot(aes(x = age, y = friendships_initiated), data=pf) + 
  ylim(0, 4000) + geom_point(alpha=1/20, position = position_jitter(h=0)) +
    coord_trans(y = "sqrt")

install.packages('dplyr')
library(dplyr)

## Using dplyr and making new dataframe variable
age_groups <- group_by(pf, age)
pf.fc_by_age <- summarise(age_groups,
          friend_count_mean = mean(friend_count),
          friend_count_median = median(friend_count),
          n = n())
pf.fc_by_age <- arrage(pf, fc_by_age, age)
head(pf.fc_by_age)

pf.fc_by_age %>%
  group_by(age) %>%
  summarise(friend_count_mean = mean(friend_count),
            friend_count_median = median(friend_count),
            n = n()) %>%
  arrange(age)

#line plot
ggplot(aes(x = age, y = friend_count_mean), data = pf.fc_by_age)+geom_line()

# Line plot + Scatterplot in one graph
ggplot(aes(x = age, y = friend_count), data = pf) + 
  xlim(13,90) +
  geom_point(alpha=1/20, position = position_jitter(h=0), color='orange') + 
  coord_trans(y = 'sqrt') + 
  geom_line(stat = 'summary', fun.y=mean) +
  geom_line(stat = 'summary', fun.y=quantile, fun.args = list(probs = .1), linetype=2, color='blue')+
  geom_line(stat = 'summary', fun.y=quantile, fun.args = list(probs = .5),color='blue')+
  geom_line(stat = 'summary', fun.y=quantile, fun.args = list(probs = .9),linetype=2, color='blue')

## Correlation with focus in area
with(subset(pf, age<=70), cor.test(pf$age, pf$friend_count, method='pearson'))

ggplot(aes(x=likes_received, y = www_likes_received), data=pf ) + 
  geom_point(alpha=1/20, position = position_jitter(h=0), color='pink')+
  xlim(0,quantile(pf$www_likes_received, 0.95))+
  ylim(0,quantile(pf$likes_received, 0.95)) +
  geom_smooth(method='lm', color='red')

cor.test(pf$likes_received, pf$www_likes_received, method='pearson')

# Create a new variable, 'age_with_months', in the 'pf' data frame.
# Be sure to save the variable in the data frame rather than creating
# a separate, stand-alone variable. You will need to use the variables
# 'age' and 'dob_month' to create the variable 'age_with_months'.
pf$age_with_months <- pf$age + (12-pf$dob_month)/12

# Create a new data frame called
# pf.fc_by_age_months that contains
# the mean friend count, the median friend
# count, and the number of users in each
# group of age_with_months. The rows of the
# data framed should be arranged in increasing
# order by the age_with_months variable.
age_with_months_groups <- group_by(pf, age_with_months)
pf.fc_by_age_months <- summarise(age_with_months_groups,
                                 friend_count_mean = mean(friend_count),
                                 friend_count_median = median(friend_count),
                                 n=n())
pf.fc_by_age_months <- arrange(pf.fc_by_age_months, age_with_months)

#line plot
ggplot(aes(x = age_with_months, y = friend_count_mean), data = subset(pf.fc_by_age_months, age_with_months<71))+geom_line()
       