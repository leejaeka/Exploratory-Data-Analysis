setwd('D:/Project/R Scripts/Exploratory-Data-Analysis/Udacity+Facebook')
library(ggplot2)
library(dplyr)
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
  
  


