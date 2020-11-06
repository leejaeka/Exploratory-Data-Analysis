setwd('D:/Project/R Scripts/Exploratory-Data-Analysis/Udacity+Facebook')
list.files()

install.packages('ggthemes', dependencies = TRUE)
library(ggthemes)
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
names(pf)
library(ggplot2)
# Plots 12 histograms
qplot(x = dob_day, data = pf) + scale_x_continuous(breaks=1:31) +
    facet_wrap(~dob_month, ncol=3)

# Same as above but not histogram
ggplot(aes(x = dob_day), data = pf) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = 1:31)

qplot(x = friend_count, data = pf) 

# Limit the axes
qplot(x = friend_count, data = pf, xlim = c(0,1000))
qplot(x = friend_count, data = pf) + scale_x_continuous(limits = c(0,1000))

# Adjust the Bin Width = bar width
qplot(x = friend_count, data = pf, binwidth = 5) + scale_x_continuous(limits = c(0,1000))

# Facet 
qplot(x = friend_count, data = pf, binwidth = 25) + 
  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000,50))+
    facet_wrap(~gender)

# Omit NA Observations    na.omit(pf) would work as well
qplot(x = friend_count, data = subset(pf,!is.na(gender)), binwidth = 25) + 
  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000,50))+
  facet_wrap(~gender, ncol = 2)

# Count, By
table(pf$gender)
by(pf$friend_count, pf$gender, summary)

# Color and getting years
qplot(x = tenure/365, data = pf, binwidth = 0.25,
      color = I('black'), fill = I('#099009')) +
        scale_x_continuous(breaks = seq(1,7,1), limits = c(0,7))

# Labelling
qplot(x = tenure/365, data = pf, binwidth = 0.25,
      xlab='Number of years using Facebook', ylab="count",
      color = I('black'), fill = I('#099009')) +
  scale_x_continuous(breaks = seq(1,7,1), limits = c(0,7))

# User Ages
qplot(x= age, data=pf, binwidth = 5, color = I('black'), fill = I('pink'))+
    scale_x_continuous(breaks = seq(13,99,5),limits = c(13,90))

# Using log 
logscale <- qplot(x= log10(friend_count), data=pf)
countscale <- qplot(x= (friend_count), data=pf)+scale_x_log10()

# Plot two or more
install.packages('gridExtra')
library(gridExtra)
grid.arrange(logscale, countscale, ncol = 2)

## Frequency Polygons
qplot(x = www_likes,
      data = subset(pf,!is.na(gender)),geom='freqpoly', color=gender ) + 
  scale_x_continuous()+scale_x_log10()

## Who has more in total
by(pf$www_likes, pf$gender, sum)

# Box plots
qplot(x=gender, y = friend_count, data = subset(pf, !is.na(gender)),
      geom = 'boxplot') + scale_y_log10()

by(pf$friendships_initiated, pf$gender, summary)

## Getting Logical
mobile_check_in <- NA
pf$mobile_check_in <- ifelse(pf$mobile_likes > 0,1,0)
pf$mobile_check_in <- factor(pf$mobile_check_in)
summary(pf$mobile_check_in)
63947/length(pf$mobile_check_in)
