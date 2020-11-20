setwd('D:/Project/R Scripts/Exploratory-Data-Analysis/Udacity+Facebook')
library(ggplot2)
library(dplyr)
library(tidyverse)
data('diamonds')
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
# 1. Create a histogram of diamond prices.
# Facet the histogram by diamond color
# and use cut to color the histogram bars.
ggplot(diamonds, aes(x=log(price), fill=cut)) + 
  geom_histogram(bins=10) + 
  scale_fill_brewer(type = 'qual') +
  facet_wrap(~color)

# Create a scatterplot of diamond price vs.
# table and color the points by the cut of
# the diamond.
ggplot(diamonds, aes(x=table, y=price, color=cut)) +
  geom_point(position=position_jitter(), alpha=1/4)+
  scale_color_brewer(type = 'qual')

# Create a scatterplot of diamond price vs.
# volume (x * y * z) and color the points by
# the clarity of diamonds. Use scale on the y-axis
# to take the log10 of price. You should also
# omit the top 1% of diamond volumes from the plot.
ggplot(subset(diamonds, x*y*z < quantile(x*y*z, 0.99)), 
       aes(x=x*y*z, y=log10(price), color=clarity)) +
  geom_point(position=position_jitter(), alpha=1/4)+
  scale_color_brewer(type = 'qual')

# Your task is to create a new variable called 'prop_initiated'
# in the Pseudo-Facebook data set. The variable should contain
# the proportion of friendships that the user initiated.
pf$prop_initiated <- pf$friendships_initiated/pf$friend_count

# Create a line graph of the median proportion of
# friendships initiated ('prop_initiated') vs.
# tenure and color the line segment by
# year_joined.bucket.
pf$year_joined <- floor(2014-pf$tenure/365)
pf$year_joined.bucket <- cut(pf$year_joined, breaks = c(2004,2009,2011,2012,2014))
ggplot(aes(x = 25*round(tenure/25), y = prop_initiated),
       data = subset(pf, !is.na(prop_initiated), !is.na(tenure))) +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = median)+
  geom_smooth()

# Smooth the last plot you created of
# of prop_initiated vs tenure colored by
# year_joined.bucket. You can bin together ranges
# of tenure or add a smoother to the plot.
mean(pf$prop_initiated[pf$year_joined.bucket=='(2012,2014]'],na.rm=TRUE)
mean(pf$prop_initiated[pf$year_joined.bucket=='(2003,2009]'],na.rm=TRUE)
mean(pf$prop_initiated[pf$year_joined.bucket=='(2009,2011]'],na.rm=TRUE)
mean(pf$prop_initiated[pf$year_joined.bucket=='(2011,2012]'],na.rm=TRUE)

# Create a scatter plot of the price/carat ratio
# of diamonds. The variable x should be
# assigned to cut. The points should be colored
# by diamond color, and the plot should be
# faceted by clarity.
cost <- diamonds$price/diamonds$carat
ggplot(data=diamonds, aes(x=cut, y=cost,color=color)) +
  facet_wrap(~clarity) +
  geom_point(position=position_jitter(), alpha=0.80)




