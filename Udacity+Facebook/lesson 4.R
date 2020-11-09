library(ggplot2) #must load the ggplot package first
install.packages('tidyverse')


qplot(x = price, data=diamonds)
# Count, By
table(diamonds$price < 500)
table(diamonds$price < 250)
table(diamonds$price >= 15000)

#quiz 5 # Explore the largest peak in the
# price histogram you created earlier.
qplot(x=price, data=diamonds, xlim=c(250,2000), binwidth=50)

# Break out the histogram of diamond prices by cut.
qplot(x = price, data = diamonds) + scale_x_continuous(breaks=1:31) +
  facet_wrap(~cut, ncol=3)

# Which cut has the lowest median price?
table(diamonds$price)
by(diamonds$price, diamonds$cut, summary)

# Create a histogram of price per carat
# and facet it by cut.
qplot(x = price*(1/carat), data = diamonds)+facet_wrap(~cut)+
  scale_x_log10()    

# Investigate the price of diamonds using box plots,
# numerical summaries, and one of the following categorical
# variables: cut, clarity, or color.
qplot(x=cut,y=price ,data = diamonds, geom = 'boxplot') +
  scale_y_log10()
qplot(x=color,y=price ,data = diamonds, geom = 'boxplot') +
  scale_y_log10()
qplot(x=clarity,y=price ,data = diamonds, geom = 'boxplot') +
  scale_y_log10()
by(diamonds$price/diamonds$carat,diamonds$color, summary)
IQR(subset(diamonds, price/carat>0)$price)

# plot price/carot 
qplot(x=color,y = price/carat,data = diamonds, geom = "boxplot")

#frequency polygon
qplot(x = carat,
      data = diamonds,geom='freqpoly', binwidth=0.01 ) + 
  scale_x_continuous()+scale_x_log10()
