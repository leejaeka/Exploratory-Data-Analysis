library(ggplot2)
library(dplyr)
data('diamonds')

#Scatterplot
qplot(x = x, y = price, data = diamonds)
#correlation
cor.test(diamonds$x, diamonds$price, method='pearson')
cor.test(diamonds$y, diamonds$price, method='pearson')
cor.test(diamonds$price, diamonds$z, method='pearson')
cor.test(diamonds$depth, diamonds$price, method='pearson')
summary(diamonds$depth)
#transparency and breaks
ggplot(data = diamonds, aes(x = depth, y = price)) + 
  geom_point(alpha=1/20) + scale_x_continuous(breaks=seq(0,79,2))
## Create a scatterplot of price vs carat
# and omit the top 1% of price and carat values.
qplot(data=diamonds, y=price, x=carat) +ylim(0,quantile(diamonds$price, 0.99))+
  xlim(0,quantile(diamonds$carat, 0.99))

# Create a scatterplot of price vs. volume (x * y * z).
# This is a very rough approximation for a diamond's volume.
diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
qplot(x=price, y=volume, data=diamonds)

#Correlation of price and volume, 0 < volume < 800
dia_sub <- subset(diamonds, diamonds$volume<800 & diamonds$volume>0)
cor.test(dia_sub$price, dia_sub$volume)

# Subset the data to exclude diamonds with a volume
# greater than or equal to 800. Also, exclude diamonds
# with a volume of 0. Adjust the transparency of the
# points and add a linear model to the plot. (See the
# Instructor Notes or look up the documentation of
# geom_smooth() for more details about smoothers.)
qplot(x=volume, y=price, data=dia_sub) + 
  stat_smooth(method = "lm", formula = y ~ x, size = 1) +
  geom_point(alpha=1/20)

# Use the function dplyr package
# to create a new data frame containing
# info on diamonds by clarity.

# Name the data frame diamondsByClarity

# The data frame should contain the following
# variables in this order.

#       (1) mean_price
#       (2) median_price
#       (3) min_price
#       (4) max_price
#       (5) n

# where n is the number of diamonds in each
# level of clarity.
dia_cla <- group_by(diamonds, clarity)
diamondsByClarity<- summarise(dia_cla,mean_price=mean(price),
                                 median_price = median(price),
                                 min_price = min(price),
                                 max_price = max(price),
                                 n=n())


# Your task is to write additional code to create two bar plots
# on one output image using the grid.arrange() function from the package
# gridExtra.
diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))
diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

library(gridExtra)
p1 <- barplot(diamonds_mp_by_clarity$mean_price)
p2 <- barplot(diamonds_mp_by_color$mean_price)  
grid.arrange(p3,p4, ncol=2, top="hello")
