library(ggplot2)
library(gridExtra)
library(dplyr)
library(scales)
data('diamonds')
install.packages('gridExtra')

plot1 <- qplot(data=diamonds, x=price, binwidth = 100) + 
  ggtitle('Price')

plot2 <- qplot(data=diamonds, x=log10(price), binwidth = 0.01) +
  ggtitle('Price (log10)')

grid.arrange(plot1, plot2, ncol =2)

# Fix overplotting
ggplot(aes(carat, price), data = diamonds) + 
  geom_point(position=position_jitter(), alpha=1/25) + 
  scale_x_continuous(x^(1/3),limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat') 

# Adjust the code below to color the points by clarity.
library(RColorBrewer)
ggplot(aes(x = carat, y = price, color=clarity), data = diamonds) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Clarity', reverse = T,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous( limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Clarity')

# Adjust the code below to color the points by cut.
# Change any other parts of the code as needed.
ggplot(aes(x = carat, y = price, color = cut), data = diamonds) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Cut', reverse = T,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Cut')

# Adjust the code below to color the points by diamond colors
# and change the titles.
ggplot(aes(x = carat, y = price, color = color), data = diamonds) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Color',
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Color')
