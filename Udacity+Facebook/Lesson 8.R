library(ggplot2)
library(gridExtra)
data('diamonds')
install.packages('gridExtra')

plot1 <- qplot(data=diamonds, x=price, binwidth = 100) + 
  ggtitle('Price')

plot2 <- qplot(data=diamonds, x=log10(price), binwidth = 0.01) +
  ggtitle('Price (log10)')

grid.arrange(plot1, plot2, ncol =2)

