library(ggplot2)
library(gridExtra)
library(plyr)
library(grid)
library(readxl)
library(matrixStats)

#loading the xls file
forest_den = read_excel("forest.xls")

#removing the columns which does not contain any value
head(forest_den)
forest_den[,5:34] <- NULL
forest_den[,32:33] <- NULL

#Showing the head of the data
head(forest_den)

#Finding the mean of the forest density and storing it 
forest_mean <- colMeans(forest_den[5:31], na.rm=TRUE)
forest_mean

#plotting the mean in barplot, density plot and histogram
barplot(forest_mean)
d1 <- density(forest_mean)
plot(d1)
hist(forest_mean, xlab = "forest mean", col = "red")

#plotting a line curve using ggplot for mean to understand the curve 
date <- 1990:2016
ggplot() + geom_line(aes(x=date,y=forest_mean),color='red') + 
  ylab('Values')+xlab('date')

#Finding the median of the forest density and storing it
forest_med <- colMedians( as.matrix(forest_den[5:31]), na.rm=TRUE)
forest_med

#plotting the median in barplot, density plot and histogram
barplot(pop_med, names = seq(1961, 2018, 1), las = 2)
d2 <- density(pop_med)
plot(d2)
hist(pop_med, xlab = "population median", col = " blue")

#plotting a line curve using ggplot for median to understand the curve 
date <- 1961:2018
ggplot() + geom_line(aes(x=date,y=pop_med),color='red') + 
  ylab('Values')+xlab('date')


#plotting forest density of different countries using ggplot
x1 <- 1990:2016
y1 <- forest_den[39,5:31]
y2 <- forest_den[108,5:31]
y3 <- forest_den[250,5:31]
t <- data.frame(cbind(x1, t(y1),t(y2),t(y3)))
colnames(t) <- c("year", "China", "India", "US")
country <- melt(t, id= "year")
ggplot(data = country , aes(x = year, y = value, color = variable)) + geom_line()

#comparison of forest density for two years(2018,2008) at a span of 10 years. 
x1 <- forest_den$`2008`
x2 <- forest_den$`2016`

y1 <- 1:264 #labelled the countries with numbers 
t <- data.frame(cbind(y1, x1, x2))
colnames(t) <- c("Country", "2006", "2016")
dat <- melt(t, id= "Country")
ggplot(data=dat, aes(x=Country,y=value,fill=variable)) +
  geom_bar(position=position_dodge(), stat= 'identity') 


par(mfrow=c(1,2))
hist(x1, main = "2006", xlab = "forest density", col = 'red')
hist(x2, main = "2016", xlab = "forest density", col = 'green')

