library(ggplot2)
library(gridExtra)
library(plyr)
library(grid)
library(readxl)
library(matrixStats)

carbon_emision = read_excel("carbondioxide.xls")
head(carbon_emision)

carbon_emision[,60:63] <- NULL
head(carbon_emision)

#Finding the mean of all countries carbon emision and storing it 
carbon_mean <- colMeans(carbon_emision[5:59], na.rm=TRUE)
carbon_mean

#plotting the mean in barplot, density plot and histogram
barplot(carbon_mean)
d1 <- density(carbon_mean)
plot(d1)
hist(carbon_mean, xlab = "carbon_emision mean", col = "red")

#plotting a line curve using ggplot for mean to understand the curve 
date <- 1960:2014
ggplot() + geom_line(aes(x=date,y=carbon_mean),color='red') + 
  ylab('Values')+xlab('date')

#Finding the median of all countries carbon emision and storing it
carbon_med <- colMedians( as.matrix(carbon_emision[5:59]), na.rm=TRUE)
carbon_med

#plotting the median in barplot, density plot and histogram
barplot(carbon_med, names = seq(1960, 2014, 1), las = 2)
d2 <- density(carbon_med)
plot(d2)
hist(carbon_med, xlab = "carbon_emision median", col = " blue")

#plotting a line curve using ggplot for median to understand the curve 
date <- 1960:2014
ggplot() + geom_line(aes(x=date,y=carbon_med),color='red') + 
  ylab('Values')+xlab('date')

#plotting carbon emision of different countries using ggplot
x1 <- 1960:2014
y1 <- carbon_emision[39,5:59]
y2 <- carbon_emision[108,5:59]
y3 <- carbon_emision[250,5:59]
t <- data.frame(cbind(x1, t(y1),t(y2),t(y3)))
colnames(t) <- c("year", "China", "India", "US")
country <- melt(t, id= "year")
ggplot(data = country , aes(x = year, y = value, color = variable)) + geom_line()


#comparison of carbon emision for two years(2010,2014) . 
x1 <- carbon_emision$`2010`
x2 <- carbon_emision$`2014`

y1 <- 1:264 #labelled the countries with numbers 
t <- data.frame(cbind(y1, x1, x2))
colnames(t) <- c("Country", "2010", "2014")
dat <- melt(t, id= "Country")
ggplot(data=dat, aes(x=Country,y=value,fill=variable)) +
  geom_bar(position=position_dodge(), stat= 'identity') 

par(mfrow=c(1,2))
hist(x1, main = "2010", xlab = "carbon emision", col = 'red')
hist(x2, main = "2014", xlab = "carbon emision", col = 'green')
