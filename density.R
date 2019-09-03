library(ggplot2)
library(gridExtra)
library(plyr)
library(grid)
library(readxl)
library(matrixStats)
library(tidyverse)

#reading the excel file 
pop_den = read_excel("population_density.xls")

#Since 1960 column doesn't contains any value we will drop this column. 
pop_den$'1960' <- NULL 


#Showing the head of the data
head(pop_den)

#Finding the mean of all the population and storing it 
pop_mean <- colMeans(pop_den[5:62], na.rm=TRUE)
pop_mean

#plotting the mean in barplot, density plot and histogram
barplot(pop_mean)
d1 <- density(pop_mean)
plot(d1)
hist(pop_mean, xlab = "population mean", col = "red")

#plotting a line curve using ggplot for mean to understand the curve 
date <- 1961:2018
ggplot() + geom_line(aes(x=date,y=pop_mean),color='red') + 
  ylab('Values')+xlab('date')

#Finding the median of all the population and storing it
pop_med <- colMedians( as.matrix(pop_den[5:62]), na.rm=TRUE)
pop_med

#plotting the median in barplot, density plot and histogram
barplot(pop_med, names = seq(1961, 2018, 1), las = 2)
d2 <- density(pop_med)
plot(d2)
hist(pop_med, xlab = "population median", col = " blue")

#plotting a line curve using ggplot for median to understand the curve 
date <- 1961:2018
ggplot() + geom_line(aes(x=date,y=pop_med),color='red') + 
  ylab('Values')+xlab('date')

#plotting population density of different countries using ggplot
x1 <- 1961:2018
y1 <- pop_den[39,5:62]
y2 <- pop_den[108,5:62]
y3 <- pop_den[250,5:62]
t <- data.frame(cbind(x1, t(y1),t(y2),t(y3)))
colnames(t) <- c("year", "China", "India", "US")
country <- melt(t, id= "year")
ggplot(data = country , aes(x = year, y = value, color = variable)) + geom_line()


#comparison of population density for two years(2018,2008) at a span of 10 years. 
x1 <- pop_den$`2008`
x2 <- pop_den$`2018`

y1 <- 1:264 #labelled the countries with numbers 
t <- data.frame(cbind(y1, x1, x2))
colnames(t) <- c("Country", "2008", "2018")
dat <- melt(t, id= "Country")
ggplot(data=dat, aes(x=Country,y=value,fill=variable)) +
  geom_bar(position=position_dodge(), stat= 'identity') 

par(mfrow=c(1,2))
hist(x1, main = "2008", xlab = "population density", col = 'red')
hist(x2, main = "2018", xlab = "population density", col = 'green')






