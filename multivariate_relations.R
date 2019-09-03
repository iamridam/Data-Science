library(ggplot2)
library(gridExtra)
library(plyr)
library(grid)
library(readxl)
library(matrixStats)

#loading the xls file
forest_den = read_excel("forest.xls")
pop_den = read_excel("population_density.xls")


forest_den[,c(5:34, 62:63)] <- NULL
head(forest_den)

pop_den[,c(5:34, 62:63)] <- NULL
head(pop_den)

pop_for <- merge( forest_den ,pop_den , by="Country Name")
head(pop_for)


#Plotting forest density and population density for Brazil 
brazil <- pop_for %>% slice(28)
xx = t(brazil[c(5:31)])
yy = t(brazil[c(35:61)])
zz = cbind(xx,yy)
zz = as.data.frame(zz)

x1 <- 1990:2016
y1 <- zz[,1]
y2 <- zz[,2]

t <- data.frame(cbind(x1,y1,y2))
colnames(t) <- c("year", "Forest density", "Population density")
country <- melt(t, id= "year")
ggplot(data = country , aes(x = year, y = value, color = variable)) + geom_line()


#Plotting forest density and population density for India
india <- pop_for %>% slice(108)
xx = t(india[c(5:31)])
yy = t(india[c(35:61)])
zz = cbind(xx,yy)
zz = as.data.frame(zz)

x1 <- 1990:2016
y1 <- zz[,1]
y2 <- zz[,2]

t <- data.frame(cbind(x1,y1,y2))
colnames(t) <- c("year", "Forest density", "Population density")
country <- melt(t, id= "year")
ggplot(data = country , aes(x = year, y = value, color = variable)) + geom_line()


## carbon_emision and forest density comparison

forest_den = read_excel("forest.xls")
carbon_emision = read_excel("carbondioxide.xls")


forest_den[,5:34] <- NULL
forest_den[,32:33] <- NULL
head(forest_den)

carbon_emision[,5:34] <- NULL
carbon_emision[,32:33] <- NULL
head(carbon_emision)

for_car <- merge( forest_den , carbon_emision, by="Country Name")
head(for_car)

#Plotting forest density vs carbon emision in India in recent past
india <- for_car %>% slice(108)
xx = t(india[c(5:29)])
yy = t(india[c(35:59)])
zz = cbind(xx,yy)
zz = as.data.frame(zz)

x1 <- 1990:2014
y1 <- zz[,1]
y2 <- zz[,2]

t <- data.frame(cbind(x1,y1,y2))
colnames(t) <- c("year", "Forest", "carbon")


coef(lm(carbon ~ Forest, data = t))
ggplot(data=t, aes(x= Forest, y= carbon))+
  xlab("Forest density")+ ylab("carbon emision")+
  geom_point()+ 
  geom_abline(intercept = 1619474.05, slope = -46487.65   ) +
  ggtitle(" Forest density vs Carbon emision in India")


temperature1 = read_excel("temp.xlsx")
temp2 = temperature1[-c(1:111),]
temp2 = data.frame(temp2)
temp2
rise <- melt(temp2, id= "Year")
ggplot(data = rise , aes(x = Year, y = value, color = variable)) + geom_line() + xlim (1990,2020)

year <- 1990:2016
US <- t(forest_den[250, 5:31])
rownames(US) = NULL
US_temp = cbind(year, US)

colnames(US_temp) <- c("Year","forest")

temp_for <- merge(temp2, US_temp, by="Year")
temp_for

ggplot(data=temp_for, aes(x= Lowess.5., y= forest))+
  xlab("temperature")+ ylab("forest")+
  geom_point()+ 
   ggtitle(" Temperature vs Forest")


year <- 1990:2014
US <- t(carbon_emision[250, 5:29])
rownames(US) = NULL
US_carbon = cbind(year, US)

colnames(US_carbon) <- c("Year","Carbon")

carbon_for <- merge(temp2, US_carbon, by="Year")
carbon_for

ggplot(data=carbon_for, aes(x= Lowess.5. , y= Carbon))+
  xlab("temperature")+ ylab("carbon_conc.")+
  geom_point()+ 
  ggtitle(" Temperature vs Carbon_conc.")








