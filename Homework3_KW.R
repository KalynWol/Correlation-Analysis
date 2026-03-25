#Geography 411 – Spring 2026 
#Homework #3 - Correlation 
#Kalyn Wolters 

wdt <- read.csv("WorldDataTable.csv")
wdt[1:10,]
head(wdt, 10)

#plotting histograms 
hist(wdt$LifeExpectancy) 
hist(wdt$GDPPerCapita) 

#Scatterplot 
plot(wdt$GDPPerCapita,wdt$LifeExpectancy) 

#Pearsons correlation test
r <- cor(wdt$GDPPerCapita,wdt$LifeExpectancy,method = "pearson")
r

n <- length(wdt$GDPPerCapita)

t <- (r * sqrt(n-2)) / sqrt(1 - r^2) 

tcrit <- qt(p = 0.025, df = n-2, lower.tail = TRUE)

cor.test(wdt$GDPPerCapita,wdt$LifeExpectancy,method = "pearson")
#Spearman's correlation test
rs <- cor(wdt$GDPPerCapita,wdt$LifeExpectancy,method = "spearman")
rs

ts <- rs * sqrt(n-1) 

tcrits <- qt(p = 0.025, df = n-1, lower.tail = TRUE)

cor.test(wdt$GDPPerCapita,wdt$LifeExpectancy,method = "spearman")

#Labelled scatter plot 
plot(wdt$GDPPerCapita,wdt$LifeExpectancy, col = "white")
text(wdt$GDPPerCapita,wdt$LifeExpectancy, labels = wdt$Country, cex = 0.5)

