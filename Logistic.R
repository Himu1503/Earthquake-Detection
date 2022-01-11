getwd()
list.files()

#Reading the csv file "Earthquake_Data_cleaned_01 (1).csv"

Earthquake <- read.csv("Earthquake_Data_cleaned_01 (1).csv")

print(head(Earthquake))

#str(), gives the structure of the file
str(Earthquake)

#for logistic regression the dependent variable should return in "YES" or "NO" format
#So considering earthquake below 5 as as minor and above 5 as major earthquake
#Changing Magnitude,Latitude,Longitude
Earthquake$mag <- ifelse(test=Earthquake$mag<5, yes='Minor', no='Major')
Earthquake$mag <- as.factor(Earthquake$mag)



#library(dplyr) for selecting pipe filter

library(dplyr)
IC<-c('India','China','Pakistan','Indonesia')
Earthquake %>% filter(Earthquake$country %in% IC)->IC_Earth
View(Earthquake)
View(IC_Earth)
str(IC_Earth)



xtabs(~ mag +country, data=IC_Earth)

#mag     China India Indonesia Pakistan
#Major    49    35       390        8
#Minor   185   148      1224       33


logistic <- glm(mag~ country, data=IC_Earth, family ="binomial")
summary(logistic)


ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2
(ll.null-ll.proposed)/ll.null

1 - pchisq(2*(ll.proposed - ll.null), df=(length(logistic$coefficients)-1))



predicted.data <- data.frame(
  probability.of.mag=logistic$fitted.values,
  country=IC_Earth$country)

predicted.data<- predicted.data[
  order(predicted.data$probability.of.mag, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

library(ggplot2)
install.packages("cowplot")
library(cowplot)

ggplot(data=predicted.data, aes(x=probability.of.mag, y=country)) +
geom_point(aes(color=country), size=5) +
xlab("mag") +
ylab("country")

