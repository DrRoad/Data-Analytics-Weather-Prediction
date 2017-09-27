#1. LOAD LIBRARIES
library(dplyr)
library(tidyr)

#2. IMPORT DATA
my_data= read.csv("GlobalLandTemperaturesByCountry.csv")
my_data<-na.omit(my_data)
## Load data into variable by using filter for Country "United States"
dataus <- my_data %>% filter(Country=="United States") %>% separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE) 
dataus2 <- dataus %>% select(Year,AverageTemperature) %>% group_by(Year) %>% dplyr:: summarise(value=mean(AverageTemperature))
dataus1970 <- dataus2 %>% filter(Year>=1970) 
colnames(dataus1970)[2]<- "Temperature"

#3. IMPORT DATA FROM CO2 DATA SET AND FETCHING UNITED STATES CO2 DATA INTO VARIABLE
myd=read.csv("CO2_per_capita_1970-2013_dataset_of_CO2_report_2014.csv") 
uscodata=myd[, c(1,198)]
colnames(uscodata)[2]<- "CO2US"

#4.CODE TO MERGE THE DATA AND WRITING IT INTO .csv FILE 
us_mergedData <- merge(dataus1970, uscodata, by.dataus1970=c("Year"),byuscodata=c("Year"))
write.csv(us_mergedData, file = "MyDataus.csv", row.names=FALSE)

#5. UNITED STATES DATA ARE STORED IN THE VARIABLE 
ustemp= us_mergedData$Temperature 
usco2= mergedData$CO2US
year=mergedData$Year

#6. PLOT BETWEEN UNITED STATES TEMPERATURE AND CO2 AND CORRELATION BETWEEN THEM 
plot(ustemp, usco2)
cor(ustemp, usco2, method="pearson")

#7. BUILDING MODEL 
model_us=lm(ustemp~usco2)
summary(model_us)

#8. NORMALITY TEST
qqnorm(rstandard(model_us))
qqline(rstandard(model_us),col="red")
shapiro.test(usco2)

#9. RESIDUAL ANALYSIS 
plot(fitted(model_us),rstandard(model_us),main="Predicted Vs Residual")
abline(a=0, b=0, col='red')
plot(usco2,rstandard(model_us),main="Predicted Vs Residual")
abline(a=0, b=0, col='red')

##INDIA
## Load data into variable by using filter for Country "INDIA"
dataindia <- my_data %>% filter(Country=="India") %>% separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE) 
dataindia2 <- dataindia %>% select(Year,AverageTemperature) %>% group_by(Year) %>% dplyr:: summarise(value=mean(AverageTemperature))
dataindia1970 <- dataindia2 %>% filter(Year>=1970) 
colnames(dataindia1970)[2]<- "Temperature"

#3. IMPORT DATA FROM CO2 DATA SET AND FETCHING INDIA CO2 DATA INTO VARIABLE
myd=read.csv("CO2_per_capita_1970-2013_dataset_of_CO2_report_2014.csv") 
indiacodata=myd[, c(1,89)]
colnames(indiacodata)[2]<- "CO2India"

#4.CODE TO MERGE THE DATA AND WRITING IT INTO .csv FILE 
mergedDataindia <- merge(dataindia1970, indiacodata, by.dataindia1970=c("Year"),byindiacodata=c("Year"))
write.csv(mergedDataindia, file = "MyDataindia.csv", row.names=FALSE)

#5. INDIA DATA ARE STORED IN THE VARIABLE 
indiatemp= mergedDataindia$Temperature 
indiaco2= mergedDataindia$CO2India
year=mergedDataindia$Year

#6. PLOT BETWEEN INDIA TEMPERATURE AND CO2 AND CORRELATION BETWEEN THEM 
plot(indiatemp, indiaco2)
cor(indiatemp, indiaco2, method="pearson")

#7. BUILDING MODEL 
model_india=lm(indiatemp~indiaco2)
summary(model_india)

#8. NORMALITY TEST
qqnorm(rstandard(model_india))
qqline(rstandard(model_india),col="red")
shapiro.test(indiaco2)

#9. RESIDUAL ANALYSIS 
plot(fitted(model_india),rstandard(model_india),main="Predicted Vs Residual")
abline(a=0, b=0, col='red')
plot(indiaco2,rstandard(model_india),main="Predicted Vs Residual")
abline(a=0, b=0, col='red')

#CHINA
## Load data into variable by using filter for Country "INDIA"
datachina <- my_data %>% filter(Country=="China") %>% separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE) 
datachina2 <- datachina %>% select(Year,AverageTemperature) %>% group_by(Year) %>% dplyr:: summarise(value=mean(AverageTemperature))
datachina1970 <- datachina2 %>% filter(Year>=1970) 
colnames(datachina1970)[2]<- "Temperature"

#3. IMPORT DATA FROM CO2 DATA SET AND FETCHING INDIA CO2 DATA INTO VARIABLE
myd=read.csv("CO2_per_capita_1970-2013_dataset_of_CO2_report_2014.csv") 
chinacodata=myd[, c(1,40)]
colnames(chinacodata)[2]<- "CO2China"

#4.CODE TO MERGE THE DATA AND WRITING IT INTO .csv FILE 
mergedDatachina <- merge(datachina1970, chinacodata, by.datachina1970=c("Year"),bychinacodata=c("Year"))
write.csv(mergedDatachina, file = "MyDatachina.csv", row.names=FALSE)
plot(mergedDatachina)

#5. INDIA DATA ARE STORED IN THE VARIABLE 
chinatemp= mergedDatachina$Temperature 
chinaco2= mergedDatachina$CO2China
year=mergedDatachina$Year

#6. PLOT BETWEEN INDIA TEMPERATURE AND CO2 AND CORRELATION BETWEEN THEM 
plot(chinatemp, chinaco2)
cor(chinatemp, chinaco2, method="pearson")

#7. BUILDING MODEL 
model_china=lm(chinatemp~chinaco2)
summary(model_china)

#8. NORMALITY TEST
qqnorm(rstandard(model_china))
qqline(rstandard(model_china),col="red")
shapiro.test(chinaco2)

#9. RESIDUAL ANALYSIS 
plot(fitted(model_china),rstandard(model_china),main="Predicted Vs Residual")
abline(a=0, b=0, col='red')
plot(chinaco2,rstandard(model_china),main="Predicted Vs Residual")
abline(a=0, b=0, col='red')

#UK
## Load data into variable by using filter for Country "UNITED KINGDOM"
datauk <- my_data %>% filter(Country=="United Kingdom") %>% separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE) 
datauk2 <- datauk %>% select(Year,AverageTemperature) %>% group_by(Year) %>% dplyr:: summarise(value=mean(AverageTemperature))
datauk1970 <- datauk2 %>% filter(Year>=1970) 
colnames(datauk1970)[2]<- "Temperature"

#3. IMPORT DATA FROM CO2 DATA SET AND FETCHING UNITED KINGDOM CO2 DATA INTO VARIABLE
myd=read.csv("CO2_per_capita_1970-2013_dataset_of_CO2_report_2014.csv") 
ukcodata=myd[, c(1,197)]
colnames(ukcodata)[2]<- "CO2UK"

#4.CODE TO MERGE THE DATA AND WRITING IT INTO .csv FILE 
mergedDatauk <- merge(datauk1970, ukcodata, by.datauk1970=c("Year"),byukcodata=c("Year"))
write.csv(mergedDatauk, file = "MyDatauk.csv", row.names=FALSE)
plot(mergedDatauk)

#5. UNITED KINGDOM DATA ARE STORED IN THE VARIABLE 
uktemp= mergedDatauk$Temperature 
ukco2= mergedDatauk$CO2UK
year=mergedData$Year

#6. PLOT BETWEEN UNITED KINGDOM TEMPERATURE AND CO2 AND CORRELATION BETWEEN THEM 
plot(uktemp, ukco2)
cor(uktemp, ukco2, method="pearson")

#7. BUILDING MODEL 
model_uk=lm(uktemp~ukco2)
summary(model_uk)

#8. NORMALITY TEST
qqnorm(rstandard(model_uk))
qqline(rstandard(model_uk),col="red")
shapiro.test(ukco2)

#9. RESIDUAL ANALYSIS 
plot(fitted(model_uk),rstandard(model_uk),main="Predicted Vs Residual")
abline(a=0, b=0, col='red')
plot(ukco2,rstandard(model_uk),main="Predicted Vs Residual")
abline(a=0, b=0, col='red')

 
