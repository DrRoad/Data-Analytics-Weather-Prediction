#1. LOAD LIBRARIES
library(ggplot2)
library(reshape2)

#2. IMPORT DATA
myd=read.csv("CO2_per_capita_1970-2013_dataset_of_CO2_report_2014.csv") 

#3. IMPORT DATA INTO VARIABLE
year= myd[,1]
Australia= myd[,11]
UnitedStates = myd[,198]
UnitedKingdom = myd[,197]
Germany=myd[,72]
China= myd[,40]
India= myd[,89]
Argentina= myd[,8]
Bangladesh=myd[,16]

#4. STORE DATA IN DATA FRAME AND PLOT FOR DEVELOPED AND DEVELOPING COUNTRIES
df <- data.frame(year, Australia, UnitedStates, UnitedKingdom, Germany, China, India, Argentina, Bangladesh)
# melt the data to a long format
df2 <- melt(data = df, id.vars = "year", variable.name = 'Country')
# plot, using the aesthetics argument 'colour'
g=ggplot(data = df2, aes(x = year, y = value, colour = Country)) + geom_line()
g

