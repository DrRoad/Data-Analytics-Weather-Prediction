#1. LOAD LIBRARIES
library(dplyr)
library(tidyr)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
library(choroplethrAdmin1)

#2. IMPORT DATA
my_data= read.csv("GlobalLandTemperaturesByState.csv")
my_data = na.omit(my_data)

#3. FILTER DATA FOR UNITED STATES 
dataus = my_data %>% filter(Country=="United States") %>% separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE) 
dataus = dataus%>% filter(State!="Hawaii" & State!="Alaska")
dataus = na.omit(dataus)	  
dataus$State = as.character(dataus$State)
dataus$State[dataus$State=="Georgia (State)"] = "Georgia"
dataus$State = as.factor(dataus$State)   		  			  
dataus2 = dataus %>% select(Year,AverageTemperature,State) %>% group_by(Year,State) %>% dplyr:: summarise(value=mean(AverageTemperature))
colnames(dataus2)[2] = "region"
dataus2$region = tolower(dataus2$region)

#4. FILTER DATA FOR UNITED STATES FOR YEAR 1850 AND SELECT REGION AND AVERAGE TEMPERATURE COLUMN 
dataus1850 = dataus2 %>% filter(Year==1850) 
dataus1850 = dataus1850[,c(2,3)]

#PLOT DISPLAYING UNITED SATES MAP WITH TEMPERATURE OF YEAR 1850 
print(state_choropleth(dataus1850, title="Average Temperatures in United States (1850)", num_colors = 8, legend="Degrees"),reference_map=TRUE)

#5. FILTER DATA FOR UNITED STATES FOR YEAR 2013 AND SELECT REGION AND AVERAGE TEMPERATURE COLUMN 
dataus2013 <- dataus2 %>% filter(Year==2013) 
dataus2013 <- dataus2013[,c(2,3)]

#PLOT DISPLAYING UNITED SATES MAP WITH TEMPERATURE OF YEAR 2013 
print(state_choropleth(dataus2013, title="Average Temperatures in United States (2013)", num_colors = 8, legend="Degrees"),reference_map=TRUE)

#6. AVERAGE TEMPERAURE DIFFERENCE FROM YEAR 1850 TO 2013
tempdiffus1850_2013 <- as.data.frame(dataus2013$value-dataus1850$value)
tempdiffus1850_2013<- cbind(dataus2013$region, tempdiffus1850_2013)
colnames(tempdiffus1850_2013)[1]<- "region"
colnames(tempdiffus1850_2013)[2]<- "value"

#PLOT DISPLAYING UNITED SATES MAP WITH AVERAGE TEMPERATURE DIFFERENCE OF YEAR 1850 TO 2013 
print(state_choropleth(tempdiffus1850_2013, title="Average Temperature Increase in United States (1850-2013)", num_colors = 8, legend="Degrees"),reference_map=TRUE)

#7. PLOT OF AVERAGE TEMPERATURE INCREASE 
usmeantemp = dataus %>% filter(Year>1850) %>% group_by(Year) %>% dplyr:: summarise(Temp = mean(AverageTemperature))
q=qplot(Year, Temp, data=usmeantemp, main="US Average Temperature 1850-2013",geom=c("point","smooth"))+ aes(colour = Temp) + scale_color_gradient(low="blue", high="red")
q

#8. BOX PLOT FOR AVERAGE TEMPERATURE INCREASE 
usyeartemp= dataus %>% filter(Year==1850 | Year==1890 | Year==1930 | Year==1970 | Year==2013) %>% group_by(Year, State) %>% dplyr:: summarise(Temp = mean(AverageTemperature)) 
usyeartemp$Year <- as.factor(usyeartemp$Year)
qplot(x =  Year, y = Temp, data = usyeartemp) + ggtitle("Average Temperature for 40 Year Intervals")+geom_boxplot(fill="grey")

