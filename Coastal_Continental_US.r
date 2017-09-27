#1. LOAD LIBRARIES
library(dplyr)
library(tidyr)
library(ggplot2)

#2. IMPORT DATA
#  Load data with variable names into the data frame "dataus"
dataus= read.csv("GlobalLandTemperaturesByState.csv");
dataus= na.omit(dataus)
# Filter data: Select country "United States"
dataustemp <- dataus%>% filter(Country=="United States") %>% separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE) 

# Filter Data: Select United States Continental region states "Illinois", "Iowa", "Colorado" for year greater than 1950
dataus_Inner <- dataustemp %>% 
              filter(State=="Illinois" | State =="Iowa" | State =="Colorado" ) %>%
              filter(Year>1950) %>%
              group_by(Year,State)%>%
              dplyr:: summarise(AverageTemperature=mean(AverageTemperature))

#Plot for Continental region of United States
ggplot(dataus_Inner, aes(x=Year, y=AverageTemperature)) +
               geom_point(color='red') +
               facet_wrap(~ State)+
               geom_smooth()

# Filter Data: Select United States Coastal region states "California", "Washington", "Florida"	 for year greater than 1950		   
dataus_Coastal <- dataustemp %>% 
              filter(State=="California" | State =="Washington" | State =="Florida" ) %>%
              filter(Year>1950) %>%
              group_by(Year,State)%>%
              dplyr:: summarise(AverageTemperature=mean(AverageTemperature))
			  
#Plot for Coastal region of United States
ggplot(dataus_Coastal, aes(x=Year, y=AverageTemperature)) +
               geom_point(color='red') +
               facet_wrap(~ State)+
               geom_smooth()