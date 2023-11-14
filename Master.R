#Authors: Jakob Anderson, Nicole Coates, Carrie Cox
# Data Cleaning & Processing

rm(list=ls())
library(readr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(choroplethr)
library(choroplethrMaps)
library(reshape2)
library(lubridate)

df <- read_csv("ClassData/group05/US_Accidents_Dec21_updated.csv")

# Provide a summary table of your data that includes, for example, how many rows, columns, missing values 

total_na<- 0
for(n in sum(is.na(df))){
  total_na= total_na + n
}
sum_data <- summarize(df, NA_total=total_na, Row_count=nrow(df), Column_count=ncol(df))

# drop NAs
df <- df[complete.cases(df), ]

# Remove unwanted columns

df$Civil_Twilight <- NULL
df$Nautical_Twilight <- NULL
df$Astronomical_Twilight <- NULL
df$End_Lat <- NULL
df$End_Lng <- NULL
df$Airport_Code <- NULL
df$Weather_Timestamp <- NULL

# Add new column Date; with only date of accident

df$Date <- as.Date(df$Start_Time, format="%M-%d-%Y")

# Only look at data from years 2019-2021

df <- subset(df, as.Date(df$Date) >= as.Date("2019-01-01"))

# Order data by start time

df <- df[order(df$Start_Time), ]

# Change data types

df$Severity <- factor(df$Severity)
levels(df$Severity)

# Change column names

names(df)[names(df) == "Sunrise_Sunset"] <- "Day_or_Night"
names(df)[names(df) == "Precipitation(in)"] <- "Precipitation"
names(df)[names(df) == "Wind_Speed(mph)"] <- "Wind_Speed"
names(df)[names(df) == "Visibility(mi)"] <- "Visibility"
names(df)[names(df) == "Pressure(in)"] <- "Pressure"
names(df)[names(df) == "Humidity(%)"] <- "Humidity"
names(df)[names(df) == "Wind_Chill(F)"] <- "Wind_Chill"
names(df)[names(df) == "Temperature(F)"] <- "Temperature"
names(df)[names(df) == "Side"] <- "Road_Side"
names(df)[names(df) == "Number"] <- "Street_Number"
names(df)[names(df) == "Distance(mi)"] <- "Accident_Distance"
names(df)[names(df) == "Start_Lng"] <- "Longitude"
names(df)[names(df) == "Start_Lat"] <- "Latitude"

# adding year column
df$Year <- substr(df$Date, 1, 4)

# summary table after cleaned and dropped columns

total_na<- 0
for(n in sum(is.na(df))){
  total_na= total_na + n
}
sum_data2 <- summarize(df, NA_count=total_na, Row_count=nrow(df), Column_count=ncol(df))
continuous_column <- summarise(df, latitude_mean=mean(df$Latitude), longitude_mean=mean(df$Longitude),distance_mean=mean(df$Accident_Distance), temperature_mean=mean(df$Temperature),
                               windchill_mean=mean(df$Wind_Chill), humidity_an=mean(df$Humidity), pressure_mean=mean(df$Pressure),
                               visibility_mean=mean(df$Visibility), windspeed_mean=mean(df$Wind_Speed), precipitation_mean=mean(df$Precipitation))

# data dictionary 


dictionary <- data.frame(column_name=c("ID", "Severity", "Start_Time","End_Time", "Latitude", "Longitude", "Accident_Distance", 
                                  "Description", "Street_Number", "Street", "Road_Side", "City", "County","State", "Zipcode",
                                  "Country","Timezone","Temperature", "Wind_Chill", "Humidity","Pressure","Visibility", "Wind_Direction",
                                  "Wind_Speed", "Precipitation", "Weather_Condition", "Amenity","Bump","Crossing", "Give_Way","Junction",
                                  "No_Exit", "Railway", "Roundabout", "Station", "Stop", "Traffic_Calming", "Traffic_Signal", "Turning_Loop",
                                  "Day_or_Night","Date", "Year"), description=c("Unique Identifier","Severity of the accident - range (1-4), 1 indicates the least impact on traffic",
                                  "Start time of accident", "End time of accident; when traffic returned to normal", "Latitude location of the accident site",
                                  "Longitude location of the accident site","The legnth of the road affected by the accident","Human provided description of the accident",
                                  "Street number in address field","Street name","Lists what side of the street the accident occured (R or L)","City name",
                                  "County name","State name (2 letter abbreviation)","5 diget Zipcode","Country name","Timezone in which the accident occured",
                                  "Temperature in Fahrenheit","Wind chill in Fahrenheit","Humidity %","Air pressure in inches","Visibility in miles",
                                  "Direction the wind was blowing","Wind speed in miles per second","Precipitation levels in inches","Weather condition during accident (Rain, Snow, etc.)",
                                  "States if the accident happend in an Amenity Zone (True/False)","States if there was a presence of a speed bump nearby (True/False)",
                                  "States if the accident happened near a crossing (True/False)","States if the accident happened near a yeild sign (True/False)",
                                  "States if the accident happened at a junction (True/False)","States if the accident happened on a no exit street (True/False)",
                                  "States if the accident happened near a railway (True/False)","States if the accident happened near a roundabout (True/False)",
                                  "States if the accident happened near a station (True/False)","States if the accident happened near a stop sign (True/False)",
                                  "If it happened near a traffic calming method (True/False)","States if the accident happened near a traffic signal (True/False)",
                                  "States if the accident happened near a turning loop (True/False)","States if the accidend happened during the day or at night",
                                  "Date"= "Date of the accident (YYYY-mm-DD)", "Year of the accident"))

# Create dictionary into png 

png("dictionary.png", width=550,height=900,bg = "white")
grid.table(dictionary)
dev.off()

# Write to file - cleaned data - the data file we will use 

write_csv(df, "traffic_data.csv")


# Car Accident Overview Analysis 

data(state.regions)
head(state.regions)

data(county.regions)
head(county.regions)

#Number of crashes by state

df <- group_by(df, State)
summ <- summarize(df, value=n())

cr <- state.regions[,c("region","abb")]
summ <- merge(cr, summ, by.x="abb", by.y="State")

summ$value<-cut(summ$value, breaks=c(0,500,5000,10000,30000,217520))
p <- state_choropleth(summ)
p <- p + ggtitle("Traffic Accidents by State")
p <- p + theme(plot.title = element_text(hjust=0.5))
p <- p + scale_fill_brewer(name="Number of Accidents",palette="Oranges", labels=c("<500","500-5000","5000-10000","10000-30000",">30000"))
print(p)


df <- ungroup(df)

#Florida Zoom in 

df_FL <-  subset(df, State =="FL")
df_FL <- group_by(df_FL, County)
df_FL$County <- tolower(df_FL$County)

summFL <- summarize(df_FL, value=n())


st <- county.regions[,c("region","county.name")]
summFL <- merge(st, summFL, by.x="county.name", by.y="County")

p <- county_choropleth(summFL, state_zoom="florida")
p <- p + ggtitle("Florida Traffic Accidents")
p <- p + theme(plot.title = element_text(hjust=0.5))
p <- p + scale_fill_brewer(name="Number of Accidents", palette = "Oranges")
print(p)


df$year <- format(df$Date, format = "%Y")

df$month <- format(df$Date, format="%m")

df <- ungroup(df)
#Crashes in Florida by month
sub2 <- subset(df, State=="FL")
sub2 <- group_by(sub2,month)
summ2 <- summarize(sub2, value=n())

p <- ggplot(data = summ2, aes(x=month, y=value, fill = "orange"))
p <- p + geom_bar(stat="identity", show.legend=FALSE)   
p <- p + ggtitle("Monthly Accidents in Florida")
p <- p + theme(plot.title = element_text(hjust=0.4))
p <- p + xlab("Month")
p <- p + ylab("Total Accidents")
p <- p + scale_x_discrete(labels=c('Jan', 'Feb', 'Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
p
###October - December is Lyft's busiest season, so seeing an uptick in Lyft crashes is not unexpected


#Accident distance and effects

rows <- grep("closed", tolower(df$Description), fixed = TRUE)
df_closed <- df[rows, ]
df_closed

df_not_closed <- df[!grepl("closed",tolower(df$Description)),]

mean(df_closed$Accident_Distance)
mean(df_not_closed$Accident_Distance)


# Analyzing Accidents by severity score

df$severe_accidents <- df$Severity
df$severe_accidents <- factor(df$severe_accidents)
levels(df$severe_accidents) <- c("Not Severe", "Not Severe", "Severe", "Severe")
levels(df$severe_accidents)
df$severe_accident_percent <- df$severe_accidents
levels(df$severe_accident_percent) <- c("False", "True")
levels(df$severe_accident_percent)

df$severe_accident_percent <- as.character(df$severe_accident_percent)
df$severe_accident_percent <- as.numeric(df$severe_accident_percent =="True")

# Summary table with year, severe vs not severe and count of accidents
df_severity <- df
df_severity <- group_by(df_severity, Year, severe_accidents)
summ1 <- summarize(df_severity, number_accidents=n())
pivot1 <- dcast(summ1, Year ~ severe_accidents, value.var="number_accidents")
df_severity <- ungroup(df_severity)

# summary table with year, percent of severe accidents
df_2019 <- subset(df_severity, Year==2019)
df_2020 <- subset(df_severity, Year==2020)
df_2021 <- subset(df_severity, Year==2021)

# table with 2019, count of accidents, percent of severe and not severe
df_2019 <- group_by(df_2019, Year)
summ2019 <- summarize(df_2019, percent_of_severe_accidents= mean(df_2019$severe_accident_percent), percent_of_nonsevere_accidents= 1-mean(df_2019$severe_accident_percent), num_accidents = n())

#2020 summary
df_2020 <- group_by(df_2020, Year)
summ2020 <- summarize(df_2020, percent_of_severe_accidents= mean(df_2020$severe_accident_percent), percent_of_nonsevere_accidents= 1-mean(df_2020$severe_accident_percent), num_accidents = n())

#2021 summary
df_2021 <- group_by(df_2021, Year)
summ2021 <- summarize(df_2021, percent_of_severe_accidents= mean(df_2021$severe_accident_percent), percent_of_nonsevere_accidents= 1-mean(df_2021$severe_accident_percent), num_accidents = n())

# bar chart showing unequal amounts of accidents per year

p1 <- ggplot(df, aes(x=Year, fill= I("lightblue")))  
p1 <- p1 + geom_bar()   
p1 <- p1 + ggtitle("Number of Car Accidents by Year")
p1 <- p1 + xlab("Year")
p1 <- p1 + ylab("Count of Accidents")
print(p1)

# 2019 accidents by severity level 

p2 <- ggplot(df_2019, aes(x=Severity, fill= I("lightblue")))  
p2 <- p2 + geom_bar()   
p2 <- p2 + ggtitle("2019 Number of Car Accidents by Severity Level")
p2 <- p2 + xlab("Severity Level")
p2 <- p2 + ylab("Count of Accidents")
print(p2)

# 2020 accidents by severity level

p3 <- ggplot(df_2020, aes(x=Severity, fill= I("lightblue")))  
p3 <- p3 + geom_bar()   
p3 <- p3 + ggtitle("2020 Number of Car Accidents by Severity Level")
p3 <- p3 + xlab("Severity Level")
p3 <- p3 + ylab("Count of Accidents")
print(p3)

# 2020 accidents by severity level

p4 <- ggplot(df_2021, aes(x=Severity, fill= I("lightblue")))  
p4 <- p4 + geom_bar()   
p4 <- p4 + ggtitle("2021 Number of Car Accidents by Severity Level")
p4 <- p4 + xlab("Severity Level")
p4 <- p4 + ylab("Count of Accidents")
print(p4)

# Accidents by severity overall
p5 <- ggplot(df, aes(x=Severity, fill= I("red")))  
p5 <- p5 + geom_bar()   
p5 <- p5 + ggtitle("2019-2021 Number of Car Accidents by Severity Level")
p5 <- p5 + xlab("Severity Level")
p5 <- p5 + ylab("Count of Accidents")
print(p5)


# Weather Effect on Car Accidents 


# Weather condition
df <- group_by(df, Weather_Condition)
weather_cond_summ <- summarise(df, num_accidents =n())

df_weather_condition <- data.frame(weather_cond_summ)
df_weather_condition <- df_weather_condition[order(df_weather_condition$num_accidents, decreasing=TRUE),]
df_weather_condition <- df_weather_condition[1:10, ]
print(df_weather_condition)

# The most accidents happen on fair and cloudy weather days. 

# compare severity scores 
df$Severity <- as.numeric(df$Severity)
df_wc <- subset(df, Weather_Condition=="Fair")
fair <- mean(df_wc$Severity)

df_wc <- subset(df, Weather_Condition=="Cloudy")
Cloudy <- mean(df_wc$Severity)

df_wc <- subset(df, Weather_Condition=="Mostly Cloudy")
M_cloudy <- mean(df_wc$Severity)

df_wc <- subset(df, Weather_Condition=="Partly Cloudy")
P_cloudy <- mean(df_wc$Severity)

df_wc <- subset(df, Weather_Condition=="Light Rain")
L_rain <- mean(df_wc$Severity)

df_wc <- subset(df, Weather_Condition=="Light Snow")
L_snow <- mean(df_wc$Severity)

df_wc <- subset(df, Weather_Condition=="Heavy Snow")
H_snow <- mean(df_wc$Severity)

df_wc <- subset(df, Weather_Condition=="T-Storm / Windy")
Tstorm <- mean(df_wc$Severity)

Severity_scores <- data.frame(condition=c("Fair", "Cloudy", "Mostly Cloudy", "Partly Cloudy", "Light Rain", "Light Snow", 
                                          "Heavy Snow", "T-Storm/Windy"), severity=c(fair, Cloudy, M_cloudy, P_cloudy, L_rain,
                                                                                     L_snow, H_snow, Tstorm))
# Heavy snow has highest severity score

df <- ungroup(df)

# temperature 
df$temp_binned <- cut(df$Temperature, c(-30,-10,10,30,50,70,90,110,130,Inf))
df <- group_by(df, temp_binned)
summ_temp <- summarise(df, num_accidents=n())

p6 <- ggplot(df, aes(x=temp_binned, fill= I("lightgreen")))  
p6 <- p6 + geom_bar()   
p6 <- p6 + ggtitle("Car Accidents by Temperature")
p6 <- p6 + xlab("Temperature")
p6 <- p6 + ylab("Count of Accidents")
print(p6)

# The most accidents occurred between temperatures 70-90.

df <- ungroup(df)

# Precipitation - not that significant to anything 
df$precip_binned <- cut(df$Precipitation, c(-1,0,1,2,3,4,5,6))
levels(df$precip_binned) <- c("no precipiation", "< 1", "< 2", "< 3", "< 4", "< 5", "< 6")
df <- group_by(df, precip_binned)
summ_precip <- summarise(df, num_accidents=n())

p7 <- ggplot(df, aes(x=precip_binned, fill= I("lightgreen")))  
p7 <- p7 + geom_bar()   
p7 <- p7 + ggtitle("Car Accidents by Precipitation")
p7 <- p7 + xlab("Precipitation(inches)")
p7 <- p7 + ylab("Count of Accidents")
print(p7)

# Most of the accidents occurred on days with no precipitation. 

df <- ungroup(df)

# wind direction
df <- group_by(df, Wind_Direction)
summ_wd <- summarise(df, num_accidents=n())
df <- ungroup(df)

# Wind direction does not seem to have any significant value to the analysis.

# visibility - in miles
df$visibility_binned <- cut(df$Visibility, c(-1,5,10,15,20,Inf))
levels(df$visibility_binned) <- c("5 or less", "6-10", "11-15", "16-20", "20+")
df <- group_by(df, visibility_binned)
summ_visibility <- summarise(df, num_accidents=n())

p8 <- ggplot(df, aes(x=visibility_binned, fill= I("lightgreen")))  
p8 <- p8 + geom_bar()   
p8 <- p8 + ggtitle("Car Accidents by Visibility")
p8 <- p8 + xlab("Visibility in Miles")
p8 <- p8 + ylab("Count of Accidents")
print(p8)

# Accidents typically occur when the visibility is between 6 to 10 miles. 

df <- ungroup(df)

# wind speed 

df$windspeed_binned <- cut(df$Wind_Speed, c(-1,10,20,30,40,50,Inf))
levels(df$windspeed_binned) <- c("10 or less", "11-20", "21-30", "31-40", "41-50", "50+")
df <- group_by(df, windspeed_binned)
summ_ws <- summarise(df, num_accidents=n())

p9 <- ggplot(df, aes(x=windspeed_binned, fill= I("lightgreen")))  
p9 <- p9 + geom_bar()   
p9 <- p9 + ggtitle("Car Accidents by Wind Speed")
p9 <- p9 + xlab("Wind Speed in MPH")
p9 <- p9 + ylab("Count of Accidents")
print(p9)

df <- ungroup(df)

# Strong wind does not seem to have a significant effect on car accidents.

# Wind Chill

df$windchill_binned <- cut(df$Wind_Chill, c(-50,-20,10,40,70,100,Inf))
levels(df$windchill_binned) <- c("under -20", "-21 to 10", "11 to 40", "41 to 70", "71 to 100", "100+")
df <- group_by(df, windchill_binned)
summ_wc <- summarise(df, num_accidents=n())

p10 <- ggplot(df, aes(x=windchill_binned, fill= I("lightgreen")))  
p10 <- p10 + geom_bar()   
p10 <- p10 + ggtitle("Car Accidents by Wind Chill")
p10 <- p10 + xlab("Wind Chill")
p10 <- p10 + ylab("Count of Accidents")
print(p10)

# More accidents tend to occur when the wind chill is between 41 to 100. 

df <- ungroup(df)

# Humidity

mean(df$Humidity)
min(df$Humidity)
max(df$Humidity)

df$humidity_binned <- cut(df$Humidity, c(-1,20,40,60,80,100,Inf))
levels(df$humidity_binned) <- c("under 20", "21-40", "41-60", "61-80", "81-100","101+")
df <- group_by(df, humidity_binned)
summ_humid <- summarise(df, num_accidents=n())

p11 <- ggplot(df, aes(x=humidity_binned, fill= I("lightgreen")))  
p11 <- p11 + geom_bar()   
p11 <- p11 + ggtitle("Car Accidents by Humidity")
p11 <- p11 + xlab("Humidity")
p11 <- p11 + ylab("Count of Accidents")
print(p11)

# Accidents are more equally distributed by humidity bins than other factors.
# However, more accidents tend to happen in high humidity levels. 

df <- ungroup(df)

# Pressure 

df$pressure_binned <- cut(df$Pressure, c(15,30,45,60,Inf))
levels(df$pressure_binned) <- c("15-30","31-45", "46-60", "61+")
df <- group_by(df, pressure_binned)
summ_pressure <- summarise(df, num_accidents=n())

p12 <- ggplot(df, aes(x=humidity_binned, fill= I("lightgreen")))  
p12 <- p12 + geom_bar()   
p12 <- p12 + ggtitle("Car Accidents by Pressure")
p12 <- p12 + xlab("Pressure")
p12 <- p12 + ylab("Count of Accidents")
print(p12)

# Air pressure has a spread out distribution of accidents. Higher pressure tends to have more accidents. 

# Creating an accident time duration column

df$Start_Hour<- hour(df$Start_Time)
df$Start_Hour
df$duration <- df$End_Time -df$Start_Time 
df$duration

### Creating a summary table that shows the count of accidents in each hour
df<- group_by(df, Start_Hour)
sum_hour <- summarize(df, Number_of_Accidents = n())
sum_hour
df <- ungroup(df)

## line plot showing this information
p <- ggplot(data = sum_hour, aes(x=Start_Hour, y=Number_of_Accidents))   
p <- p + geom_line(color = "#0072B2") # Change line color to blue
p <- p + labs(title = "Hourly Car Accident Trends", x = "Hour", y = "Number of Accidents") 
p <- p + theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(b = 10)))
p <- p + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14)) 
p

## As we can see from the line chart most of the accidents occure in hour 16 or 4pm.

# Road Feature Analysis

sum_trafic<- summarize(df, num_of_amenity = sum(Amenity),
                       num_of_bump = sum(Bump),
                       num_of_crossing = sum(Crossing),
                       num_of_give_way = sum(Give_Way),
                       num_of_junction = sum(Junction),
                       num_of_no_exit = sum(No_Exit),
                       num_of_railway= sum(Railway),
                       num_of_roundabout= sum(Roundabout),
                       num_of_station = sum(Station),
                       num_of_stop = sum(Stop),
                       num_of_traffic_calming = sum(Traffic_Calming),
                       num_of_traffic_signal = sum(Traffic_Signal),
                       num_of_turning_loop= sum(Turning_Loop))
sum_trafic
### The majority of accidents are coming from Crossing and Traffic Signals

### Changing the dateframe so it is in a format that can be easily read into a bar graph
road_features<- c()
for(n in names(sum_trafic)){
  tmp<- substr(n, 8, nchar(n))
  road_features <- append(road_features, tmp)
}

count_accidents<- c()
for(n in names(sum_trafic)){
  for(v in sum_trafic[n]){
    count_accidents<- append(count_accidents, v)
  }
}


## Creating new Data Frame with new names. 
df_rf <- data.frame(Road_Features = road_features, Accidents = count_accidents)

## Reordering the columns
df_rf$Road_Features <- reorder(df_rf$Road_Features, df_rf$Accidents)

## Creating bar graph
p <- ggplot(data = df_rf, aes(x= Accidents, y=Road_Features ))   
p <- p + geom_bar(stat = "identity", fill = "red")
p

## Majority of accidents are coming from traffic_signals and crossings.

### Maps at the national level

data(state.regions)
head(state.regions)

data(county.regions)
head(county.regions)

df_ts <- subset(df, Traffic_Signal == TRUE)
df_ts <- group_by(df_ts, State)

summ_ts <- summarize(df_ts, value=n())

cr <- state.regions[,c("region","abb")]
summ_ts <- merge(cr, summ_ts, by.x="abb", by.y="State")
head(summ_ts)

p <- state_choropleth(summ_ts)
p

### Florida has the most traffic signal accidents in the country 26,841 


### Crossings

df_c <- subset(df, Crossing == TRUE)
df_c <- group_by(df_c, State)

summ_c <- summarize(df_c, value=n())

cr <- state.regions[,c("region","abb")]
summ_c <- merge(cr, summ_c, by.x="abb", by.y="State")
head(summ_c)

p <- state_choropleth(summ_c)
p

### As we can see from the graph, Florida has the most crossing accident 44,155


### finding duration Max, Min, Avg and adding it to the new df
max_v <- c()
min_v<- c()
avg_v<- c()
for(n in names(df[27:39])){
  tmp <- subset(df, df[[n]] == TRUE)
  print(nrow(tmp))
  max_v <- append(max_v, max(tmp$duration))
  min_v <- append(min_v, min(tmp$duration))
  avg_v <- append(avg_v, mean(tmp$duration))
  
}

### Took note that the last element didn't have anything (Turning_Loop)
### Therefore, I'm going to drop the last element

max_v <- max_v[1:12]
min_v<- min_v[1:12]
avg_v<- avg_v[1:12]

df_rfd <- data.frame(Road_Features = road_features[1:12], Accidents = count_accidents[1:12], 
                     Min_traffic_disruption_time=abs(min_v), Max_traffic_disruption_time=abs(max_v),
                     Avg_traffic_disruption_time=abs(avg_v))


#### Reordering the road features based on the avg columns

df_rfd$Road_Features <- reorder(df_rfd$Road_Features, df_rfd$Avg_traffic_disruption_time)

#### Creating a bar graph on Avg traffic disruption time based on road features

p <- ggplot(data = df_rfd, aes(x= Avg_traffic_disruption_time, y=Road_Features ))   
p <- p + geom_bar(stat = "identity", fill = "blue")
p

### railways and amenity have the longest avg. traffic disruption time


#### Reordering the road features based on the max columns

df_rfd$Road_Features <- reorder(df_rfd$Road_Features, df_rfd$Max_traffic_disruption_time)

#### Creating a bar graph on Max traffic disruption time based on road features

p <- ggplot(data = df_rfd, aes(x= Max_traffic_disruption_time, y=Road_Features ))   
p <- p + geom_bar(stat = "identity", fill = "green")
p

### railways and amenity have the longest max. traffic disruption time



#### Reordering the road features based on the min columns

df_rfd$Road_Features <- reorder(df_rfd$Road_Features, df_rfd$Min_traffic_disruption_time)

#### Creating a bar graph on Max traffic disruption time based on road features

p <- ggplot(data = df_rfd, aes(x= Min_traffic_disruption_time, y=Road_Features ))   
p <- p + geom_bar(stat = "identity", fill = "orange")
p

### Roundabouts have the longest minimum traffic disruption time
### traffic signal and crossing have the shortest minimum traffic disruption


### Avg Duration by state

df_d <- group_by(df, State)

summ_d <- summarize(df_d, value= mean(duration))

summ_d$value <- substr(summ_d$value,1,6)

summ_d$value <- as.numeric(summ_d$value)

cr <- state.regions[,c("region","abb")]
summ_d <- merge(cr, summ_d, by.x="abb", by.y="State")
head(summ_d)

p <- state_choropleth(summ_d)
p





