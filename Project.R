## There was an issue with this dateset, the dates are incorrect. I have to bolt together many sub-datasets from the UK government
## Website in order to get the correct information

#setwd("C:/Users/Edward/Google Drive/Masters/Exercises/Semester2/DataMining/Data/project/road_accidents/UK_Traffic_Accidents_R")
ls()
rm(list=ls())


# The Accidents and vehicle information are in separate files, and a separate file per year ...
raw_data.accidents.2015 <- read.csv("Accidents_2015.csv")
raw_data.vehicles.2015 <- read.csv("Vehicles_2015.csv")


raw_data.main_vehicles.2015 = raw_data.vehicles.2015[raw_data.vehicles.2015$Vehicle_Reference == 1,]


raw_data.merged.2015 = merge(raw_data.main_vehicles.2015,raw_data.accidents.2015, by = "Accident_Index" )


barplot(table(raw_data.merged.2015$Sex_of_Driver))

raw_data.accidents.2005_2014 <- read.csv("Accidents0514.csv")
raw_data.vehicles.2005_2014 <- read.csv("Vehicles0514.csv")

raw_data.main_vehicles.2005_2014 <- raw_data.vehicles.2005_2014[raw_data.vehicles.2005_2014$Vehicle_Reference == 1,]
head(raw_data.main_vehicles.2005_2014)
#for some reason the formatting of Accident_Index is wrong in this dataset
raw_data.merged.2005_2014 <- merge(raw_data.main_vehicles.2005_2014,raw_data.accidents.2005_2014, by = "ï..Accident_Index" )
# I will rename it here
names(raw_data.merged.2005_2014)[names(raw_data.merged.2005_2014)=="ï..Accident_Index"] <- "Accident_Index"

barplot(table(raw_data.merged.2005_2014$Sex_of_Driver))

barplot(table(raw_data.merged.2005_2014$Sex_of_Driver,raw_data.merged.2005_2014$Age_Band_of_Driver))

# It appears that male drivers appears to crash more than female drivers, I will explore this in more detail later
# I have noticed the 2015 dataset contains one additional column, "Vehicle_IMD_Decile" , I will remove this now

raw_data.merged.2015$Vehicle_IMD_Decile <-NULL

# Now lets combine the two datasets to get all the data from 2004-2015 in one dataset
raw_data.merged.2005_2015 <- rbind(raw_data.merged.2005_2014,raw_data.merged.2015)

#lets do some tidying up
workspace <-ls()
rm(list = workspace[-6])

# The dataset left has 1,780,570 entries, we should be able to get some useful statistics out of this many entries.

# However, some items an NA or -1 as their entries, first I will remove them
na_count <-sapply(raw_data.merged.2005_2015, function(y) sum(length(which(is.na(y)))))

colSums(raw_data.merged.2005_2015 == -1)
# A number of entries can be removed as there are not that interested:
# Junction_Location, Journey_Purpose_of_Driver, Was_Vehicle_Left_Hand_Drive. Engine_Capacity_.CC. Driver_IMD_Decile Age_of_Vehicle
# Propulsion_Code Driver_Home_Area_Type X2nd_Road_Class Junction_Control X2nd_Road_Number 
to.remove = c("Junction_Location","Journey_Purpose_of_Driver","Was_Vehicle_Left_Hand_Drive.","Engine_Capacity_.CC.",
              "Driver_IMD_Decile","Age_of_Vehicle","Propulsion_Code","Driver_Home_Area_Type","X2nd_Road_Class"
              ,"Junction_Control","X2nd_Road_Number","Pedestrian_Crossing.Human_Control","Did_Police_Officer_Attend_Scene_of_Accident",
              "Pedestrian_Crossing.Physical_Facilities")
# lets negate the in command in order to remove these columns
`%ni%` <- Negate(`%in%`)

raw_data.merged.2005_2015.sub <- subset(raw_data.merged.2005_2015, select = names(raw_data.merged.2005_2015) %ni% to.remove)
colSums(raw_data.merged.2005_2015.sub == -1)

# I am now going to remove rows that contain missing data, we currently have 1.78 million rows, unfortuantely the age of driver
# is missing in 180,000 rows, 10% of the data. This is quite a key statistic so I will remove these rows. 
# So before removing 1,780,570 samples

final_data1 <- raw_data.merged.2005_2015.sub[!apply(raw_data.merged.2005_2015.sub, 1, function(x) {any(x == -1)}),]
# Remove some entries where sex is 0 or 3, (it should be 1 or 2)
final_data2 <-subset(final_data1,final_data1[,"Sex_of_Driver"] < 3)
final_data <-subset(final_data2,final_data2[,"Sex_of_Driver"] > 0)
# After there are now 1,597853 observations, still quite alot


# Lets start with some Basic statistics:
# Male vs. female crashes
male_vs_female <- table(final_data$Sex_of_Driver)
cols <- c("blue", "red")[(male_vs_female > 0) ] 
barplot(male_vs_female, col =cols,names.arg = c("Male", "Female"))
# Male vs. female crashes by age group

male_vs_female_vs_age <- table(final_data$Sex_of_Driver, final_data$Age_of_Driver)

barplot(male_vs_female_vs_age[1,],col = "blue", xlab = "Age", ylab = "Crashes")
barplot(male_vs_female_vs_age[2,],col = "red", xlab = "Age", ylab = "Crashes")
# This plot is quite interesting, there are regular peaks every 5 years at 25, 30, 35, 40, 45, 50. 
# Do people really crash more at multiples of 5, probably not, it is more likely the person making the report rounded to the nearest 5 years

# There is also an "Age Band" variable with age bands corresponding to 
# 1: 0-5, 2: 5-10, 3: 11-15, 4: 16-20, 5: 21-25, 6, 26-35, 7: 36-45, 8: 46-55, 9: 56-65, 10: 66-75, 11: 76+

male_vs_female_vs_age_band <- table(final_data$Sex_of_Driver, final_data$Age_Band_of_Driver)

barplot(male_vs_female_vs_age_band,col = c("blue", "red"))

# Number of crashes per year
# This requires some formating of the dates in the file, it looks like dates are in the DD/MM/YYYY format, forunately we did a similar
# exercise in the practical sessions there is also Day_of_Week and Time
head(final_data$Date)

final_data$Date2 <- as.POSIXlt(final_data$Date, format = "%d/%m/%Y")
final_data$Year <- format(final_data$Date2,'%Y')
final_data$Month <- format(final_data$Date2,'%m')
head(final_data$Date2)
head(final_data$Time)
library(chron)

final_data$Time2 <- times(format(final_data$Time, format="%H:%M"))

final_data$Time2 <- paste(final_data$Time,":00",sep = "")
head(final_data$Time2)
final_data$Time2 <- times(format(final_data$Time2, format="%H:%M"))
head(final_data$Time2)
# lets bin the times into 1 hour blocks
time.tag <- chron(times = c("00:00:00", "01:00:00", "02:00:00", "03:00:00", "04:00:00", "05:00:00",
                            "06:00:00", "07:00:00", "08:00:00", "09:00:00", "10:00:00", "11:00:00",
                            "12:00:00", "13:00:00", "14:00:00", "15:00:00", "16:00:00", "17:00:00",
                            "18:00:00", "19:00:00", "20:00:00", "21:00:00", "22:00:00", "23:00:00", "23:59:00"))

final_data$Time_binned <- cut(final_data$Time2, breaks = time.tag,
                              labels = c("00-01","01-02","02-03","03-04","04-05","05-06","06-07","07-08","08-09","09-10",
                                         "10-11","11-12","12-13","13-14","14-15","15-16","16-17","17-18","18-19","19-20",
                                         "20-21","21-22","22-23","23-00"), include.lowest = TRUE)

final_data$NamedWeekday <- weekdays(final_data$Date2)
# Unfortunately R orders these alphabetically, so I will add the labels myself
barplot(table(final_data$Day_of_Week), names.arg = c("Mon","Tue", "Wed", "Thur","Fri", "Sat", "Sun"))
# Now we can make an interesting plot, number of crimes as a function of weekday and time of day
# I notices the the days of week start at Sunday=1, Monday =2 etc, I will re-number these so Monday=1 ... Sunday=7

final_data$Day_of_Week <- ((final_data$Day_of_Week + 5) %% 7) + 1
weekdays_vs_hours <- table(final_data$Day_of_Week,final_data$Time_binned)

# install.packages("reshape")
# install.packages("ggthemes")
library(reshape)
weekdays_vs_hours_data <- melt(weekdays_vs_hours)
library(ggplot2)
library(ggthemes)

# Crashes based on time of day vs. Day of week

ggplot(weekdays_vs_hours_data) + aes(x = Var.2, y = Var.1, fill = value) +
  labs(x="Time of Day", y="Day of Week", title="Number of Crashes per hour vs. day of week from 2004-2015")+
  scale_fill_distiller(palette = "Spectral") + coord_equal() + 
  geom_tile(color="white", size=0.01) +
  scale_y_discrete(limits = "Ideal",labels = list("Mon","Tue", "Wed", "Thur","Fri", "Sat", "Sun"))
# This is a very interesting plot, the information it shows is somewhat expected, we have many more crashes between 8-9 am and 5-6 pm 
# On Monday to Friday, also on Friday and Saturday evening there are more crashes late into the night / early morning.



male_weekdays_vs_hours <- table(subset(final_data,final_data[,"Sex_of_Driver"]==1)$Day_of_Week,subset(final_data,final_data[,"Sex_of_Driver"]==1)$Time_binned)
male_weekdays_vs_hours_data <- melt(male_weekdays_vs_hours)

ggplot(male_weekdays_vs_hours_data) + aes(x = Var.2, y = Var.1, fill = value) +
  labs(x="Time of Day", y="Day of Week", title="Number of Crashes per hour vs. day of week from 2004-2015")+
  scale_fill_distiller(palette = "Spectral") + coord_equal() + 
  geom_tile(color="white", size=0.01) +
  scale_y_discrete(limits = "Ideal",labels = list("Mon","Tue", "Wed", "Thur","Fri", "Sat", "Sun"))

female_weekdays_vs_hours <- table(subset(final_data,final_data[,"Sex_of_Driver"]==2)$Day_of_Week,subset(final_data,final_data[,"Sex_of_Driver"]==2)$Time_binned)
female_weekdays_vs_hours_data <- melt(female_weekdays_vs_hours)

ggplot(female_weekdays_vs_hours_data) + aes(x = Var.2, y = Var.1, fill = value) +
  labs(x="Time of Day", y="Day of Week", title="Number of Crashes per hour vs. day of week from 2004-2015")+
  scale_fill_distiller(palette = "Spectral") + coord_equal() + 
  geom_tile(color="white", size=0.01) +
  scale_y_discrete(limits = "Ideal",labels = list("Mon","Tue", "Wed", "Thur","Fri", "Sat", "Sun"))


barplot(table(final_data$Year))
# This is a very interesting plot, the number of crashes per year has decrease year upon year.
# Crashes based on month of year
month_vs_year <- table(final_data$Month,final_data$Year)
month_vs_year_data <- melt(month_vs_year)


ggplot(month_vs_year_data) + aes(x = Var.2, y = Var.1, fill = value) +
  labs(x="Year", y="Month", title="Number of Crashes per Month vs. Year  from 2004-2015")+
  scale_fill_distiller(palette = "Spectral") + coord_equal() + 
  geom_tile(color="white", size=0.01) 
# Crashes per year

table(final_data$Year)
barplot(table(final_data$Year))

# it also might be interesting so see the different road types in the dataset, there is an attribute "Road_Type"
table(final_data$Road_Type)
barplot(table(final_data$Road_Type), names.arg = c("Round-\r\nabout","One way\r\n Street", "Dual\r\nCarriage-\r\nway", "Single\r\nCarriage-\r\nway","Slip Road", "Unknown"), las=2)
# Its clear that single-carriageway roads are the most dangerous, and/or the most used
# Lets see road type vs. severity
barplot(table(final_data$Accident_Severity,final_data$Road_Type),names.arg = c("Round-\r\nabout","One way\r\n Street", "Dual\r\nCarriage-\r\nway", "Single\r\nCarriage-\r\nway","Slip Road", "Unknown"), las=2)

road_type_vs_severity_data <- melt(table(final_data$Accident_Severity,final_data$Road_Type))

ggplot(road_type_vs_severity_data) + aes(x = Var.2, y = Var.1, fill = value) +
  labs(x="Road Type", y="Severity", title="Road type vs. severity  from 2004-2015")+
  scale_fill_distiller(palette = "Spectral") + coord_equal() + 
  geom_tile(color="white", size=0.01) 


# Severity vs. age

severity_vs_age_data <- melt(table(final_data$Accident_Severity, final_data$Age_of_Driver))
ggplot(severity_vs_age_data) + aes(x = Var.2, y = Var.1, fill = value) +
  labs(x="Age", y="Severity", title="Age vs. Severity")+
  scale_fill_distiller(palette = "Spectral") + coord_equal() + 
  geom_tile(color="white", size=0.01) 
# This is somewhat interesting, but will not be included in the report

# Lets do some spatial plots of this data.

library(RgoogleMaps)
library(sp)
library(ggplot2)
library(ggmap)
library(maps)

# Overall plot of crashes in the UK, I did try a density plot, but it was overwhelmed by the crashes in london that
# Everywhere else was appearing as zeroes
united_kingdom <- get_map("united kingdom", zoom = 5)
# Plot of uk accidents with speed limit as color
final_data_2015 <- subset(final_data,final_data[, "Year"] == 2015)
win.graph(800,600,10)
ggmap(united_kingdom, extent="device") +scale_x_continuous(limits = c(-10.5,2.0),expand =c(0,0)) + scale_y_continuous(limits = c(49.8,59.4),expand =c(0,0)) +
                        geom_point(aes(x = final_data_2015$Longitude, y= final_data_2015$Latitude,color=final_data_2015$Speed_limit),
                                   alpha = 0.1, size = 0.01, data = final_data_2015)
# Plot of uk accidents with Accident severity as color
win.graph(800,600,10)
ggmap(united_kingdom, extent="device") +scale_x_continuous(limits = c(-10.5,2.0),expand =c(0,0)) + scale_y_continuous(limits = c(49.8,59.4),expand =c(0,0)) +
  geom_point(aes(x = final_data_2015$Longitude, y= final_data_2015$Latitude,color=final_data_2015$Accident_Severity),
             alpha = 0.1, size = 0.01, data = final_data_2015)
# This plot is interesting and generally shows that major citys and motorways have alot of accidents.
# Lets focus on two cities: London and Bath (my home town)

#Plot of london with Accident severity as color
london_map <- get_map("London", zoom = 12)
win.graph(800,600,10)
ggmap(london_map, extent="device") +
  geom_point(aes(x = final_data_2015$Longitude, y= final_data_2015$Latitude,color=final_data_2015$Accident_Severity),
             alpha = 0.5, size = 1, data = final_data_2015)


bath_map <- get_map("Bath, United Kingdom", zoom = 14)
ggmap(bath_map)

# As Bath is quite a small city I can plot all the accidents from 2005-2015

win.graph(800,600,10)
ggmap(bath_map, extent="device") +
  geom_point(aes(x = final_data$Longitude, y= final_data$Latitude, color=final_data$Year),
             alpha = 0.5, size = 2, data = final_data)
win.graph(800,600,10)
ggmap(bath_map, extent="device") +
  geom_point(aes(x = final_data$Longitude, y= final_data$Latitude, color=final_data$Accident_Severity),
             alpha = 0.5, size = 2, data = final_data)


######################################################################################
############  Please ignore subsequent lines as they were not sucessfully ############
######################################################################################







# Lets try and predict whether a driver is male or female using a random forest

# To start with lets a small subset of the data final data 2015

library(caret)
library(doParallel)

# lets split into test and train

y = final_data_2015$Sex_of_Driver
x <- final_data_2015[ ,!(colnames(final_data_2015) %in% c("Sex_of_Driver"))]

# remove dates as these are not compatible

forest_data <- final_data_2015[ ,!(colnames(final_data_2015) %in% c("Date2", "Time2", "Time_binned","NamedWeekday","NamedWeekday",
                                                                    "LSOA_of_Accident_Location","Local_Authority_.Highway.","Time",
                                                                    "Date","Accident_Index","Vehicle_Reference","Year","Month"))]

head(forest_data)
summary(forest_data)
set.seed(42)
idx <- sample(1:nrow(forest_data), 30000)

test = forest_data[-idx,]
train = forest_data[idx,]



clus <- parallel::makeCluster(spec = 6, type = 'PSOCK')
registerDoParallel(clus)

ctrl.train <- trainControl(method='repeatedcv', number = 3, repeats=3)

fit.rf <- train(Sex_of_Driver ~ ., data= train, method= 'rf', metrix='Accuracy',
                tuneGrid=expand.grid(.mtry=1:6), trControl=ctrl.train, ntree=10)

stopCluster(clus)














