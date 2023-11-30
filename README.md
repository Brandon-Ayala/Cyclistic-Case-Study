# Table of Contents

1. [Who is Cyclistic?](#who)
2. [Ask Phase: What is the Goal?](#ask)
3. [Prepare Phase: Understanding the Data](#prepare)
4. [Process Phase: Cleaning and Organizing the Data](#process)
    * 4.1. [Excel](#excel)
    * 4.2. [R](#r)
5. [Analyze and Share Phase: What Can We Learn From the Data?](#analyze)
    * 5.1 [How many total (casual/annual) rides were taken?](#analyze1)
    * 5.2 [What types of bikes do (casual/annual) riders use the most?](#analyze2)
    * 5.3 [What is the median ride duration of a (casual/annual) rider?](#analyze3)
    * 5.4 [What time of day do (casual/annual) riders most often ride at?](#analyze4)
    * 5.5 [Which days of the week do (casual/annual) riders most often take rides?](#analyze5)
    * 5.6 [What time of the year has the most (casual/annual) riders?](#analyze6)
    * 5.7 [Which start stations have the most (casual/annual) riders?](#analyze7)
6. [Act Phase: Conclusions and Recommendations](#act)

<a id="who"></a> 
# 1. Who is Cyclistic?

Cyclistic is a fictional bike-share company which launched in 2016 in Chicago, Illinois. The company offers 5,824 bicycles and 692 docking stations across Chicago. Their bikes can be unlocked from a station and returned to any other station in the system at any time. Their unique selling point is that they also offer reclining bikes, hand tricycles, and cargo bikes to make bike-sharing more inclusive to people with disabilities and riders who can't use a standard two-wheeled bike.

<a id="ask"></a>
# 2. Ask Phase: What is the Goal?

There are two types of riders that Cyclistic defines: Casual Riders and Cyclistic Annual Members. Customers who purchase single-ride or full-day passes are referred to as Casual Riders, and customers who purchase annual memberships are Annual Members. Lily Moreno, the director of marketing for Cyclistic, believes that the company's future success depends on maximizing the number of annual memberships which are more profitable. Rather than attract new customers, she wants to convert Casual Riders into Annual Members. The goals of this analysis are:

1. Understand how Casual Riders and Annual Members use Cyclistic bikes differently.
2. Determine what motivates Casual Riders to purchase annual memberships with Cyclistic.
3. Recommend a new digital marketing strategy to convert Casual Riders into Annual Members.

<a id="prepare"></a>
# 3. Prepare Phase: Understanding the Data

The data used is provided by Motivate International Inc. under [this license.](https://divvybikes.com/data-license-agreement) The full list of spreadsheets available can be found [here.](https://divvy-tripdata.s3.amazonaws.com/index.html) I decided to use the 12 spreadsheets ranging from January 2022 to December 2022 so that I can get an idea of rider trends through the most recent full calendar year. The data provided includes the following information:

* **ride_id:** A unique ID for each ride taken.
* **rideable_type:** The type of bike used for the ride; Classic, Electric, or Docked.
* **started_at:** The time the ride started.
* **ended_at:** The time the ride ended.
* **start_station:** The name of the station that the ride started at.
* **start_station_id:** The ID of the station that the ride started at.
* **end_station:** The name of the station that the ride ended at.
* **end_station_id:** The ID of the station that the ride ended at.
* **start_lat:** The starting latitude of the ride.
* **start_lng:** The starting longitude of the ride.
* **end_lat:** The ending latitude of the ride.
* **end_lng:** The ending longitude of the ride.
* **member_casual:** The type of rider; Casual or Member.

The data provided seems largely reliable and credible. There are over 5.6 million rides of data available and none of the data is input by users. The website the data is from states that [rides shorter than 60 seconds were removed](https://divvybikes.com/system-data) but I didn't find that to be true; There were still a lot of rides under 60 seconds. I will remove rides under 60 seconds as well as rides over 24 hours when analyzing the data. I also found that there were a lot of rides that didn't have a starting station or ending station which will be removed during this analysis. Regardless, I believe the data provided will be plenty to theorize how Casual Riders differ from Annual Members.

<a id="process"></a>
# 4. Process Phase: Cleaning and Organizing the Data

<a id="excel"></a>
### **Excel**

* Renamed all of the sheets to make them easier to understand (e.g. From "202201-divvy-tripdata" -> "Jan_2022_Divvy_Trip_Data_Clean")
* Checked for and removed duplicates
    * 2 found in March
* Reformatted "started_at" and "ended_at" columns to yyyy-mm-dd hh:mm:ss
* Created new column "month" by using function ``=TEXT(C2,"mmmm")``
* Created new column "day_of_week" by using function ``=CHOOSE(WEEKDAY(C2),"Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")``
* Used filters to check columns "rideable_type", "member_casual", and "month" for values that shouldn't belong
    * None were found
* Used filters to check for missing values in "start_station_name" and "start_station_id"
    * 16260 found in January - All Electric (3894 Casual, 12366 Member)
    * 18580 found in February - All Electric (4054 Casual, 14526 Member)
    * 47246 found in March - All Electric (14513 Casual, 32733 Member)
    * 70887 found in April - All Electric (22961 Casual, 47926 Member)
    * 86704 found in May - All Electric (37282 Casual, 49422 Member)
    * 92944 found in June - All Electric (46854 Casual, 46090 Member)
    * 112031 found in July - All Electric (56645 Casual, 55386 Member)
    * 112037 found in August - All Electric (53812 Casual, 58225 Member)
    * 103780 found in September - All Electric (46272 Casual, 57508 Member)
    * 91355 found in October - All Electric (35803 Casual, 55552 Member)
    * 51957 found in November - All Electric (16980 Casual, 34977 Member)
    * 29283 found in December - All Electric (8337 Casual, 20946 Member)
* Used filters to check for missing values in "end_station_name" and "end_station_id"
    * 17927 found in January - 370 Classic (79 Casual, 291 Member) - 18 Docked (All Casual) - 17539 Electric (4615 Casual, 12924 Member)
    * 20355 found in February - 191 Classic (50 Casual, 141 Member) - 17 Docked (All Casual) - 20147 Electric (4940 Casual, 15207 Member)
    * 51157 found in March - 147 Classic (115 Casual, 32 Member) - 136 Docked (All Casual) - 50874 Electric (17357 Casual, 33517 Member)
    * 75288 found in April - 188 Classic (165 Casual, 23 Member) - 136 Docked (All Casual) - 74964 Electric (26789 Casual, 48175 Member)
    * 93171 found in May - 445 Classic (367 Casual, 78 Member) - 289 Docked (All Casual) - 92437 Electric (43955 Casual, 48482 Member)
    * 100152 found in June - 651 Classic (548 Casual, 103 Member) - 428 Docked (All Casual) - 99073 Electric (54013 Casual, 45060 Member)
    * 120951 found in July - 497 Classic (407 Casual, 80 Member) - 456 Docked (All Casual) - 119998 Electric (66297 Casual, 53701 Member)
    * 120522 found in August - 416 Classic (428 Casual, 88 Member) - 435 Docked (All Casual) - 119671 Electric (62270 Casual, 57401 Member)
    * 111185 found in September - 393 Classic (295 Casual, 98 Member) - 338 Docked (All Casual) - 110454 Electric (53362 Casual, 57092 Member)
    * 96617 found in October - 268 Classic (165 Casual, 103 Member) - 218 Docked (All Casual) - 96131 Electric (41119 Casual, 55012 Member)
    * 54259 found in November - 143 Classic (98 Casual, 45 Member) - 92 Docked (All Casual) - 54024 Electric (19318 Casual, 34706 Member)
    * 31158 found in December - 79 Classic (52 Casual, 27 Member) - 53 Docked (All Casual) - 31026 Electric (9561 Casual, 21465 Member)

<a id="r"></a>    
### **R**

```
# Install the packages used

install.packages("tidyverse")
install.packages("readxl")
install.packages("ggrepel")
install.packages("mapview")
install.packages("sf")
install.packages("leaflet")
install.packages("leafem")

# Load the packages used

library("tidyverse")
library("readxl")
library("ggrepel")
library("mapview")
library("sf")
library("leaflet")
library("leafem")

# This prevents visualizations from having their axis labels turn into
# scientific notation

options(scipen=10000)

# Import the data from an Excel workbook file into R

Jan_2022_Divvy <- read_excel(file.choose())
Feb_2022_Divvy <- read_excel(file.choose())
March_2022_Divvy <- read_excel(file.choose())
April_2022_Divvy <- read_excel(file.choose())
May_2022_Divvy <- read_excel(file.choose())
June_2022_Divvy <- read_excel(file.choose())
July_2022_Divvy <- read_excel(file.choose())
Aug_2022_Divvy <- read_excel(file.choose())
Sep_2022_Divvy <- read_excel(file.choose())
Oct_2022_Divvy <- read_excel(file.choose())
Nov_2022_Divvy <- read_excel(file.choose())
Dec_2022_Divvy <- read_excel(file.choose())

# Bind the data together into one dataframe

Divvy <- rbind(Jan_2022_Divvy, Feb_2022_Divvy, March_2022_Divvy,
                    April_2022_Divvy, May_2022_Divvy, June_2022_Divvy,
                    July_2022_Divvy, Aug_2022_Divvy, Sep_2022_Divvy,
                    Oct_2022_Divvy, Nov_2022_Divvy, Dec_2022_Divvy)

# Check the number of NA values per column

colSums(is.na(Divvy))
```

![image](Visualizations\colsums_is_na.png)

```
# Check the number of rows with NA values

sum(!complete.cases(Divvy))
```

![image](Visualizations\sum_incomplete_observations.png)

```
# Create new dataframe with rows removed that have NA values

Divvy_Clean <- na.omit(Divvy)

# Create a new column for ride duration in seconds

Divvy_Clean$ride_duration = difftime(Divvy_Clean$ended_at,
                                     Divvy_Clean$started_at,
                                     units = "secs")

# Remove rows that have ride duration under 1 minute or over 24 hours

Divvy_Clean = Divvy_Clean[Divvy_Clean$ride_duration > 60 &
                          Divvy_Clean$ride_duration < 86400,]

# Convert ride_duration to numeric for future calculation

Divvy_Clean$ride_duration <- as.numeric(Divvy_Clean$ride_duration)

# Reorder the columns so that ride_duration is after ended_at

Divvy_Clean <- Divvy_Clean[, c(1, 2, 3, 4, 16, 5, 6, 7, 8,
                               9, 10, 11, 12, 13, 14, 15)]
```

From here the data is clean enough for me to work with and start answering questions. The questions I focused on for this analysis are:

* How many total (casual/annual) rides were taken?
* What types of bikes do (casual/annual) riders use the most?
* What is the median ride duration of a (casual/annual) rider?
* What time of day do (casual/annual) riders most often ride at?
* Which days of the week do (casual/annual) riders most often take rides?
* Which time of the year has the most (casual/annual) riders?
* Which start stations have the most (casual/annual) riders?

<a id="analyze"></a>
# 5. Analyze and Share Phase: What Can We Learn From the Data?

<a id="analyze1"></a> 
### **How many total (casual/annual) rides were taken in 2022?**

```
# Visualize the total number of casual and member rides

ggplot(data = Divvy_Clean, aes(x = member_casual, fill = member_casual)) +
  geom_bar() +
  geom_text(aes(label = format(..count.., big.mark = ",")),
            stat="count",
            vjust = 1.5,
            color = "white",
            size = 5.5) +
  labs(x = "Casual Rides / Member Rides",
       y = "Number of Rides",
       title = "Total Number of Casual and Member Rides in 2022") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::comma)
```

<img src="Visualizations\Total_Number_Casual_Member_Rides_2022.png" width="920px" height="700px">

**Conclusion:** There are more rides taken by Annual Members than Casual Riders.

<a id="analyze2"></a>
### **What types of bikes do (casual/annual) riders use the most?**

```
# Create a dataframe to answer:
# What types of bikes do casual riders and annual members use the most?

bike_types <- Divvy_Clean %>% 
  group_by(member_casual) %>% 
  count(rideable_type)

# Arrange the types of bikes properly for the next visualization

bike_types$rideable_type <- factor(bike_types$rideable_type,
                                              c("docked_bike", "electric_bike",
                                                "classic_bike"))

# Visualize the number of casual and annual rides with each type of bike

ggplot(data = bike_types,
       aes(x = member_casual, y = n, fill = rideable_type)) +
  scale_fill_manual(labels = c('Docked Bike', 'Electric Bike', 'Classic Bike'),
                    values=c('#999999','#E69F00', '#5276ad')) +
  geom_bar(stat = "identity", position = "dodge", width = .7) +
  annotate("text", x = .77, y = 131000, label = "164,520", color = "white", size = 4.5) +
  annotate("text", x = 1, y = 600008, label = "633,008", color = "white", size = 4.5) +
  annotate("text", x = 1.23, y = 786570, label = "819,570", color = "white", size = 4.5) +
  annotate("text", x = 1.82, y = 785936, label = "818,936", color = "white", size = 4.5) +
  annotate("text", x = 2.18, y = 1536000, label = "1,569,000", color = "white", size = 4.5) +
  labs(x = "Casual Rides / Member Rides", 
       y = "Number of Rides",
       title = "Number of Rides by User Type and Type of Bike in 2022") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())+
  scale_y_continuous(labels = scales::comma)
```

<img src="Visualizations\Total_Number_Casual_Member_Rides_By_Bike_Type_2022.png" width="920px" height="700px">

**Conclusion:** Casual Riders use electric bikes more often (percentage-wise) than Annual Members.

<a id="analyze3"></a>
### **What is the median ride duration of a (casual/annual) rider?**

```
# Create a dataframe to answer:
# What is the average and median duration of rides for casual riders and annual members?

summary_duration <- Divvy_Clean %>% 
  group_by(member_casual) %>% 
  summarize(avg_duration = mean(ride_duration),
            median_duration = median(ride_duration))
  
# Visualize the median duration of rides for each user type

ggplot(data = summary_duration,
       aes(x = member_casual, y = median_duration / 60, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Casual Rides / Member Rides",
       y = "Median in Minutes",
       title = "Median Ride Duration in Minutes by Each User Type in 2022") +
  geom_text(stat = "identity",
            aes(label = round(median_duration / 60, digits = 2)),
            vjust = 1.5, color = "white", size = 6) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())
```

<img src="Visualizations\Median_Ride_Duration_Casual_Member_2022.png" width="920px" height="700px">

**Conclusion:** Casual Riders typically take rides that are longer than Annual Member rides.

<a id="analyze4"></a>
### **What time of day do (casual/annual) riders most often ride at?**

```
# Create a dataframe to answer:
# What time of day do casual riders most often ride at?

casual_rides_by_hour <- Divvy_Clean %>% 
  filter(member_casual == 'casual') %>% 
  mutate(hr = lubridate::hour(started_at)) %>% 
  count(hr)

casual_rides_by_hour <- casual_rides_by_hour %>% 
  rename("num_rides" = "n")


# Create a dataframe to answer:
# What time of day do annual members most often ride at?

member_rides_by_hour <- Divvy_Clean %>% 
  filter(member_casual == 'member') %>% 
  mutate(hr = lubridate::hour(started_at)) %>% 
  count(hr)

member_rides_by_hour <- member_rides_by_hour %>% 
  rename("num_rides" = "n")

# Visualize the number of rides that casual riders take by time of day

ggplot(data = casual_rides_by_hour,
       aes(x = hr, y = num_rides)) +
  geom_bar(stat = "identity", fill = "bisque4") +
  labs(x = "Time of Day",
       y = "Number of Rides",
       title = "Number of Casual User Rides by Time of Day in 2022") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = casual_rides_by_hour$hr) +
  scale_y_continuous(labels = scales::comma)

# Visualize the number of rides that annual members take by time of day

ggplot(data = member_rides_by_hour,
       aes(x = hr, y = num_rides)) +
  geom_bar(stat = "identity", fill = "bisque4") +
  labs(x = "Time of Day",
       y = "Number of Rides",
       title = "Number of Annual Member Rides by Time of Day in 2022") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = member_rides_by_hour$hr) +
  scale_y_continuous(labels = scales::comma)
```

<img src="Visualizations\Casual_Rides_by_Time_of_Day_2022.png" width="920px" height="700px">

**Conclusion:** Casual Riders start rides the most often between 3pm and 7pm. There are no other peaks in usership through the day.

<img src="Visualizations\Member_Rides_by_Time_of_Day_2022.png" width="920px" height="700px">

**Conclusion:** Annual Members start rides the most often between 4pm and 7pm, but also have more rides starting in the morning between 7am and 9am.

<a id="analyze5"></a>
### **Which days of the week do (casual/annual) riders most often take rides?**

```
# Create a dataframe to answer:
# Which days of the week do casual riders most often take rides?

casual_rides_by_weekday <- Divvy_Clean %>%
  group_by(day_of_week) %>% 
  filter(member_casual == 'casual') %>% 
  count(member_casual)

# Create a dataframe to answer:
# Which days of the week do annual members most often take rides?

member_rides_by_weekday <- Divvy_Clean %>%
  group_by(day_of_week) %>% 
  filter(member_casual == 'member') %>% 
  count(member_casual)

# Arrange the days of the week properly for the next visualizations

casual_rides_by_weekday$day_of_week <- factor(casual_rides_by_weekday$day_of_week,
                                          c("Monday", "Tuesday", "Wednesday",
                                            "Thursday", "Friday", "Saturday", "Sunday"))

member_rides_by_weekday$day_of_week <- factor(member_rides_by_weekday$day_of_week,
                                               c("Monday", "Tuesday", "Wednesday",
                                                 "Thursday", "Friday", "Saturday", "Sunday"))

# Visualize the total number of rides taken by casual riders per weekday

ggplot(data = casual_rides_by_weekday,
       aes(x = day_of_week, y = n, group = 1)) +
  geom_line(linewidth = 1, color = "bisque4") +
  geom_point(size = 4) +
  labs(x = "Day of Week",
       y = "Number of Rides",
       title = "Total Number of Casual Rides Per Weekday in 2022") +
  geom_label_repel(aes(label = format(n, big.mark = ",")), nudge_x = .35) +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = scales::comma)

# Visualize the total number of rides taken by annual members per weekday

ggplot(data = member_rides_by_weekday,
       aes(x = day_of_week, y = n, group = 1)) +
  geom_line(linewidth = 1, color = "bisque4") +
  geom_point(size = 4) +
  labs(x = "Day of Week",
       y = "Number of Rides",
       title = "Total Number of Member Rides Per Weekday in 2022") +
  geom_label_repel(aes(label = format(n, big.mark = ",")), nudge_x = .35) +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = scales::comma)
```

<img src="Visualizations\Total_Number_Casual_Rides_Weekday_2022.png" width="920px" height="700px">

**Conclusion:** Casual Riders take more rides during the weekend. The day with the most rides taken is Saturday. Usage on weekdays goes down significantly.

<img src="Visualizations\Total_Number_Member_Rides_Weekday_2022.png" width="920px" height="700px">

**Conclusion:** Annual Members take more rides during weekdays. The days with the most rides are Tuesday, Wednesday, and Thursday, which are all nearly equal. Usage on the weekend goes down significantly.

<a id="analyze6"></a>
### **What time of the year has the most (casual/annual) riders?**

```
# Create a dataframe to answer:
# What time of the year has the most casual riders?

casual_rides_by_week <- Divvy_Clean %>% 
  filter(member_casual == 'casual') %>% 
  group_by(week = format(started_at, '%Y-%U')) %>% 
  count(week)

casual_rides_by_week <- casual_rides_by_week %>% 
  rename("num_rides" = "n")

# Create a dataframe to answer:
# What time of the year has the most annual members?

member_rides_by_week <- Divvy_Clean %>% 
  filter(member_casual == 'member') %>% 
  group_by(week = format(started_at, '%Y-%U')) %>% 
  count(week)

member_rides_by_week <- member_rides_by_week %>% 
  rename("num_rides" = "n")

# Visualize the number of rides that casual riders take
# split into 52 weeks

casual_rides_by_week %>% 
  mutate(total_rides = case_when(num_rides >= 50000 ~ "50k+",
                                 num_rides <= 49999 ~ "Below 50k")) %>% 
  ggplot(aes(x = week, y = num_rides, fill = total_rides)) +
  scale_fill_manual(values=c('#5276ad','bisque4')) +
  geom_bar(position = 'dodge', stat = 'identity') +
  labs(x = "Weeks",
       y = "Number of Rides",
       title = "Number of Casual Rides by Week in 2022") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::comma)

# Visualize the number of rides that annual members take
# split into 52 weeks

member_rides_by_week %>% 
  mutate(total_rides = case_when(num_rides >= 50000 ~ "50k+",
                                 num_rides <= 49999 ~ "Below 50k")) %>% 
  ggplot(aes(x = week, y = num_rides, fill = total_rides)) +
  scale_fill_manual(values=c('#5276ad','bisque4')) +
  geom_bar(position = 'dodge', stat = 'identity') +
  labs(x = "Weeks",
       y = "Number of Rides",
       title = "Number of Member Rides by Week in 2022") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::comma)
```

<img src="Visualizations\Casual_Rides_by_Week_2022.png" width="920px" height="700px">

**Conclusion:** Casual Riders take the most rides during late Spring and Summer. Usage during Winter is very low.

<img src="Visualizations\Member_Rides_by_Week_2022.png" width="920px" height="700px">

**Conclusion:** Annual Members take the most rides during Spring and Summer. Usage during Winter drops.

<a id="analyze7"></a>
### **Which start stations have the most (casual/annual) riders?**

```
# Create a dataframe to answer:
# Which start stations have the most casual riders?

casual_start_stations <- Divvy_Clean %>% 
  filter(member_casual == 'casual') %>% 
  group_by(start_station_name, start_lat, start_lng) %>% 
  count(start_station_name) %>%
  arrange(desc(n))

# Only include the 10 most used start stations for casual riders

casual_start_stations <- casual_start_stations[1:10,]

# Create a dataframe to answer:
# Which start stations have the most annual members?

member_start_stations <- Divvy_Clean %>% 
  filter(member_casual == 'member') %>% 
  group_by(start_station_name, start_lat, start_lng) %>% 
  count(start_station_name) %>%
  arrange(desc(n))

# Only include the 10 most used start stations for annual members

member_start_stations <- member_start_stations[1:10,]

# Visualize the top 10 start stations for casual rides

casual_sdf = st_as_sf(casual_start_stations,
                    coords = c("start_lng", "start_lat"), crs = 4326) %>% 
  group_by(start_station_name)

casual_sdf <- casual_sdf %>% 
  rename("num_rides" = "n")

mapview(casual_sdf, label = casual_start_stations$start_station_name,
        legend = FALSE,
        cex = "num_rides",
        alpha = 0)

# Visualize the top 10 start stations for annual members

member_sdf = st_as_sf(member_start_stations,
                      coords = c("start_lng", "start_lat"), crs = 4326) %>% 
  group_by(start_station_name)

member_sdf <- member_sdf %>% 
  rename("num_rides" = "n")

mapview(member_sdf, label = member_start_stations$start_station_name,
        legend = FALSE,
        cex = "num_rides",
        alpha = 0)
  ```

#### **Top 10 Start Stations for Casual Riders**
  
<img src="Visualizations\Top_10_Casual_Start_Stations_2022.png" width="445px" height="650px">

**Conclusion:** Casual Riders start most of their rides at locations near attractions and parks, especially near the shore of Lake Michigan. (Navy Pier, Millenium Park, Burnham Park, Theater on the Lake)

#### **Top 10 Start Stations for Annual Members**

<img src="Visualizations\Top_10_Member_Start_Stations_2022.png" width="435px" height="700px">

**Conclusion:** Annual Members start most of their rides at locations in the city.

<a id="act"></a>
# 6. Act Phase: Conclusions and Recommendations

Based on my analysis I was able to better understand how Casual Riders and Annual Members use Cyclistic bikes and how they differ from each other.

#### The Casual Rider

* Rides a classic bike most often, but will nearly as often choose an electric bike.
* Rides tend to last around 14 minutes.
* Rides most often start from 3pm to 7pm.
* Rides most often during the weekend, especially Saturday.
* Rides most often during late Spring and all of Summer. Almost never rides during Winter.
* Rides tend to start at locations near the shore of Lake Michigan, especially near parks and attractions like Navy pier, Millenium Park, and Theater on the Lake.

#### The Annual Member

* Rides a classic bike most often.
* Rides tend to last around 9 minutes.
* Rides most often start from 7am to 9am, and from 4pm to 7pm.
* Rides most often during weekdays, especially Tuesday, Wednesday, and Thursday.
* Rides most often during all of Spring and Summer. Rides during Winter happen less often.
* Rides tend to start at locations in the city.

It appears that Casual Riders use Cyclistic bikes for leisure (on weekends, 3pm-7pm, at or near parks and attractions) while Annual Members use the bikes as a main mode of transportation for work (on weekdays, 7am-9am, 4pm-7pm, in the city). Usage drops for both types of users during Winter, likely  because Chicago gets snow from December to March. I believe the two main motivations for Casual Riders to purchase Annual Memberships are how often they will use Cyclistic bikes and whether it will be a main mode of transporation for them.

The digital marketing strategy I recommend has two parts: Video Marketing and Social Media Posts. Creating captivating videos of people using Cyclistic bikes in all types of weather conditions would normalize riding bikes through all seasons of the year for leisure and recreation, especially around popular parks and attractions. Social Media Posts sharing these videos will allows people to share their experience with Cyclistic bikes as well as the areas featured in the videos.

I would also recommend for Cyclistic to consider the viability of seasonal passes for Spring and Summer because of the increased amount of rides taken during those times of year. This would give people the option to test out long-term membership without having to commit to a full year.

Finally, I would recommend that there be more analysis done on the amount of electric bikes available, the amount of stations available in the city, the amount of stations available near popular parks and attractions, and which casual riders are paying for a single ride or a day pass. A more in-depth analysis could be done if each ride were given a unique user ID to determine actual revenue amount per user type, which may help answer whether Casual Riders or Annual Members generate the most profit.