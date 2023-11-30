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

# Check the number of rows with NA values

sum(!complete.cases(Divvy))

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

# Create a table to answer:
# How many total casual and member rides are there?

table(Divvy_Clean$member_casual)

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

# Create a dataframe to answer:
# Which types of bikes do casual riders and annual members use the most?

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