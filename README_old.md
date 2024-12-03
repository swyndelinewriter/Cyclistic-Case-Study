---
title: "Cyclistic Casual Rider Conversion Case Study"
author: "W. E. Mitchell"
date: "November 9, 2024"
output: html_document
---

#### Setup

```{r setup, include=FALSE}
# Set working directory to location of CSV data file
setwd("C:/Users/willd/Documents/Data Analysis Certification/Cyclistic Case Study/Cyclistic_data_CSV_202410_202311")

# Load necessary libraries
library(dplyr)
library(tidyverse)
library(scales)

# Load data
file_list <- list.files(pattern = "*.csv")
tripdata_combined <- file_list %>% map_df(~read.csv(.))

# Check data
head(tripdata_combined)
str(tripdata_combined)
```

# Cyclistic Casestudy Scenerio

This Case study was the capstone for my Google Datanalyst certification. The Scenerio is a fictional bike share company called Cyclistic.

In this scenerio I am junior data analyst working on the marketing analyst team at Cyclistic, a bike-share company in Chicago. The director of marketing believes the company’s future success depends on maximizing the number of annual memberships. Therefore, your team wants to understand how casual riders and annual members use Cyclistic bikes diŦerently. From these insights, your team will design a new marketing strategy to convert casual riders into annual members. But ůrst, Cyclistic executives must approve your recommendations, so they must be backed up with compelling data insights and professional data visualizations.

The Data is from Motivate International Inc. and was made available under [THIS LICENSE AGREEMENT](https://divvybikes.com/data-license-agreement). The data was anonymized to remove any data linked to an individual. The dataset can be found [here.](https://divvy-tripdata.s3.amazonaws.com/index.html)

## Stakeholders

* **Lily Moreno:** The director of marketing and your manager. Moreno is responsible for the development of campaigns and initiatives to promote the bike-share program. These may include email, social media, and other channels.

* **Cyclistic marketing analytics team:** A team of data analysts who are responsible for collecting, analyzing, and reporting data that helps guide Cyclistic marketing strategy. You joined this team six months ago and have been busy learning about Cyclistic’s mission and business goals—as well as how you, as a junior data analyst, can help Cyclistic achieve them.

* **Cyclistic executive team:** The notoriously detail-oriented executive team will decide whether to approve the recommended marketing program.

# ASK

Three question will guide the marketing department:
1. How do annual members and casual riders use Cyclistic bikes differently?
2. Why would casual riders buy Cyclistic annual memberships?
3. How can Cyclistic use digital media to influence casual riders to become members?

## Deliverables
    1. A clear statement of the business task
    2. A description of all data sources used
    3. Documentaion of any cleaning or manipulation of data
    4. A summary of data analysis
    5. Supporting visualizations and key findings
    6. Your top three recommendations based on your analysis

## Statement of Business Task
Develop marketing strategies aimed at converting casual riders into paid annual membership holders. By analyzing the different usage patterns of members and casual riders, we can identify the most effective tactics for encouraging casual riders to upgrade to memberships.

# Prepare

## Description of All Data Sources Used

The dataset for this case study was retrieved from its AWS storage location and downloaded to my local machine for processing and cleaning. The data is organized into comma-separated variable files, detailing all trips that took place each month from November 2023 to October 2024. This dataset is reliable, as it has been provided by our stakeholders, and each entry represents a unique trip with a distinct ride ID, ensuring the originality of the dataset.

The dataset includes comprehensive information on trips taken by Cyclistic customers, with the most recent data being only thirty days old, making it highly current. I have visually inspected each file using a spreadsheet program. Each dataset entry comprises start and stop times, latitude and longitude coordinates for the origin and destination, the type of bicycle used, and whether the user was a member or a casual user. Additionally, the dataset includes the names and IDs of the starting and ending stations, although these entries are not entirely consistent. This might allow us to identify if certain station pairs are more frequently used.

The goal is to analyze this data to determine patterns in distance traveled and duration of use, thereby identifying differences in usage between members and casual users. These insights will be invaluable in developing strategies to convert casual users into annual members.

All CSV files were combined into a single dataframe called tripdata_combined


# PROCESS

## Documentation of any Cleaning or Data Manipulation

Significant portions of data are missing from certain entries, particularly the starting and ending stations, or occasionally both. It is unclear whether this issue stems from a technical fault or indicates that the bicycle was not at a station when it was taken or returned. Despite this, the data should still be usable as it includes the start time stored as Year-Month-Day-Hour-Minute-Second as CHR datatype.

First, I used the function is.na(dataframe) to check for any missing values within the dataset. Upon identifying missing values, I implemented a code to remove any rows with missing data in the essential columns for my analysis: started_at, ended_at, rideable_type, and member_casual. Additionally, I ensured that there were no duplicate entries and verified that the data types were consistent across all entries in these columns. Subsequently, I converted the started_at and ended_at columns into a Year-Month-Day-Hour-Minute-Second format. Finally, I calculated the duration of each trip in seconds and stored this information in a new column named ride_length.

### Clean Data using `dplyr` Package
Store cleaned data in a new dataframe and remove rows with missing values in essential columns.

```{r clean_data}
head(tripdata_combined)
str(tripdata_combined)

tripdata_combined_clean <- tripdata_combined %>% 
  filter(
    !is.na(started_at) &
    !is.na(ended_at) & 
    !is.na(rideable_type) & 
    !is.na(member_casual)
  ) %>% 
  distinct() %>% 
  mutate(
    rideable_type = as.character(rideable_type),
    started_at = as.character(started_at),
    ended_at = as.character(ended_at),
    member_casual = as.character(member_casual)
  )

```

### Inspect Cleaned Data
Inspect the cleaned data to ensure it is processed correctly.

```{r inspect_cleaned_data}
tripdata_combined_clean <-  tripdata_combined_clean %>% 
  mutate(
    started_at = ymd_hms(started_at),
    ended_at = ymd_hms(ended_at)
  )
```

### Convert started_at and ended_at to Date-Time
Convert the started_at and ended_at columns to date-time format.

```{r convert_to_YMD_HMS}
tripdata_combined_clean <-  tripdata_combined_clean %>% 
  mutate(
    started_at = ymd_hms(started_at),
    ended_at = ymd_hms(ended_at)
  )
```

### Calculate Trip Duration and Weekday
Calculate the duration of each trip and determine the day of the week for each trip.

```{r calculate_trip_duration_weekday}
tripdata_combined_clean <- tripdata_combined_clean %>% 
  mutate(ride_length = ended_at - started_at) %>% 
  mutate(weekday = wday(started_at, label = TRUE))
```

### Verify Data Processing
Verify the data processing steps and inspect the final cleaned data.

```{r verify_data_processing}
str(tripdata_combined_clean)
head(tripdata_combined_clean)
View(tripdata_combined_clean)
```

# ANALYZE

### Classify Days as Weekdays or Weekends
Classify days as weekdays or weekends.

```{r classify_days}
tripdata_combined_clean <- tripdata_combined_clean %>% mutate(day_type = ifelse(weekday %in% c("Sat", "Sun"), "weekend", "weekday"))
```

### Calculate Average Ride Length
Calculate the average ride length for members and casual riders.

```{r calculate_avg_ride_length}
# Calculate average ride length for members and casual riders
avg_ride_length <- tripdata_combined_clean %>%
  group_by(member_casual) %>%
  summarise(avg_ride_length = mean(ride_length, na.rm = TRUE))

# Calculate average ride length for weekdays and weekends for members and casual riders
avg_ride_length_day_type <- tripdata_combined_clean %>%
  group_by(member_casual, day_type) %>%
  summarise(avg_ride_length = mean(ride_length, na.rm = TRUE), .groups = 'drop')

# Separate variables for members and casual riders
avg_ride_length_members <- avg_ride_length %>%
  filter(member_casual == "member") %>%
  .$avg_ride_length

avg_ride_length_casual <- avg_ride_length %>%
  filter(member_casual == "casual") %>%
  .$avg_ride_length

# Separate variables for average ride length on weekdays and weekends for members
avg_ride_length_members_weekday <- avg_ride_length_day_type %>%
  filter(member_casual == "member" & day_type == "weekday") %>%
  .$avg_ride_length

avg_ride_length_members_weekend <- avg_ride_length_day_type %>%
  filter(member_casual == "member" & day_type == "weekend") %>%
  .$avg_ride_length

# Separate variables for average ride length on weekdays and weekends for casual riders
avg_ride_length_casual_weekday <- avg_ride_length_day_type %>%
  filter(member_casual == "casual" & day_type == "weekday") %>%
  .$avg_ride_length

avg_ride_length_casual_weekend <- avg_ride_length_day_type %>%
  filter(member_casual == "casual" & day_type == "weekend") %>%
  .$avg_ride_length

# Display results
print(avg_ride_length_members)
print(avg_ride_length_casual)
print(avg_ride_length_members_weekday)
print(avg_ride_length_members_weekend)
print(avg_ride_length_casual_weekday)
print(avg_ride_length_casual_weekend)
```

### Determine the Most Common Day of Usage
Determine the most common day of usage for members and casual riders.

```{r most_common_day_usage}
# Group by member status and weekday, then count the occurrences
weekday_counts <- tripdata_combined_clean %>%
  group_by(member_casual, weekday) %>%
  summarise(count = n(), .groups = 'drop')

# Find Most Common Day for Members
most_common_day_member <- weekday_counts %>%
  filter(member_casual == "member") %>%
  slice_max(count) %>%
  select(weekday, count)

# Find Most Common Day for Casuals
most_common_day_casual <- weekday_counts %>%
  filter(member_casual == "casual") %>%
  slice_max(count) %>%
  select(weekday, count)

# Display Results
print(most_common_day_casual)
print(most_common_day_member)
print(weekday_counts)
```

### Determine Average Number of Trips Taken on Weekend vs. Weekdays
Calculate the average number of trips taken on weekends versus weekdays.

```{r avg_trips_day_weekend}
# Classify 'weekday' as weekend or weekday
tripdata_combined_clean <- tripdata_combined_clean %>%
  mutate(day_type = ifelse(weekday %in% c("Sat", "Sun"), "weekend", "weekday"))

# Group by 'member_casual' and 'day_type', then count
day_type_counts <- tripdata_combined_clean %>%
  group_by(member_casual, day_type) %>%
  summarise(count = n(), .groups = 'drop')

# Calculate average number of trips weekdays vs weekends
avg_trips <- day_type_counts %>%
  group_by(member_casual) %>%
  summarise(
    avg_weekday_trips = mean(count[day_type == "weekday"]),
    avg_weekend_trips = mean(count[day_type == "weekend"])
  )

# Display results
print(avg_trips)
```

### Calculate Average Trip Length for Both Casual and Member Users
Calculate average trip length for both casual and member users.

```{r acg_trip_length_cas_memb}
# Convert 'ride_length' from difftime to numeric minutes
tripdata_combined_clean <- tripdata_combined_clean %>%
  mutate(ride_length_minutes = as.numeric(ride_length, units = "mins"))

# Calculate average ride length for each user type
avg_ride_length_by_day <- tripdata_combined_clean %>%
  group_by(member_casual, weekday) %>%
  summarise(avg_ride_length = mean(ride_length_minutes, na.rm = TRUE), .groups = 'drop')

# Split results into separate dataframes
avg_ride_length_casual <- avg_ride_length_by_day %>%
  filter(member_casual == "casual")
avg_ride_length_members <- avg_ride_length_by_day %>%
  filter(member_casual == "member")

# Display results
print(avg_ride_length_casual)
print(avg_ride_length_members)
```

##A Summary of Data Analysis

One of the most notable findings was the difference in ride lengths between the two user groups. While members take significantly more trips than casual riders, it was surprising to observe that the average ride length for casual users is more than twice that of members.

This trend suggests that members likely have stations located closer to their starting points and destinations. Therefore, the primary usage difference between the two groups appears to be the proximity of stations to their regular travel routes.

# SHARE

## Supporting Visualizations and Key Findings

### Number of Trips by Day of the Week

#### Define the Order for Weekdays
Define the correct order for the weekdays.

```{r week_day_order1}
# Define the correct order for weekdays
weekday_order <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
```

#### Convert 'weekday' to a Factor
Convert the 'weekday' column to a factor with the specified order and calculate the count in millions.

```{r convert_weekday_factor}
weekday_counts <- weekday_counts %>% 
  mutate(weekday = factor(weekday, levels = weekday_order),
         count_in_millions = count / 1000000)
```

#### Add Units to Y-Axis Labels
Create a function to add units to the Y-axis labels.

```{r add_y_unit1}
add_mil <- function(x){
  paste0(scales::comma_format()(x), "Mil")
}
```

#### Bar Plot
Create a bar plot to visualize the number of trips by day of the week.

```{r number_of_trips_by_day}
library(ggplot2)

ggplot(weekday_counts, aes(
  x = weekday, y = count_in_millions, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("casual" = "blue", "member" = "red")) +
  labs(
    title = "Number of Trips by Day of the Week",
    x = "Day of the Week",
    y = "Count of Trips (in millions)",
    fill = "User Type"
  ) +
  scale_y_continuous(labels = add_mil) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

### Visualization of Average Trip lengths by user type per day

#### Combine Data for Casual and Member Users
Combine `avg_ride_length_casual` and `avg_ride_length_members` into a single dataframe.

```{r combine_trip_lengths}
avg_ride_length_combined <- bind_rows(avg_ride_length_casual, avg_ride_length_members)
```

#### Define the Order for Weekdays
Define the correct order for the weekdays and convert the 'weekday' column to a factor with the specified order.

```{r week_day_order2}
weekday_order <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

avg_ride_length_combined <- avg_ride_length_combined %>%
  mutate(weekday = factor(weekday, levels = weekday_order))
```

#### Add Units to Y-Axis Labels
Create a custom function to add 'Min' to the y-axis labels.

```{r add_y_unit2}
add_min <- function(x) {
  paste0(comma(x), " Min")
}
```

#### Bar Plot
Create a bar plot to visualize the average ride lengths by day of the week for casual and member users.

```{r avg_trip_lengths_by_day}
library(ggplot2)

ggplot(avg_ride_length_combined, aes(
  x = weekday, y = avg_ride_length, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("casual" = "blue", "member" = "red")) +
  labs(
    title = "Average Ride Length by Day of the Week",
    x = "Day of the Week",
    y = "Average Ride Length in Minutes",
    fill = "User Type"
  ) +
  scale_y_continuous(labels = add_min) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

## Key Findings As Related to Business Task

* How do annual members and casual riders use Cyclistic bikes differently?
  * Members use bikes primarily for commuting while casuals seem to primarily use them for recreation.

* Why would casual riders buy Cyclistic annual memberships?
  * If the value of being members were demonstrated over their current means of commuting.
  
* How can Cyclistic use digital media to influence casual riders to become members?
  * Digital marketing that focuses on how much money can be saved by riding to work-week.
  * Targeted advertising on areas with a high density of commuters.
  * Build partnerships with local employers to offer services as part of employee befits packages.
  
# ACT

## Top Three Recommendations Based on Analysis

1. **Promotional Campaigns**
  * *"Commute & Save" Offers* - Highlight the cost savings of an annual membership compared to occasional rentals, especially for commuters.
  * *Limited-Time Discounts* - Offer discounted rates or free trials for the first month to encourage sign-ups.

2. **Targeted Advertising:**
  * *Geographical Targeting* - Focus on areas with a high density of potential commuters, like residential neighborhoods near business districts.
  * *Segmented Marketing* - Tailor messages to different demographic groups, emphasizing convenience for young professionals and cost savings for families.
  
3. **Workplace Partnerships:**
  * *Corporate Memberships* - Partner with businesses to offer corporate memberships as part of employee benefits packages.
  * *Incentive Programs* - Encourage employers to provide incentives for employees who bike to work, such as wellness programs or reimbursements.
