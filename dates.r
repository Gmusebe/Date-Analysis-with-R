# Date and Time Analysis:

# Install libraries:
install.packages("lubridate","date","tidyverse","dplyr")

# Load libraries:
library(tidyverse)
library(lubridate)
library(dplyr)
library(date)

# Set working direstory:
setwd("~/RProjects/Dates_&_Time")

# Load data:
date <- read.csv("dates.csv", header = TRUE, na.strings = "")
attach(date)

# _____________________________________
# Install the excel package:
# install.library("readxl")

# Load the library in the environment:
# library("readxl")

# import data:
# read_excel("dates.xlsx")
#_____________________________________

# Set Dates column as Date data type:
date$Date <- as.Date(date$Date)

class(date$Date)
# Returns Date

date = data.frame(Date = date$Date) %>%
  mutate_at(vars(Date), funs(year, month, day))

# View output:
View(date)

date <- date %>% 
  add_column(D_time = paste(date$month,date$day,date$year, sep = ""),
             Year = date$year,
             Month = month.abb[date$month],
             Week = paste(lubridate::isoweek(date$Date), sep = ""),
             Day = weekdays(date$Date, abbreviate = TRUE),
             .after = "Date")

# Drop the numeric month and day variables and the duplicated year:
date <- select(date, select = -c(year, month, day))

# Save output in a .csv file:
write.csv(date, "date_output.csv")

