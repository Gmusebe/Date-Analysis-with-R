# Date and Time Analysis:

# Install libraries:
install.packages("lubridate","date","tidyverse","dplyr")

# Load libraries:
library(tidyverse)
library(lubridate)
library(dplyr)
library(date)

# Set working directory:
setwd("~/RProjects/Dates_&_Time")

# Load data:
date <- read.csv("dates.csv", header = TRUE, na.strings = "")
attach(date)

# View table
head(date)

# _____________________________________
# Install the excel package:
install.library("readxl")

# Load the library in the environment:
library("readxl")

# import data:
read_excel("dates.xlsx")
#_____________________________________


# Dates
# ____________________________________
# Set Dates column as Date data type:
date$Date <- as.Date(date$Date)

class(date$Date)
# Returns Date

df_date = data.frame(Date = date$Date) %>%
  mutate_at(vars(Date), funs(year, month, day))

# View output:
View(df_date)

df_date <- df_date %>% 
  add_column(D_time = paste(df_date$month,df_date$day,df_date$year, sep = ""),
             Year = df_date$year,
             Month = month.abb[df_date$month],
             Week = paste(lubridate::isoweek(df_date$Date), sep = ""),
             Day = weekdays(df_date$Date, abbreviate = TRUE),
             .after = "Date")

# Drop the numeric month and day variables and the duplicated year:
df_date <- select(df_date, select = -c(year, month, day))

# ____________________________________
# Time

# View head Time variables:
head(date$Time)

# data type of "Time"
class(date$Time)

# Format to 24hrs
date$Time <- format(as.POSIXct(date$Time,format='%I:%M %p'),format="%H:%M")

# To display the seconds"
# format(as.POSIXct(date$Time,format='%I:%M %p'),format="%H:%M:%S")

# _________________
# For reverse of "Time" to 12hrs format
format(strptime(date$Time, '%H:%M'), '%I:%M %p')
# _________________

# Merge the "D_time" & "Time" in "df_date" data:
df_date$D_time <- paste(df_date$D_time, date$Time, sep = " ")

# Save output in a .csv file:
 write.csv(df_date, "date_output.csv")
 
# Visualize
# ___________________________
 
 # Set up visualization tool
 library(quantmod)
 library(magrittr)
 library(ggplot2)
 library(plotly)
 library(broom)
 

# Set period of data needed:
start_date = as.Date("2018-01-01")
stop_date = as.Date("2021-03-18")

# Request quantmod to get the stock prices for:
# 1. Nike NKE
# 2. Tesla TSLA
# 3. Apple AAPL

  getSymbols(c("NKE","TSLA","AAPL"), src = "yahoo", from = start_date,
             to = stop_date)
  
# Create a dataframe that captures the adjusted price
stocks = as.xts(data.frame(
  NKE = NKE[,"NKE.Adjusted"],
  AAPL = AAPL[,"AAPL.Adjusted"],
  TSLA = TSLA[,"TSLA.Adjusted"]
  )
)

# 1) Assigning names to the columns given
names(stocks) = c("NKE", "AAPL", "TSLA")

# 2) define the index column as a date.
index(stocks) = as.Date(index(stocks))

# Visualize plot:
# Daily trends for the Nike, Apple and Tesla stocks: 
stocks_series = tidy(stocks) %>% 
  ggplot(aes(x=index,y=value)) +
  geom_line(aes(color = series), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800","#FF0000")) +
  labs(title = "Daily Stock Prices January 2018 - Feb 2021",
       subtitle = "End of Day Adjusted Prices",
       caption = " Source: Yahoo Finance") +
  xlab("Date") + ylab("Price") +
  theme_minimal()

ggplotly(stocks_series)

# For Tesla (TSLA) trends
tidy(stocks$TSLA) %>% 
  ggplot(aes(x=index,y=value)) +
  geom_line(color = "steelblue", size = 1) +
  labs(title = "TSLA Daily Stock Prices January 2018 - Feb 2021",
       
       subtitle = "End of Day Adjusted Prices",
       caption = " Source: Yahoo Finance") +
  xlab("Date") + ylab("Price") +
  theme_minimal() + 
  stat_smooth(
    color = "#FF0000", fill = "#FC4E07",
    method = "loess"
  )

# Area graphing:
p <- tidy(stocks) %>% 
  ggplot(aes(x=index,y=value)) +
  geom_area(aes(color = series, fill = series),
  alpha = 0.5, position = position_dodge(0.8)) +
  scale_color_manual(values = c("#00AFBB", "#000000","#FF0000")) +
  labs(title = "Daily Stock Prices January 2018 - Feb 2021",
       
       subtitle = "End of Day Adjusted Prices",
       caption = " Source: Yahoo Finance") +
  xlab("Date") + ylab("Price") +
  theme_minimal()

ggplotly(p)


# Monthly Averages
# Change the xts object to a dataframe:
stocks1 <- data.frame(stocks)

# Extract months from the date and add it as a variable:
stocks1 <- stocks1 %>%
  add_column(
    Trading_Month = months(as.Date(index(stocks)),abbreviate = FALSE)
  )

# Find monthly averages:
Monthly_Averages <- stocks1 %>%
  group_by(Trading_Month) %>% 
  summarise_each(funs(mean))

# Plot monthly averages:
ggplot(data = Monthly_Averages, aes(factor(x= Trading_Month,levels = month.name, ordered = TRUE))) +
  geom_line(aes(y = AAPL), color = "darkred", group =  1) + 
    geom_line(aes(y = NKE), color="steelblue", linetype="twodash", group =  1) +
  labs(x = "Month",
       y = "Price") +
  labs(title = "Monthly Stock Prices Averages",
       subtitle = "AAPL & NKE Averages",
       caption = " Source: Yahoo Finance") +
  ylab("Monthly_Avg Price") +
  theme_minimal()

# End