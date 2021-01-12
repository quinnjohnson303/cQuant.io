#Setting working directory
setwd("~/Desktop/cQuant Programming")

#Loading in packages
library(tidyverse)
library(ggplot2)
library(vroom)
library(lubridate)
library(gganimate)
library(transformr)

#### TASK 1 ####
#Reading in historical price data
price2016 <- vroom("historicalPriceData_ERCOT_DA_Prices_2016.csv")
price2017 <- vroom("historicalPriceData_ERCOT_DA_Prices_2017.csv")
price2018 <- vroom("historicalPriceData_ERCOT_DA_Prices_2018.csv")
price2019 <- vroom("historicalPriceData_ERCOT_DA_Prices_2019.csv")

#Combining all price data into a dataframe
price <- rbind(price2016, price2017, price2018, price2019)

#Checking if any data types need to be altered
lapply(price, class)
#All good

#### TASK 2 ####
#Initializing new df using price data
avgPrice <- price %>%
  
  #Creating year variable to help groupings
  mutate(Year = year(Date),
  
         #Creating month variable to help groupings
         Month = month(Date)) %>%
  
  #Grouping by settlement point, month, and year
  group_by(SettlementPoint, Month, Year) %>%
  
  #Calculating the average price for each group
  summarise(AveragePrice = mean(Price)) %>%
  
  #Sorting by year-month
  arrange(Year, Month)

#### TASK 3 ####
#Using write.csv to save avgPrice as a csv in the working directory
write.csv(avgPrice, "AveragePriceByMonth.csv")

#### TASK 4 ####
#Initializing new df using price data
hrVolatility <- price %>%
  
  #Keeping only data from hubs
  filter(., str_detect(SettlementPoint, "HB_")) %>%
  
  #Keeping only data with price > 0
  filter(., Price > 0) %>%
  
  #Creating year variable to help groupings
  mutate(Year = year(Date)) %>%
  
  #Grouping by settlement point, month, and year
  group_by(SettlementPoint, Year) %>%
  
  #Calculating the hourly volatility for each group
  summarise(HourlyVolatility = sd(log(Price))) %>%
  
  #Sorting by year
  arrange(Year)

#### TASK 5 ####
#Using write.csv to save hrVolatility as a csv in the working directory
write.csv(hrVolatility, "HourlyVolatilityByYear.csv")

#### TASK 6 ####
#Initializing new df using hourly volatility data
maxVolatility <- hrVolatility %>%
  
  #Grouping by year so we get each year's maximum hourly volatility
  group_by(Year) %>%
  
  #Keeping only the maximum value and its row
  filter(HourlyVolatility == max(HourlyVolatility))
  
#Using write.csv to save maxVolatility as a csv in the working directory
write.csv(maxVolatility, "MaxVolatilityByYear.csv")

#### TASK 7 ####
#Initializing function to read in a settlement point and create and write a csv with the spot price data
translation <- function(settlement){
  
  #Initializing new df using price data
  settlement_data <- price %>%
    
    #Keeping data from only desired settlement point
    filter(SettlementPoint == settlement) %>%
    
    #Creating hour variable to help with column naming
    mutate(hour = hour(Date),
           
           #Separating date to help with grouping of prices
           Date = ymd(strptime(Date, "%Y-%m-%d")),
           
           #Renaming SettlementPoint to Variable 
           #I know there are many other ways to do this, I just know this way works fine
           Variable = SettlementPoint) %>%
    
    #Removing unnecessary column
    select(-SettlementPoint) %>%
    
    #Grouing by date
    group_by(Date) %>%
    
    #Spreading the data so the hour is the column and price is in the rows
    spread(hour, Price) %>%
    
    #Moving Variable to in front of Date
    relocate(Variable, .before = Date)
  
  #Renaming the hour columns to include the "X" as seen in the supplemental data
  #This data goes from 0-23, not 1-24, as the historical data provided used the hour-beginning convention
  colnames(settlement_data)[3:ncol(settlement_data)] <- paste0("X", colnames(settlement_data)[3:ncol(settlement_data)])
  
  #Forming the filename for the specific settlement
  filename <- paste0("spot_", settlement, ".csv")
  
  #Writing the csv
  write.csv(settlement_data, filename)
  
  #Returning a statement saying that the settlement is done
  return(paste0(settlement, " Done!"))
}

#Getting a list of each of the settlement points to feed to the function
settlementList <- unique(price$SettlementPoint) 

#Calling the function for each of the points
lapply(settlementList, translation)

#### BONUS - MEAN PLOTS ####
#Beginning  plot of settlement hubs
#Getting only data from settlement hubs
avgPriceHB <- filter(avgPrice, str_detect(SettlementPoint, "HB_"))

#Removing HB_ from the beginning of the settlement point names
avgPriceHB$SettlementPoint <- str_remove(avgPriceHB$SettlementPoint, "HB_")

#Reconsructing date to be used in the plot
avgPriceHB$Date <- as.Date(paste(avgPriceHB$Year, avgPriceHB$Month, 1, sep = "-"))

#Plotting the average plot across months and for each settlement point
ggplot(data = avgPriceHB, aes(x = format(Date, "%Y-%m"), y = AveragePrice, group = SettlementPoint)) +
  #Line plot
  geom_line(aes(col = SettlementPoint)) +
  #Making the x axis ticks vertical
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 7))+
  #Renaming everything
  labs(col = "Hub", x = "Month", y = "Average Price ($)", title = "Average Monthly Price by Hub") + 
  #Making better y axis
  scale_y_continuous(breaks = seq(0, 130, 10), labels = seq(0, 130, 10))

#Beginning  plot of loading zones
#Getting only data from settlement hubs
avgPriceLZ <- filter(avgPrice, str_detect(SettlementPoint, "LZ_"))

#Removing LZ_ from the beginning of the settlement point names
avgPriceLZ$SettlementPoint <- str_remove(avgPriceLZ$SettlementPoint, "LZ_")

#Reconsructing date to be used in the plot
avgPriceLZ$Date <- as.Date(paste(avgPriceLZ$Year, avgPriceLZ$Month, 1, sep = "-"))

#Plotting the average plot across months and for each settlement point
ggplot(data = avgPriceLZ, aes(x = format(Date, "%Y-%m"), y = AveragePrice, group = SettlementPoint)) +
  #Line plot
  geom_line(aes(col = SettlementPoint)) +
  #Making the x axis ticks vertical
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 7))+
  #Renaming everything
  labs(col = "Loading Zone", x = "Month", y = "Average Price ($)", title = "Average Monthly Price by Loading Zone") + 
  #Making better y axis
  scale_y_continuous(breaks = seq(0, 130, 10), labels = seq(0, 130, 10))

#### BONUS - VOLATILITY PLOT ####

#Removing HB_ from the beginning of the settlement point names
hrVolatility$SettlementPoint <- str_remove(hrVolatility$SettlementPoint, "HB_")

#Doing similar plot as the previous bonus task
ggplot(data = hrVolatility, aes(x = Year, y = HourlyVolatility, group = SettlementPoint, col = SettlementPoint)) + 
  #Line plot
  geom_line() +
  #Renaming
  labs(y = "Hourly Volatility", title = "Hourly Volatility by Year", col = "Hub",
       subtitle = "NOTE: Only have 2019 data for \'PAN\' (1.07) ")+
  #Making better y axis
  scale_y_continuous(breaks = seq(0, 1.1, .2), labels = seq(0, 1.1, .2)) #+ 
  #Optional animation of change over time
  #transition_reveal(Year)


  
