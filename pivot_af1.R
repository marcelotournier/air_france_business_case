library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

AF <- read_excel(
  "Google Drive/Marcelo 2018/HULT/Module B/R/business_cases/air_france/Air+France+Case+Spreadsheet+Supplement.xls", 
  sheet = "DoubleClick")

AF <- data.frame(AF)

AF$Keyword.ID = NULL
AF$Keyword.Type = NULL

# PIVOT TABLE WITH PUBLISHER NAMES, AMOUNT($), BOOKING COUNTS, AND VOLUMES:

Total_Bookings_per_Publisher <- # Get a summary table with counts and sums of booking vol per publisher
  AF %>% #get the AF dataframe...
  group_by(Publisher.Name) %>% #Group by Publisher name
  summarise(Sum.Bookings = sum(Total.Volume.of.Bookings), # Create columns for sum of volume
            Amount.Bookings = sum(Amount), # Create columns for sum of amount
            Count.Bookings = n()) # and another column for count of bookings

# new variable with ratio of bookings per publisher
Total_Bookings_per_Publisher$Ratio.Bookings <- 
  Total_Bookings_per_Publisher$Sum.Bookings/Total_Bookings_per_Publisher$Count.Bookings

# new variable spliting publisher in 2 cols: publisher (Google, MSN...) and scope(us,global)
split_publisher <- str_split_fixed(Total_Bookings_per_Publisher$Publisher.Name, " - ", 2)
Total_Bookings_per_Publisher <- cbind(split_publisher,Total_Bookings_per_Publisher)

Total_Bookings_per_Publisher$Publisher.Name <- NULL
colnames(Total_Bookings_per_Publisher) <- c("Publisher","Scope","Sum.Bookings","Amount", "Count.Bookings","Ratio.Bookings")

US_Global <- # Get a summary table with counts and sums of booking vol per US-Global Scope
  Total_Bookings_per_Publisher %>% #get the AF dataframe...
  group_by(Scope) %>% #Group by Publisher name
  summarise(Sum.Bookings = sum(Sum.Bookings), # Create columns for sum of volume
            Amount = sum(Amount), # Create columns for sum of amount
            Count.Bookings = sum(Count.Bookings)) # and another column for count of bookings
