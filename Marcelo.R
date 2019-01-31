library(readxl)
AF <- read_excel("/Users/marcelotournier/Google Drive/Marcelo 2018/HULT/Module B/R/business_cases/air_france/Air+France+Case+Spreadsheet+Supplement.xls", sheet = "DoubleClick")
View(AF)

AF <- data.frame(AF)

AF$Keyword.ID <- NULL
AF$Keyword.Type <- NULL
AF$Click.Charges <- NULL


AF$Bid.Strategy <- gsub(
  x=AF$Bid.Strategy,pattern="Position 1 -2 Target",replacement="Position 1-2 Target")

AF$Bid.Strategy <- gsub(
  x=AF$Bid.Strategy,pattern="Postiion 1-4 Bid Strategy",replacement="Position 1-4 Bid Strategy")

AF$Bid.Strategy <- gsub(
  x=AF$Bid.Strategy,pattern="Postiion 1-4 Bid Strategy",replacement="Position 1-4 Bid Strategy")


## Getting descriptive statistics: match type, click, amount, total vol bookings


#Counting unique values in columns:

for (i in 1:ncol(AF))
{
    if (is.character(AF[,i])){
      print(summary(as.factor(AF[,i])))
    }
}

library(dplyr)
library(tidyr)
library(stringr)


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


split_publisher <- str_split_fixed(Total_Bookings_per_Publisher$Publisher.Name, " - ", 2)
Total_Bookings_per_Publisher <- cbind(split_publisher,Total_Bookings_per_Publisher)

Total_Bookings_per_Publisher$Publisher.Name <- NULL
colnames(Total_Bookings_per_Publisher) <- c("Publisher","Scope","Sum.Bookings","Amount", "Count.Bookings","Ratio.Bookings")


# KAYAK INFO: 
cols <- c("Search Engine"	,"Clicks",	"Media Cost",	"Total Bookings"	,
  "Avg Ticket"	,"Total Revenue",	"Net Revenue")
kayak <- c("Kayak",	"2,839",	"$3,567.13","208","$1,123.53","$233,694.00","$230,126.87")
total_ads <- data.frame(rbind(kayak),row.names = NULL)
colnames(total_ads) <- cols

################################
# KEYWORD STRATEGIES


#
#Questions : 

#- How to use assist keywords to lead consumers to a desired behavior?
#- Should keywords be added or dropped from the campaign? 
#- Should campaign tactics or copy be adjusted to improve campaign performance?

# COUNTING KEYWORDS

keyword_match <- # Get a summary table with counts and sums of booking vol per US-Global Scope
  AF %>% #get the AF dataframe...
  group_by(Match.Type) %>% #Group by Publisher name
  summarise(Sum.Bookings = sum(Total.Volume.of.Bookings), # Create columns for sum of volume
            Amount = sum(Amount)) # Create columns for sum of amount


#regx experiments:
grep(x=AF$Keyword,pattern = "paris",value=T)
grep(x=AF$Keyword,pattern = "paris",value=F)
grepl(x=AF$Keyword,pattern = "paris") & grepl(x=AF$Keyword,pattern = "france")
grepl(x=AF$Keyword,pattern = "paris")