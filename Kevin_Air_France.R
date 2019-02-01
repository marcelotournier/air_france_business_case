#importing the air france data
library(readxl)
Air_France_Case_Spreadsheet_Supplement <- read_excel("Hult/Data Science - R/Datasets/Air France Case Spreadsheet Supplement.xls", 
                                                     sheet = "DoubleClick")
View(Air_France_Case_Spreadsheet_Supplement)

Air_France <- Air_France_Case_Spreadsheet_Supplement

summary(Air_France)

unique(Air_France$`Publisher ID`)
unique(Air_France$`Publisher Name`)
length(unique(Air_France$`Keyword ID`))
summary(as.factor(Air_France$`Keyword ID`))
summary(as.factor(Air_France$`Publisher Name`))

as.data.frame(Air_France)
AF <- data.frame(Air_France)
for (i in 1:11) {
  print(summary(as.factor(AF[,i])))
  
}

library(sqldf)

Volume_SQL <- sqldf("SELECT `Publisher.Name`, SUM(`Total.Volume.of.Bookings`)
                    FROM AF
                    GROUP BY `Publisher.Name`;
                    ")



#Marcelo code
install.packages("tidyverse")

library(tidyr)
library(dplyr)
Total_Bookings_per_Publisher <- # Get a summary table with counts and sums of booking vol per publisher
  AF %>% #get the AF dataframe...
  group_by(Publisher.Name) %>% #Group by Publisher name
  summarise(Sum.Bookings = sum(Total.Volume.of.Bookings), # Create columns for sum of volume
            Pub.Unique.Keywords = n(), # and another column for unique keywords per publisher
            Sum.Total.Cost = sum(Total.Cost),
            Sum.Clicks = sum(Clicks),
            Sum.Impressions = sum(Impressions),
            Sum.Amount= sum(Amount),
            Avg.Ticket.Price = Sum.Amount/Sum.Bookings,
            Profit = Sum.Amount-Sum.Total.Cost) 

# new variable with ratio of bookings per publisher
Total_Bookings_per_Publisher$Ratio.Bookings <- 
  Total_Bookings_per_Publisher$Sum.Bookings/Total_Bookings_per_Publisher$Count.Bookings








x<- c()
for (i in 1:nrow(AF)){
  if (AF$Click.Charges[i] != AF$Total.Cost[i]){
    print(i)}
  else {
    x[i] <- c(i)
    }
}
print(x)

g_us_cost <- sum(AF$Total.Cost[which(AF$Publisher.Name=="Google - US")])

yahoo_amount <- sum(AF$Amount[which(AF$Publisher.Name=='Yahoo - US')])
print(yahoo_amount)
yahoo_cost <- sum(AF$Total.Cost[which(AF$Publisher.Name== 'Yahoo - US')])
print(yahoo_cost)
yahoo_amount - yahoo_cost

pub_id_vec <- c("Google - US","Google - Global","MSN - US","MSN - Global","Yahoo - US","Overture - US","Overture - Global")

for (i in 1:7){
  paste0("x",i) <- c(i)
  print(paste0("x",i)
  
}
sum(AF$Total.Cost[which(AF$Publisher.Name== pub_id_vec[i])])


