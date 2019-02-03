library(bigrquery)
proj_id <- 'hult-r-project'
sql <- "SELECT * FROM `air_france.AF`"
#tb <- 
AF <- bq_table_download(bq_project_query(proj_id, sql))
#AF <- data.frame(AF)
summary(AF)
AF$Profit <- (AF$Amount - AF$Total_Cost)
AF$ROI <- (AF$Profit / AF$Total_Cost)
AF$ROA <- (AF$Amount / AF$Total_Cost)

# Subsets 1: By Publisher, Global and US

library(sqldf)

Publisher_Performance <- sqldf("
                       SELECT
                       CASE WHEN Publisher_Name LIKE '%Google%' THEN 'Google'
                       WHEN Publisher_Name LIKE '%MSN%' THEN 'MSN'
                       WHEN Publisher_Name LIKE '%Overture%' THEN 'Overture'
                       WHEN Publisher_Name LIKE '%Yahoo%' THEN 'Yahoo'
                       ELSE 'Not defined'
                       END AS Publisher,
                       Sum(Profit) AS sum_profit, 
                       SUM(Amount)/SUM(Total_Cost) AS ROA
                       FROM AF
                       GROUP BY Publisher
                       ORDER BY sum(Profit) DESC;
                       ")
Scale_Performance <- sqldf("
                               SELECT
                               CASE WHEN Publisher_Name LIKE '%Global%' THEN 'Global'
                               WHEN Publisher_Name LIKE '%US%' THEN 'US'
                               ELSE 'Not defined'
                               END AS Scale,
                               Sum(Profit) AS sum_profit, 
                               SUM(Amount)/SUM(Total_Cost) AS ROA
                               FROM AF
                               GROUP BY Scale
                               ORDER BY sum(Profit) DESC;
                               ")
Global_Performance <- sqldf("
                               SELECT Publisher_Name,
                               Sum(Profit) AS sum_profit, 
                               SUM(Amount)/SUM(Total_Cost) AS ROA
                               FROM AF
                               WHERE Publisher_Name LIKE '%Global%'
                               GROUP BY Publisher_Name
                               ORDER BY sum(Profit) DESC;
                               ")
US_Performance <- sqldf("
                            SELECT Publisher_Name,
                            Sum(Profit) AS sum_profit, 
                            SUM(Amount)/SUM(Total_Cost) AS ROA
                            FROM AF
                            WHERE Publisher_Name LIKE '%US%'
                            GROUP BY Publisher_Name
                            ORDER BY sum(Profit) DESC;
                            ")
############################################################################################
Publisher_KPI <- sqldf("
                              SELECT
                               CASE WHEN Publisher_Name LIKE '%Google%' THEN 'Google'
                               WHEN Publisher_Name LIKE '%MSN%' THEN 'MSN'
                               WHEN Publisher_Name LIKE '%Overture%' THEN 'Overture'
                               WHEN Publisher_Name LIKE '%Yahoo%' THEN 'Yahoo'
                               ELSE 'Not defined'
                               END AS Publisher,
                               SUM(Clicks) AS sum_clicks,
                               SUM(Total_Cost) AS sum_cost,
                               SUM(Total_Volume_of_Bookings) AS sum_bookings,
                               SUM(Amount) AS sum_revenue,
                               (SUM(Amount) - SUM(Total_Cost)) AS sum_profit,
                               SUM(Amount)/SUM(Total_Cost) AS ROA
                               FROM AF
                               GROUP BY Publisher
                               ORDER BY sum(Profit) DESC;
                               ")
Kayak_clicks <- 2839
kayak_media_cost <- 3567.13
Kayak_total_bookings <- 208
kayak_total_revenue <- 233694.00
Kayak_profit <- 230126.87
Kayak_ROA <- 65.51
Kayak_obj <- c(Kayak_clicks, kayak_media_cost, Kayak_total_bookings, kayak_total_revenue, Kayak_profit, Kayak_ROA)

MD_obj <- 1.5* c(mean(Publisher_KPI[,2]),mean(Publisher_KPI[,3]),mean(Publisher_KPI[,4]),mean(Publisher_KPI[,5]),mean(Publisher_KPI[,6]),mean(Publisher_KPI[,7]))
Google <- c(Publisher_KPI[1,2], Publisher_KPI[1,3], Publisher_KPI[1,4], Publisher_KPI[1,5], Publisher_KPI[1,6], Publisher_KPI[1,7]) 
Yahoo <- c(Publisher_KPI[2,2], Publisher_KPI[2,3], Publisher_KPI[2,4], Publisher_KPI[2,5], Publisher_KPI[2,6], Publisher_KPI[2,7])
Overture <- c(Publisher_KPI[3,2], Publisher_KPI[3,3], Publisher_KPI[3,4], Publisher_KPI[3,5], Publisher_KPI[3,6], Publisher_KPI[3,7])
MSN <- c(Publisher_KPI[4,2], Publisher_KPI[4,3], Publisher_KPI[4,4], Publisher_KPI[4,5], Publisher_KPI[4,6], Publisher_KPI[4,7])

############################################################################

Publisher_KPI <- sqldf("
                              SELECT
                               CASE WHEN Publisher_Name LIKE '%Google%' THEN 'Google'
                               WHEN Publisher_Name LIKE '%MSN%' THEN 'MSN'
                               WHEN Publisher_Name LIKE '%Overture%' THEN 'Overture'
                               WHEN Publisher_Name LIKE '%Yahoo%' THEN 'Yahoo'
                               ELSE 'Not defined'
                               END AS Publisher,
                               SUM(Clicks) AS sum_clicks,
                               AVG(Engine_Click_Thru__) AS click_through_rate,
                               AVG(Trans__Conv___) AS conversion_rate,
                               SUM(Impressions) AS impression,
                               AVG(Avg__Pos_) AS average_position,
                               AVG(Total_Cost__Trans_) AS cost_per_conversion,
                               SUM(Total_Cost) AS sum_cost,
                               SUM(Total_Volume_of_Bookings) AS sum_bookings,
                               SUM(Amount) AS sum_revenue,
                               SUM(Amount)/SUM(Total_Cost) AS ROA
                               FROM AF
                               WHERE Profit > 0
                               GROUP BY Publisher
                               ")
MD_obj <- c(1.5*mean(Publisher_KPI[,2]),
            1.5*mean(Publisher_KPI[,3]),
            1.5*mean(Publisher_KPI[,4]),
            1.5*mean(Publisher_KPI[,5]),
            1.5*mean(Publisher_KPI[,6]),
            1.5*mean(Publisher_KPI[,7]),
            0.75*mean(Publisher_KPI[,8]),
            1.5*mean(Publisher_KPI[,9]),
            1.5*mean(Publisher_KPI[,10]),
            1.5*mean(Publisher_KPI[,11]))
Google <- c(Publisher_KPI[1,2], Publisher_KPI[1,3], Publisher_KPI[1,4], Publisher_KPI[1,5], Publisher_KPI[1,6], Publisher_KPI[1,7], Publisher_KPI[1,8], Publisher_KPI[1,9], Publisher_KPI[1,10], Publisher_KPI[1,11]) 
Yahoo <- c(Publisher_KPI[2,2], Publisher_KPI[2,3], Publisher_KPI[2,4], Publisher_KPI[2,5], Publisher_KPI[2,6], Publisher_KPI[2,7], Publisher_KPI[2,8], Publisher_KPI[2,9], Publisher_KPI[2,10], Publisher_KPI[2,11])
Overture <- c(Publisher_KPI[3,2], Publisher_KPI[3,3], Publisher_KPI[3,4], Publisher_KPI[3,5], Publisher_KPI[3,6], Publisher_KPI[3,7], Publisher_KPI[3,8], Publisher_KPI[3,9], Publisher_KPI[3,10], Publisher_KPI[3,11])
MSN <- c(Publisher_KPI[4,2], Publisher_KPI[4,3], Publisher_KPI[4,4], Publisher_KPI[4,5], Publisher_KPI[4,6], Publisher_KPI[4,7], Publisher_KPI[4,8], Publisher_KPI[4,9], Publisher_KPI[4,10], Publisher_KPI[4,11])

################## Optimization Model ##############
library(minpack.lm)
my_func <- function(b1, b2, b3, b4){
  obj <- b1*Google + b2*Yahoo + b3*Overture + b4*MSN
  return(as.numeric(obj))
}

KPI_model <- nlsLM(MD_obj ~ my_func( b1, b2, b3, b4))
summary(KPI_model)








############################################################################

library(ggplot2)
library(plotly)
# ggplot(data=AF, aes(Profit, ROA))+geom_point(aes(color = Publisher_Name)) # not linear relationship
ggplot(data=AF, aes(Amount, ROI))+geom_point(aes(color = Publisher_Name)) # not linear relationship
ggplot(data=AF, aes(Profit, ROI))+geom_point(aes(color = Publisher_Name)) # not linear relationship
ggplot(data=AF, aes(Amount, ROA))+geom_point(aes(color = Publisher_Name)) # not linear relationship

ggplot(data=AF, aes(Total_Volume_of_Bookings, Amount))+geom_point(aes(color = Publisher_Name)) #linear relationship
ggplot(data=AF, aes(Total_Volume_of_Bookings, Profit))+geom_point(aes(color = Publisher_Name)) #linear relationship 
ggplot(data=AF, aes(Amount, Profit))+geom_point(aes(color = Publisher_Name)) #linear relationship 
ggplot(data=AF, aes(ROA, ROI))+geom_point(aes(color = Publisher_Name)) # linear relationship
plot_ly(Publisher_Performance, x = ~ROA, y = ~ROI,  type="scatter", mode = "markers" , color = ~Publisher)
plot_ly(Publisher_Performance, x = ~sum_bookings, y = ~sum_profit,  type="scatter", mode = "markers" , color = ~Publisher)

