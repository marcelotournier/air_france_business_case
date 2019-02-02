library(bigrquery)
proj_id <- 'hult-r-project'
sql <- "SELECT * FROM `air_france.AF`"
tb <- bq_project_query(proj_id, sql)
google_AF <- bq_table_download(tb)
AF <- data.frame(google_AF)
summary(AF)


# Counting unique values in columns:

for (i in 1:ncol(AF)){
  if (is.character(AF[,i])){
    print(summary(as.factor(AF[,i])))
  }
}

#summary(as.factor(AF$Keyword))
#summary(as.factor(AF$Campaign)) # 1214 unassigned
#summary(as.factor(AF$Category)) # 1214 unassigned
#This column contain values of destinations, 'airflight gernal', 'discountr', and 'vaccation'.
#It does not seem to be valuable because of the quality of record. 
#summary(as.factor(AF$Keyword_Group)) # 1214 unassigned
#summary(as.factor(AF$Keyword_Type)) # Useless. This is columns has only one value "unassigned".
#summary(as.factor(AF$Keyword_ID)) # Unique IDs for each observation
#summary(as.factor(AF$Match_Type)) # 48 N/A. What is it?
#summary(as.factor(AF$Bid_Strategy)) # 1224 NA's. What is it?
#summary(as.factor(AF$Status)) # 1120 Unavalable 
#unique(AF$Publisher_ID) # 7 IDs
#unique(AF$Publisher_Name) # 7 names




# Impute missing values with average. But there is no missing values.
for( v in ncol(AF)){
  if( mode(AF$v) == 'numeric') {
    for(i in 1:nrow(AF)){
      AF$v[which(is.na(AF$v))] <- mean(AF$v[which(!is.na(AF$v))])
      return(AF)
    }
  }
}

AF[which(is.na(AF$Amount)), ] # Result: there are no missing values in numeric columes. 



# Subsets 1: By Publisher, Global and US

library(sqldf)

publisher_performance <- sqldf("SELECT Publisher_Name, 
                                 ROUND(SUM(Amount)/SUM(Total_Cost),3) AS ROA
                                 FROM AF
                                 GROUP BY Publisher_Name
                                 ORDER BY ROUND(SUM(Amount)/SUM(Total_Cost),3) DESC;
                                ")
Publisher_KPI <- sqldf("
                       SELECT Publisher_Name,
                       ROUND(Sum(Impressions),3) AS Total_Impression,
                       ROUND(AVG(Engine_Click_Thru__),3) AS Avg_Click_Through,
                       ROUND(AVG(Trans__Conv___),3) AS Avg_Trans_Conv__,                      
                       ROUND(SUM(Amount)/SUM(Total_Cost),3) AS ROA
                       FROM AF
                       GROUP BY Publisher_Name;
                       ")
library(minpack.lm)
max_impression <- max(Publisher_KPI$Total_Impression)
max_click_through <- max(Publisher_KPI$Avg_Click_Through)
max_trans_con <- max(Publisher_KPI$Avg_Trans_Conv__)
max_roa <- max(Publisher_KPI$ROA)

MD_obj <- c(max_impression, max_click_through, max_trans_con, max_roa)
GG <- c(1808326, 8.994, 0.433, 7.686) 
GU <- c(3855689, 15.090, 0.415, 4.936)
MG <- c(139979, 7.336, 1.133, 11.967)
MU <- c(170120, 8.264, 0.731, 11.277)
OG <- c(17898727, 3.548, 0.236, 6.689)
OU <- c(17062488, 2.673, 0.095, 2.447)
YH <- c(933345, 16.059, 1.829, 19.098)
my_func <- function(b1, b2, b3, b4, b5, b6, b7){
  obj <- b1*GG + b2*GU + b3*MG + b4*MU + b5*OG + b6*OU + b7*YH
  return(as.numeric(obj))
}


KPI_model <- nlsLM(MD_obj ~ my_func(b1, b2, b3, b4, b5, b6, b7))
summary(KPI_model)


################################ Optimization Model for US Publishers ####################################################
Publisher_KPI_Global <- sqldf("
                       SELECT Publisher_Name,
                       ROUND(Sum(Impressions),3) AS Total_Impression,
                       ROUND(AVG(Engine_Click_Thru__),3) AS Avg_Click_Through,
                       ROUND(AVG(Trans__Conv___),3) AS Avg_Trans_Conv__,                      
                       ROUND(SUM(Amount)/SUM(Total_Cost),3) AS ROA
                       FROM AF
                       WHERE Publisher_Name LIKE '%Global%'
                       GROUP BY Publisher_Name;
                       ")

max_impression_g <- max(Publisher_KPI_Global$Total_Impression)
max_click_through_g <- max(Publisher_KPI_Global$Avg_Click_Through)
max_trans_con_g <- max(Publisher_KPI_Global$Avg_Trans_Conv__)
max_roa_g <- max(Publisher_KPI_Global$ROA)

MD_obj_g <- c(max_impression_g, max_click_through_g, max_trans_con_g, max_roa_g)

my_func_g <- function(b1, b2, b3){
  obj <- b1*GG + b2*MG + b3*OG
  return(as.numeric(obj))
}

KPI_model_g <- nlsLM(MD_obj_g ~ my_func_g(b1, b2, b3))
summary(KPI_model_g)


######################### Optimization model for US Publishers #######################################
Publisher_KPI_US <- sqldf("
                              SELECT Publisher_Name,
                              ROUND(Sum(Impressions),3) AS Total_Impression,
                              ROUND(AVG(Engine_Click_Thru__),3) AS Avg_Click_Through,
                              ROUND(AVG(Trans__Conv___),3) AS Avg_Trans_Conv__,                      
                              ROUND(SUM(Amount)/SUM(Total_Cost),3) AS ROA
                              FROM AF
                              WHERE Publisher_Name LIKE '%US%'
                              GROUP BY Publisher_Name;
                              ")

max_impression_us <- max(Publisher_KPI_US$Total_Impression)
max_click_through_us <- max(Publisher_KPI_US$Avg_Click_Through)
max_trans_con_us <- max(Publisher_KPI_US$Avg_Trans_Conv__)
max_roa_us <- max(Publisher_KPI_US$ROA)

MD_obj_us <- c(max_impression_us, max_click_through_us, max_trans_con_us, max_roa_us)

my_func_us <- function(b1, b2, b3, b4){
  obj <- b1*GU + b2*MU + b3*OU + b4*YH
  return(as.numeric(obj))
}

KPI_model_us <- nlsLM(MD_obj_us ~ my_func_us(b1, b2, b3, b4))
summary(KPI_model_us)


# Subset 2: By campaigns in each publisher

