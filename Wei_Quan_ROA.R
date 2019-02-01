AF <- as.data.frame(Air_France_Case_Spreadsheet_Supplement)

sum(AF$Amount) / sum(AF$`Total Cost`)
ROA_Whole<-sum(AF$Amount) / sum(AF$`Total Cost`)



Google_US_Cost <- sum(AF$`Total Cost`[which(AF$`Publisher Name`=="Google - US")])
Google_US_Revenue <- sum(AF$Amount[which(AF$`Publisher Name`=="Google - US")])
Google_US_ROA <- Google_US_Revenue / Google_US_Cost

Google_Global_Cost <- sum(AF$`Total Cost`[which(AF$`Publisher Name`=="Google - Global")])
Google_Global_Revenue <- sum(AF$Amount[which(AF$`Publisher Name`=="Google - Global")])
Google_Global_ROA <- Google_Global_Revenue / Google_Global_Cost

Google_Cost <- sum(AF$`Total Cost`[which(AF$`Publisher Name`=="Google - Global" | AF$`Publisher Name`=="Google - US") ])
Google_Revenue <- sum(AF$Amount[which(AF$`Publisher Name`=="Google - Global" | AF$`Publisher Name`=="Google - US")])
Google_ROA <- Google_Revenue / Google_Cost

MSN_US_Cost <- sum(AF$`Total Cost`[which(AF$`Publisher Name`=="MSN - US")])
MSN_US_Revenue <- sum(AF$Amount[which(AF$`Publisher Name`=="MSN - US")])
MSN_US_ROA <- MSN_US_Revenue / MSN_US_Cost

MSN_Global_Cost <- sum(AF$`Total Cost`[which(AF$`Publisher Name`=="MSN - Global")])
MSN_Global_Revenue <- sum(AF$Amount[which(AF$`Publisher Name`=="MSN - Global")])
MSN_Global_ROA <- MSN_Global_Revenue / MSN_Global_Cost

MSN_Cost <- sum(AF$`Total Cost`[which(AF$`Publisher Name`=="MSN - Global" | AF$`Publisher Name`=="MSN - US") ])
MSN_Revenue <- sum(AF$Amount[which(AF$`Publisher Name`=="MSN - Global" | AF$`Publisher Name`=="MSN - US")])
MSN_ROA <- Google_Revenue / Google_Cost

Overture_US_Cost <- sum(AF$`Total Cost`[which(AF$`Publisher Name`=="Overture - US")])
Overture_US_Revenue <- sum(AF$Amount[which(AF$`Publisher Name`=="Overture - US")])
Overture_US_ROA <- Overture_US_Revenue / Overture_US_Cost

Overture_Global_Cost <- sum(AF$`Total Cost`[which(AF$`Publisher Name`=="Overture - Global")])
Overture_Global_Revenue <- sum(AF$Amount[which(AF$`Publisher Name`=="Overture - Global")])
Overture_Global_ROA <- Overture_Global_Revenue / Overture_Global_Cost

Overture_Cost <- sum(AF$`Total Cost`[which(AF$`Publisher Name`=="Overture - Global" | AF$`Publisher Name`=="Overture - US") ])
Overture_Revenue <- sum(AF$Amount[which(AF$`Publisher Name`=="Overture - Global" | AF$`Publisher Name`=="Overture - US")])
Overture_ROA <- Overture_Revenue / Overture_Cost

Yahoo_US_Cost <- sum(AF$`Total Cost`[which(AF$`Publisher Name`=="Yahoo - US")])
Yahoo_US_Revenue <- sum(AF$Amount[which(AF$`Publisher Name`=="Yahoo - US")])
Yahoo_US_ROA <- Yahoo_US_Revenue / Yahoo_US_Cost

Yahoo_ROA <- Yahoo_US_ROA

US_Cost <- sum(AF$`Total Cost`[which(AF$`Publisher ID` == "K2966" | AF$`Publisher ID` == "K2615" | AF$`Publisher ID` == "K435" | AF$`Publisher ID` == "1122")])
US_Revenue <- sum(AF$Amount[which(AF$`Publisher ID` == "K2966" | AF$`Publisher ID` == "K2615" | AF$`Publisher ID` == "K435" | AF$`Publisher ID` == "1122")])
US_ROA <- US_Revenue / US_Cost

Global_Cost <- sum(AF$`Total Cost`[which(AF$`Publisher ID` == "K2003" | AF$`Publisher ID` == "K1175" | AF$`Publisher ID` == "K1123")])
Global_Revenue <- sum(AF$Amount[which(AF$`Publisher ID` == "K2003" | AF$`Publisher ID` == "K1175" | AF$`Publisher ID` == "K1123")])
Global_ROA <- Global_Revenue / Global_Cost

