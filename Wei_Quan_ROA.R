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
MSN_ROA <- MSN_Revenue / MSN_Cost

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

ROA <- matrix(c(Google_Global_ROA, MSN_Global_ROA, Overture_Global_ROA, "NA", Global_ROA, Google_US_ROA, MSN_US_ROA, Overture_US_ROA, Yahoo_US_ROA, US_ROA, Google_ROA, MSN_ROA, Overture_ROA, Yahoo_ROA, ROA_Whole), ncol = 3, nrow = 5)
colnames(ROA) <- c("Global", "US", "Total")
rownames(ROA) <- c("Google", "MSN", "Overture", "Yahoo", "Total")
ROA <- as.table(ROA)


Air_France_Brand_French_Destinations_ROA <- sum(AF$Amount[which(AF$Campaign == "Air France Brand & French Destinations")]) / sum(AF$`Total Cost`[which(AF$Campaign == "Air France Brand & French Destinations")])

Air_France_Branded_ROA <- sum(AF$Amount[which(AF$Campaign == "Air France Branded")]) / sum(AF$`Total Cost`[which(AF$Campaign == "Air France Branded")])

Business_Class_ROA <- sum(AF$Amount[which(AF$Campaign == "Business Class")]) / sum(AF$`Total Cost`[which(AF$Campaign == "Business Class")])

French_Destinations_ROA <- sum(AF$Amount[which(AF$Campaign == "French Destinations")]) / sum(AF$`Total Cost`[which(AF$Campaign == "French Destinations")])

Business_Class_ROA <- sum(AF$Amount[which(AF$Campaign == "General Terms")]) / sum(AF$`Total Cost`[which(AF$Campaign == "General Terms")])

Business_Class_ROA <- sum(AF$Amount[which(AF$Campaign == "Geo Targeted Atlanta")]) / sum(AF$`Total Cost`[which(AF$Campaign == "Geo Targeted Atlanta")])

Business_Class_ROA <- sum(AF$Amount[which(AF$Campaign == "Geo Targeted Boston")]) / sum(AF$`Total Cost`[which(AF$Campaign == "Geo Targeted Boston")])

Business_Class_ROA <- sum(AF$Amount[which(AF$Campaign == "Geo Targeted Chicago")]) / sum(AF$`Total Cost`[which(AF$Campaign == "Geo Targeted Chicago")])

Business_Class_ROA <- sum(AF$Amount[which(AF$Campaign == "Geo Targeted Cincinnati")]) / sum(AF$`Total Cost`[which(AF$Campaign == "Geo Targeted Cincinnati")])

Business_Class_ROA <- sum(AF$Amount[which(AF$Campaign == "Geo Targeted DC")]) / sum(AF$`Total Cost`[which(AF$Campaign == "Geo Targeted DC")])

Business_Class_ROA <- sum(AF$Amount[which(AF$Campaign == "Geo Targeted Detroit")]) / sum(AF$`Total Cost`[which(AF$Campaign == "Geo Targeted Detroit")])

Business_Class_ROA <- sum(AF$Amount[which(AF$Campaign == "Geo Targeted Houston")]) / sum(AF$`Total Cost`[which(AF$Campaign == "Geo Targeted Houston")])

Business_Class_ROA <- sum(AF$Amount[which(AF$Campaign == "Geo Targeted Los Angeles")]) / sum(AF$`Total Cost`[which(AF$Campaign == "Geo Targeted Los Angeles")])

Business_Class_ROA <- sum(AF$Amount[which(AF$Campaign == "Geo Targeted Miami")]) / sum(AF$`Total Cost`[which(AF$Campaign == "Geo Targeted Miami")])

Business_Class_ROA <- sum(AF$Amount[which(AF$Campaign == "Geo Targeted New York")]) / sum(AF$`Total Cost`[which(AF$Campaign == "Geo Targeted New York")])

Business_Class_ROA <- sum(AF$Amount[which(AF$Campaign == "Geo Targeted Philadelphia")]) / sum(AF$`Total Cost`[which(AF$Campaign == "Geo Targeted Philadelphia")])

Business_Class_ROA <- sum(AF$Amount[which(AF$Campaign == "Geo Targeted San Francisco")]) / sum(AF$`Total Cost`[which(AF$Campaign == "Geo Targeted San Francisco")])

Business_Class_ROA <- sum(AF$Amount[which(AF$Campaign == "Geo Targeted Seattle")]) / sum(AF$`Total Cost`[which(AF$Campaign == "Geo Targeted Seattle")])

Business_Class_ROA <- sum(AF$Amount[which(AF$Campaign == "Google_Yearlong 2006")]) / sum(AF$`Total Cost`[which(AF$Campaign == "Google_Yearlong 2006")])

Business_Class_ROA <- sum(AF$Amount[which(AF$Campaign == "Outside Western Europe")]) / sum(AF$`Total Cost`[which(AF$Campaign == "Outside Western Europe")])

Business_Class_ROA <- sum(AF$Amount[which(AF$Campaign == "Paris & France Terms")]) / sum(AF$`Total Cost`[which(AF$Campaign == "Paris & France Terms")])

Business_Class_ROA <- sum(AF$Amount[which(AF$Campaign == "Western Europe Destinations")]) / sum(AF$`Total Cost`[which(AF$Campaign == "Western Europe Destinations")])

Business_Class_ROA <- sum(AF$Amount[which(AF$Campaign == "Unassigned")]) / sum(AF$`Total Cost`[which(AF$Campaign == "Unassigned")])
