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
kayak <- c(NA,	2839,	3567.13,208,1123.53,233694,230126.87)
word_strategy <- data.frame(rbind(kayak),row.names = NULL)
word_strategy[,1] <- "Kayak"
colnames(word_strategy) <- cols

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
grepl(x=AF$Keyword,pattern = "paris")
grepl(x=AF$Keyword,pattern = "paris") & grepl(x=AF$Keyword,pattern = "france")
grepl(x=AF$Keyword,pattern = "paris")

words_paris <- select(AF[grepl(x=AF$Keyword,pattern = "paris") | 
                           grepl(x=AF$Keyword,pattern = "france"),],
                      c(Publisher.Name,
                        Keyword,
                        Amount,
                        Total.Cost,
                        Total.Volume.of.Bookings))


# keyword strategy - broad vs specific?
# We need packages:
library(tidyr)
library(dplyr)

keyword_strategy <- # Get a summary table 
  AF %>% #get the AF dataframe...
  group_by(Match.Type) %>% #Group by Publisher name
  summarise(Sum.Bookings = sum(Total.Volume.of.Bookings), # Create columns for sum of volume
            Amount.Bookings = sum(Amount), # Create columns for sum of amount
            Count.Bookings = n()) # and another column for count of bookings



zero_bookings <- AF[AF$Total.Volume.of.Bookings == 0,]

###########################################################
# WORD CLOUD

# Install
install.packages('devtools')
library(devtools)
slam_url <- "https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-14.tar.gz"
#slam_url <- "https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz"
install_url(slam_url)

install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

#text <- str_split(AF$Keyword, " ")
#docs <- Corpus(VectorSource(text))

#docs <- Corpus(VectorSource(AF$Keyword))
join_airfrance <- gsub(x=AF$Keyword,pattern = "air france",replacement = "airfrance")
#join_airfrance <- gsub(x=AF$Keyword,pattern = "[",replacement = " ",fixed=T)
#join_airfrance <- gsub(x=AF$Keyword,pattern = "]",replacement = " ",fixed=T)

docs <- Corpus(VectorSource(join_airfrance))
inspect(docs)

# Text transformation:
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")


# text cleaning:

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
#docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

# final cleaning on d

head(d, 20) # top 20 words - ~68% of all keywords

# wordcloud! 

set.seed(1234)
wordcloud(words = d$word,scale=c(6,.5), freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# plotting top 20 keywords:
ggplot(data=d[1:20,], aes(reorder(word,-freq), freq))+
geom_col(fill="orange")+
  labs(title = "Air France Digital Marketing: Top 20 keywords")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 12))
  

#WORDCLOUD on Zero bookings

zero_join_af <- gsub(x=zero_bookings$Keyword,pattern = "air france",replacement = "airfrance")
docs <- Corpus(VectorSource(zero_join_af))
inspect(docs)

# Text transformation:
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")


# text cleaning:

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
#docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d_zero <- data.frame(word = names(v),freq=v)

# final cleaning on d

head(d_zero, 20) # top 20 words - ~68% of all keywords

# wordcloud! 

set.seed(1234)
wordcloud(words = d_zero$word, freq = d_zero$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# plotting top 20 keywords without bookings:
ggplot(data=d_zero[1:20,], aes(reorder(word,-freq), freq))+
  geom_col(fill="salmon")+
  labs(title = "Air France: Top 20 keywords without bookings")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 12))

#WORDCLOUD on has_bookings

book_join_af <- gsub(x=has_bookings$Keyword,pattern = "air france",replacement = "airfrance")
docs <- Corpus(VectorSource(book_join_af))
inspect(docs)

# Text transformation:
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")


# text cleaning:

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
#docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d_has <- data.frame(word = names(v),freq=v)

# final cleaning on d

head(d_zero, 20) # top 20 words - ~68% of all keywords

# wordcloud! 

set.seed(1234)
wordcloud(words = d_has$word,scale=c(6,.5), freq = d_has$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# plotting top 20 keywords with bookings:
ggplot(data=d_has[1:20,], aes(reorder(word,-freq), freq))+
  geom_col(fill="magenta")+
  labs(title = "Air France: Top 20 keywords with bookings")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 12))

####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################

# Business Analysis of Keywords:

# Aggregated data to compare with kayak:
library(dplyr)
library(tidyr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# KAYAK INFO: 
cols <- c("Keyword"	,"Clicks",	"Media Cost",	"Total Bookings"	,
          "Avg Ticket"	,"Total Revenue",	"Net Revenue")
kayak <- c(NA,	2839,	3567.13,208,1123.53,233694,230126.87)
word_strategy <- data.frame(rbind(kayak),row.names = NULL)
word_strategy[,1] <- "Kayak (BENCHMARK)"
colnames(word_strategy) <- cols

join_airfrance <- gsub(x=AF$Keyword,pattern = "air france",replacement = "airfrance")
docs <- Corpus(VectorSource(join_airfrance))
#inspect(docs)

# Text transformation:
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

#docs <- tm_map(docs, toSpace, "/")
#docs <- tm_map(docs, toSpace, "@")
#docs <- tm_map(docs, toSpace, "\\|")


# text cleaning:

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
#docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word

# Remove noisy keywords:
docs <- tm_map(docs, removeWords, c("franc", "par","tou")) 
# Remove punctuations
#docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
#docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)



# get a vector with kws which appear more than 100 times:
#top_keywords <- as.character(d[d$freq>100,'word'])

# or get a vector with all kws:
top_keywords <- as.character(d[,'word'])

# Create a loop to:
for (i in 1:length(top_keywords)){
  # print(top_keywords[i]) # for debugging
  
  #1. get a dataframe with each word
  AF_kw <- AF[grep(x=AF$Keyword,pattern = top_keywords[i],fixed = T),]
  
  #2. summarize the values to match "word_strategy":
  word_strategy[i+1,1] <- top_keywords[i]
  
  # number of clicks sum(Clicks)
  word_strategy[i+1,2] <- sum(AF_kw$Clicks)
  
  # media cost sum(Total.Cost)
  word_strategy[i+1,3] <- sum(AF_kw$Total.Cost)
  
  # total sum of bookings sum(Total.Volume.of.Bookings)
  word_strategy[i+1,4] <- sum(AF_kw$Total.Volume.of.Bookings)
  
  # total revenue sum(Amount)
  word_strategy[i+1,6] <- sum(AF_kw$Amount)
  
  # average ticket revenue -> calculated field afterwards
  word_strategy[i+1,5] <- word_strategy[i+1,6]/word_strategy[i+1,4]
  
  # net revenue -> calculated field afterwards 
  word_strategy[i+1,7] <- word_strategy[i+1,6]-word_strategy[i+1,3]
  
  #print(word_strategy) #for debugging
  # add the results to word_strategy as a new row
  
}

# new column - Marketing ROA
word_strategy$ROA <- round(word_strategy$`Total Revenue`/word_strategy$`Media Cost`,2)

# new column -  Cost in Pct of total marketing budget
word_strategy$`Cost Pct Mkt budget` <- round(word_strategy$`Media Cost`/(sum(AF$Total.Cost)+word_strategy[1,3]),2)

# new column -  Pct total revenue per keyword
word_strategy$`Pct total revenue` <- round(word_strategy$`Total Revenue`/(sum(AF$Amount)+word_strategy[1,6]),2)


View(word_strategy)

# Looking at the campaigns who generated > $ 10,000 in revenue AND had an ROA > 1:
word_focus <- word_strategy[word_strategy$`Net Revenue`>10000 & word_strategy$ROA>1,]
View(word_focus)

# Looking at the worst campaigns, with ROA < or = to 1:
word_junk <- word_strategy[word_strategy$ROA<=1,]
View(word_junk)

# RECOMMENDATION - LIST OF 302 KEYWORDS TO AVOID!!!!
