library(tidyverse)
library(elastic)

conn <- elastic::connect(host="130.155.204.198", errors = "complete")
ping(conn)$version$number

#select dates for each yearr
start <- '2014-01-01'
end <- '2014-12-31'

#set filtering parameters
eval(parse(text = paste0('twitter <- \'{ "query":{
"bool": {
  "must":[
    {"term":{"platform":"twitter"}},
    {"range": {"publisheddate": {"gte": "', start, '", "lt": "', end, '"}}}
  ],
  "must_not":[
    {"match_phrase":{"categoryruletext":"MEDIA"}},
    {"match_phrase":{"categoryruletext":"TRANSPORT"}}
  ]
}
}}\'')))

#define which fields to get
output <- Search(conn, index = "social_capital",body = twitter,size = 500000, source = "authorid,activities,categoryruletext,keyterms,keywords,sentiment,emotions,hashtags,link,publisheddate,topactivities,gender,medias,indvorg")$hits$hits

#create dataframe
df <- data.frame()
#input fields into dataframe; unlist fields that can have multiple entries
for(o in output){
  thingsIwant <- o$`_source`
  
  authorid <- thingsIwant$authorid
  if(is.null(thingsIwant$activities)){
    activities <- NA
  }else{
    activities <- unlist(thingsIwant$activities)
    activities <- paste(activities, collapse = ", ")
  }
  if(is.null(thingsIwant$categoryruletext)){
    categoryruletext <- NA
  }else{
    categoryruletext <- unlist(thingsIwant$categoryruletext)
    categoryruletext <- paste(categoryruletext, collapse = ", ")
  }
  if(is.null(thingsIwant$keyterms)){
    keyterms <- NA
  }else{
    keyterms <- unlist(thingsIwant$keyterms)
    keyterms <- paste(keyterms, collapse = ", ")
  }
  if(is.null(thingsIwant$keywords)){
    keywords <- NA
  }else{
    keywords <- unlist(thingsIwant$keywords)
    keywords <- paste(keywords, collapse = ", ")
  }
  if(is.null(thingsIwant$sentiment)){
    sentiment <- NA
  }else{
    sentiment <- unlist(thingsIwant$sentiment)
    sentiment <- paste(sentiment, collapse = ", ")
  }
  if(is.null(thingsIwant$emotions)){
    emotions <- NA
  }else{
    emotions <- unlist(thingsIwant$emotions)
    emotions <- paste(emotions, collapse = ", ")
  }
  if(is.null(thingsIwant$hashtags)){
    hashtags <- NA
  }else{
    hashtags <- unlist(thingsIwant$hashtags)
    hashtags <- paste(hashtags, collapse = ", ")
  }
  if(is.null(thingsIwant$topactivities)){
    topactivities <- NA
  }else{
    topactivities <- unlist(thingsIwant$topactivities)
    topactivities <- paste(topactivities, collapse = ", ")
  }
  publisheddate <- thingsIwant$publisheddate
  link <- thingsIwant$link
  if(is.null(thingsIwant$gender)){
    gender <- NA
  }else{
    gender <- unlist(thingsIwant$gender)
    gender <- paste(gender, collapse = ", ")
  }
  medias <- thingsIwant$medias
  if(is.null(thingsIwant$indvorg)){
    indvorg <- NA
  }else{
    indvorg <- unlist(thingsIwant$indvorg)
    indvorg <- paste(indvorg, collapse = ", ")
  }
  
  df_line <- data.frame(authorid,activities,categoryruletext,keyterms,keywords,sentiment,emotions,hashtags,link,publisheddate,topactivities,gender,medias,indvorg)
  df <- rbind(df,df_line)
}

df <- df %>%
  mutate(publisheddate = as.POSIXct(publisheddate)) %>%
  arrange(publisheddate)

write_csv(df, paste0("data/social_capital_data_",start,"_",end,".csv"))

str(df)
min(df$publisheddate)  
max(df$publisheddate)  

# ######
# #read in and join datasets
# d1 <- read_csv("data/social_capital_data_2012-01-01_2012-12-31.csv.csv")
# d2 <- read_csv("data/social_capital_data_2013-01-01_2013-12-31.csv.csv")
# d3 <- read_csv("data/social_capital_data_2014-01-01_2014-12-31.csv.csv")
# d4 <- read_csv("data/social_capital_data_2015-01-01_2015-12-31.csv.csv")
# d5 <- read_csv("data/social_capital_data_2016-01-01_2016-12-31.csv.csv")
# d6 <- read_csv("data/social_capital_data_2017-01-01_2017-12-31.csv.csv")
# d7 <- read_csv("data/social_capital_data_2018-01-01_2018-12-31.csv.csv")
# d8 <- read_csv("data/social_capital_data_2019-01-01_2019-12-31.csv.csv")
# 
# data <- do.call("rbind", list(d1,d2,d3,d4,d5,d6,d7,d8))
# write_csv(data, paste0("data/social_capital_data_2015-2019.csv"))


