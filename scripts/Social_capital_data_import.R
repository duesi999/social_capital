library(tidyverse)
library(elastic)

conn <- elastic::connect(host="130.155.204.198", errors = "complete")
ping(conn)$version$number

start <- '2019-09-16'
end <- '2019-10-15'

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


output <- Search(conn, index = "social_capital",body = twitter,size = 10000, source = "authorid,activities,categoryruletext,keyterms,keywords,sentiment,emotions,hashtags,link,publisheddate,topactivities,gender,medias,indvorg")$hits$hits
df <- data.frame()

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

######
#read in and join datasets
d1 <- read_csv("data/social_capital_data_2015-01-01_2015-31-01.csv")
d2 <- read_csv("data/social_capital_data_2015-04-01_2015-05-27.csv")
d3 <- read_csv("data/social_capital_data_2015-05-28_2015-07-24.csv")
d4 <- read_csv("data/social_capital_data_2015-07-25_2015-09-16.csv")
d5 <- read_csv("data/social_capital_data_2015-09-17_2015-11-10.csv")
d6 <- read_csv("data/social_capital_data_2015-11-11_2016-01-06.csv")
d7 <- read_csv("data/social_capital_data_2016-01-07_2016-03-08.csv")
d8 <- read_csv("data/social_capital_data_2016-03-09_2016-05-03.csv")
d9 <- read_csv("data/social_capital_data_2016-05-04_2016-06-09.csv")
d10 <- read_csv("data/social_capital_data_2016-06-10_2016-07-21.csv")
d11 <- read_csv("data/social_capital_data_2016-07-22_2016-08-29.csv")
d12 <- read_csv("data/social_capital_data_2016-08-30_2016-10-09.csv")
d13 <- read_csv("data/social_capital_data_2016-10-10_2016-11-29.csv")
d14 <- read_csv("data/social_capital_data_2016-11-30_2017-01-25.csv")
d15 <- read_csv("data/social_capital_data_2017-01-26_2017-03-10.csv")
d16 <- read_csv("data/social_capital_data_2017-03-11_2017-04-27.csv")
d17 <- read_csv("data/social_capital_data_2017-04-28_2017-06-16.csv")
d18 <- read_csv("data/social_capital_data_2017-06-17_2017-08-03.csv")
d19 <- read_csv("data/social_capital_data_2017-08-04_2017-09-15.csv")
d20 <- read_csv("data/social_capital_data_2017-09-16_2017-10-15.csv")
d21 <- read_csv("data/social_capital_data_2017-10-16_2017-11-15.csv")
d22 <- read_csv("data/social_capital_data_2017-11-16_2017-12-15.csv")
d23 <- read_csv("data/social_capital_data_2017-12-16_2018-01-15.csv")
d24 <- read_csv("data/social_capital_data_2018-01-16_2018-02-15.csv")
d25 <- read_csv("data/social_capital_data_2018-02-16_2018-03-15.csv")
d26 <- read_csv("data/social_capital_data_2018-03-16_2018-04-15.csv")
d27 <- read_csv("data/social_capital_data_2018-04-16_2018-05-15.csv")
d28 <- read_csv("data/social_capital_data_2018-05-16_2018-06-15.csv")
d29 <- read_csv("data/social_capital_data_2018-06-16_2018-07-15.csv")
d31 <- read_csv("data/social_capital_data_2018-07-16_2018-08-15.csv")
d32 <- read_csv("data/social_capital_data_2018-08-16_2018-09-15.csv")
d33 <- read_csv("data/social_capital_data_2018-09-16_2018-10-15.csv")
d34 <- read_csv("data/social_capital_data_2018-10-16_2018-11-15.csv")
d35 <- read_csv("data/social_capital_data_2018-11-16_2018-12-15.csv")
d36 <- read_csv("data/social_capital_data_2018-12-16_2019-01-15.csv")
d37 <- read_csv("data/social_capital_data_2019-01-16_2019-02-15.csv")
d38 <- read_csv("data/social_capital_data_2019-02-16_2019-03-15.csv")
d39 <- read_csv("data/social_capital_data_2019-03-16_2019-04-15.csv")
d40 <- read_csv("data/social_capital_data_2019-04-16_2019-05-15.csv")
d41 <- read_csv("data/social_capital_data_2019-05-16_2019-06-15.csv")
d42 <- read_csv("data/social_capital_data_2019-06-16_2019-07-15.csv")
d43 <- read_csv("data/social_capital_data_2019-07-16_2019-08-15.csv")
d44 <- read_csv("data/social_capital_data_2019-08-16_2019-09-15.csv")
d45 <- read_csv("data/social_capital_data_2019-09-16_2019-10-15.csv")


data <- do.call("rbind", list(d1,d2,d3,d4,d5,d6,d7,d8,d9,
                              d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,
                              d20,d21,d22,d23,d24,d25,d26,d27,d28,d29,
                              d30,d31,d32,d33,d34,d35,d36,d37,d38,d39,
                              d40,d41,d42,d43,d44,d45))
write_csv(data, paste0("data/social_capital_data_2015-2019.csv"))


