library(tidyverse)
library(elastic)

#stablish connection with elastic search
conn <- elastic::connect(host="130.155.204.198", errors = "complete")
#ping(conn)$version$number

#select dates for each year
start <- '2019-01-01'
end <- '2019-12-31'

#set filtering parameters; filter for start and end dates, get twitter feeds, and exclude Media and Transport search categories
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
output <- Search(conn, index = "social_capital",body = twitter,size = 500000, source = "authorid,activities,categoryruletext,keyterms,sentiment,emotions,hashtags,link,publisheddate,topactivities,gender,indvorg")$hits$hits

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
  if(is.null(thingsIwant$indvorg)){
    indvorg <- NA
  }else{
    indvorg <- unlist(thingsIwant$indvorg)
    indvorg <- paste(indvorg, collapse = ", ")
  }
  
  df_line <- data.frame(authorid,activities,categoryruletext,keyterms,sentiment,emotions,hashtags,link,publisheddate,topactivities,gender,indvorg)
  df <- rbind(df,df_line)
}

#convert publisheddate to date format
df <- df %>%
  mutate(publisheddate = as.POSIXct(publisheddate)) %>%
  arrange(publisheddate)

#write to csv
write_csv(df, paste0("data/social_capital_data_",start,"_",end,".csv"))


########
#add author field
#set filtering parameters; filter for start and end dates, get twitter feeds, and exclude Media and Transport search categories
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
output_authors <- Search(conn, index = "social_capital",body = twitter,size = 500000, source = "authorid,author")$hits$hits


df_authors <- setDF(data.table::rbindlist(
  lapply(output_authors, "[[", "_source"),
  fill = TRUE, use.names = TRUE
))
df_authors$authorid <- as.factor(df_authors$authorid)
df_authors$author <- as.factor(df_authors$author)
df_authors <- distinct(df_authors)

#write to csv
write_csv(df_authors, paste0("data/social_capital_data_authors_",start,"_",end,".csv"))

