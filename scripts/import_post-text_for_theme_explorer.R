library(tidyverse)
library(elastic)

#stablish connection with elastic search
conn <- elastic::connect(host="130.155.204.198", errors = "complete")
#ping(conn)$version$number

#select dates for each year
start <- '2013-10-05'
end <- '2013-11-09'

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
#define which fields to get
output <- Search(conn, index = "social_capital",body = twitter,size = 500000, source = "authorid,activities,categoryruletext,keyterms,sentiment,emotions,hashtags,link,publisheddate,topactivities,gender,indvorg,text")$hits$hits

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
  if(is.null(thingsIwant$text)){
    text <- NA
  }else{
    text <- unlist(thingsIwant$text)
    text <- paste(text, collapse = ", ")
}
    
  df_line <- data.frame(authorid,activities,categoryruletext,keyterms,sentiment,emotions,hashtags,link,publisheddate,topactivities,gender,indvorg,text)
  df <- rbind(df,df_line)
}


#convert publisheddate to date format
df <- df %>%
  mutate(publisheddate = as.POSIXct(publisheddate)) %>%
  arrange(publisheddate)

#write to csv
write_csv(df, paste0("data/social_capital_data_",start,"_",end,".csv"))

str(df)
min(df$publisheddate)  
max(df$publisheddate)  


# ###################
# #manipulated datasets for the three different case studies - please ignore
# #######
# df_fires <-read_csv("data/bush_fires_social_capital_data_2013-10-05_2013-11-09.csv")
# 
# 
# df_fires_1 <- df_fires %>%
#   #filter post frpm Sydney and only from councils
#   filter(topactivities == "SYDNEY",
#          grepl("COUNCIL", activities)) %>%
#   separate_rows(categoryruletext,sep=",\\s+") %>%
#   rename(council = categoryruletext) %>%
#   #select councils on the coast
#   filter(council == "BLUE MOUNTAINS" | council == "CAMPBELLTOWN" | council == "CAMDEN" | council == "PENRITH"
#          | council == "HAWKESBURY" | council == "WOLLONDILLY" | council == "LIVERPOOL")
# head(df_fires_1)
# write_csv(df_fires_1, "data/filtered_bush_fires_social_capital_data_2013-10-05_2013-11-09.csv")
# 
# 
# df_storm <- read_csv("data/storm_social_capital_data_2016-06-03_2016-06-09.csv")
# 
# df_storm_1 <- df_storm %>%
#   #filter post frpm Sydney and only from councils
#   filter(topactivities == "SYDNEY",
#          grepl("COUNCIL", activities)) %>%
#   separate_rows(categoryruletext,sep=",\\s+") %>%
#   rename(council = categoryruletext) %>%
#   #select councils on the coast
#   filter(council == "NORTHERN BEACHES" | council == "MOSMAN" | council == "WOOLLAHRA" | council == "RANDWICK" | council == "SUTHERLAND")
# head(df_storm_1)
# write_csv(df_storm_1, "data/filtered_storm_social_capital_data_2013-10-05_2013-11-09.csv")
# 
# 
# 
# ##########
# #select dates during and after fire for bushfire data set
# 
# df_fires_2 <- df_fires %>%
#   #filter post frpm Sydney and only from councils
#   filter(topactivities == "SYDNEY",
#          grepl("COUNCIL", activities),
#          publisheddate >= "2013-10-17 00:00:00") %>%
#   separate_rows(categoryruletext,sep=",\\s+") %>%
#   rename(council = categoryruletext) %>%
#   #select councils on the coast
#   filter(council == "BLUE MOUNTAINS" | council == "CAMPBELLTOWN" | council == "CAMDEN" | council == "PENRITH"
#          | council == "HAWKESBURY" | council == "WOLLONDILLY" | council == "LIVERPOOL")
# head(df_fires_2)
# write_csv(df_fires_2, "data/filtered_bush_fires_social_capital_data_2013-10-17_2013-11-09.csv")
# 
# 
