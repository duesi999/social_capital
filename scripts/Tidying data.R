library(tidyverse)

# ######
# #read in and join yearly datasets
# d1 <- read_csv("data/social_capital_data_2012-01-01_2012-12-31.csv")
# d2 <- read_csv("data/social_capital_data_2013-01-01_2013-12-31.csv")
# d3 <- read_csv("data/social_capital_data_2014-01-01_2014-12-31.csv")
# d4 <- read_csv("data/social_capital_data_2015-01-01_2015-12-31.csv")
# d5 <- read_csv("data/social_capital_data_2016-01-01_2016-12-31.csv")
# d6 <- read_csv("data/social_capital_data_2017-01-01_2017-12-31.csv")
# d7 <- read_csv("data/social_capital_data_2018-01-01_2018-12-31.csv")
# d8 <- read_csv("data/social_capital_data_2019-01-01_2019-12-31.csv")
# 
# df <- do.call("rbind", list(d1,d2,d3,d4,d5,d6,d7,d8))
# write_csv(df, paste0("data/social_capital_data_2015-2019.csv"))

df <- read_csv("data/social_capital_data_2015-2019.csv")

#######
#data cleaning
#remove keywords column - this must ahve been included mistakenly (typo of keyterm); will take this variable out of the data import eventually
#remove medias column - all are values are the same: microblog
#filter for Sydney data
df_sydney <- df %>%
  select(-keywords) %>%
  filter(topactivities == "SYDNEY") %>%
  mutate(
    authorid = factor(authorid),
    activities = factor(activities),
    categoryruletext = factor(categoryruletext),
    sentiment = factor(sentiment),
    emotions = factor(emotions),
    topactivities = factor(topactivities),
    gender = factor(gender),
    indvorg = factor(indvorg)
  ) %>%
  #get rid of multiple entries in categoryruletext field
  separate(categoryruletext, c("council", "council2", "council3"), sep = ", ") %>%
  select(-council2, -council3) %>%
  #add district colum - https://www.planning.nsw.gov.au/Plans-for-your-area/A-Metropolis-of-Three-Cities/Greater-Sydney-Districts/Five-districts
  mutate(district = case_when(council == "BLUE MOUNTAINS"  ~ "Western",
                              council == "HAWKESBURY"   ~ "Western",
                              council == "PENRITH"   ~ "Western",
                              council == "CAMDEN"   ~ "Western",
                              council == "CAMPBELLTOWN"   ~ "Western",
                              council == "FAIRFIELD"   ~ "Western",
                              council == "LIVERPOOL"   ~ "Western",
                              council == "WOLLONDILLY"   ~ "Western",
                              council == "BLACKTOWN"   ~ "Central",
                              council == "CUMBERLAND"   ~ "Central",
                              council == "PARRAMATTA"  ~ "Central",
                              council == "HILLS"   ~ "Central",
                              council == "BAYSIDE"   ~ "Eastern",
                              council == "BURWOOD"   ~ "Eastern",
                              council == "CANADA BAY"  ~ "Eastern",
                              council == "INNER WEST"  ~ "Eastern",
                              council == "RANDWICK" ~ "Eastern",
                              council == "STRATHFIELD"  ~ "Eastern",
                              council == "WOOLLAHRA" ~ "Eastern",
                              council == "WAVERLEY"  ~ "Eastern",
                              council ==  "CITY"  ~ "Eastern",
                              council ==  "HORNSBY"  ~ "North",
                              council ==  "HUNTER'S HILL"  ~ "North",
                              council ==   "LANE COVE"  ~ "North",
                              council ==   "NORTHERN BEACHES" ~ "North",
                              council ==   "KU-RING-GAI" ~ "North",
                              council ==   "MOSMAN" ~ "North",
                              council ==   "WILLOUGHBY" ~ "North",
                              council ==   "RYDE" ~ "North",
                              council ==   "WILLOUGHBY" ~ "North",
                              council ==   "NORTH" ~ "North",
                              council ==   "GEORGES RIVER" ~ "South",
                              council ==   "CANTERBURY-BANKSTOWN" ~ "South",
                              council ==   "SUTHERLAND" ~ "South"))






