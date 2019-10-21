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
df_authorid <- read_csv("data/social_capital_data_authorid_2015-2019.csv") %>%
  mutate(authorid = factor(authorid),
         author = factor(author))

#######
#data cleaning
#remove keywords column - this must ahve been included mistakenly (typo of keyterm); will take this variable out of the data import eventually
#remove medias column - all are values are the same: microblog
#filter for Sydney data
df_sydney <- df %>%
  select(-keywords, -medias) %>%
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
  ###get rid of multiple entries in categoryruletext field - this only keeps the first council mentioned and discards the rest if more are mentioned in a post
  ## separate(categoryruletext, c("council", "council2", "council3"), sep = ", ") %>%
  ## select(-council2, -council3) %>%
  #instead separate into new rows if more than one council is mentioned - this "increases the number of observations"
  separate_rows(categoryruletext,sep=",\\s+") %>%
  rename(council = categoryruletext) %>%
  left_join(df_authors, by = "authorid")

  

###
#tidy council names - convert all to lower case fist, then make first letter capital
df_sydney$council <- tolower(df_sydney$council)
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
df_sydney$council <- sapply(df_sydney$council, simpleCap)

#rename those council names that don't match ABS names
df_sydney <- df_sydney %>%
  mutate(council = case_when(council == "Canterbury-bankstown" ~"Canterbury-Bankstown",
                             council == "City" ~ "Sydney",
                             council == "Sutherland" ~ "Sutherland Shire",
                             council == "Hills" ~ "The Hills Shire",
                             council == "North" ~ "North Sydney",
                             council == "Hunter's Hill" ~ "Hunters Hill",
                             council == "Waverly" ~ "Waverley",
                             #include a final catch-all test and replacement to get rest of data
                             TRUE ~ council)) %>%
  filter(council != "NANA")
  
#add district colum - https://www.planning.nsw.gov.au/Plans-for-your-area/A-Metropolis-of-Three-Cities/Greater-Sydney-Districts/Five-districts
df_sydney <- df_sydney %>%
  mutate(district = case_when(council == "Blue Mountains"  ~ "Western",
                              council == "Hawkesbury"   ~ "Western",
                              council == "Penrith"   ~ "Western",
                              council == "Camden"   ~ "Western",
                              council == "Campbelltown"   ~ "Western",
                              council == "Fairfield"   ~ "Western",
                              council == "Liverpool"   ~ "Western",
                              council == "Wollondilly"   ~ "Western",
                              council == "Blacktown"   ~ "Central",
                              council == "Cumberland"   ~ "Central",
                              council == "Parramatta"  ~ "Central",
                              council == "The Hills Shire" ~ "Central",
                              council == "Bayside"   ~ "Eastern",
                              council == "Burwood"   ~ "Eastern",
                              council == "Canada Bay"  ~ "Eastern",
                              council == "Inner West"  ~ "Eastern",
                              council == "Randwick" ~ "Eastern",
                              council == "Strathfield"  ~ "Eastern",
                              council == "Woollahra" ~ "Eastern",
                              council == "Waverley"  ~ "Eastern",
                              council ==  "Sydney"  ~ "Eastern",
                              council ==  "Hornsby"  ~ "North",
                              council ==  "Hunters Hill"  ~ "North",
                              council ==   "Lane Cove"  ~ "North",
                              council ==   "Northern Beaches" ~ "North",
                              council ==   "Ku-ring-gai" ~ "North",
                              council ==   "Mosman" ~ "North",
                              council ==   "Willoughby" ~ "North",
                              council ==   "Ryde" ~ "North",
                              council ==   "North Sydney" ~ "North",
                              council ==   "Georges River" ~ "South",
                              council ==   "Canterbury-Bankstown" ~ "South",
                              council ==   "Sutherland Shire" ~ "South"))


####
#read in Sydney population statistics
pop_sydney <- read_csv("data/ABS/population_sydney_councils.csv")

#remove bracketed string content
pop_sydney$council <- gsub(".\\(.*?\\)", "", pop_sydney$LGA)




