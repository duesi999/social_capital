######
library(tidyverse)
library(forcats)
library(ggrepel)
library(cowplot)
##in case it's loaded, detach the elastic package because it masks sone dplyr functions
#detach(package:elastic, unload = TRUE)

data <- read_csv("data/social_capital_data_2015-2019.csv")

glimpse(data)


# sentiment proportion in each council
council_sentiment <- df_sydney %>%
  group_by(council, sentiment) %>%
  summarise(count=n()) %>%
  drop_na() %>%
  #calculate percentage of counts
  mutate(freq = count / sum(count)*100)

# #create pie chart
# filename <-"Sentiment per coucil"
# ggplot(council_sentiment, aes(x="", y=freq, fill=sentiment)) + 
#   geom_bar(stat="identity", width=1) +
#   coord_polar("y", start=0) + 
#   geom_text(aes(label = paste0(round(freq), "%")), position = position_stack(vjust = 0.5), size = 1.5) +
#   labs(x = NULL, y = NULL, fill = NULL, title = filename) +
#   theme(axis.line = element_blank(),
#                                     axis.text = element_blank(),
#                                     axis.ticks = element_blank(),
#                                     plot.title = element_text(hjust = 0.5, color = "#666666"),
#                                     text = element_text(size=5.5)) +
#   facet_wrap(~council)
# ggsave(filename = paste0("results/", filename, ".pdf"), height = 12, width = 12, unit = "cm")

#stacked bar chart
filename <-"Sentiment per coucil"
ggplot(council_sentiment, aes(x= reorder(council, desc(council)), y=freq, fill = sentiment)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label = paste0(round(freq), "%")), position = position_stack(vjust = 0.5), size = 2) +
  labs(x = "councils", y = "percent", title = filename) +
  theme(plot.title = element_text(hjust = 0.5, color = "#666666"),
        text = element_text(size=7)) +
  coord_flip()
ggsave(filename = paste0("results/", filename,"_bar_chart",".pdf"), height = 12, width = 10, unit = "cm")

# sentiment proportion in each district
district_sentiment <- df_sydney %>%
  group_by(district, sentiment) %>%
  summarise(count=n()) %>%
  drop_na() %>%
  #calculate percentage of counts
  mutate(freq = count / sum(count)*100)

# #create pie chart
# filename <-"Sentiment per district"
# ggplot(district_sentiment, aes(x="", y=freq, fill=sentiment)) + 
#   geom_bar(stat="identity", width=1) +
#   coord_polar("y", start=0) + 
#   geom_text(aes(label = paste0(round(freq), "%")), position = position_stack(vjust = 0.5), size = 3) +
#   labs(x = NULL, y = NULL, fill = NULL, title = filename) +
#   theme(axis.line = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         plot.title = element_text(hjust = 0.5, color = "#666666"),
#         text = element_text(size=5.5)) +
#   facet_wrap(~district)
# ggsave(filename = paste0("results/", filename, ".pdf"), height = 12, width = 12, unit = "cm")

#stacked bar chart
filename <-"Sentiment per district"
ggplot(district_sentiment, aes(x= reorder(district, desc(district)), y=freq, fill = sentiment)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label = paste0(round(freq), "%")), position = position_stack(vjust = 0.5), size = 2) +
  labs(x = "districts", y = "percent", title = filename) +
  theme(plot.title = element_text(hjust = 0.5, color = "#666666"),
        text = element_text(size=9)) +
  coord_flip()
ggsave(filename = paste0("results/", filename,"_bar_chart",".pdf"), height = 4, width = 12, unit = "cm")



# emotion proportion in each council
council_emotions <- df_sydney %>%
  group_by(council, emotions) %>%
  summarise(count=n()) %>%
  drop_na() %>%
  #calculate percentage of counts
  mutate(freq = count / sum(count)*100)

# #create pie chart
# filename <-"Emotions per coucil"
# ggplot(council_emotions, aes(x="", y=freq, fill=emotions)) + 
#   geom_col(position='stack', width=1) +
#   coord_polar("y", start=0) + 
#   geom_text(aes(label = paste0(round(freq), "%"), x = 1.6), position = position_stack(vjust = 0.5), size =1.6) +
#   labs(x = NULL, y = NULL, fill = NULL, title = filename) +
#   theme(axis.line = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         plot.title = element_text(hjust = 0.5, color = "#666666"),
#         text = element_text(size=7)) +
#   facet_wrap(~council)
# ggsave(filename = paste0("results/", filename, ".pdf"), height = 12, width = 12, unit = "cm")

#stacked bar chart
filename <-"Emotions per coucil"
ggplot(council_emotions, aes(x= reorder(council, desc(council)), y=freq, fill = emotions)) + 
  geom_bar(stat="identity") +
 # geom_text_repel(aes(label = paste0(round(freq), "%")), position = position_stack(vjust = 0.5), size = 1.5, box.padding = 0.01, direction = "x") +
  labs(x = "councils", y = "percent", title = filename) +
  theme(plot.title = element_text(hjust = 0.5, color = "#666666"),
        text = element_text(size=7),
        axis.text = element_text(size = 5)) +
  scale_y_continuous(limits = (c(0,100))) +
  coord_flip() +
  facet_wrap(~emotions, ncol = 7)
ggsave(filename = paste0("results/", filename,"_bar_chart",".pdf"), height = 15, width = 15, unit = "cm")


# emotions proportion in each district
districts_emotions <- df_sydney %>%
  group_by(district, emotions) %>%
  summarise(count=n()) %>%
  drop_na() %>%
  #calculate percentage of counts
  mutate(freq = count / sum(count)*100)

# #create pie chart
# filename <-"Emotions per district"
# ggplot(districts_emotions, aes(x="", y=freq, fill=emotions)) + 
#   geom_bar(stat="identity", width=1) +
#   coord_polar("y", start=0) + 
#   geom_text(aes(label = paste0(round(freq), "%"), x = 1.6), position = position_stack(vjust = 0.5), size = 3) +
#   labs(x = NULL, y = NULL, fill = NULL, title = filename) +
#   theme(axis.line = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         plot.title = element_text(hjust = 0.5, color = "#666666"),
#         text = element_text(size=5.5)) +
#   facet_wrap(~district)
# ggsave(filename = paste0("results/", filename, ".pdf"), height = 12, width = 12, unit = "cm")

#stacked bar chart
filename <-"Emotions per district"
ggplot(district_emotions, aes(x= reorder(district, desc(district)), y=freq, fill = emotions)) + 
  geom_bar(stat="identity") +
#  geom_text(aes(label = paste0(round(freq), "%")), position = position_stack(vjust = 0.5), size = 3) +
  labs(x = "districts", y = "percent", title = filename) +
  theme(plot.title = element_text(hjust = 0.5, color = "#666666"),
        text = element_text(size=7),
        axis.text = element_text(size = 5)) +
  scale_y_continuous(limits = (c(0,100))) +
  coord_flip() +
  facet_wrap(~emotions, ncol = 7)
ggsave(filename = paste0("results/", filename,"_bar_chart",".pdf"), height = 5, width = 15, unit = "cm")


#number of posts per council
#join with population df
no_posts_council <- df_sydney %>%
#  filter(indvorg == "individual") %>%
  group_by(council) %>%
  summarise(post_count = n()) %>%
  left_join(pop_sydney, by = "council") %>%
  mutate(posts_per_pop = post_count/population*100) %>%
  arrange(desc(posts_per_pop))

#unique authors in councils
authors_unique_council <- df_sydney %>%
#  filter(indvorg == "individual") %>%
  group_by(authorid, council) %>%
  summarise(count = n()) %>%
  arrange(council, desc(count))

#number of unique voices
authors_percent_unique_council <- authors_unique_council %>%
  group_by(council) %>%
  summarise(unique_authors = n()) %>%
  left_join(no_posts_council, by = "council") %>%
  mutate(percent_unique_authors = round(unique_authors/post_count*100)) %>%
#  arrange(desc(post_count))%>%
  drop_na()

#plot
options(scipen = 999)
filename <- "Council population (green), number of posts (blue) and percent unique authors (red) per council"
ggplot(authors_percent_unique_council, aes(x= reorder(council, post_count))) +
  geom_bar(aes(y = population), stat = "identity", fill = "green", alpha = 0.5) +
  geom_bar(aes(y = post_count), stat = "identity", fill = "#b3cde3") +
  geom_bar(aes(y = unique_authors), stat = "identity", fill = "red", alpha = 0.7) +
  geom_text(aes(y = unique_authors, label = paste0(percent_unique_authors, "%")), position = position_nudge(y = -5000), colour = "black", size = 3) +
  geom_text(aes(y = post_count, label = post_count), position = position_nudge(y = 10500), colour = "black", size = 3) +
  geom_text(aes(y = population, label = population), position = position_nudge(y = 15000), colour = "black", size = 3) +
  coord_flip() +
  labs(x = "councils", y = "counts", title = filename) +
  theme(plot.title = element_text(hjust = 0.5, color = "black"),
        axis.text = element_text(size = 11))
ggsave(filename = paste0("results/", filename, ".pdf"))


#number of posts per district
#join with population df
no_posts_district <- df_sydney %>%
  #  filter(indvorg == "individual") %>%
  group_by(district, council) %>%
  summarise(post_count = n()) %>%
  left_join(pop_sydney, by = "council") %>%
  mutate(posts_per_pop = post_count/population*100) %>%
  group_by(district) %>%
  summarise(post_count = sum(post_count),
            posts_per_pop = sum(posts_per_pop),
            population = sum(population)) %>%
  arrange(desc(posts_per_pop))

#unique authors in district
authors_unique_district <- df_sydney %>%
  #  filter(indvorg == "individual") %>%
  group_by(authorid, district) %>%
  summarise(count = n()) %>%
  arrange(district, desc(count))

#number of unique voices
authors_percent_unique_district <- authors_unique_district %>%
  group_by(district) %>%
  summarise(unique_authors = n()) %>%
  left_join(no_posts_district, by = "district") %>%
  mutate(percent_unique_authors = round(unique_authors/post_count*100)) %>%
  #  arrange(desc(post_count))%>%
  drop_na()

filename <- "District pop. (green), no of posts (blue) and % unique authors (red)"
ggplot(authors_percent_unique_district, aes(x= reorder(district, post_count))) +
  geom_bar(aes(y = population), stat = "identity", fill = "green", alpha = 0.5) +
  geom_bar(aes(y = post_count), stat = "identity", fill = "#b3cde3") +
  geom_bar(aes(y = unique_authors), stat = "identity", fill = "red", alpha = 0.7) +
  geom_text(aes(y = unique_authors, label = paste0(percent_unique_authors, "%")), position = position_nudge(y = -10000), colour = "black", size = 2) +
  geom_text(aes(y = post_count, label = post_count), position = position_nudge(y = 17000), colour = "black", size = 2) +
  geom_text(aes(y = population, label = population), colour = "black", size = 2) +
  coord_flip() +
  labs(x = "districts", y = "counts", title = filename) +
  theme(plot.title = element_text(hjust = 0.5, color = "black", size = 10),
        axis.text = element_text(size = 11))
ggsave(filename = paste0("results/", filename, ".pdf"),  height = 5, width = 15, unit = "cm")



########
#gender
df_gender <- df_sydney %>%
  filter(gender != "unknown") %>%
  group_by(council, district, gender) %>%
  summarise(count=n()) %>%
  mutate(count_perc = round(count/sum(count)*100)) %>%
  drop_na() 

##plot gender by district
# create graphing function
out_list <- list()
district.graph <- function(df, na.rm = TRUE, ...){
  
  # create list of councils in data to loop over 
  district_list <- unique(df$district)
 
  # create for loop to produce plotly graphs 
  for (i in seq_along(district_list)) { 
    
plot <- ggplot(filter(df, district == district_list[i]), aes(x= reorder(council, desc(council)), y=count_perc, fill = gender)) + 
 geom_bar(stat="identity") +
#  geom_text(aes(label = paste0(round(freq), "%")), position = position_stack(vjust = 0.5), size = 2) +
  labs(x = "councils", y = "count", title = district_list[i]) +
  theme(plot.title = element_text(hjust = 0.5, color = "#666666"),
        text = element_text(size=7)) +
  coord_flip()
    
#  print(plot)
#ggsave(filename = paste0("results/", district_list[i],"_bar_chart",".pdf"), height = 12, width = 10, unit = "cm")
out_list[[i]] <<- plot
# paste0("plot_",district_list[i]) <- plot
#assign(paste0("chart",district_list[i], sep = ''), plot)

  }
}
district.graph(df_gender)

plot_grid(out_list[[1]], out_list[[2]], out_list[[3]], out_list[[4]])
ggsave("results/Gender by district and council.pdf",  height = 15, width = 15, unit = "cm")
