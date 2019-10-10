######
library(tidyverse)
library(forcats)
detach(package:elastic, unload = TRUE)

data <- read_csv("data/social_capital_data_2015-2019.csv")

glimpse(data)


# sentiment proportion in each council
council_sentiment <- data_sydney %>%
  group_by(council, sentiment) %>%
  summarise(count=n()) %>%
  drop_na() %>%
  #calculate percentage of counts
  mutate(freq = count / sum(count)*100)

#create pie chart
filename <-"Sentiment per coucil"
ggplot(council_sentiment, aes(x="", y=freq, fill=sentiment)) + 
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(freq), "%")), position = position_stack(vjust = 0.5), size = 2) +
  labs(x = NULL, y = NULL, fill = NULL, title = filename) +
  theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"),
                                    text = element_text(size=5.5)) +
  facet_wrap(~council)
ggsave(filename = paste0("results/", filename, ".pdf"), height = 12, width = 12, unit = "cm")

# sentiment proportion in each district
council_sentiment <- data_sydney %>%
  group_by(district, sentiment) %>%
  summarise(count=n()) %>%
  drop_na() %>%
  #calculate percentage of counts
  mutate(freq = count / sum(count)*100)

#create pie chart
filename <-"Sentiment per district"
ggplot(council_sentiment, aes(x="", y=freq, fill=sentiment)) + 
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(freq), "%")), position = position_stack(vjust = 0.5), size = 2) +
  labs(x = NULL, y = NULL, fill = NULL, title = filename) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"),
        text = element_text(size=5.5)) +
  facet_wrap(~district)
ggsave(filename = paste0("results/", filename, ".pdf"), height = 12, width = 12, unit = "cm")



# emotion proportion in each council
council_emotions <- data_sydney %>%
  group_by(council, emotions) %>%
  summarise(count=n()) %>%
  drop_na() %>%
  #calculate percentage of counts
  mutate(freq = count / sum(count)*100)

#create pie chart
filename <-"Emotions per coucil"
ggplot(council_emotions, aes(x="", y=freq, fill=emotions)) + 
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(freq), "%")), position = position_stack(vjust = 0.5), size = 2) +
  labs(x = NULL, y = NULL, fill = NULL, title = filename) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"),
        text = element_text(size=5.5)) +
  facet_wrap(~council)
ggsave(filename = paste0("results/", filename, ".pdf"), height = 12, width = 12, unit = "cm")

# sentiment proportion in each district
council_sentiment <- data_sydney %>%
  group_by(district, sentiment) %>%
  summarise(count=n()) %>%
  drop_na() %>%
  #calculate percentage of counts
  mutate(freq = count / sum(count)*100)

#create pie chart
filename <-"Sentiment per district"
ggplot(council_sentiment, aes(x="", y=freq, fill=sentiment)) + 
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(freq), "%")), position = position_stack(vjust = 0.5), size = 2) +
  labs(x = NULL, y = NULL, fill = NULL, title = filename) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"),
        text = element_text(size=5.5)) +
  facet_wrap(~district)
ggsave(filename = paste0("results/", filename, ".pdf"), height = 12, width = 12, unit = "cm")


#uniuqe authors in councils
authors_1 <- data_sydney %>%
  filter(indvorg == "individual") %>%
  group_by(authorid, council) %>%
  summarise(count = n()) %>%
  arrange(council, desc(count))

#number of unique voices
authors_2 <- authors_1 %>%
  group_by(council) %>%
  summarise(unique_authors = n())
