knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)  
library(ggplot2)  
library(readr)
library(skimr)  
library(caret)      
library(lubridate)  
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(gridExtra)
FIFA19 <- read_csv("Desktop/ExploringR/FIFA19.csv")
head(FIFA19)
skim(FIFA19)
summary(FIFA19)
colSums(is.na(FIFA19))
clean_wage <- function(wage) {
  as.numeric(gsub("€|K", "", wage)) * 1000
}

clean_value <- function(value) {
  value <- gsub("€", "", value) 
  if (grepl("M", value)) {
    as.numeric(gsub("M", "", value)) * 1e6 
  } else if (grepl("K", value)) {
    as.numeric(gsub("K", "", value)) * 1e3 
  } else {
    as.numeric(value) 
  }
}

FIFA19 <- FIFA19 %>%
  mutate(
    Wage = sapply(Wage, clean_wage),
    Value = sapply(Value, clean_value)
  )
nationality_counts <- FIFA19 %>%
  group_by(Nationality) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

world <- left_join(world, nationality_counts, by = c("name" = "Nationality"))

world$Count[is.na(world$Count)] <- 0

heatmap <- ggplot(data = world) +
  geom_sf(aes(fill = Count), color = "white") +
  scale_fill_gradient(low = "white", high = "blue", na.value = "grey50") +
  labs(title = "Number of Players from Each Country",
       fill = "Number of Players") +
  theme_minimal()

print(heatmap)
summary(FIFA19[, c('Age', 'Overall','Wage','Value')])
ggplot(FIFA19, aes(x = Age, y = Overall)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relationship between Age and Overall Rating",
       x = "Age",
       y = "Overall Rating")
model <- lm(Overall ~ Age, data = FIFA19)
summary(model)
position_distribution <- FIFA19 %>%
  group_by(Position) %>%
  summarise(Count = n())

ggplot(position_distribution, aes(x = Position, y = Count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p1 <- ggplot(FIFA19, aes(x=Age)) +
  geom_histogram(binwidth=1, fill='blue', color='black', alpha=0.7) +
  labs(title='Distribution of Age', x='Age', y='Count')

p2 <- ggplot(FIFA19, aes(x=Overall)) +
  geom_histogram(binwidth=1, fill='green', color='black', alpha=0.7) +
  labs(title='Distribution of Overall Rating', x='Overall Rating', y='Count')

p3 <- ggplot(FIFA19, aes(x=Potential)) +
  geom_histogram(binwidth=1, fill='red', color='black', alpha=0.7) +
  labs(title='Distribution of Potential', x='Potential', y='Count')

grid.arrange(p1, p2, p3, ncol=1)
corr <- cor(FIFA19[c('Overall','Potential','Wage','Value')])
corr

top_10_nationalities <- FIFA19 %>%
  group_by(Nationality) %>%
  summarise(Avg_Overall = mean(Overall, na.rm = TRUE)) %>%
  arrange(desc(Avg_Overall)) %>%
  head(10)%>%
  pull(Nationality)

print(top_10_nationalities)

players_by_top_10_nationalities <- FIFA19 %>%
  filter(Nationality %in% top_10_nationalities) %>%
  group_by(Nationality) %>%
  summarise(Player_Count = n()) %>%
  arrange(desc(Player_Count))

players_by_top_10_nationalities
player_counts <- FIFA19 %>%
  group_by(Nationality) %>%
  summarise(Player_Count = n())

nationalities_with_300_plus_players <- player_counts %>%
  filter(Player_Count > 300) %>%
  pull(Nationality)
top_10_nationalities_by_avg_rating <- FIFA19 %>%
  filter(Nationality %in% nationalities_with_300_plus_players) %>%
  group_by(Nationality) %>%
  summarise(Avg_Overall = mean(Overall, na.rm = TRUE)) %>%
  arrange(desc(Avg_Overall)) %>%
  head(10)

top_10_nationalities_by_avg_rating

ggplot(top_10_nationalities_by_avg_rating, aes(x = Avg_Overall, y = reorder(Nationality, Avg_Overall))) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Average Overall Rating", y = "Nationality", title = "Average overall rating by nationalities") +
  scale_x_continuous(breaks = seq(0, max(top_10_nationalities_by_avg_rating$Avg_Overall), by = 5)) + 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8, hjust = 0))

FIFA19$Difference <- FIFA19$Potential - FIFA19$Overall

fifa_sorted <- FIFA19[order(-FIFA19$Difference),]

result <- fifa_sorted[c('Name', 'Age', 'Nationality', 'Overall', 'Potential', 'Wage', 'Contract Valid Until', 'Release Clause', 'Speciality')][1:30,]

print(n=30, result)
FIFA19_filtered <- FIFA19[FIFA19$Overall >= 65, ]

FIFA19_filtered$Difference <- FIFA19_filtered$Potential - FIFA19_filtered$Overall

fifa_sorted <- FIFA19_filtered[order(-FIFA19_filtered$Difference), ]

result <- fifa_sorted[c('Name', 'Age', 'Nationality', 'Overall', 'Potential', 'Wage', 'Contract Valid Until', 'Release Clause', 'Speciality')][1:30, ]

print(n = 30, result)

FIFA24 <- read_csv("Desktop/ExploringR/FIFA24.csv")

FIFA24_filtered <- FIFA24[FIFA24$overall >= 65, ]

FIFA24_filtered$Difference <- FIFA24_filtered$potential - FIFA24_filtered$overall

fifa_sorted <- FIFA24_filtered[order(-FIFA24_filtered$Difference), ]

result <- fifa_sorted[c('short_name', 'age', 'nationality_name', 'overall', 'potential', 'wage_eur', 'club_contract_valid_until_year', 'club_position')][1:30, ]

print(n = 30, result)
