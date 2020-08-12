library(tidyverse)
library(tidytext)
library(wordcloud2)
library(tvthemes)


#Load Data
tuesdata <- tidytuesdayR::tt_load('2020-08-11')
avatar <- tuesdata$avatar

#Prepare words by character
characters <- c("Aang","Katara", "Sokka", "Zuko")
words <- avatar %>%
  select(character,character_words) %>%
  filter(character %in% characters) %>%
  unnest_tokens(word,character_words) %>%
  anti_join(stop_words) %>%
  filter(!(word %in% c("hey","yeah")))

# Aang Wordcloud
cols <- avatar_pal(palette = "AirNomads",n = 7)
aang_words <- words %>%
  filter(character == "Aang") %>%
  count(word) %>%
  arrange(desc(n)) %>%
  filter(n > 1) %>%
  rename(freq = n) 

wordcloud2(aang_words,figPath = "aang.png",size = 1.3,
           color = rep(cols(7),nrow(aang_words)),backgroundColor = "#ece5d3")

# Katara wordcloud
cols <- avatar_pal(palette = "WaterTribe",n = 8)
katara_words <- words %>%
  filter(character == "Katara") %>%
  count(word) %>%
  arrange(desc(n)) %>%
  filter(n > 1) %>%
  rename(freq = n) 

wordcloud2(katara_words,figPath = "katara.png",size = 2.5,
             color = rep(cols(8),nrow(katara_words)),backgroundColor = "#ece5d3")

# Sokka wordcloud
sokka_words<- words %>%
  filter(character == "Sokka") %>%
  count(word) %>%
  arrange(desc(n)) %>%
  filter(n > 1) %>%
  rename(freq = n)

wordcloud2(sokka_words,figPath = "sokka2.png",size = 1.5,
           color = rep(cols(8),nrow(sokka_words)),backgroundColor = "#ece5d3")

# Zuko wordcloud
cols <- avatar_pal(palette = "FireNation",n = 8)
zuko_words <-words %>%
  filter(character == "Zuko") %>%
  count(word) %>%
  arrange(desc(n)) %>%
  filter(n > 1) %>%
  rename(freq = n) 

wordcloud2(zuko_words,figPath = "zuko.png",size = 1.5,
           color = rep(cols(7),nrow(zuko_words)),backgroundColor = "#ece5d3")

