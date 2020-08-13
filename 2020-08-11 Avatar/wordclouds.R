library(tidyverse)
library(tidytext)
library(wordcloud2)
library(tvthemes)


#Load Data
tuesdata <- tidytuesdayR::tt_load('2020-08-11')
avatar <- tuesdata$avatar

#Prepare words by character
#characters <- c("Aang","Katara", "Sokka", "Zuko")
words <- avatar %>%
  select(character,character_words) %>%
  #filter(character %in% characters) %>%
  unnest_tokens(word,character_words) %>%
  anti_join(stop_words) %>%
  filter(!(word %in% c("hey","yeah")))

# Aang word cloud
cols <- avatar_pal(palette = "AirNomads",n = 7)
aang_words <- words %>%
  filter(character == "Aang") %>%
  count(word) %>%
  arrange(desc(n)) %>%
  filter(n > 1) %>%
  rename(freq = n) 

wordcloud2(aang_words,figPath = "image/aang.png",size = 1.3,
           color = rep(cols(7),nrow(aang_words)),backgroundColor = "#ece5d3")

# Katara word cloud
cols <- avatar_pal(palette = "WaterTribe",n = 8)
katara_words <- words %>%
  filter(character == "Katara") %>%
  count(word) %>%
  arrange(desc(n)) %>%
  filter(n > 1) %>%
  rename(freq = n) 

wordcloud2(katara_words,figPath = "image/katara.png",size = .5,
             color = rep(cols(8),nrow(katara_words)),backgroundColor = "#ece5d3")

# Sokka word cloud
sokka_words<- words %>%
  filter(character == "Sokka") %>%
  count(word) %>%
  arrange(desc(n)) %>%
  filter(n > 1) %>%
  rename(freq = n) %>%
  mutate(freq = round(sqrt(freq)))

wordcloud2(sokka_words,figPath = "image/sokka.png",size = .5,
           color = rep(cols(8),nrow(sokka_words)),backgroundColor = "#ece5d3")

# Zuko word cloud
cols <- avatar_pal(palette = "FireNation",n = 8)
zuko_words <-words %>%
  filter(character == "Zuko") %>%
  count(word) %>%
  arrange(desc(n)) %>%
  filter(n > 1) %>%
  rename(freq = n) %>%
  mutate(freq = round(sqrt(freq)))

wordcloud2(zuko_words,figPath = "image/zuko.png",size = .5,
           color = rep(cols(7),nrow(zuko_words)),backgroundColor = "#ece5d3")

# Iroh word cloud
iroh_words <- words %>%
  filter(character == "Iroh") %>%
  count(word) %>%
  arrange(desc(n)) %>%
  filter(n > 1) %>%
  rename(freq = n) %>%
  mutate(freq = round(freq^(2/3)))

wordcloud2(iroh_words,figPath = "image/iroh.png",size = .6,
           color = rep(cols(7),nrow(iroh_words)),backgroundColor = "#ece5d3")


# Toph word cloud
cols <- avatar_pal(palette = "EarthKingdom",n=8)
toph_words <- words %>%
  filter(character == "Toph") %>%
  count(word) %>%
  arrange(desc(n)) %>%
  filter(n > 1) %>%
  rename(freq = n) 

wordcloud2(toph_words,figPath = "image/toph.png",size = .7,
           color = rep(cols(8),nrow(toph_words)),backgroundColor = "#ece5d3")


