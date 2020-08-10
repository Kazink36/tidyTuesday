library(tidyverse)
library(tvthemes)
library(ggpubr)
library(grid) 
library(reshape2)

# Load Data
tuesdata <- tidytuesdayR::tt_load('2020-08-11')
avatar <- tuesdata$avatar
scene_description <- tuesdata$scene_description

#Fix missing data https://www.imdb.com/title/tt10512638/
avatar <- avatar %>%
  mutate(imdb_rating = ifelse(chapter == "The Siege of the North, Part 2",9.7,imdb_rating))

# Choose Characters and count lines per episode
characters <- c("Aang","Katara", "Sokka", "Zuko", "Iroh", "Toph")
char_lines <- avatar %>%
  select(chapter,character,character_words,imdb_rating) %>%
  filter(character %in% characters) %>%
  group_by(chapter,character) %>%
  summarize(num_lines = n(),
            imdb_rating = mean(imdb_rating))

# Function to generate plots by character so that each can have it's own palette
fontCol <- "gray10"
plot_imdb_lines <- function(data,char) {
  pal <- case_when(char %in% c("Sokka","Katara") ~ "WaterTribe",
                   char %in% c("Zuko","Iroh") ~ "FireNation",
                   char == "Aang" ~ "AirNomads",
                   char == "Toph" ~ "EarthKingdom")
  data %>%
    filter(character == char) %>%
    ggplot(aes(x = num_lines,y = imdb_rating,color = ..x..)) +
    geom_point() +
    geom_smooth(method = "lm",se = FALSE) +
    scale_y_continuous(limits = c(5,10)) +
    scale_color_avatar(palette = pal,type = "continuous") +
    labs(title = char,x = "",y="") +
    # function from theme_36.R
    theme_36(color = fontCol) +
    theme(legend.position = "none")
}
g1 <- char_lines %>%
  plot_imdb_lines("Aang")
g2 <- char_lines %>%
  plot_imdb_lines("Toph")
g3 <- char_lines %>%
  plot_imdb_lines("Katara")
g4 <- char_lines %>%
  plot_imdb_lines("Sokka")
g5 <- char_lines %>%
  plot_imdb_lines("Iroh")
g6 <- char_lines %>%
  plot_imdb_lines("Zuko") 


gtot <- ggarrange(g1,g2,g3,g4,g5,g6,nrow = 3,ncol = 2)
lab1 <- annotate_figure(gtot,
                top = text_grob("Avatar: The Last Airbender", color = fontCol, face = "bold", size = 14),
                bottom = text_grob("Number of Lines", color = fontCol),
                left = text_grob("IMDB Rating", color = fontCol, rot = 90)
)
lab2 <- annotate_figure(lab1,
          bottom = text_grob("Source: Avatar Wiki collected by Avery Robins \nVisualization: Jared Lee | @JaredDLee ",
                             color = fontCol,hjust = 1, x = 1, size = 10))
png(filename = "avatar_imdb_numlines.png",width = 3750,height = 5000,res = 500)
# function from theme_36.R
finish_plot(lab2,h = c(34,59),l = 69,c = 40)
dev.off()
