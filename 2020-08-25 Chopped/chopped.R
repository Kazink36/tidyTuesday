library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2020-08-25')
chopped <- tuesdata$chopped

ingredients <- chopped %>%
  select(episode_rating,appetizer,entree,dessert) %>%
  separate(appetizer, paste0("appIng",1:4),sep = ", ") %>%
  separate(entree, paste0("entIng",1:6),sep = ", ") %>%
  separate(dessert, paste0("desIng",1:5),sep = ", ") %>%
  pivot_longer(appIng1:desIng5) %>%
  drop_na()  %>%
  mutate(name = case_when(name %in% paste0("appIng",1:4) ~ "Appetizer",
                          name %in% paste0("entIng",1:6) ~ "Entree",
                          name %in% paste0("desIng",1:5) ~ "Dessert"))

plot_data<-ingredients %>%
  filter(str_detect(value,"cheese")) %>%
  mutate(value = str_remove(value,"boxed "),
         value = ifelse(str_detect(value,"mac "),"macaroni and cheese",value)) %>%
  group_by(value) %>%
  mutate(n = n(),rating = mean(episode_rating,na.rm = TRUE)) %>%
  filter(n > 2) %>%
  ungroup() %>%
  mutate(value = tools::toTitleCase(value),
         value = as.factor(value),
         name = fct_relevel(name,"Appetizer","Entree","Dessert")
         ) 
labels <- plot_data %>%
  transmute(label = value, x = as.numeric(value)) %>%
  distinct() %>%
  arrange(x) %>%
  mutate(angle = c(-30,-70,70,30,-10,-45,90,50,10))
ggplot() +
  geom_polygon(data = tibble(x = c(.2,.2,9.3,9.3),y = c(6.5,9.5,9.5,6.5)),mapping = aes(x,y),fill = "#ffe829")+
  geom_segment(data = tibble(x = seq(.5,8.5,1),y = 6.5,yend = 9.5),
               mapping = aes(x = x,xend = x,y = y,yend = yend),color = "gray10") +
  geom_point(data = plot_data,aes(x = as.numeric(value),y = episode_rating,color = name),shape = 16,
             position = position_jitter(height = .1,width = .3),size = 5) +
  geom_text(data = labels,mapping = aes(x = x,y = 10,label = label,angle = angle)) +
  scale_color_manual(values = c("#fdc010","#ff9103","#fb6d00")) +
  coord_polar(clip = "off") +
  labs(x = "",y = "",title = "Chopped Cheese",
       subtitle = "IMDB Rating of Chopped episode when cheese is one of the ingredients.",
       caption = "Viz: @JaredDLee | Data: Kaggle") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(fill = "#ffe829"))
