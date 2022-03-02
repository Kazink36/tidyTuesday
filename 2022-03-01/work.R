tuesdata <- tidytuesdayR::tt_load(2022, week = 9)

library(tidyverse)
library(geofacet)

data <- tuesdata$stations

glimpse(data)

data <- data %>%
  janitor::clean_names()

factor_order <- data %>%
  filter(!is.na(facility_type)) %>%
  count(facility_type,sort = TRUE) %>%
  pull(facility_type)

plot_data <- data %>%
  filter(!is.na(facility_type)) %>%
  count(state,facility_type,sort = TRUE) %>%
  mutate(facility_type_fct = fct_relevel(facility_type,factor_order),
         facility_type_fct = fct_lump_n(facility_type_fct,10,w = n),
         facility_type_new = str_replace_all(facility_type,"_"," "),
         facility_type_new = tools::toTitleCase(tolower(facility_type_new)),
         facility_type_new = ifelse(facility_type_fct == "Other","Other", facility_type_new),
         facility_type_new = factor(facility_type_new,
                                    levels = c(
                                      "Convenience Store",
                                      "Hotel",
                                      "Car Dealer",
                                      "Fuel Reseller",
                                      "Muni Gov",
                                      "Fed Gov",
                                      "Office Bldg",
                                      "Shopping Center",
                                      "Rental Car Return",
                                      "Utility",
                                      "Other"
                                    ))) %>%
  group_by(state) %>%
  mutate(prop = n/sum(n))

labels <- plot_data %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  mutate(facility_type_new = str_replace_all(facility_type,"_"," "),
         facility_type_new = tools::toTitleCase(tolower(facility_type_new))) %>%
  mutate(label = paste0(facility_type_new,"\n",round(prop*100,1),"%"))

plot_data %>%
  ggplot(aes(x = facility_type_fct, y = prop)) +
  geom_col(aes(fill = facility_type_new),color = "gray20") +
  geom_text(data = labels,aes(x = 6,y = .5, label = label)) +
  facet_geo(~ state,label = "name") +
  scale_fill_brewer(palette = "Spectral") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "",y = "",
       title = "Types of Facilities with Alternative Fuel Stations",
       subtitle = "The percentage of each facility type with an alternative fuel station out of all such facilities in each state",
       caption = "@JaredDLee | Data: US Department of Transportation") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        text = element_text(family = "Oswald"),
        title = element_text(family = "Oswald"),
        legend.position = c(0.2,0.95),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 50, face = "bold", hjust = .5,color = scales::muted("green")),
        plot.subtitle = element_text(size = 24,hjust = 0.5),
        plot.caption = element_text(size = 14),
        legend.background = element_rect(color = "gray20"),
        strip.text = element_text(size = 16,face = "bold"),
        legend.direction = "horizontal")

ggsave("station_type_state.png",width = 12, height = 8)
