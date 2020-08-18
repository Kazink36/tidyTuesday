library(tidyverse)
library(cowplot)
library(grid)
library(gridExtra)

# Load Data
tuesdata <- tidytuesdayR::tt_load('2020-08-18')
plants <- tuesdata$plants
threats <- tuesdata$threats
actions <- tuesdata$actions


# Threat type by year
plot_data <- threats %>%
  filter(threatened == 1 & threat_type != "Unknown" & year_last_seen != "NA") %>%
  count(threat_type,year_last_seen) %>%
  mutate(year_last_seen = fct_relevel(year_last_seen,"Before 1900")) %>%
  arrange(year_last_seen) %>%
  group_by(threat_type) %>%
  mutate(sum = cumsum(n)) 
g1 <- plot_data %>%
  ggplot(aes(x = year_last_seen, y = sum,color = threat_type,group = threat_type)) +
  geom_line() +
  geom_point() +
  # geom_label(data = plot_data%>%filter(year_last_seen == "2000-2020")%>% arrange(sum)%>%ungroup()%>%mutate(y =row_number()),
  #           aes(x = 1.5,y = y*20,label = threat_type,color = threat_type),
  #           nudge_y = 4,family = "Valentina",alpha = .75,fill = "gray50") +
  labs(y = "Number of Plants", x = "Year Plant Was Last Seen",title = "Threat Type") +
  scale_color_brewer(palette = "Spectral") +
  theme_minimal() +
  theme(text = element_text(family = "Valentina"),
        axis.text.x = element_text(angle = 50,hjust = .7,vjust = .8),
        legend.position = "none"
  )
ggsave("threat_type_by_year.png",width = 8,height = 8,units = "in",dpi = "retina")



#Threats_by_country
top_country_threats <- threats %>%
  filter(threatened == 1 & threat_type != "Unknown") %>%
  count(country,sort = TRUE) %>%
  top_n(7,n) %>% select(country) %>%
  left_join(ISOcodes::ISO_3166_1%>%select(Alpha_2,Name),by = c("country" = "Name")) %>%
  mutate(Alpha_2 = ifelse(country == "Tanzania","TZ",Alpha_2))
g2 <- threats %>%
  filter(threatened == 1 & threat_type != "Unknown") %>%
  filter(country %in% top_country_threats$country) %>%
  count(country,threat_type) %>%
  group_by(country) %>%
  mutate(tot = sum(n),prop = n/tot) %>%
  ggplot() +
  geom_col(aes(y = country,x = n,fill = threat_type),position = "fill") + 
  ggimage::geom_flag(data = top_country_threats,aes(x = -.1,y = country,image = Alpha_2)) +
  labs(x = "Countries with Most Threatened Plants", y = "") +
  scale_x_continuous(labels = scales::percent,breaks = c(0,.25,.5,.75,1)) +
  scale_fill_brewer(palette = "Spectral") +
  theme(text = element_text(family = "Valentina"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.box.just = "left") 
ggsave("threats_by_country.png",width = 8,height = 8,units = "in",dpi = "retina")


g3 <- actions %>%
  filter(action_taken == 1 & action_type != "Unknown") %>%
  count(continent,action_type,sort = TRUE) %>%
  group_by(continent) %>% mutate(tot = sum(n),prop = n/tot) %>%
  group_by(action_type) %>% mutate(tot_act = sum(n),prop_act = n/tot_act,
                                   prop_max = max(prop),value = prop/prop_max) %>% 
  select(continent,action_type,value)%>%
  pivot_wider(id_cols = continent,values_from = value,names_from = action_type,values_fill = 0)%>%
  gather("action_type","prop",`Land & Water Protection`:`Education & Awareness`) %>% ungroup() %>%
  ggplot(aes(x = action_type,y = prop,group = continent,fill = continent)) +
  ylab(NULL) + xlab(NULL) +
  geom_col(position = "dodge") +
  geom_vline(xintercept = c(.5,1.5,2.5,3.5,4.5),color = "grey92") +
  coord_polar(clip = "off") +
  scale_fill_brewer(palette = "Spectral") +
  theme_minimal() +
  ggtitle("Action Taken",subtitle = "Normalized by Continent with the \nhighest proportion of that action") +
  theme(text = element_text(family = "Valentina"),
        axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        plot.background = element_rect(color = "gray92"),
        plot.margin = margin(10,35,0,35))
ggsave("action_by_cont.png",width = 8,height = 8,units = "in",dpi = "retina") 



png("final_plant.png",width = 10, height = 8, units = "in",res = 320)
gridExtra::grid.arrange(g1,g2,g3,layout_matrix = rbind(c(1,3),c(2,2)),heights = c(2,1),
                        top = textGrob("Plant Extinction",
                                       gp=gpar(fontsize=20,font=8,fontfamily = "Valentina")),
                        bottom = textGrob("Data: IUCN | Viz: @JaredDLee",
                                          gp = gpar(fontsize = 12,font = 8, fontfamily = "Valentina"),
                                          hjust = -.6))
dev.off()



