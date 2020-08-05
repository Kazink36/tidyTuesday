library(tidyverse)
library(magick)
library(cowplot)
library(gganimate)
library(transformr)

# Load Data
tuesdata <- tidytuesdayR::tt_load('2020-08-04')
energy_types <- tuesdata$energy_types
country_totals <- tuesdata$country_totals
# European Regions: Eurovoc
# https://en.wikipedia.org/wiki/Eurovoc
regions <- read_csv("EurovocRegion.csv") 

# Pivot the data:
# Each observation is the energy production for each country and each year
country_pivoted <- bind_rows(energy_types,country_totals) %>%
  # Turn yearly values to rows
  pivot_longer(`2016`:`2018`,names_to = "year") %>%
  select(-level) %>%
  # Turn types into columns
  pivot_wider(names_from = type,values_from = value)  %>%
  # Add missing country_name
  mutate(country_name = case_when(country == "UK" ~ "United Kingdom",
                                  country == "EL" ~ "Greece",
                                  TRUE ~ country_name)) %>%
  # Conver to TWh
  mutate_if(is.numeric,function(x){x/1000}) %>%
  # join with Eurovoc regions
  left_join(regions) %>%
  # Calculate new values and percentages
  mutate(Renewable = Hydro + Wind + Solar + Geothermal,
         Clean = Renewable + Nuclear,
         percRenew = round(Renewable / `Total net production` * 100,2),
         percClean = round(Clean / `Total net production` *100,2),
         percThermal = round(`Conventional thermal` / `Total net production` * 100, 2))

# Read Eurovoc map
img <- png::readPNG("euro_regions.png")
# Custom function for positioning labels and map
## v : vector of numbers
## width : how far min and max are from mid
## pos : position along v from 0 to 1
minmax <- function(v,width,pos) {
  x <- list()
  mid <- (max(v) + min(v)) * pos
  x$mid <- mid
  x$max <- mid + width
  x$min <- mid - width 
  return(x)
}
# Placement of map
rastery <- minmax(country_pivoted$Clean,150,.25)
rasterx <- minmax(country_pivoted$`Conventional thermal`,150,.8)
# Plot of Clean Energy vs Conventional Thermal Energy
g1 <- country_pivoted %>%
  ggplot(aes(x = `Conventional thermal`, y = Clean,
             label = country)) +
  annotation_raster(img, ymin = rastery$min,ymax= rastery$max,
                    xmin = rasterx$min,xmax = rasterx$max) +
  geom_point(aes(size = `Total net production`,color = region),alpha = .6) +
  geom_text(aes(size = (`Total net production`)/2)) +
  geom_label(data = tibble(year = c("2016","2017","2018")),size = 5,
             x = rasterx$mid,y = rastery$mid,aes(label = year),fill = "gray75",alpha = .4) +
  theme_minimal() +
  labs(x = "Conventional Thermal Energy [TWh]",y = "Clean Energy [TWh]", 
       title = "European Energy Production: 2016-2018",
       caption = "Data: Eurostat | Visualization: Jared Lee | @JaredDLee") + 
  scale_color_manual(values = c("#e62121","#0376d3","#f6d603","#67e863")) +
  #coord_cartesian(xlim = c(0,100000),ylim = c(0,100000)) +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "gray75"),
        panel.grid = element_line(color = "gray25")
        )
g1

# Animation

anim <- g1 + 
  transition_time(as.numeric(year)) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()
animate(anim, fps = 20, width = 750, height = 720, end_pause = 30)
anim_save("energy.gif")


# Placement of map
rastery <- minmax(c(0,50),15,.25)
rasterx <- minmax(c(0,50),15,.8)
# Same plot zoomed in
g2 <- country_pivoted %>%
  ggplot(aes(x = `Conventional thermal`, y = Clean,
             label = country)) +
  annotation_raster(img, ymin = rastery$min,ymax= rastery$max,
                    xmin = rasterx$min,xmax = rasterx$max) +
  geom_point(aes(color = region),alpha = .6,size = 5) +
  geom_text(size = 3) +
  geom_label(data = tibble(year = c("2016","2017","2018")),size = 5,
             x = rasterx$mid,y = rastery$mid-6 ,aes(label = year),fill = "gray75",alpha = .4) +
  theme_minimal() +
  labs(x = "Conventional Thermal Energy [TWh]",y = "Clean Energy [TWh]", 
       title = "European Energy Production: 2016-2018",
       caption = "Data: Eurostat | Visualization: Jared Lee | @JaredDLee") + 
  scale_color_manual(values = c("#e62121","#0376d3","#f6d603","#67e863")) +
  coord_cartesian(xlim = c(0,50),ylim = c(0,50)) +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "gray75"),
        panel.grid = element_line(color = "gray25")
  )
g2

#Animation
anim <- g2 + 
  transition_time(as.numeric(year)) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()
animate(anim, fps = 20, width = 750, height = 720, end_pause = 30)
anim_save("energyZoom.gif")



