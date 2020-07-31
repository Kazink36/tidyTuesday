library(tidyverse)
library(grid)
library(gridExtra)

# Load Data
tuesdata <- tidytuesdayR::tt_load('2020-07-28')
penguins <- tuesdata$penguins %>% drop_na()

# Function for drawing histograms
# var : Variable from data set to use
# facet_label : Boolean, plot includes Island names on left side when TRUE
# addCaption : Boolean, adds caption for the source material when TRUE
peng_hist <- function(var = c("bill_length_mm","bill_depth_mm","flipper_length_mm","body_mass_g"),
                      facet_label = TRUE,addCaption = FALSE) {
  label <- case_when(var == "bill_length_mm" ~ "Bill Length [mm]",
                     var == "bill_depth_mm" ~ "Bill Depth [mm]",
                     var == "flipper_length_mm" ~ "Flipper Length [mm]",
                     var == "body_mass_g" ~ "Body Mass [g]")
  if (facet_label) {
    labeller = function(x){return(x)}
  } else {
    labeller = function(x){return("")}
  }
  h <- penguins %>%
    ggplot() +
    geom_histogram(aes(x = .data[[var[[1]]]],fill = species),position = "identity",alpha = 0.8) +
    scale_fill_manual(values = c("#ff8701","#cf65d0","#0e7e80")) +
    scale_y_continuous(position = "right") +
    labs(x = label,y = "",caption = "") +
    facet_wrap(~island,ncol = 1,strip.position = "left",labeller = labeller) +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.minor.y = element_blank(),
          text = element_text(family = "Charter"),
          axis.text.y = element_blank())
  if (addCaption) {
    h <- h + labs(caption = "Source: Gorman, Williams & Fraser (2014) | Illustrations: Allison Horst")
  } 
  return(h)
}

# Generate the different histograms
h1 <- peng_hist("bill_length_mm")
h2 <- peng_hist("bill_depth_mm",facet_label = FALSE)
h3 <- peng_hist("flipper_length_mm",facet_label = FALSE)
h4 <- peng_hist("body_mass_g",facet_label = FALSE,addCaption = TRUE)

# Get and draw the images
img <- image_read("https://allisonhorst.github.io/palmerpenguins/man/figures/palmerpenguins.png")
img <- ggdraw()+draw_image(img,scale = .7)
img2 <- image_read("https://allisonhorst.github.io/palmerpenguins/man/figures/lter_penguins.png")
img2 <- ggdraw() + draw_image(img2)

# Save Plots
png("palmer_penguins.png",width = 30,height = 23, units = "cm",res = 300)
grid.arrange(h1,h2,h3,h4,img,img2,nrow = 2,heights = c(2,1),
             layout_matrix = rbind(c(1,2,3,4),c(5,6,6,6)),
             top = textGrob("Features of Palmer Penguins from 2007 to 2009 by Island", gp=gpar(fontsize=20,font=8,fontfamily = "Charter")))
dev.off()
