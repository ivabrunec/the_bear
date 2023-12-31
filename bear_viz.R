## visualization of sentiment analysis previously performed in python
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(showtext)
library(dplyr)
library(ggplot2)
library(packcircles)
library(extrafont)
loadfonts(device='win')

# read in processed data with polarity scores
bear_data <- read.csv('forks_polarity_all.csv')

bear_sum_scene <- bear_data |>
  group_by(scene) |>
  summarise(mean_pol = mean(polarity),
            mean_loc = mean(X))

ggplot() +
  geom_path(data = bear_data, aes(x = X, y = polarity_rolling,
                                  color = scene),
            alpha = .2) +
  geom_point(data = bear_sum_scene, aes(x = mean_loc, y = mean_pol,
                                        color = scene)) 

#### circle packing ####
# let's try something fun
# pack circles - each circle is a score, per scene
# loop over scenes & then bind them back together
# we want each scene to have the same scale, and calculating independently achieves that
# i'm sure this could be done with an apply statement somehow but this is quick and works

# initiate list
data_circles <- list()

for (scene_num in unique(bear_data$scene)){
  temp_data <- filter(bear_data, scene == scene_num)
  # generate vector of areas of 1, this will be used for circle packing
  areas <- c(rep(1, nrow(temp_data))) 
  packing <- circleProgressiveLayout(areas) 
  
  # now add the packing data to existing df
  temp_data_circles <- cbind(temp_data, packing)
  
  # save to list so we can re-transform to long format later
  data_circles[[scene_num]] <- temp_data_circles
}

bear_data_circles <- do.call(rbind, data_circles)

# plot and add scene image backgrounds ####
# facet_wrap is not compatible with different image backgrounds (as far as i know)
# so we're doing this a stupid way: 
# loop over scenes, generate plot for each of them, and add background conditional on the scene number
# later on, we'll combine everything using magick
# scroll below if you just want the simple facet_wrap solution without backgrounds

# set image titles
# these are manually set, and the background images are manually combined with plots
scene_titles <- c(" 'that's something.' ",
                  " 'fuck you, cousin' ",
                  " 'first week is forks.' ",
                  " '(fork clangs)' ",
                  " 'I can do respect.' ",
                  " 'did you talk about \nthe smudge?' ",
                  " 'a little bit hard to say' ",
                  " 'oh, my gosh' ",
                  " '(sighs)' ",
                  " 'every night, you make\n somebody's day' ",
                  " 'go get 'em, Richie.' ",
                  " 'it's a love story, \nbaby just say yes' ",
                  " 'acts of service' ",
                  " 'I don't need anything' ",
                  " 'time well spent' ")
library(khroma)

roma <- color('roma')
custom_colors <- rev(roma(256))

for (scene_num in unique(bear_data_circles$scene)){
  temp <- filter(bear_data_circles, scene == scene_num)
  
  ggplot() +
    annotate("text", x = 0, y = 10, label = scene_titles[scene_num],
             family = 'Helvetica Neue', fontface='bold.italic', size = 20, color = 'white',
             lineheight=.7) +
    ggforce::geom_circle(data = temp,
                         aes(x0 = x, y0 = y, r = radius, fill = polarity_rolling),
                         color = NA, alpha = .9) +
    #scale_alpha_identity() +
    scale_fill_gradientn(
      colours = custom_colors,
      values = scales::rescale(c(-1, 0, 1)),
      limits = c(-0.42,0.42)
    ) +
    xlim(c(-12,12)) +
    ylim(c(-12,12)) +
    coord_equal() +
    theme_void() +
    theme(strip.background = element_blank(),
          strip.text.x = element_blank(),
          legend.title = element_blank(),
          legend.position = 'none',
          legend.margin = margin(t = 20, unit = "pt"),
          legend.key.height = unit(dev.size()[1] / 15, "cm"))
  
  file_name <- paste0('plot_',scene_num,'.png')
  ggsave(file_name, height = 10, width = 10, dpi =300)
}

## NOT USED: geom_facet if you just want to plot the data without backgrounds ####
ggplot() +
  ggforce::geom_circle(data = bear_data_circles,
                       aes(x0 = x, y0 = y, r = radius, fill = polarity_rolling,
                           alpha = ifelse(polarity_rolling == 0, 0.5, 1)),
                       color = NA) +
  facet_wrap(~scene, ncol = 5) +
  scale_alpha_identity() +
  scale_fill_gradientn(
    colours = custom_colors,
    values = scales::rescale(c(-1, 0, 1)),
    limits = c(-0.42,0.42)
  ) +
  coord_equal() +
  theme_void() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.margin = margin(t = 20, unit = "pt"),
        legend.key.height = unit(dev.size()[1] / 15, "cm"),
        legend.text = element_text(color= 'white')) 
ggsave('bear_facet_all.png', width = 10, height = 6, dpi = 300)
