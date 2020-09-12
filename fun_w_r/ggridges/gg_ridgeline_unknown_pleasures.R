
library('dplyr')
library('tidyr')
library('tidyverse')
library('ggridges')
library('gganimate')

URL <- "https://gist.githubusercontent.com/borgar/31c1e476b8e92a11d7e9/raw/0fae97dab6830ecee185a63c1cee0008f6778ff6/pulsar.csv"
pulsar <- read.csv(URL, header = FALSE) %>%
  mutate(row = row_number()) %>%
  gather(col, height, -row) %>%
  mutate(
    col = sub("^V", "", col) %>% as.integer()
  )

p <- ggplot(pulsar, aes(x = col, y = row, height = height, group = row)) +
  geom_ridgeline(min_height = min(pulsar$height),
                 scale= 0.2,
                 size = 1,
                 fill = "black",
                 colour = "white") +
  scale_y_reverse() +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black", color = "black")
  ) 

print(p)

p <- p + transition_states(row) +
  shadow_mark(alpha = 0.8)

# save animated gif
animate(p, duration = 8, fps = 15, height = 600, width = 350, renderer = gifski_renderer())

anim_save(here::here("figures","unknown_pleasures.gif"))

# save .mov
library('av')
animate(p, duration = 8, fps = 15, height = 600, width = 350, renderer = av_renderer())
anim_save(here::here("figures","unknown_pleasures.mp4"))




