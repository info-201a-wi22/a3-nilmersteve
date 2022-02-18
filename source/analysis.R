dt <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

library(tidyverse)
library(maps)
library(mapproj)
library(patchwork)

tot_prop_king_2018 <- dt %>%
  filter(year == 2018) %>%
  filter(county_name == "King County") %>%
  filter(state == "WA") %>%
  summarize(var = black_pop_15to64 / white_pop_15to64) %>%
  pull(var)
      
jail_prop_king_2018 <- dt %>%
  filter(year == 2018) %>%
  filter(county_name == "King County") %>%
  filter(state == "WA") %>%
  summarize(var = black_jail_pop / white_jail_pop) %>%
  pull(var)

percent_in_jail_black <- dt %>%
  filter(county_name == "King County") %>%
  filter(state == "WA") %>%
  filter(year == 2018) %>%
  summarize(var = (black_jail_pop / black_pop_15to64) * 100) %>%
  pull(var)

percent_in_jail_white <- dt %>%
  filter(county_name == "King County") %>%
  filter(state == "WA") %>%
  filter(year == 2018) %>%
  summarize(var = (white_jail_pop / white_pop_15to64) * 100) %>%
  pull(var)

avg_pop_prop <- dt %>%
  filter(county_name == "King County") %>%
  filter(state == "WA") %>%
  summarize(var = mean(black_pop_15to64, na.rm=TRUE) / mean(white_pop_15to64, na.rm=TRUE)) %>%
  pull(var)

avg_jailed_prop <- dt %>%
  filter(county_name == "King County") %>%
  filter(state == "WA") %>%
  summarize(var = mean(black_jail_pop, na.rm=TRUE) / mean(white_jail_pop, na.rm=TRUE)) %>%
  pull(var)

tot_prop_king <- dt %>%
  filter(county_name == "King County") %>%
  filter(state == "WA") %>%
  filter(year > 1989) %>%
  summarize(var = black_pop_15to64 / white_pop_15to64) %>%
  pull(var)

jail_prop_king <- dt %>%
  filter(county_name == "King County") %>%
  filter(state == "WA") %>%
  filter(year > 1989) %>%
  summarize(var = black_jail_pop / white_jail_pop) %>%
  pull(var)

years_recorded <- dt %>%
  filter(county_name == "King County") %>%
  filter(state == "WA") %>%
  filter(year > 1989) %>%
  pull(year)

chart_df <- data.frame(years_recorded, jail_prop_king, tot_prop_king)


all_percent_in_jail_black <- dt %>%
  filter(county_name == "King County") %>%
  filter(state == "WA") %>%
  filter(year > 1989) %>%
  summarize(var = (black_jail_pop / black_pop_15to64) * 100) %>%
  pull(var)

all_percent_in_jail_white <- dt %>%
  filter(county_name == "King County") %>%
  filter(state == "WA") %>%
  filter(year > 1989) %>%
  summarize(var = (white_jail_pop / white_pop_15to64) * 100) %>%
  pull(var)

disparity_in_2018 <- dt %>%
  filter(year == 2018) %>%
  mutate(overall_prop = black_pop_15to64 / white_pop_15to64) %>%
  mutate(jail_prop = black_jail_pop / white_jail_pop) %>%
  mutate(difference = jail_prop - overall_prop)
  
chart2_df <- data.frame(years_recorded, all_percent_in_jail_black, all_percent_in_jail_white)

county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

map_data <- county_shapes %>%
  left_join(disparity_in_2018, by = "fips") %>%
  filter(state == "WA")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
)

cases_map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = difference),
    color = "gray", size = 0.3) +
  coord_map()+
  scale_fill_continuous(limits = c(min(map_data$difference), max(map_data$difference)), na.value = "white", low = "yellow", high = "red")+
  blank_theme +
  labs(title = "Difference Between Overall Proportion and Proportion in Jail", fill='Proportion of Races in Jail Minus Total Proportion') 