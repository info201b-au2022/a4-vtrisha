install.packages('ggplot2')
library(ggplot2)
library(tidyverse)
install.packages('dplyr')
library(dplyr)
urlfile <- ("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
incarceration <- read.csv(url(urlfile))

highest_jail_pop_2018 <- incarceration %>%
  filter(year == max(year, na.rm = T)) %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = T)) %>%
  pull(state, total_jail_pop)

highest_jail_pop_2018

highest_jail_pop_year <- incarceration %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = T)) %>%
  pull(year)

highest_jail_pop_year

highest_state_with_black <- incarceration %>%
  filter(year == max(year, na.rm = T)) %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = T)) %>%
  pull(state, black_jail_pop)

highest_state_with_black

get_year_jail_pop <- incarceration %>%
  select(year, total_pop) %>%
  arrange(year) %>%
  group_by(year) %>%
  summarize(total = sum(total_pop))

plot_jail_pop_for_us <- ggplot(tot_pop_df, aes(x = year, y = total)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  labs(titles = "Population through the Years")

plot_jail_pop_for_us

get_jail_pop_by_states <- function(states) {
  d = subset(incarceration, state %in% states) 
  d = d %>% select(state, total_jail_pop, total_pop)
  return(d)
}
states = c("WA", "OR", "CA", "NY")
plot_jail_pop_by_states <- function(states) {
  df = get_jail_pop_by_states(states)
  df = df %>% select(state, total_jail_pop, total_pop)
  plot = ggplot(df, aes(x = state, y = total_jail_pop, color = state)) +
    geom_line() + 
    ggtitle("Jail Population for Specific States") +
    labs(y = "Total Jail Population",
         x = "States",
         caption = "Jail Population for States ")
  return(plot)
}

plot_jail_pop_by_states(states)

gender_difference <- incarceration %>%
  select(year, female_jail_pop, male_jail_pop) %>%
  na.omit() %>%
  arrange(year) %>%
  group_by(year) %>%
  summarize(female = sum(female_jail_pop), male =  sum(male_jail_pop))

tot_jail_pop <- incarceration %>%
  select(year, total_jail_pop) %>%
  na.omit() %>%
  arrange(year) %>%
  group_by(year) %>%
  summarize(total = sum(total_jail_pop))

comparison_chart <- ggplot(gender_diff_df) +
  geom_area(aes(x = year, y = male, fill = "Male")) +
  geom_area(aes(x = year, y = female, fill = "Female")) +
  labs(title = "Jail Population compared to Gender", x = "Year", y = "Jail Population")

comparison_chart

map <- map_data("state")
install.packages('usmap')
library(usmap)
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
black_state <- incarceration %>%
  group_by(state) %>%
  filter(year == max(year, na.rm = T)) %>%
  filter(black_jail_pop == black_jail_pop) %>%
  filter(total_pop == total_pop) %>%
  mutate(total_pop / black_jail_pop) %>%
  summarize(
    pop = sum(black_jail_pop), total = max(total_pop),
    mutate = sum(total_pop / black_jail_pop)
  )
map <- plot_usmap(
  data = blackx_state, values = "pop", color = "black",
  name = "Black Jail Population"
) +
  coord_fixed(1) +
  blank_theme +
  scale_fill_gradientn(
    colours = c("white", "brown"),
    breaks = c(10, 100, 1000, 10000),
    trans = "log10", name = "Black Jail Population"
  ) +
  labs(title = "The United States", subtitle = "Black Jail Population in
       2018", name = "Black Jail Population") +
  theme(legend.position = "right")

map
