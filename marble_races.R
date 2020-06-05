library(tidyverse)
library(ggridges)
library(ggthemes)
library(ggcharts)




tbl_marbles_org <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')


# Track Lengths by Site

tbl_marbles_org %>% distinct(site, track_length_m) %>% 
  ggcharts::lollipop_chart(data = ., 
                           x = site, 
                           y = track_length_m, sort = TRUE, 
                           horizontal = TRUE, 
                           line_color = "orange", 
                           point_color = "red") + 
  theme_solarized_2(light = FALSE) + 
  theme(axis.text.x = element_text(color = "white"), 
        axis.text.y = element_text(color = "white"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white"), 
        plot.title = element_text(color = "white")) +
  labs(
    x = "Tracks", 
    y = "Track Length (in Metres)", 
    caption = "Tidy Tuesday | Marble Racing | @arnabp123"
  ) + 
  ggtitle("Track Lengths")
  



  


tbl_marbles_qualifiers <- mutate(tbl_marbles_org, 
                      date = parse_date(date, format = "%d-%b-%y"), 
                      race_type = case_when(str_detect(race, "Q") ~ "Qualifiers", 
                                            TRUE ~ "Main Race")) %>%
                      filter(race_type == "Qualifiers")


tbl_marbles_main_races <- mutate(tbl_marbles_org, 
                                 date = parse_date(date, format = "%d-%b-%y"), 
                                 race_type = case_when(str_detect(race, "Q") ~ "Qualifiers", 
                                                       TRUE ~ "Main Race")) %>%
                          filter(race_type == "Main Race")


# Qualifiers 

# Qualifiers - Team Name v/s Team Rank

p_qual_team_ranks <- tbl_marbles_qualifiers %>% group_by(site, race) %>% 
  mutate(team_rank = dense_rank(avg_time_lap, 
                                na.last = TRUE, 
                                ties.method = "min")) %>%
  ungroup() %>% 
  select(site, team_name, team_rank, pole, avg_time_lap) %>% 
  ggplot() + 
  geom_density_ridges(mapping = aes(x = team_rank, 
                                    y = team_name, 
                                    fill = team_name, 
                                    color = team_name, 
                                    alpha = 0.85), 
                      show.legend = FALSE ) + 
  scale_fill_discrete(h = c(0, 360)) + 
  scale_color_discrete(h = c(0, 360)) + 
  theme_solarized_2(light = FALSE) + 
  theme(axis.text.x = element_text(color = "white"), 
        axis.text.y = element_text(color = "white"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white"), 
        plot.title = element_text(color = "white")) +
  labs(
    x = "Team Rank", 
    y = "Team Name", 
    caption = "Tidy Tuesday | Marble Racing | @arnabp123"
  ) + 
  ggtitle("Qualifiers - Team Name v/s Team Rank")


p_qual_team_ranks


# Qualifiers - Team Rank v/s Pole Positions

p_qual_team_poles <- tbl_marbles_qualifiers %>% group_by(site, race) %>% 
  mutate(team_rank = dense_rank(avg_time_lap, 
                                na.last = TRUE, 
                                ties.method = "min")) %>%
  ungroup() %>% 
  select(site, team_name, team_rank, pole, avg_time_lap) %>% 
  ggplot() + 
  geom_density_ridges(mapping = aes(x = team_rank, 
                                    y = reorder(pole, as.integer(str_replace(pole, "P", ""))), 
                                    fill = pole, 
                                    color = pole), 
                      show.legend = FALSE ) + 
  scale_fill_discrete(h = c(0, 360)) + 
  scale_color_discrete(h = c(0, 360)) + 
  theme_solarized_2(light = FALSE) + 
  theme(axis.text.x = element_text(color = "white"), 
        axis.text.y = element_text(color = "white"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white"), 
        plot.title = element_text(color = "white")) +
  labs(
    x = "Team Rank", 
    y = "Pole Position", 
    caption = "Tidy Tuesday | Marble Racing | @arnabp123"
  ) + 
  ggtitle("Qualifiers - Team Rank v/s Pole Positions")

p_qual_team_poles



#Main Races

# Tracks by their Number of Laps

p_track_laps <- tbl_marbles_main_races %>% distinct(site, number_laps) %>% 
  ggcharts::lollipop_chart(data = ., 
                           x = site, 
                           y = number_laps, 
                           sort = TRUE, 
                           horizontal = TRUE, 
                           line_color = "orange", 
                           point_color = "red") + 
  theme_solarized_2(light = FALSE) + 
  theme(axis.text.x = element_text(color = "white"), 
        axis.text.y = element_text(color = "white"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white"), 
        plot.title = element_text(color = "white")) +
  labs(
    x = "Tracks", 
    y = "No of Laps", 
    caption = "Tidy Tuesday | Marble Racing | @arnabp123"
  ) + 
  ggtitle("Track - Laps")

p_track_laps


# Main Race - Team Performances

p_main_race_team_performances <- tbl_marbles_main_races %>% group_by(site, race) %>% 
  mutate(team_rank = dense_rank(desc(points))) %>% ungroup() %>% 
  select(site, race, team_name, team_rank) %>%
  ggplot() + 
  geom_boxplot(mapping = aes(x = reorder(team_name, desc(team_rank), FUN = median), 
                             y = team_rank, 
                             fill = team_name), 
               show.legend = FALSE, 
               color = "white") + 
  coord_flip() + 
  theme_solarized_2(light = FALSE) + 
  theme(axis.text.x = element_text(color = "white"), 
        axis.text.y = element_text(color = "white"),
        axis.title.x = element_text(color = "white"),
        axis.title.y = element_text(color = "white"), 
        plot.title = element_text(color = "white")) +
  labs(
    x = "Team Name", 
    y = "Team Rank", 
    caption = "Tidy Tuesday | Marble Racing | @arnabp123"
  ) + 
  ggtitle("Main Race - Team Performances")

p_main_race_team_performances


# Qualifiers and Main Race Comparisons

tbl_teams_performances <- union(
  tbl_marbles_qualifiers %>% group_by(site, race) %>% 
  mutate(team_rank = dense_rank(avg_time_lap, 
                                na.last = TRUE, 
                                ties.method = "min")) %>%
  ungroup() %>% 
  select(team_name, team_rank, race_type) %>% 
    group_by(team_name, race_type) %>% summarise(avg_rank = mean(team_rank, na.rm = TRUE)) %>% 
    ungroup(), 
  tbl_marbles_main_races %>% group_by(site, race) %>% 
    mutate(team_rank = dense_rank(desc(points))) %>% ungroup() %>% 
    select(team_name, team_rank, race_type) %>% 
    group_by(team_name, race_type) %>% summarise(avg_rank = mean(team_rank, na.rm = TRUE)) %>% 
    ungroup())



p_pyramid_chart <- ggcharts::pyramid_chart(data = tbl_teams_performances, 
                        x = team_name, 
                        y = avg_rank, 
                        group = race_type, 
                        sort = "ascending", 
                        xlab = "Mean Team Rank",
                        title = "Team Performances - Mean Ranks", 
                        bar_colors = c("green", "orange")) + 
  labs(
    caption = "Tidy Tuesday | Marble Racing | @arnabp123"
  ) 

  
p_pyramid_chart









