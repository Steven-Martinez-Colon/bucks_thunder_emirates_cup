# Edited by Steven Martinez

############## Loading Libraries Function ####################

load_libraries <- function(packages) {
  # Check for missing packages
  missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  # install missing packages
  if(length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
  
  # Load all packages
  lapply(packages, library, character.only = TRUE)
  
  cat("All packages are loaded succesfully.\n")
}


# Loading necessary libraries
load_libraries(c("tidyverse", "lubridate", "stats", "ggplot2", "corrplot", "stringr", "stringi", "class",
                 "tidymodels", "modeldata", "themis", "vip", "baguette", "janitor", "rvest",
                 "yardstick", "gsheet", "caret", "randomForest", "here", "tibble", "dplyr", "ISAR",
                 "tidyr", "mgcv", "teamcolors", "baseballr", "Lahman", "remotes", "ggcorrplot", "broom", "readr",
                 "glmnet", "xgboost", "Matrix", "Metrics", "reshape2", "DMwR2", "smotefamily", "highcharter",
                 "jpeg", "grid", "gridExtra", "RCurl", "gt", "purrr"))

# Load only the necessary functions from 'car'
library(car, exclude = "select")

# Turning off warning messages
options(warnings = 0)

############################## Loading Dataset ####################################

# Loading Dataset
df <- read_csv("data/data_mil_okc_121724_shots.csv")

################### Data Cleaning ############################################

# Looking at the structure of the dataset
str(df)

# Changing some variables that are in character form into numeric
df <- df %>%
  mutate(across(c(loc_x_passer, loc_y_passer, rimDepth, rimLeftRight, arcAngle), as.numeric))


# Changing binary variables to factors
df <- df %>%
  mutate(across(where(~ is.numeric(.) && all(na.omit(.) %in% c(0, 1))),
                as.factor))

# Creating a column for 2-point makes and attempts
df <- df %>%
  mutate(
    fga2 = ifelse(fga == 1 & fga3 == 0, 1, 0),
    fg2 = ifelse(fg == 1 & fg3 == 0, 1, 0)
    )

########################### Image for Shot Charts ################################

# Load and prepare a half-court image as a background for shot charts.
# The image is sourced from the specified URL and converted into a raster graphic
# object using `rasterGrob`, which can then be added to a ggplot using 
# `annotation_custom()`.
#
# This background image replaces the need to manually draw court lines,
# providing a visually accurate and professional court representation.

# half court image
courtImg.URL <- "https://thedatagame.com.au/wp-content/uploads/2016/03/nba_court.jpg"
court <- rasterGrob(
  readJPEG(getURLContent(courtImg.URL)),
  width=unit(1,"npc"),
  height=unit(1,"npc")
)

################## Player Performance EDA ####################

# Ensure key binary shot variables are numeric
df <- df %>%
  mutate(
    fga = as.numeric(as.character(fga)),
    fg = as.numeric(as.character(fg)),
    fga3 = as.numeric(as.character(fga3)),
    fg3 = as.numeric(as.character(fg3)),
    fg2 = as.numeric(as.character(fg2)),
    fga2 = as.numeric(as.character(fga2))
  )

# Player-level shooting summary
player_summary <- df %>% 
  group_by(player_nba_shooter, team_nba_off) %>% 
  summarise(
    
    FGA = sum(fga, na.rm = TRUE),
    FGM = sum(fg, na.rm = TRUE),
    FG_percent = round(FGM / FGA, 3),
    
    FG2A = sum(fga2, na.rm = TRUE),
    FG2M = sum(fg2, na.rm = TRUE),
    FG2_percent = round(FG2M / FG2A, 3),
    
    FG3A = sum(fga3, na.rm = TRUE),
    FG3M = sum(fg3, na.rm = TRUE),
    FG3_percent = round(FG3M / FG3A, 3),
    
    eFG_percent = round((FGM + 0.5 * FG3M) / FGA, 3),
    avg_qSQ = round(mean(qSQ, na.rm = TRUE), 3),
    
    Points = FG2M * 2 + FG3M * 3
  ) %>%
  arrange(desc(FGA))

# Note: The points calculated for each player are by made field goals only.
# It's not a representation of total points for the game since it is missing free throws made.

# View Table
print(player_summary)


# Basic FGA vs FG% bar plot (color by team)
ggplot(player_summary, aes(x = reorder(player_nba_shooter, FGA), y = FGA, fill = team_nba_off)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Field Goal Attempts by Player",
    x = "Player",
    y = "FGA"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("MIL" = "#00471B", "OKC" = "#007AC1"))  # Bucks green, Thunder blue


# players by FGA for display
player_summary <- player_summary %>%
  mutate(player_nba_shooter = reorder(player_nba_shooter, FGA))

# Define color by team
bar_colors <- ifelse(player_summary$team_nba_off == "MIL", "#00471B", "#007AC1")

# Create highchart
highchart() %>%
  hc_chart(type = "bar") %>%
  hc_title(text = "Field Goal Attempts by Player") %>%
  hc_xAxis(
    categories = player_summary$player_nba_shooter,
    title = list(text = "Player")
  ) %>%
  hc_yAxis(
    title = list(text = "Field Goal Attempts")
  ) %>%
  hc_plotOptions(
    bar = list(dataLabels = list(enabled = TRUE))
  ) %>%
  hc_tooltip(
    pointFormat = paste(
      "<b>Team:</b> {point.custom.team}<br>",
      "<b>FG%:</b> {point.custom.fg_percent}<br>",
      "<b>FG3%:</b> {point.custom.fg3_percent}<br>",
      "<b>eFG%:</b> {point.custom.efg_percent}<br>",
      "<b>Points:</b> {point.custom.points}<br>"
    ),
    useHTML = TRUE
  ) %>%
  hc_add_series(
    data = lapply(1:nrow(player_summary), function(i) {
      list(
        y = player_summary$FGA[i],
        color = bar_colors[i],
        custom = list(
          team = player_summary$team_nba_off[i],
          fg_percent = player_summary$FG_percent[i],
          fg3_percent = player_summary$FG3_percent[i],
          efg_percent = player_summary$eFG_percent[i],
          points = player_summary$Points[i]
        )
      )
    }),
    type = "bar",
    showInLegend = FALSE
  )

#_________________ Star Player Performance ________________________

# Manually define your key players (adjust names if needed)
stars <- c("Antetokounmpo, Giannis", "Lillard, Damian",
           "Gilgeous-Alexander, Shai", "Williams, Jalen")

# Filter and prepare table
star_table <- df %>%
  filter(player_nba_shooter %in% stars) %>%
  group_by(player_nba_shooter, team_nba_off) %>%
  summarise(
    FGA = sum(fga, na.rm = TRUE),
    FGM = sum(fg, na.rm = TRUE),
    FG_percent = round(FGM / FGA, 3),
    FG3A = sum(fga3, na.rm = TRUE),
    FG3M = sum(fg3, na.rm = TRUE),
    FG3_percent = round(FG3M / FG3A, 3),
    eFG_percent = round((FGM + 0.5 * FG3M) / FGA, 3),
    Points = sum(fg2 * 2 + fg3 * 3, na.rm = TRUE),
    avg_qSQ = round(mean(qSQ, na.rm = TRUE), 3)
  ) %>%
  ungroup() %>%
  rename(
    Player = player_nba_shooter,
    Team = team_nba_off,
    `FG%` = FG_percent,
    `3PA` = FG3A,
    `3PM` = FG3M,
    `3P%` = FG3_percent,
    `eFG%` = eFG_percent,
    `qSQ` = avg_qSQ
  ) %>%
  arrange(desc(Points))  # or sort however you'd like

# Display table using gt
star_table %>%
  gt() %>%
  tab_header(
    title = md("**Star Player Comparison**"),
    subtitle = "Shot performance metrics for Bucks and Thunder primary scorers"
  ) %>%
  fmt_percent(columns = c(`FG%`, `3P%`, `eFG%`), decimals = 1) %>%  # ← no qSQ here
  fmt_number(columns = c(`qSQ`), decimals = 3) %>%  # ← format qSQ as a decimal
  fmt_number(columns = c(FGA, FGM, `3PA`, Points), decimals = 0) %>%
  cols_align(align = "center") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Player)
  ) %>% 
  tab_source_note(
    source_note = md("*Point totals reflect only field goals made; free throws are not included.*")
  )

# ______________________ Supporting Players Performance _______________________

# Supporting cast = everyone with 5+ FGA and not a star
supporting_players <- df %>%
  filter(!player_nba_shooter %in% c("Antetokounmpo, Giannis", "Lillard, Damian", 
                                    "Gilgeous-Alexander, Shai", "Williams, Jalen")) %>%
  group_by(player_nba_shooter, team_nba_off) %>%
  summarise(
    FGA = sum(fga, na.rm = TRUE),
    FGM = sum(fg, na.rm = TRUE),
    FG_percent = round(FGM / FGA, 3),
    FG3A = sum(fga3, na.rm = TRUE),
    FG3M = sum(fg3, na.rm = TRUE),
    FG3_percent = round(FG3M / FG3A, 3),
    eFG_percent = round((FGM + 0.5 * FG3M) / FGA, 3),
    Points = sum(fg2 * 2 + fg3 * 3, na.rm = TRUE),
    avg_qSQ = round(mean(qSQ, na.rm = TRUE), 3)
  ) %>%
  filter(FGA >= 5) %>%
  ungroup() %>%
  rename(
    Player = player_nba_shooter,
    Team = team_nba_off,
    `FG%` = FG_percent,
    `3PA` = FG3A,
    `3PM` = FG3M,
    `3P%` = FG3_percent,
    `eFG%` = eFG_percent,
    `qSQ` = avg_qSQ
  ) %>%
  arrange(desc(Points))


supporting_players %>%
  gt() %>%
  tab_header(
    title = md("**Supporting Cast Performance**"),
    subtitle = "Non-star players with 5+ FGA / Points "
  ) %>%
  fmt_percent(columns = c(`FG%`, `3P%`, `eFG%`), decimals = 1) %>%
  fmt_number(columns = c(`qSQ`), decimals = 3) %>%
  fmt_number(columns = c(FGA, FGM, `3PA`, Points), decimals = 0) %>%
  cols_align(align = "center") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Player)
  ) %>% 
  tab_source_note(
    source_note = md("*Point totals reflect only field goals made; free throws are not included.*")
  )

# _________________________ Player Defensive Summary ______________________________-

# Player Defensive Summary
defender_summary <- df %>%
  filter(fga == 1, !is.na(player_nba_closestDef), !is.na(fg)) %>%
  group_by(Defender = player_nba_closestDef, Team = team_nba_def) %>%
  summarise(
    Shots_Defended = n(),
    FGM_Against = sum(fg, na.rm = TRUE),
    FG_Percent_Against = round(FGM_Against / Shots_Defended, 3),
    Avg_Def_Dist = round(mean(closestDefDist, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(Shots_Defended))

# Player defensive summary table
defender_summary %>%
  gt() %>%
  tab_header(
    title = md("**Shooting Efficiency Against Closest Defenders**"),
    subtitle = "Defender-level impact on opponent shooting"
  ) %>%
  fmt_percent(columns = FG_Percent_Against, decimals = 1) %>%
  fmt_number(columns = c(Shots_Defended, FGM_Against), decimals = 0) %>%
  cols_label(
    FG_Percent_Against = "FG% Allowed",
    Avg_Def_Dist = "Avg Distance"
  ) %>%
  cols_align(align = "center")


####################### Team Performance EDA #####################################

# Calculating field goal average by each team
fg_avg <- df %>%
  group_by(team_nba_off) %>%
  summarise(
    makes = sum(fg, na.rm = TRUE),
    attempts = sum(fga, na.rm = TRUE),
    fg_pct = makes / attempts
  )

# Looking at the the field goal average
print(fg_avg)

# Calculating three-point field goal average by each team
fg3_avg <- df %>% 
  group_by(team_nba_off) %>% 
  summarise(
    makes = sum(fg3, na.rm = TRUE),
    attempts = sum(fga3, na.rm = TRUE),
    fg3_pct = makes / attempts
  )

# Looking at the three point average by each team
print(fg3_avg)

# Shot Density Heatmap by Team
df %>%
  ggplot(aes(x = loc_x, y = loc_y)) +
  annotation_custom(court, xmin = -25, xmax = 25, ymin = 0, ymax = 47) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.6) +
  facet_wrap(~ team_nba_off) +
  scale_fill_viridis_c() +
  coord_fixed() +
  labs(title = "Shot Density Heatmap by Team") +
  theme_minimal()

# Shot chart for 3 pointers made with passers by each team
df %>% 
  filter(fg3 == 1) %>% 
  ggplot() +
  
  # Add court background
  annotation_custom(court, xmin = -25, xmax = 25, ymin = 0, ymax = 47) +
  
  # Shooter positions
  geom_point(aes(x = loc_x, y = loc_y), color = "blue", size = 2, alpha = 0.6) +
  
  # Passer positions
  geom_point(aes(x = loc_x_passer, y = loc_y_passer), color = "green", size = 2, alpha = 0.6) +
  
  # Pass lines
  geom_segment(aes(x = loc_x_passer, y = loc_y_passer, 
                   xend = loc_x, yend = loc_y),
               arrow = arrow(length = unit(0.12, "inches")),
               color = "gray30", alpha = 0.4) +
  
  # Facet by team
  facet_wrap(~ team_nba_off) +
  
  coord_fixed() +
  labs(title = "Three Point FGs Made Shot Chart with Passes by Team",
       subtitle = "Shooter (blue), Passer (green), Pass (gray)",
       x = "Court X", y = "Court Y") +
  theme_minimal()


# Shot chart for 3 pointers made by each team
df %>% 
  filter(fg3 == 1) %>% 
  ggplot() +
  
  # Add court background
  annotation_custom(court, xmin = -25, xmax = 25, ymin = 0, ymax = 47) +
  
  # Shooter positions
  geom_point(aes(x = loc_x, y = loc_y), color = "blue", size = 2, alpha = 0.6) +
  
  # Facet by team
  facet_wrap(~ team_nba_off) +
  
  coord_fixed() +
  labs(title = "Three Point FGs Made By Each Team",
       x = "Court X", y = "Court Y") +
  theme_minimal()

# Three pointers summary for each team
three_point_summary <- df %>%
  group_by(team_nba_off) %>%
  summarise(
    `3PA` = sum(fga3, na.rm = TRUE),
    `3PM` = sum(fg3, na.rm = TRUE),
    `3P%` = round(`3PM` / `3PA`, 3),
    Total_Shots = n(),
    `3PT Share` = round(`3PA` / Total_Shots, 3)
  ) %>%
  rename(Team = team_nba_off)

# Table showing three point summary
three_point_summary %>%
  gt() %>%
  tab_header(
    title = md("**Team Three-Point Shooting Summary**"),
    subtitle = "Based on made and attempted 3PT field goals"
  ) %>%
  fmt_percent(columns = c(`3P%`, `3PT Share`), decimals = 1) %>%
  fmt_number(columns = c(`3PA`, `3PM`, Total_Shots), decimals = 0) %>%
  cols_align(align = "center")


# Summary of assists and catch and shoot attempts
assist_cas_summary <- df %>%
  group_by(team_nba_off) %>%
  summarise(
    FGA = sum(fga, na.rm = TRUE),
    Made_FG = sum(fg, na.rm = TRUE),
    Assisted_Makes = sum(fg == 1 & assisted == 1, na.rm = TRUE),
    CatchShoot_Attempts = sum(catchAndShoot == 1, na.rm = TRUE),
    
    Assisted_Shot_Pct = round(Assisted_Makes / Made_FG, 3),
    CatchShoot_Rate = round(CatchShoot_Attempts / FGA, 3)
  ) %>%
  rename(Team = team_nba_off)


# Table showing assists vs catch and shoot
assist_cas_summary %>%
  gt() %>%
  tab_header(
    title = md("**Assisted Shots and Catch-and-Shoot Rate**"),
    subtitle = "Team-level comparison"
  ) %>%
  fmt_percent(columns = c(`Assisted_Shot_Pct`, `CatchShoot_Rate`), decimals = 1) %>%
  fmt_number(columns = c(FGA, Made_FG, CatchShoot_Attempts), decimals = 0) %>%
  cols_label(
    Assisted_Shot_Pct = "Assisted FG%",
    CatchShoot_Rate = "Catch-and-Shoot FGA%"
  ) %>%
  cols_align(align = "center")


# Looking at shooter speed for each team and their field goals made and missed
df %>% 
  group_by(fg, team_nba_off) %>% 
  summarise(
    avg_shooter_speed = mean(shooterSpeed, na.rm = TRUE)
  )

# Shooter speed summary
speed_summary <- df %>%
  filter(fga == 1, !is.na(shooterSpeed)) %>%
  mutate(Shot_Result = ifelse(fg == 1, "Made", "Missed")) %>%
  group_by(Team = team_nba_off, Shot_Result) %>%
  summarise(
    avg_speed = round(mean(shooterSpeed, na.rm = TRUE), 2),
    shot_count = n(),
    .groups = "drop"
  )


# Bar plot of shooter speed and shot result
highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Average Shooter Speed by Team and Shot Result") %>%
  hc_xAxis(categories = unique(speed_summary$Team)) %>%
  hc_yAxis(title = list(text = "Avg. Shooter Speed (ft/s)")) %>%
  hc_plotOptions(
    column = list(
      dataLabels = list(enabled = TRUE),
      grouping = TRUE,
      pointPadding = 0.2,
      borderWidth = 0
    )
  ) %>%
  hc_tooltip(
    useHTML = TRUE,
    headerFormat = "<b>{point.name}</b><br>",
    pointFormat = paste(
      "Result: {series.name}<br>",
      "Avg Speed: {point.y} ft/s<br>",
      "Shots: {point.custom.shots}"
    )
  ) %>%
  hc_add_series(
    name = "Made",
    data = pmap(
      speed_summary %>% filter(Shot_Result == "Made"),
      function(Team, Shot_Result, avg_speed, shot_count) {
        list(
          y = avg_speed,
          name = Team,
          custom = list(shots = shot_count)
        )
      }
    ),
    color = "#2ECC71"
  ) %>%
  hc_add_series(
    name = "Missed",
    data = pmap(
      speed_summary %>% filter(Shot_Result == "Missed"),
      function(Team, Shot_Result, avg_speed, shot_count) {
        list(
          y = avg_speed,
          name = Team,
          custom = list(shots = shot_count)
        )
      }
    ),
    color = "#E74C3C"
  )

# Looking at team defense
print(df %>% 
        group_by(team_nba_def, contestLevel) %>% 
        summarise(
          count = n(),
          `fg%` = sum(fg) / sum(fga),
          fg2 = sum(fg2),
          fga3 = sum(fga3),
          fg3 = sum(fg3),
          avg_defender_dist = mean(closestDefDist),
          shooterSpeed = mean(shooterSpeed)
        ), n = 40)

# Creating a table for the team defense
df %>%
  group_by(team_nba_def, contestLevel) %>%
  summarise(
    count = n(),
    `fg%` = sum(fg) / sum(fga),
    fg2 = sum(fg2),
    fga3 = sum(fga3),
    fg3 = sum(fg3),
    avg_defender_dist = mean(closestDefDist),
    shooterSpeed = mean(shooterSpeed),
    .groups = "drop"
  ) %>%
  rename(`Team on Defense` = team_nba_def) %>%
  gt() %>%
  tab_header(
    title = md("**Defensive Impact by Contest Level**"),
    subtitle = "FG%, contest distance, and shooter speed vs. each defense"
  ) %>%
  fmt_percent(columns = `fg%`, decimals = 1) %>%
  fmt_number(columns = c(avg_defender_dist, shooterSpeed), decimals = 2) %>%
  cols_align(align = "center")







######################### Miscellaneous EDA - Not used in the report #######################################################

# Summary of Bucks team shots
df %>% 
  filter(team_nba_off == "MIL") %>% 
  summary(df)

# Summary of Thunder team shots
df %>% 
  filter(team_nba_off == "OKC") %>% 
  summary(df)

df %>% 
  group_by(region) %>% 
  filter(player_nba_shooter == "Portis, Bobby") %>% 
  summarise(
    distance = mean(distance)
  )

df %>% 
  filter(player_nba_shooter == "Portis, Bobby") %>% 
  count(region)

df %>% 
  group_by(region) %>% 
  filter(player_nba_shooter == "Lopez, Brook") %>% 
  summarise(
    distance = mean(distance)
  )

df %>% 
  filter(player_nba_shooter == "Lopez, Brook") %>% 
  count(region)

print(df %>% 
  group_by(team_nba_off) %>% 
  count(complexShotType),
  n = 40
  )



# Table of contested level for Bucks
df %>%
  filter(team_nba_off == "MIL") %>%
  group_by(contestLevel) %>% 
  summarise(
    count = n(),
    threes_taken = sum(fga3 == 1, na.rm = TRUE),
    made_threes = sum(fg3 == 1, na.rm = TRUE),
    mean_closestDefDist = mean(closestDefDist, na.rm = TRUE),
    .groups = "drop"
  )

# Table of contested level for Thunder
df %>%
  filter(team_nba_off == "OKC") %>%
  group_by(contestLevel) %>% 
  summarise(
    count = n(),
    threes_taken = sum(fga3 == 1, na.rm = TRUE),
    made_threes = sum(fg3 == 1, na.rm = TRUE),
    mean_closestDefDist = mean(closestDefDist, na.rm = TRUE),
    .groups = "drop"
  )


# Select only numeric columns and compute correlation
cor_matrix <- cor(select_if(df, is.numeric), use = "pairwise.complete.obs")


# View top correlations
print(cor_matrix)


df %>% 
  group_by(team_nba_off) %>% 
  summarise(
    shotClock = mean(shotClock, na.rm = TRUE)
  )

df %>% 
  filter(team_nba_off == "MIL") %>% 
  count(region)


df %>% 
  filter(team_nba_off == "OKC") %>% 
  count(region)


df %>%
  group_by(team_nba_off) %>%
  summarise(
    min = min(shooterVelAngle, na.rm = TRUE),
    q1 = quantile(shooterVelAngle, 0.25, na.rm = TRUE),
    median = median(shooterVelAngle, na.rm = TRUE),
    q3 = quantile(shooterVelAngle, 0.75, na.rm = TRUE),
    max = max(shooterVelAngle, na.rm = TRUE),
    mean = mean(shooterVelAngle, na.rm = TRUE),
    .groups = "drop"
  )

df %>% 
  group_by(team_nba_off, shotType) %>% 
  summarise(
    min = min(shooterSpeed, na.rm = TRUE),
    q1 = quantile(shooterSpeed, 0.25, na.rm = TRUE),
    median = median(shooterSpeed, na.rm = TRUE),
    q3 = quantile(shooterSpeed, 0.75, na.rm = TRUE),
    max = max(shooterSpeed, na.rm = TRUE),
    mean = mean(shooterSpeed, na.rm = TRUE)
  )

df %>% 
  group_by(team_nba_off) %>% 
  count(shotType)

complex_shot_type <- df %>% 
  group_by(team_nba_off, complexShotType) %>% 
  summarise(
    count = n(),
    min = min(shooterSpeed, na.rm = TRUE),
    q1 = quantile(shooterSpeed, 0.25, na.rm = TRUE),
    median = median(shooterSpeed, na.rm = TRUE),
    q3 = quantile(shooterSpeed, 0.75, na.rm = TRUE),
    max = max(shooterSpeed, na.rm = TRUE),
    mean = mean(shooterSpeed, na.rm = TRUE),
    .groups = "drop"
  )
  #count(complexShotType)

print(complex_shot_type, n = 40)


print(df %>% 
  group_by(team_nba_off, complexShotType) %>% 
  summarise(
    count = n(),
    fg2 = sum(fg2),
    fga3 = sum(fga3),
    fg3 = sum(fg3),
    avg_defender_dist = mean(closestDefDist),
    shooterSpeed = mean(shooterSpeed)
  ), n = 40)



print(df %>% 
        group_by(team_nba_def, contestLevel) %>% 
        summarise(
          count = n(),
          `fg%` = sum(fg) / sum(fga),
          fg2 = sum(fg2),
          fga3 = sum(fga3),
          fg3 = sum(fg3),
          avg_defender_dist = mean(closestDefDist),
          shooterSpeed = mean(shooterSpeed)
        ), n = 40)


df %>%
  group_by(team_nba_def, contestLevel) %>%
  summarise(
    count = n(),
    `fg%` = sum(fg) / sum(fga),
    fg2 = sum(fg2),
    fga3 = sum(fga3),
    fg3 = sum(fg3),
    avg_defender_dist = mean(closestDefDist),
    shooterSpeed = mean(shooterSpeed),
    .groups = "drop"
  ) %>%
  rename(`Team on Defense` = team_nba_def) %>%
  gt() %>%
  tab_header(
    title = md("**Defensive Impact by Contest Level**"),
    subtitle = "FG%, contest distance, and shooter speed vs. each defense"
  ) %>%
  fmt_percent(columns = `fg%`, decimals = 1) %>%
  fmt_number(columns = c(avg_defender_dist, shooterSpeed), decimals = 2) %>%
  cols_align(align = "center")


######################## Graphs ##############################

# Histogram distribution of 
ggplot(df, aes(x = qSQ, color = team_nba_off, fill = team_nba_off)) +
  geom_histogram(binwidth = 10, alpha = 0.7) +
  labs(title = "Probability a Shot goes In", x = "Probability a Shot Goes In (qSQ)", y = "Counts") +
  theme_bw()

ggplot(df, aes(x = closestDefDist, color = team_nba_off, fill = team_nba_off)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Probability a Shot goes In", x = "Probability a Shot Goes In (qSQ)", y = "Counts") +
  theme_bw()

# Boxplot of shot angle by each team
ggplot(df, aes(x = shotAngle, color = team_nba_off, fill = team_nba_off)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Shot Angle by each Team", x = "Shot Angle") +
  theme_bw()

# Boxplot of shooter speed by each team
ggplot(df, aes(x = shooterSpeed, color = team_nba_off, fill = team_nba_off)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Shooter speed by each Team", x = "Shooter Speed") +
  theme_bw()

ggplot(df, aes(x = closestDefDist, y = qSQ, color = team_nba_off)) +
  geom_point() +
  theme_bw()


# Shot chart for 3 pointers made in the game by each team
df %>% 
  filter(fg3 == 1) %>% 
  ggplot() +
  
  # Add court background
  annotation_custom(court, xmin = -25, xmax = 25, ymin = 0, ymax = 47) +
  
  # Shooter positions
  geom_point(aes(x = loc_x, y = loc_y), color = "blue", size = 2, alpha = 0.6) +
  
  # Facet by team
  facet_wrap(~ team_nba_off) +
  
  coord_fixed() +
  labs(title = "Three Point FGs Made By Each Team",
       x = "Court X", y = "Court Y") +
  theme_minimal()









































































