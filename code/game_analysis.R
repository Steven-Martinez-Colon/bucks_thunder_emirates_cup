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
                 "jpeg", "grid", "gridExtra", "RCurl"))

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

# Creating a column for two-point field goals
df <- df %>%
  mutate(fg2 = ifelse(fg == 1 & fg3 == 0, 1, 0))

# Changing binary variables to factors
df <- df %>%
  mutate(across(where(~ is.numeric(.) && all(na.omit(.) %in% c(0, 1))),
                as.factor))

################## EDA ####################

# Summary of Bucks team shots
df %>% 
  filter(team_nba_off == "MIL") %>% 
  summary(df)

# Summary of Thunder team shots
df %>% 
  filter(team_nba_off == "OKC") %>% 
  summary(df)

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
  group_by(fg, team_nba_off) %>% 
  summarise(
    avg_shooter_speed = mean(shooterSpeed, na.rm = TRUE)
  )


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
        group_by(team_nba_off, contestLevel) %>% 
        summarise(
          count = n(),
          fg2 = sum(fg2),
          fga3 = sum(fga3),
          fg3 = sum(fg3),
          avg_defender_dist = mean(closestDefDist),
          shooterSpeed = mean(shooterSpeed)
        ), n = 40)

######################## Graphs ##############################

# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------


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
  labs(title = "Three Point FGs Made Shot Chart with Passes by Team",
       subtitle = "Shooter (blue), Passer (green), Pass (gray)",
       x = "Court X", y = "Court Y") +
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




































