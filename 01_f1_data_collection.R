######################################################## FORMULA 1 RACE SIMULATION #######################################################  
##################################################### 01. DATA COLLECTION & PREPARATION ##################################################

library(tidyverse)
library(dplyr)
library(rvest)
library(ggplot2)

# Link to DraftKings F1 betting odds (Odds for upcoming race - Austria GP, June 30, 2024)
dk <- "https://sportsbook.draftkings.com/leagues/motorsports/formula-1"
webpage <- read_html(dk)

# Scrape driver names 
drivers <- webpage %>%
  html_nodes(".side-rail-name") %>% 
  html_text(trim = TRUE) %>%
  unique() 

# Scrape outright race winner odds 
win_odds <- webpage %>%
  html_nodes(xpath = "//p[text()='Race Winner']/following-sibling::ul[@class='outcomes']") %>%
  html_nodes("li") %>%
  html_text(trim = TRUE)

# Create a dataframe of drivers and corresponding odds 
df <- data.frame(drivers[1:20], win_odds) %>%
  rename("driver" = drivers.1.20.) %>%
  mutate(win_odds = as.numeric(gsub("\\−","-",win_odds)),
         win_odds = as.numeric(gsub("\\+", "", win_odds)))

head(df)

# Read in driver and team data, originally retrieved from ESPN 
teams <- read.csv("/Users/lauren.manis/Desktop/f1_drivers_teams.csv") %>%
  rename("driver" = "NAME", "team" = "TEAM") %>%
  select(driver, team)
  
# Join teams back to drivers 
df <- df %>% 
  left_join(teams, by="driver") 

# Functions to convert american to decimal odds 
convert_odds <- function(odds) {
  if (odds > 0) {
    return(1 + (odds / 100))
  } else if (odds < 0) {
    return(1 - (100 / odds))
  } else {
    return(NA) 
  }
}

# Test the function 
print(convert_odds(200)) 
print(convert_odds(-200)) 

# Compute the implied probability of each driver winning the race based on outright odds 
df <- df %>%
  mutate(decimal_odds = sapply(win_odds, convert_odds),
         implied_prob = 1 / decimal_odds)

# Compute sportsbook overround / vig applied
ovr <- sum(df$implied_prob)
ovr

# Compute true probabilities, under the simpligying assumption that vig is distributed equally across selections 
df <- df %>%
  mutate(true_prob = implied_prob / ovr)

# Ensure true probs sum to 1 
sum(df$true_prob)

# For later use, create a dataframe of finishing positions and corresponding point values 
pos <- seq(1,20,1)
pts <- c(25, 18, 15, 12, 10, 8, 6, 4, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

points <- data.frame(pos, pts)

view(df)
