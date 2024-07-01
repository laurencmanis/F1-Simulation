######################################################## FORMULA 1 RACE SIMULATION #####################################################  
####################################################### 04. BETTING ODDS CONVERSIONS ###################################################
# Converting probabilities to fair odds (no-vig), ie, pricing additional/new markets 

# Function to convert probabilities to American odds 
convert_prob_to_odds <- function(prob) {
  if (prob == 0) {
    return(1000000) 
  }
  decimal_odds <- 1 / prob
  if (decimal_odds >= 2.0) {
    return((decimal_odds - 1) * 100)
  } else {
    return(-100 / (decimal_odds - 1))
  }
}

# Apply the function to probabilities 

# 1. Top 3 Finish, Top 5 Finish, Top 10 Finish
top_n <- top_n %>% 
  mutate(
    top_3_odds = sapply(top_3, convert_prob_to_odds),
    top_5_odds = sapply(top_5, convert_prob_to_odds),
    top_10_odds = sapply(top_10, convert_prob_to_odds))

head(top_n)
view(top_n)

# 2. Driver Head to Heads
teammate_probs <- teammate_probs %>%
  mutate(odds = sapply(probability, convert_prob_to_odds))

view(teammate_probs)

view(prob_df)

# 3. Driver Points Scored
driver_pts <- driver_pts %>% 
  mutate(
    over_odds = sapply(prob_over, convert_prob_to_odds),
    under_odds = sapply(prob_under, convert_prob_to_odds))

view(driver_pts)

# 4. Team Total Points
team_exp_pts <- team_exp_pts %>%
  mutate(
    over_odds = sapply(prob_over, convert_prob_to_odds),
    under_odds = sapply(prob_under, convert_prob_to_odds))

team_exp_drivers <- team_exp_drivers %>%
  mutate(
    over_odds_dr = sapply(prob_over, convert_prob_to_odds),
    under_odds_dr = sapply(prob_under, convert_prob_to_odds))

team_max_points_count <- team_max_points_count %>%
  mutate(
    most_points_odds = sapply(most_points_prob, convert_prob_to_odds))

team_props <- team_exp_pts %>%
  left_join(team_exp_drivers, by="team") %>%
  left_join(team_max_points_count, by="team")

view(team_props)

# 5. Podium Props/Exactas
podium_df <- podium_df %>%
  mutate(
    odds = sapply(proportion, convert_prob_to_odds))

view(podium_df)

podium_exact <- podium_exact %>%
  mutate(
    odds = sapply(proportion, convert_prob_to_odds))

view(podium_exact)


