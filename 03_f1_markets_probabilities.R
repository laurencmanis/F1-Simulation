######################################################## FORMULA 1 RACE SIMULATION #####################################################  
##################################################### 03. BETTING MARKETS & PRICING #####################################################

head(sims_df)
head(sims)

##########################################  1. Top 3 Finish, Top 5 Finish, Top 10 Finish ##########################################    
### Or in F1 Terms, Podium Finish, To Score 10+ Points, To Score Points 

# Compute the probability of each driver finishing in the top 3 (on the podium), top 5, and top 10 (in the points) of the race 
top_n <- sims_df %>%
  group_by(driver) %>%
  summarise(
    top_3 = sum(if_else(finish < 4, 1, 0)/num_simulations),
    top_5 = sum(if_else(finish < 6, 1, 0)/num_simulations),
    top_10 = sum(if_else(finish < 11, 1, 0)/num_simulations
    ))

view(top_n)

# Sense Checks
# Ensure no driver has a higher chance of finishing in the top 3 than the top 5, etc.
top_n %>% filter(top_3 > top_5)
top_n %>% filter(top_5 > top_10)

# Ensure no driver with 0% probability of a top 10 finish has a top 5 finishing probability >0% 
top_n %>% filter(top_10 == 0 & top_5 > 0)
top_n %>% filter(top_5 == 0 & top_3 > 0)

##########################################  2. Driver Head to Heads ##########################################
### Driver A to Place Higher than Driver B, Driver to Place Higher than their Teammate 

# List of drivers
drivers <- unique(sims_df$driver)

# Initialize an empty matrix to store probabilities
prob_matrix <- matrix(0, nrow = length(drivers), ncol = length(drivers))
colnames(prob_matrix) <- drivers
rownames(prob_matrix) <- drivers

# Loop through each pair of drivers
for (i in 1:length(drivers)) {
  for (j in 1:length(drivers)) {
    if (i != j) {
      driver_a <- drivers[i]
      driver_b <- drivers[j]
      
      # Select outcomes for drivers a and b only
      driver_sims <- sims[, c(driver_a, driver_b)]
      
      # Compute the probability of driver a placing higher than driver b
      prob_matrix[i, j] <- sum(if_else(driver_sims[,1] < driver_sims[,2], 1, 0)) / num_simulations
    }}
}

# Convert the matrix to a dataframe
prob_df <- as.data.frame(prob_matrix)

# Rows = Driver A, Cols = Driver B --> Values = Prob. of Driver A beating Driver B (prob. of driver in row name beating driver in col name)
view(prob_df)

# Testing with 2 drivers 
driver_a <- 'Max Verstappen'
driver_b <- 'Daniel Ricciardo'

# Return probability of driver_a beating driver_b -- Makes sense from an F1 perspective 
prob_df[driver_a, driver_b]

# Create a team lookup from the original sims data frame
team_lookup <- df %>%
  select(driver, team) %>%
  distinct()

# Initialize an empty dataframe to store results
teammate_probs <- data.frame(driver_a = character(),driver_b = character(), probability = numeric())

# Loop through each pair of teammates 
for (i in 1:length(drivers)) {
  for (j in 1:length(drivers)) {
    if (i != j) {
      driver_a <- drivers[i]
      driver_b <- drivers[j]
      
      # Check if both drivers are from the same team
      team_a <- team_lookup %>% filter(driver == driver_a) %>% pull(team)
      team_b <- team_lookup %>% filter(driver == driver_b) %>% pull(team)
      
      if (team_a == team_b) {
        prob <- prob_df[driver_a, driver_b]
        teammate_probs <- rbind(teammate_probs, data.frame(driver_a, driver_b, probability = prob))
      }}}
}

# Probability of driver_a beating driver_b - teammates 
teammate_probs

##########################################  3. Driver Points Scored ##########################################
### Driver - Points, OU, X+

# Join point values to race positions 
sims_df <- sims_df %>%
  rename("pos"=finish) %>%
  left_join(points, by="pos")

# Compute mean points scored per driver, as most likely points to score in the race 
driver_pts <- sims_df %>%
  group_by(driver) %>%
  summarise(mean_pts = mean(pts))

# Function to round to the nearest 0.5
round_half <- function(x) {
  rounded <- round(x * 2) / 2
  if (rounded == floor(rounded)) {
    return(rounded + 0.5)
  } else {
    return(rounded)
  }
}

# Apply the function to get baseline driver point values 
driver_pts <- driver_pts %>% mutate(mean_pts = sapply(mean_pts, round_half))

# Compute probability of driver going over or under their expected points
driver_pts <- sims_df %>%
  left_join(driver_pts, by="driver") %>%
  group_by(driver, mean_pts) %>%
  summarise(prob_over = sum(pts > mean_pts)/num_simulations,
            prob_under = sum(pts < mean_pts)/num_simulations)
  

driver_pts %>% arrange(-mean_pts)
  
##########################################  4. Team Total Points ##########################################
### Team - Total Points, OU, X+, Team to Score the Most Points, Team to Score Double Points 

# Indicate simulation number 
sims_df <- sims_df %>%
  mutate(sim_num = rep(1:5000, each = 20))

# Compute mean total points scored by team 
team_pts <- sims_df %>%
  group_by(team, sim_num) %>%
  summarise(tot_points = sum(pts),
            drivers_in_pts = sum(if_else(pts > 0, 1, 0)))

# Compute mean total points scored by team 
team_exp <- team_pts %>%
  group_by(team) %>%
  summarise(mean_pts = mean(tot_points),
            mean_pts = sapply(mean_pts, round_half),
            mean_drivers = mean(drivers_in_pts),
            mean_drivers = if_else(mean_drivers < 1.25, 0.5, 1.5))

# Compute probability of a team going over or under their expected points
team_exp_pts <- team_pts %>%
  left_join(team_exp, by="team") %>%
  group_by(team, mean_pts) %>%
  summarise(prob_over = sum(tot_points > mean_pts)/num_simulations,
            prob_under = sum(tot_points < mean_pts)/num_simulations)

# Compute probability of a team going over or under their expected drivers scoring points 
team_exp_drivers <- team_pts %>%
  left_join(team_exp, by="team") %>%
  group_by(team, mean_drivers) %>%
  summarise(prob_over = sum(drivers_in_pts > mean_drivers)/num_simulations,
            prob_under = sum(drivers_in_pts < mean_drivers)/num_simulations)

# Find the team with the maximum points in each simulation
max_points_per_sim <- team_pts %>%
  group_by(sim_num) %>%
  filter(tot_points == max(tot_points)) %>%
  ungroup()

# Compute probability of each team scoring the most total points 
team_max_points_count <- max_points_per_sim %>%
  count(team, name = "most_points_sims") %>%
  mutate(most_points_prob = most_points_sims / num_simulations)

##########################################  5. Podium Props/Exactas ##########################################
### Podium Drivers, Podium Exact

# Probability of 3 drivers finishing on the podium 
# Initialize an empty df to store results
podium_df <- data.frame(drivers = character(), proportion = numeric(), stringsAsFactors = FALSE)

# Loop through each combination
for (combination in driver_combinations) {
  driver_x <- combination[1]
  driver_y <- combination[2]
  driver_z <- combination[3]
  
  # Locate the 3 drivers' results
  drivers_xyz <- sims[, c(driver_x, driver_y, driver_z)]
  
  # Identify the proportion of simulations in which all three drivers finished on the podium
  podiums <- sum(if_else(drivers_xyz[, 1] < 4 & drivers_xyz[, 2] < 4 & drivers_xyz[, 3] < 4, 1, 0)) / num_simulations
  
  # Append the result to the data frame
  podium_df <- rbind(podium_df, data.frame(drivers = paste(driver_x, driver_y, driver_z, sep = "_"), proportion = podiums))
}

# Exclude combinations with 0% probability 
podium_df <- podium_df %>% filter(proportion > 0.01) %>% arrange(-proportion)
head(podium_df)

# Probability of exact podium result 
# Initialize an empty data frame to store results
podium_exact <- data.frame(drivers = character(),position = character(),proportion = numeric(),stringsAsFactors = FALSE)

# Define all permutations of 1st, 2nd, and 3rd places
positions <- list(
  c(1, 2, 3),
  c(1, 3, 2),
  c(2, 1, 3),
  c(2, 3, 1),
  c(3, 1, 2),
  c(3, 2, 1)
)

# Loop through each combination
for (combination in driver_combinations) {
  driver_x <- combination[1]
  driver_y <- combination[2]
  driver_z <- combination[3]
  
  # Locate the 3 drivers' results
  drivers_xyz <- sims[, c(driver_x, driver_y, driver_z)]
  
  # Loop through each permutation of positions
  for (pos in positions) {
    # Check the proportion of simulations where drivers finish in the specified order
    proportion <- sum(if_else(drivers_xyz[, 1] == pos[1] & drivers_xyz[, 2] == pos[2] & drivers_xyz[, 3] == pos[3], 1, 0)) / num_simulations
    
    # Store the result
    podium_exact <- rbind(podium_exact,
      data.frame(drivers = paste(driver_x, driver_y, driver_z, sep = "-"),position = paste(pos, collapse = "_"),
                 proportion = proportion,stringsAsFactors = FALSE)
    )
  }
}

# Exclude combinations with 0% probability 
podium_exact <- podium_exact %>% filter(proportion > 0.01) %>% arrange(-proportion)
head(podium_exact)
########################################## ########################################## ##########################################

