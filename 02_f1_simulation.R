######################################################## FORMULA 1 RACE SIMULATION #####################################################  
######################################################## 02. RACE SIMULATION MODEL #####################################################

head(df)

# Function to simulate the race 
set.seed(123) 
num_simulations <- 5000
prob <- df$true_prob

sim <- function(prob) {
  drivers <- length(prob)
  outcomes <- sample(1:drivers, size = drivers, replace = FALSE, prob = prob)
  return(outcomes)
}

# Run 5,000 simulations
simulations <- replicate(num_simulations, sim(prob))

# Store results in a dataframe and rename columns as drivers 
sims <- as.data.frame(t(simulations))
colnames(sims) <- df$driver

# Reshape the data from wide to long 
sims_df <- sims %>% 
  pivot_longer(cols = everything(), names_to = "driver", values_to = "finish") %>%
  left_join(df, by="driver") %>%
  select(driver, finish, team)

head(sims_df)

