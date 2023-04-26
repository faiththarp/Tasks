set.seed(42)  # Set the random seed for reproducibility

x <- rnorm(n = 100, mean = 0, sd = 2)  # Generate 100 random normal numbers with mean 0 and variance 4

print(x)  # Print the values of x
set.seed(42)  # Set the random seed for reproducibility

y <- x*5 + 2 + runif(100, min = 0, max = 0.1)  # Multiply x by 5, add 2, and add 100 random uniform numbers between 0 and 0.1

print(y)  # Print the values of y
model <- lm(y ~ x)  # Fit a linear model of y on x
summary(model)  # Print the summary of the linear model
lm(formula = y ~ x)
# Set up empty vectors to store results
slopes <- vector("numeric", 100)
intercepts <- vector("numeric", 100)
random_nums <- vector("numeric", 100)

for (i in 1:100) {
  # Generate random scaling factor
  z <- rnorm(1, mean = 1, sd = 0.1)
  # Generate x and transform it
  x <- rnorm(100, mean = 0, sd = 2)
  y <- 5*x*z + 2 + runif(100, min = 0, max = 0.1)
  # Fit linear model and store slope and intercept
  model <- lm(y ~ x)
  slopes[i] <- coef(model)[2]
  intercepts[i] <- coef(model)[1]
  random_nums[i] <- z
}

# Print summary of results
summary(data.frame(random_nums, slopes, intercepts))
# Plot z vs estimated slope
pdf("plot10.pdf")
plot(random_nums, slopes, xlab = "Random Scaling Factor (z)", ylab = "Estimated Slope")
dev.off()

# Define number of trials
n_trials <- 10000

# Define function to simulate one trial
monty_hall_trial <- function() {
  # Define doors
  doors <- c("goat", "goat", "car")
  
  # Shuffle doors
  doors <- sample(doors)
  
  # Choose a door
  chosen_door <- sample(1:3, 1)
  
  # Determine which door Monty Hall opens
  if (doors[chosen_door] == "car") {
    open_door <- sample(c(2, 3), 1)
  } else {
    open_door <- setdiff(1:3, c(chosen_door, which(doors == "car")))
  }
  
  # Determine whether to switch or stick with original choice
  switch_choice <- ifelse(doors[setdiff(1:3, c(chosen_door, open_door))] == "car", 1, 0)
  
  # Return switch choice result
  return(switch_choice)
}

# Simulate trials and calculate proportion of wins by switching choice
results <- replicate(n_trials, monty_hall_trial())
prop_wins_by_switching <- sum(results) / n_trials

# Print results
cat("Proportion of wins by switching choice:", prop_wins_by_switching, "\n")


# Set the number of simulations and initialize the win counters
n_sims <- 10000
win_stay <- 0
win_change <- 0

# Run the simulations
for (i in 1:n_sims) {
  # Randomly assign the prize to a door (1, 2, or 3)
  prize <- sample(1:3, 1)
  
  # Randomly choose a door (1, 2, or 3)
  choice <- sample(1:3, 1)
  
  # Host opens a door that does not have the prize and is not the chosen door
  open_door <- sample(setdiff(1:3, c(prize, choice)), 1)
  
  # Determine the result if the player sticks with their original choice
  if (choice == prize) {
    win_stay <- win_stay + 1
  }
  
  # Determine the result if the player changes their choice
  new_choice <- sample(setdiff(1:3, c(choice, open_door)), 1)
  if (new_choice == prize) {
    win_change <- win_change + 1
  }
}

# Create the bar graph
barplot(c(win_stay, win_change), names.arg = c("Stick with original choice", "Change choice"), 
        xlab = "Strategy", ylab = "Number of wins", col = c("blue", "red"))
