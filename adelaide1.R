# Load the necessary libraries
library(readxl)
library(zoo)  # For na.approx

# Read the Excel file
adelaide_data <- read_excel("C:/Users/smoha/Desktop/Adelaide time series(1).xlsx")

# Clean the data (example: remove rows with NAs)
adelaide_data_cleaned <- na.omit(adelaide_data)

# Convert the remaining values to a numeric array
Y_T <- as.numeric(adelaide_data_cleaned[[1]])  # Adjust the index based on your data
print(Y_T)
######################################1####################################
calculate_moving_averages <- function(values) {
  n <- length(values)
  moving_averages <- numeric(n)
  
  # 1. Calculate the 2-CMA for the first value
  moving_averages[1] <- values[1]
  
  # 2. Calculate the 4-CMA for the second value
  moving_averages[2] <- mean(values[1:2])
  
  # 3. Calculate the 4-CMA for the third to sixth values
  for (i in 3:min(6, n)) {
    moving_averages[i] <- mean(values[(i-2):i])
  }
  
  # 4. Calculate the 12-CMA from the seventh value onward
  for (i in 7:n) {
    moving_averages[i] <- mean(values[(i-6):(i+5)], na.rm = TRUE)
  }
  
  return(moving_averages)
}

# Calculate moving averages
T_T <- calculate_moving_averages(Y_T)
#######################################2######################################
# Print the result
print(T_T)

S_TE_T <- Y_T / T_T

# Print the ratios
print(S_TE_T)
########################################3#####################################
apply_3x3_MA <- function(array_data) {
  n <- length(array_data)
  
  # Create a vector to store the 3x3 moving average
  ma_3x3 <- numeric(n)
  
  # Calculate the 3x3 moving average
  for (i in 2:(n - 1)) {  # Avoid edges where 3x3 window isn't available
    # Take the mean of the 3x3 neighborhood around each element
    ma_3x3[i] <- mean(array_data[(i - 1):(i + 1)], na.rm = TRUE)
  }
  
  # Handle the first and last elements separately (since they have fewer neighbors)
  ma_3x3[1] <- mean(array_data[1:2], na.rm = TRUE)  # First element
  ma_3x3[n] <- mean(array_data[(n - 1):n], na.rm = TRUE)  # Last element
  
  return(ma_3x3)
}


# Apply the 3x3 moving average to S_tE_t
S_T <- apply_3x3_MA(S_TE_T)

# Print the 3x3 moving average result
print("3x3 Moving Average of S_tE_t:")
print(S_T)

E_T <- S_TE_T/S_T
print(E_T)

# Custom function to calculate centered moving average
centered_moving_average <- function(data) {
  n <- length(data)
  ma_result <- numeric(n)  # Initialize the result vector

  for (i in 1:n) {
    # Define the range for the moving average (1 before, current, 1 after)
    start_index <- max(1, i - 1)
    end_index <- min(n, i + 1)
    
    # Extract the values in the range
    values <- data[start_index:end_index]
    
    # Calculate the mean, ignoring NAs
    ma_result[i] <- mean(values, na.rm = TRUE)
  }
  
  return(ma_result)
}

# Step 1: Calculate centered moving average
centered_ma <- centered_moving_average(E_T)

# Step 2: Calculate centered ratios (E_T / CMA)
# Using ifelse to avoid NA in centered_ma
centered_ratios <- E_T / ifelse(is.na(centered_ma), 1, centered_ma)  # Replace NA with 1 to avoid NA in division

# Step 3: Define a threshold for extreme values
threshold_high <- 1.2
threshold_low <- 0.8

# Step 4: Adjust extreme values based on the threshold
adjusted_E_T <- ifelse(centered_ratios > threshold_high, centered_ma * threshold_high,
                       ifelse(centered_ratios < threshold_low, centered_ma * threshold_low, E_T))

# Step 5: Optionally apply a smoothing technique (another moving average)
E_Tnew <- centered_moving_average(adjusted_E_T)

# Print the adjusted E_T values
print(adjusted_E_T)
print(E_Tnew)
#########################################4#################################
S_TE_Tnew <- S_T*E_Tnew

S_Tnew <- apply_3x3_MA(S_TE_Tnew)

print(S_Tnew)

# Assume S_Tnew is your adjusted series
# and it has been calculated already

# Number of years in the data
years <- length(S_Tnew) / 12

# Initialize a vector to store the adjusted values
S_Tnew_adjusted <- numeric(length(S_Tnew))

# Loop through each year to adjust the sums
for (year in 1:years) {
  # Calculate the start and end indices for the current 12-month period
  start_index <- (year - 1) * 12 + 1
  end_index <- year * 12
  
  # Calculate the sum for the current 12-month period
  total_sum <- sum(S_Tnew[start_index:end_index], na.rm = TRUE)
  
  # Calculate the adjustment factor to make the sum approximately 12
  adjustment_factor <- 12 / total_sum
  
  # Adjust each month in the current period
  S_Tnew_adjusted[start_index:end_index] <- S_Tnew[start_index:end_index] * adjustment_factor
}

# Print the adjusted values
print(S_Tnew_adjusted)

# Optionally, you can check the sum of each 12-month period
adjusted_sums <- sapply(1:years, function(year) sum(S_Tnew_adjusted[((year - 1) * 12 + 1):(year * 12)], na.rm = TRUE))
)
print(adjusted_sums)  # This should be approximately 12 for each year
##########################################5################################
T_TE_T <- Y_T/S_Tnew_adjusted

print(T_TE_T)
###########################################6###############################
# Given Henderson weights
henderson_weights <- c(-0.004, -0.011, -0.016, -0.015, -0.005,
                       0.013, 0.039, 0.068, 0.097, 0.122,
                       0.138, 0.148, 0.138, 0.122, 0.097,
                       0.068, 0.039, 0.013, -0.005, -0.015,
                       -0.016, -0.011, -0.004)

# Apply Henderson 23-MA weights

T_T6 <- filter(T_TE_T, henderson_weights, sides = 2)
print(T_T6)

# Step 2: Estimate Missing Values using Linear Regression
for (i in 422:432) {
    if (is.na(T_T6[i])) {
        # Extract the last 20 values (402 to 421 for i = 422)
        last_20_values <- T_T6[(i - 20):(i - 1)]
        
        # Create indices for the last 20 values
        indices <- seq(1, 20)
        
        # Remove NA values from last_20_values and indices
        valid_indices <- !is.na(last_20_values)
        x <- indices[valid_indices]
        y <- last_20_values[valid_indices]
        
        # Check if we have enough data to fit a model
        if (length(x) > 1) {
            # Fit a linear model
            model <- lm(y ~ x)
            
            # Predict the next value (for i = 21)
            next_value <- predict(model, newdata = data.frame(x = 21))
            
            # Fill the missing value
            T_T6[i] <- next_value
        }
    }
}

for (i in 11:1) {
    if (is.na(T_T6[i])) {
        # Extract the last 20 values (e.g., for i = 11, we look at values 2 to 11)
        last_20_values <- T_T6[(i + 1):(i + 20)]
        
        # Create indices for the last 20 values
        indices <- seq(1, length(last_20_values))
        
        # Remove NA values from last_20_values and indices
        valid_indices <- !is.na(last_20_values)
        x <- indices[valid_indices]
        y <- last_20_values[valid_indices]
        
        # Check if we have enough data to fit a model
        if (length(x) > 1) {
            # Fit a linear model
            model <- lm(y ~ x)
            
            # Predict the next value (for i = 21)
            next_value <- predict(model, newdata = data.frame(x = length(last_20_values) + 1))
            
            # Fill the missing value
            T_T6[i] <- next_value
        }
    }
}

# Print the filled data
print(T_T6)
############################################7#############################
S_TE_T7 <- Y_T/T_T6

print(S_TE_T7)
#############################################8############################
# Function to calculate 3x5 Moving Average
three_x_five_ma <- function(data) {
  # Calculate the moving average using rollapply
  smoothed_values <- rollapply(data, width = 3, FUN = mean, fill = NA, align = "center", na.rm = TRUE)

  # Interpolate to fill remaining NAs
  smoothed_values <- na.approx(smoothed_values, na.rm = FALSE)

  # Fill initial NA values using linear interpolation for the first value
  if (is.na(smoothed_values[1])) {
    first_non_na <- which(!is.na(smoothed_values))[1]
    smoothed_values[1] <- smoothed_values[first_non_na]  # Replace with the first non-NA
  }

  # Fill trailing NA values using linear interpolation for the last value
  if (is.na(smoothed_values[length(smoothed_values)])) {
    last_non_na <- tail(which(!is.na(smoothed_values)), 1)
    smoothed_values[length(smoothed_values)] <- smoothed_values[last_non_na]  # Replace with the last non-NA
  }

  return(smoothed_values)
}

# Apply the 3x5 moving average
S_T8 <- three_x_five_ma(S_TE_T7)

# Print the smoothed data
print(S_T8)

E_T8 <- S_TE_T7
print(E_T8)
# Step 1: Calculate centered moving average
centered_ma <- centered_moving_average(E_T8)

# Step 2: Calculate centered ratios (E_T8 / CMA)
# Using ifelse to avoid NA in centered_ma
centered_ratios <- E_T8 / ifelse(is.na(centered_ma), 1, centered_ma)  # Replace NA with 1 to avoid NA in division

# Step 3: Define a threshold for extreme values
threshold_high <- 1.2
threshold_low <- 0.8

# Step 4: Adjust extreme values based on the threshold
adjusted_E_T8 <- ifelse(centered_ratios > threshold_high, centered_ma * threshold_high,
                       ifelse(centered_ratios < threshold_low, centered_ma * threshold_low, E_T8))

# Step 5: Optionally apply a smoothing technique (another moving average)
E_Tnew8 <- centered_moving_average(adjusted_E_T8)

# Print the adjusted E_T values
print(adjusted_E_T8)
print(E_Tnew8)
##############################################9###########################
S_TE_Tnew9 <- S_T*E_Tnew8

S_Tnew9 <- three_x_five_ma(S_TE_Tnew9)

print(S_Tnew9)

# Assume S_Tnew is your adjusted series
# and it has been calculated already

# Number of years in the data
years <- length(S_Tnew9) / 12

# Initialize a vector to store the adjusted values
S_Tnew_adjusted9 <- numeric(length(S_Tnew9))

# Loop through each year to adjust the sums
for (year in 1:years) {
  # Calculate the start and end indices for the current 12-month period
  start_index <- (year - 1) * 12 + 1
  end_index <- year * 12
  
  # Calculate the sum for the current 12-month period
  total_sum <- sum(S_Tnew9[start_index:end_index], na.rm = TRUE)
  
  # Calculate the adjustment factor to make the sum approximately 12
  adjustment_factor <- 12 / total_sum
  
  # Adjust each month in the current period
  S_Tnew_adjusted9[start_index:end_index] <- S_Tnew9[start_index:end_index] * adjustment_factor
}

# Print the adjusted values
print(S_Tnew_adjusted9)

# Optionally, you can check the sum of each 12-month period
adjusted_sums <- sapply(1:years, function(year) sum(S_Tnew_adjusted9[((year - 1) * 12 + 1):(year * 12)], na.rm = TRUE))
)
print(adjusted_sums)  # This should be approximately 12 for each year
######################################10######################################

T_TE_T10 <- Y_T/S_Tnew_adjusted9

print(T_TE_T10)
######################################11######################################
E_T11 <-T_TE_T10/T_T6
print(E_T11)
######################################12####################################
centered_ma <- centered_moving_average(E_T11)

# Step 2: Calculate centered ratios (E_T11 / CMA)
# Using ifelse to avoid NA in centered_ma
centered_ratios <- E_T11 / ifelse(is.na(centered_ma), 1, centered_ma)  # Replace NA with 1 to avoid NA in division

# Step 3: Define a threshold for extreme values
threshold_high <- 1.2
threshold_low <- 0.8

# Step 4: Adjust extreme values based on the threshold
adjusted_E_T12 <- ifelse(centered_ratios > threshold_high, centered_ma * threshold_high,
                       ifelse(centered_ratios < threshold_low, centered_ma * threshold_low, E_T11))

# Step 5: Optionally apply a smoothing technique (another moving average)
E_Tnew12 <- centered_moving_average(adjusted_E_T11)

# Print the adjusted E_T values
print(adjusted_E_T12)
print(E_Tnew12)

Y_Ta <-E_Tnew12*T_T6*S_Tnew_adjusted9

print(Y_Ta)
####################################################################################################################
####################################################################################################################
####################################################################################################################
T_Ta <- calculate_moving_averages(Y_Ta)
#######################################2######################################
# Print the result
print(T_Ta)

S_TE_Ta <- Y_Ta / T_Ta

# Print the ratios
print(S_TE_Ta)
########################################3#####################################
# Apply the 3x3 moving average to S_tE_t
S_Ta <- apply_3x3_MA(S_TE_Ta)

# Print the 3x3 moving average result
print("3x3 Moving Average of S_tE_ta:")
print(S_Ta)

E_Ta <- S_TE_Ta/S_Ta
print(E_Ta)

# Step 1: Calculate centered moving average
centered_ma <- centered_moving_average(E_Ta)

# Step 2: Calculate centered ratios (E_Ta / CMA)
# Using ifelse to avoid NA in centered_ma
centered_ratios <- E_Ta / ifelse(is.na(centered_ma), 1, centered_ma)  # Replace NA with 1 to avoid NA in division

# Step 3: Define a threshold for extreme values
threshold_high <- 1.2
threshold_low <- 0.8

# Step 4: Adjust extreme values based on the threshold
adjusted_E_Ta <- ifelse(centered_ratios > threshold_high, centered_ma * threshold_high,
                       ifelse(centered_ratios < threshold_low, centered_ma * threshold_low, E_Ta))

# Step 5: Optionally apply a smoothing technique (another moving average)
E_Tnewa <- centered_moving_average(adjusted_E_Ta)

# Print the adjusted E_T values
print(adjusted_E_Ta)
print(E_Tnewa)
#########################################4#################################
S_TE_Tnewa <- S_T*E_Tnewa

S_Tnewa <- apply_3x3_MA(S_TE_Tnewa)

print(S_Tnewa)

# Assume S_Tnew is your adjusted series
# and it has been calculated already

# Number of years in the data
years <- length(S_Tnewa) / 12

# Initialize a vector to store the adjusted values
S_Tnew_adjusteda <- numeric(length(S_Tnewa))

# Loop through each year to adjust the sums
for (year in 1:years) {
  # Calculate the start and end indices for the current 12-month period
  start_index <- (year - 1) * 12 + 1
  end_index <- year * 12
  
  # Calculate the sum for the current 12-month period
  total_sum <- sum(S_Tnewa[start_index:end_index], na.rm = TRUE)
  
  # Calculate the adjustment factor to make the sum approximately 12
  adjustment_factor <- 12 / total_sum
  
  # Adjust each month in the current period
  S_Tnew_adjusteda[start_index:end_index] <- S_Tnewa[start_index:end_index] * adjustment_factor
}

# Print the adjusted values
print(S_Tnew_adjusteda)
##########################################5################################ 
T_TE_Ta <- Y_Ta/S_Tnew_adjusteda

print(T_TE_Ta) 
###########################################6###############################
T_T6a <- filter(T_TE_Ta, henderson_weights, sides = 2)
print(T_T6a)

# Step 2: Estimate Missing Values using Linear Regression
for (i in 422:432) {
    if (is.na(T_T6a[i])) {
        # Extract the last 20 values (402 to 421 for i = 422)
        last_20_values <- T_T6a[(i - 20):(i - 1)]
        
        # Create indices for the last 20 values
        indices <- seq(1, 20)
        
        # Remove NA values from last_20_values and indices
        valid_indices <- !is.na(last_20_values)
        x <- indices[valid_indices]
        y <- last_20_values[valid_indices]
        
        # Check if we have enough data to fit a model
        if (length(x) > 1) {
            # Fit a linear model
            model <- lm(y ~ x)
            
            # Predict the next value (for i = 21)
            next_value <- predict(model, newdata = data.frame(x = 21))
            
            # Fill the missing value
            T_T6a[i] <- next_value
        }
    }
}

for (i in 11:1) {
    if (is.na(T_T6a[i])) {
        # Extract the last 20 values (e.g., for i = 11, we look at values 2 to 11)
        last_20_values <- T_T6a[(i + 1):(i + 20)]
        
        # Create indices for the last 20 values
        indices <- seq(1, length(last_20_values))
        
        # Remove NA values from last_20_values and indices
        valid_indices <- !is.na(last_20_values)
        x <- indices[valid_indices]
        y <- last_20_values[valid_indices]
        
        # Check if we have enough data to fit a model
        if (length(x) > 1) {
            # Fit a linear model
            model <- lm(y ~ x)
            
            # Predict the next value (for i = 21)
            next_value <- predict(model, newdata = data.frame(x = length(last_20_values) + 1))
            
            # Fill the missing value
            T_T6a[i] <- next_value
        }
    }
}

# Print the filled data
print(T_T6a)
############################################7#############################
S_TE_T7a <- Y_Ta/T_T6a

print(S_TE_T7a)
#############################################8############################
# Apply the 3x5 moving average
S_T8a <- three_x_five_ma(S_TE_T7a)

# Print the smoothed data
print(S_T8a)

E_T8a <- S_TE_T7a
print(E_T8a)
# Step 1: Calculate centered moving average
centered_ma <- centered_moving_average(E_T8a)

# Step 2: Calculate centered ratios (E_T8a / CMA)
# Using ifelse to avoid NA in centered_ma
centered_ratios <- E_T8a / ifelse(is.na(centered_ma), 1, centered_ma)  # Replace NA with 1 to avoid NA in division

# Step 3: Define a threshold for extreme values
threshold_high <- 1.2
threshold_low <- 0.8

# Step 4: Adjust extreme values based on the threshold
adjusted_E_T8a <- ifelse(centered_ratios > threshold_high, centered_ma * threshold_high,
                       ifelse(centered_ratios < threshold_low, centered_ma * threshold_low, E_T8a))

# Step 5: Optionally apply a smoothing technique (another moving average)
E_Tnew8a <- centered_moving_average(adjusted_E_T8a)

# Print the adjusted E_T values
print(adjusted_E_T8a)
print(E_Tnew8a) 
##############################################9###########################
S_TE_Tnew9a <- S_T*E_Tnew8a

S_Tnew9a <- three_x_five_ma(S_TE_Tnew9a)

print(S_Tnew9a)

# Assume S_Tnew is your adjusted series
# and it has been calculated already

# Number of years in the data
years <- length(S_Tnew9a) / 12

# Initialize a vector to store the adjusted values
S_Tnew_adjusted9a <- numeric(length(S_Tnew9a))

# Loop through each year to adjust the sums
for (year in 1:years) {
  # Calculate the start and end indices for the current 12-month period
  start_index <- (year - 1) * 12 + 1
  end_index <- year * 12
  
  # Calculate the sum for the current 12-month period
  total_sum <- sum(S_Tnew9a[start_index:end_index], na.rm = TRUE)
  
  # Calculate the adjustment factor to make the sum approximately 12
  adjustment_factor <- 12 / total_sum
  
  # Adjust each month in the current period
  S_Tnew_adjusted9a[start_index:end_index] <- S_Tnew9a[start_index:end_index] * adjustment_factor
}

# Print the adjusted values
print(S_Tnew_adjusted9a)
######################################10######################################

T_TE_T10a <- Y_T/S_Tnew_adjusted9a

print(T_TE_T10a)
######################################11######################################
E_T11a <-T_TE_T10a/T_T6a
print(E_T11a)
######################################12####################################
centered_ma <- centered_moving_average(E_T11a)

# Step 2: Calculate centered ratios (E_T11a / CMA)
# Using ifelse to avoid NA in centered_ma
centered_ratios <- E_T11a / ifelse(is.na(centered_ma), 1, centered_ma)  # Replace NA with 1 to avoid NA in division

# Step 3: Define a threshold for extreme values
threshold_high <- 1.2
threshold_low <- 0.8

# Step 4: Adjust extreme values based on the threshold
adjusted_E_T12a <- ifelse(centered_ratios > threshold_high, centered_ma * threshold_high,
                       ifelse(centered_ratios < threshold_low, centered_ma * threshold_low, E_T11a))

# Step 5: Optionally apply a smoothing technique (another moving average)
E_Tnew12a <- centered_moving_average(adjusted_E_T12a)

# Print the adjusted E_T values
print(adjusted_E_T12a)
print(E_Tnew12a)

Y_Tb <-E_Tnew12a*T_T6a*S_Tnew_adjusted9a

print(Y_Tb)
###############################################################################
###############################################################################
#######################################1######################################
T_Tb <- calculate_moving_averages(Y_Tb)
#######################################2######################################
# Print the result
print(T_Tb)

S_TE_Tb <- Y_Tb / T_Tb

# Print the ratios
print(S_TE_Tb)
########################################3#####################################
# Apply the 3x3 moving average to S_TE_Tb
S_Tb <- apply_3x3_MA(S_TE_Tb)

# Print the 3x3 moving average result
print("3x3 Moving Average of S_TE_Tb:")
print(S_Tb)

E_Tb <- S_TE_Tb / S_Tb
print(E_Tb)

# Step 1: Calculate centered moving average
centered_ma <- centered_moving_average(E_Tb)

# Step 2: Calculate centered ratios (E_Tb / CMA)
# Using ifelse to avoid NA in centered_ma
centered_ratios <- E_Tb / ifelse(is.na(centered_ma), 1, centered_ma)  # Replace NA with 1 to avoid NA in division

# Step 3: Define a threshold for extreme values
threshold_high <- 1.2
threshold_low <- 0.8

# Step 4: Adjust extreme values based on the threshold
adjusted_E_Tb <- ifelse(centered_ratios > threshold_high, centered_ma * threshold_high,
                        ifelse(centered_ratios < threshold_low, centered_ma * threshold_low, E_Tb))

# Step 5: Optionally apply a smoothing technique (another moving average)
E_Tnewb <- centered_moving_average(adjusted_E_Tb)

# Print the adjusted E_T values
print(adjusted_E_Tb)
print(E_Tnewb)
#########################################4#################################
S_TE_Tnewb <- S_Tb * E_Tnewb

S_Tnewb <- apply_3x3_MA(S_TE_Tnewb)

print(S_Tnewb)

# Number of years in the data
years <- length(S_Tnewb) / 12

# Initialize a vector to store the adjusted values
S_Tnew_adjustedb <- numeric(length(S_Tnewb))

# Loop through each year to adjust the sums
for (year in 1:years) {
  # Calculate the start and end indices for the current 12-month period
  start_index <- (year - 1) * 12 + 1
  end_index <- year * 12
  
  # Calculate the sum for the current 12-month period
  total_sum <- sum(S_Tnewb[start_index:end_index], na.rm = TRUE)
  
  # Calculate the adjustment factor to make the sum approximately 12
  adjustment_factor <- 12 / total_sum
  
  # Adjust each month in the current period
  S_Tnew_adjustedb[start_index:end_index] <- S_Tnewb[start_index:end_index] * adjustment_factor
}

# Print the adjusted values
print(S_Tnew_adjustedb)
##########################################5################################ 
T_TE_Tb <- Y_Tb / S_Tnew_adjustedb

print(T_TE_Tb) 
###########################################6###############################
T_T6b <- filter(T_TE_Tb, henderson_weights, sides = 2)
print(T_T6b)

# Step 2: Estimate Missing Values using Linear Regression
for (i in 422:432) {
    if (is.na(T_T6b[i])) {
        # Extract the last 20 values (402 to 421 for i = 422)
        last_20_values <- T_T6b[(i - 20):(i - 1)]
        
        # Create indices for the last 20 values
        indices <- seq(1, 20)
        
        # Remove NA values from last_20_values and indices
        valid_indices <- !is.na(last_20_values)
        x <- indices[valid_indices]
        y <- last_20_values[valid_indices]
        
        # Check if we have enough data to fit a model
        if (length(x) > 1) {
            # Fit a linear model
            model <- lm(y ~ x)
            
            # Predict the next value (for i = 21)
            next_value <- predict(model, newdata = data.frame(x = 21))
            
            # Fill the missing value
            T_T6b[i] <- next_value
        }
    }
}

for (i in 11:1) {
    if (is.na(T_T6b[i])) {
        # Extract the last 20 values (e.g., for i = 11, we look at values 2 to 11)
        last_20_values <- T_T6b[(i + 1):(i + 20)]
        
        # Create indices for the last 20 values
        indices <- seq(1, length(last_20_values))
        
        # Remove NA values from last_20_values and indices
        valid_indices <- !is.na(last_20_values)
        x <- indices[valid_indices]
        y <- last_20_values[valid_indices]
        
        # Check if we have enough data to fit a model
        if (length(x) > 1) {
            # Fit a linear model
            model <- lm(y ~ x)
            
            # Predict the next value (for i = 21)
            next_value <- predict(model, newdata = data.frame(x = length(last_20_values) + 1))
            
            # Fill the missing value
            T_T6b[i] <- next_value
        }
    }
}

# Print the filled data
print(T_T6b)
############################################7#############################
S_TE_T7b <- Y_Tb / T_T6b

print(S_TE_T7b)
#############################################8############################
# Apply the 3x5 moving average
S_T8b <- three_x_five_ma(S_TE_T7b)

# Print the smoothed data
print(S_T8b)

E_T8b <- S_TE_T7b
print(E_T8b)
# Step 1: Calculate centered moving average
centered_ma <- centered_moving_average(E_T8b)

# Step 2: Calculate centered ratios (E_T8b / CMA)
# Using ifelse to avoid NA in centered_ma
centered_ratios <- E_T8b / ifelse(is.na(centered_ma), 1, centered_ma)  # Replace NA with 1 to avoid NA in division

# Step 3: Define a threshold for extreme values
threshold_high <- 1.2
threshold_low <- 0.8

# Step 4: Adjust extreme values based on the threshold
adjusted_E_T8b <- ifelse(centered_ratios > threshold_high, centered_ma * threshold_high,
                         ifelse(centered_ratios < threshold_low, centered_ma * threshold_low, E_T8b))

# Step 5: Optionally apply a smoothing technique (another moving average)
E_Tnew8b <- centered_moving_average(adjusted_E_T8b)

# Print the adjusted E_T values
print(adjusted_E_T8b)
print(E_Tnew8b) 
##############################################9###########################
S_TE_Tnew9b <- S_Tb * E_Tnew8b

S_Tnew9b <- three_x_five_ma(S_TE_Tnew9b)

print(S_Tnew9b)

# Number of years in the data
years <- length(S_Tnew9b) / 12

# Initialize a vector to store the adjusted values
S_Tnew_adjusted9b <- numeric(length(S_Tnew9b))

# Loop through each year to adjust the sums
for (year in 1:years) {
  # Calculate the start and end indices for the current 12-month period
  start_index <- (year - 1) * 12 + 1
  end_index <- year * 12
  
  # Calculate the sum for the current 12-month period
  total_sum <- sum(S_Tnew9b[start_index:end_index], na.rm = TRUE)
  
  # Calculate the adjustment factor to make the sum approximately 12
  adjustment_factor <- 12 / total_sum
  
  # Adjust each month in the current period
  S_Tnew_adjusted9b[start_index:end_index] <- S_Tnew9b[start_index:end_index] * adjustment_factor
}

# Print the adjusted values
print(S_Tnew_adjusted9b)
######################################10######################################

T_TE_T10b <- Y_Tb / S_Tnew_adjusted9b

print(T_TE_T10b)
######################################11######################################
E_T11b <- T_TE_T10b / T_T6b
print(E_T11b)
######################################12####################################
centered_ma <- centered_moving_average(E_T11b)

# Step 2: Calculate centered ratios (E_T11b / CMA)
# Using ifelse to avoid NA in centered_ma
centered_ratios <- E_T11b / ifelse(is.na(centered_ma), 1, centered_ma)  # Replace NA with 1 to avoid NA in division

# Step 3: Define a threshold for extreme values
threshold_high <- 1.2
threshold_low <- 0.8

# Step 4: Adjust extreme values based on the threshold
adjusted_E_T12b <- ifelse(centered_ratios > threshold_high, centered_ma * threshold_high,
                          ifelse(centered_ratios < threshold_low, centered_ma * threshold_low, E_T11b))

# Step 5: Optionally apply a smoothing technique (another moving average)
E_Tnew12b <- centered_moving_average(adjusted_E_T12b)

# Print the adjusted E_T values
print(adjusted_E_T12b)
print(E_Tnew12b)

Y_TFinal <-E_Tnew12a*T_T6a*S_Tnew_adjusted9a

print(Y_TFinal)
###############################################################################
###############################################################################
###############################################################################

forecast_error <- Y_TFinal - Y_T

# Step 3: Calculate Accuracy Metrics
MAE <- mean(abs(forecast_error))
MSE <- mean(forecast_error^2)
MAPE <- mean(abs(forecast_error / Y_Tb)) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Mean Absolute Percentage Error (MAPE):", MAPE, "%\n")

mean_Y_T <- mean(Y_T)

# Calculate SS_res and SS_tot
SS_res <- sum((Y_T - Y_TFinal)^2)
SS_tot <- sum((Y_T - mean_Y_T)^2)

# Calculate R^2
R_squared <- 1 - (SS_res / SS_tot)

# Print R^2 value
print(R_squared)
