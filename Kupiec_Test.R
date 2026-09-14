# Prepare the data
actual_returns <- c(0, 1, 0, 1, 0, 0, 1, 1, 1, 0)
predicted_probabilities <- c(0.2, 0.8, 0.3, 0.9, 0.4, 0.6, 0.7, 0.5, 0.6, 0.3)
confidence_level <- 0.95

# Compute the Kupiec test statistic
num_obs <- length(actual_returns)
num_failures <- sum(actual_returns)
num_successes <- num_obs - num_failures
p <- mean(predicted_probabilities)

test_statistic <- -2 * (num_failures * log(p) + num_successes * log(1 - p))
critical_value <- qchisq(confidence_level, df = 1)

p_value <- 1 - pchisq(test_statistic, df = 1)

print(paste("Kupiec test statistic:", test_statistic))
print(paste("p-value:", p_value))
print(paste("Critical value at", confidence_level, "confidence level:", critical_value))
