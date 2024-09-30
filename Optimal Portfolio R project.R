# Load necessary libraries
if (!require("quantmod")) install.packages("quantmod", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)

library(quantmod)
library(ggplot2)

# Set risk-free rate (annualized)
risk_free_rate <- 0.02 / 12  # Monthly risk-free rate (2% annualized)

# Function to get stock data with customizable date range
get_stock_data <- function(symbols, start_date, end_date) {
  stock_data <- do.call(merge, lapply(symbols, function(x) {
    to.monthly(getSymbols(x, src = 'yahoo', auto.assign = FALSE, from = start_date, to = end_date), 
               indexAt = "lastof", OHLC = FALSE)[, 4]  # Close prices
  }))
  colnames(stock_data) <- symbols
  return(stock_data)  # Use monthly prices
}

# Function to calculate log returns
calculate_log_returns <- function(prices) {
  returns <- na.omit(diff(log(prices)))  # Logarithmic returns
  return(returns)
}

# Function to calculate excess returns
calculate_excess_returns <- function(returns) {
  return(returns - risk_free_rate)  # Subtract the risk-free rate from returns
}

# Function to generate portfolios
generate_portfolios <- function(num_portfolios, n, averages, covariances) {
  results <- matrix(NA, nrow = num_portfolios, ncol = 3)
  colnames(results) <- c("Return", "Risk", "Sharpe")
  
  for (i in 1:num_portfolios) {
    weights <- runif(n)  # Generate random weights
    weights <- weights / sum(weights)  # Normalize weights
    
    # Calculate expected portfolio return and risk
    portfolio_return <- sum(weights * averages) + risk_free_rate  # Include risk-free rate
    portfolio_risk <- sqrt(t(weights) %*% covariances %*% weights)  # Portfolio risk (standard deviation)
    
    results[i, ] <- c(portfolio_return, portfolio_risk, (portfolio_return - risk_free_rate) / portfolio_risk)  # Store results
  }
  
  return(as.data.frame(results))
}

# Function to plot the efficient frontier
plot_efficient_frontier <- function(results_df) {
  ggplot(results_df, aes(x = Risk, y = Return)) +
    geom_point(alpha = 0.5, color = "blue") +
    labs(title = "Efficient Frontier (Mean-Variance Optimization)", 
         x = "Risk (Standard Deviation)", 
         y = "Expected Return") +
    theme_minimal() +
    geom_smooth(se = FALSE, method = "loess", color = "red", span = 0.5)  # Use loess for smooth curve
}

# Main execution
symbols <- c("AAPL", "MSFT", "GOOG", "AMZN")  # Example stock symbols

# Set the date range for analysis
start_date <- "2020-01-01"  # Change this date as needed
end_date <- "2023-01-01"    # Change this date as needed

# Step 1: Gather historical stock prices
monthly_prices <- get_stock_data(symbols, start_date, end_date)

# Step 2: Calculate monthly logarithmic returns
monthly_returns <- calculate_log_returns(monthly_prices)

# Step 3: Calculate excess returns
excess_returns <- calculate_excess_returns(monthly_returns)

# Step 4: Calculate averages and covariance matrix
averages <- colMeans(excess_returns, na.rm = TRUE)  # Averages of excess returns
covariances <- cov(excess_returns, use = "pairwise.complete.obs")  # Covariance matrix of excess returns

# Step 5: Generate envelope portfolios with evenly spaced weights
num_portfolios <- 10000  # Number of portfolios to simulate
results_df <- generate_portfolios(num_portfolios, length(symbols), averages, covariances)

# Step 6: Sort results to create a smooth curve
results_df <- results_df[order(results_df$Risk), ]  # Sort by Risk

# Step 7: Plot the efficient frontier
plot_efficient_frontier(results_df)

# Step 8: Identify optimal portfolio (maximum Sharpe Ratio)
optimal_portfolio_index <- which.max(results_df$Sharpe)
optimal_portfolio <- results_df[optimal_portfolio_index, ]

# Display results
cat("Optimal Portfolio (Max Sharpe Ratio):\n")
print(optimal_portfolio)
