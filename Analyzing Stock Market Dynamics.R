# Install and load necessary packages
library(quantmod)
library(dplyr)
library(plotly)
library(stats)
library(ggplot2)

# Define the list of ticker symbols
ticker_symbols <- c("AAPL", "MSFT")

# Fetch historical stock prices
getSymbols(ticker_symbols, src = "yahoo", from = Sys.Date() - 365, to = Sys.Date())

# Combine data for all companies into a single data frame
combined_data <- do.call(merge, lapply(ticker_symbols, function(x) get(x)))

# View the combined data
head(combined_data)

#Descriptive Statistics
summary(AAPL)
summary(MSFT)

# Plot candlestick chart for AAPL
candleChart(AAPL, theme = "white", type = "candles")

# Plot candlestick chart for MSFT
candleChart(MSFT, theme = "white")

# Plot closing prices for each company separately
plot(combined_data$AAPL.Adjusted, type = "l", col = "blue",
     main = "Adjusted Closing Prices for Apple (AAPL)", ylab = "Price (USD)", xlab = "Date")

plot(combined_data$MSFT.Adjusted, type = "l", col = "purple",
     main = "Adjusted Closing Prices for Microsoft (MSFT)", ylab = "Price (USD)", xlab = "Date")


# Combine the closing prices into a single data frame
adjusted_closing_prices <- data.frame(AAPL = combined_data$AAPL.Adjusted,
                                      MSFT = combined_data$MSFT.Adjusted)


# Calculate daily returns for each company separately
AAPL_Daily_Return <- diff(combined_data$AAPL.Adjusted) / lag(combined_data$AAPL.Adjusted)
MSFT_Daily_Return <- diff(combined_data$MSFT.Adjusted) / lag(combined_data$MSFT.Adjusted)

# View the first few rows of each daily returns
head(AAPL_Daily_Return)
head(MSFT_Daily_Return)


# Combine daily returns into a matrix
daily_returns <- cbind(AAPL_Daily_Return[, "AAPL.Adjusted"], 
                       MSFT_Daily_Return[, "MSFT.Adjusted"])


# Plot histogram for AAPL
hist(AAPL_Daily_Return, breaks = 30, col = "blue", main = "AAPL Daily Returns", xlab = "Daily Return", ylab = "Frequency")

# Plot histogram for MSFT
hist(MSFT_Daily_Return, breaks = 30, col = "purple", main = "MSFT Daily Returns", xlab = "Daily Return", ylab = "Frequency")




# Fetch historical market index data (e.g., NASDAQ Composite Index)
getSymbols("^IXIC", src = "yahoo", from = Sys.Date() - 365, to = Sys.Date())

# Calculate market index returns (e.g., daily percentage change in adjusted close)
nasdaq_returns <- diff(Cl(IXIC)) / lag(Cl(IXIC))

# Plot market index returns
plot(index(nasdaq_returns), nasdaq_returns, type = "l", 
     col = "blue", main = "NASDAQ Composite Index Daily Returns", 
     ylab = "Daily Return", xlab = "Date")

# Calculate trading volume for all companies
AAPL_Volume <- Vo(AAPL)
MSFT_Volume <- Vo(MSFT)

# Combine volume data into a dataframe
volume_df <- data.frame(Date = index(AAPL), 
                        AAPL_Volume = AAPL_Volume, 
                        MSFT_Volume = MSFT_Volume)

# Calculate moving averages for each company
AAPL_MA_20 <- SMA(Cl(AAPL), n = 20)
AAPL_MA_50 <- SMA(Cl(AAPL), n = 50)
AAPL_MA_200 <- SMA(Cl(AAPL), n = 200)

MSFT_MA_20 <- SMA(Cl(MSFT), n = 20)
MSFT_MA_50 <- SMA(Cl(MSFT), n = 50)
MSFT_MA_200 <- SMA(Cl(MSFT), n = 200)


# Plot moving averages for AAPL
plot(index(AAPL), Cl(AAPL), type = "l", col = "black", main = "Moving Averages", 
     ylab = "Price", xlab = "Date")
lines(index(AAPL), AAPL_MA_20, col = "blue")
lines(index(AAPL), AAPL_MA_50, col = "red")
lines(index(AAPL), AAPL_MA_200, col = "green")
legend("topright", legend = c("Price", "MA 20", "MA 50", "MA 200"), 
       col = c("black", "blue", "red", "green"), lty = 1)

# Plot moving averages for MSFT
plot(index(MSFT), Cl(MSFT), type = "l", col = "black", main = "Moving Averages", 
     ylab = "Price", xlab = "Date")
lines(index(MSFT), MSFT_MA_20, col = "blue")
lines(index(MSFT), MSFT_MA_50, col = "red")
lines(index(MSFT), MSFT_MA_200, col = "green")
legend("topright", legend = c("Price", "MA 20", "MA 50", "MA 200"), 
       col = c("black", "blue", "red", "green"), lty = 1)

# Perform linear regression for AAPL using NASDAQ returns
model_AAPL <- lm(AAPL_Daily_Return ~ nasdaq_returns + AAPL_Volume + AAPL_MA_20 + AAPL_MA_50 + AAPL_MA_200)
summary(model_AAPL)

# Compute ANOVA for the linear regression model
anova_AAPL <- anova(model_AAPL)
print(anova_AAPL)

# Check for normality of residuals using a Q-Q plot
qqnorm(model_AAPL$residuals)
qqline(model_AAPL$residuals)

# Perform linear regression for MSFT using NASDAQ returns
model_MSFT <- lm(MSFT_Daily_Return ~ nasdaq_returns + MSFT_Volume + MSFT_MA_20 + MSFT_MA_50 + MSFT_MA_200)
summary(model_MSFT)

# Compute ANOVA for the linear regression model
anova_MSFT <- anova(model_MSFT)
print(anova_MSFT)

# Check for normality of residuals using a Q-Q plot
qqnorm(model_MSFT$residuals)
qqline(model_MSFT$residuals)
