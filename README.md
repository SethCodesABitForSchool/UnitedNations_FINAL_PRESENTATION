# UnitedNationsBYE_Final - Please do not use/cite
# Create a data frame with the example data
stock_data <- data.frame(
  Month = c(
    "Jan '22", "Feb '22", "Mar '22", "Apr '22", "May '22", "Jun '22",
    "Jul '22", "Aug '22", "Sep '22", "Oct '22", "Nov '22", "Dec '22",
    "Jan '23", "Feb '23", "Mar '23", "Apr '23", "May '23", "Jun '23",
    "Jul '23", "Aug '23", "Sep '23", "Oct '23", "Nov '23", "Dec '23"
  ),
  Stock_Price = c(
    50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105,
    110, 115, 120, 125, 130, 135, 140, 145, 150, 155, 160, 165
  )
)

# View the created data frame
print(stock_data)

# Load the necessary libraries (if not already loaded)
# install.packages("dplyr")  # Install dplyr if you haven't already
install.packages("dplyr")


# Create a numerical variable for the months (e.g., 1 for Jan '22, 2 for Feb '22, etc.)
stock_data <- stock_data %>%
  mutate(Numeric_Month = seq_along(Month))

# Perform a linear regression to estimate the trend
model <- lm(Stock_Price ~ Numeric_Month, data = stock_data)

# Extract the estimated coefficients
beta_0 <- coef(model)[1]  # Intercept
beta_1 <- coef(model)[2]  # Slope

# Calculate the trend component for each month
stock_data <- stock_data %>%
  mutate(Trend_Component = beta_0 + beta_1 * Numeric_Month)

# View the results
print(stock_data)


# Load the 'dplyr' library explicitly
library(dplyr)

# Now, you should be able to use the pipe operator
stock_data <- stock_data %>%
  mutate(Numeric_Month = seq_along(Month))



# Install and load the ggplot2 package if not already installed
# install.packages("ggplot2")
library(ggplot2)

# Create a line plot of Stock_Price and Trend_Component over time
ggplot(stock_data, aes(x = Month)) +
  geom_line(aes(y = Stock_Price, color = "Stock Price"), size = 1) +
  geom_line(aes(y = Trend_Component, color = "Trend Component"), size = 1, linetype = "dashed") +
  labs(title = "Stock Price and Trend Component Over Time", y = "Price/Trend", color = "Legend") +
  scale_color_manual(values = c("Stock Price" = "blue", "Trend Component" = "red")) +
  theme_minimal()









ggplot(stock_data, aes(x = Month)) +
  geom_line(aes(y = Stock_Price, color = "Stock Price", group = 1), linewidth = 1) +
  geom_line(aes(y = Trend_Component, color = "Trend Component", group = 2), linewidth = 1, linetype = "dashed") +
  labs(title = "Stock Price and Trend Component Over Time", y = "Price/Trend", color = "Legend") +
  scale_color_manual(values = c("Stock Price" = "blue", "Trend Component" = "red")) +
  theme_minimal()



# Separate the data into two data frames: one for Stock_Price and one for Trend_Component
stock_price_data <- stock_data %>%
  select(Month, Stock_Price)

trend_data <- stock_data %>%
  select(Month, Trend_Component)

# Create separate line plots for Stock_Price and Trend_Component
ggplot() +
  geom_line(data = stock_price_data, aes(x = Month, y = Stock_Price, color = "Stock Price"), size = 1) +
  geom_line(data = trend_data, aes(x = Month, y = Trend_Component, color = "Trend Component"), size = 1, linetype = "dashed") +
  labs(title = "Stock Price and Trend Component Over Time", y = "Price/Trend", color = "Legend") +
  scale_color_manual(values = c("Stock Price" = "blue", "Trend Component" = "red")) +
  theme_minimal()





# Convert the "Month" column to a factor
stock_data$Month <- factor(stock_data$Month, levels = unique(stock_data$Month))

# Separate the data into two data frames: one for Stock_Price and one for Trend_Component
stock_price_data <- stock_data %>%
  select(Month, Stock_Price)

trend_data <- stock_data %>%
  select(Month, Trend_Component)

# Create separate line plots for Stock_Price and Trend_Component
ggplot() +
  geom_line(data = stock_price_data, aes(x = Month, y = Stock_Price, color = "Stock Price"), size = 1) +
  geom_line(data = trend_data, aes(x = Month, y = Trend_Component, color = "Trend Component"), size = 1, linetype = "dashed") +
  labs(title = "Stock Price and Trend Component Over Time", y = "Price/Trend", color = "Legend") +
  scale_color_manual(values = c("Stock Price" = "blue", "Trend Component" = "red")) +
  theme_minimal()

# Convert the "Month" column to a date format
stock_data$Month <- as.Date(paste0("01-", stock_data$Month), format = "%d-%b '%y")

# Separate the data into two data frames: one for Stock_Price and one for Trend_Component
stock_price_data <- stock_data %>%
  select(Month, Stock_Price)

trend_data <- stock_data %>%
  select(Month, Trend_Component)

# Create separate line plots for Stock_Price and Trend_Component
ggplot() +
  geom_line(data = stock_price_data, aes(x = Month, y = Stock_Price, color = "Stock Price"), size = 1) +
  geom_line(data = trend_data, aes(x = Month, y = Trend_Component, color = "Trend Component"), size = 1, linetype = "dashed") +
  labs(title = "Stock Price and Trend Component Over Time", y = "Price/Trend", color = "Legend") +
  scale_color_manual(values = c("Stock Price" = "blue", "Trend Component" = "red")) +
  theme_minimal()

library(ggplot2)

# Create a line plot for Stock_Price
stock_plot <- ggplot(stock_data, aes(x = Month, y = Stock_Price)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Stock Price Over Time", y = "Price") +
  theme_minimal()

# Create a line plot for Trend_Component
trend_plot <- ggplot(stock_data, aes(x = Month, y = Trend_Component)) +
  geom_line(color = "red", size = 1, linetype = "dashed") +
  labs(title = "Trend Component Over Time", y = "Trend") +
  theme_minimal()

# Display both plots side by side
library(gridExtra)
grid.arrange(stock_plot, trend_plot, ncol = 2)


install.packages("gridExtra")
library(gridExtra)
# Create a line plot for Stock_Price
stock_plot <- ggplot(stock_data, aes(x = Month, y = Stock_Price)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Stock Price Over Time", y = "Price") +
  theme_minimal()

# Create a line plot for Trend_Component
trend_plot <- ggplot(stock_data, aes(x = Month, y = Trend_Component)) +
  geom_line(color = "red", size = 1, linetype = "dashed") +
  labs(title = "Trend Component Over Time", y = "Trend") +
  theme_minimal()

# Display both plots side by side
grid.arrange(stock_plot, trend_plot, ncol = 2)


# Set a seed for reproducibility
set.seed(123)

# Create a sequence of months
months <- seq(as.Date("2022-01-01"), by = "months", length.out = 24)

# Simulate stock price data with fluctuations
stock_prices <- 100 + cumsum(rnorm(length(months), mean = 0.5, sd = 5))

# Create a data frame
stock_data_fluctuations <- data.frame(
  Month = months,
  Stock_Price = stock_prices
)


library(ggplot2)

# Create a line plot of Stock_Price
ggplot(stock_data_fluctuations, aes(x = Month, y = Stock_Price)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Stock Price Over Time with Fluctuations", y = "Price", x = "Month") +
  theme_minimal()



# Fit a linear regression model to the stock price data
lm_model <- lm(Stock_Price ~ Month, data = stock_data_fluctuations)

# Extract the fitted values (trend component) from the model
trend_component <- lm_model$fitted.values

# Create a data frame with the Month and Trend_Component columns
trend_data <- data.frame(Month = stock_data_fluctuations$Month, Trend_Component = trend_component)



library(ggplot2)

# Create a line plot of the Trend_Component
ggplot(trend_data, aes(x = Month, y = Trend_Component)) +
  geom_line(color = "red", size = 1, linetype = "dashed") +
  labs(title = "Extracted Trend Component Over Time", y = "Trend", x = "Month") +
  theme_minimal()
library(ggplot2)

# Create a line plot of the Trend_Component
ggplot(trend_data, aes(x = Month, y = Trend_Component)) +
  geom_line(color = "red", size = 1, linetype = "dashed") +
  labs(title = "Extracted Trend Component Over Time", y = "Trend", x = "Month") +
  theme_minimal()



# Install and load the forecast package if not already installed
# install.packages("forecast")
library(forecast)

# Decompose the time series data
decomposed_data <- stl(ts(stock_data_fluctuations$Stock_Price, frequency = 12), s.window = "periodic")

# Extract the trend component
trend_component <- decomposed_data$time.series[, "trend"]

# Create a data frame with the Month and Trend_Component columns
trend_data <- data.frame(Month = time(decomposed_data$time.series), Trend_Component = trend_component)

# Create a line plot of the Trend_Component
library(ggplot2)

ggplot(trend_data, aes(x = Month, y = Trend_Component)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Extracted Trend Component Over Time", y = "Trend", x = "Month") +
  theme_minimal()

install.packages("forecast")




# Calculate a simple moving average with a window size (e.g., 3 months)
window_size <- 3
trend_component <- zoo::rollmean(stock_data_fluctuations$Stock_Price, k = window_size, fill = NA)

# Create a data frame with the Month and Trend_Component columns
trend_data <- data.frame(Month = stock_data_fluctuations$Month, Trend_Component = trend_component)

# Create a line plot of the Trend_Component
library(ggplot2)

ggplot(trend_data, aes(x = Month, y = Trend_Component)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Extracted Trend Component Using Moving Average", y = "Trend", x = "Month") +
  theme_minimal()



# Fit an AR model to the stock price data
order_of_AR_model <- 1  # Choose the order of the AR model (e.g., 1 for AR(1))
ar_model <- arima(stock_data_fluctuations$Stock_Price, order = c(order_of_AR_model, 0, 0))

# Extract the fitted values (trend component) from the AR model
trend_component <- ar_model$fitted

# Create a data frame with the Month and Trend_Component columns
trend_data <- data.frame(Month = stock_data_fluctuations$Month, Trend_Component = trend_component)

# Create a line plot of the Trend_Component
library(ggplot2)

ggplot(trend_data, aes(x = Month, y = Trend_Component)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Extracted Trend Component Using AR Model", y = "Trend", x = "Month") +
  theme_minimal()


# Fit an AR(1) model to the stock price data
ar_model <- arima(stock_data_fluctuations$Stock_Price, order = c(1, 0, 0))

# Extract the fitted values (trend component) from the AR model
trend_component <- ar_model$fitted

# Create a data frame with the Month and Trend_Component columns
trend_data <- data.frame(Month = stock_data_fluctuations$Month, Trend_Component = trend_component)

# Create a line plot of the Trend_Component
library(ggplot2)

ggplot(trend_data, aes(x = Month, y = Trend_Component)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Extracted Trend Component Using AR(1) Model", y = "Trend", x = "Month") +
  theme_minimal()

# Perform first-order differencing to make the data stationary
differenced_data <- diff(stock_data_fluctuations$Stock_Price, lag = 1)

# Fit an ARIMA(0,1,1) model to the differenced data
arima_model <- arima(differenced_data, order = c(0, 1, 1))

# Integrate the differenced series to obtain the estimated trend component
trend_component <- cumsum(arima_model$residuals)

# Create a data frame with the Month and Trend_Component columns
trend_data <- data.frame(Month = stock_data_fluctuations$Month[-1], Trend_Component = trend_component)

# Create a line plot of the Trend_Component
library(ggplot2)

ggplot(trend_data, aes(x = Month, y = Trend_Component)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Extracted Trend Component Using ARIMA(0,1,1) Model", y = "Trend", x = "Month") +
  theme_minimal()




# Perform first-order differencing to make the data stationary
differenced_data <- diff(stock_data_fluctuations$Stock_Price, lag = 1)

# Fit an ARIMA(0,1,1) model to the differenced data
arima_model <- arima(differenced_data, order = c(0, 1, 1))

# Integrate the differenced series to obtain the estimated trend component
trend_component <- cumsum(arima_model$residuals)

# Create a data frame with the Month and Trend_Component columns
trend_data <- data.frame(Month = stock_data_fluctuations$Month[-1], Trend_Component = trend_component)

# Create a line plot of the Trend_Component
library(ggplot2)

ggplot(trend_data, aes(x = Month, y = Trend_Component)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Extracted Trend Component Using ARIMA(0,1,1) Model", y = "Trend", x = "Month") +
  theme_minimal()




install.packages("ggplot2")
# Install and load the necessary library if not already installed
# install.packages("ggplot2")
library(ggplot2)
# Create a vector of daily stock prices
stock_prices <- c(50.00, 51.25, 52.00, 51.75, 52.50, 53.00, 52.75, 52.25, 52.50, 53.25, 54.00, 54.50)
# Calculate and plot autocorrelations
autocorrelation_result <- acf(stock_prices, lag.max = 30)  # You can adjust the lag.max value as needed

# Plot autocorrelations
plot(autocorrelation_result, main = "Autocorrelations of Daily Stock Prices")
# Load the ggplot2 library (if not already loaded)
library(ggplot2)

# Create a line plot of the Trend_Component
ggplot(trend_data, aes(x = Month, y = Trend_Component)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Extracted Trend Component Using ARIMA(0,1,1) Model", y = "Trend", x = "Month") +
  theme_minimal()


# Create a line plot of the Trend_Component
ggplot(trend_data, aes(x = Month, y = Trend_Component)) +
  geom_line(color = "red", linewidth = 1) +  # Use linewidth instead of size
  labs(title = "Extracted Trend Component Using ARMA(0,1,1) Model", y = "Trend", x = "Month") +
  theme_minimal()

