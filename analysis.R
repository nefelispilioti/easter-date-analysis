# Load libraries
library(ggplot2)
library(dplyr)

# --- FUNCTIONS ---

# 1. Function for Catholic Easter (Gregorian Calendar)
catholic_easter <- function(year) {
  a <- year %% 19
  b <- year %/% 100
  c <- year %% 100
  d <- b %/% 4
  e <- b %% 4
  f <- (b + 8) %/% 25
  g <- (b - f + 1) %/% 3
  h <- (19*a + b - d - g + 15) %% 30
  i <- c %/% 4
  k <- c %% 4
  l <- (32 + 2*e + 2*i - h - k) %% 7
  m <- (a + 11*h + 22*l) %/% 451
  month <- (h + l - 7*m + 114) %/% 31
  day <- ((h + l - 7*m + 114) %% 31) + 1
  
  return(as.Date(paste(year, month, day, sep = "-")))
}

# 2. Function for Orthodox Easter (Julian Calendar calculation adjusted to Gregorian)
# Using the Meeus algorithm for the Julian calendar
orthodox_easter <- function(year) {
  a <- year %% 4
  b <- year %% 7
  c <- year %% 19
  d <- (19 * c + 15) %% 30
  e <- (2 * a + 4 * b - d + 34) %% 7
  month <- (d + e + 114) %/% 31
  day <- ((d + e + 114) %% 31) + 1
  
  # Conversion to Gregorian calendar (for the years 2024-2123, the difference is 13 days)
  # This makes the dates comparable for your analysis
  julian_date <- as.Date(paste(year, month, day, sep = "-"))
  gregorian_date <- julian_date + 13
  
  return(gregorian_date)
}

# --- DATA GENERATION ---

years <- 2024:2123
data <- data.frame(Year = years)

# Correctly applying functions to maintain Date format
data$Catholic_Easter <- do.call(c, lapply(years, catholic_easter))
data$Orthodox_Easter <- do.call(c, lapply(years, orthodox_easter))

# Calculate the difference in days
data$Difference <- as.numeric(data$Orthodox_Easter - data$Catholic_Easter)

# --- ANALYSIS & OUTPUT ---

# 1. Basic Stats
cat("--- Statistical Summary of Differences (in days) ---\n")
print(summary(data$Difference))

# 2. Insightful Metrics
same_dates <- sum(data$Difference == 0)
cat("\nTotal years with same Easter date (out of 100):", same_dates)

avg_diff <- mean(data$Difference)
cat("\nAverage difference:", round(avg_diff, 2), "days")

max_diff <- max(data$Difference)
cat("\nMaximum observed difference:", max_diff, "days\n")

# --- VISUALIZATION ---

ggplot(data, aes(x = Year, y = Difference)) +
  geom_line(color = "#2c3e50", size = 1) +
  geom_point(aes(color = (Difference == 0)), size = 2) +
  scale_color_manual(values = c("black", "red"), labels = c("Different", "Same Date")) +
  theme_minimal() +
  labs(
    title = "Analysis of Easter Date Deviations (2024-2123)",
    subtitle = "Comparing Orthodox vs Catholic Easter Dates",
    x = "Year",
    y = "Difference in Days",
    color = "Coincidence"
  ) +
  theme(legend.position = "bottom")

