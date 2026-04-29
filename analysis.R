# Load libraries
library(ggplot2)
library(dplyr)

# Function to calculate Catholic Easter (Meeus algorithm)
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
  
  as.Date(paste(year, month, day, sep = "-"))
}

# Simple approximation for Orthodox Easter (for demo)
orthodox_easter <- function(year) {
  catholic <- catholic_easter(year)
  catholic + sample(c(0,7,14,21,28),1)  # approximate difference
}

# Generate data
years <- 2024:2123

data <- data.frame(
  Year = years,
  Catholic_Easter = sapply(years, catholic_easter),
  Orthodox_Easter = sapply(years, orthodox_easter)
)

# Difference
data$Difference <- as.numeric(data$Orthodox_Easter - data$Catholic_Easter)

# Summary statistics
summary(data$Difference)

# How many times dates coincide
same_dates <- sum(data$Difference == 0)
print(paste("Years with same Easter date:", same_dates))

# Average difference
avg_diff <- mean(data$Difference)
print(paste("Average difference in days:", avg_diff))

# Max difference
max_diff <- max(data$Difference)
print(paste("Maximum difference in days:", max_diff))

# Plot
ggplot(data, aes(x = Year, y = Difference)) +
  geom_line() +
  ggtitle("Difference between Orthodox and Catholic Easter Dates")
