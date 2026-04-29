# Load libraries
library(ggplot2)
library(dplyr)

# Example dataset (θα το επεκτείνουμε)
data <- data.frame(
  Year = 2024:2030,
  Orthodox_Easter = as.Date(c("2024-05-05","2025-04-20","2026-04-12","2027-05-02","2028-04-16","2029-04-08","2030-04-28")),
  Catholic_Easter = as.Date(c("2024-03-31","2025-04-20","2026-04-05","2027-03-28","2028-04-16","2029-04-01","2030-04-21"))
)

# Calculate difference
data$Difference <- as.numeric(data$Orthodox_Easter - data$Catholic_Easter)

# Plot
ggplot(data, aes(x = Year, y = Difference)) +
  geom_line() +
  ggtitle("Difference between Orthodox and Catholic Easter")
