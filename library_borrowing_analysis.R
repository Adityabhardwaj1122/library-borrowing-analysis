# Load necessary libraries
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")

library(tidyverse)
library(lubridate)
library(ggplot2)

# Load the dataset
url <- "E:/Rdatascience project/library_borrowing.csv"
library_data <- read.csv(url)

# Convert dates to Date format
library_data$BorrowDate <- as.Date(library_data$BorrowDate, format="%Y-%m-%d")
library_data$ReturnDate <- as.Date(library_data$ReturnDate, format="%Y-%m-%d")

# Convert UserAge to numeric
library_data$UserAge <- as.numeric(library_data$UserAge)

# Calculate borrowing duration
library_data$Duration <- as.numeric(library_data$ReturnDate - library_data$BorrowDate)

# Summary statistics
summary_stats <- library_data %>%
  group_by(Genre) %>%
  summarise(
    TotalBorrows = n(),
    AvgDuration = mean(Duration),
    MinDuration = min(Duration),
    MaxDuration = max(Duration)
  )
print(summary_stats)

# Most popular books
popular_books <- library_data %>%
  group_by(BookTitle) %>%
  summarise(Borrows = n()) %>%
  arrange(desc(Borrows)) %>%
  top_n(10)
print(popular_books)

# Plotting
# Borrowing duration by genre
ggplot(library_data, aes(x=Genre, y=Duration)) +
  geom_boxplot() +
  labs(title="Borrowing Duration by Genre", x="Genre", y="Duration (days)")

# Borrowing frequency by age group
library_data <- library_data %>%
  mutate(AgeGroup = cut(UserAge, breaks=c(0, 20, 30, 40, 50, 60, Inf), right=FALSE, labels=c("0-19", "20-29", "30-39", "40-49", "50-59", "60+")))

ggplot(library_data, aes(x=AgeGroup)) +
  geom_bar(fill="skyblue") +
  labs(title="Borrowing Frequency by Age Group", x="Age Group", y="Number of Borrows")

# Borrowing frequency by gender
ggplot(library_data, aes(x=UserGender)) +
  geom_bar(fill="lightgreen") +
  labs(title="Borrowing Frequency by Gender", x="Gender", y="Number of Borrows")

# Borrowing frequency by genre
ggplot(library_data, aes(x=Genre)) +
  geom_bar(fill="salmon") +
  labs(title="Borrowing Frequency by Genre", x="Genre", y="Number of Borrows")

# Show plots
dev.off()
