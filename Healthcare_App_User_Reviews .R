
# Load necessary library
library(dplyr)

# Read the csv file
reviews_df <- read.csv("reviews.csv")

# Display the first few rows
head(reviews_df)


#for analysis i will be starting with the following tasks:
1. Count the number of reviews for each app.
2. Calculate the average rating for each app.
3. Find the number of reviews for each app
4. Extract the date from the date-time string and count the number of reviews per day.


# Count the number of reviews for each app
review_counts <- reviews_df %>%
  group_by(Healthcare.App) %>%
  summarise(Count = n())

# Calculate the average rating for each app
average_rating <- reviews_df %>%
  group_by(Healthcare.App) %>%
  summarise(Avg.Rating = mean(Rating))

# Count the number of reviews for each rating score
rating_counts <- reviews_df %>%
  group_by(Rating) %>%
  summarise(Count = n())

# Extract the date from the date-time string and count the number of reviews per day
reviews_df$Date <- as.Date(reviews_df$Date, format = "%d-%m-%Y")
reviews_per_day <- reviews_df %>%
  group_by(Date) %>%
  summarise(Count = n())


#visualization using the "ggplot2" library

# Load necessary library
library(ggplot2)

# Bar plot of the number of reviews for each app to understand which apps have the most feedback from users
ggplot(data = review_counts, aes(x = Healthcare.App, y = Count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Healthcare App", y = "Number of Reviews", title = "Number of Reviews per App")


# Bar plot of the average rating for each app to understand how users generally feel about each app.
ggplot(data = average_rating, aes(x = Healthcare.App, y = Avg.Rating)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Healthcare App", y = "Average Rating", title = "Average Rating per App")

# Histogram of the ratings to understand the distribution of ratings across all reviews.
ggplot(data = reviews_df, aes(x = Rating)) +
  geom_histogram(binwidth = 1, color = "black", fill = "blue") +
  labs(x = "Rating", y = "Count", title = "Distribution of Ratings")


#Time series plot of the number of reviews per day to understand trends in the volume of reviews over time.
ggplot(data = reviews_per_day, aes(x = Date, y = Count)) +
  geom_line() +
  labs(x = "Date", y = "Number of Reviews", title = "Number of Reviews Over Time")














