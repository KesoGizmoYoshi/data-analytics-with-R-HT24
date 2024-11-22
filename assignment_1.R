library(tidyverse)
data(msleep)
?msleep

# 1. Convert into factors
col_names <- c("genus", "vore", "order", "conservation")
msleep[col_names] <- lapply(msleep[col_names] , factor)

# 2. Shortest sleep time
shortest_sleep <- min(msleep$sleep_total)
shortest_sleep_mammal <- toString(msleep[msleep$sleep_total == shortest_sleep, "name"])

# 3. Most missing
most_missing <- names(msleep)[which.max(colSums(is.na(msleep)))]
missing_values <- sum(is.na(msleep[,most_missing]))

# 4. Correlations
correlations <- cor(msleep[,6:11], use = "complete.obs")
correlations <- msleep |>
  select(where(is.numeric)) |>
  cor(use = "complete.obs")
correlations

# 5. Highest correlation
correlations_copy <- correlations
highest_corr <- max(correlations_copy[correlations_copy < 1])

# 6. Sleep time distribution
sleep_histogram <- ggplot(msleep) +
  aes(x = sleep_total) +
  geom_histogram(bins = 30L, fill = "#7ADEFF") +
  labs(x = "Total sleep", 
       y = "Count") +
  theme_dark()

# 7. Bar chart for food categories

food_barchart <- ggplot(msleep) +
aes(x = vore, fill = name) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

# 8. Grouped box plot for sleep time

sleep_boxplot <- ggplot(msleep) +
aes(x = vore, y = sleep_total) +
  geom_boxplot(fill = "#C5DAFF") +
  theme_minimal()

# 9. Longest average sleep time
avg_sleep <- msleep |>
  group_by(vore) |>
  summarize(average_sleep = mean(sleep_total, na.rm = TRUE))

highest_average <- max(avg_sleep$average_sleep)

# 10. REM sleep vs. total sleep, colored by order
sleep_scatterplot <- ggplot(msleep) +
aes(x = sleep_total, y = sleep_rem, colour = order) +
  geom_point() +
  scale_color_hue(direction = 1) +
  theme_minimal()

# 11. REM sleep vs. total sleep for the order most common in the data
sleep_scatterplot2 <- ggplot(msleep) +
  aes(x = sleep_total, y = sleep_rem) +
  geom_point(colour = "#112446") +
  theme_minimal()
