# 1. Correlations
data(iris)
correlations <- cor(iris[,1:4], use = "complete.obs")
correlations
# I can see both. 
# A high positive correlation between the petal width-length, this indicates that there is a strong linear relationship.
# But for sepal the correlation is negative and really low, since its close to zero it indicates that there is a very weak linear relationship.

# 2. Plot Sepal.Width against Sepal.Length
library(ggplot2)
plot1 <- ggplot(iris) +
  aes(x = Sepal.Width, y = Sepal.Length) +
  geom_point(colour = "#112446") +
  theme_minimal()
plot1
# Perfect match. The points for sepal is very scattered, so its easy to see that there is a very weak linear relationship between width-length.
# I did also plot Petal, just to confirm the suspected high linear relationship between width-length.

# 3. Fit a linear model using Sepal.Width as predictor and Sepal.Length as response
model1 <- iris |>
  lm(formula = Sepal.Length ~ Sepal.Width)
model1
# Yes, it does match. The weak slope (-0.2234), does confirm the very weak linear relationship observed in task 1 and 2.


# 4. Setosa correlations
correlations_setosa <- cor(iris[iris$Species == "setosa",1:4], use = "complete.obs")
correlations_setosa
# There is no negative values anymore. 
# And i can observe that the correlation is alot higher for sepal compared petal this time around.
# This indicates that setosa have a stronger linear relationship for sepal (width-length).
# This is like the complete opposite to the overall correlations, which had a very weak linear relationship for sepal.

# 5. Plot Sepal.Width against Sepal.Length, color by species
library(ggplot2)
plot2 <- ggplot(iris) +
  aes(x = Sepal.Width, y = Sepal.Length, colour = Species) +
  geom_point() +
  scale_color_hue(direction = 1) +
  theme_minimal()
plot2
# Yes, because its easy to observe a linear relationship for the species setosa in the plot.

# 6. Fit second model using species and Sepal.Width as predictors and Sepal.Length as response
model2 <- iris |>
  lm(formula = Sepal.Length ~ Sepal.Width + Species)
summary(model2)
# The relationship between Sepal.Width and Sepal.Length is now adjusted to species, 
# and now we can see how each species contributes to Sepal.Length.

# 7. Predict the sepal length of a setosa with a sepal width of 3.6 cm
prediction <- model2 |>
  predict(data.frame(Sepal.Width = 3.6, Species = "setosa"))
prediction
summary(iris[iris$Species == "setosa", "Sepal.Length"])
# I do think the prediction seems reasonable, because its both close to the mean and the median of sepal length for setosas.

# Download the dataset from Canvas an place in current directory
getwd()

# Load the data
library(tidyverse)
diabetes_data <- read_csv("a2_diabetes.csv") # Don't change this line!

# Glucose seemed very relevant because of the lecture, but also BMI and age because common knowledge.

# 8. Recode Outcome as a factor
diabetes_data$Outcome <- as.factor(diabetes_data$Outcome)

# 9. Find a good logistic regression model
diabetes_data_no_na <- diabetes_data

mean_glucose <- mean(diabetes_data$Glucose, na.rm = TRUE)
mean_blood_pressure <- mean(diabetes_data$BloodPressure, na.rm = TRUE)
mean_skin_thickness <- mean(diabetes_data$SkinThickness, na.rm = TRUE)
mean_insulin <- mean(diabetes_data$Insulin, na.rm = TRUE)
mean_bmi <- mean(diabetes_data$BMI, na.rm = TRUE)

imputed_data <- diabetes_data_no_na |>
  mutate(
    Glucose = replace_na(Glucose, mean_glucose),
    BloodPressure = replace_na(BloodPressure, mean_blood_pressure),
    SkinThickness = replace_na(SkinThickness, mean_skin_thickness),
    Insulin = replace_na(Insulin, mean_insulin),
    BMI = replace_na(BMI, mean_bmi)
  )

logistic_model <- imputed_data |> glm(formula = Outcome ~ Glucose + BMI + Age, family = "binomial")

accuracy <- imputed_data |> 
  mutate(prediction = ifelse(predict(logistic_model, imputed_data, type = "response") > 0.5, "pos", "neg")) |> 
  summarize(accuracy = mean(prediction == ifelse(Outcome == 1, "pos", "neg"), na.rm = TRUE))
# I think that using the same data for regression and evaluation risks overfitting,
# leading to overly optimistic accuracy that may not generalize to new data.