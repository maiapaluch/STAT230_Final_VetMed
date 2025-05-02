# Load required packages
library(tidyverse)

# Load the dataset
vet_data <- read_csv("cleaned_canine_clinical_data.csv")

# Check structure
glimpse(vet_data)
summary(vet_data)

# Convert Visit_Outcome into binary recovery variable
vet_data <- vet_data %>%
  mutate(Recovered = if_else(Visit_Outcome == "Recovered", 1, 0),
         Mortality = if_else(Visit_Outcome == "Deceased", 1, 0))

# LOGISTIC REGRESSION: Predict Recovery
model1 <- glm(Recovered ~ Age_Years + Weight_KG + Neuter_Status + Dog_Breed,
              data = vet_data, family = "binomial")

summary(model1)

# PLOT: Predicted recovery probability by age
vet_data %>%
  mutate(prob_recover = predict(model1, type = "response")) %>%
  ggplot(aes(x = Age_Years, y = prob_recover)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  labs(title = "Predicted Recovery Probability by Age",
       x = "Age (Years)",
       y = "Probability of Recovery")

# CHI-SQUARE TEST: Neuter Status vs Mortality
table(vet_data$Neuter_Status, vet_data$Mortality)
chisq.test(table(vet_data$Neuter_Status, vet_data$Mortality))

# DESCRIPTIVE ANALYSIS: Chronic Illness by Breed
vet_data %>%
  filter(Diagnosis_Type == "Chronic") %>%
  group_by(Dog_Breed) %>%
  summarise(Avg_Age = mean(Age_Years), Count = n()) %>%
  ggplot(aes(x = reorder(Dog_Breed, -Avg_Age), y = Avg_Age)) +
  geom_col() +
  coord_flip() +
  labs(title = "Average Age of Chronic Diagnosis by Breed",
       x = "Breed",
       y = "Average Age")

