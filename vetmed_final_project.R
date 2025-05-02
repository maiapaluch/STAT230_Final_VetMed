library(tidyverse)

# load data set
data <- read_csv("canine_clinical_records_2024.csv")

# preview
glimpse(data)

# binary recovery outcome
data <- data %>%
  mutate(Recovered = if_else(Visit_Outcome == 'Recovered', 1, 0))

# logistic regression model
model1 <- glm(Recovered ~ Age_Years + Weight_KG + Neuter_Status + Dog_Breed,
              data = data, family = "binomial")
summary(model1)

# predicted recovery plot
data %>%
  mutate(prob_recover = predict(model1, type = "response")) %>%
  ggplot(aes(x = Age_Years, y = prob_recover)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  labs(title = "Predicted Recovery by Age",
       y = "Probability of Recovery", x = "Age (Years)")

# mortality variable
data <- data %>%
  mutate(Mortality = if_else(Visit_Outcome == 'Deceased', 1, 0))

# chi-square test
table(data$Neuter_Status, data$Mortality)
chisq.test(table(data$Neuter_Status, data$Mortality))

# chronic illness analysis
chronic_data <- data %>%
  filter(Diagnosis_Type == "Chronic")

chronic_data %>%
  group_by(Dog_Breed) %>%
  summarise(Average_Age = mean(Age_Years, na.rm = TRUE),
            Count = n()) %>%
  ggplot(aes(x = reorder(Dog_Breed, -Average_Age), y = Average_Age)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Avg Age of Chronic Diagnosis by Breed", y = "Avg Age", x = "Breed")