# Load necessary libraries
library(readr)
library(dplyr)
library(mice)
library(tidyr)
library(ggplot2)

# Load and impute data
data <- read.csv("marketing_campaign.csv", sep = ";")
summary(data)
imputed_data <- mice(data, m = 1, method = 'pmm', maxit = 5, seed = 500)
data <- complete(imputed_data)
head(data)

# Segment income into three categories: Low, Medium, High
data <- data %>%
  mutate(Income_Segment = case_when(
    Income < quantile(Income, 0.33) ~ "Low",
    Income < quantile(Income, 0.66) ~ "Medium",
    TRUE ~ "High"
  ))

# Plotting the data with Income Segments -------------------------------------

# Number of samples according to Education, segmented by Income
ggplot(data, aes(x = Education, fill = Income_Segment)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Number of Samples According to Education, Segmented by Income")

# Income distribution by Education level, segmented by Income
ggplot(data, aes(x = Education, y = Income, fill = Income_Segment)) +
  geom_violin(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Income Distribution by Education Level, Segmented by Income")

# Relationship between KidHome and Income, segmented by Income
ggplot(data, aes(x = Kidhome, y = Income, color = Income_Segment)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Relationship Between KidHome and Income, Segmented by Income")

# Relationship between TeenHome and Income, segmented by Income
ggplot(data, aes(x = Teenhome, y = Income, color = Income_Segment)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Relationship Between TeenHome and Income, Segmented by Income")

# Relationship between MntWines and NumWebVisitsMonth, segmented by Income
ggplot(data, aes(x = MntWines, y = NumWebVisitsMonth, color = Income_Segment)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Relationship Between NumWebVisitsMonth and MntWines, Segmented by Income")

# Relationship between Income and NumStorePurchases, segmented by Income
ggplot(data, aes(x = Income, y = NumStorePurchases, color = Income_Segment)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Relationship Between NumStorePurchases and Income, Segmented by Income")


# Response Rate by Education, segmented by Income ----------------------------

response_education <- data %>%
  count(Response, Education, Income_Segment) %>%
  group_by(Education, Income_Segment) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(response_education, aes(x = "", y = percentage, fill = factor(Response))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  facet_grid(Income_Segment ~ Education) +
  theme_void() +
  labs(title = "Response Rate by Education, Segmented by Income")

# Total Spent for Products by Marital Status, segmented by Income ------------

total_spent <- data %>%
  group_by(Marital_Status, Income_Segment) %>%
  summarize(across(starts_with("Mnt"), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = starts_with("Mnt"), names_to = "Product", values_to = "Total")

ggplot(total_spent, aes(x = Marital_Status, y = Total, fill = Product)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Income_Segment) +
  theme_minimal() +
  labs(title = "Total Spent for Products According to Marital Status, Segmented by Income")


# Purchase Type According to Age, segmented by Income ------------------------

purchase_type_age <- data %>%
  group_by(Year_Birth, Income_Segment) %>%
  summarize(across(c("NumWebPurchases", "NumCatalogPurchases", "NumStorePurchases"), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = c("NumWebPurchases", "NumCatalogPurchases", "NumStorePurchases"),
               names_to = "PurchaseType", values_to = "Total")

ggplot(purchase_type_age, aes(x = Year_Birth, y = Total, fill = PurchaseType)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Income_Segment) +
  theme_minimal() +
  labs(title = "Number of Purchase Type According to Age, Segmented by Income")

