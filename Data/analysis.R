# Load libraries
library(dplyr)
library(lubridate)
library(purrr)
library(corrplot)
library(reshape2)
library(ggplot2)
library(cluster)
library(factoextra)
library(plotly)
library(e1071)
library(MASS)
library(caret)
library(mice)

select = dplyr::select

# Data Loading and Preprocessing ----------------------------------------------

# Load and impute data
data <- read.csv("marketing_campaign.csv", sep = ";")
imputed_data <- mice(data, m = 1, method = 'pmm', maxit = 5, seed = 500)
data <- complete(imputed_data)

# Convert date and extract year and month
data$Dt_Customer <- as.Date(data$Dt_Customer, format = "%Y-%m-%d")
data <- data %>%
  mutate(Year = year(Dt_Customer),
         Month = month(Dt_Customer))

# Categorize age groups and segment income
data <- data %>%
  mutate(Age = case_when(
    Year_Birth <= 1959 ~ "Elderly",
    Year_Birth > 1959 & Year_Birth <= 1977 ~ "MiddleAge",
    TRUE ~ "Young"
  ),
  Income_Segment = case_when(
    Income < quantile(Income, 0.33) ~ "Low",
    Income < quantile(Income, 0.66) ~ "Medium",
    TRUE ~ "High"
  ))

# Remove unused columns
data <- data %>% select(-Dt_Customer, -ID, -Z_CostContact, -Z_Revenue, -Year_Birth)

# Convert specific columns to numeric
columns_to_convert <- c("Education", "Marital_Status", "Age", "Income_Segment")
data[columns_to_convert] <- lapply(data[columns_to_convert], function(x) as.numeric(as.factor(x)))

# Define variables for inference and correlation
inference_col <- c("Education", "Marital_Status", "Age", "Year", "Income_Segment")
data_inf <- data %>% select(all_of(inference_col))
data_cor <- data %>% select(-all_of(inference_col))

# PCA Analysis and Scree Plot ------------------------------------------------

# Perform PCA and Scree plot
pca_result <- prcomp(data_cor, center = TRUE, scale. = TRUE)
variance <- pca_result$sdev^2
proportion_variance <- variance / sum(variance)
pca_df <- data.frame(Principal_Component = seq_along(proportion_variance), Proportion_Variance = proportion_variance)

ggplot(pca_df, aes(x = Principal_Component, y = Proportion_Variance)) +
  geom_point() +
  geom_line() +
  labs(title = "Scree Plot", x = "Principal Component", y = "Proportion of Variance Explained") +
  theme_minimal()

# PCA plot for each inference variable
pca_scores <- as.data.frame(pca_result$x[, 1:2])
colnames(pca_scores) <- c("PC1", "PC2")

plot_pca_by_group <- function(group_var) {
  pca_scores$Group <- data[[group_var]]
  ggplot(pca_scores, aes(x = PC1, y = PC2, color = as.factor(Group))) +
    geom_point(alpha = 0.7) +
    labs(title = paste("PCA Plot by", group_var), x = "Principal Component 1", y = "Principal Component 2", color = group_var) +
    theme_minimal()
}
plots <- map(inference_col, plot_pca_by_group)
plots

# Correlation Heatmap ---------------------------------------------------------

# Convert inference columns to numeric for correlation calculation
data_inf[] <- lapply(data_inf, function(x) if (is.factor(x) || is.character(x)) as.numeric(as.character(x)) else x)
cor_matrix <- cor(data_inf)

# Plot correlation heatmap
melted_cor_matrix <- melt(cor_matrix)
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Heatmap", x = "", y = "")
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

# Optimal Number of Clusters --------------------------------------------------

# Elbow method for optimal clusters
wss <- sapply(1:10, function(k) kmeans(data_cor, centers = k, nstart = 25)$tot.withinss)
elbow_plot <- data.frame(Clusters = 1:10, WSS = wss)
ggplot(elbow_plot, aes(x = Clusters, y = WSS)) +
  geom_point() +
  geom_line() +
  labs(title = "Elbow Method for Optimal Number of Clusters", x = "Number of Clusters", y = "Within-cluster Sum of Squares")

# Apply K-means clustering with optimal clusters (e.g., k=3) ------------------

set.seed(123)
optimal_k <- 3
kmeans_result <- kmeans(as.data.frame(pca_result$x[, 1:5]), centers = optimal_k, nstart = 25)
data$Cluster <- factor(kmeans_result$cluster)

# 2D PCA plot with clusters
pca_scores <- as.data.frame(pca_result$x[, 1:3])
colnames(pca_scores) <- c("PC1", "PC2", "PC3")
pca_scores$Cluster <- data$Cluster

ggplot(pca_scores, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(alpha = 0.7) +
  labs(title = "K-means Clustering (k = 3) Visualized on PCA Components", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()

# Optional 3D PCA plot
plot_ly(pca_scores, x = ~PC1, y = ~PC2, z = ~PC3, color = ~Cluster, type = "scatter3d", mode = "markers") %>%
  layout(title = "3D Visualization of Clusters on PCA Components")

# Contingency Tables for Inference Variables ----------------------------------

contingency_tables <- lapply(inference_col, function(var) table(data[[var]], data$Cluster))
names(contingency_tables) <- inference_col
contingency_tables

# LDA, QDA, and Naive Bayes for Classification --------------------------------

class_vars <- c("Education", "Marital_Status", "Age", "Year", "Income_Segment")

results <- data.frame(
  Class_Variable = character(),
  Model = character(),
  Misclassification_Rate = numeric(),
  stringsAsFactors = FALSE
)

# Evaluate LDA, QDA, and Naive Bayes for each class variable
for (class_var in class_vars) {

  set.seed(123)
  trainIndex <- createDataPartition(data[[class_var]], p = 0.9, list = FALSE)
  train_data <- data[trainIndex, ]
  test_data <- data[-trainIndex, ]

  # LDA
  lda_model <- lda(as.formula(paste(class_var, "~ .")), data = train_data)
  lda_pred <- predict(lda_model, test_data)$class
  lda_misclassification_rate <- mean(lda_pred != test_data[[class_var]])
  results <- rbind(results, data.frame(Class_Variable = class_var, Model = "LDA", Misclassification_Rate = lda_misclassification_rate))

  # QDA
  qda_model <- tryCatch({
    qda(as.formula(paste(class_var, "~ .")), data = train_data)
  }, error = function(e) NULL)
  if (!is.null(qda_model)) {
    qda_pred <- predict(qda_model, test_data)$class
    qda_misclassification_rate <- mean(qda_pred != test_data[[class_var]])
    results <- rbind(results, data.frame(Class_Variable = class_var, Model = "QDA", Misclassification_Rate = qda_misclassification_rate))
  } else {
    results <- rbind(results, data.frame(Class_Variable = class_var, Model = "QDA", Misclassification_Rate = NA))
  }

  # Naive Bayes
  nb_model <- naiveBayes(as.formula(paste(class_var, "~ .")), data = train_data)
  nb_pred <- predict(nb_model, test_data)
  nb_misclassification_rate <- mean(nb_pred != test_data[[class_var]])
  results <- rbind(results, data.frame(Class_Variable = class_var, Model = "Naive Bayes", Misclassification_Rate = nb_misclassification_rate))
}

# Display the misclassification results
print(results)



