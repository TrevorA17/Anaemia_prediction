# Load anaemia dataset with appropriate column types
AnaemiaData <- read.csv("data/anemia_dataset.csv", colClasses = c(
  Red_Pixel = "numeric",
  Green_pixel = "numeric",
  Blue_pixel = "numeric",
  Hb = "numeric",           # Haemoglobin levels
  Anaemic = "factor"        # Yes or No (classification target)
))

# Load required library
library(caret)

# Set seed for reproducibility
set.seed(123)

# Create index for 70% training data
train_index <- createDataPartition(AnaemiaData$Anaemic, p = 0.7, list = FALSE)

# Split the data
train_data <- AnaemiaData[train_index, ]
test_data  <- AnaemiaData[-train_index, ]

# Confirm dimensions
cat("Training set rows:", nrow(train_data), "\n")
cat("Testing set rows:", nrow(test_data), "\n")

# Load required libraries
library(caret)

# Set seed for reproducibility
set.seed(123)

# Define bootstrap control
bootstrap_control <- trainControl(
  method = "boot",
  number = 100,              # 100 bootstrap resamples
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

# Make sure the response variable is a factor with valid levels
train_data$Anaemic <- factor(train_data$Anaemic, levels = c("Yes", "No"))

# Train a logistic regression model with bootstrapping
boot_model <- train(
  Anaemic ~ ., data = train_data,
  method = "glm",
  family = "binomial",
  trControl = bootstrap_control,
  metric = "ROC"
)

# Output model summary
print(boot_model)

# Load required library
library(caret)

# Set seed for reproducibility
set.seed(123)

# Define 10-fold cross-validation control
cv_control <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# Ensure the response variable is a factor with correct levels
train_data$Anaemic <- factor(train_data$Anaemic, levels = c("Yes", "No"))

# Train a logistic regression model using cross-validation
cv_model <- train(
  Anaemic ~ ., data = train_data,
  method = "glm",
  family = "binomial",
  trControl = cv_control,
  metric = "ROC"
)

# Print model summary
print(cv_model)

# Load required packages
library(caret)
library(gbm)
library(randomForest)

# Set seed for reproducibility
set.seed(123)

# Ensure target is a factor
train_data$Anaemic <- factor(train_data$Anaemic, levels = c("Yes", "No"))

# Define common CV control
cv_control <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# Train Logistic Regression
model_glm <- train(
  Anaemic ~ ., data = train_data,
  method = "glm",
  family = "binomial",
  trControl = cv_control,
  metric = "ROC"
)

# Train Random Forest
model_rf <- train(
  Anaemic ~ ., data = train_data,
  method = "rf",
  trControl = cv_control,
  metric = "ROC"
)

# Train Gradient Boosting Machine
model_gbm <- train(
  Anaemic ~ ., data = train_data,
  method = "gbm",
  verbose = FALSE,
  trControl = cv_control,
  metric = "ROC"
)

# Print summaries
print(model_glm)
print(model_rf)
print(model_gbm)

# Compare the three models
results <- resamples(list(
  GLM = model_glm,
  RF = model_rf,
  GBM = model_gbm
))

# Summary statistics (ROC, Sensitivity, Specificity)
summary(results)

# Boxplots for comparison
bwplot(results, metric = "ROC")
bwplot(results, metric = "Sens")
bwplot(results, metric = "Spec")

# Dotplot comparison
dotplot(results, metric = "ROC")

