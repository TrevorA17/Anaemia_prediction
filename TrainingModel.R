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
