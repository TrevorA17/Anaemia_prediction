# Load anaemia dataset with appropriate column types
AnaemiaData <- read.csv("data/anemia_dataset.csv", colClasses = c(
  Red_Pixel = "numeric",
  Green_pixel = "numeric",
  Blue_pixel = "numeric",
  Hb = "numeric",           # Haemoglobin levels
  Anaemic = "factor"        # Yes or No (classification target)
))

# View the structure of the dataset
str(AnaemiaData)

# Check if any missing values exist
any(is.na(AnaemiaData))  # Returns TRUE if any NA is present

# Count of missing values per column
colSums(is.na(AnaemiaData))

# Summary to visually inspect data completeness
summary(AnaemiaData)
