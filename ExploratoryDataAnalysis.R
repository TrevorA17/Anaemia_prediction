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

# Quick check on data
summary(AnaemiaData)

#View dataset
View(AnaemiaData)

# Measures of Frequency
# Frequency of the target variable
table(AnaemiaData$Anaemic)

# Frequency of Hb ranges (e.g. grouping Hb levels)
cut_Hb <- cut(AnaemiaData$Hb, breaks = c(0, 7, 10, 13, 16, Inf), right = FALSE)
table(cut_Hb)

# Measures of Central Tendency
# Mean
mean(AnaemiaData$Red_Pixel)
mean(AnaemiaData$Green_pixel)
mean(AnaemiaData$Blue_pixel)
mean(AnaemiaData$Hb)

# Median
median(AnaemiaData$Red_Pixel)
median(AnaemiaData$Green_pixel)
median(AnaemiaData$Blue_pixel)
median(AnaemiaData$Hb)

# Mode function
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

get_mode(AnaemiaData$Anaemic)

# Measures of Distribution
# Range
range(AnaemiaData$Hb)

# Variance and standard deviation
var(AnaemiaData$Hb)
sd(AnaemiaData$Hb)

# Skewness and kurtosis (requires e1071)
library(e1071)
skewness(AnaemiaData$Hb)
kurtosis(AnaemiaData$Hb)

# Measures of Relationship
# Correlation matrix among pixel values and Hb
cor(AnaemiaData[, c("Red_Pixel", "Green_pixel", "Blue_pixel", "Hb")])

# Chi-square test between Anaemic and binned Hb
Hb_group <- cut(AnaemiaData$Hb, breaks = c(0, 10, 12, Inf), labels = c("Low", "Medium", "High"))
chisq.test(table(Hb_group, AnaemiaData$Anaemic))

# One-way ANOVA: Does Hb significantly differ by Anaemic status?
anova_result <- aov(Hb ~ Anaemic, data = AnaemiaData)

# Summary of ANOVA test
summary(anova_result)

# Tukey HSD post-hoc test
TukeyHSD(anova_result)
