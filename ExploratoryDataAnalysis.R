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

# Histogram for Hb
hist(AnaemiaData$Hb, main = "Distribution of Hb Levels", xlab = "Hb", col = "skyblue", border = "white")

# Bar plot for Anaemic status
barplot(table(AnaemiaData$Anaemic), main = "Anaemic Status Frequency", col = c("red", "green"), ylab = "Count")

# Boxplots for pixel values
boxplot(AnaemiaData$Red_Pixel, main = "Red Pixel Distribution", col = "tomato")
boxplot(AnaemiaData$Green_pixel, main = "Green Pixel Distribution", col = "lightgreen")
boxplot(AnaemiaData$Blue_pixel, main = "Blue Pixel Distribution", col = "lightblue")

# Boxplot of Hb by Anaemic status
boxplot(Hb ~ Anaemic, data = AnaemiaData,
        main = "Hb Levels by Anaemic Status", col = c("red", "green"),
        ylab = "Haemoglobin (Hb)")

# Scatterplot matrix for pixel values and Hb
pairs(AnaemiaData[, c("Red_Pixel", "Green_pixel", "Blue_pixel", "Hb")],
      main = "Scatterplot Matrix", col = AnaemiaData$Anaemic)

# Colored scatterplot: Hb vs Red_Pixel
plot(AnaemiaData$Red_Pixel, AnaemiaData$Hb,
     col = ifelse(AnaemiaData$Anaemic == "Yes", "red", "green"),
     pch = 19, xlab = "Red Pixel", ylab = "Hb", main = "Hb vs Red Pixel")

legend("topright", legend = c("Anaemic", "Non-Anaemic"),
       col = c("red", "green"), pch = 19)
