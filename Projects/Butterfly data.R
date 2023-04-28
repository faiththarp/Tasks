setwd('~/Desktop/Evolution/Tasks/Projects')
# The higher the mass of a butterfly the shortest the distance it will glide.
> View(Butterfly_data)
> View(Butterfly_data)
> library(readxl)
> Updated_Butterfly_data <- read_excel("Updated Butterfly data.xlsx")
View(Updated_Butterfly_data)
> # The higher the mass of a butterfly the shortest the distance it will glide
  > data <- data.frame(Updated_Butterfly-data)
# The higher the mass of a butterfly the shortest the distance it will glide
> library(Updated Butterfly data)
# The higher the mass of a butterfly the shortest the distance it will glide
> library("ggplot2")
> read.csv("/Users/faiththarp/Desktop/Updated Butterfly.data.csv")
read.csv("Updated Butterfly.data.csv")
dir()
Data <- read.csv("Updated Butterfly data.csv")
head(Data)
plot(Data[,3], Data[,2])
Cols <- length(unique(Data[,1]))
Cols
Cols <- rainbow(length(unique(Data[,1])))
names(Cols) <- unique(Data[,1])
pdf("butterflydata.pdf")
plot(Data[,3], Data[,2], pch=16, col=Cols[Data[,1]])
par(mar=c(4,5,1,1), las=1, mgp=c(1.75, 0.5, 0), tck=-0.01)
plot(Data[,3], Data[,2], pch=16, col=Cols[Data[,1]], xlab="distance", ylab="weight")
dev.off()
# Read in butterfly data from a CSV file
Data <- read.csv("Updated Butterfly data.csv")

# Print the first few rows of the data
head(Data)

# Create a scatterplot of weight versus distance
plot(Data[,3], Data[,2])

# Determine the number of unique butterfly species in the data
Cols <- length(unique(Data[,1]))
Cols

# Generate a vector of colors for each butterfly species
Cols <- rainbow(length(unique(Data[,1])))
names(Cols) <- unique(Data[,1])

# Save the scatterplot as a PDF file
pdf("butterflydata.pdf")

# Customize the scatterplot with colors for each butterfly species
plot(Data[,3], Data[,2], pch=16, col=Cols[Data[,1]])

# Adjust the margins, label sizes, and tick marks
par(mar=c(4,5,1,1), las=1, mgp=c(1.75, 0.5, 0), tck=-0.01)

# Add labels to the axes
plot(Data[,3], Data[,2], pch=16, col=Cols[Data[,1]], xlab="distance", ylab="weight")

# Legend for butterfly species
legend("topright", legend=names(Cols), col=Cols, pch=16)

# Create data frame
data <- data.frame(Species = c("Achilles", "Aurora", "Cisseis", "Deidamia", "Godartii", "Helenor", "Marcus", "Menelaus", "Rhetenor", "Sulkowskyi", "Theseus"),
                   Distance = c(4.0389, 3.2975, 4.9151, 4.6201, 4.0093, 4.6949, 4.0939, 5.0825, 3.8943, 4.3779, 4.0284),
                   Weight = c(0.5865, 0.1000, 0.5809, 0.6390, 0.6250, 0.4929, 0.2199, 0.5990, 0.5562, 9.5000, 0.3125))

# Check normality assumption
par(mfrow=c(1,2))
hist(data$Distance, main="Average Distance", xlab="Distance (m)")
qqnorm(data$Distance); qqline(data$Distance)
hist(data$Weight, main="Average Weight", xlab="Weight (g)")
qqnorm(data$Weight); qqline(data$Weight)

# Check homogeneity of variances assumption
par(mfrow=c(1,1))
boxplot(Distance ~ Species, data=data, main="Average Distance", ylab="Distance (m)", xlab="Species")
boxplot(Weight ~ Species, data=data, main="Average Weight", ylab="Weight (g)", xlab="Species")
leveneTest(Distance ~ Species, data=data)
leveneTest(Weight ~ Species, data=data)

# Perform ANOVA test
fit <- lm(cbind(Distance, Weight) ~ Species, data=data)
anova(fit)


# create data frame
data <- data.frame(
  species = c("Achilles", "Aurora", "Cisseis", "Deidamia", "Godartii", "Helenor", "Marcus", "Menelaus", "Rhetenor", "Sulkowskyi", "Theseus"),
  distance = c(4.0389, 3.2975, 4.9151, 4.6201, 4.0093, 4.6949, 4.0939, 5.0825, 3.8943, 4.3779, 4.0284),
  weight = c(0.5865, 0.1, 0.5809, 0.639, 0.625, 0.4929, 0.2199, 0.599, 0.5562, 9.5, 0.3125)
)

# conduct ANOVA
result <- aov(weight ~ species, data = data)

# print ANOVA table
summary(result)

# conduct Tukey's HSD test
tukey_result <- TukeyHSD(result)

# print Tukey's HSD table
tukey_result


# Load the ggplot2 library
library(ggplot2)

# Create a box plot for the average distance
ggplot(data, aes(x = Species, y = Distance)) +
  geom_boxplot() +
  labs(x = "Species", y = "Average Distance (m)") +
  ggtitle("Box plot of Average Distance by Species")

# Create a box plot for the average weight
ggplot(data, aes(x = Species, y = Weight)) +
  geom_boxplot() +
  labs(x = "Species", y = "Average Weight (g)") +
  ggtitle("Box plot of Average Weight by Species")

# Create a dataframe with the given data
data <- data.frame(Species = c("Achilles", "Aurora", "Cisseis", "Deidamia", "Godartii", "Helenor", "Marcus", "Menelaus", "Rhetenor", "Sulkowskyi", "Theseus"),
                   Distance = c(4.0389, 3.2975, 4.9151, 4.6201, 4.0093, 4.6949, 4.0939, 5.0825, 3.8943, 4.3779, 4.0284),
                   Weight = c(0.5865, 0.1, 0.5809, 0.639, 0.625, 0.4929, 0.2199, 0.599, 0.5562, 9.5, 0.3125))

# Perform ANOVA test on Distance
distance_model <- aov(Distance ~ Species, data = data)
summary(distance_model)

# Perform ANOVA test on Weight
weight_model <- aov(Weight ~ Species, data = data)
summary(weight_model)

# Create a dataframe of data
data <- data.frame(
  species = c("Achilles", "Aurora", "Cisseis", "Deidamia", "Godartii", "Helenor", "Marcus", "Menelaus", "Rhetenor", "Sulkowskyi", "Theseus"),
  mean = c(4.0389, 3.2975, 4.9151, 4.6201, 4.0093, 4.6949, 4.0939, 5.0825, 3.8943, 4.3779, 4.0284),
  se = c(0.5865, 0.1000, 0.5809, 0.6390, 0.6250, 0.4929, 0.2199, 0.5990, 0.5562, 9.5000, 0.3125)
)

# Calculate lower and upper bounds of confidence interval
data$lower <- data$mean - 1.96 * data$se
data$upper <- data$mean + 1.96 * data$se

# Create plot with error bars
ggplot(data, aes(x = species, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(title = "95% Confidence Interval") +
  theme_minimal() +
  xlab("Species") +
  ylab("Mean value")
dev.off()

