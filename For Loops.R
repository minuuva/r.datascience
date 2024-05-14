setwd("/Users/cmw/Desktop/STAT 1601")
final.grades <- read.csv("finalgrades.csv")
nfl <- read.csv("nfl.csv")

# Question 1: Final Grade and Letter Grade ------------------------------------
# a. b.
final.grades$finalgrade = NA
final.grades$lettergrade <- NA
for (i in 1:nrow(final.grades)) {
  final.grades$finalgrade[i] <- weighted.mean(
    final.grades[i, c("Attendance", "Homework", "Exam1", "Exam2", "FinalExam", "Project")], 
    w = c(0.05, 0.25, 0.15, 0.15, 0.15, 0.25), na.rm = TRUE)
  if (final.grades$finalgrade[i] >= 83) {
    final.grades$lettergrade[i] = "Pass"
  } else {
    final.grades$lettergrade[i] = "Fail"
  }
}

# c.
table(final.grades$lettergrade)

# d.
# 4 students failed, 6 students passed

# Question 2: Square-Root of the Median ---------------------------------------

# Part A ______________________________________________________________________
column.medians.length.a <- 14
column.medians <- rep(NA, column.medians.length.a)
j <- 2

for (i in 1:ncol(nfl)) {
  if (is.numeric(nfl[[i]])) {
    column_median <- median(nfl[[i]], na.rm = TRUE)
    column.medians[j] <- column_median
  }
  j = j + 2
}
column.medians

# Part B ______________________________________________________________________
column.medians.length.b <- 23
column.medians <- rep(NA, column.medians.length.b)
j <- 2

for (i in 1:ncol(nfl)) {
  j <- j + 3
  if (is.numeric(nfl[[i]])) {
    column_median <- median(nfl[[i]], na.rm = TRUE)
    column.medians[j] <- column_median
    }
}
column.medians

# Part C ______________________________________________________________________
column.medians.length.c <- 10
column.medians <- rep(NA, column.medians.length.c)
j <- 2

for (i in 1:ncol(nfl)) {
  if (is.numeric(nfl[[i]])) {
    column_median <- median(nfl[[i]], na.rm = TRUE)
    column.medians[j] <- column_median
      j <- j + 2
    }
  }

column.medians

# Question 3: Boxplots --------------------------------------------------------
par(mfrow = c(5, 1), mar = c(2, 2, 2, 2))

for (i in 1:ncol(nfl)) {
  if(is.numeric(nfl[,i])) {
    boxplot(nfl[,i], horizontal = TRUE, main = names(nfl[i]), xlab = "Value")
  }
}

# Question 4: Plot of Height and Weight ---------------------------------------
library(ggplot2)

# Part A ______________________________________________________________________
par(mfrow = c(1, 1))
sorted <- nfl[order(nfl$Weight, nfl$Height),]

ggplot(data = sorted, aes(x = Weight, y = Height)) +
  geom_point(shape = 1) +  
  geom_line(linetype = "dashed") +  
  labs(x = "Weight", y = "Height", title = "Relationship between Weight and Height among NFL Players")

# Part B ______________________________________________________________________
mean_weight <- mean(sorted$Weight, na.rm = TRUE)
mean_height <- mean(sorted$Height, na.rm = TRUE)

ggplot(sorted, aes(x = Weight, y = Height)) +
  geom_point() +
  geom_vline(xintercept = mean_weight, color = "blue", size = 1) +
  geom_hline(yintercept = mean_height, color = "red", size = 1) + 
  labs(title = "Relationship between Weight and Height among NFL Players
       with Mean Lines", x = "Weight", y = "Height")

# Question 5: Plot of Year and Side -------------------------------------------
ggplot(data = nfl, aes(x = Side, y = Age)) + geom_boxplot() + 
  labs(x = "Player Side", y = "Player Age", title = "Comparison of Player Age across Sides")

