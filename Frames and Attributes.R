## Question 1: Homework3.csv --------------------------------------------------
setwd("/Users/cmw/Desktop/STAT 1601")

### Part A: Importing Homework3.csv  ----
Homework3 <- read.csv("Homework3.csv", header = FALSE)

### Part B: Changing Column Names  ----
colnames(Homework3) <- c("Year", "Quarter", "US.Stream", "Int.Stream", "All.Rev")

### Part C: Determine the Number of Columns & Rows ----
ncol(Homework3)
nrow(Homework3)

### Part D: Printing Top 8 Rows & Bottom 18 rows ----
head(Homework3, n = 8L)
tail(Homework3, n = 18L)

## Question 2: Meaningful Sequence --------------------------------------------

### Part A: Recreate the meaningful data set ----
Sequence = c("meaningful", "meaningful", "meaningless", "meaningless", 
"meaningless", "meaningless")
Score = c(3, 3, 4, 7, 7, 7)
Caffeine = c("no", "no", "yes", "yes", "no", "yes") 
SleepHrs = c(7.83, 8, 6, 6, 7, 7.75)
Sleep.Cat = c("7orMore", "7orMore", "LessThan7", "LessThan7", "7orMore", "7orMore")

meaningful <- data.frame(Sequence, Score, Caffeine, SleepHrs, Sleep.Cat)

print(meaningful)

### Part B: Dimension function ----
sleep_matrix <- meaningful$SleepHrs
dim(sleep_matrix) <- c(2, 3)

### Part C: Row names ----
rownames(sleep_matrix) <- c("A", "B")
print(sleep_matrix)