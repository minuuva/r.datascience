### Load the provided data set into R _________________________________________
setwd("/Users/cmw/Desktop/STAT 1601")
attendance <- read.csv("attendance.csv")
grades <- read.csv("grades.csv")
officehr <- read.csv("officehours_wider.csv")
library(tidyverse)

# Question 1: PVA -------------------------------------------------------------

# Part A ______________________________________________________________________
PVA <- read.csv("PVA.csv")
PVA_BaseR <- read.csv("PVA.csv")

# Part B ______________________________________________________________________

# Part i ________
PVA_BaseR$diff.in.gift <- PVA_BaseR$Largest.Gift - PVA_BaseR$Smallest.Gift

# Part ii ________
PVA_BaseR <- PVA_BaseR[, -c(3:4)]

# Part iii ________
subset(PVA_BaseR, (Age <= 60 & Income >= 5))

# Part iv ________
iv <- subset(PVA_BaseR, (Current.Gift > 50 & Time.Between.Gifts > 12))
mean(iv$Current.Gift)

# Part v ________
table(PVA_BaseR$Income[PVA_BaseR$Other.Gifts != 0])

# Part vi ________
aggregate(Current.Gift ~ Income, data = PVA_BaseR, FUN = mean)

# Part C ______________________________________________________________________

# Part i ________
PVA <- mutate(PVA, diff.in.gift = Largest.Gift - Smallest.Gift)

# Part ii ________
PVA <- select(PVA, -c(3:4))

# Part iii ________
filter(PVA, Age <= 60 & Income >= 5)

# Part iv ________
PVA |>
  filter(Current.Gift > 50 & Time.Between.Gifts > 12) |>
  summarize(average.current.gift = mean(Current.Gift, na.rm = TRUE))

# Part v ________
PVA |>
  filter(Other.Gifts != 0) |>
  count(Income)

# Part vi ________
PVA |>
  group_by(Income) |>
  summarize(n = n(), average.current.gift = mean(Current.Gift, na.rm = TRUE))

# Question 2: Grades ----------------------------------------------------------

# Part A ______________________________________________________________________
grades <- rename(grades, Homework = Rename1)
grades <- rename(grades, Exam1 = Rename2)
grades <- rename(grades, Exam2 = Rename3)
grades <- rename(grades, FinalExam = Rename4)
grades <- rename(grades, Project = Rename5)

# Part B ______________________________________________________________________
merge1 <- merge(attendance, grades)

# Part C ______________________________________________________________________
merge2 <- merge(officehr, merge1, all.y = T)

# Part D ______________________________________________________________________
merge2 <- merge2 |>
  mutate(Num.Attend = if_else(is.na(Num.Attend), 0, (Num.Attend)))

# Part E ______________________________________________________________________
merge2 <- mutate(merge2, FinalGrade = 0.05 * Attendance + 0.25 * 
                   (Homework+2*Extra.Credit.Days) + 0.15 * Exam1 + 0.15 * Exam2 +
                   0.15 * FinalExam + 0.25 * Project + Num.Attend)
merge2 <- mutate(merge2,LetterGrade = 
                   if_else(round(FinalGrade) >= 90, "A",
                           if_else(round(FinalGrade) >= 87, "B+",
                                   if_else(round(FinalGrade) >= 83, "B",
                                           if_else(round(FinalGrade) >= 80, "B-",
                                                   if_else(round(FinalGrade) >= 70, "C",
                                                           if_else(round(FinalGrade) >= 60, "D",
                                                                   "F")))))))

# Part F ______________________________________________________________________
merge2 |>
  ggplot(aes(x = LetterGrade)) +
  geom_bar(color = 'blue', fill = 'green') +
  labs(x = 'Grade', y = 'Count', title = 'Distribution of Letter Grades')

# Part G ______________________________________________________________________
merge2 <- arrange(merge2, LetterGrade, FinalExam)
