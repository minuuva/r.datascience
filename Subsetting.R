# Question 1: Import Data -----------------------------------------------------
setwd("/Users/cmw/Desktop/STAT 1601")
attendance <- read.csv("attendance.csv")
officehours <- read.csv("officehours.csv")
grades <- read.csv("grades.csv")

# Question 2: Displaying Data -------------------------------------------------
head(attendance, n = 8)
head(officehours, n = 8)
head(grades, n = 8)

# Question 3: Rename Columns --------------------------------------------------
colnames(grades) <- c("StudentID", "Section", "Homework", "Exam1", "Exam2",
                      "FinalExam", "Project")

# Question 4: Unique StudentIDs -----------------------------------------------
unique.attendance <- unique(attendance$StudentID)
# 1  2  3  4  5  6  7  8  9 10
unique.officehours <- unique(officehours$StudentID)
# 1  3  5  7  9 11 13 14 15 16 17 19 20 21 22 23 24 25 26 27 28 30 32 33 34 35
unique.officehours.sorted <- sort(unique.officehours)
unique.grades <- unique(grades$StudentID)
# 1  2  3  4  5  6  7  8  9 10 11 13 14 15 16 17 19 20 21 22 23 24 25 26 27 28
# 30 32 33 34 35 36 37 38 39

# Students with the IDS (1, 3, 5, 7, 9) are the ones that appear in all three sets.

# The attendance data set contains the shortest range of Student IDs (1 to 10),
# suggesting that it may only cover a small group of students.
# The officehours data set includes a bigger range of Student IDs (1 to 35), 
# without some numbers. This may indicate a bigger group of students that could
# be seeking additional help.
# The grades data set has the biggest range of Student IDs (1 to 39),
# which may represent a complete record of students across classes.

# Question 5: Merge -----------------------------------------------------------
# Part A: Only Attendance ____
only.attendance <- merge(attendance, officehours, all.x = TRUE)

# Part B: In Common ____
in.common <- merge(attendance, officehours, all = FALSE)

# Part C: Only Officehours ____
only.officehours <- merge(attendance, officehours, all.y = TRUE)

# Part D: All Information ____
all.info <- merge(attendance, officehours, all = TRUE)

# Question 6: Merge -----------------------------------------------------------

# Part A: Similarities and Differences ____
unique(only.attendance$StudentID)
# 1 2 3 4 5 6 7 8 9 10
# A similarity is that unique Student IDs in this output matches exactly with those 
# in the attendance data set. However, a difference is that this output does not
# include any additional Student IDs from the officehours data set that are not
# already in the attendance dataset. This is because the all.x = TRUE
# ensures that all entries from the attendance data set are retained,
# whether or not they have a matching entry in the officehours dataset.
# This is why no new StudentIDs from officehours are added.

unique(in.common$StudentID)
# 1 3 5 7 9
# The Student IDs in this output are in both the attendance and officehours data sets. 
# However, only the Student IDs that are in both data sets are retained,
# without those that are unique to either data set. This is because all = FALSE
# only keeps rows with Student IDs that are in both data sets; this only includes
# students that attend classes and office hours.

unique(only.officehours$StudentID)
# 1  3  5  7  9 11 13 14 15 16 17 19 20 21 22 23 24 25 26 27 28 30 32 33 34 35
# This output is 1 3 5 7 9, which are also in the attendance data set,
# and other IDs that are only in the officehours data set. However, this output
# includes Student IDs that are only from the officehours data set
# (11 13 14 15 16 17 19 20 21 22 23 24 25 26 27 28 30 32 33 34 35), which are not
# in the attendance data set. This is because all.y = TRUE inlucdes all entries
# from the officehours data set are included, whether or not they have a match
# in the attendance data sets. 

unique(all.info$StudentID) 
# 1  2  3  4  5  6  7  8  9 10 11 13 14 15 16 17 19 20 21 22 23 24 25 26 27 28
# 30 32 33 34 35
# This output includes all StudentIDs from both the attendance and officehours
# data sets without excluding any. The obvious difference from others is that
# this is the most largest output as it has every unique StudentID from both
# data sets. This is due to the all = TRUE which is an outer join that includes
# all StudentIDs from both data sets.

# Part B: Difference in Number of Rows ____
nrow(only.attendance) # 24
# The fact that there are 24 rows but only 10 unique students in only.attendance
# tells us that some students are recorded several times in the attendance dataset.
# It means multiple attendances.

nrow(in.common) # 19
# The 19 rows but only 5 unique students, also demonstrates multiple records
# per student. Because this data set represents those that are in both 
# attendance and officehours, the repeated rows may be due to same students
# attending classes and office hours.

nrow(only.officehours) # 65
# The 65 rows but 25 unique students shows multiple instances of students
# in officehours, indicating that there are many who need extra help.

nrow(all.info) # 70
# The 70 rows and 35 unique StudentIDs portrays the highest level of engagement
# across both attendance and office hours. The additional rows show the repeated
# entries of students in both datas ets.

# Question 7: Final Grades ----------------------------------------------------

# Part A: Final Grades Data Frame ____
# I noticed that the grades data set has students in section 1 and 2 while the
# attendance data set only has students in section. Therefore, I first created
# a subset from the grades data set that only includes students in section 1. 
section1grade <- subset(grades, Section == "Section 1")
Q7df <- merge(section1grade, attendance, by = "StudentID", all = FALSE)
Q7df <- Q7df[, -c(1, 2, 8, 9, 10, 11, 12, 13, 15)]

# Part B: Final Grade ____
Q7df$finalgrade <- apply(Q7df, MARGIN =1, FUN = weighted.mean, weight =c(0.25, 0.15, 0.15,
                                                                  0.15, 0.25, 0.05))
# Part C: Letter Grades ____
lettergrade <- ifelse(Q7df$finalgrade >= 89.5, "A",
                      ifelse(Q7df$finalgrade >= 86.5, "B+",
                             ifelse(Q7df$finalgrade >=82.5, "B",
                                    ifelse(Q7df$finalgrade >= 79.5, "B-",
                                           "UnSat"))))

Q7df$lettergrade <- lettergrade

# Part D: Table of Letter Grades ____
table(Q7df$lettergrade)

# Question 8: Sorting Data Set ------------------------------------------------
Q7df$lettergrade <- factor(Q7df$lettergrade, levels = c("A", "B+", "B", "B-", "UnSat"))
# Using the factor() function we learend in class, I converted the letter grade
# to show as the correct order. 
Q7df <- Q7df[order(Q7df$lettergrade, Q7df$finalgrade), ]
# This ensures that the final grade is in ascending order.