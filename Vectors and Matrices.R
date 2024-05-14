## Question 1: Troubleshooting Code -------------------------------------------
# For each of the following lines of code describe what is wrong with the code.
# On a new line in the RScript, fix the code so it will run. Be sure to write 
# your answers as comments in this RScript.

### Part A: New Vector ----
New Vector <- c(2, 4, 6, 8, 10, 12, 14)
#A variable name cannot contain spaces.
NewVector <- c(2, 4, 6, 8, 10, 12, 14)

### Part B: 1vector----
1vector <- c("value1", "value2", "value3")
#A variable name cannot start with a number.
vector1 <- c("value1", "value2", "value3")

### Part C: Someth!ng ----
Someth!ng <- c(200L, 400L, 300L, 900L)
#A variable name cannot contain special characters, one of which is "!"
Something <- c(200L, 400L, 300L, 900L)

### Part D: trouble ----
trouble <- ("A", "A", "C", "D")
#This vector is missing the concatenate function "c()"
trouble <- c("A", "A", "C", "D")

### Part E: more.trouble ----
# Run the code to produce the vector vector_of_values. Then identify what is 
# wrong about more.trouble.
vector_of_values <- c(1:8)
more.trouble <- matrix(vector_of_values, nrow = 3, ncol = 4)
#The code in line 38 produces numeric values from 1 to 8. However,
#more.trouble creates a matrix with 3 rows and 4 columns,producing 12 cells 
#which is more than what vector_of_values can fill. 
more.trouble <- matrix(vector_of_values, nrow = 4, ncol = 2)

### Part F: print ----
# This code is attempting to print the vector of values that was created in Part
# E. Why doesn't this run?
print(Vector_of_values)
#R is case sensitive; this means that it recognizes vector_of_values and 
#Vector_of_values as different variables. Vector_of_values is a variable that
#does not exist in this script. This is caused by the capital V which should be
#a lower case v.
print(vector_of_values)

## Question 2: Constructing Vectors -------------------------------------------
# For this question, you will be reconstructing the vectors that are displayed
# in the Homework 2_Topic 1_3.pdf file.

# For each vector you create, identify the class and length of the vector as 
# given in the environment window in RStudio. Be sure to write these answers as
# comments.

### Part A: ----
partAvector <- c("Cayden", "Ari", "Bacci", "Kristen")
print(partAvector)
#class <- character 
#length <- 4

### Part B: ----
partBvector <- c(0:8)
print(partBvector)
#class <- integer
#length <- 9

### Part C: ----
partCvector1 <- c("A", "B", "C", "D")
print(partCvector1)
#class <- character
#length <- 4

partCvector2 <- c("Z", "Y", "X", "W")
print(partCvector2)
#class <- character
#length <- 4

partCvector <- c(partCvector1, partCvector2)
print(partCvector)
#class <- character
#length <- 8

## Question 3: Constructing Matrices ------------------------------------------
# For this question, you will be reconstructing the vectors that are displayed 
# in the Homework 2_Topic 1_3.pdf file.

### Part A:  ----
homework3A <- c(4:28)
matrixA <- matrix(homework3A, nrow = 5, ncol = 5)
print(matrixA)

### Part B:  ----
homework3B <- c(4:28)
matrixB <- matrix(homework3B, nrow = 5, ncol = 5, byrow = TRUE)
print(matrixB)

## Question 4: Describe Process -----------------------------------------------
# For this question, I would like you to see what happens when these vectors 
# combine. Describe what happens and what kind of vector all_info is.


mwf <- c("Class 1", "Class 2", "Class 3")
tth <- c("Class 4", "Class 5", "Class 6")
credits <- c(3, 4, 2, 1, 3, 3)

all_info <- c(mwf, tth, credits)

#The vectors "mwf" and "tth" are formed by character string values. The vector
#"credits" contain numeric values. The vector "all_info" combines these three 
#vectors but while doing so, it changes the numeric values into character 
#strings. This results in the "all_info" vector being a character vector.