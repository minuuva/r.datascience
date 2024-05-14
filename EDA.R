# Meaningful Data Continued __________________________________________________

sequence <- c("meaningful", "meaningful", "meaningless", "meaningless", "meaningful",
              "meaningful", "meaningless", "meaningless", "meaningless", "meaningful",
              "meaningful", "meaningless", "meaningless", "meaningful", "meaningless",
              "meaningful", "meaningless", "meaningless", "meaningful", "meaningful",
              "meaningful", "meaningful", "meaningless", "meaningless", "meaningful", 
              "meaningless")
score <- c("18", "17", "14", "4", "18", "21", "7", "15", "14", "30", "18", "7",
           "17", "9", "21", "18", "8", "7", "3", "24", "3", "15", "14", "14", 
           "11", "14")
caffeine <- c("yes", "yes", "yes", "yes", "yes", "yes", "yes", "no", "yes", "no", 
              "no", "no", "yes", "yes", "no", "no", "yes", "yes", "no", "no", 
              "no", "yes", "yes", "yes", "yes", "no")
sleep.hrs <- c("5", "5", "5.5", "6", "6", "6", "6", "6.5", "6.5", "6.5", "7", 
               "7", "7", "7", "7.5", "7.5", "7.67", "7.75", "7.83", "8", "8", 
               "8", "8", "8", "8.167", "9")
sleep.cat <- c("LessThan7", "LessThan7", "LessThan7", "LessThan7", "LessThan7",
               "LessThan7", "LessThan7", "LessThan7", "LessThan7", "LessThan7",
               "7orMore", "7orMore", "7orMore", "7orMore", "7orMore", "7orMore",
               "7orMore", "7orMore", "7orMore", "7orMore", "7orMore", "7orMore",
               "7orMore", "7orMore", "7orMore", "7orMore")

meaningful <- data.frame(sequence, 
                         score, 
                         caffeine, 
                         sleep.hrs, 
                         sleep.cat)
as.integer(sleep.hrs)

## Question 1: Identifying Class ----
class(meaningful$sequence) #character
class(meaningful$score) #character
class(meaningful$caffeine) #character
class(meaningful$sleep.hrs) #character
class(meaningful$sleep.cat) #character

## Question 2: Coercing Vectors ----

as.integer(sleep.hrs)
as.numeric(sleep.hrs)

#a
#as.numeric(sleep.hrs) is the best method because when the values of sleep.hrs
#is converted to numerical values, it will preserve the decimal points. However,
#as.integer(sleep.hrs) will round all the decimal values down to the nearest
#integer, leading to a loss of information. 

#b
newsleep.hrs <- as.numeric(sleep.hrs)

## Question 3: Score ----
#a. calculating the average score is NOT possible in its current form. This is
#because the class of the score vector is character, as seen by the quotes.
#Mathematical operations require the vector to consist of numeric or integer vales.

#b.
newscore <- as.numeric(meaningful$score)

#c.
averagescore <- mean(newscore)
print(averagescore) #13.88462

## Question 4: Caffeine Frequency ----
table(meaningful$caffeine) # no (10) yes (16)

## Question 5: Caffeine Frequency ----
table(caffeine, sleep.cat)
#This code creates a two dimensional table that shows us the frequency distributions
#of two factors: caffeine and sleep.cat. It demonstrates the relationship between
#caffeine usage and sleep time. For instance, we can see the number of students
#who consumed caffeine and slept less than 7 hours compared to the those who did 
#not consume caffeine. This is different than the code in question 4 which only
#shows the frequency of caffeine usage. 

## Question 6: Seconds ----
sleep.secs <- newsleep.hrs * 3600 #3600, since there are 3600 seconds in an hour.

## Question 7: 5 Num Sum for Score ----
summary(newscore)

## Question 8: Sleep Hrs Vector ----
#a. 
sleep.hrs.stat <- c(mean(newsleep.hrs),
                    sd(newsleep.hrs),
                    sum(newsleep.hrs),
                    IQR(newsleep.hrs))


#b.
sort(sleep.hrs.stat)

#c.
#. 1.049867   1.832500   7.016038 182.417000

## Question 9: Scores Ranked ----
order(newscore, decreasing = FALSE)

## Question 10: Correlation ----
# In the following function, you need to change score.vector.as.numbers and
# sleep.hr.vector.as.numbers to whatever you named your score and sleep.hrs
# vectors.
cor(newscore, newsleep.hrs)

#a. 
zscore.score <- (newscore - mean(newscore)) / sd(newscore)
zscore.score

zscore.sleep.hrs <- (newsleep.hrs - mean(newsleep.hrs)) / sd(newsleep.hrs)
zscore.score

#b.
productofzscore <- zscore.score * zscore.sleep.hrs
productofzscore

#c.
averageofproduct <- sum(productofzscore) / length(productofzscore)
averageofproduct # -0.1817194
cor(newscore, newsleep.hrs) # -0.1889881