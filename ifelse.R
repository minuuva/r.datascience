### Load the provided data set into R _________________________________________
setwd("/Users/cmw/Desktop/STAT 1601")
NFL <- read.csv("NFL.csv")

# Question 1: Age -------------------------------------------------------------
mean(NFL$Age, na.rm = TRUE) # 25.36723

# Question 2: College ---------------------------------------------------------
table(NFL$College, useNA = "ifany")

# Question 3: Rename Column ---------------------------------------------------
collegedf <- as.data.frame(player.per.college)
colnames(collegedf)[1] <- "College"


# Question 4: Weight ----------------------------------------------------------
#a
NFL.copy <- NFL
NFL.copy$kg_conversion <- 0.453592
NFL.copy$Weight_kg_PartA <- NFL.copy$Weight * NFL.copy$kg_conversion

#b
NFL.copy$Weight_kg_PartB <- NFL$Weight * 0.453592

# Question 5: Drop Columns ----------------------------------------------------
Quest5 <- NFL.copy[, -c(12:14)]

# Question 6: Subsetting Data with Square Brackets ----------------------------
selected16 <- NFL[NFL$Team == "Philadelphia" & 
                    (NFL$Side == "Offense" | NFL$Side == "Special Teams") &
                    (NFL$Years == 0 | NFL$Years > 6) & !is.na(NFL$Jersey), ]

# Question 7: Subsetting Data with subset() Function --------------------------
selected11 <- subset(NFL, NFL$Team == "New England" & 
                       (NFL$Side == "Defense" | NFL$Side == "Special Teams") & 
                       NFL$Height > 74 & !is.na(NFL$Jersey))

# Question 8: Summary of Weight by Side ---------------------------------------
summary(NFL$Weight[NFL$Side == "Offense"])
summary(NFL$Weight[NFL$Side == "Defense"])
summary(NFL$Weight[NFL$Side == "Special Teams"])

# Question 9: Missing Jersey Numbers ------------------------------------------
nonumber <- NFL[is.na(NFL$Jersey),]

# Question 10: Sequence -------------------------------------------------------
favorite <- NFL[NFL$Team == "Chicago",]
favorite$Rowby2 <- seq(from = 2, by = 2, length.out = nrow(favorite))

# Question 11: Playtime -------------------------------------------------------
NFL$Playtime <- ifelse(NFL$Years == 0, "Rookie",
                       ifelse(NFL$Years >= 1 & NFL$Years <= 4, "1-4 years",
                       ifelse(NFL$Years >= 5 & NFL$Years <= 8, "5-8 years",
                       ifelse(NFL$Years >= 9 & NFL$Years <= 12, "9-12 years",
                       ifelse(NFL$Years >= 13, "13+ years", NA)))))