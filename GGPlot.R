### Load the provided data set into R _________________________________________
setwd("/Users/cmw/Desktop/STAT 1601")
library(ggplot2)
nfl <- read.csv("nfl.csv")

# Question 1: Position Group vs. Years ----------------------------------------
ggplot(data = nfl, aes(x = PositionGrp, y = Years)) +
  geom_point() +
  labs(title = "NFL Experience per Position Group",
         x = "Position Group", y = "Years in NFL")

# This scatterplot is not the most appropriate graph because the variables
# we wish to explore are categorical and quantitative. Therefore, I will be
# creating a box plot.

ggplot(nfl, aes(x = PositionGrp, y = Years)) +
  geom_boxplot(fill = "lightgreen", colour = "black") +
  labs(title = "NFL Experience per Position Group",
       x = "Position Group",
       y = "Years in NFL")

# Question 2: Playtime --------------------------------------------------------

# Part A ______________________________________________________________________
nfl$Playtime <- factor(
  ifelse(nfl$Years == 0, "Rookie",
         ifelse(nfl$Years <= 4, "1 - 4 years",
                ifelse(nfl$Years <= 8, "5 - 8 years",
                       ifelse(nfl$Years <= 12, "9 - 12 years", "13+ years")))),
  levels = c("Rookie", "1 - 4 years", "5 - 8 years", "9 - 12 years", "13+ years")
  )


# Part B ______________________________________________________________________
ggplot(data = nfl, aes(x = Playtime)) +
  geom_bar(fill = "lightblue", colour = "black") +
  scale_y_continuous(breaks = seq(0, 1500, by = 100)) +
  labs(title = "Bar Graph of NFL Career Lengths", x = "Playtime in Years",
       y = "Number of Players") +
  theme_light()
# In order to figure out an appropriate endpoint for the y scale, I created a 
# frequency table using the table(nfl$Playtime) function. I found that the 
# playtime with the highest number of players is 1 - 4 years with 1293 players.
# Therefore, I chose to use 1500 as the endpoint. 
  
# Question 3: Weight by Side --------------------------------------------------

# Part A ______________________________________________________________________
ggplot(data = nfl, mapping = aes(x = Weight, fill = Side)) +
  geom_histogram(bins = 25, color = "white") +
  theme_minimal() +
  labs(title = "Histogram of NFL Player Weight Distribution based on Side",
       x = "Weight in Pounds", y = "Count")

# Part B ______________________________________________________________________
ggplot(data = nfl, mapping = aes(x = Weight, color = Side)) +
  geom_histogram(bins = 25) +
  theme_minimal() +
  labs(title = "Histogram of NFL Player Weight Distribution based on Side",
       x = "Weight in Pounds", y = "Count")

# Part C ______________________________________________________________________
ggplot(data = nfl, mapping = aes(x = Weight)) +
  geom_histogram(bins = 25, color = "blue") +
  facet_wrap(~Side, ncol = 1) +
  theme_minimal() +
  labs(title = "Histogram of NFL Player Weight Distribution based on Side",
       x = "Weight in Pounds", y = "Count")

# Part D ______________________________________________________________________
# The graph in part C which uses the facet_wrap function is the most effective
# since it clearly portrays the distribution within each group without the
# overlapping that occurs in a single plot (part A/B). The facet approach allows
# viewers to identify the full shape of each distribution and easily compare
# them across the groups without confusion from overlapping bars. 


# Question 4: Weight & Height -------------------------------------------------

# Part A ______________________________________________________________________
ggplot(data = nfl, aes(x = Weight, y = Height, shape = Side, color = Position)) + 
  geom_point() +
  labs(title = "NFL Player Height by Weight with Side and Position",
         x = "Weight (pounds)", 
         y = "Height (inches)")

ggplot(data = nfl, aes(x = Weight, y = Height, shape = Position, color = Side)) + 
  geom_point() +
  labs(title = "NFL Player Height by Weight with Side and Position",
         x = "Weight (pounds)", 
         y = "Height (inches)")

# Out of the two graphs, the first graph which uses color = Position is clearly
# the most effective graph. The primary reason is the fact that the second graph
# is unable to assign a different shape for each of the player positions. In fact,
# the warning message says that the shape palette can only deal with a maximum of
# 6 discrete values. However, the first graph uses color to distinguish positions.
# Therefore, it is able to clearly depict all positions in the graph. 

# Part B ______________________________________________________________________
ggplot(data = nfl, aes(x = Weight, y = Height, color = Position)) +
  geom_point() +
  facet_wrap(~ Side) +
  theme_minimal() +
  labs(title = "NFL Player Height and Weight, Faceted by Side",
       x = "Weight (pounds)", 
       y = "Height (inches)")

ggplot(data = nfl, aes(x = Weight, y = Height, color = Side)) +
  geom_point() +
  facet_wrap(~ Position) +
  theme_minimal() +
  labs(title = "NFL Player Height and Weight, Faceted by Position",
       x = "Weight (pounds)", 
       y = "Height (inches)")

# The first graph is the more effective choice. This is because it has a clear
# side by side comparison of the relationship between weight and height for all
# three sides (defense, offense, special teams). The viewers are able to easily
# distinguish the distribution of pattern. However, the second graph displays a
# total of 23 graphs for each position in one window. Therefore, it is hard to
# identify the relationship between two variables. 

# Part C ______________________________________________________________________
offense.side <- subset(nfl, Side == "Offense")
defense.side <- subset(nfl, Side == "Defense")
specialteams.side <- subset(nfl, Side == "Special Teams")

ggplot(offense.side, aes(x = Weight, y = Height, color = Position)) +
  geom_point() +
  theme_minimal() +
  labs(title = "NFL Offensive Player Height by Weight",
       x = "Weight (pounds)", 
       y = "Height (inches)")

ggplot(defense.side, aes(x = Weight, y = Height, color = Position)) +
  geom_point() +
  theme_minimal() +
  labs(title = "NFL Defensive Player Height by Weight",
       x = "Weight (pounds)", 
       y = "Height (inches)")

ggplot(specialteams.side, aes(x = Weight, y = Height, color = Position)) +
  geom_point() +
  theme_minimal() +
  labs(title = "NFL Special Teams Player Height by Weight",
       x = "Weight (pounds)", 
       y = "Height (inches)")

# Part D ______________________________________________________________________
# The first graph in part B is the best in displaying the four variables for
# three reasons: clarity, simplicity, and comparison purposes. In terms of
# clarity, the faceted graph avoids overlapping/overplotting by separating
# the data points based on 'Side', which makes it easier to compare the
# weight and height distributions across Sides without distraction. Due to its
# clarity, it also demonstrates simplicity. This graph is less cluttered and
# allows the viewer to focus on the relationship between height and weight
# within each side. Lastly, this graph allows for direct comparison between
# the groups on the same axes scales, making it easier to differentiate the
# distributions of height and weight across Sides.

# Part E ______________________________________________________________________
ggplot(nfl, aes(x = Weight, y = Height)) +
  geom_point() +
  geom_vline(xintercept = median(nfl$Weight), color = "purple", lwd = 2) + 
  geom_hline(yintercept = median(nfl$Height), color = "green", lwd = 2) +
  labs(title = "NFL Player Height and Weight with Median Lines",
       x = "Weight (pounds)",
       y = "Height (inches)") +
  theme_minimal()


# Part F ______________________________________________________________________
ggplot(nfl, aes(x = Weight, y = Height, color = Years)) +
  geom_point() +
  labs(title = "NFL Player Height and Weight with Experience",
       x = "Weight (pounds)",
       y = "Height (inches)",
       color = "Years of Experience") +
  theme_minimal()

# The lines of codes above produce a scatterplot that portrays the relationship
# between height and weight of NFL players with years being the distinguishing 
# factor. The color of the points in the scatterplot depicts the number of years
# of experience for each player. 'Color = Years' part of the code assigns a color
# to the points, where each shade of color represents the different number of 
# years of experience.
