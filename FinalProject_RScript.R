# Final Project ---------------------------------------------------------------

# Answer the questions in this document. Submit your RScript to Gradescope.

## Pre-amble: Any modifications required for your data ________________________
setwd("/Users/cmw/Desktop/STAT 1601")

library(tidyverse)
library(ggplot2)

ds.data <- read.csv("dssalaries.csv")
ds.data <- ds.data[, -1]
ds.data$experience_level <- factor(
  ifelse(ds.data$experience_level == "EN", "Entry",
         ifelse(ds.data$experience_level == "MI", "Mid",
                ifelse(ds.data$experience_level == "SE", "Senior",
                       ifelse(ds.data$experience_level == "EX", "Exec", 
                              "Other")))),
  levels = c("Entry", "Mid", "Senior", "Exec"))

ds.data$company_size <- factor(ds.data$company_size, levels = c("S", "M", "L"))

# _____________________________________________________________________________
## Component 1: Base R --------------------------------------------------------

### Creation of New Variables ----
# Minu Choi
ds.data$salary_category <- factor(
  ifelse(ds.data$salary_in_usd <= quantile(ds.data$salary_in_usd, 0.25), "Low",
         ifelse(ds.data$salary_in_usd <= median(ds.data$salary_in_usd), "Low-to-Mid",
                ifelse(ds.data$salary_in_usd <= quantile(ds.data$salary_in_usd, 0.75), "Mid-to-High",
                       ifelse(ds.data$salary_in_usd <= max(ds.data$salary_in_usd), "High", "High")))),
  levels = c("Low", "Low-to-Mid", "Mid-to-High", "High"))

# Dongju Han
ds.data$ExchangeRates = ds.data$salary_in_usd / ds.data$salary

# Patrick Ho
ds.data$salary_in_euro <- ds.data$salary_in_usd * 0.93

# Bonny Koo
cost_of_living_estimate <- c(
  DE = 3000, JP = 3000, GB = 3000, HN = 800, US = 3000, HU = 1500, 
  NZ = 3000, FR = 3000, IN = 800, PK = 800, CN = 1500, GR = 1500,
  AE = 3000, NL = 3000, MX = 1500, CA = 3000, AT = 3000, NG = 800,
  ES = 1500, PT = 1500, DK = 3000, IT = 3000, HR = 1500, LU = 3000,
  PL = 1500, SG = 3000, RO = 1500, IQ = 800, BR = 1500, BE = 3000,
  UA = 800, IL = 3000, RU = 1500, MT = 1500, CL = 1500, IR = 800,
  CO = 1500, MD = 800, KE = 800, SI = 1500, CH = 3000, VN = 800,
  AS = 800, TR = 1500, CZ = 1500, DZ = 800, EE = 1500, MY = 1500,
  AU = 3000, IE = 3000
)

ds.data$cost_of_living <- cost_of_living_estimate[as.character(ds.data$company_location)]

### EDA with no Subset ----
# Minu Choi
table(ds.data$experience_level, ds.data$company_size)

# Dongju Han
aggregate(salary_in_usd ~ company_size, data = ds.data, FUN = mean)
#The aggregate() function groups data based on one or more categorical variables
#and computes summary statistics for each group. The "~" separates the variables
#to be summarized in the left side from the grouping variables in the right side. 
#The data parameter specifies the data frame I'm using, and the FUN = parameter
#defines the function to be applied in each group of data. 

# Patrick Ho
cor(ds.data$remote_ratio, ds.data$salary_in_usd)
summary(ds.data$remote_ratio)
summary(ds.data$salary_in_usd)

# Bonny Koo
table(ds.data$experience_level)

### EDA with Subset ----
# Minu Choi
fulltime <- subset(ds.data, employment_type == "FT")
parttime <- subset(ds.data, employment_type == "PT")
contract <- subset(ds.data, employment_type == "CT")
freelance <- subset(ds.data, employment_type == "FL")

print(paste('Average Full Time Salary:', mean(fulltime$salary_in_usd)))
print(paste('Average Part Time Salary:', mean(parttime$salary_in_usd)))
print(paste('Average Contract Salary:', mean(contract$salary_in_usd)))
print(paste('Average Freelance Salary:', mean(freelance$salary_in_usd)))

# Dongju Han
location_to_continent <- function(country) {
  continent_map <- c(
    US = "North America", 
    DE = "Europe", 
    GB = "Europe", 
    JP = "Asia",
    IN = "Asia", 
    CA = "North America", 
    FR = "Europe", 
    BR = "South America",
    AU = "Oceania", 
    CN = "Asia"
  )
  continent <- continent_map[country]
  if (is.null(continent)) {
    return("Other")
  } else {
    return(continent)
  }
}
ds.data$continent <- sapply(ds.data$company_location, location_to_continent)

table(ds.data$continent)
#By using table() function, I realized that there are five different continents. 
europe <- ds.data[ds.data$continent == "Europe", ]
asia <- ds.data[ds.data$continent == "Asia", ]
North_America <- ds.data[ds.data$continent == "North America", ]
Oceania <- ds.data[ds.data$continent == "Oceania", ]
South_America <- ds.data[ds.data$continent == "South America", ]

average_remote_ratio_EU <- mean(europe$remote_ratio, na.rm = TRUE) #52.778
average_remote_ratio_asia <- mean(asia$remote_ratio, na.rm = TRUE) #53.125
average_remote_ratio_NA <- mean(North_America$remote_ratio, na.rm = TRUE) #77.53247
average_remote_ratio_OCE <- mean(Oceania$remote_ratio, na.rm = TRUE) #83.33
average_remote_ratio_SA <- mean(South_America$remote_ratio, na.rm = TRUE) #33.333

# Patrick Ho
salary2020 <- subset(ds.data, work_year == 2020)
summary(salary2020$salary_in_usd)
salary2021 <- subset(ds.data, work_year == 2021)
summary(salary2021$salary_in_usd)
salary2022 <- subset(ds.data, work_year == 2022)
summary(salary2022$salary_in_usd)

# Bonny Koo
entry_level_data <- ds.data[ds.data$experience_level == "Entry", ]
average_salary_by_continent <- aggregate(salary_in_usd ~ continent, 
                                         data = entry_level_data, FUN = mean)
average_salary_by_continent$salary_in_usd <- round(
  average_salary_by_continent$salary_in_usd, 2)

print(average_salary_by_continent)

# _____________________________________________________________________________
## Component 2: ggplot --------------------------------------------------------

### Single Quantitative ----
# Bonny Koo
# Base R
experience <- table(ds.data$experience_level)
barplot(experience,
        main = "Distribution of Experience Levels", 
        xlab = "Experience Level", 
        ylab = "Count", 
        col = "lightblue", 
        border = "black")

# ggplot
ggplot(ds.data, aes(x = experience_level)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Experience Levels",
       x = "Experience Level",
       y = "Count") +
  theme_minimal()

### Single Categorical ----
# Minu Choi
# Base R
company_sizes <- table(ds.data$company_size)
barplot(company_sizes,
        main = "Distribution of Companies in terms of Size",
        xlab = "Company Size",
        ylab = "Count",
        col = "grey",
        border = "blue")

# Dongju Han
# Base R
continent_counts <- table(ds.data$continent)
barplot(continent_counts, main = "Number of Data Scientist in Each Continent", 
        xlab = "Continents", ylab = "Count", col = "red")

### Single Quantitative and Single Categorical ----
# Dongju Han
#Base R
continents <- c("Europe", "Asia", "North America", "Oceania", "South America")
average_rr <- c(average_remote_ratio_EU, average_remote_ratio_asia, 
                average_remote_ratio_NA, average_remote_ratio_OCE,
                average_remote_ratio_SA)
average_remote_ratio_df <- data.frame(Continent = continents, Average_Remote_Ratio = average_rr)

barplot(average_remote_ratio_df$Average_Remote_Ratio,
        names.arg = average_remote_ratio_df$Continent, 
        main = "Barplot for Average Remote Ratio in each Continent",
        xlab = "Continents", ylab = "Percentage (%)")

#ggplot
ggplot(ds.data, aes(x = company_size, y = salary_in_usd)) +
  geom_boxplot() +
  labs(title = "Boxplot for Salary in USD on each Company Size",
       x = "Company Size",
       y = "Salary in USD") +
  scale_y_continuous(breaks = seq(0, 600000, 50000)) +
  theme_gray()

### Two Quantitative ----
# Patrick Ho
# Base R
ds.data$rr <- factor(ifelse(ds.data$remote_ratio == 0, "0",
                            ifelse(ds.data$remote_ratio <= 50, "50",
                                   "100")),
                     levels = c("0", "50", "100"))


dotchart(ds.data$salary_in_usd, groups = ds.data$rr,
         xlim = c(0,300000),
         xlab = "Salary in USD", ylab = "Remote Ratio",
         main = "Dotchart of the Relationship between Remote Ratio and Salary")

# ggplot
ggplot(data = ds.data)+
  geom_boxplot(mapping = aes(x = work_year, y = salary_in_usd))+
  labs(
    x = "Year", y = "salary in USD",
    title = "Relationship between Year and Salary") + 
  scale_x_continuous(limits = c(2020, 2022), breaks=c(2020, 2021, 2022))+
  scale_y_continuous(breaks=seq(0, 700000, by = 50000))

### Two Categorical ----
# Bonny Koo
# ggplot
location_to_continent <- function(location) {
  continent_map <- c(US = "North America", DE = "Europe", GB = "Europe", JP = "Asia", 
                     IN = "Asia", CA = "North America", FR = "Europe", BR = "South America", 
                     AU = "Oceania", CN = "Asia")
  continent <- continent_map[location]
  if (is.null(continent)) {
    return("Other")
  } else {
    return(continent)
  }
}

ds.data$continent <- sapply(ds.data$company_location, location_to_continent)

experience_by_continent <- table(ds.data$continent, ds.data$experience_level)

barplot(experience_by_continent, beside = TRUE, 
        main = "Experience Level Distribution by Continent",
        xlab = "Experience Level", ylab = "Count",
        col = rainbow(ncol(experience_by_continent)),
        legend.text = rownames(experience_by_continent))

### 3 Variables ----
# Minu Choi
# ggplot
ggplot(ds.data, aes(x = experience_level, y = salary_in_usd, fill = experience_level)) +
  geom_boxplot() +
  facet_wrap(~ company_size) +
  labs(title = "Relationship between Salary and Experience Level across Company Size",
       x = "Experience Level",
       y = "Salary in USD",
       fill = "Experience Level") +
  scale_y_continuous(breaks = seq(0, 700000, by = 50000)) +
  theme_minimal() +
  theme(text = element_text(size = 11, face = "bold"))

### 4 Variables ----
# Patrick Ho
# ggplot
year <- as.factor(ds.data$work_year)

ggplot(data = ds.data, mapping = aes(x = year, y = salary_in_usd))+
  geom_boxplot(mapping = aes(fill = company_size))+
  facet_wrap(~experience_level)+
  labs(x = "Year", y= "Salary in USD",
       title = "Relationship of Salary and Year by Company Size and Experience Level")+
  scale_y_continuous(breaks=seq(0, 700000, by = 50000))

# _____________________________________________________________________________
## Component 3: Tidyverse -----------------------------------------------------

### Creation of New Variables -------------------------------------------------
# Minu Choi
ds.data <- ds.data |>
  mutate(salary_category = case_when(
    salary_in_usd <= quantile(salary_in_usd, 0.25) ~ "Low",
    salary_in_usd <= median(salary_in_usd) ~ "Low-to-Mid",
    salary_in_usd <= quantile(salary_in_usd, 0.75) ~ "Mid-to-High",
    TRUE ~ "High"
  )) |>
  mutate(salary_category = factor(salary_category, 
                                  levels = c("Low", "Low-to-Mid", "Mid-to-High", "High")))

# Dongju Han
ds.data <- ds.data |>
  mutate(ExchangeRates = salary_in_usd / salary)

# Patrick Ho
ds.data <- mutate(ds.data, salary_in_euro = salary_in_usd * 0.93)

# Bonny Koo
cost_of_living_estimate <- c(
  DE = 3000, JP = 3000, GB = 3000, HN = 800, US = 3000, HU = 1500, 
  NZ = 3000, FR = 3000, IN = 800, PK = 800, CN = 1500, GR = 1500,
  AE = 3000, NL = 3000, MX = 1500, CA = 3000, AT = 3000, NG = 800,
  ES = 1500, PT = 1500, DK = 3000, IT = 3000, HR = 1500, LU = 3000,
  PL = 1500, SG = 3000, RO = 1500, IQ = 800, BR = 1500, BE = 3000,
  UA = 800, IL = 3000, RU = 1500, MT = 1500, CL = 1500, IR = 800,
  CO = 1500, MD = 800, KE = 800, SI = 1500, CH = 3000, VN = 800,
  AS = 800, TR = 1500, CZ = 1500, DZ = 800, EE = 1500, MY = 1500,
  AU = 3000, IE = 3000
)


ds.data<- ds.data |>
  mutate(cost_of_living = cost_of_living_estimate[as.character(company_location)],
         cost_of_living = if_else(is.na(cost_of_living), mean(cost_of_living_estimate, na.rm = TRUE), cost_of_living))

### EDA with no Subset --------------------------------------------------------
# Minu Choi
ds.data |>
  count(experience_level, company_size)

# Dongju Han
ds.data|>
  group_by(company_size) |>
  summarize(mean_salary = mean(salary_in_usd))

# Patrick Ho
ds.data |> 
  group_by(work_year) |> 
  summarize(
    ave.rr = mean(remote_ratio, na.rm = T),
    med.rr = median(remote_ratio, na.rm = T),
    sd.rr = sd(remote_ratio, na.rm = T),
    ave.salary= mean(salary_in_usd, na.rm = T),
    med.salary = median(salary_in_usd, na.rm = T),
    sd.salary = sd(salary_in_usd, na.rm = T)
  )

# Bonny Koo
ds.data |>
  count(experience_level) |>
  arrange(desc(n))  

### EDA with Subset -----------------------------------------------------------
# Minu Choi
ds.data |>
  filter(employment_type %in% c("FT", "PT", "CT", "FL")) |>
  group_by(employment_type) |>
  summarise(Average_Salary = mean(salary_in_usd, na.rm = TRUE))

# Dongju Han
ds.data |>
  mutate(continent = sapply(company_location, location_to_continent)) |> 
  filter(!is.na(continent)) |>
  group_by(continent) |>
  summarize(average_remote_ratio = mean(remote_ratio, na.rm = TRUE))

# Patrick Ho
ds.data |>
  group_by(work_year) |> 
  select(salary_in_usd) |>
  summarize(
    ave.salary= mean(salary_in_usd, na.rm = T),
    med.salary = median(salary_in_usd, na.rm = T),
    sd.salary = sd(salary_in_usd, na.rm = T))

# Bonny Koo
entry_level_data <- ds.data |>
  filter(experience_level == "Entry")

average_salary_by_continent <- entry_level_data |>
  add_count(continent) |> 
  group_by(continent) |>
  summarise(
    average_salary = mean(salary_in_usd, na.rm = TRUE),
    count = first(n)
  ) |>
  mutate(average_salary = round(average_salary, 2))

print(average_salary_by_continent)
