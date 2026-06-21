# libraries 
library(tidyverse)
library(corrplot)

# load
diabetes <- read.csv("diabetic_data.csv")
mapping <- read.csv("IDS_mapping.csv")

# checking data loaded properly
dim(diabetes)
dim(mapping)
names(diabetes)

# investigating the response variable
table(diabetes$readmitted)
prop.table(table(diabetes$readmitted))

# creating the target variable
diabetes$readmitted_binary <- ifelse(
  diabetes$readmitted == "<30",
  1,
  0
)

# target variable check
table(diabetes$readmitted_binary)
prop.table(table(diabetes$readmitted_binary))

# ID missing values
colSums(diabetes == "?")
sort(colSums(diabetes == "?"), decreasing = TRUE)

# calculating missing percentages
missing_pct <- round(
  colSums(diabetes == "?") / nrow(diabetes) * 100,
  2
)

sort(missing_pct, decreasing = TRUE)

# new dataframe... removing weight, medical_specialty, and payer code
diabetes_clean <- diabetes
diabetes_clean <- diabetes_clean %>%
  select(-weight,
         -medical_specialty,
         -payer_code)
dim(diabetes_clean)

# remaining missing values
sort(colSums(diabetes_clean == "?"), decreasing = TRUE)

# converting ? to unknown
diabetes_clean$race[diabetes_clean$race == "?"] <- "Unknown"
diabetes_clean$diag_1[diabetes_clean$diag_1 == "?"] <- "Unknown"
diabetes_clean$diag_2[diabetes_clean$diag_2 == "?"] <- "Unknown"
diabetes_clean$diag_3[diabetes_clean$diag_3 == "?"] <- "Unknown"

sort(colSums(diabetes_clean == "?"), decreasing = TRUE)

# removing ID variables and original target variable
diabetes_clean <- diabetes_clean %>%
  select(-encounter_id,
         -patient_nbr,
         -readmitted)

dim(diabetes_clean)

# correlation analysis
numeric_vars <- diabetes_clean %>%
  select(where(is.numeric))

cor_matrix <- cor(numeric_vars)

corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.cex = 0.8)

# finding the strongest predictors
aggregate(number_inpatient ~ readmitted_binary,
          data = diabetes_clean,
          FUN = mean)

aggregate(number_emergency ~ readmitted_binary,
          data = diabetes_clean,
          FUN = mean)

aggregate(num_medications ~ readmitted_binary,
          data = diabetes_clean,
          FUN = mean)

aggregate(time_in_hospital ~ readmitted_binary,
          data = diabetes_clean,
          mean)

aggregate(number_diagnoses ~ readmitted_binary,
          data = diabetes_clean,
          mean)

# target variable distribution plot
ggplot(diabetes_clean,
       aes(x = factor(readmitted_binary))) +
  geom_bar() +
  labs(
    title = "Distribution of 30-Day Readmissions",
    x = "Readmitted Within 30 Days (0 = No, 1 = Yes)",
    y = "Count"
  )

# time in hospital
ggplot(diabetes_clean,
       aes(x = time_in_hospital)) +
  geom_histogram(bins = 20) +
  labs(
    title = "Distribution of Time in Hospital",
    x = "Days in Hospital",
    y = "Count"
  )

# age vs readmission
ggplot(diabetes_clean,
       aes(x = age,
           fill = factor(readmitted_binary))) +
  geom_bar(position = "fill") +
  labs(
    title = "Readmission Rate by Age Group",
    x = "Age Group",
    y = "Proportion",
    fill = "Readmitted"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# inpatient visits vs readmission
ggplot(diabetes_clean,
       aes(x = factor(readmitted_binary),
           y = number_inpatient)) +
  geom_boxplot() +
  labs(
    title = "Prior Inpatient Visits by Readmission Status",
    x = "Readmitted Within 30 Days (0 = No, 1 = Yes)",
    y = "Number of Prior Inpatient Visits"
  )

# emergency vs readmission
ggplot(diabetes_clean,
       aes(x = factor(readmitted_binary),
           y = number_emergency)) +
  geom_boxplot() +
  labs(
    title = "Prior Emergency Visits by Readmission Status",
    x = "Readmitted Within 30 Days (0 = No, 1 = Yes)",
    y = "Number of Prior Emergency Visits"
  )

# number of diagnoses vs readmission
ggplot(diabetes_clean,
       aes(x = factor(readmitted_binary),
           y = number_diagnoses)) +
  geom_boxplot() +
  labs(
    title = "Number of Diagnoses by Readmission Status",
    x = "Readmitted Within 30 Days (0 = No, 1 = Yes)",
    y = "Number of Diagnoses"
  )

# number of meds vs readmission
ggplot(diabetes_clean,
       aes(x = factor(readmitted_binary),
           y = num_medications)) +
  geom_boxplot() +
  labs(
    title = "Number of Medications by Readmission Status",
    x = "Readmitted Within 30 Days (0 = No, 1 = Yes)",
    y = "Number of Medications"
  )
