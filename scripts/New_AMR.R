library(tidyverse)
library(report)
library(gtsummary)
library(gt)
library(easystats)
library(readxl)
library(dplyr)
library(readr)

install.packages("forcats")
install.packages('ordinal')
install.packages("MASS")
library(forcats)
library(ordinal)
library(MASS)

# import data
data <- read_excel("raw_data/AMR_Parental_KAP.xlsx")
view(data)

glimpse(data)

# Convert response variable into factor
data$Knowledge_Level <- as.factor(data$Knowledge_Level)
data$Attitude_Level <- as.factor(data$Attitude_Level)
data$Practice_Level <- as.factor(data$Practice_Level)

glimpse(data)

# Select Variable
df <- data %>% dplyr::select(1:9, Knowledge_Level, Attitude_Level, Practice_Level)
glimpse(df)

#Factor Reorder
df %>%
  mutate_at(vars(Knowledge_Level), ~ fct_relevel(., c("Poor","Moderate","Good")))
  mutate_at(vars(Attitude_Level), ~ fct_relevel(., c("Negative","Uncertain","Positive")))
  mutate_at(vars(Practice_Level), ~ fct_relevel(., c("Misuse","Good")))
  
# Summary of response Variable
df %>%
  dplyr::select(Knowledge_Level, Attitude_Level, Practice_Level) %>%
  tbl_summary()

reg_table

# Ordinal Logistic Regression for Attitude
model_attitude <- clm(Attitude_Level ~
                        Parent_s_age_years+
                        Parent_s_sex+
                        Parent_s_education_level+
                        Employment_status+
                        Family_type+
                        Your_average_household_income_per_month_BDT+
                        Child_s_sex+
                        Child_s_age_years+
                        Number_of_children,
                      data = df)

summary(model_attitude)
broom::tidy(model_attitude)

reg_table1 <- model_attitude %>%
  tbl_regression(exponentiate = TRUE) %>%
  bold_p(t = 0.05) %>%
  as_gt() %>%
  gtsave("tables/Table6_UV_Ordinal.docx")



# Ordinal Logistic Regression for Practice
model_Practice <- clm(Practice_Level ~
                        Parent_s_age_years+
                        Parent_s_sex+
                        Parent_s_education_level+
                        Employment_status+
                        Family_type+
                        Your_average_household_income_per_month_BDT+
                        Child_s_sex+
                        Child_s_age_years+
                        Number_of_children,
                      data = df)

summary(model_Practice)
broom::tidy(model_Practice)

reg_table2 <- model_Practice %>%
  tbl_regression(exponentiate = TRUE) %>%
  bold_p(t = 0.05) %>%
  as_gt() %>%
  gtsave("tables/Table8_UV_Ordinal_Practice.docx")




















