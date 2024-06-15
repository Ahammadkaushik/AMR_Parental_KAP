library(tidyverse)
library(report)
library(gtsummary)
library(gt)
library(easystats)
library(readxl)
library(ggsci)
library(naniar)
library(dplyr)
library(skimr)
library(readr)
install.packages("rmarkdown")
library(rmarkdown)

# import data
data <- read_excel("raw_data/AMR_Parental_KAP.xlsx")
view(data)

# Demographic characteristics of study participants
data |>
  select(1:12) |>
  tbl_summary(statistic = list(
    all_continuous() ~ "{mean} ({sd})"
  )) |>
  as_gt() |>
  gtsave("tables/Table1_Demographics.docx")

# Major sources of information about antibiotic
data |>
  select(47:55) |>
  tbl_summary(statistic = list(
    all_continuous() ~ "{mean} ({sd})"
  )) |>
  as_gt() |>
  gtsave("tables/Table2_major_source.docx")

# Level of knowledge, attitudes, and practices
data |>
  select(69:71) |>
  tbl_summary(statistic = list(
    all_continuous() ~ "{mean} ({sd})"
  )) |>
  as_gt() |>
  gtsave("tables/Table3_level_of_KAP.docx")

# Factors associated with the level of knowledge among parents
data |>
  select(1:9, Knowledge_Level) |>
  tbl_uvregression(
    method = glm,
    y = Knowledge_Level,
    method.args = list(family = binomial),
    exponentiate = T
  ) |>
  bold_p(t = 0.05) |>
  as_gt() |>
  gtsave("tables/Table4_Factors associated with the level of knowledge.docx")


# quality of knowledge level (binary outcome)
data <- data |>
  mutate(Knowledge_Level_Binary = case_when(
    Total_Knowledge_Score < 6 ~ "Poor",
    Total_Knowledge_Score > 5 ~ "Good"
  ))

# quality of knowledge level (binary outcome)
data <- data |>
  mutate(Knowledge_Level_Binary_Code = case_when(
    Total_Knowledge_Score < 6 ~ 0, # Poor
    Total_Knowledge_Score > 5 ~ 1 # Good
  ))

# quality of knowledge level (ordinal outcome)
data <- data |>
  mutate(Knowledge_Level_Ordinal = case_when(
    Total_Knowledge_Score < 5 ~ "Poor",
    Total_Knowledge_Score >= 5 & Total_Knowledge_Score <= 7 ~ "Moderate",
    Total_Knowledge_Score > 7 ~ "Good"
  ))

# quality of knowledge level (ordinal outcome)
data <- data |>
  mutate(Knowledge_Level_Ordinal_Code = case_when(
    Total_Knowledge_Score < 5 ~ 1, # Poor
    Total_Knowledge_Score >= 5 & Total_Knowledge_Score <= 7 ~ 2, # Moderate
    Total_Knowledge_Score > 7 ~ 3 # Good
  ))

# export data
write.csv(data, "raw_data/AMR_KAP_Preprocessed.csv", row.names = F)

# import data
data <- read.csv("raw_data/AMR_KAP_Preprocessed.csv")
View(data)

# Factors associated with the level of knowledge among parents
data |>
  select(1:9, Knowledge_Level_Binary_Code) |>
  tbl_uvregression(
    method = glm,
    y = Knowledge_Level_Binary_Code,
    method.args = list(family = binomial),
    exponentiate = T
  ) |>
  bold_p(t = 0.05) |>
  as_gt() |>
  gtsave("tables/Table4_Factor_Level_of_knowledge_UV_LogReg.docx")


data |>
  select(1:9, Total_Knowledge_Score) |>
  tbl_uvregression(
    method = lm,
    y = Total_Knowledge_Score
  ) |>
  bold_p(t = 0.05) |>
  as_gt() |>
  gtsave("tables/Table6_UV_LinReg.docx")











