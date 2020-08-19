library(tidyverse)

library(SEER2R)
#Read SEER case files for ovarian cancer from 2004-2015 into data.frame
lgess.df <- read.SeerStat("lgess.dic", UseVarLabelsInData = TRUE)

#Change data.frame to tibble
lgess.tib <- as_tibble(lgess.df)

#Rename columns
lgess.tib = rename(lgess.tib,
                   Age.Group = "Age_recode_with_1_year_olds",
                   Race = "Race_and_origin_recode_NHW_NHB_NHAIAN_NHAPI_Hispanic",
                   Histo = "Histologic_Type_ICDO3",
                   SEER.stage = "SEER_Combined_Summary_Stage_2000_2004",
                   Surg = "RX_SummSurg_Prim_Site_1998",
                   Chemo = "Chemotherapy_recode_yes_no/unk",
                   Rads = "Radiation_recode",
                   COD = "SEER_causespecific_death_classification",
                   SurvMonths = "Survival_months",
                   DiagAge = "Age_at_diagnosis",
                   CutoffStatus = "Vital_status_recode_study_cutoff_used",
                   DiagMonth = "Month_of_diagnosis_recode",
                   DiagYear = "Year_of_diagnosis",
                   FollowMonth = "Month_of_followup_recode",
                   FollowYear = "Year_of_followup_recode",
                   CutoffStatus2 = "End_Calc_Vital_Status_Adjusted",
                   MonthsFollowed = "Number_of_Intervals_Calculated")

#Change Grades to numeric values
lgess.tib$Grade <- ifelse(lgess.tib$Grade == "Poorly differentiated; Grade III", 3,
                          ifelse(lgess.tib$Grade == "Moderately differentiated; Grade II", 2, 
                                 ifelse(lgess.tib$Grade == "Well differentiated; Grade I", 1,
                                        ifelse(lgess.tib$Grade == "Undifferentiated; anaplastic; Grade IV", 4, "Unk"))))

#Change Race to manageable names
lgess.tib$Race <- ifelse(lgess.tib$Race == "Hispanic (All Races)", "Hisp",
                         ifelse(lgess.tib$Race == "Non-Hispanic Asian or Pacific Islander", "API",
                                ifelse(lgess.tib$Race == "Non-Hispanic American Indian/Alaska Native", "Native",
                                       ifelse(lgess.tib$Race == "Non-Hispanic Black", "Black",
                                              ifelse(lgess.tib$Race == "Non-Hispanic White", "White", "Unk")))))
#Combine Diagnosis Month and Diagnosis Year to Date and Follow-up Month/year to Date
lgess.tib = lgess.tib %>% 
  unite(DiagDate, c(DiagYear, DiagMonth), sep = "-")

library(zoo)
lgess.tib$DiagDate <- as.yearmon(lgess.tib$DiagDate, format = "%Y-%B")

lgess.tib = lgess.tib %>% 
  unite(FollowDate, c(FollowYear, FollowMonth), sep = "-")

lgess.tib$FollowDate <- as.yearmon(lgess.tib$FollowDate, format = "%Y-%B")

#Change COD variable to Alive(0) or Dead(1)
lgess.tib$COD <- ifelse(lgess.tib$COD == "Alive or dead of other cause", 0, 1)
