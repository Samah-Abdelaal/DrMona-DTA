# Data management

library(tidyverse)
library(stringr)

heart <- readxl::read_xlsx("MASTER SHEET FACTOR.xlsx")

heart <- heart %>% 
  mutate_at(c("SIMPSONS LVEDV", "SIMPSONS LVESV"), str_replace, "ML", "")

heart <- heart %>% 
  mutate_at("AGE", str_replace, "yr", "")

heart <- heart %>% 
  mutate_at("AGE", str_replace, "hr", "")

heart <- heart %>% 
  mutate_at("HR", str_replace, "bpm", "")

heart <- heart %>% 
  mutate_at("s.troponin", str_replace, "pg/ml", "")

heart <- heart %>% 
  mutate_at("HB", str_replace, "g/dl", "")

heart <- heart %>% tidyr::separate(BP, c("SBP", "DBP"), sep = "/")
heart$DBP <- ifelse(heart$DBP == "8O", "80", heart$DBP)
heart$SEX <- ifelse(heart$SEX == "F", "female", "male")
heart$`LIPID PROFIL` <- ifelse(heart$`LIPID PROFIL` == "highe", "high", "normal")
heart$`FOLLOW UP DURING IN HOSPITAL` <- ifelse(
  heart$`FOLLOW UP DURING IN HOSPITAL` == "developed symptom of haert failuer",
  "developed symptoms of heart failure", "discharged with good general condition")

heart <- heart %>% 
  mutate(HTN = ifelse(
    grepl("HTT", COMORBIDITIES) == TRUE | grepl("HTN", heart$COMORBIDITIES) == TRUE,
                      "Yes", "No"))

heart <- heart %>% 
  mutate(DM = ifelse(
    grepl("DM", COMORBIDITIES) == TRUE,
    "Yes", "No"))         

heart <- heart %>% 
  mutate(smoking = ifelse(
    grepl("x-smoker", COMORBIDITIES) == TRUE, "Ex-smoker",
    ifelse(grepl("smoker", COMORBIDITIES) == TRUE, "Smoker",
  "No"
  )))

# have to convert separately
heart$PATIENTS <- as.numeric(heart$PATIENTS)
heart$AGE <- as.numeric(heart$AGE)
heart$SBP <- as.numeric(heart$SBP)
heart$DBP <- as.numeric(heart$DBP)
heart$HR <- as.numeric(heart$HR)
heart$s.troponin <- as.numeric(heart$s.troponin)
heart$HB <- as.numeric(heart$HB)
heart$BL.SUGAR <- as.numeric(heart$BL.SUGAR)
heart$`SIMPSONS LVESV` <- as.numeric(heart$`SIMPSONS LVESV`)
heart$`SIMPSONS LVEDV` <- as.numeric(heart$`SIMPSONS LVEDV`)
heart$`SIMPSONS LVEF` <- 100 * as.numeric(heart$`SIMPSONS LVEF`)
heart$AP4GLS <- 100 * as.numeric(heart$AP4GLS)
heart$AP3GLS <- 100 * as.numeric(heart$AP3GLS)
heart$AP2GLS <- 100 * as.numeric(heart$AP2GLS)
heart$LVGLS <- 100 * as.numeric(heart$LVGLS)


apply(heart, 2, unique)
str(heart)

colnames(heart) <- c("id", "age", "sex", "comorbidities",
                     "sbp", "dbp", "hr", "troponin", "hb",
                     "lipid_profile", "hba1c", "ecg",
                     "simpsons_LVESV", "simpsons_LVEDV",
                     "simpsons_LVEF", "ap4gls", "ap3gls",
                     "ap2gls", "lvgls", "followup",
                     "htn", "dm", "smoking")

final_heart <- heart[, -4]

final_heart <- final_heart %>% 
  dplyr::mutate(total_gls = (rowSums(final_heart[, 15:17])/3) *-1,
                gls_group = case_when(
                  total_gls <= 10 ~ "GLS less than -10",
                  total_gls > 10 & total_gls < 16 ~ "GLS higher than -10 & less than -16",
                  total_gls >= 16 ~ "GLS higher than -16"
                ),
                glsgroup = case_when(
                  total_gls <= 10 ~ "Heart failure",
                  total_gls > 10 & total_gls < 16 ~ "Reduced",
                  total_gls >= 16 ~ "Normal"
                ))

final_heart <- final_heart %>% 
  dplyr::mutate(simpsons_group = case_when(
    simpsons_LVEF > 50 ~ "Normal",
    simpsons_LVEF > 40 & simpsons_LVEF < 50 ~ "Reduced",
    simpsons_LVEF < 40 ~ "Heart failure"
  ))

final_heart <- final_heart %>% 
  dplyr::mutate(gls = ifelse(glsgroup == "Heart failure", "Diseased", "Normal"),
                simpson = ifelse(simpsons_group == "Heart failure", "Diseased", "Normal"),
                prognosis = ifelse(followup == "developed symptoms of heart failure", "Diseased", "Normal"))

write.csv(final_heart, "LVF assessment.csv", row.names = FALSE)

# failed to convert multiple variables at same time
heart[c(1,2, 5:9, 11, 13:19)] <- lapply(heart[c(1,2, 5:9, 11, 13:19)],
                                        function(x) as.numeric(heart[[x]]))
heart %>% 
  mutate_at(vars(heart[c(1,2, 5:9, 11, 13:19)]), as.numeric())
###