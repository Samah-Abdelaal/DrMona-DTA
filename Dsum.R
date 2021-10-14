# Data summarization

library(tidyverse)
library(flextable)
library(officer)
library(gtsummary)

timeDate <- format(Sys.Date(), "%Y - %m - %d")

hrt <- read.csv("LVF assessment.csv")
attach(hrt)

(clinical <- hrt %>% 
  select(age, sex, htn, dm, hba1c, smoking, hb, lipid_profile,
         sbp, dbp, hr, troponin,
         ecg, gls_group, simpsons_LVESV,
         simpsons_LVEDV, simpsons_LVEF, followup) %>% 
  gtsummary::tbl_summary(
    by = gls_group,
    type = list(
      age ~ "continuous2",
    sex ~ "categorical",
    htn ~ "categorical",
    dm ~ "categorical",
    hba1c ~ "continuous2",
    smoking ~ "categorical",
    hb ~ "continuous2",
    lipid_profile ~ "categorical",
    sbp ~ "continuous2",
    dbp ~ "continuous2",
    hr ~ "continuous2",
    troponin ~ "continuous2",
    ecg ~ "categorical",
    followup ~ "categorical",
    simpsons_LVESV ~ "continuous2",
    simpsons_LVEDV ~ "continuous2",
    simpsons_LVEF ~ "continuous2"
    ),
    statistic = all_continuous() ~ c("{mean} ± {sd}", "{median} ({IQR})"),
    missing = "no",
    digits = all_continuous() ~ 2,
    label = list(
      age = "Patient age",
      sex = "Gender",
      sbp = "Systolic blood pressure",
      dbp = "Diastolic blood pressure",
      hr = 'Heart rate',
      troponin = "S.Troponin",
      hb = "Hemoglobin level",
      lipid_profile = "Lipid Profile",
      hba1c = "HbA1c",
      ecg = "Electrocardiogram",
      followup = "Follow up during in-hospital stay",
      htn = "Hypertension",
      dm = "Diabetes Mellitus",
      smoking = "History of smoking",
      simpsons_LVESV = "LVESV",
      simpsons_LVEDV = "LVEDV",
      simpsons_LVEF = "LVEF"
    )
  ) %>% modify_header(label = "Variable") %>%
    add_overall() %>% 
  add_p(pvalue_fun = function(x) style_pvalue(x, digits = 2))  %>% 
  bold_labels() %>% bold_p(t = 0.05) %>% as_flex_table())
# %>%
  #theme_gtsummary_journal(journal = "jama") 

(clinical <- flextable::width(clinical, width = 1.2))
(clinical <- add_header_lines(clinical, values = "Table (1): Clinical and echocardiographic characteristics"))
(clinical <- bold(clinical, part = "header"))

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, value = clinical)
docx <- print(doc, target = paste("Participant-Characteristics ",
                                  timeDate, ".docx",
                                  sep = ""))

# Seperate tables

# sex

(sextab <- hrt %>% 
  select(sex, gls_group) %>% 
  tbl_summary(
    by = gls_group,
    type = sex ~ "categorical",
    missing = "no",
    digits = sex ~ 0,
    label = list(
      sex ~ "Gender"
    )
  ) %>% 
    modify_header(label = "Variable") %>% 
    add_overall() %>% 
    add_p(pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>% 
  bold_p() %>% 
  bold_labels() %>% 
  as_flex_table())

(sextab <- flextable::width(sextab, width = 1.2))
(sextab <- add_header_lines(sextab, values = "Table (1): Patients' gender in relation to GLS groups"))
(sextab <- bold(sextab, part = "header"))

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, value = sextab)
docx <- print(doc, target = "Gender.docx")

# age

tabage <- hrt %>% 
  select(age, gls_group) %>% 
  tbl_summary(
    by = gls_group,
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{mean} ± {sd}", "{median} ({IQR})"),
    missing = "no",
    digits = all_continuous() ~ 2,
    label = list(
      age ~ "Patient age"
    )
  ) %>% 
  modify_header(label = "Variable") %>% 
  add_overall() %>% 
  add_p(pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>% 
  bold_p() %>% 
  bold_labels() %>% 
  as_flex_table()

(tabage <- flextable::width(tabage, width = 1.2))
(tabage <- add_header_lines(tabage, values = "Table (2): Difference in patients' age across GLS groups"))
(tabage <- bold(tabage, part = "header"))

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, value = tabage)
docx <- print(doc, target = "Age.docx")

# HTN

(htntab <- hrt %>% 
    select(htn, gls_group) %>% 
    tbl_summary(
      by = gls_group,
      type = htn ~ "categorical",
      missing = "no",
      digits = htn ~ 0,
      label = list(
        htn ~ "Hypertension"
      )
    ) %>% 
    modify_header(label = "Variable") %>% 
    add_overall() %>% 
    add_p(pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>% 
    bold_p() %>% 
    bold_labels() %>% 
    as_flex_table())

(htntab <- flextable::width(htntab, width = 1.2))
(htntab <- add_header_lines(htntab, values = "Table (3): Distribution of hypertension in relation to GLS groups"))
(htntab <- bold(htntab, part = "header"))

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, value = htntab)
docx <- print(doc, target = "Hypertension.docx")

# DM

(dmtab <- hrt %>% 
    select(dm, hba1c, gls_group) %>% 
    tbl_summary(
      by = gls_group,
      type = list(
        dm ~ "categorical",
        hba1c ~ "continuous2"
      ),
      statistic = all_continuous() ~ c("{mean} ± {sd}", "{median} ({IQR})"),
      missing = "no",
      digits = list(
        dm ~ 0,
        hba1c ~ 2
      ),
      label = list(
        dm ~ "Diabetes Mellitus",
        hba1c ~ "HbA1c"
      )
    ) %>% 
    modify_header(label = "Variable") %>% 
    add_overall() %>% 
    add_p(pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>% 
    bold_p() %>% 
    bold_labels() %>% 
    as_flex_table())

(dmtab <- flextable::width(dmtab, width = 1.2))
(dmtab <- add_header_lines(dmtab, values = "Table (4): Distribution of diabetes mellitus and HbA1c in relation to GLS groups"))
(dmtab <- bold(dmtab, part = "header"))

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, value = dmtab)
docx <- print(doc, target = "DM.docx")

# smoking

(smok <- hrt %>% 
    select(smoking, gls_group) %>% 
    tbl_summary(
      by = gls_group,
      type = smoking ~ "categorical",
      missing = "no",
      digits = smoking ~ 0,
      label = list(
        smoking ~ "History of smoking"
      )
    ) %>% 
    modify_header(label = "Variable") %>% 
    add_overall() %>% 
    add_p(pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>% 
    bold_p() %>% 
    bold_labels() %>% 
    as_flex_table())

(smok <- flextable::width(smok, width = 1.2))
(smok <- add_header_lines(smok, values = "Table (5): Distribution of smoking history in relation to GLS groups"))
(smok <- bold(smok, part = "header"))

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, value = smok)
docx <- print(doc, target = "Smoking.docx")

# Hemoglobin

(hbtab <- hrt %>% 
  select(hb, gls_group) %>% 
  tbl_summary(
    by = gls_group,
    type = list(hb ~ "continuous2"),
    statistic = list(hb ~ c("{mean} ± {sd}", "{median} ({IQR})")),
    missing = "no",
    digits = all_continuous() ~ 2,
    label = list(
      hb ~ "Hemoglobin level"
    )
  ) %>% 
  modify_header(label = "Variable") %>% 
  add_overall() %>% 
  add_p(pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>% 
  bold_p() %>% 
  bold_labels() %>% 
  as_flex_table())

(hbtab <- flextable::width(hbtab, width = 1.2))
(hbtab <- add_header_lines(hbtab, values = "Table (6): Difference in patients' hemoglobin levels across GLS groups"))
(hbtab <- bold(hbtab, part = "header"))

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, value = hbtab)
docx <- print(doc, target = "Hb.docx")

# Lipid profile

(liptab <- hrt %>% 
    select(lipid_profile, gls_group) %>% 
    tbl_summary(
      by = gls_group,
      type = lipid_profile ~ "categorical",
      missing = "no",
      digits = lipid_profile ~ 0,
      label = list(
        lipid_profile ~ "Lipid profile"
      )
    ) %>% 
    modify_header(label = "Variable") %>% 
    add_overall() %>% 
    add_p(pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>% 
    bold_p() %>% 
    bold_labels() %>% 
    as_flex_table())

(liptab <- flextable::width(liptab, width = 1.2))
(liptab <- add_header_lines(liptab, values = "Table (7): Distribution of lipid profile in relation to GLS groups"))
(liptab <- bold(liptab, part = "header"))

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, value = liptab)
docx <- print(doc, target = "Lipid profile.docx")

# systolic BP

(stab <- hrt %>% 
    select(sbp, gls_group) %>% 
    tbl_summary(
      by = gls_group,
      type = list(sbp ~ "continuous2"),
      statistic = list(sbp ~ c("{mean} ± {sd}", "{median} ({IQR})")),
      missing = "no",
      digits = all_continuous() ~ 2,
      label = list(
        sbp ~ "Systolic blood pressure"
      )
    ) %>% 
    modify_header(label = "Variable") %>% 
    add_overall() %>% 
    add_p(pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>% 
    bold_p() %>% 
    bold_labels() %>% 
    as_flex_table())

(stab <- flextable::width(stab, width = 1.2))
(stab <- add_header_lines(stab, values = "Table (8): Difference in patients' systolic blood pressure across GLS groups"))
(stab <- bold(stab, part = "header"))

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, value = stab)
docx <- print(doc, target = "SBP.docx")

# Diastolic BP

(dtab <- hrt %>% 
    select(dbp, gls_group) %>% 
    tbl_summary(
      by = gls_group,
      type = list(dbp ~ "continuous2"),
      statistic = list(dbp ~ c("{mean} ± {sd}", "{median} ({IQR})")),
      missing = "no",
      digits = all_continuous() ~ 2,
      label = list(
        dbp ~ "Diastolic blood pressure"
      )
    ) %>% 
    modify_header(label = "Variable") %>% 
    add_overall() %>% 
    add_p(pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>% 
    bold_p() %>% 
    bold_labels() %>% 
    as_flex_table())

(dtab <- flextable::width(dtab, width = 1.2))
(dtab <- add_header_lines(dtab, values = "Table (9): Difference in patients' diastolic blood pressure across GLS groups"))
(dtab <- bold(dtab, part = "header"))

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, value = dtab)
docx <- print(doc, target = "DBP.docx")

# HR

(hrtab <- hrt %>% 
    select(hr, gls_group) %>% 
    tbl_summary(
      by = gls_group,
      type = list(hr ~ "continuous2"),
      statistic = list(hr ~ c("{mean} ± {sd}", "{median} ({IQR})")),
      missing = "no",
      digits = all_continuous() ~ 2,
      label = list(
        hr ~ "Heart rate"
      )
    ) %>% 
    modify_header(label = "Variable") %>% 
    add_overall() %>% 
    add_p(pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>% 
    bold_p() %>% 
    bold_labels() %>% 
    as_flex_table())

(hrtab <- flextable::width(hrtab, width = 1.2))
(hrtab <- add_header_lines(hrtab, values = "Table (10): Difference in patients' heart rate across GLS groups"))
(hrtab <- bold(hrtab, part = "header"))

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, value = hrtab)
docx <- print(doc, target = "HR.docx")

# Troponin

(trotab <- hrt %>% 
    select(troponin, gls_group) %>% 
    tbl_summary(
      by = gls_group,
      type = list(troponin ~ "continuous2"),
      statistic = list(troponin ~ c("{mean} ± {sd}", "{median} ({IQR})")),
      missing = "no",
      digits = all_continuous() ~ 2,
      label = list(
        troponin ~ "Troponin"
      )
    ) %>% 
    modify_header(label = "Variable") %>% 
    add_overall() %>% 
    add_p(pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>% 
    bold_p() %>% 
    bold_labels() %>% 
    as_flex_table())

(trotab <- flextable::width(trotab, width = 1.2))
(trotab <- add_header_lines(trotab, values = "Table (11): Difference in patients' troponin level across GLS groups"))
(trotab <- bold(trotab, part = "header"))

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, value = trotab)
docx <- print(doc, target = "Troponin.docx")

# LVESV

(esv <- hrt %>% 
    select(simpsons_LVESV, gls_group) %>% 
    tbl_summary(
      by = gls_group,
      type = list(simpsons_LVESV ~ "continuous2"),
      statistic = list(simpsons_LVESV ~ c("{mean} ± {sd}", "{median} ({IQR})")),
      missing = "no",
      digits = all_continuous() ~ 2,
      label = list(
        simpsons_LVESV ~ "LVESV"
      )
    ) %>% 
    modify_header(label = "Variable") %>% 
    add_overall() %>% 
    add_p(pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>% 
    bold_p() %>% 
    bold_labels() %>% 
    as_flex_table())

(esv <- flextable::width(esv, width = 1.2))
(esv <- add_header_lines(esv, values = "Table (12): Difference in patients' left ventricular end systolic volume across GLS groups"))
(esv <- bold(esv, part = "header"))

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, value = esv)
docx <- print(doc, target = "LVESV.docx")

# LVEDV

(edv <- hrt %>% 
    select(simpsons_LVEDV, gls_group) %>% 
    tbl_summary(
      by = gls_group,
      type = list(simpsons_LVEDV ~ "continuous2"),
      statistic = list(simpsons_LVEDV ~ c("{mean} ± {sd}", "{median} ({IQR})")),
      missing = "no",
      digits = all_continuous() ~ 2,
      label = list(
        simpsons_LVEDV ~ "LVEDV"
      )
    ) %>% 
    modify_header(label = "Variable") %>% 
    add_overall() %>% 
    add_p(pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>% 
    bold_p() %>% 
    bold_labels() %>% 
    as_flex_table())

(edv <- flextable::width(edv, width = 1.2))
(edv <- add_header_lines(edv, values = "Table (13): Difference in patients' left ventricular end diastolic volume across GLS groups"))
(edv <- bold(edv, part = "header"))

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, value = edv)
docx <- print(doc, target = "LVEDV.docx")

# LVEF

(ef <- hrt %>% 
    select(simpsons_LVEF, gls_group) %>% 
    tbl_summary(
      by = gls_group,
      type = list(simpsons_LVEF ~ "continuous2"),
      statistic = list(simpsons_LVEF ~ c("{mean} ± {sd}", "{median} ({IQR})")),
      missing = "no",
      digits = all_continuous() ~ 2,
      label = list(
        simpsons_LVEF ~ "LVEF"
      )
    ) %>% 
    modify_header(label = "Variable") %>% 
    add_overall() %>% 
    add_p(pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>% 
    bold_p() %>% 
    bold_labels() %>% 
    as_flex_table())

(ef <- flextable::width(ef, width = 1.2))
(ef <- add_header_lines(ef, values = "Table (14): Difference in patients' left ventricular ejection fraction across GLS groups"))
(ef <- bold(ef, part = "header"))

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, value = ef)
docx <- print(doc, target = "LVEF.docx")

# Followup

(fol <- hrt %>% 
    select(followup, gls_group) %>% 
    tbl_summary(
      by = gls_group,
      type = followup ~ "categorical",
      missing = "no",
      digits = followup ~ 0,
      label = list(
        followup ~ "Follow up during in-hospital stay"
      )
    ) %>% 
    modify_header(label = "Variable") %>% 
    add_overall() %>% 
    add_p(pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>% 
    bold_p() %>% 
    bold_labels() %>% 
    as_flex_table())

(fol <- flextable::width(fol, width = 1.2))
(fol <- add_header_lines(fol, values = "Table (15): Distribution of follow up results in relation to GLS groups"))
(fol <- bold(fol, part = "header"))

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, value = fol)
docx <- print(doc, target = "Follow up.docx")

  
# crosstables

# Follow up vs Simpson

cross_simp <- hrt %>% 
  tbl_cross(
    row = simpson,
    col = prognosis,
    missing = "no",
    label = list(
      prognosis ~ "Follow up",
      simpson ~ "Simpson's LVEF"
    )
  ) %>% add_p() %>% 
  bold_p(t = 0.05) %>% as_flex_table()

(cross_simp <- flextable::width(cross_simp, width = 1.3))
(cross_simp <- bold(cross_simp, part = "header"))
(cross_simp <- add_header_lines(cross_simp,
  values = "Table (16): Differentiation of patients according to Simpson's LVEF in relation to follow up"))
(cross_simp <- bold(cross_simp, part = "header"))

# Follow up vs GLS

cross_gls <- hrt %>% 
  tbl_cross(
    row = gls,
    col = prognosis,
    missing = "no",
    label = list(
      prognosis ~ "Follow up",
      gls ~ "GLS"
    )
  ) %>% add_p() %>% 
  bold_p(t = 0.05) %>% as_flex_table()

(cross_gls <- flextable::width(cross_gls, width = 1.3))
(cross_gls <- bold(cross_gls, part = "header"))
(cross_gls <- add_header_lines(cross_gls,
  values = "Table (17): Differentiation of patients according to GLS score in relation to follow up"))
(cross_gls <- bold(cross_gls, part = "header"))

# Simpson vs GLS

cross_rel <- hrt %>% 
  tbl_cross(
    row = simpson,
    col = gls,
    missing = "no",
    label = list(
      gls ~ "GLS",
      simpson ~ "Simpson's LVEF"
    )
  ) %>% add_p() %>% 
  bold_p(t = 0.05) %>% as_flex_table()

(cross_rel <- flextable::width(cross_rel, width = 1.3))
(cross_rel <- bold(cross_rel, part = "header"))
(cross_rel <- add_header_lines(cross_rel,
                               values = "Table (18): Differentiation of patients according to GLS score in relation to Simpson's LVEF"))
(cross_rel <- bold(cross_rel, part = "header"))
(cross_rel <- add_footer_lines(cross_rel, values = "Kappa = 0.47, P value = 0.004"))
(cross_rel <- bold(cross_rel, part = "footer"))

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, value = cross_simp, split = T)
doc <- flextable::body_add_flextable(doc, value = cross_gls, split = T)

doc <- flextable::body_add_flextable(doc, value = cross_rel, split = T)
timeDate <- format(Sys.Date(), "%Y- %m- %d")
docx <- print(doc, target = "reliability.docx")

# Diagnostic Test Accuracy

(dta <- table(simpson, gls))

vcd::Kappa(dta)
caret::confusionMatrix(dta)

# Simpson

(dtasimpson <- table(simpson, prognosis))
caret::confusionMatrix(dtasimpson)

validsimpson <- data.frame(Item = c("Sensitivity", "Specificity",
                                    "PPV", "NPV", "Accuracy"),
                           Value = c("71 %", "100 %", "100 %", "87 %", "90 %"))
(v1 <- flextable(validsimpson))
(v1 <- bold(v1, j = 1))
(v1 <- bold(v1, part = "header"))
(v1 <- flextable::width(v1, width = 1.5))
(v1 <- add_header_lines(v1, values = "Table (19): Validity: Simpson's LVEF"))
(v1 <- bold(v1, part = "header"))

# GLS

(dtagls <- table(gls, prognosis))
caret::confusionMatrix(dtagls)

validgls <- data.frame(Item = c("Sensitivity", "Specificity",
                                "PPV", "NPV", "Accuracy"),
                       Value = c("75 %", "100 %", "100 %", "88 %", "91 %"))
(v2 <- flextable(validgls))
(v2 <- bold(v2, j = 1))
(v2 <- bold(v2, part = "header"))
(v2 <- flextable::width(v2, width = 1.5))
(v2 <- add_header_lines(v2, values = "Table (20): Validity: GLS"))
(v2 <- bold(v2, part = "header"))

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, value = v1)
doc <- flextable::body_add_flextable(doc, value = v2)
timeDate <- format(Sys.Date(), "%Y- %m- %d")
docx <- print(doc, target = "DTA.docx")

# Gold standard test is vertical NOT horizontal, you idiot!

# compine word files

doc <- officer::read_docx()
doc <- doc %>% 
  body_add_docx(src = "Gender.docx") %>% 
  body_add_docx(src = "Age.docx") %>% 
  body_add_docx(src = "Hypertension.docx") %>% 
  body_add_docx(src = "DM.docx") %>% 
  body_add_docx(src = "Smoking.docx") %>% 
  body_add_docx(src = "Hb.docx") %>% 
  body_add_docx(src = "Lipid profile.docx") %>% 
  body_add_docx(src = "SBP.docx") %>% 
  body_add_docx(src = "DBP.docx") %>% 
  body_add_docx(src = "HR.docx") %>% 
  body_add_docx(src = "Troponin.docx") %>% 
  body_add_docx(src = "LVESV.docx") %>% 
  body_add_docx(src = "LVEDV.docx") %>% 
  body_add_docx(src = "LVEF.docx") %>% 
  body_add_docx(src = "Follow up.docx") %>% 
  body_add_docx(src = "reliability.docx") %>% 
  body_add_docx(src = "DTA.docx") %>% 
  print(
    target = paste("Seperate tables ", timeDate, ".docx", sep = "")
  )

###############################
library(corrplot)

# Correlation matrix table and graph

corel_names <- c("Age", "SBP", "DBP", "HR", "Troponin", "Hb",
                 "LVESV", "LVEDV", "LVEF", "GLS")

cor_data <- hrt[, c(2, 4:8, 12:14, 23)]

colnames(cor_data) <- corel_names

library(modelsummary)

(cs <- datasummary_correlation(cor_data,
                               output = "flextable"))
(cs <- bold(cs, part = "header"))
(cs <- add_header_lines(cs, values = "Table (2) Correlation between various clinical characteristics and GLS score"))
(cs <- bold(cs, part = "header"))
(cs <- bold(cs, j = 1))

c <- cor(cor_data)

corrplot(c,
         method = 'number',
         number.digits = 2,
         title = "Figure (1) Correlation between various clinical characteristics and GLS score")
