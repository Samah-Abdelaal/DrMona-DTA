# Data visualization

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(hrbrthemes)
library(gtsummary)
library(flextable)
library(officer)

hrt <- read.csv("LVF assessment.csv")
attach(hrt)

# association between GLS and risk factors

# first: sociodemographics

# viz

# Age

hrt %>% 
  ggplot(aes(x = age,
             y = total_gls)) +
  geom_point() #mmmm,...no

# try grouping by GLS

ggboxplot(hrt,
          x = "gls_group",
          y = "age",
          add = "point",
          color = "coral3",
          fill = "coral"
) +
  labs(x = "GLS group",
       y = "Age") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme(
    axis.title = element_text(face = "bold")
  )

# sex

# grouped by GLS

hrt %>% 
  group_by(gls_group) %>% 
  count(sex) %>% 
  mutate(pct = (n / sum(n)) * 100,
         label = paste0(round(pct), "%")) %>% 
  ggplot(aes(
    x = gls_group,
    y = pct,
    fill = sex
  )
  ) + geom_col(position = "dodge") +
  labs(x = "GLS group",
       y = "Percentage %") + 
  geom_text(
    aes(label = label),
    check_overlap = TRUE
  ) +
  theme_minimal() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(name = "Sex")

# not grouped by GLS

hrt %>% 
  count(sex) %>% 
  mutate(pct = (n / sum(n)) * 100,
         label = paste0(round(pct), "%")) %>% 
  ggplot(aes(
    x = sex,
    y = pct,
  )
  ) + geom_col(position = "dodge",
               col = "coral3",
               fill = "coral3",
               width = 0.6) +
  labs(x = "Gender",
       y = "Percentage %") + 
  geom_text(
    aes(label = label),
    check_overlap = TRUE,
    vjust = -0.5
  ) +
  theme_minimal() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(
    axis.title = element_text(face = "bold")
  )

# HTN

hrt %>% 
  group_by(gls_group) %>% 
  count(htn) %>% 
  mutate(pct = (n / sum(n)) * 100,
         label = paste0(round(pct), "%")) %>% 
  ggplot(aes(
    x = gls_group,
    y = pct,
    fill = htn
  )
  ) + geom_col(position = "dodge") +
  labs(x = "GLS group",
       y = "Percentage %") + 
  geom_text(
    aes(label = label),
    check_overlap = TRUE
  ) +
  theme_minimal() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(name = "Hypertension")

# not grouped by GLS

ggboxplot(hrt,
          x = "htn",
          y = "total_gls",
          add = "point",
          color = "plum4",
          fill = "plum"
) +
  labs(x = "Hypertension",
       y = "GLS") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme(
    axis.title = element_text(face = "bold")
  )

# DM

hrt %>% 
  group_by(gls_group) %>% 
  count(dm) %>% 
  mutate(pct = (n / sum(n)) * 100,
         label = paste0(round(pct), "%")) %>% 
  ggplot(aes(
    x = gls_group,
    y = pct,
    fill = dm
  )
  ) + geom_col(position = "dodge") +
  labs(x = "GLS group",
       y = "Percentage %") + 
  geom_text(
    aes(label = label),
    check_overlap = TRUE
  ) +
  theme_minimal() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(name = "Diabetes Mellitus")

# not grouped by GLS

ggboxplot(hrt,
          x = "dm",
          y = "total_gls",
          add = "point",
          color = "plum4",
          fill = "plum"
) +
  labs(x = "Diabetes Mellitus",
       y = "GLS") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme(
    axis.title = element_text(face = "bold")
  )

# Smoking

hrt %>% 
  group_by(gls_group) %>% 
  count(smoking) %>% 
  mutate(pct = (n / sum(n)) * 100,
         label = paste0(round(pct), "%")) %>% 
  ggplot(aes(
    x = gls_group,
    y = pct,
    fill = smoking
  )
  ) + geom_col(position = "dodge") +
  labs(x = "GLS group",
       y = "Percentage %") + 
  geom_text(
    aes(label = label),
    check_overlap = TRUE
  ) +
  theme_minimal() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(name = "Smoking")

# not grouped by GLS

ggboxplot(hrt,
          x = "smoking",
          y = "total_gls",
          add = "point",
          color = "grey28",
          fill = "slategrey"
) +
  labs(x = "Smoking",
       y = "GLS") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme(
    axis.title = element_text(face = "bold")
  )

# Hemoglobin

ggboxplot(hrt,
          x = "gls_group",
          y = "hb",
          add = "point",
          color = "darkred",
          fill = "red3"
) +
  labs(x = "GLS group",
       y = "Hemoglobin level") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme(
    axis.title = element_text(face = "bold")
  )

# lipid profile

# grouped by GLS

hrt %>% 
  group_by(gls_group) %>% 
  count(lipid_profile) %>% 
  mutate(pct = (n / sum(n)) * 100,
         label = paste0(round(pct), "%")) %>% 
  ggplot(aes(
    x = gls_group,
    y = pct,
    fill = lipid_profile
  )
  ) + geom_col(position = "dodge") +
  labs(x = "GLS group",
       y = "Percentage %") + 
  geom_text(
    aes(label = label),
    check_overlap = TRUE
  ) +
  theme_minimal() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(name = "Lipid profile")

# not grouped by GLS BUT comparing mean total GLS

ggboxplot(hrt,
          x = "lipid_profile",
          y = "total_gls",
          add = "point",
          color = "olivedrab4",
          fill = "olivedrab3"
) +
  labs(x = "Lipid Profile",
       y = "GLS") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme(
    axis.title = element_text(face = "bold")
  )

# SBP

ggboxplot(hrt,
          x = "gls_group",
          y = "sbp",
          add = "point",
          color = "orangered3",
          fill = "orangered1"
) +
  labs(x = "GLS group",
       y = "Systolic BP") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme(
    axis.title = element_text(face = "bold")
  )

# DBP

ggboxplot(hrt,
          x = "gls_group",
          y = "dbp",
          add = "point",
          color = "steelblue",
          fill = "steelblue1"
) +
  labs(x = "GLS group",
       y = "Diastolic BP") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme(
    axis.title = element_text(face = "bold")
  )

# HR

ggboxplot(hrt,
          x = "gls_group",
          y = "hr",
          add = "point",
          color = "springgreen4",
          fill = "springgreen2"
) +
  labs(x = "GLS group",
       y = "Heart rate") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme(
    axis.title = element_text(face = "bold")
  )

# troponin

ggboxplot(hrt,
          x = "gls_group",
          y = "troponin",
          add = "point",
          color = "slateblue4",
          fill = "slateblue1"
) +
  labs(x = "GLS group",
       y = "Troponin") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme(
    axis.title = element_text(face = "bold")
  )


# followup

# grouped by GLS

hrt %>% 
  group_by(gls_group) %>% 
  count(followup) %>% 
  mutate(pct = (n / sum(n)) * 100,
         label = paste0(round(pct), "%")) %>% 
  ggplot(aes(
    x = gls_group,
    y = pct,
    fill = followup
  )
  ) + geom_col(position = "dodge", width = 0.6) +
  labs(x = "GLS group",
       y = "Percentage %") + 
  geom_text(
    aes(label = label),
    check_overlap = TRUE
  ) +
  theme_minimal() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(name = "Follow up") +
  theme(
    legend.position = "top"
  )

# not grouped by GLS

ggboxplot(hrt,
          x = "followup",
          y = "total_gls",
          add = "point",
          color = "lightpink4",
          fill = "lightpink"
) +
  labs(x = "Follow up",
       y = "GLS") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme(
    axis.title = element_text(face = "bold")
  ) #+
#  annotate("text", x = 2.5, y = 9, label = "mean")

# LVEF

hrt %>% 
  ggplot(
    aes(
      x = simpsons_LVEF,
      y = total_gls
    )
  ) +
  geom_point(col = "orangered3") +
  geom_smooth(method = "lm",
              se = F, col = "grey50", size = 0.5) +
  labs(
    x = "LVEF",
    y = "GLS"
  ) +
  annotate("text", x = 30, y = 17,
           label = "r = 0.82, P < 0.001",
           fontface = "bold",
           size = 5, color = "red3") +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold")
  )

# grouped by GLS

ggboxplot(hrt,
          x = "gls_group",
          y = "simpsons_LVEF",
          add = "point",
          color = "steelblue",
          fill = "steelblue1"
) +
  labs(x = "GLS group",
       y = "LVEF") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme(
    axis.title = element_text(face = "bold")
  )

# sensitivity

simpson <- ifelse(hrt$simpson == "Diseased", "Heart failure", hrt$simpson)
prognosis <- ifelse(hrt$prognosis == "Diseased", "Heart failure", hrt$prognosis)
gls <- ifelse(hrt$gls == "Diseased", "Heart failure", hrt$gls)

(s <- as.data.frame(prop.table(table(simpson, prognosis), 2)))

ggplot(data = s,
       aes(
         x = prognosis,
         y = Freq,
         fill = simpson
       )
) +
  geom_col(width = 0.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = paste0(round(Freq*100), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(x = "Follow up",
       y = "Percentage %",
       fill = "Simpson's LVEF") +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )

(g <- as.data.frame(prop.table(table(gls, prognosis), 2)))

#as.data.frame(table(prognosis=="Diseased", gls))

#g <- as.data.frame(table(prognosis=="Diseased", gls))

ggplot(data = g,
    aes(
      x = prognosis,
      y = Freq,
      fill = gls
    )
  ) +
  geom_col(width = 0.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = paste0(round(Freq*100), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(x = "Follow up",
       y = "Percentage %",
       fill = "GLS") +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )

# Note: if you want PPV switch the table variables

# ROC curves

# GLS

gls_roc <- ifelse(gls == "Diseased", 1, 0)
glm.fit=glm(gls_roc ~ total_gls, family = "binomial")
pROC::roc(gls_roc, glm.fit$fitted.values, plot = TRUE,
          percent = TRUE, legacy.axes = TRUE,
          col = "#377eb8", lwd = 4,
          print.auc = TRUE, print.auc.y = 40)

# Simpson

hrt <- hrt %>%
  mutate(sim_roc = ifelse(simpson == "Diseased", 1, 0))
hrtroc <- hrt %>% 
  filter(!is.na(sim_roc), !is.na(simpsons_LVEF))
attach(hrtroc)

glm.fit2=glm(sim_roc ~ simpsons_LVEF, family = "binomial")
pROC::roc(sim_roc, glm.fit2$fitted.values, plot = TRUE,
          percent = TRUE, legacy.axes = TRUE,
          col = "#4daf4a", lwd = 4,
          print.auc = TRUE, print.auc.y = 40)
length(glm.fit2$fitted.values)
length(hrtroc$sim_roc)
length(hrtroc$simpsons_LVEF)
class(hrtroc$sim_roc)
par(pty = "m")

######

hrt %>% 
  filter(!is.na(glsgroup), !is.na(simpsons_group)) %>% 
  ggplot(
    aes(
      x = glsgroup,
      fill = simpsons_group
    )
  ) + geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format())


hrt %>% 
  filter(!is.na(glsgroup), !is.na(simpsons_group)) %>% 
  group_by(glsgroup) %>% 
  count(simpsons_group) %>% 
  mutate(pct = (n / sum(n)) * 100,
         label = paste0(round(pct), "%")) %>% 
  ggplot(aes(
    x = glsgroup,
    fill = simpsons_group
  )
  ) + geom_bar(position = "fill") +
  labs(x = "GLS group",
       y = "Percentage %") + 
  geom_text(
    aes(x = glsgroup, y = pct, label = label),
    position_stack(),
    check_overlap = TRUE
  ) +
  theme_minimal() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(name = "Simpsons")
############################################################
# unused

ggplot(
  data = hrt,
  aes(
    x = gls_group,
    fill = sex 
  )
) + geom_bar(position = "dodge",
             aes(y = (..count..) / sum(..count..))) + 
  labs(title = "Figure (1) Bar graph showing distribution of sex across GLS groups",
       x = "GLS group",
       y = "Percentage %") + 
  theme_minimal() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_discrete(name = "Sex") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

ggplot(
  data = hrt,
  aes(
    x = gls_group,
    fill = lipid_profile 
  )
) + geom_bar(position = "dodge") + 
  labs(title = "Figure (2) Bar graph showing distribution of Dyslipidemia across GLS groups",
       x = "GLS group",
       y = "Count") + 
  theme_minimal() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_fill_discrete(name = "Lipid profile") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
