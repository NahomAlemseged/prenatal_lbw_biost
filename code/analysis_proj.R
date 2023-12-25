# LIBRARY LOAD
library (ggplot2)
library (dplyr)
library(readr)
library(tidyr)
library(tidyverse)
library(gtsummary)
###########################################################

setwd("C:\\Users\\nahomw\\Desktop\\assignments\\First Quarter\\Biostat\\Project_data_analysis")
df <- read_table("BirthsKingCounty2001-Biost514-517-2022.txt")
nrow(df)  # See the umber of Columns 
sum(is.na(df))  # Detect Null values 
## FACTORING VARIABLES######
df_1 <- df
df$sex <- factor(df$sex, levels = c("F","M"), labels = c(0,1))
df$race <- factor(df$race, levels = c("asian","black","hispanic","white","other"), labels = c(1,2,3,4,5))
df$smoker <- factor(df$smoker, levels = c("N","Y"), labels = c(0,1))
df$drinker <- factor(df$drinker, levels = c("N","Y"), labels = c(0,1))
###############################################################################
# Descriptive table
###############################################################################
df$married <- as.character(df$married)
df$welfare <- as.character(df$welfare)
typeof(df$married)
typeof(df$welfare)

df <- df%>%
  select(-c(("plural")))
  
df %>%
  tbl_summary(by = firstep,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 2
  ) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**")%>%
  modify_caption("**Table 1: Respondents Characteristics**") %>%
  bold_labels()

chisq.test(df$race, df$firstep, correct=FALSE)
#######################################################################
# Summary table of only categorical variables
#######################################################################

df %>%
  # select(c("sex","married","welfare","smoker","drinker","firstep",))%>%
  tbl_summary(by = firstep,
              statistic = list(all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 2
  ) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**")%>%
  modify_caption("**Table 2: Association of dichotomous variables**") %>%
  bold_labels()

df_1 <- df
###################################################################
# OUTLIERS
####################################################################
# BOplot
ggplot(df, aes(x=bwt),  fill = label) + 
  geom_boxplot(size = .8, width = 0.8) +
  geom_histogram(color = "yellow")

ggplot(df, aes(x=bwt),  fill = label) + 
  geom_boxplot(size = .1, width = 0.1)
df_1 <- df
###################################################################
# t test and wilkox independent comoparison between bwt and firstep
####################################################################

t.test (bwt ~ firstep, var.equal=TRUE, data = df)
wilcox.test(bwt ~ firstep, data=df, na.rm=TRUE, paired=FALSE)
############################################################################
df_ <- df %>%
  subset(smoker == 1) 
t.test(bwt ~ firstep, var.equal=TRUE, data = df_)

df_ <- df %>%
  subset(smoker == 0) 
t.test(bwt ~ firstep, var.equal=TRUE, data = df_)

df_ <- df %>%
  subset(welfare == 1) 
t.test(bwt ~ firstep, var.equal=TRUE, data = df_)

df_ <- df %>%
  subset(welfare == 0) 
t.test(bwt ~ firstep, var.equal=TRUE, data = df_)

df_ <- df %>%
  subset(married == 1) 
t.test(bwt ~ firstep, var.equal=TRUE, data = df_)

df_ <- df %>%
  subset(married == 0) 
t.test(bwt ~ firstep, var.equal=TRUE, data = df_)

########################################################################
df_1$married <- factor(df_1$married, levels = c(0,1), labels = c("Single","married"))
df_1$firstep <- factor(df_1$firstep, levels = c(0,1), labels = c("nonfirstep","firstep"))
df_1$welfare <- factor(df_1$welfare, levels = c(0,1), labels = c("not_on_welfare","on_welfare"))
df_1$smoker <- factor(df_1$smoker, levels = c(0,1), labels = c("non_smoker","smoker"))
########################################################################
ggplot(df_1,aes(x= firstep, y=bwt, group = firstep)) + 
  geom_boxplot() + 
  facet_grid(cols = vars(married))
####################################################################
########################################################################
ggplot(df_1,aes(x= firstep, y=bwt, fill = firstep)) + 
  geom_boxplot() + 
  facet_grid(cols = vars(welfare))
########################################################################
########################################################################
ggplot(df_1,aes(x= firstep, y=bwt, fill = firstep)) + 
  geom_boxplot() + 
  facet_grid(cols = vars(smoker))
########################################################################
df_1$race <- factor(df_1$race, levels = c(1,2,3,4,5), labels =  c("asian","black","hispanic","white","other"))

########################################################################
ggplot(df_1,aes(x= firstep, y=bwt, fill = firstep)) + 
  geom_boxplot() + 
  facet_grid(cols = vars(race))
########################################################################
# STRATIFIED OW BIRTH WEIGHT
########################################################################
df_1$bwt_1 <- cut(df_1$bwt, breaks = c(-Inf,1500,2500,Inf), labels = c("Very low birth weight","Low birth weight","Normal"))
#################################################################################
df %>%
  tbl_summary(by = firstep,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 2
  ) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**")%>%
  modify_caption("**Table 1: Respondents Characteristics**") %>%
  bold_labels()
######################################################################################
######################################################################################





