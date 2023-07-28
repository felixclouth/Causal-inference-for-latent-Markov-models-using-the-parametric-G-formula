
# last edit: 28/07/2023

# accompanying code to the manuscript:
# "Causal inference for latent Markov models using the parametric G-formula"

# F.J. Clouth, MSc.
# Department of Methodology and Statistics, Tilburg University
# f.j.clouth@tilburguniversity.edu




### Part 1
# Data preparation

library(dplyr)
library(foreign)
library(rio)
library(ggplot2)
library(haven)
library(mice)
library(parabar)

setwd("adjust/wd")


# LISS data can be accessed here: https://www.dataarchive.lissdata.nl/study_units/view/1
# load health data from the LISS core study wave 2017
health17 <- read.spss("ch17j_EN_1.0p.sav", to.data.frame = T)
# load background data from the LISS core study wave 2017
back17 <- read_sav("avars_201711_EN_1.0p.sav")

# recoding
back17$id <- back17$nomem_encr

table(back17$geslacht)
back17$gender <- "Male"
back17$gender[back17$geslacht==2] <- "Female"
table(back17$geslacht, back17$gender, useNA = "always")

table(back17$leeftijd)
back17$age <- back17$leeftijd

table(back17$burgstat)
back17$HHstatus <- NA
back17$HHstatus[back17$burgstat==1] <- "Married"
back17$HHstatus[back17$burgstat==2] <- "Separated"
back17$HHstatus[back17$burgstat==3] <- "Divorced"
back17$HHstatus[back17$burgstat==4] <- "Widowed"
back17$HHstatus[back17$burgstat==5] <- "Never been married"
table(back17$burgstat, back17$HHstatus, useNA = "always")

summary(back17$brutoink_f)
back17$income <- back17$brutoink_f
summary(back17$brutohh_f)
back17$HHincome <- back17$brutohh_f

table(back17$oplzon)
table(back17$oplcat)
back17$education <- NA
back17$education[back17$oplcat==1] <- "Primary school"
back17$education[back17$oplcat==2] <- "vmbo"
back17$education[back17$oplcat==3] <- "havo/vwo"
back17$education[back17$oplcat==4] <- "mbo"
back17$education[back17$oplcat==5] <- "hbo"
back17$education[back17$oplcat==6] <- "wo"
table(back17$oplcat, back17$education, useNA = "always")

table(back17$herkomstgroep)
back17$origin <- NA
back17$origin[back17$herkomstgroep==0] <- "Dutch"
back17$origin[back17$herkomstgroep==101] <- "first gen West"
back17$origin[back17$herkomstgroep==102] <- "first gen non-West"
back17$origin[back17$herkomstgroep==201] <- "second gen West"
back17$origin[back17$herkomstgroep==202] <- "second gen non-West"
table(back17$herkomstgroep, back17$origin, useNA = "always")

table(back17$belbezig)
back17$job <- NA
back17$job[back17$belbezig==1] <- "Paid employment"
back17$job[back17$belbezig==2] <- "Works or assists in family business"
back17$job[back17$belbezig==3] <- "Autonomous professional, freelancer, or self-employed"
back17$job[back17$belbezig==4] <- "Job seeker following job loss"
back17$job[back17$belbezig==5] <- "First-time job seeker"
back17$job[back17$belbezig==6] <- "Exempted from job seeking following job loss"
back17$job[back17$belbezig==7] <- "Attends school or is studying"
back17$job[back17$belbezig==8] <- "Takes care of the housekeeping"
back17$job[back17$belbezig==9] <- "Is pensioner ([voluntary] early retirement, old age pension scheme)"
back17$job[back17$belbezig==10] <- "Has (partial) work disability"
back17$job[back17$belbezig==11] <- "Performs unpaid work while retaining unemployment benefit"
back17$job[back17$belbezig==12] <- "Performs voluntary work"
back17$job[back17$belbezig==13] <- "Does something else"
back17$job[back17$belbezig==14] <- "Is too young to have an occupation"
table(back17$belbezig, back17$job, useNA = "always")

back17$job_cat <- NA
back17$job_cat[back17$belbezig==1] <- "Paid work"
back17$job_cat[back17$belbezig==2] <- "Paid work"
back17$job_cat[back17$belbezig==3] <- "Paid work"
back17$job_cat[back17$belbezig==4] <- "No paid work"
back17$job_cat[back17$belbezig==5] <- "No paid work"
back17$job_cat[back17$belbezig==6] <- "No paid work"
back17$job_cat[back17$belbezig==7] <- "School/ too young"
back17$job_cat[back17$belbezig==8] <- "No paid work"
back17$job_cat[back17$belbezig==9] <- "Pensioner"
back17$job_cat[back17$belbezig==10] <- "No paid work"
back17$job_cat[back17$belbezig==11] <- "No paid work"
back17$job_cat[back17$belbezig==12] <- "No paid work"
back17$job_cat[back17$belbezig==13] <- "No paid work"
back17$job_cat[back17$belbezig==14] <- "School/ too young"
table(back17$job, back17$job_cat, useNA = "always")
table(back17$job_cat, useNA = "always")

back17 <- back17 %>%
  select(id, job_cat, gender, age, HHstatus, income, HHincome, education, origin)


health17$id <- health17$nomem_encr

health17$wave <- 1
health17$year <- 2017

table(health17$ch17j004, useNA = "always")
health17$health <- NA
health17$health[health17$ch17j004 == "poor"] <- 1
health17$health[health17$ch17j004 == "moderate"] <- 2
health17$health[health17$ch17j004 == "good"] <- 3
health17$health[health17$ch17j004 == "very good"] <- 4
health17$health[health17$ch17j004 == "excellent"] <- 5
table(health17$health, health17$ch17j004, useNA = "always")

table(health17$ch17j011, useNA = "always")
health17$anxious <- NA
health17$anxious[health17$ch17j011 == "never"] <- 1
health17$anxious[health17$ch17j011 == "seldom"] <- 2
health17$anxious[health17$ch17j011 == "sometimes"] <- 3
health17$anxious[health17$ch17j011 == "often"] <- 4
health17$anxious[health17$ch17j011 == "mostly"] <- 5
health17$anxious[health17$ch17j011 == "continuously"] <- 6
table(health17$anxious, useNA = "always")

table(health17$ch17j012, useNA = "always")
health17$feeldown <- NA
health17$feeldown[health17$ch17j012 == "never"] <- 1
health17$feeldown[health17$ch17j012 == "seldom"] <- 2
health17$feeldown[health17$ch17j012 == "sometimes"] <- 3
health17$feeldown[health17$ch17j012 == "often"] <- 4
health17$feeldown[health17$ch17j012 == "mostly"] <- 5
health17$feeldown[health17$ch17j012 == "continuously"] <- 6
table(health17$feeldown, useNA = "always")

table(health17$ch17j013, useNA = "always")
health17$feelcalm <- NA
health17$feelcalm[health17$ch17j013 == "never"] <- 6
health17$feelcalm[health17$ch17j013 == "seldom"] <- 5
health17$feelcalm[health17$ch17j013 == "sometimes"] <- 4
health17$feelcalm[health17$ch17j013 == "often"] <- 3
health17$feelcalm[health17$ch17j013 == "mostly"] <- 2
health17$feelcalm[health17$ch17j013 == "continuously"] <- 1
table(health17$feelcalm, useNA = "always")

table(health17$ch17j014, useNA = "always")
health17$depressed <- NA
health17$depressed[health17$ch17j014 == "never"] <- 1
health17$depressed[health17$ch17j014 == "seldom"] <- 2
health17$depressed[health17$ch17j014 == "sometimes"] <- 3
health17$depressed[health17$ch17j014 == "often"] <- 4
health17$depressed[health17$ch17j014 == "mostly"] <- 5
health17$depressed[health17$ch17j014 == "continuously"] <- 6
table(health17$depressed, useNA = "always")

table(health17$ch17j015, useNA = "always")
health17$happy <- NA
health17$happy[health17$ch17j015 == "never"] <- 6
health17$happy[health17$ch17j015 == "seldom"] <- 5
health17$happy[health17$ch17j015 == "sometimes"] <- 4
health17$happy[health17$ch17j015 == "often"] <- 3
health17$happy[health17$ch17j015 == "mostly"] <- 2
health17$happy[health17$ch17j015 == "continuously"] <- 1
table(health17$happy, useNA = "always")

health17 <- health17 %>%
  select(id, wave, year, health, anxious, feeldown, feelcalm, depressed, happy)

# merge background data and health data
wave17 <- merge(back17, health17, by = "id")

table(wave17$job_cat, useNA = "always")

# filter out individuals outside the workforce
wave17 <- wave17 %>%
  filter(job_cat != "School/ too young") %>%
  filter(job_cat != "Pensioner")
table(wave17$job_cat, useNA = "always")




# repeat the same for wave 2018
health18 <- read.spss("ch18k_EN_1.0p.sav", to.data.frame = T)
back18 <- read_sav("avars_201811_EN_1.0p.sav")

back18$id <- back18$nomem_encr

table(back18$geslacht)
back18$gender <- "Male"
back18$gender[back18$geslacht==2] <- "Female"
table(back18$geslacht, back18$gender, useNA = "always")

table(back18$leeftijd)
back18$age <- back18$leeftijd

table(back18$burgstat)
back18$HHstatus <- NA
back18$HHstatus[back18$burgstat==1] <- "Married"
back18$HHstatus[back18$burgstat==2] <- "Separated"
back18$HHstatus[back18$burgstat==3] <- "Divorced"
back18$HHstatus[back18$burgstat==4] <- "Widowed"
back18$HHstatus[back18$burgstat==5] <- "Never been married"
table(back18$burgstat, back18$HHstatus, useNA = "always")

summary(back18$brutoink_f)
back18$income <- back18$brutoink_f
summary(back18$brutohh_f)
back18$HHincome <- back18$brutohh_f

table(back18$oplzon)
table(back18$oplcat)
back18$education <- NA
back18$education[back18$oplcat==1] <- "Primary school"
back18$education[back18$oplcat==2] <- "vmbo"
back18$education[back18$oplcat==3] <- "havo/vwo"
back18$education[back18$oplcat==4] <- "mbo"
back18$education[back18$oplcat==5] <- "hbo"
back18$education[back18$oplcat==6] <- "wo"
table(back18$oplcat, back18$education, useNA = "always")

table(back18$herkomstgroep)
back18$origin <- NA
back18$origin[back18$herkomstgroep==0] <- "Dutch"
back18$origin[back18$herkomstgroep==101] <- "first gen West"
back18$origin[back18$herkomstgroep==102] <- "first gen non-West"
back18$origin[back18$herkomstgroep==201] <- "second gen West"
back18$origin[back18$herkomstgroep==202] <- "second gen non-West"
table(back18$herkomstgroep, back18$origin, useNA = "always")

table(back18$belbezig)
back18$job <- NA
back18$job[back18$belbezig==1] <- "Paid employment"
back18$job[back18$belbezig==2] <- "Works or assists in family business"
back18$job[back18$belbezig==3] <- "Autonomous professional, freelancer, or self-employed"
back18$job[back18$belbezig==4] <- "Job seeker following job loss"
back18$job[back18$belbezig==5] <- "First-time job seeker"
back18$job[back18$belbezig==6] <- "Exempted from job seeking following job loss"
back18$job[back18$belbezig==7] <- "Attends school or is studying"
back18$job[back18$belbezig==8] <- "Takes care of the housekeeping"
back18$job[back18$belbezig==9] <- "Is pensioner ([voluntary] early retirement, old age pension scheme)"
back18$job[back18$belbezig==10] <- "Has (partial) work disability"
back18$job[back18$belbezig==11] <- "Performs unpaid work while retaining unemployment benefit"
back18$job[back18$belbezig==12] <- "Performs voluntary work"
back18$job[back18$belbezig==13] <- "Does something else"
back18$job[back18$belbezig==14] <- "Is too young to have an occupation"
table(back18$belbezig, back18$job, useNA = "always")

back18$job_cat <- NA
back18$job_cat[back18$belbezig==1] <- "Paid work"
back18$job_cat[back18$belbezig==2] <- "Paid work"
back18$job_cat[back18$belbezig==3] <- "Paid work"
back18$job_cat[back18$belbezig==4] <- "No paid work"
back18$job_cat[back18$belbezig==5] <- "No paid work"
back18$job_cat[back18$belbezig==6] <- "No paid work"
back18$job_cat[back18$belbezig==7] <- "School/ too young"
back18$job_cat[back18$belbezig==8] <- "No paid work"
back18$job_cat[back18$belbezig==9] <- "Pensioner"
back18$job_cat[back18$belbezig==10] <- "No paid work"
back18$job_cat[back18$belbezig==11] <- "No paid work"
back18$job_cat[back18$belbezig==12] <- "No paid work"
back18$job_cat[back18$belbezig==13] <- "No paid work"
back18$job_cat[back18$belbezig==14] <- "School/ too young"
table(back18$job, back18$job_cat, useNA = "always")
table(back18$job_cat, useNA = "always")

back18 <- back18 %>%
  select(id, job_cat, gender, age, HHstatus, income, HHincome, education, origin)


health18$id <- health18$nomem_encr

health18$wave <- 2
health18$year <- 2018

table(health18$ch18k004, useNA = "always")
health18$health <- NA
health18$health[health18$ch18k004 == "poor"] <- 1
health18$health[health18$ch18k004 == "moderate"] <- 2
health18$health[health18$ch18k004 == "good"] <- 3
health18$health[health18$ch18k004 == "very good"] <- 4
health18$health[health18$ch18k004 == "excellent"] <- 5
table(health18$health, health18$ch18k004, useNA = "always")

table(health18$ch18k011, useNA = "always")
health18$anxious <- NA
health18$anxious[health18$ch18k011 == "never"] <- 1
health18$anxious[health18$ch18k011 == "seldom"] <- 2
health18$anxious[health18$ch18k011 == "sometimes"] <- 3
health18$anxious[health18$ch18k011 == "often"] <- 4
health18$anxious[health18$ch18k011 == "mostly"] <- 5
health18$anxious[health18$ch18k011 == "continuously"] <- 6
table(health18$anxious, useNA = "always")

table(health18$ch18k012, useNA = "always")
health18$feeldown <- NA
health18$feeldown[health18$ch18k012 == "never"] <- 1
health18$feeldown[health18$ch18k012 == "seldom"] <- 2
health18$feeldown[health18$ch18k012 == "sometimes"] <- 3
health18$feeldown[health18$ch18k012 == "often"] <- 4
health18$feeldown[health18$ch18k012 == "mostly"] <- 5
health18$feeldown[health18$ch18k012 == "continuously"] <- 6
table(health18$feeldown, useNA = "always")

table(health18$ch18k013, useNA = "always")
health18$feelcalm <- NA
health18$feelcalm[health18$ch18k013 == "never"] <- 6
health18$feelcalm[health18$ch18k013 == "seldom"] <- 5
health18$feelcalm[health18$ch18k013 == "sometimes"] <- 4
health18$feelcalm[health18$ch18k013 == "often"] <- 3
health18$feelcalm[health18$ch18k013 == "mostly"] <- 2
health18$feelcalm[health18$ch18k013 == "continuously"] <- 1
table(health18$feelcalm, useNA = "always")

table(health18$ch18k014, useNA = "always")
health18$depressed <- NA
health18$depressed[health18$ch18k014 == "never"] <- 1
health18$depressed[health18$ch18k014 == "seldom"] <- 2
health18$depressed[health18$ch18k014 == "sometimes"] <- 3
health18$depressed[health18$ch18k014 == "often"] <- 4
health18$depressed[health18$ch18k014 == "mostly"] <- 5
health18$depressed[health18$ch18k014 == "continuously"] <- 6
table(health18$depressed, useNA = "always")

table(health18$ch18k015, useNA = "always")
health18$happy <- NA
health18$happy[health18$ch18k015 == "never"] <- 6
health18$happy[health18$ch18k015 == "seldom"] <- 5
health18$happy[health18$ch18k015 == "sometimes"] <- 4
health18$happy[health18$ch18k015 == "often"] <- 3
health18$happy[health18$ch18k015 == "mostly"] <- 2
health18$happy[health18$ch18k015 == "continuously"] <- 1
table(health18$happy, useNA = "always")

health18 <- health18 %>%
  select(id, wave, year, health, anxious, feeldown, feelcalm, depressed, happy)


wave18 <- merge(back18, health18, by = "id")

table(wave18$job_cat, useNA = "always")

wave18 <- wave18 %>%
  filter(job_cat != "School/ too young") %>%
  filter(job_cat != "Pensioner")
table(wave18$job_cat, useNA = "always")




# repeat the same for wave 2019
health19 <- read.spss("ch19l_EN_1.0p.sav", to.data.frame = T)
back19 <- read_sav("avars_201911_EN_1.0p.sav")


back19$id <- back19$nomem_encr

table(back19$geslacht)
back19$gender <- "Male"
back19$gender[back19$geslacht==2] <- "Female"
table(back19$geslacht, back19$gender, useNA = "always")

table(back19$leeftijd)
back19$age <- back19$leeftijd

table(back19$burgstat)
back19$HHstatus <- NA
back19$HHstatus[back19$burgstat==1] <- "Married"
back19$HHstatus[back19$burgstat==2] <- "Separated"
back19$HHstatus[back19$burgstat==3] <- "Divorced"
back19$HHstatus[back19$burgstat==4] <- "Widowed"
back19$HHstatus[back19$burgstat==5] <- "Never been married"
table(back19$burgstat, back19$HHstatus, useNA = "always")

summary(back19$brutoink_f)
back19$income <- back19$brutoink_f
summary(back19$brutohh_f)
back19$HHincome <- back19$brutohh_f

table(back19$oplzon)
table(back19$oplcat)
back19$education <- NA
back19$education[back19$oplcat==1] <- "Primary school"
back19$education[back19$oplcat==2] <- "vmbo"
back19$education[back19$oplcat==3] <- "havo/vwo"
back19$education[back19$oplcat==4] <- "mbo"
back19$education[back19$oplcat==5] <- "hbo"
back19$education[back19$oplcat==6] <- "wo"
table(back19$oplcat, back19$education, useNA = "always")

table(back19$herkomstgroep)
back19$origin <- NA
back19$origin[back19$herkomstgroep==0] <- "Dutch"
back19$origin[back19$herkomstgroep==101] <- "first gen West"
back19$origin[back19$herkomstgroep==102] <- "first gen non-West"
back19$origin[back19$herkomstgroep==201] <- "second gen West"
back19$origin[back19$herkomstgroep==202] <- "second gen non-West"
table(back19$herkomstgroep, back19$origin, useNA = "always")

table(back19$belbezig)
back19$job <- NA
back19$job[back19$belbezig==1] <- "Paid employment"
back19$job[back19$belbezig==2] <- "Works or assists in family business"
back19$job[back19$belbezig==3] <- "Autonomous professional, freelancer, or self-employed"
back19$job[back19$belbezig==4] <- "Job seeker following job loss"
back19$job[back19$belbezig==5] <- "First-time job seeker"
back19$job[back19$belbezig==6] <- "Exempted from job seeking following job loss"
back19$job[back19$belbezig==7] <- "Attends school or is studying"
back19$job[back19$belbezig==8] <- "Takes care of the housekeeping"
back19$job[back19$belbezig==9] <- "Is pensioner ([voluntary] early retirement, old age pension scheme)"
back19$job[back19$belbezig==10] <- "Has (partial) work disability"
back19$job[back19$belbezig==11] <- "Performs unpaid work while retaining unemployment benefit"
back19$job[back19$belbezig==12] <- "Performs voluntary work"
back19$job[back19$belbezig==13] <- "Does something else"
back19$job[back19$belbezig==14] <- "Is too young to have an occupation"
table(back19$belbezig, back19$job, useNA = "always")

back19$job_cat <- NA
back19$job_cat[back19$belbezig==1] <- "Paid work"
back19$job_cat[back19$belbezig==2] <- "Paid work"
back19$job_cat[back19$belbezig==3] <- "Paid work"
back19$job_cat[back19$belbezig==4] <- "No paid work"
back19$job_cat[back19$belbezig==5] <- "No paid work"
back19$job_cat[back19$belbezig==6] <- "No paid work"
back19$job_cat[back19$belbezig==7] <- "School/ too young"
back19$job_cat[back19$belbezig==8] <- "No paid work"
back19$job_cat[back19$belbezig==9] <- "Pensioner"
back19$job_cat[back19$belbezig==10] <- "No paid work"
back19$job_cat[back19$belbezig==11] <- "No paid work"
back19$job_cat[back19$belbezig==12] <- "No paid work"
back19$job_cat[back19$belbezig==13] <- "No paid work"
back19$job_cat[back19$belbezig==14] <- "School/ too young"
table(back19$job, back19$job_cat, useNA = "always")
table(back19$job_cat, useNA = "always")

back19 <- back19 %>%
  select(id, job_cat, gender, age, HHstatus, income, HHincome, education, origin)


health19$id <- health19$nomem_encr

health19$wave <- 3
health19$year <- 2019

table(health19$ch19l004, useNA = "always")
health19$health <- NA
health19$health[health19$ch19l004 == "poor"] <- 1
health19$health[health19$ch19l004 == "moderate"] <- 2
health19$health[health19$ch19l004 == "good"] <- 3
health19$health[health19$ch19l004 == "very good"] <- 4
health19$health[health19$ch19l004 == "excellent"] <- 5
table(health19$health, health19$ch19l004, useNA = "always")

table(health19$ch19l011, useNA = "always")
health19$anxious <- NA
health19$anxious[health19$ch19l011 == "never"] <- 1
health19$anxious[health19$ch19l011 == "seldom"] <- 2
health19$anxious[health19$ch19l011 == "sometimes"] <- 3
health19$anxious[health19$ch19l011 == "often"] <- 4
health19$anxious[health19$ch19l011 == "mostly"] <- 5
health19$anxious[health19$ch19l011 == "continuously"] <- 6
table(health19$anxious, useNA = "always")

table(health19$ch19l012, useNA = "always")
health19$feeldown <- NA
health19$feeldown[health19$ch19l012 == "never"] <- 1
health19$feeldown[health19$ch19l012 == "seldom"] <- 2
health19$feeldown[health19$ch19l012 == "sometimes"] <- 3
health19$feeldown[health19$ch19l012 == "often"] <- 4
health19$feeldown[health19$ch19l012 == "mostly"] <- 5
health19$feeldown[health19$ch19l012 == "continuously"] <- 6
table(health19$feeldown, useNA = "always")

table(health19$ch19l013, useNA = "always")
health19$feelcalm <- NA
health19$feelcalm[health19$ch19l013 == "never"] <- 6
health19$feelcalm[health19$ch19l013 == "seldom"] <- 5
health19$feelcalm[health19$ch19l013 == "sometimes"] <- 4
health19$feelcalm[health19$ch19l013 == "often"] <- 3
health19$feelcalm[health19$ch19l013 == "mostly"] <- 2
health19$feelcalm[health19$ch19l013 == "continuously"] <- 1
table(health19$feelcalm, useNA = "always")

table(health19$ch19l014, useNA = "always")
health19$depressed <- NA
health19$depressed[health19$ch19l014 == "never"] <- 1
health19$depressed[health19$ch19l014 == "seldom"] <- 2
health19$depressed[health19$ch19l014 == "sometimes"] <- 3
health19$depressed[health19$ch19l014 == "often"] <- 4
health19$depressed[health19$ch19l014 == "mostly"] <- 5
health19$depressed[health19$ch19l014 == "continuously"] <- 6
table(health19$depressed, useNA = "always")

table(health19$ch19l015, useNA = "always")
health19$happy <- NA
health19$happy[health19$ch19l015 == "never"] <- 6
health19$happy[health19$ch19l015 == "seldom"] <- 5
health19$happy[health19$ch19l015 == "sometimes"] <- 4
health19$happy[health19$ch19l015 == "often"] <- 3
health19$happy[health19$ch19l015 == "mostly"] <- 2
health19$happy[health19$ch19l015 == "continuously"] <- 1
table(health19$happy, useNA = "always")

health19 <- health19 %>%
  select(id, wave, year, health, anxious, feeldown, feelcalm, depressed, happy)


wave19 <- merge(back19, health19, by = "id")

table(wave19$job_cat, useNA = "always")

wave19 <- wave19 %>%
  filter(job_cat != "School/ too young") %>%
  filter(job_cat != "Pensioner")
table(wave19$job_cat, useNA = "always")





# repeat the same for wave 2020
health20 <- read.spss("ch20m_EN_1.0p.sav", to.data.frame = T)
back20 <- read_sav("avars_202011_EN_1.0p.sav")


back20$id <- back20$nomem_encr

table(back20$geslacht)
back20$gender <- "Male"
back20$gender[back20$geslacht==2] <- "Female"
table(back20$geslacht, back20$gender, useNA = "always")

table(back20$leeftijd)
back20$age <- back20$leeftijd

table(back20$burgstat)
back20$HHstatus <- NA
back20$HHstatus[back20$burgstat==1] <- "Married"
back20$HHstatus[back20$burgstat==2] <- "Separated"
back20$HHstatus[back20$burgstat==3] <- "Divorced"
back20$HHstatus[back20$burgstat==4] <- "Widowed"
back20$HHstatus[back20$burgstat==5] <- "Never been married"
table(back20$burgstat, back20$HHstatus, useNA = "always")

summary(back20$brutoink_f)
back20$income <- back20$brutoink_f
summary(back20$brutohh_f)
back20$HHincome <- back20$brutohh_f

table(back20$oplzon)
table(back20$oplcat)
back20$education <- NA
back20$education[back20$oplcat==1] <- "Primary school"
back20$education[back20$oplcat==2] <- "vmbo"
back20$education[back20$oplcat==3] <- "havo/vwo"
back20$education[back20$oplcat==4] <- "mbo"
back20$education[back20$oplcat==5] <- "hbo"
back20$education[back20$oplcat==6] <- "wo"
table(back20$oplcat, back20$education, useNA = "always")

table(back20$herkomstgroep)
back20$origin <- NA
back20$origin[back20$herkomstgroep==0] <- "Dutch"
back20$origin[back20$herkomstgroep==101] <- "first gen West"
back20$origin[back20$herkomstgroep==102] <- "first gen non-West"
back20$origin[back20$herkomstgroep==201] <- "second gen West"
back20$origin[back20$herkomstgroep==202] <- "second gen non-West"
table(back20$herkomstgroep, back20$origin, useNA = "always")

table(back20$belbezig)
back20$job <- NA
back20$job[back20$belbezig==1] <- "Paid employment"
back20$job[back20$belbezig==2] <- "Works or assists in family business"
back20$job[back20$belbezig==3] <- "Autonomous professional, freelancer, or self-employed"
back20$job[back20$belbezig==4] <- "Job seeker following job loss"
back20$job[back20$belbezig==5] <- "First-time job seeker"
back20$job[back20$belbezig==6] <- "Exempted from job seeking following job loss"
back20$job[back20$belbezig==7] <- "Attends school or is studying"
back20$job[back20$belbezig==8] <- "Takes care of the housekeeping"
back20$job[back20$belbezig==9] <- "Is pensioner ([voluntary] early retirement, old age pension scheme)"
back20$job[back20$belbezig==10] <- "Has (partial) work disability"
back20$job[back20$belbezig==11] <- "Performs unpaid work while retaining unemployment benefit"
back20$job[back20$belbezig==12] <- "Performs voluntary work"
back20$job[back20$belbezig==13] <- "Does something else"
back20$job[back20$belbezig==14] <- "Is too young to have an occupation"
table(back20$belbezig, back20$job, useNA = "always")

back20$job_cat <- NA
back20$job_cat[back20$belbezig==1] <- "Paid work"
back20$job_cat[back20$belbezig==2] <- "Paid work"
back20$job_cat[back20$belbezig==3] <- "Paid work"
back20$job_cat[back20$belbezig==4] <- "No paid work"
back20$job_cat[back20$belbezig==5] <- "No paid work"
back20$job_cat[back20$belbezig==6] <- "No paid work"
back20$job_cat[back20$belbezig==7] <- "School/ too young"
back20$job_cat[back20$belbezig==8] <- "No paid work"
back20$job_cat[back20$belbezig==9] <- "Pensioner"
back20$job_cat[back20$belbezig==10] <- "No paid work"
back20$job_cat[back20$belbezig==11] <- "No paid work"
back20$job_cat[back20$belbezig==12] <- "No paid work"
back20$job_cat[back20$belbezig==13] <- "No paid work"
back20$job_cat[back20$belbezig==14] <- "School/ too young"
table(back20$job, back20$job_cat, useNA = "always")
table(back20$job_cat, useNA = "always")

back20 <- back20 %>%
  select(id, job_cat, gender, age, HHstatus, income, HHincome, education, origin)


health20$id <- health20$nomem_encr

health20$wave <- 4
health20$year <- 2020

table(health20$ch20m004, useNA = "always")
health20$health <- NA
health20$health[health20$ch20m004 == "poor"] <- 1
health20$health[health20$ch20m004 == "moderate"] <- 2
health20$health[health20$ch20m004 == "good"] <- 3
health20$health[health20$ch20m004 == "very good"] <- 4
health20$health[health20$ch20m004 == "excellent"] <- 5
table(health20$health, health20$ch20m004, useNA = "always")

table(health20$ch20m011, useNA = "always")
health20$anxious <- NA
health20$anxious[health20$ch20m011 == "never"] <- 1
health20$anxious[health20$ch20m011 == "seldom"] <- 2
health20$anxious[health20$ch20m011 == "sometimes"] <- 3
health20$anxious[health20$ch20m011 == "often"] <- 4
health20$anxious[health20$ch20m011 == "mostly"] <- 5
health20$anxious[health20$ch20m011 == "continuously"] <- 6
table(health20$anxious, useNA = "always")

table(health20$ch20m012, useNA = "always")
health20$feeldown <- NA
health20$feeldown[health20$ch20m012 == "never"] <- 1
health20$feeldown[health20$ch20m012 == "seldom"] <- 2
health20$feeldown[health20$ch20m012 == "sometimes"] <- 3
health20$feeldown[health20$ch20m012 == "often"] <- 4
health20$feeldown[health20$ch20m012 == "mostly"] <- 5
health20$feeldown[health20$ch20m012 == "continuously"] <- 6
table(health20$feeldown, useNA = "always")

table(health20$ch20m013, useNA = "always")
health20$feelcalm <- NA
health20$feelcalm[health20$ch20m013 == "never"] <- 6
health20$feelcalm[health20$ch20m013 == "seldom"] <- 5
health20$feelcalm[health20$ch20m013 == "sometimes"] <- 4
health20$feelcalm[health20$ch20m013 == "often"] <- 3
health20$feelcalm[health20$ch20m013 == "mostly"] <- 2
health20$feelcalm[health20$ch20m013 == "continuously"] <- 1
table(health20$feelcalm, useNA = "always")

table(health20$ch20m014, useNA = "always")
health20$depressed <- NA
health20$depressed[health20$ch20m014 == "never"] <- 1
health20$depressed[health20$ch20m014 == "seldom"] <- 2
health20$depressed[health20$ch20m014 == "sometimes"] <- 3
health20$depressed[health20$ch20m014 == "often"] <- 4
health20$depressed[health20$ch20m014 == "mostly"] <- 5
health20$depressed[health20$ch20m014 == "continuously"] <- 6
table(health20$depressed, useNA = "always")

table(health20$ch20m015, useNA = "always")
health20$happy <- NA
health20$happy[health20$ch20m015 == "never"] <- 6
health20$happy[health20$ch20m015 == "seldom"] <- 5
health20$happy[health20$ch20m015 == "sometimes"] <- 4
health20$happy[health20$ch20m015 == "often"] <- 3
health20$happy[health20$ch20m015 == "mostly"] <- 2
health20$happy[health20$ch20m015 == "continuously"] <- 1
table(health20$happy, useNA = "always")

health20 <- health20 %>%
  select(id, wave, year, health, anxious, feeldown, feelcalm, depressed, happy)


wave20 <- merge(back20, health20, by = "id")

table(wave20$job_cat, useNA = "always")

wave20 <- wave20 %>%
  filter(job_cat != "School/ too young") %>%
  filter(job_cat != "Pensioner")
table(wave20$job_cat, useNA = "always")




# repeat the same for wave 2021
health21 <- read.spss("ch21n_EN_1.0p.sav", to.data.frame = T)
back21 <- read_sav("avars_202111_EN_1.0p.sav")


back21$id <- back21$nomem_encr

table(back21$geslacht)
back21$gender <- "Male"
back21$gender[back21$geslacht==2] <- "Female"
table(back21$geslacht, back21$gender, useNA = "always")

table(back21$leeftijd)
back21$age <- back21$leeftijd

table(back21$burgstat)
back21$HHstatus <- NA
back21$HHstatus[back21$burgstat==1] <- "Married"
back21$HHstatus[back21$burgstat==2] <- "Separated"
back21$HHstatus[back21$burgstat==3] <- "Divorced"
back21$HHstatus[back21$burgstat==4] <- "Widowed"
back21$HHstatus[back21$burgstat==5] <- "Never been married"
table(back21$burgstat, back21$HHstatus, useNA = "always")

summary(back21$brutoink_f)
back21$income <- back21$brutoink_f
summary(back21$brutohh_f)
back21$HHincome <- back21$brutohh_f

table(back21$oplzon)
table(back21$oplcat)
back21$education <- NA
back21$education[back21$oplcat==1] <- "Primary school"
back21$education[back21$oplcat==2] <- "vmbo"
back21$education[back21$oplcat==3] <- "havo/vwo"
back21$education[back21$oplcat==4] <- "mbo"
back21$education[back21$oplcat==5] <- "hbo"
back21$education[back21$oplcat==6] <- "wo"
table(back21$oplcat, back21$education, useNA = "always")

table(back21$herkomstgroep)
back21$origin <- NA
back21$origin[back21$herkomstgroep==0] <- "Dutch"
back21$origin[back21$herkomstgroep==101] <- "first gen West"
back21$origin[back21$herkomstgroep==102] <- "first gen non-West"
back21$origin[back21$herkomstgroep==201] <- "second gen West"
back21$origin[back21$herkomstgroep==202] <- "second gen non-West"
table(back21$herkomstgroep, back21$origin, useNA = "always")

table(back21$belbezig)
back21$job <- NA
back21$job[back21$belbezig==1] <- "Paid employment"
back21$job[back21$belbezig==2] <- "Works or assists in family business"
back21$job[back21$belbezig==3] <- "Autonomous professional, freelancer, or self-employed"
back21$job[back21$belbezig==4] <- "Job seeker following job loss"
back21$job[back21$belbezig==5] <- "First-time job seeker"
back21$job[back21$belbezig==6] <- "Exempted from job seeking following job loss"
back21$job[back21$belbezig==7] <- "Attends school or is studying"
back21$job[back21$belbezig==8] <- "Takes care of the housekeeping"
back21$job[back21$belbezig==9] <- "Is pensioner ([voluntary] early retirement, old age pension scheme)"
back21$job[back21$belbezig==10] <- "Has (partial) work disability"
back21$job[back21$belbezig==11] <- "Performs unpaid work while retaining unemployment benefit"
back21$job[back21$belbezig==12] <- "Performs voluntary work"
back21$job[back21$belbezig==13] <- "Does something else"
back21$job[back21$belbezig==14] <- "Is too young to have an occupation"
table(back21$belbezig, back21$job, useNA = "always")

back21$job_cat <- NA
back21$job_cat[back21$belbezig==1] <- "Paid work"
back21$job_cat[back21$belbezig==2] <- "Paid work"
back21$job_cat[back21$belbezig==3] <- "Paid work"
back21$job_cat[back21$belbezig==4] <- "No paid work"
back21$job_cat[back21$belbezig==5] <- "No paid work"
back21$job_cat[back21$belbezig==6] <- "No paid work"
back21$job_cat[back21$belbezig==7] <- "School/ too young"
back21$job_cat[back21$belbezig==8] <- "No paid work"
back21$job_cat[back21$belbezig==9] <- "Pensioner"
back21$job_cat[back21$belbezig==10] <- "No paid work"
back21$job_cat[back21$belbezig==11] <- "No paid work"
back21$job_cat[back21$belbezig==12] <- "No paid work"
back21$job_cat[back21$belbezig==13] <- "No paid work"
back21$job_cat[back21$belbezig==14] <- "School/ too young"
table(back21$job, back21$job_cat, useNA = "always")
table(back21$job_cat, useNA = "always")

back21 <- back21 %>%
  select(id, job_cat, gender, age, HHstatus, income, HHincome, education, origin)


health21$id <- health21$nomem_encr

health21$wave <- 5
health21$year <- 2021

table(health21$ch21n004, useNA = "always")
health21$health <- NA
health21$health[health21$ch21n004 == "poor"] <- 1
health21$health[health21$ch21n004 == "moderate"] <- 2
health21$health[health21$ch21n004 == "good"] <- 3
health21$health[health21$ch21n004 == "very good"] <- 4
health21$health[health21$ch21n004 == "excellent"] <- 5
table(health21$health, health21$ch21n004, useNA = "always")

table(health21$ch21n011, useNA = "always")
health21$anxious <- NA
health21$anxious[health21$ch21n011 == "never"] <- 1
health21$anxious[health21$ch21n011 == "seldom"] <- 2
health21$anxious[health21$ch21n011 == "sometimes"] <- 3
health21$anxious[health21$ch21n011 == "often"] <- 4
health21$anxious[health21$ch21n011 == "mostly"] <- 5
health21$anxious[health21$ch21n011 == "continuously"] <- 6
table(health21$anxious, useNA = "always")

table(health21$ch21n012, useNA = "always")
health21$feeldown <- NA
health21$feeldown[health21$ch21n012 == "never"] <- 1
health21$feeldown[health21$ch21n012 == "seldom"] <- 2
health21$feeldown[health21$ch21n012 == "sometimes"] <- 3
health21$feeldown[health21$ch21n012 == "often"] <- 4
health21$feeldown[health21$ch21n012 == "mostly"] <- 5
health21$feeldown[health21$ch21n012 == "continuously"] <- 6
table(health21$feeldown, useNA = "always")

table(health21$ch21n013, useNA = "always")
health21$feelcalm <- NA
health21$feelcalm[health21$ch21n013 == "never"] <- 6
health21$feelcalm[health21$ch21n013 == "seldom"] <- 5
health21$feelcalm[health21$ch21n013 == "sometimes"] <- 4
health21$feelcalm[health21$ch21n013 == "often"] <- 3
health21$feelcalm[health21$ch21n013 == "mostly"] <- 2
health21$feelcalm[health21$ch21n013 == "continuously"] <- 1
table(health21$feelcalm, useNA = "always")

table(health21$ch21n014, useNA = "always")
health21$depressed <- NA
health21$depressed[health21$ch21n014 == "never"] <- 1
health21$depressed[health21$ch21n014 == "seldom"] <- 2
health21$depressed[health21$ch21n014 == "sometimes"] <- 3
health21$depressed[health21$ch21n014 == "often"] <- 4
health21$depressed[health21$ch21n014 == "mostly"] <- 5
health21$depressed[health21$ch21n014 == "continuously"] <- 6
table(health21$depressed, useNA = "always")

table(health21$ch21n015, useNA = "always")
health21$happy <- NA
health21$happy[health21$ch21n015 == "never"] <- 6
health21$happy[health21$ch21n015 == "seldom"] <- 5
health21$happy[health21$ch21n015 == "sometimes"] <- 4
health21$happy[health21$ch21n015 == "often"] <- 3
health21$happy[health21$ch21n015 == "mostly"] <- 2
health21$happy[health21$ch21n015 == "continuously"] <- 1
table(health21$happy, useNA = "always")

health21 <- health21 %>%
  select(id, wave, year, health, anxious, feeldown, feelcalm, depressed, happy)


wave21 <- merge(back21, health21, by = "id")

table(wave21$job_cat, useNA = "always")

wave21 <- wave21 %>%
  filter(job_cat != "School/ too young") %>%
  filter(job_cat != "Pensioner")
table(wave21$job_cat, useNA = "always")



# append all waves
liss_job_long <- rbind(wave17, wave18, wave19, wave20, wave21)

export(liss_job_long, "liss_job_long.sav")





### Part 2
# perform analysis

# Step 1: run LCA step 1

makeNewSyntax <- function(syntaxName){
  
  newSyntaxToBe <- utils::capture.output(cat(paste("
//LG6.0//
version = 6.0
infile 'C:\Users\fjclouth\Desktop\Tilburg PhD\Projects\4th paper\multiple imputation analysis\liss_job_long.sav'

model
options
   maxthreads=8;
   algorithm 
      tolerance=1e-08 emtolerance=0.01 emiterations=250 nriterations=50 ;
   startvalues
      seed=0 sets=16 tolerance=1e-05 iterations=50;
   bayes
      categorical=1 variances=1 latent=1 poisson=1;
   montecarlo
      seed=0 sets=0 replicates=500 tolerance=1e-08;
   quadrature  nodes=10;
   missing  includeall;
   output      
      parameters=effect  betaopts=wl standarderrors profile probmeans=posterior
      loadings bivariateresiduals estimatedvalues=model reorderclasses;
   outfile  'C:\Users\fjclouth\Desktop\Tilburg PhD\Projects\4th paper\multiple imputation analysis\LISS_data3.sav'
      classification=posterior      keep id, job_cat, gender, age, HHstatus, income, 
		 HHincome, education, origin, wave, year, health;
variables
   dependent anxious, feeldown, feelcalm, depressed, happy;
   latent
      Cluster nominal 3;
equations
   Cluster <- 1;
   anxious <- 1 + Cluster;
   feeldown <- 1 + Cluster;
   feelcalm <- 1 + Cluster;
   depressed <- 1 + Cluster;
   happy <- 1 + Cluster;
end model")))
  utils::write.table(newSyntaxToBe, paste0(syntaxName, ".lgs"), row.names = FALSE, quote = FALSE, col.names = FALSE)
}

LG <- "adjust wd/LatentGOLD6.1/lg61.exe"

makeNewSyntax(syntaxName = "3classSyntax")

setwd("adjust wd")

shell(paste(LG, "3classSyntax.lgs", "/b"))


# classifications from Step1 are stored in "liss_data3.sav"

# Step 2: multiple imputation

liss_job_long <- read_sav("liss_data3.sav")

table(liss_job_long$job_cat, useNA = "always")
table(liss_job_long$gender, useNA = "always")
table(liss_job_long$age, useNA = "always")
table(liss_job_long$HHstatus, useNA = "always")
table(is.na(liss_job_long$HHincome), useNA = "always")
table(liss_job_long$education, useNA = "always")
table(liss_job_long$origin, useNA = "always")
table(liss_job_long$health, useNA = "always")

liss_job_long$education[liss_job_long$education == ""] <- NA
liss_job_long$origin[liss_job_long$origin == ""] <- NA


# copy values of non-timevarying covariates to other waves
filterNAeduID <- liss_job_long$id[is.na(liss_job_long$education)]

filterNAedu2 <- liss_job_long %>%
  filter(id %in% filterNAeduID)

filterNAedu2 <- liss_job_long %>%
  filter(id %in% filterNAeduID) %>%
  group_by(id) %>% 
  mutate(education_imp = ifelse(any(!is.na(education)), education, NA))

filterNAedu2 <- filterNAedu2 %>%
  arrange(desc(wave)) %>%
  group_by(id) %>% 
  mutate(education_imp2 = ifelse(any(!is.na(education)), education, NA))

checkedu <- filterNAedu2 %>%
  select(id, wave, education, education_imp, education_imp2)

filterNAedu2 <- filterNAedu2 %>%
  mutate(education_imp = ifelse(!is.na(education_imp2), education_imp2, education_imp))

liss_job_long$education_imp <- liss_job_long$education

for(i in liss_job_long$id) {
  if(i %in% filterNAedu2$id) {liss_job_long$education_imp[liss_job_long$id == i] <- filterNAedu2$education_imp[filterNAedu2$id == i]}
}

checkedu2 <- liss_job_long %>%
  select(id, wave, education, education_imp)

table(liss_job_long$education, liss_job_long$education_imp, useNA = "always")

filterNAorigID <- liss_job_long$id[is.na(liss_job_long$origin)]
filterNAorig <- liss_job_long %>%
  filter(id %in% filterNAorigID)

filterNAorig <- liss_job_long %>%
  filter(id %in% filterNAorigID) %>%
  group_by(id) %>% 
  mutate(origin_imp = ifelse(any(!is.na(origin)), origin, NA))

filterNAorig2 <- filterNAorig %>%
  arrange(desc(wave)) %>%
  group_by(id) %>% 
  mutate(origin_imp2 = ifelse(any(!is.na(origin)), origin, NA))

checkorig<- filterNAorig2 %>%
  select(id, wave, origin, origin_imp, origin_imp2)

filterNAorig2 <- filterNAorig2 %>%
  mutate(origin_imp = ifelse(!is.na(origin_imp2), origin_imp2, origin_imp))

liss_job_long$origin_imp <- liss_job_long$origin

for(i in liss_job_long$id) {
  if(i %in% filterNAorig2$id) {liss_job_long$origin_imp[liss_job_long$id == i] <- filterNAorig2$origin_imp[filterNAorig2$id == i]}
}

checkorig2 <- liss_job_long %>%
  select(id, wave, origin, origin_imp)

table(liss_job_long$origin, liss_job_long$origin_imp, useNA = "always")

liss_job_long <- rename(liss_job_long, Cluster_1 = "Cluster#1")
liss_job_long <- rename(liss_job_long, Cluster_2 = "Cluster#2")
liss_job_long <- rename(liss_job_long, Cluster_3 = "Cluster#3")
liss_job_long <- rename(liss_job_long, Cluster = "Cluster#")


# imputation using MICE
impmethod <- character(ncol(liss_job_long))
names(impmethod) <- colnames(liss_job_long)
impmethod["HHincome"] <- "2l.lmer"
impmethod

pm <- make.predictorMatrix(liss_job_long)
pm["anxious",] <- 0
pm["feeldown",] <- 0
pm["feelcalm",] <- 0
pm["depressed",] <- 0
pm["happy",] <- 0
pm["id",] <- 0
pm["job_cat",] <- 0
pm["gender",] <- 0
pm["age",] <- 0
pm["HHstatus",] <- 0
pm["income",] <- 0
pm["anxious",] <- 0
pm["education",] <- 0
pm["origin",] <- 0
pm["wave",] <- 0
pm["year",] <- 0
pm["health",] <- 0
pm["Cluster_1",] <- 0
pm["Cluster_2",] <- 0
pm["Cluster_3",] <- 0
pm["Cluster",] <- 0
pm["education_imp",] <- 0
pm["origin_imp",] <- 0
pm["HHincome", ] <- c(0, 0, 0, 0, 0, -2, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1)
pm

liss_job_long_imp <- mice(liss_job_long, m=5, predictorMatrix = pm,
                          method=impmethod, maxit=10, printFlag = FALSE, seed=1874)

liss_job_long_imp <- complete(liss_job_long_imp, action = "long")

liss_job_long_imp <- liss_job_long_imp %>%
  rename(rid = .id,
         imp = .imp)


# create lagged variables
liss_job_long_imp <- liss_job_long_imp %>%
  group_by(id) %>%
  mutate(., job_cat_lag = lag(job_cat)) %>%
  mutate(., health_lag = lag(health)) %>%
  mutate(., HHstatus_lag = lag(HHstatus)) %>%
  mutate(., income_lag = lag(income)) %>%
  mutate(., HHincome_lag = lag(HHincome)) %>%
  mutate(., Cluster_lag = lag(`Cluster`)) %>%
  mutate(., Cluster1_lag = lag(`Cluster_1`)) %>%
  mutate(., Cluster2_lag = lag(`Cluster_2`)) %>%
  mutate(., Cluster3_lag = lag(`Cluster_3`))

liss_test <- liss_job_long_imp %>%
  select(id, wave, job_cat, job_cat_lag, health, health_lag, HHstatus, HHstatus_lag, income, income_lag, HHincome, HHincome_lag, `Cluster`, `Cluster_1`, `Cluster_2`, `Cluster_3`, Cluster_lag, Cluster1_lag, Cluster2_lag, Cluster3_lag)

liss_test <- liss_job_long_imp %>%
  select(id, wave, `Cluster`, `Cluster_1`, `Cluster_2`, `Cluster_3`, Cluster_lag, Cluster1_lag, Cluster2_lag, Cluster3_lag)

# remove data from 2017 because no lagged data available
liss_job_long_imp <- liss_job_long_imp %>%
  filter(wave != 1)

export(liss_job_long_imp, "liss_job_long_imp_lag.sav")





# Step 3: run step 3 LMM for each time varying variable

# latent states
makeNewSyntax <- function(syntaxName){
  
  newSyntaxToBe <- utils::capture.output(cat(paste("
//LG6.0//
version = 6.0
infile 'C:\Users\fjclouth\Desktop\Tilburg PhD\Projects\4th paper\multiple imputation analysis\liss_job_long_imp_lag.sav'

model
options
   maxthreads=8;
   algorithm 
      tolerance=1e-008 emtolerance=0,01 emiterations=250 nriterations=50;
   startvalues
      seed=0 sets=16 tolerance=1e-005 iterations=50;
   bayes
      categorical=1 variances=1 latent=1 poisson=1;
   montecarlo
      seed=0 sets=0 replicates=500 tolerance=1e-008;
   quadrature  nodes=10;
   missing  excludeall;
   step3 modal ml;
   output      
      parameters=first  betaopts=wl standarderrors=robust profile=posterior write='C:\Users\fjclouth\Desktop\Tilburg PhD\Projects\4th paper\multiple imputation analysis\sim_cluster.csv'
      parametercovariances simulateparameters probmeans=posterior; 
variables
   independent job_cat nominal, age, gender nominal, health nominal, HHstatus nominal, HHincome, education nominal, origin nominal, year nominal;
   caseid id;
   latent 
     Cluster dynamic nominal posterior = ( Cluster#1 Cluster#2 Cluster#3 );
equations
   Cluster[=0] <- 1 + job_cat + age + gender + health + HHstatus + HHincome + education + origin;

   Cluster <- 1 + Cluster[-1] + job_cat + age + gender + health + HHstatus + HHincome + education + origin + year;
end model")))
  utils::write.table(newSyntaxToBe, paste0(syntaxName, ".lgs"), row.names = FALSE, quote = FALSE, col.names = FALSE)
}

LG <- "adjust wd/LatentGOLD6.1/lg61.exe"

makeNewSyntax(syntaxName = "LISS_step3_cluster")

setwd("adjust wd")

shell(paste(LG, "LISS_step3_cluster.lgs", "/b"))


# employement status
makeNewSyntax <- function(syntaxName){
  
  newSyntaxToBe <- utils::capture.output(cat(paste("
//LG6.0//
version = 6.0
infile 'C:\Users\fjclouth\Desktop\Tilburg PhD\Projects\4th paper\multiple imputation analysis\liss_job_long_imp_lag.sav'

model
options
   maxthreads=8;
   algorithm 
      tolerance=1e-008 emtolerance=0,01 emiterations=250 nriterations=50;
   startvalues
      seed=0 sets=16 tolerance=1e-005 iterations=50;
   bayes
      categorical=1 variances=1 latent=1 poisson=1;
   montecarlo
      seed=0 sets=0 replicates=500 tolerance=1e-008;
   quadrature  nodes=10;
   missing  excludeall;
   step3 modal ml;
   output      
      parameters=first  betaopts=wl standarderrors=robust profile=posterior write='C:\Users\fjclouth\Desktop\Tilburg PhD\Projects\4th paper\multiple imputation analysis\sim_job.csv'
      parametercovariances simulateparameters probmeans=posterior;
variables
   independent job_cat_lag nominal, age, gender nominal, health_lag nominal, HHstatus_lag nominal, HHincome_lag, education nominal, origin nominal, year nominal;
   dependent job_cat nominal;
   caseid id;
   latent 
     Cluster nominal posterior = ( Cluster1_lag Cluster2_lag Cluster3_lag );
equations
   job_cat <- 1 + Cluster + job_cat_lag + health_lag + age + gender + HHstatus_lag + HHincome_lag + education + origin + year;
end model")))
  utils::write.table(newSyntaxToBe, paste0(syntaxName, ".lgs"), row.names = FALSE, quote = FALSE, col.names = FALSE)
}

LG <- "adjust wd/LatentGOLD6.1/lg61.exe"

makeNewSyntax(syntaxName = "LISS_step3_job")

setwd("adjust wd")

shell(paste(LG, "LISS_step3_job.lgs", "/b"))



# Household status
makeNewSyntax <- function(syntaxName){
  
  newSyntaxToBe <- utils::capture.output(cat(paste("
//LG6.0//
version = 6.0
infile 'C:\Users\fjclouth\Desktop\Tilburg PhD\Projects\4th paper\multiple imputation analysis\liss_job_long_imp_lag.sav'

model
title HHstatus;
options
   maxthreads=8;
   algorithm 
      tolerance=1e-008 emtolerance=0,01 emiterations=250 nriterations=50;
   startvalues
      seed=0 sets=16 tolerance=1e-005 iterations=50;
   bayes
      categorical=1 variances=1 latent=1 poisson=1;
   montecarlo
      seed=0 sets=0 replicates=500 tolerance=1e-008;
   quadrature  nodes=10;
   missing  excludeall;
   step3 modal ml;
   output      
      parameters=first  betaopts=wl standarderrors=robust profile=posterior write='C:\Users\fjclouth\Desktop\Tilburg PhD\Projects\4th paper\multiple imputation analysis\sim_HHstatus.csv'
      parametercovariances simulateparameters probmeans=posterior;
variables
   independent job_cat_lag nominal, age, gender nominal, health_lag nominal, HHstatus_lag nominal, HHincome_lag, education nominal, origin nominal, year nominal;
   dependent HHstatus nominal;
   caseid id;
   latent 
     Cluster dynamic nominal posterior = ( Cluster1_lag Cluster2_lag Cluster3_lag );
equations
   HHstatus <- 1 + Cluster + job_cat_lag + health_lag + age + gender + HHstatus_lag + HHincome_lag + education + origin + year;
end model")))
  utils::write.table(newSyntaxToBe, paste0(syntaxName, ".lgs"), row.names = FALSE, quote = FALSE, col.names = FALSE)
}

LG <- "adjust wd/LatentGOLD6.1/lg61.exe"

makeNewSyntax(syntaxName = "LISS_step3_HHstatus")

setwd("adjust wd")

shell(paste(LG, "LISS_step3_HHstatus.lgs", "/b"))



# Household income
makeNewSyntax <- function(syntaxName){
  
  newSyntaxToBe <- utils::capture.output(cat(paste("
//LG6.0//
version = 6.0
infile 'C:\Users\fjclouth\Desktop\Tilburg PhD\Projects\4th paper\multiple imputation analysis\liss_job_long_imp_lag.sav'

model
title HHincome;
options
   maxthreads=8;
   algorithm 
      tolerance=1e-008 emtolerance=0,01 emiterations=250 nriterations=50;
   startvalues
      seed=0 sets=16 tolerance=1e-005 iterations=50;
   bayes
      categorical=1 variances=1 latent=1 poisson=1;
   montecarlo
      seed=0 sets=0 replicates=500 tolerance=1e-008;
   quadrature  nodes=10;
   missing  excludeall;
   step3 modal ml;
   output      
      parameters=first  betaopts=wl standarderrors=robust profile=posterior write='C:\Users\fjclouth\Desktop\Tilburg PhD\Projects\4th paper\multiple imputation analysis\sim_HHincome.csv'
      parametercovariances simulateparameters probmeans=posterior;
variables
   independent job_cat_lag nominal, age, gender nominal, health_lag nominal, HHstatus_lag nominal, HHincome_lag, education nominal, origin nominal, year nominal;
   dependent HHincome continuous;
   caseid id;
   latent 
     Cluster dynamic nominal posterior = ( Cluster1_lag Cluster2_lag Cluster3_lag );
equations
   HHincome <- 1 + Cluster + job_cat_lag + health_lag + age + gender + HHstatus_lag + HHincome_lag + education + origin + year;
end model")))
  utils::write.table(newSyntaxToBe, paste0(syntaxName, ".lgs"), row.names = FALSE, quote = FALSE, col.names = FALSE)
}

LG <- "adjust wd/LatentGOLD6.1/lg61.exe"

makeNewSyntax(syntaxName = "LISS_step3_HHincome")

setwd("adjust wd")

shell(paste(LG, "LISS_step3_HHincome.lgs", "/b"))



# health
makeNewSyntax <- function(syntaxName){
  
  newSyntaxToBe <- utils::capture.output(cat(paste("
//LG6.0//
version = 6.0
infile 'C:\Users\fjclouth\Desktop\Tilburg PhD\Projects\4th paper\multiple imputation analysis\liss_job_long_imp_lag.sav'

model
title health;
options
   maxthreads=8;
   algorithm 
      tolerance=1e-008 emtolerance=0,01 emiterations=250 nriterations=50;
   startvalues
      seed=0 sets=16 tolerance=1e-005 iterations=50;
   bayes
      categorical=1 variances=1 latent=1 poisson=1;
   montecarlo
      seed=0 sets=0 replicates=500 tolerance=1e-008;
   quadrature  nodes=10;
   missing  excludeall;
   step3 modal ml;
   output      
      parameters=first  betaopts=wl standarderrors=robust profile=posterior write='C:\Users\fjclouth\Desktop\Tilburg PhD\Projects\4th paper\multiple imputation analysis\sim_health.csv'
      parametercovariances simulateparameters probmeans=posterior;
variables
   independent job_cat_lag nominal, age, gender nominal, health_lag nominal, HHstatus_lag nominal, HHincome_lag, education nominal, origin nominal, year nominal;
   dependent health nominal;
   caseid id;
   latent 
     Cluster dynamic nominal posterior = ( Cluster1_lag Cluster2_lag Cluster3_lag );
equations
   health <- 1 + Cluster + job_cat_lag + health_lag + age + gender + HHstatus_lag + HHincome_lag + education + origin + year;
end model")))
  utils::write.table(newSyntaxToBe, paste0(syntaxName, ".lgs"), row.names = FALSE, quote = FALSE, col.names = FALSE)
}

LG <- "adjust wd/LatentGOLD6.1/lg61.exe"

makeNewSyntax(syntaxName = "LISS_step3_health")

setwd("adjust wd")

shell(paste(LG, "LISS_step3_health.lgs", "/b"))




### Part 3
# run g-formula

# create "scoring data": recode into dummies
liss_score_cov <- liss_job_long_imp_lag %>%
  select(Cluster_lag, job_cat_lag, health_lag, age, gender, HHstatus_lag, HHincome_lag, education, origin, year)

liss_score_init <- liss_job_long_imp_lag %>%
  select(job_cat, age, gender, health, HHstatus, HHincome, education, origin, year)

liss_score_trans <- liss_job_long_imp_lag %>%
  select(Cluster_lag, job_cat, age, gender, health, HHstatus, HHincome, education, origin, year)

n <- 18448

# for all variables scoring the time varying confounders
score_data_cov <- matrix(0, n, 27)
score_data_cov[, 1] <- 1
score_data_cov[, 2][liss_score_cov$Cluster_lag == 2] <- 1
score_data_cov[, 3][liss_score_cov$Cluster_lag == 3] <- 1
score_data_cov[, 4][liss_score_cov$job_cat_lag == "Paid work"] <- 1
score_data_cov[, 5][liss_score_cov$health_lag == 2] <- 1
score_data_cov[, 6][liss_score_cov$health_lag == 3] <- 1
score_data_cov[, 7][liss_score_cov$health_lag == 4] <- 1
score_data_cov[, 8][liss_score_cov$health_lag == 5] <- 1
score_data_cov[, 9] <- liss_score_cov$age
score_data_cov[, 10][liss_score_cov$gender == "Male"] <- 1
score_data_cov[, 11][liss_score_cov$HHstatus_lag == "Married"] <- 1
score_data_cov[, 12][liss_score_cov$HHstatus_lag == "Never been married"] <- 1
score_data_cov[, 13][liss_score_cov$HHstatus_lag == "Separated"] <- 1
score_data_cov[, 14][liss_score_cov$HHstatus_lag == "Widowed"] <- 1
score_data_cov[, 15] <- liss_score_cov$HHincome_lag
score_data_cov[, 16][liss_score_cov$education == "hbo"] <- 1
score_data_cov[, 17][liss_score_cov$education == "mbo"] <- 1
score_data_cov[, 18][liss_score_cov$education == "Primary school"] <- 1
score_data_cov[, 19][liss_score_cov$education == "vmbo"] <- 1
score_data_cov[, 20][liss_score_cov$education == "wo"] <- 1
score_data_cov[, 21][liss_score_cov$origin == "first gen non-West"] <- 1
score_data_cov[, 22][liss_score_cov$origin == "first gen West"] <- 1
score_data_cov[, 23][liss_score_cov$origin == "second gen non-West"] <- 1
score_data_cov[, 24][liss_score_cov$origin == "second gen West"] <- 1
score_data_cov[, 25][liss_score_cov$year == 2019] <- 1
score_data_cov[, 26][liss_score_cov$year == 2020] <- 1
score_data_cov[, 27][liss_score_cov$year == 2021] <- 1

# for all variables scoring the initial state probabilities
score_data_init <- matrix(0, n, 22)
score_data_init[, 1] <- 1
score_data_init[, 2][liss_score_init$job_cat == "Paid work"] <- 1
score_data_init[, 3] <- liss_score_init$age
score_data_init[, 4][liss_score_init$gender == "Male"] <- 1
score_data_init[, 5][liss_score_init$health == 2] <- 1
score_data_init[, 6][liss_score_init$health == 3] <- 1
score_data_init[, 7][liss_score_init$health == 4] <- 1
score_data_init[, 8][liss_score_init$health == 5] <- 1
score_data_init[, 9][liss_score_init$HHstatus == "Married"] <- 1
score_data_init[, 10][liss_score_init$HHstatus == "Never been married"] <- 1
score_data_init[, 11][liss_score_init$HHstatus == "Separated"] <- 1
score_data_init[, 12][liss_score_init$HHstatus == "Widowed"] <- 1
score_data_init[, 13] <- liss_score_init$HHincome
score_data_init[, 14][liss_score_init$education == "hbo"] <- 1
score_data_init[, 15][liss_score_init$education == "mbo"] <- 1
score_data_init[, 16][liss_score_init$education == "Primary school"] <- 1
score_data_init[, 17][liss_score_init$education == "vmbo"] <- 1
score_data_init[, 18][liss_score_init$education == "wo"] <- 1
score_data_init[, 19][liss_score_init$origin == "first gen non-West"] <- 1
score_data_init[, 20][liss_score_init$origin == "first gen West"] <- 1
score_data_init[, 21][liss_score_init$origin == "second gen non-West"] <- 1
score_data_init[, 22][liss_score_init$origin == "second gen West"] <- 1

# for all variables scoring the transition probabilities
score_data_trans <- matrix(0, n, 26)
score_data_trans[, 1] <- 1
score_data_trans[, 2][liss_score_trans$Cluster_lag == 2] <- 1
score_data_trans[, 3][liss_score_trans$Cluster_lag == 3] <- 1
score_data_trans[, 4][liss_score_trans$job_cat == "Paid work"] <- 1
score_data_trans[, 5] <- liss_score_trans$age
score_data_trans[, 6][liss_score_trans$gender == "Male"] <- 1
score_data_trans[, 7][liss_score_trans$health == 2] <- 1
score_data_trans[, 8][liss_score_trans$health == 3] <- 1
score_data_trans[, 9][liss_score_trans$health == 4] <- 1
score_data_trans[, 10][liss_score_trans$health == 5] <- 1
score_data_trans[, 11][liss_score_trans$HHstatus == "Married"] <- 1
score_data_trans[, 12][liss_score_trans$HHstatus == "Never been married"] <- 1
score_data_trans[, 13][liss_score_trans$HHstatus == "Separated"] <- 1
score_data_trans[, 14][liss_score_trans$HHstatus == "Widowed"] <- 1
score_data_trans[, 15] <- liss_score_trans$HHincome
score_data_trans[, 16][liss_score_trans$education == "hbo"] <- 1
score_data_trans[, 17][liss_score_trans$education == "mbo"] <- 1
score_data_trans[, 18][liss_score_trans$education == "Primary school"] <- 1
score_data_trans[, 19][liss_score_trans$education == "vmbo"] <- 1
score_data_trans[, 20][liss_score_trans$education == "wo"] <- 1
score_data_trans[, 21][liss_score_trans$origin == "first gen non-West"] <- 1
score_data_trans[, 22][liss_score_trans$origin == "first gen West"] <- 1
score_data_trans[, 23][liss_score_trans$origin == "second gen non-West"] <- 1
score_data_trans[, 24][liss_score_trans$origin == "second gen West"] <- 1
score_data_trans[, 25][liss_score_trans$year == 2020] <- 1
score_data_trans[, 26][liss_score_trans$year == 2021] <- 1



# rearrange parameter estimates from LG
job_par <- read.csv("sim_job.csv", header = F)
job_pars <- matrix(NA, 500, 27)
inc_par <- read.csv("sim_HHincome.csv", header = F)
inc_pars <- matrix(NA, 500, 27)
health_par <- read.csv("sim_health.csv", header = F)
health_pars <- array(NA, dim = c(500, 4, 27))
HHstatus_par <- read.csv("sim_HHstatus.csv", header = F)
HHstatus_pars <- array(NA, dim = c(500, 4, 27))

step <- seq(4, 1501, 3)
stepk <- append(1, rep(c(1:500), each = 3))

for(i in step) {
  k <- stepk[i]
  
  job_par1 <- as.character(job_par[i, 6:32])
  job_par1 <- strsplit(job_par1, ",")
  job_pars[k, ] <- matrix(as.numeric(job_par1), 1, 27, byrow = T)
  
  inc_par1 <- as.character(inc_par[i, 6:32])
  inc_par1 <- strsplit(inc_par1, ",")
  inc_pars[k, ] <- matrix(as.numeric(inc_par1), 1, 27, byrow = T)
  
  health_par1 <- as.character(health_par[i, 6:113])
  health_par1 <- strsplit(health_par1, ",")
  health_pars[k, , ] <- matrix(as.numeric(health_par1), 4, 27, byrow = F)
  
  HHstatus_par1 <- as.character(HHstatus_par[i, 6:113])
  HHstatus_par1 <- strsplit(HHstatus_par1, ",")
  HHstatus_pars[k, , ] <- matrix(as.numeric(HHstatus_par1), 4, 27, byrow = F)
}

cluster_par <- read.csv("sim_cluster.csv", header = F)

cluster_init_pars <- array(NA, dim = c(500, 2, 22))
cluster_trans_pars <- array(NA, dim = c(500, 2, 26))

for(i in step) {
  k <- stepk[i]
  cluster_init_par1 <- as.character(cluster_par[i, 6:49])
  cluster_init_par1 <- strsplit(cluster_init_par1, ",")
  cluster_init_pars[k, , ] <- matrix(as.numeric(cluster_init_par1), 2, 22, byrow = F)
  cluster_trans_par1 <- as.character(cluster_par[i, 50:101])
  cluster_trans_par1 <- strsplit(cluster_trans_par1, ",")
  cluster_trans_pars[k, , ] <- matrix(as.numeric(cluster_trans_par1), 2, 26, byrow = F)
}


# split data into the four waves
score_data_cov_t1 <- score_data_cov[1:4612, ]
score_data_init_t1 <- score_data_init[1:4612, ]
score_data_trans_t1 <- score_data_trans[1:4612, ]

score_data_cov_t2 <- score_data_cov[4613:9224, ]
score_data_init_t2 <- score_data_init[4613:9224, ]
score_data_trans_t2 <- score_data_trans[4613:9224, ]

score_data_cov_t3 <- score_data_cov[9225:13836, ]
score_data_init_t3 <- score_data_init[9225:13836, ]
score_data_trans_t3 <- score_data_trans[9225:13836, ]

score_data_cov_t4 <- score_data_cov[13837:18448, ]
score_data_init_t4 <- score_data_init[13837:18448, ]
score_data_trans_t4 <- score_data_trans[13837:18448, ]



# function for simulation step in g-formula
score_gformula <- function (condition, n = 4612, t = 4) {
  bs <- as.numeric(condition["bs"])
  mc <- as.numeric(condition["mc"])
  scenario <- condition["scenario"]
  
  
  job_pred <- matrix(NA, n, 1)
  job_prob <- matrix(NA, n, 1)
  
  HHinc_pred <- matrix(NA, n, 1)
  HHinc_exp <- matrix(NA, n, 1)
  HHinc_res <- matrix(NA, n, 1)
  HHinc.res.sd <- matrix(NA, 1, 1)
  
  health_pred <- matrix(NA, n, 1)
  health_prob_1 <- matrix(NA, n, 1)
  health_prob_2 <- matrix(NA, n, 1)
  health_prob_3 <- matrix(NA, n, 1)
  health_prob_4 <- matrix(NA, n, 1)
  health_prob_5 <- matrix(NA, n, 1)
  
  HHstatus_pred <- matrix(NA, n, 1)
  HHstatus_prob_1 <- matrix(NA, n, 1)
  HHstatus_prob_2 <- matrix(NA, n, 1)
  HHstatus_prob_3 <- matrix(NA, n, 1)
  HHstatus_prob_4 <- matrix(NA, n, 1)
  HHstatus_prob_5 <- matrix(NA, n, 1)
  
  cluster_init_pred <- matrix(NA, n, 1)
  cluster_init_prob_1 <- matrix(NA, n, 1)
  cluster_init_prob_2 <- matrix(NA, n, 1)
  cluster_init_prob_3 <- matrix(NA, n, 1)
  
  cluster_trans_pred <- matrix(NA, n, 1)
  cluster_trans_prob_1 <- matrix(NA, n, 1)
  cluster_trans_prob_2 <- matrix(NA, n, 1)
  cluster_trans_prob_3 <- matrix(NA, n, 1)
  
  
  final_preds <- matrix(NA, n*t, 27)
  
  
  HHinc_exp <- score_data_cov_t1 %*% inc_pars[bs, ]
  HHinc_res <- HHinc_exp - score_data_cov_t1[, 15]
  HHinc.res.sd <- sd(HHinc_res)
  HHinc_pred <- HHinc_exp + rnorm(n, 0, HHinc.res.sd)
  
  health_logit_2 <- score_data_cov_t1 %*% health_pars[bs, 1, ]
  health_logit_3 <- score_data_cov_t1 %*% health_pars[bs, 2, ]
  health_logit_4 <- score_data_cov_t1 %*% health_pars[bs, 3, ]
  health_logit_5 <- score_data_cov_t1 %*% health_pars[bs, 4, ]
  health_prob_2 <- exp(health_logit_2) / (1 + exp(health_logit_2) + exp(health_logit_3) + exp(health_logit_4) + exp(health_logit_5))
  health_prob_3 <- exp(health_logit_3) / (1 + exp(health_logit_2) + exp(health_logit_3) + exp(health_logit_4) + exp(health_logit_5))
  health_prob_4 <- exp(health_logit_4) / (1 + exp(health_logit_2) + exp(health_logit_3) + exp(health_logit_4) + exp(health_logit_5))
  health_prob_5 <- exp(health_logit_5) / (1 + exp(health_logit_2) + exp(health_logit_3) + exp(health_logit_4) + exp(health_logit_5))
  health_prob_2[is.na(health_prob_2)] <- 1
  for(i in 1:n) {
    health_prob_2[i, 1][isTRUE(all.equal(health_prob_2[i, 1], 1))] <- 1
  }
  health_prob_3[is.na(health_prob_3)] <- 1
  for(i in 1:n) {
    health_prob_3[i, 1][isTRUE(all.equal(health_prob_3[i, 1], 1))] <- 1
  }
  health_prob_4[is.na(health_prob_4)] <- 1
  for(i in 1:n) {
    health_prob_4[i, 1][isTRUE(all.equal(health_prob_4[i, 1], 1))] <- 1
  }
  health_prob_5[is.na(health_prob_5)] <- 1
  for(i in 1:n) {
    health_prob_5[i, 1][isTRUE(all.equal(health_prob_5[i, 1], 1))] <- 1
  }
  for(i in 1:n) {
    if(health_prob_2[i, 1] == 1) {
      health_prob_1[i, 1] <- health_prob_3[i, 1] <- health_prob_4[i, 1] <- health_prob_5[i, 1] <- 0
    } else if(health_prob_3[i, 1] == 1) {
      health_prob_1[i, 1] <- health_prob_2[i, 1] <- health_prob_4[i, 1] <- health_prob_5[i, 1] <- 0
    } else if(health_prob_4[i, 1] == 1) {
      health_prob_1[i, 1] <- health_prob_2[i, 1] <- health_prob_3[i, 1] <- health_prob_5[i, 1] <- 0
    } else if(health_prob_5[i, 1] == 1) {
      health_prob_1[i, 1] <- health_prob_2[i, 1] <- health_prob_3[i, 1] <- health_prob_4[i, 1] <- 0
    } else {
      health_prob_1[i, 1] <- 1 - (health_prob_2[i, 1] + health_prob_3[i, 1] + health_prob_4[i, 1] + health_prob_5[i, 1])
    }
  }
  
  
  HHstatus_logit_2 <- score_data_cov_t1 %*% HHstatus_pars[bs, 1, ]
  HHstatus_logit_3 <- score_data_cov_t1 %*% HHstatus_pars[bs, 2, ]
  HHstatus_logit_4 <- score_data_cov_t1 %*% HHstatus_pars[bs, 3, ]
  HHstatus_logit_5 <- score_data_cov_t1 %*% HHstatus_pars[bs, 4, ]
  HHstatus_prob_2 <- exp(HHstatus_logit_2) / (1 + exp(HHstatus_logit_2) + exp(HHstatus_logit_3) + exp(HHstatus_logit_4) + exp(HHstatus_logit_5))
  HHstatus_prob_3 <- exp(HHstatus_logit_3) / (1 + exp(HHstatus_logit_2) + exp(HHstatus_logit_3) + exp(HHstatus_logit_4) + exp(HHstatus_logit_5))
  HHstatus_prob_4 <- exp(HHstatus_logit_4) / (1 + exp(HHstatus_logit_2) + exp(HHstatus_logit_3) + exp(HHstatus_logit_4) + exp(HHstatus_logit_5))
  HHstatus_prob_5 <- exp(HHstatus_logit_5) / (1 + exp(HHstatus_logit_2) + exp(HHstatus_logit_3) + exp(HHstatus_logit_4) + exp(HHstatus_logit_5))
  HHstatus_prob_2[is.na(HHstatus_prob_2)] <- 1
  for(i in 1:n) {
    HHstatus_prob_2[i, 1][isTRUE(all.equal(HHstatus_prob_2[i, 1], 1))] <- 1
  }
  HHstatus_prob_3[is.na(HHstatus_prob_3)] <- 1
  for(i in 1:n) {
    HHstatus_prob_3[i, 1][isTRUE(all.equal(HHstatus_prob_3[i, 1], 1))] <- 1
  }
  HHstatus_prob_4[is.na(HHstatus_prob_4)] <- 1
  for(i in 1:n) {
    HHstatus_prob_4[i, 1][isTRUE(all.equal(HHstatus_prob_4[i, 1], 1))] <- 1
  }
  HHstatus_prob_5[is.na(HHstatus_prob_5)] <- 1
  for(i in 1:n) {
    HHstatus_prob_5[i, 1][isTRUE(all.equal(HHstatus_prob_5[i, 1], 1))] <- 1
  }
  for(i in 1:n) {
    if(HHstatus_prob_2[i, 1] == 1) {
      HHstatus_prob_1[i, 1] <- HHstatus_prob_3[i, 1] <- HHstatus_prob_4[i, 1] <- HHstatus_prob_5[i, 1] <- 0
    } else if(HHstatus_prob_3[i, 1] == 1) {
      HHstatus_prob_1[i, 1] <- HHstatus_prob_2[i, 1] <- HHstatus_prob_4[i, 1] <- HHstatus_prob_5[i, 1] <- 0
    } else if(HHstatus_prob_4[i, 1] == 1) {
      HHstatus_prob_1[i, 1] <- HHstatus_prob_2[i, 1] <- HHstatus_prob_3[i, 1] <- HHstatus_prob_5[i, 1] <- 0
    } else if(HHstatus_prob_5[i, 1] == 1) {
      HHstatus_prob_1[i, 1] <- HHstatus_prob_2[i, 1] <- HHstatus_prob_3[i, 1] <- HHstatus_prob_4[i, 1] <- 0
    } else {
      HHstatus_prob_1[i, 1] <- 1 - (HHstatus_prob_2[i, 1] + HHstatus_prob_3[i, 1] + HHstatus_prob_4[i, 1] + HHstatus_prob_5[i, 1])
    }
  }
  
  for(i in 1:n) {
    health_pred[i, 1] <- sample(c(1:5), size = 1, prob = c(health_prob_1[i, 1], health_prob_2[i, 1], health_prob_3[i, 1], health_prob_4[i, 1], health_prob_5[i, 1]), replace = T)
    HHstatus_pred[i, 1] <- sample(c(1:5), size = 1, prob = c(HHstatus_prob_1[i, 1], HHstatus_prob_2[i, 1], HHstatus_prob_3[i, 1], HHstatus_prob_4[i, 1], HHstatus_prob_5[i, 1]), replace = T)
  }
  
  score_data_cov_t1[, 15] <- HHinc_pred
  score_data_cov_t1[, 5][health_pred != 2] <- 0
  score_data_cov_t1[, 5][health_pred == 2] <- 1
  score_data_cov_t1[, 6][health_pred != 3] <- 0
  score_data_cov_t1[, 6][health_pred == 3] <- 1
  score_data_cov_t1[, 7][health_pred != 4] <- 0
  score_data_cov_t1[, 7][health_pred == 4] <- 1
  score_data_cov_t1[, 8][health_pred != 5] <- 0
  score_data_cov_t1[, 8][health_pred == 5] <- 1
  score_data_cov_t1[, 11][HHstatus_pred != 2] <- 0
  score_data_cov_t1[, 11][HHstatus_pred == 2] <- 1
  score_data_cov_t1[, 12][HHstatus_pred != 3] <- 0
  score_data_cov_t1[, 12][HHstatus_pred == 3] <- 1
  score_data_cov_t1[, 13][HHstatus_pred != 4] <- 0
  score_data_cov_t1[, 13][HHstatus_pred == 4] <- 1
  score_data_cov_t1[, 14][HHstatus_pred != 5] <- 0
  score_data_cov_t1[, 14][HHstatus_pred == 5] <- 1
  
  if(scenario == "nc") {
    job_logit <- score_data_cov_t1 %*% job_pars[bs, ]
    job_prob <- exp(job_logit) / (1 + exp(job_logit))
    
    for(i in 1:n) {
      job_pred[i, 1] <- sample(c(1, 0), size = 1, prob = c(job_prob[i, 1], 1 - job_prob[i, 1]), replace = T)
    }
    
    score_data_cov_t1[, 4] <- job_pred
  } else if(scenario == "PO0") {
    score_data_cov_t1[, 4] <- 0
  } else if(scenario == "PO1") {
    score_data_cov_t1[, 4] <- 1
  }
  
  
  score_data_init_t1[, 2] <- score_data_cov_t1[, 4]
  score_data_init_t1[, 5] <- score_data_cov_t1[, 5]
  score_data_init_t1[, 6] <- score_data_cov_t1[, 6]
  score_data_init_t1[, 7] <- score_data_cov_t1[, 7]
  score_data_init_t1[, 8] <- score_data_cov_t1[, 8]
  score_data_init_t1[, 9] <- score_data_cov_t1[, 11]
  score_data_init_t1[, 10] <- score_data_cov_t1[, 12]
  score_data_init_t1[, 11] <- score_data_cov_t1[, 13]
  score_data_init_t1[, 12] <- score_data_cov_t1[, 14]
  score_data_init_t1[, 13] <- score_data_cov_t1[, 15]
  
  cluster_init_logit_2 <- score_data_init_t1 %*% cluster_init_pars[bs, 1, ]
  cluster_init_logit_3 <- score_data_init_t1 %*% cluster_init_pars[bs, 2, ]
  cluster_init_prob_2 <- exp(cluster_init_logit_2) / (1 + exp(cluster_init_logit_2) + exp(cluster_init_logit_3))
  cluster_init_prob_3 <- exp(cluster_init_logit_3) / (1 + exp(cluster_init_logit_2) + exp(cluster_init_logit_3))
  cluster_init_prob_1 <- 1 - (cluster_init_prob_2 + cluster_init_prob_3)
  
  for(i in 1:n) {
    cluster_init_pred[i, 1] <- sample(c(1:3), size = 1, prob = c(cluster_init_prob_1[i, 1], cluster_init_prob_2[i, 1], cluster_init_prob_3[i, 1]), replace = T)
  }

  # t = 2
  
  score_data_cov_t2[, 2][cluster_init_pred != 2] <- 0
  score_data_cov_t2[, 2][cluster_init_pred == 2] <- 1
  score_data_cov_t2[, 3][cluster_init_pred != 3] <- 0
  score_data_cov_t2[, 3][cluster_init_pred == 3] <- 1
  score_data_cov_t2[, 4] <- score_data_cov_t1[, 4]
  score_data_cov_t2[, 5] <- score_data_cov_t1[, 5]
  score_data_cov_t2[, 6] <- score_data_cov_t1[, 6]
  score_data_cov_t2[, 7] <- score_data_cov_t1[, 7]
  score_data_cov_t2[, 8] <- score_data_cov_t1[, 8]
  score_data_cov_t2[, 11] <- score_data_cov_t1[, 11]
  score_data_cov_t2[, 12] <- score_data_cov_t1[, 12]
  score_data_cov_t2[, 13] <- score_data_cov_t1[, 13]
  score_data_cov_t2[, 14] <- score_data_cov_t1[, 14]
  score_data_cov_t2[, 15] <- score_data_cov_t1[, 15]
  
  
  HHinc_exp <- score_data_cov_t2 %*% inc_pars[bs, ]
  HHinc_res <- HHinc_exp - score_data_cov_t2[, 15]
  HHinc.res.sd <- sd(HHinc_res)
  HHinc_pred <- HHinc_exp + rnorm(n, 0, HHinc.res.sd)
  
  health_logit_2 <- score_data_cov_t2 %*% health_pars[bs, 1, ]
  health_logit_3 <- score_data_cov_t2 %*% health_pars[bs, 2, ]
  health_logit_4 <- score_data_cov_t2 %*% health_pars[bs, 3, ]
  health_logit_5 <- score_data_cov_t2 %*% health_pars[bs, 4, ]
  health_prob_2 <- exp(health_logit_2) / (1 + exp(health_logit_2) + exp(health_logit_3) + exp(health_logit_4) + exp(health_logit_5))
  health_prob_3 <- exp(health_logit_3) / (1 + exp(health_logit_2) + exp(health_logit_3) + exp(health_logit_4) + exp(health_logit_5))
  health_prob_4 <- exp(health_logit_4) / (1 + exp(health_logit_2) + exp(health_logit_3) + exp(health_logit_4) + exp(health_logit_5))
  health_prob_5 <- exp(health_logit_5) / (1 + exp(health_logit_2) + exp(health_logit_3) + exp(health_logit_4) + exp(health_logit_5))
  health_prob_2[is.na(health_prob_2)] <- 1
  for(i in 1:n) {
    health_prob_2[i, 1][isTRUE(all.equal(health_prob_2[i, 1], 1))] <- 1
  }
  health_prob_3[is.na(health_prob_3)] <- 1
  for(i in 1:n) {
    health_prob_3[i, 1][isTRUE(all.equal(health_prob_3[i, 1], 1))] <- 1
  }
  health_prob_4[is.na(health_prob_4)] <- 1
  for(i in 1:n) {
    health_prob_4[i, 1][isTRUE(all.equal(health_prob_4[i, 1], 1))] <- 1
  }
  health_prob_5[is.na(health_prob_5)] <- 1
  for(i in 1:n) {
    health_prob_5[i, 1][isTRUE(all.equal(health_prob_5[i, 1], 1))] <- 1
  }
  for(i in 1:n) {
    if(health_prob_2[i, 1] == 1) {
      health_prob_1[i, 1] <- health_prob_3[i, 1] <- health_prob_4[i, 1] <- health_prob_5[i, 1] <- 0
    } else if(health_prob_3[i, 1] == 1) {
      health_prob_1[i, 1] <- health_prob_2[i, 1] <- health_prob_4[i, 1] <- health_prob_5[i, 1] <- 0
    } else if(health_prob_4[i, 1] == 1) {
      health_prob_1[i, 1] <- health_prob_2[i, 1] <- health_prob_3[i, 1] <- health_prob_5[i, 1] <- 0
    } else if(health_prob_5[i, 1] == 1) {
      health_prob_1[i, 1] <- health_prob_2[i, 1] <- health_prob_3[i, 1] <- health_prob_4[i, 1] <- 0
    } else {
      health_prob_1[i, 1] <- 1 - (health_prob_2[i, 1] + health_prob_3[i, 1] + health_prob_4[i, 1] + health_prob_5[i, 1])
    }
  }
  
  HHstatus_logit_2 <- score_data_cov_t2 %*% HHstatus_pars[bs, 1, ]
  HHstatus_logit_3 <- score_data_cov_t2 %*% HHstatus_pars[bs, 2, ]
  HHstatus_logit_4 <- score_data_cov_t2 %*% HHstatus_pars[bs, 3, ]
  HHstatus_logit_5 <- score_data_cov_t2 %*% HHstatus_pars[bs, 4, ]
  HHstatus_prob_2 <- exp(HHstatus_logit_2) / (1 + exp(HHstatus_logit_2) + exp(HHstatus_logit_3) + exp(HHstatus_logit_4) + exp(HHstatus_logit_5))
  HHstatus_prob_3 <- exp(HHstatus_logit_3) / (1 + exp(HHstatus_logit_2) + exp(HHstatus_logit_3) + exp(HHstatus_logit_4) + exp(HHstatus_logit_5))
  HHstatus_prob_4 <- exp(HHstatus_logit_4) / (1 + exp(HHstatus_logit_2) + exp(HHstatus_logit_3) + exp(HHstatus_logit_4) + exp(HHstatus_logit_5))
  HHstatus_prob_5 <- exp(HHstatus_logit_5) / (1 + exp(HHstatus_logit_2) + exp(HHstatus_logit_3) + exp(HHstatus_logit_4) + exp(HHstatus_logit_5))
  HHstatus_prob_2[is.na(HHstatus_prob_2)] <- 1
  for(i in 1:n) {
    HHstatus_prob_2[i, 1][isTRUE(all.equal(HHstatus_prob_2[i, 1], 1))] <- 1
  }
  HHstatus_prob_3[is.na(HHstatus_prob_3)] <- 1
  for(i in 1:n) {
    HHstatus_prob_3[i, 1][isTRUE(all.equal(HHstatus_prob_3[i, 1], 1))] <- 1
  }
  HHstatus_prob_4[is.na(HHstatus_prob_4)] <- 1
  for(i in 1:n) {
    HHstatus_prob_4[i, 1][isTRUE(all.equal(HHstatus_prob_4[i, 1], 1))] <- 1
  }
  HHstatus_prob_5[is.na(HHstatus_prob_5)] <- 1
  for(i in 1:n) {
    HHstatus_prob_5[i, 1][isTRUE(all.equal(HHstatus_prob_5[i, 1], 1))] <- 1
  }
  for(i in 1:n) {
    if(HHstatus_prob_2[i, 1] == 1) {
      HHstatus_prob_1[i, 1] <- HHstatus_prob_3[i, 1] <- HHstatus_prob_4[i, 1] <- HHstatus_prob_5[i, 1] <- 0
    } else if(HHstatus_prob_3[i, 1] == 1) {
      HHstatus_prob_1[i, 1] <- HHstatus_prob_2[i, 1] <- HHstatus_prob_4[i, 1] <- HHstatus_prob_5[i, 1] <- 0
    } else if(HHstatus_prob_4[i, 1] == 1) {
      HHstatus_prob_1[i, 1] <- HHstatus_prob_2[i, 1] <- HHstatus_prob_3[i, 1] <- HHstatus_prob_5[i, 1] <- 0
    } else if(HHstatus_prob_5[i, 1] == 1) {
      HHstatus_prob_1[i, 1] <- HHstatus_prob_2[i, 1] <- HHstatus_prob_3[i, 1] <- HHstatus_prob_4[i, 1] <- 0
    } else {
      HHstatus_prob_1[i, 1] <- 1 - (HHstatus_prob_2[i, 1] + HHstatus_prob_3[i, 1] + HHstatus_prob_4[i, 1] + HHstatus_prob_5[i, 1])
    }
  }
  
  for(i in 1:n) {
    health_pred[i, 1] <- sample(c(1:5), size = 1, prob = c(health_prob_1[i, 1], health_prob_2[i, 1], health_prob_3[i, 1], health_prob_4[i, 1], health_prob_5[i, 1]), replace = T)
    HHstatus_pred[i, 1] <- sample(c(1:5), size = 1, prob = c(HHstatus_prob_1[i, 1], HHstatus_prob_2[i, 1], HHstatus_prob_3[i, 1], HHstatus_prob_4[i, 1], HHstatus_prob_5[i, 1]), replace = T)
  }
  
  score_data_cov_t2[, 15] <- HHinc_pred
  score_data_cov_t2[, 5][health_pred != 2] <- 0
  score_data_cov_t2[, 5][health_pred == 2] <- 1
  score_data_cov_t2[, 6][health_pred != 3] <- 0
  score_data_cov_t2[, 6][health_pred == 3] <- 1
  score_data_cov_t2[, 7][health_pred != 4] <- 0
  score_data_cov_t2[, 7][health_pred == 4] <- 1
  score_data_cov_t2[, 8][health_pred != 5] <- 0
  score_data_cov_t2[, 8][health_pred == 5] <- 1
  score_data_cov_t2[, 11][HHstatus_pred != 2] <- 0
  score_data_cov_t2[, 11][HHstatus_pred == 2] <- 1
  score_data_cov_t2[, 12][HHstatus_pred != 3] <- 0
  score_data_cov_t2[, 12][HHstatus_pred == 3] <- 1
  score_data_cov_t2[, 13][HHstatus_pred != 4] <- 0
  score_data_cov_t2[, 13][HHstatus_pred == 4] <- 1
  score_data_cov_t2[, 14][HHstatus_pred != 5] <- 0
  score_data_cov_t2[, 14][HHstatus_pred == 5] <- 1
  
  
  if(scenario == "nc") {
    job_logit <- score_data_cov_t2 %*% job_pars[bs, ]
    job_prob <- exp(job_logit) / (1 + exp(job_logit))
    
    for(i in 1:n) {
      job_pred[i, 1] <- sample(c(1, 0), size = 1, prob = c(job_prob[i, 1], 1 - job_prob[i, 1]), replace = T)
    }
    
    score_data_cov_t2[, 4] <- job_pred
  } else if(scenario == "PO0") {
    score_data_cov_t2[, 4] <- 0
  } else if(scenario == "PO1") {
    score_data_cov_t2[, 4] <- 1
  }
  
  
  score_data_trans_t2[, 2] <- score_data_cov_t2[, 2] 
  score_data_trans_t2[, 3] <- score_data_cov_t2[, 3]
  score_data_trans_t2[, 4] <- score_data_cov_t2[, 4]
  score_data_trans_t2[, 7] <- score_data_cov_t2[, 5]
  score_data_trans_t2[, 8] <- score_data_cov_t2[, 6]
  score_data_trans_t2[, 9] <- score_data_cov_t2[, 7]
  score_data_trans_t2[, 10] <- score_data_cov_t2[, 8]
  score_data_trans_t2[, 11] <- score_data_cov_t2[, 11]
  score_data_trans_t2[, 12] <- score_data_cov_t2[, 12]
  score_data_trans_t2[, 13] <- score_data_cov_t2[, 13]
  score_data_trans_t2[, 14] <- score_data_cov_t2[, 14]
  score_data_trans_t2[, 15] <- score_data_cov_t2[, 15]
  
  cluster_trans_logit_2 <- score_data_trans_t2 %*% cluster_trans_pars[bs, 1, ]
  cluster_trans_logit_3 <- score_data_trans_t2 %*% cluster_trans_pars[bs, 2, ]
  
  cluster_trans_prob_2 <- exp(cluster_trans_logit_2) / (1 + exp(cluster_trans_logit_2) + exp(cluster_trans_logit_3))
  cluster_trans_prob_2[is.na(cluster_trans_prob_2)] <- 1
  for(i in 1:n) {
    cluster_trans_prob_2[i, 1][isTRUE(all.equal(cluster_trans_prob_2[i, 1], 1))] <- 1
  }
  cluster_trans_prob_3 <- exp(cluster_trans_logit_3) / (1 + exp(cluster_trans_logit_2) + exp(cluster_trans_logit_3))
  cluster_trans_prob_3[is.na(cluster_trans_prob_3)] <- 1
  for(i in 1:n) {
    cluster_trans_prob_3[i, 1][isTRUE(all.equal(cluster_trans_prob_3[i, 1], 1))] <- 1
  }
  cluster_trans_prob_2[cluster_trans_prob_3 == 1] <- 0
  cluster_trans_prob_3[cluster_trans_prob_2 == 1] <- 0
  cluster_trans_prob_1 <- 1 - (cluster_trans_prob_2 + cluster_trans_prob_3)
  
  
  for(i in 1:n) {
    cluster_trans_pred[i, 1] <- sample(c(1:3), size = 1, prob = c(cluster_trans_prob_1[i, 1], cluster_trans_prob_2[i, 1], cluster_trans_prob_3[i, 1]), replace = T)
  }
 
  
  # t = 3
  
  score_data_cov_t3[, 2][cluster_trans_pred != 2] <- 0
  score_data_cov_t3[, 2][cluster_trans_pred == 2] <- 1
  score_data_cov_t3[, 3][cluster_trans_pred != 3] <- 0
  score_data_cov_t3[, 3][cluster_trans_pred == 3] <- 1
  score_data_cov_t3[, 4] <- score_data_cov_t2[, 4]
  score_data_cov_t3[, 5] <- score_data_cov_t2[, 5]
  score_data_cov_t3[, 6] <- score_data_cov_t2[, 6]
  score_data_cov_t3[, 7] <- score_data_cov_t2[, 7]
  score_data_cov_t3[, 8] <- score_data_cov_t2[, 8]
  score_data_cov_t3[, 11] <- score_data_cov_t2[, 11]
  score_data_cov_t3[, 12] <- score_data_cov_t2[, 12]
  score_data_cov_t3[, 13] <- score_data_cov_t2[, 13]
  score_data_cov_t3[, 14] <- score_data_cov_t2[, 14]
  score_data_cov_t3[, 15] <- score_data_cov_t2[, 15]
  
  
  HHinc_exp <- score_data_cov_t3 %*% inc_pars[bs, ]
  HHinc_res <- HHinc_exp - score_data_cov_t3[, 15]
  HHinc.res.sd <- sd(HHinc_res)
  HHinc_pred <- HHinc_exp + rnorm(n, 0, HHinc.res.sd)
  
  health_logit_2 <- score_data_cov_t3 %*% health_pars[bs, 1, ]
  health_logit_3 <- score_data_cov_t3 %*% health_pars[bs, 2, ]
  health_logit_4 <- score_data_cov_t3 %*% health_pars[bs, 3, ]
  health_logit_5 <- score_data_cov_t3 %*% health_pars[bs, 4, ]
  health_prob_2 <- exp(health_logit_2) / (1 + exp(health_logit_2) + exp(health_logit_3) + exp(health_logit_4) + exp(health_logit_5))
  health_prob_3 <- exp(health_logit_3) / (1 + exp(health_logit_2) + exp(health_logit_3) + exp(health_logit_4) + exp(health_logit_5))
  health_prob_4 <- exp(health_logit_4) / (1 + exp(health_logit_2) + exp(health_logit_3) + exp(health_logit_4) + exp(health_logit_5))
  health_prob_5 <- exp(health_logit_5) / (1 + exp(health_logit_2) + exp(health_logit_3) + exp(health_logit_4) + exp(health_logit_5))
  health_prob_2[is.na(health_prob_2)] <- 1
  for(i in 1:n) {
    health_prob_2[i, 1][isTRUE(all.equal(health_prob_2[i, 1], 1))] <- 1
  }
  health_prob_3[is.na(health_prob_3)] <- 1
  for(i in 1:n) {
    health_prob_3[i, 1][isTRUE(all.equal(health_prob_3[i, 1], 1))] <- 1
  }
  health_prob_4[is.na(health_prob_4)] <- 1
  for(i in 1:n) {
    health_prob_4[i, 1][isTRUE(all.equal(health_prob_4[i, 1], 1))] <- 1
  }
  health_prob_5[is.na(health_prob_5)] <- 1
  for(i in 1:n) {
    health_prob_5[i, 1][isTRUE(all.equal(health_prob_5[i, 1], 1))] <- 1
  }
  for(i in 1:n) {
    if(health_prob_2[i, 1] == 1) {
      health_prob_1[i, 1] <- health_prob_3[i, 1] <- health_prob_4[i, 1] <- health_prob_5[i, 1] <- 0
    } else if(health_prob_3[i, 1] == 1) {
      health_prob_1[i, 1] <- health_prob_2[i, 1] <- health_prob_4[i, 1] <- health_prob_5[i, 1] <- 0
    } else if(health_prob_4[i, 1] == 1) {
      health_prob_1[i, 1] <- health_prob_2[i, 1] <- health_prob_3[i, 1] <- health_prob_5[i, 1] <- 0
    } else if(health_prob_5[i, 1] == 1) {
      health_prob_1[i, 1] <- health_prob_2[i, 1] <- health_prob_3[i, 1] <- health_prob_4[i, 1] <- 0
    } else {
      health_prob_1[i, 1] <- 1 - (health_prob_2[i, 1] + health_prob_3[i, 1] + health_prob_4[i, 1] + health_prob_5[i, 1])
    }
  }
  
  HHstatus_logit_2 <- score_data_cov_t3 %*% HHstatus_pars[bs, 1, ]
  HHstatus_logit_3 <- score_data_cov_t3 %*% HHstatus_pars[bs, 2, ]
  HHstatus_logit_4 <- score_data_cov_t3 %*% HHstatus_pars[bs, 3, ]
  HHstatus_logit_5 <- score_data_cov_t3 %*% HHstatus_pars[bs, 4, ]
  HHstatus_prob_2 <- exp(HHstatus_logit_2) / (1 + exp(HHstatus_logit_2) + exp(HHstatus_logit_3) + exp(HHstatus_logit_4) + exp(HHstatus_logit_5))
  HHstatus_prob_3 <- exp(HHstatus_logit_3) / (1 + exp(HHstatus_logit_2) + exp(HHstatus_logit_3) + exp(HHstatus_logit_4) + exp(HHstatus_logit_5))
  HHstatus_prob_4 <- exp(HHstatus_logit_4) / (1 + exp(HHstatus_logit_2) + exp(HHstatus_logit_3) + exp(HHstatus_logit_4) + exp(HHstatus_logit_5))
  HHstatus_prob_5 <- exp(HHstatus_logit_5) / (1 + exp(HHstatus_logit_2) + exp(HHstatus_logit_3) + exp(HHstatus_logit_4) + exp(HHstatus_logit_5))
  HHstatus_prob_2[is.na(HHstatus_prob_2)] <- 1
  for(i in 1:n) {
    HHstatus_prob_2[i, 1][isTRUE(all.equal(HHstatus_prob_2[i, 1], 1))] <- 1
  }
  HHstatus_prob_3[is.na(HHstatus_prob_3)] <- 1
  for(i in 1:n) {
    HHstatus_prob_3[i, 1][isTRUE(all.equal(HHstatus_prob_3[i, 1], 1))] <- 1
  }
  HHstatus_prob_4[is.na(HHstatus_prob_4)] <- 1
  for(i in 1:n) {
    HHstatus_prob_4[i, 1][isTRUE(all.equal(HHstatus_prob_4[i, 1], 1))] <- 1
  }
  HHstatus_prob_5[is.na(HHstatus_prob_5)] <- 1
  for(i in 1:n) {
    HHstatus_prob_5[i, 1][isTRUE(all.equal(HHstatus_prob_5[i, 1], 1))] <- 1
  }
  for(i in 1:n) {
    if(HHstatus_prob_2[i, 1] == 1) {
      HHstatus_prob_1[i, 1] <- HHstatus_prob_3[i, 1] <- HHstatus_prob_4[i, 1] <- HHstatus_prob_5[i, 1] <- 0
    } else if(HHstatus_prob_3[i, 1] == 1) {
      HHstatus_prob_1[i, 1] <- HHstatus_prob_2[i, 1] <- HHstatus_prob_4[i, 1] <- HHstatus_prob_5[i, 1] <- 0
    } else if(HHstatus_prob_4[i, 1] == 1) {
      HHstatus_prob_1[i, 1] <- HHstatus_prob_2[i, 1] <- HHstatus_prob_3[i, 1] <- HHstatus_prob_5[i, 1] <- 0
    } else if(HHstatus_prob_5[i, 1] == 1) {
      HHstatus_prob_1[i, 1] <- HHstatus_prob_2[i, 1] <- HHstatus_prob_3[i, 1] <- HHstatus_prob_4[i, 1] <- 0
    } else {
      HHstatus_prob_1[i, 1] <- 1 - (HHstatus_prob_2[i, 1] + HHstatus_prob_3[i, 1] + HHstatus_prob_4[i, 1] + HHstatus_prob_5[i, 1])
    }
  }
  
  for(i in 1:n) {
    health_pred[i, 1] <- sample(c(1:5), size = 1, prob = c(health_prob_1[i, 1], health_prob_2[i, 1], health_prob_3[i, 1], health_prob_4[i, 1], health_prob_5[i, 1]), replace = T)
    HHstatus_pred[i, 1] <- sample(c(1:5), size = 1, prob = c(HHstatus_prob_1[i, 1], HHstatus_prob_2[i, 1], HHstatus_prob_3[i, 1], HHstatus_prob_4[i, 1], HHstatus_prob_5[i, 1]), replace = T)
  }
  
  score_data_cov_t3[, 15] <- HHinc_pred
  score_data_cov_t3[, 5][health_pred != 2] <- 0
  score_data_cov_t3[, 5][health_pred == 2] <- 1
  score_data_cov_t3[, 6][health_pred != 3] <- 0
  score_data_cov_t3[, 6][health_pred == 3] <- 1
  score_data_cov_t3[, 7][health_pred != 4] <- 0
  score_data_cov_t3[, 7][health_pred == 4] <- 1
  score_data_cov_t3[, 8][health_pred != 5] <- 0
  score_data_cov_t3[, 8][health_pred == 5] <- 1
  score_data_cov_t3[, 11][HHstatus_pred != 2] <- 0
  score_data_cov_t3[, 11][HHstatus_pred == 2] <- 1
  score_data_cov_t3[, 12][HHstatus_pred != 3] <- 0
  score_data_cov_t3[, 12][HHstatus_pred == 3] <- 1
  score_data_cov_t3[, 13][HHstatus_pred != 4] <- 0
  score_data_cov_t3[, 13][HHstatus_pred == 4] <- 1
  score_data_cov_t3[, 14][HHstatus_pred != 5] <- 0
  score_data_cov_t3[, 14][HHstatus_pred == 5] <- 1
  
  
  if(scenario == "nc") {
    job_logit <- score_data_cov_t3 %*% job_pars[bs, ]
    job_prob <- exp(job_logit) / (1 + exp(job_logit))
    
    for(i in 1:n) {
      job_pred[i, 1] <- sample(c(1, 0), size = 1, prob = c(job_prob[i, 1], 1 - job_prob[i, 1]), replace = T)
    }
    
    score_data_cov_t3[, 4] <- job_pred
  } else if(scenario == "PO0") {
    score_data_cov_t3[, 4] <- 0
  } else if(scenario == "PO1") {
    score_data_cov_t3[, 4] <- 1
  }
  
  
  score_data_trans_t3[, 2] <- score_data_cov_t3[, 2] 
  score_data_trans_t3[, 3] <- score_data_cov_t3[, 3]
  score_data_trans_t3[, 4] <- score_data_cov_t3[, 4]
  score_data_trans_t3[, 7] <- score_data_cov_t3[, 5]
  score_data_trans_t3[, 8] <- score_data_cov_t3[, 6]
  score_data_trans_t3[, 9] <- score_data_cov_t3[, 7]
  score_data_trans_t3[, 10] <- score_data_cov_t3[, 8]
  score_data_trans_t3[, 11] <- score_data_cov_t3[, 11]
  score_data_trans_t3[, 12] <- score_data_cov_t3[, 12]
  score_data_trans_t3[, 13] <- score_data_cov_t3[, 13]
  score_data_trans_t3[, 14] <- score_data_cov_t3[, 14]
  score_data_trans_t3[, 15] <- score_data_cov_t3[, 15]
  
  cluster_trans_logit_2 <- score_data_trans_t3 %*% cluster_trans_pars[bs, 1, ]
  cluster_trans_logit_3 <- score_data_trans_t3 %*% cluster_trans_pars[bs, 2, ]
  
  cluster_trans_prob_2 <- exp(cluster_trans_logit_2) / (1 + exp(cluster_trans_logit_2) + exp(cluster_trans_logit_3))
  cluster_trans_prob_2[is.na(cluster_trans_prob_2)] <- 1
  for(i in 1:n) {
    cluster_trans_prob_2[i, 1][isTRUE(all.equal(cluster_trans_prob_2[i, 1], 1))] <- 1
  }
  cluster_trans_prob_3 <- exp(cluster_trans_logit_3) / (1 + exp(cluster_trans_logit_2) + exp(cluster_trans_logit_3))
  cluster_trans_prob_3[is.na(cluster_trans_prob_3)] <- 1
  for(i in 1:n) {
    cluster_trans_prob_3[i, 1][isTRUE(all.equal(cluster_trans_prob_3[i, 1], 1))] <- 1
  }
  cluster_trans_prob_2[cluster_trans_prob_3 == 1] <- 0
  cluster_trans_prob_3[cluster_trans_prob_2 == 1] <- 0
  cluster_trans_prob_1 <- 1 - (cluster_trans_prob_2 + cluster_trans_prob_3)
  
  
  for(i in 1:n) {
    cluster_trans_pred[i, 1] <- sample(c(1:3), size = 1, prob = c(cluster_trans_prob_1[i, 1], cluster_trans_prob_2[i, 1], cluster_trans_prob_3[i, 1]), replace = T)
  }
  

  # t = 4
  
  score_data_cov_t4[, 2][cluster_trans_pred != 2] <- 0
  score_data_cov_t4[, 2][cluster_trans_pred == 2] <- 1
  score_data_cov_t4[, 3][cluster_trans_pred != 3] <- 0
  score_data_cov_t4[, 3][cluster_trans_pred == 3] <- 1
  score_data_cov_t4[, 4] <- score_data_cov_t3[, 4]
  score_data_cov_t4[, 5] <- score_data_cov_t3[, 5]
  score_data_cov_t4[, 6] <- score_data_cov_t3[, 6]
  score_data_cov_t4[, 7] <- score_data_cov_t3[, 7]
  score_data_cov_t4[, 8] <- score_data_cov_t3[, 8]
  score_data_cov_t4[, 11] <- score_data_cov_t3[, 11]
  score_data_cov_t4[, 12] <- score_data_cov_t3[, 12]
  score_data_cov_t4[, 13] <- score_data_cov_t3[, 13]
  score_data_cov_t4[, 14] <- score_data_cov_t3[, 14]
  score_data_cov_t4[, 15] <- score_data_cov_t3[, 15]
  
  
  HHinc_exp <- score_data_cov_t4 %*% inc_pars[bs, ]
  HHinc_res <- HHinc_exp - score_data_cov_t4[, 15]
  HHinc.res.sd <- sd(HHinc_res)
  HHinc_pred <- HHinc_exp + rnorm(n, 0, HHinc.res.sd)
  
  health_logit_2 <- score_data_cov_t4 %*% health_pars[bs, 1, ]
  health_logit_3 <- score_data_cov_t4 %*% health_pars[bs, 2, ]
  health_logit_4 <- score_data_cov_t4 %*% health_pars[bs, 3, ]
  health_logit_5 <- score_data_cov_t4 %*% health_pars[bs, 4, ]
  health_prob_2 <- exp(health_logit_2) / (1 + exp(health_logit_2) + exp(health_logit_3) + exp(health_logit_4) + exp(health_logit_5))
  health_prob_3 <- exp(health_logit_3) / (1 + exp(health_logit_2) + exp(health_logit_3) + exp(health_logit_4) + exp(health_logit_5))
  health_prob_4 <- exp(health_logit_4) / (1 + exp(health_logit_2) + exp(health_logit_3) + exp(health_logit_4) + exp(health_logit_5))
  health_prob_5 <- exp(health_logit_5) / (1 + exp(health_logit_2) + exp(health_logit_3) + exp(health_logit_4) + exp(health_logit_5))
  health_prob_2[is.na(health_prob_2)] <- 1
  for(i in 1:n) {
    health_prob_2[i, 1][isTRUE(all.equal(health_prob_2[i, 1], 1))] <- 1
  }
  health_prob_3[is.na(health_prob_3)] <- 1
  for(i in 1:n) {
    health_prob_3[i, 1][isTRUE(all.equal(health_prob_3[i, 1], 1))] <- 1
  }
  health_prob_4[is.na(health_prob_4)] <- 1
  for(i in 1:n) {
    health_prob_4[i, 1][isTRUE(all.equal(health_prob_4[i, 1], 1))] <- 1
  }
  health_prob_5[is.na(health_prob_5)] <- 1
  for(i in 1:n) {
    health_prob_5[i, 1][isTRUE(all.equal(health_prob_5[i, 1], 1))] <- 1
  }
  for(i in 1:n) {
    if(health_prob_2[i, 1] == 1) {
      health_prob_1[i, 1] <- health_prob_3[i, 1] <- health_prob_4[i, 1] <- health_prob_5[i, 1] <- 0
    } else if(health_prob_3[i, 1] == 1) {
      health_prob_1[i, 1] <- health_prob_2[i, 1] <- health_prob_4[i, 1] <- health_prob_5[i, 1] <- 0
    } else if(health_prob_4[i, 1] == 1) {
      health_prob_1[i, 1] <- health_prob_2[i, 1] <- health_prob_3[i, 1] <- health_prob_5[i, 1] <- 0
    } else if(health_prob_5[i, 1] == 1) {
      health_prob_1[i, 1] <- health_prob_2[i, 1] <- health_prob_3[i, 1] <- health_prob_4[i, 1] <- 0
    } else {
      health_prob_1[i, 1] <- 1 - (health_prob_2[i, 1] + health_prob_3[i, 1] + health_prob_4[i, 1] + health_prob_5[i, 1])
    }
  }
  
  HHstatus_logit_2 <- score_data_cov_t4 %*% HHstatus_pars[bs, 1, ]
  HHstatus_logit_3 <- score_data_cov_t4 %*% HHstatus_pars[bs, 2, ]
  HHstatus_logit_4 <- score_data_cov_t4 %*% HHstatus_pars[bs, 3, ]
  HHstatus_logit_5 <- score_data_cov_t4 %*% HHstatus_pars[bs, 4, ]
  HHstatus_prob_2 <- exp(HHstatus_logit_2) / (1 + exp(HHstatus_logit_2) + exp(HHstatus_logit_3) + exp(HHstatus_logit_4) + exp(HHstatus_logit_5))
  HHstatus_prob_3 <- exp(HHstatus_logit_3) / (1 + exp(HHstatus_logit_2) + exp(HHstatus_logit_3) + exp(HHstatus_logit_4) + exp(HHstatus_logit_5))
  HHstatus_prob_4 <- exp(HHstatus_logit_4) / (1 + exp(HHstatus_logit_2) + exp(HHstatus_logit_3) + exp(HHstatus_logit_4) + exp(HHstatus_logit_5))
  HHstatus_prob_5 <- exp(HHstatus_logit_5) / (1 + exp(HHstatus_logit_2) + exp(HHstatus_logit_3) + exp(HHstatus_logit_4) + exp(HHstatus_logit_5))
  HHstatus_prob_2[is.na(HHstatus_prob_2)] <- 1
  for(i in 1:n) {
    HHstatus_prob_2[i, 1][isTRUE(all.equal(HHstatus_prob_2[i, 1], 1))] <- 1
  }
  HHstatus_prob_3[is.na(HHstatus_prob_3)] <- 1
  for(i in 1:n) {
    HHstatus_prob_3[i, 1][isTRUE(all.equal(HHstatus_prob_3[i, 1], 1))] <- 1
  }
  HHstatus_prob_4[is.na(HHstatus_prob_4)] <- 1
  for(i in 1:n) {
    HHstatus_prob_4[i, 1][isTRUE(all.equal(HHstatus_prob_4[i, 1], 1))] <- 1
  }
  HHstatus_prob_5[is.na(HHstatus_prob_5)] <- 1
  for(i in 1:n) {
    HHstatus_prob_5[i, 1][isTRUE(all.equal(HHstatus_prob_5[i, 1], 1))] <- 1
  }
  for(i in 1:n) {
    if(HHstatus_prob_2[i, 1] == 1) {
      HHstatus_prob_1[i, 1] <- HHstatus_prob_3[i, 1] <- HHstatus_prob_4[i, 1] <- HHstatus_prob_5[i, 1] <- 0
    } else if(HHstatus_prob_3[i, 1] == 1) {
      HHstatus_prob_1[i, 1] <- HHstatus_prob_2[i, 1] <- HHstatus_prob_4[i, 1] <- HHstatus_prob_5[i, 1] <- 0
    } else if(HHstatus_prob_4[i, 1] == 1) {
      HHstatus_prob_1[i, 1] <- HHstatus_prob_2[i, 1] <- HHstatus_prob_3[i, 1] <- HHstatus_prob_5[i, 1] <- 0
    } else if(HHstatus_prob_5[i, 1] == 1) {
      HHstatus_prob_1[i, 1] <- HHstatus_prob_2[i, 1] <- HHstatus_prob_3[i, 1] <- HHstatus_prob_4[i, 1] <- 0
    } else {
      HHstatus_prob_1[i, 1] <- 1 - (HHstatus_prob_2[i, 1] + HHstatus_prob_3[i, 1] + HHstatus_prob_4[i, 1] + HHstatus_prob_5[i, 1])
    }
  }
  
  for(i in 1:n) {
    health_pred[i, 1] <- sample(c(1:5), size = 1, prob = c(health_prob_1[i, 1], health_prob_2[i, 1], health_prob_3[i, 1], health_prob_4[i, 1], health_prob_5[i, 1]), replace = T)
    HHstatus_pred[i, 1] <- sample(c(1:5), size = 1, prob = c(HHstatus_prob_1[i, 1], HHstatus_prob_2[i, 1], HHstatus_prob_3[i, 1], HHstatus_prob_4[i, 1], HHstatus_prob_5[i, 1]), replace = T)
  }
  
  score_data_cov_t4[, 15] <- HHinc_pred
  score_data_cov_t4[, 5][health_pred != 2] <- 0
  score_data_cov_t4[, 5][health_pred == 2] <- 1
  score_data_cov_t4[, 6][health_pred != 3] <- 0
  score_data_cov_t4[, 6][health_pred == 3] <- 1
  score_data_cov_t4[, 7][health_pred != 4] <- 0
  score_data_cov_t4[, 7][health_pred == 4] <- 1
  score_data_cov_t4[, 8][health_pred != 5] <- 0
  score_data_cov_t4[, 8][health_pred == 5] <- 1
  score_data_cov_t4[, 11][HHstatus_pred != 2] <- 0
  score_data_cov_t4[, 11][HHstatus_pred == 2] <- 1
  score_data_cov_t4[, 12][HHstatus_pred != 3] <- 0
  score_data_cov_t4[, 12][HHstatus_pred == 3] <- 1
  score_data_cov_t4[, 13][HHstatus_pred != 4] <- 0
  score_data_cov_t4[, 13][HHstatus_pred == 4] <- 1
  score_data_cov_t4[, 14][HHstatus_pred != 5] <- 0
  score_data_cov_t4[, 14][HHstatus_pred == 5] <- 1
  
  
  if(scenario == "nc") {
    job_logit <- score_data_cov_t4 %*% job_pars[bs, ]
    job_prob <- exp(job_logit) / (1 + exp(job_logit))
    
    for(i in 1:n) {
      job_pred[i, 1] <- sample(c(1, 0), size = 1, prob = c(job_prob[i, 1], 1 - job_prob[i, 1]), replace = T)
    }
    
    score_data_cov_t4[, 4] <- job_pred
  } else if(scenario == "PO0") {
    score_data_cov_t4[, 4] <- 0
  } else if(scenario == "PO1") {
    score_data_cov_t4[, 4] <- 1
  }
  
  
  score_data_trans_t4[, 2] <- score_data_cov_t4[, 2] 
  score_data_trans_t4[, 3] <- score_data_cov_t4[, 3]
  score_data_trans_t4[, 4] <- score_data_cov_t4[, 4]
  score_data_trans_t4[, 7] <- score_data_cov_t4[, 5]
  score_data_trans_t4[, 8] <- score_data_cov_t4[, 6]
  score_data_trans_t4[, 9] <- score_data_cov_t4[, 7]
  score_data_trans_t4[, 10] <- score_data_cov_t4[, 8]
  score_data_trans_t4[, 11] <- score_data_cov_t4[, 11]
  score_data_trans_t4[, 12] <- score_data_cov_t4[, 12]
  score_data_trans_t4[, 13] <- score_data_cov_t4[, 13]
  score_data_trans_t4[, 14] <- score_data_cov_t4[, 14]
  score_data_trans_t4[, 15] <- score_data_cov_t4[, 15]
  
  cluster_trans_logit_2 <- score_data_trans_t4 %*% cluster_trans_pars[bs, 1, ]
  cluster_trans_logit_3 <- score_data_trans_t4 %*% cluster_trans_pars[bs, 2, ]
  
  cluster_trans_prob_2 <- exp(cluster_trans_logit_2) / (1 + exp(cluster_trans_logit_2) + exp(cluster_trans_logit_3))
  cluster_trans_prob_2[is.na(cluster_trans_prob_2)] <- 1
  for(i in 1:n) {
    cluster_trans_prob_2[i, 1][isTRUE(all.equal(cluster_trans_prob_2[i, 1], 1))] <- 1
  }
  cluster_trans_prob_3 <- exp(cluster_trans_logit_3) / (1 + exp(cluster_trans_logit_2) + exp(cluster_trans_logit_3))
  cluster_trans_prob_3[is.na(cluster_trans_prob_3)] <- 1
  for(i in 1:n) {
    cluster_trans_prob_3[i, 1][isTRUE(all.equal(cluster_trans_prob_3[i, 1], 1))] <- 1
  }
  cluster_trans_prob_2[cluster_trans_prob_3 == 1] <- 0
  cluster_trans_prob_3[cluster_trans_prob_2 == 1] <- 0
  cluster_trans_prob_1 <- 1 - (cluster_trans_prob_2 + cluster_trans_prob_3)
  
  
  for(i in 1:n) {
    cluster_trans_pred[i, 1] <- sample(c(1:3), size = 1, prob = c(cluster_trans_prob_1[i, 1], cluster_trans_prob_2[i, 1], cluster_trans_prob_3[i, 1]), replace = T)
  }
  
  
  final_preds[1:4612, 4:27] <- score_data_cov_t1[, 4:27]
  final_preds[1:4612, 2:3] <- score_data_cov_t2[, 2:3]
  final_preds[4613:9224, 4:27] <- score_data_cov_t2[, 4:27]
  final_preds[4613:9224, 2:3] <- score_data_cov_t3[, 2:3]
  final_preds[9225:13836, 4:27] <- score_data_cov_t3[, 4:27]
  final_preds[9225:13836, 2:3] <- score_data_cov_t4[, 2:3]
  final_preds[13837:18448, 4:27] <- score_data_cov_t4[, 4:27]
  final_preds[13837:18448, 2][cluster_trans_pred != 2] <- 0 
  final_preds[13837:18448, 2][cluster_trans_pred == 2] <- 1 
  final_preds[13837:18448, 3][cluster_trans_pred != 3] <- 0 
  final_preds[13837:18448, 3][cluster_trans_pred == 3] <- 1 
  
  
  final_preds[, 1] <- liss_job_long_imp_lag$id
  
  
  pred_data <- data.frame(id = final_preds[, 1])
  
  pred_data$bootid <- bs
  pred_data$mcid <- mc
  pred_data$cluster[final_preds[, 2] == 0 & final_preds[, 3] == 0] <- 1
  pred_data$cluster[final_preds[, 2] == 1] <- 2
  pred_data$cluster[final_preds[, 3] == 1] <- 3
  pred_data$job <- final_preds[, 4]
  pred_data$health[final_preds[, 5] == 0 & final_preds[, 6] == 0 & final_preds[, 7] == 0 & final_preds[, 8] == 0] <- 1
  pred_data$health[final_preds[, 5] == 1] <- 2
  pred_data$health[final_preds[, 6] == 1] <- 3
  pred_data$health[final_preds[, 7] == 1] <- 4
  pred_data$health[final_preds[, 8] == 1] <- 5
  pred_data$age <- final_preds[, 9]
  pred_data$gender[final_preds[, 10] == 0] <- "female"
  pred_data$gender[final_preds[, 10] == 1] <- "male"
  pred_data$HHstatus[final_preds[, 11] == 0 & final_preds[, 12] == 0 & final_preds[, 13] == 0 & final_preds[, 14] == 0] <- "Divorced"
  pred_data$HHstatus[final_preds[, 11] == 1] <- "Married"
  pred_data$HHstatus[final_preds[, 12] == 1] <- "Never been married"
  pred_data$HHstatus[final_preds[, 13] == 1] <- "Separated"
  pred_data$HHstatus[final_preds[, 14] == 1] <- "Widowed"
  pred_data$HHincome <- final_preds[, 15]
  pred_data$education[final_preds[, 16] == 0 & final_preds[, 17] == 0 & final_preds[, 18] == 0 & final_preds[, 19] == 0 & final_preds[, 20] == 0] <- "havo/vwo"
  pred_data$education[final_preds[, 16] == 1] <- "hbo"
  pred_data$education[final_preds[, 17] == 1] <- "mbo"
  pred_data$education[final_preds[, 18] == 1] <- "primary school"
  pred_data$education[final_preds[, 19] == 1] <- "vmbo"
  pred_data$education[final_preds[, 20] == 1] <- "wo"
  pred_data$origin[final_preds[, 21] == 0 & final_preds[, 22] == 0 & final_preds[, 23] == 0 & final_preds[, 24] == 0] <- "Dutch"
  pred_data$origin[final_preds[, 21] == 1] <- "first gen non-West"
  pred_data$origin[final_preds[, 22] == 1] <- "first gen West"
  pred_data$origin[final_preds[, 23] == 1] <- "second gen non-West"
  pred_data$origin[final_preds[, 24] == 1] <- "second gen West"
  pred_data$year[final_preds[, 25] == 0 & final_preds[, 26] == 0 & final_preds[, 27] == 0] <- 2018
  pred_data$year[final_preds[, 25] == 1] <- 2019
  pred_data$year[final_preds[, 26] == 1] <- 2020
  pred_data$year[final_preds[, 27] == 1] <- 2021
  
  
  rio::export(pred_data, paste0(scenario, "_bs", bs, "_mc", mc, ".sav"))
  
}



# run g-formula in parallel using parabar
create_conditions <- function(bses, mces, scenarios) {
  conditions <- apply(expand.grid(
    bs = bses,
    mc = mces,
    scenario = scenarios,
    stringsAsFactors = FALSE
  ), 1, as.list)
}

# Create your conditions.
conditions <- create_conditions(
  bses = c(1:500),
  mces = c(1:100),
  scenarios = c("nc", "PO0", "PO1")
)

# Start backend.
backend <- parabar::start_backend(30)

# What is on the backend.
parabar::peek(backend)

# Export.
parabar::export(backend, c(
  "liss_job_long_imp_lag", 
  "score_data_cov_t1", 
  "score_data_init_t1", 
  "score_data_trans_t1", 
  "score_data_cov_t2", 
  "score_data_init_t2", 
  "score_data_trans_t2", 
  "score_data_cov_t3", 
  "score_data_init_t3", 
  "score_data_trans_t3", 
  "score_data_cov_t4", 
  "score_data_init_t4", 
  "score_data_trans_t4",
  "job_pars",
  "inc_pars",
  "health_pars",
  "HHstatus_pars",
  "cluster_init_pars",
  "cluster_trans_pars"
))

# Peek.
parabar::peek(backend)

# Run task over conditions in parallel.
results <- parabar::par_sapply(backend, x = conditions, fun = score_gformula, n = 4612, t = 4)

# Stop the backend.
parabar::stop_backend(backend)





### Part 4
# summarize results from g-formula

# potential outcome under never exposed
PO0_results <- array(data = NA, dim = c(3, 4, 500, 100))
for(b in 1:500) {
  for(m in 1:100) {
    res <- read_sav(paste0("PO0_bs", b, "_mc", m, ".sav"))
    PO0_results[, , b, m] <- as.matrix(table(res$cluster, res$year))
    print(c(b, m))
  }
}
save(PO0_results, file = "PO0_results.RData")


meanMC_PO0_results <- array(data = NA, dim = c(3, 4, 500))
for(r in 1:3) {
  for(c in 1:4) {
    for(b in 1:500) {
      meanMC_PO0_results[r, c, b] <- mean(PO0_results[r, c, b, ])
      print(b)
    }
  }
}


PO0_prop <- array(data = NA, dim = c(3, 4, 500))
for(b in 1:500) {
  PO0_prop[, , b] <- prop.table(meanMC_PO0_results[, , b], 2)
}


# potential outcome under always exposed
PO1_results <- array(data = NA, dim = c(3, 4, 500, 100))
for(b in 1:500) {
  for(m in 1:100) {
    res <- read_sav(paste0("PO1_bs", b, "_mc", m, ".sav"))
    PO1_results[, , b, m] <- as.matrix(table(res$cluster, res$year))
    print(c(b, m))
  }
}
save(PO1_results, file = "PO1_results.RData")


meanMC_PO1_results <- array(data = NA, dim = c(3, 4, 500))
for(r in 1:3) {
  for(c in 1:4) {
    for(b in 1:500) {
      meanMC_PO1_results[r, c, b] <- mean(PO1_results[r, c, b, ])
      print(b)
    }
  }
}


PO1_prop <- array(data = NA, dim = c(3, 4, 500))
for(b in 1:500) {
  PO1_prop[, , b] <- prop.table(meanMC_PO1_results[, , b], 2)
}

PO1_prop[, , 1]
PO0_prop[, , 1]

ATE_b <- PO1_prop[, , ] - PO0_prop[, , ]
ATE <- matrix(NA, 3, 4)
ATE_.025 <- matrix(NA, 3, 4)
ATE_.975 <- matrix(NA, 3, 4)
M_PO1_prop <- matrix(NA, 3, 4)
M_PO1_prop_.025 <- matrix(NA, 3, 4)
M_PO1_prop_.975 <- matrix(NA, 3, 4)
M_PO0_prop <- matrix(NA, 3, 4)
M_PO0_prop_.025 <- matrix(NA, 3, 4)
M_PO0_prop_.975 <- matrix(NA, 3, 4)
for(r in 1:3) {
  for(c in 1:4) {
    ATE[r, c] <- mean(ATE_b[r, c, ])
    ATE_.025[r, c] <- quantile(ATE_b[r, c, ], .025)
    ATE_.975[r, c] <- quantile(ATE_b[r, c, ], .975)
    M_PO1_prop[r, c] <- mean(PO1_prop[r, c, ])
    M_PO1_prop_.025[r, c] <- quantile(PO1_prop[r, c, ], .025)
    M_PO1_prop_.975[r, c] <- quantile(PO1_prop[r, c, ], .975)
    M_PO0_prop[r, c] <- mean(PO0_prop[r, c, ])
    M_PO0_prop_.025[r, c] <- quantile(PO0_prop[r, c, ], .025)
    M_PO0_prop_.975[r, c] <- quantile(PO0_prop[r, c, ], .975)
  }
}


write.csv(round(M_PO1_prop, 3), file = "M_PO1_prop.csv")
write.csv(round(M_PO1_prop_.025, 3), file = "M_PO1_prop_.025.csv")
write.csv(round(M_PO1_prop_.975, 3), file = "M_PO1_prop_.975.csv")
write.csv(round(M_PO0_prop, 3), file = "M_PO0_prop.csv")
write.csv(round(M_PO0_prop_.025, 3), file = "M_PO0_prop_.025.csv")
write.csv(round(M_PO0_prop_.975, 3), file = "M_PO0_prop_.975.csv")
write.csv(round(ATE, 3), file = "M_ATE.csv")
write.csv(round(ATE_.025, 3), file = "M_ATE_.025.csv")
write.csv(round(ATE_.975, 3), file = "M_ATE_.975.csv")


# natural course scenario
nc_results <- array(data = NA, dim = c(3, 4, 500, 100))
nc_job <- array(data = NA, dim = c(2, 4, 500, 100))
nc_health <- array(data = NA, dim = c(5, 4, 500, 100))
nc_HH <- array(data = NA, dim = c(5, 4, 500, 100))
nc_inc <- array(data = NA, dim = c(12, 1, 500, 100))
for(b in 1:500) {
  for(m in 1:100) {
    res <- read_sav(paste0("nc_bs", b, "_mc", m, ".sav"))
    nc_results[, , b, m] <- as.matrix(table(res$cluster, res$year))
    nc_job[, , b, m] <- as.matrix(table(res$job, res$year))
    nc_health[, , b, m] <- as.matrix(table(res$health, res$year))
    nc_HH[, , b, m] <- as.matrix(table(res$HHstatus, res$year))
    a <- res %>%
      group_by(year) %>%
      summarise(quantile(HHincome, probs = c(.25, .5, .75), na.rm = T), sd(HHincome, na.rm = T))
    nc_inc[, 1, b, m] <- matrix(unlist(a[2]), 12, 1)
    print(c(b, m))
  }
}
save(nc_results, file = "nc_results.RData")
save(nc_job, file = "nc_job.RData")
save(nc_health, file = "nc_health.RData")
save(nc_HH, file = "nc_HH.RData")
save(nc_inc, file = "nc_inc.RData")


load("nc_results.RData")
meanMC_nc_results <- array(data = NA, dim = c(3, 4, 500))
for(r in 1:3) {
  for(c in 1:4) {
    for(b in 1:500) {
      meanMC_nc_results[r, c, b] <- mean(nc_results[r, c, b, ])
      print(b)
    }
  }
}


nc_prop <- array(data = NA, dim = c(3, 4, 500))
for(b in 1:500) {
  nc_prop[, , b] <- prop.table(meanMC_nc_results[, , b], 2)
}


NC <- matrix(NA, 3, 4)
NC_.025 <- matrix(NA, 3, 4)
NC_.975 <- matrix(NA, 3, 4)
for(r in 1:3) {
  for(c in 1:4) {
    NC[r, c] <- mean(nc_prop[r, c, ])
    NC_.025[r, c] <- quantile(nc_prop[r, c, ], .025)
    NC_.975[r, c] <- quantile(nc_prop[r, c, ], .975)
  }
}

write.csv(NC, file = "NC_cluster.csv")
write.csv(NC_.025, file = "NC_.025_cluster.csv")
write.csv(NC_.975, file = "NC_.975_cluster.csv")

load("nc_job.RData")
meanMC_nc_job <- array(data = NA, dim = c(2, 4, 500))
for(r in 1:2) {
  for(c in 1:4) {
    for(b in 1:500) {
      meanMC_nc_job[r, c, b] <- mean(nc_job[r, c, b, ])
      print(b)
    }
  }
}

nc_prop_job <- array(data = NA, dim = c(2, 4, 500))
for(b in 1:500) {
  nc_prop_job[, , b] <- prop.table(meanMC_nc_job[, , b], 2)
}

NCj <- matrix(NA, 2, 4)
NCj_.025 <- matrix(NA, 2, 4)
NCj_.975 <- matrix(NA, 2, 4)
for(r in 1:2) {
  for(c in 1:4) {
    NCj[r, c] <- mean(nc_prop_job[r, c, ])
    NCj_.025[r, c] <- quantile(nc_prop_job[r, c, ], .025)
    NCj_.975[r, c] <- quantile(nc_prop_job[r, c, ], .975)
  }
}

write.csv(round(NCj, 3), file = "NC_job.csv")
write.csv(round(NCj_.025, 3), file = "NC_.025_job.csv")
write.csv(round(NCj_.975, 3), file = "NC_.975_job.csv")

load("nc_health.RData")
meanMC_nc_health <- array(data = NA, dim = c(5, 4, 500))
for(r in 1:5) {
  for(c in 1:4) {
    for(b in 1:500) {
      meanMC_nc_health[r, c, b] <- mean(nc_health[r, c, b, ])
      print(b)
    }
  }
}

nc_prop_health <- array(data = NA, dim = c(5, 4, 500))
for(b in 1:500) {
  nc_prop_health[, , b] <- prop.table(meanMC_nc_health[, , b], 2)
}

NChealth <- matrix(NA, 5, 4)
NChealth_.025 <- matrix(NA, 5, 4)
NChealth_.975 <- matrix(NA, 5, 4)
for(r in 1:5) {
  for(c in 1:4) {
    NChealth[r, c] <- mean(nc_prop_health[r, c, ])
    NChealth_.025[r, c] <- quantile(nc_prop_health[r, c, ], .025)
    NChealth_.975[r, c] <- quantile(nc_prop_health[r, c, ], .975)
  }
}

write.csv(round(NChealth, 3), file = "NC_health.csv")
write.csv(round(NChealth_.025, 3), file = "NC_.025_health.csv")
write.csv(round(NChealth_.975, 3), file = "NC_.975_health.csv")

load("nc_HH.RData")
meanMC_nc_HH <- array(data = NA, dim = c(5, 4, 500))
for(r in 1:5) {
  for(c in 1:4) {
    for(b in 1:500) {
      meanMC_nc_HH[r, c, b] <- mean(nc_HH[r, c, b, ])
      print(b)
    }
  }
}

nc_prop_HH <- array(data = NA, dim = c(5, 4, 500))
for(b in 1:500) {
  nc_prop_HH[, , b] <- prop.table(meanMC_nc_HH[, , b], 2)
}

NCHH <- matrix(NA, 5, 4)
NCHH_.025 <- matrix(NA, 5, 4)
NCHH_.975 <- matrix(NA, 5, 4)
for(r in 1:5) {
  for(c in 1:4) {
    NCHH[r, c] <- mean(nc_prop_HH[r, c, ])
    NCHH_.025[r, c] <- quantile(nc_prop_HH[r, c, ], .025)
    NCHH_.975[r, c] <- quantile(nc_prop_HH[r, c, ], .975)
  }
}

write.csv(round(NCHH, 3), file = "NC_HH.csv")
write.csv(round(NCHH_.025, 3), file = "NC_.025_HH.csv")
write.csv(round(NCHH_.975, 3), file = "NC_.975_HH.csv")

load("nc_inc.RData")
meanMC_nc_inc <- array(data = NA, dim = c(12, 1, 500))
for(r in 1:12) {
  for(c in 1:1) {
    for(b in 1:500) {
      meanMC_nc_inc[r, c, b] <- mean(nc_inc[r, c, b, ])
      print(b)
    }
  }
}

NCinc <- matrix(NA, 12, 1)
NCinc_.025 <- matrix(NA, 12, 1)
NCinc_.975 <- matrix(NA, 12, 1)
for(r in 1:12) {
  for(c in 1:1) {
    NCinc[r, c] <- mean(meanMC_nc_inc[r, c, ])
    NCinc_.025[r, c] <- quantile(meanMC_nc_inc[r, c, ], .025)
    NCinc_.975[r, c] <- quantile(meanMC_nc_inc[r, c, ], .975)
  }
}

write.csv(round(NCinc, 0), file = "NC_inc.csv")
write.csv(round(NCinc_.025, 0), file = "NC_.025_inc.csv")
write.csv(round(NCinc_.975, 0), file = "NC_.975_inc.csv")


