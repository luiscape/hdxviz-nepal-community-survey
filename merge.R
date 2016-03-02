# ------------------------------------
#
#  MERGING:
#  -------
#
#  Merging Nepal community survey data.
#
#
#  Author: Luis Capelo <capelo@un.org>
# ------------------------------------

library(dplyr)
library(readr)
library(Hmisc)
library(lubridate)

#
#  Load data form orignal CSVs.
#
round1 <- read.csv('data/preprocessing/round1.csv', na.strings='n/a')
round2 <- read.csv('data/preprocessing/round2.csv', na.strings='n/a')
round3 <- read_csv('data/preprocessing/round3.csv')
round4 <- read_csv('data/preprocessing/round4.csv')
round5 <- read_csv('data/preprocessing/round5.csv')
round6 <- read_csv('data/preprocessing/round6.csv', locale = locale(encoding='latin1'))

#
#  Adding round label.
#
round1$Round <- 1
round2$Round <- 2
round3$Round <- 3
round4$Round <- 4
round5$Round <- 5
round6$Round <- 6

#
#  Helper function. Helps identify
#  what columns match and what are
#  missing.
#
makeAssessment <- function () {
  
  #
  #  Share of questions that remain the same
  #  since the first questionnaire.
  #
  shares = c(
    1 - as.numeric(summary(names(round1) %in% names(round2))[[3]]) / length(names(round1)),
    1 - as.numeric(summary(names(round1) %in% names(round3))[[3]]) / length(names(round2)),
    1 - as.numeric(summary(names(round1) %in% names(round4))[[3]]) / length(names(round3)),
    1 - as.numeric(summary(names(round1) %in% names(round5))[[3]]) / length(names(round5)),
    1 - as.numeric(summary(names(round1) %in% names(round6))[[3]]) / length(names(round6))
  )
  cat(
    paste0('Approximate share of questions that remain the same throughtout the 6 rounds: ', mean(shares), '\n')
    )
  
  #
  #  Identify which variables are missing.
  #
  name_analysis <- data.frame(table_names = names(round1), in_round2 = names(round1) %in% names(round2))
  name_analysis2 <- data.frame(table_names = names(round2), in_round3 = names(round2) %in% names(round3))
  name_analysis3 <- data.frame(table_names = names(round3), in_round4 = names(round3) %in% names(round4))
  name_analysis4 <- data.frame(table_names = names(round4), in_round5 = names(round4) %in% names(round5))
  name_analysis5 <- data.frame(table_names = names(round5), in_round6 = names(round5) %in% names(round6))
}

#
#  Merge all results into a
#  single data.frame.
#
mergeAll <- function() {
  
  #
  #  Merge first 3 rounds.
  #
  intermediate <- rbind(round1, round2)
  rounds1_3 <- rbind(intermediate, round3)
  
  #
  #  Selecting only relevant columns
  #  from rounds 1 to 3.
  #
  select_round1_3 <- select(rounds1_3, 
                          Date, District, VDC_Municipality, Ward,
                          Age, Gender, Ethnicity, Ethnicity_Other,
                          Occupation, Occupation_Other, Do_you_have_any_health_problem,
                          A0JS, A1JS, A2JS, A3JS, B0JS, C0JS, C1JS, C2JS,
                          D0JS, E0JS, Round)
  
  #
  #  Selecting only relevant columns
  #  from round 4.
  #
  round4$Date <- NA
  select_round4 <- select(round4, 
                                Date, District, VDC_Municipality, Ward,
                                Age, Gender, Ethnicity, Ethnicity_Other,
                                Occupation, Occupation_Other, Do_you_have_any_health_problem,
                                A0JS, A1JS, A2JS, A3JS, B0JS, C0JS, C1JS, C2JS,
                                D0JS, E0JS, Round)
  
  
  #
  #  Selecting relevant columns from round 5.
  #
  round5$Date <- NA
  select_round5 <- select(round5, 
                          Date, District, VDC_Municipality, Ward,
                          Age, Gender, Ethnicity, Ethnicity_Other,
                          Occupation, Occupation_Other, Do_you_have_any_health_problem,
                          A0JS, A1JS, A2JS, A3JS, B0JS, C0JS, C1JS, C2JS,
                          D0JS, E0JS, Round)
  
  #
  #  Selecting relevant columns from round 6.
  #
  round6$Date <- NA
  select_round6 <- select(round6, 
                          Date, District, VDC_Municipality, Ward,
                          Age, Gender, Ethnicity, Ethnicity_Other,
                          Occupation, Occupation_Other, Do_you_have_any_health_problem,
                          A0JS, A1JS, A2JS, A3JS, B0JS, C0JS, C1JS, C2JS,
                          D0JS, E0JS, Round)
  
  
  #
  #  Combining all results.
  #
  rounds1_4 <- rbind(select_round1_3, select_round4)
  rounds1_5 <- rbind(rounds1_4, select_round5)
  rounds1_6 <- rbind(rounds1_5, select_round6)
  return(rounds1_6)
}

#
# Clean the dataset and select variables.
#
cleanAndSelect <- function(df) {
  df <- select(df, 
               Round, Date, Gender, District, 
               Ward, Age, Ethnicity, Ethnicity_Other, 
               Occupation, Occupation_Other, Do_you_have_any_health_problem, 
               A0JS, A1JS, B0JS, C0JS, C1JS, D0JS, E0JS)
  
  df$District <- capitalize(as.character(df$District))
  df$Ethnicity <- capitalize(as.character(df$Ethnicity))
  
  #
  #  Cleaning occupation names.
  #
  df$Occupation <- as.character(df$Occupation)
  df$Occupation <- ifelse(df$Occupation == 'ngo_worker_business', 'NGO Worker / Business', df$Occupation)
  df$Occupation <- ifelse(df$Occupation == 'ngo_worker_bus', 'NGO Worker / Business', df$Occupation)
  df$Occupation <- ifelse(df$Occupation == 'NGO worker/Business', 'NGO Worker / Business', df$Occupation)
  df$Occupation <- ifelse(df$Occupation == 'farmer_laborer', 'Farmer / Laborer', df$Occupation)
  df$Occupation <- ifelse(df$Occupation == 'Farmer/laborer', 'Farmer / Laborer', df$Occupation)
  df$Occupation <- ifelse(df$Occupation == 'government_service__i_e__teach', 'Government Services (i.e. Teacher)', df$Occupation)
  df$Occupation <- ifelse(df$Occupation == 'skilled_worker__i_e__carpenter', 'Skilled Worker (i.e. Carpenter)', df$Occupation)
  df$Occupation <- ifelse(df$Occupation == 'skilled_worker', 'Skilled Worker (i.e. Carpenter)', df$Occupation)
  df$Occupation <- ifelse(df$Occupation == 'Skilled worker (i.e. carpenter)', 'Skilled Worker (i.e. Carpenter)', df$Occupation)
  df$Occupation <- ifelse(df$Occupation == 'government_ser', 'Government Servant', df$Occupation)
  df$Occupation <- ifelse(df$Occupation == 'Government Services (i.e. Teacher)', 'Government Servant', df$Occupation)
  df$Occupation <- ifelse(df$Occupation == 'Government Servant', 'Government (i.e. teacher, health worker, army)', df$Occupation)
  df$Occupation <- ifelse(df$Occupation == 'other', 'Other', df$Occupation)
  df$Occupation <- ifelse(df$Occupation == 'Others', 'Other', df$Occupation)

  #
  #  Cleaning Age.
  #
  df$Age <- as.character(df$Age)
  df$Age <- ifelse(df$Age == '15_24', '15 to 24', df$Age)
  df$Age <- ifelse(df$Age == '25_39', '25 to 39', df$Age)
  df$Age <- ifelse(df$Age == '40_54', '40 to 54', df$Age)
  df$Age <- ifelse(df$Age == '55', '55+', df$Age)
  df$Age <- ifelse(df$Age == '55_greater', '55+', df$Age)
  df$Age <- ifelse(df$Age == '25-39', '25 to 39', df$Age)
  df$Age <- ifelse(df$Age == '40-54', '40 to 54', df$Age)
  df$Age <- ifelse(df$Age == 'don_t_know', "Don't know", df$Age)
  df$Age <- ifelse(df$Age == 'refused', 'Refused', df$Age)
  
  #
  #  Cleaning 'Do you have any health problems' entries.
  #
  df$Do_you_have_any_health_problem <- as.character(df$Do_you_have_any_health_problem)
  df$Do_you_have_any_health_problem <- ifelse(df$Do_you_have_any_health_problem == 'no_difficulty', 'No difficulty.', df$Do_you_have_any_health_problem)
  df$Do_you_have_any_health_problem <- ifelse(df$Do_you_have_any_health_problem == 'yes__some_diff', 'Yes, some difficulty.', df$Do_you_have_any_health_problem)
  df$Do_you_have_any_health_problem <- ifelse(df$Do_you_have_any_health_problem == 'yes__a_lot_of_', 'Yes, a lot of difficulty.', df$Do_you_have_any_health_problem)
  df$Do_you_have_any_health_problem <- ifelse(df$Do_you_have_any_health_problem == 'cannot_do_at_a', 'Cannot do at all.', df$Do_you_have_any_health_problem)
  
  #
  #  Cleaning A0JS
  #
  df$A0JS <- as.character(df$A0JS)
  df$A0JS <- ifelse(df$A0JS == '1___not_at_all', '1 Not at all', df$A0JS)
  df$A0JS <- ifelse(df$A0JS == '1 _ Not at all', '1 Not at all', df$A0JS)
  df$A0JS <- ifelse(df$A0JS == '1_not_at_all', '1 Not at all', df$A0JS)
  
  df$A0JS <- ifelse(df$A0JS == '2___very_little', '2 Very little', df$A0JS)
  df$A0JS <- ifelse(df$A0JS == '2 _ Not very much', '2 Very little', df$A0JS)
  df$A0JS <- ifelse(df$A0JS == '2_not_very_much', '2 Very little', df$A0JS)
  
  df$A0JS <- ifelse(df$A0JS == '3___neutral', '3 Neutral', df$A0JS)
  df$A0JS <- ifelse(df$A0JS == '3 _ Neutral', '3 Neutral', df$A0JS)
  df$A0JS <- ifelse(df$A0JS == '3_neutral', '3 Neutral', df$A0JS)
  
  df$A0JS <- ifelse(df$A0JS == '4___mostly_yes', '4 Mostly yes', df$A0JS)
  df$A0JS <- ifelse(df$A0JS == '4_ Mostly yes', '4 Mostly yes', df$A0JS)
  df$A0JS <- ifelse(df$A0JS == '4_somewhat_yes', '4 Mostly yes', df$A0JS)
  
  df$A0JS <- ifelse(df$A0JS == '5___completely_yes', '5 Completely yes', df$A0JS)
  df$A0JS <- ifelse(df$A0JS == '5 _ Completely yes', '5 Completely yes', df$A0JS)
  df$A0JS <- ifelse(df$A0JS == '5_completely_yes', '5 Completely yes', df$A0JS)
  
  df$A0JS <- ifelse(df$A0JS == 'don_t_know', "6 Don't know", df$A0JS)
  df$A0JS <- ifelse(df$A0JS == 'Dont know', "6 Don't know", df$A0JS)
  df$A0JS <- ifelse(df$A0JS == "Don\u0090t know", "6 Don't know", df$A0JS)
  
  df$A0JS <- ifelse(df$A0JS == 'refused', "7 Refused", df$A0JS)
  df$A0JS <- ifelse(df$A0JS == 'Refused', "7 Refused", df$A0JS)
  
  
  
  #
  #  Cleaning A1JS
  #
  df$A1JS <- as.character(df$A1JS)
  for (i in 1:nrow(df)) {
    entry = df$A1JS[i]
    df$A1JS[i] <- capitalize(gsub("_", " ", entry))
    df$A1JS[i] <- gsub("  ", " / ", df$A1JS[i])
  }
  df$A1JS <- ifelse(df$A1JS == 'Short term she', "Short-term shelter (tent / shelterbox)", df$A1JS)
  df$A1JS <- ifelse(df$A1JS == 'Short term shelter / tent shelt', "Short-term shelter (tent / shelterbox)", df$A1JS)
  df$A1JS <- ifelse(df$A1JS == 'Short term shelter (tent/shelterbox)', "Short-term shelter (tent / shelterbox)", df$A1JS)
  df$A1JS <- ifelse(df$A1JS == 'Short-term shelter (tent/shelterbox)', "Short-term shelter (tent / shelterbox)", df$A1JS)
  
  df$A1JS <- ifelse(df$A1JS == 'Toilets sanitation', "Toilets / sanitation", df$A1JS)
  df$A1JS <- ifelse(df$A1JS == 'Toilets/sanitation', "Toilets / sanitation", df$A1JS)
  
  df$A1JS <- ifelse(df$A1JS == 'Long term shelter (housing)', "Long term shelter / housing", df$A1JS)
  df$A1JS <- ifelse(df$A1JS == 'Long-term shelter (housing)', "Long term shelter / housing", df$A1JS)
  
  df$A1JS <- ifelse(df$A1JS == 'Other', "Others", df$A1JS)
  
  
  #
  #  Cleaning A2JS
  #
#   df$A2JS <- as.character(df$A2JS)
#   for (i in 1:nrow(df)) {
#     entry = df$A2JS[i]
#     df$A2JS[i] <- capitalize(gsub("_", " ", entry))
#     df$A2JS[i] <- gsub("  ", " / ",df$A2JS[i])
#   }
  
  #
  #  Cleaning A3JS
  #
#   df$A3JS <- as.character(df$A3JS)
#   for (i in 1:nrow(df)) {
#     entry = df$A3JS[i]
#     df$A3JS[i] <- capitalize(gsub("_", " ", entry))
#     df$A3JS[i] <- gsub("  ", " / ", df$A3JS[i])
#   }
  
  #
  #  Cleaning B0JS
  #
  df$B0JS <- as.character(df$B0JS)
  df$B0JS <- ifelse(df$B0JS == '1___not_at_all', '1 Not at all', df$B0JS)
  df$B0JS <- ifelse(df$B0JS == '1 _ Not at all', '1 Not at all', df$B0JS)
  df$B0JS <- ifelse(df$B0JS == '1_not_at_all', '1 Not at all', df$B0JS)
  
  df$B0JS <- ifelse(df$B0JS == '2___very_little', '2 Very little', df$B0JS)
  df$B0JS <- ifelse(df$B0JS == '2 _ Not very much', '2 Very little', df$B0JS)
  df$B0JS <- ifelse(df$B0JS == '2_not_very_much', '2 Very little', df$B0JS)
  
  df$B0JS <- ifelse(df$B0JS == '3___neutral', '3 Neutral', df$B0JS)
  df$B0JS <- ifelse(df$B0JS == '3 _ Neutral', '3 Neutral', df$B0JS)
  df$B0JS <- ifelse(df$B0JS == '3_neutral', '3 Neutral', df$B0JS)
  
  df$B0JS <- ifelse(df$B0JS == '4_Mostly yes', '4 Mostly yes', df$B0JS)
  df$B0JS <- ifelse(df$B0JS == '4___mostly_yes', '4 Mostly yes', df$B0JS)
  df$B0JS <- ifelse(df$B0JS == '4_ Mostly yes', '4 Mostly yes', df$B0JS)
  df$B0JS <- ifelse(df$B0JS == '4_somewhat_yes', '4 Mostly yes', df$B0JS)
  
  df$B0JS <- ifelse(df$B0JS == '5___completely_yes', '5 Completely yes', df$B0JS)
  df$B0JS <- ifelse(df$B0JS == '5 _ Completely yes', '5 Completely yes', df$B0JS)
  df$B0JS <- ifelse(df$B0JS == '5_completely_yes', '5 Completely yes', df$B0JS)
   
  df$B0JS <- ifelse(df$B0JS == 'don_t_know', "6 Don't know", df$B0JS)
  df$B0JS <- ifelse(df$B0JS == 'Dont know', "6 Don't know", df$B0JS)
  df$B0JS <- ifelse(df$B0JS == "Don\u0090t know", "6 Don't know", df$B0JS)
  
  df$B0JS <- ifelse(df$B0JS == 'refused', "7 Refused", df$B0JS)
  df$B0JS <- ifelse(df$B0JS == 'Refused', "7 Refused", df$B0JS)
  
  #
  #  Cleaning C0JS
  #
  df$C0JS <- as.character(df$C0JS)
  df$C0JS <- ifelse(df$C0JS == '1___not_at_all', '1 Not at all', df$C0JS)
  df$C0JS <- ifelse(df$C0JS == '1 _ Not at all', '1 Not at all', df$C0JS)
  df$C0JS <- ifelse(df$C0JS == '1_not_at_all', '1 Not at all', df$C0JS)
  
  df$C0JS <- ifelse(df$C0JS == '2___very_little', '2 Very little', df$C0JS)
  df$C0JS <- ifelse(df$C0JS == '2 _ Not very much', '2 Very little', df$C0JS)
  df$C0JS <- ifelse(df$C0JS == '2_not_very_much', '2 Very little', df$C0JS)
  
  df$C0JS <- ifelse(df$C0JS == '3___neutral', '3 Neutral', df$C0JS)
  df$C0JS <- ifelse(df$C0JS == '3 _ Neutral', '3 Neutral', df$C0JS)
  df$C0JS <- ifelse(df$C0JS == '3_neutral', '3 Neutral', df$C0JS)
  
  df$C0JS <- ifelse(df$C0JS == '4_Mostly yes', '4 Mostly yes', df$C0JS)
  df$C0JS <- ifelse(df$C0JS == '4___mostly_yes', '4 Mostly yes', df$C0JS)
  df$C0JS <- ifelse(df$C0JS == '4_ Mostly yes', '4 Mostly yes', df$C0JS)
  df$C0JS <- ifelse(df$C0JS == '4_somewhat_yes', '4 Mostly yes', df$C0JS)
  
  df$C0JS <- ifelse(df$C0JS == '5___completely_yes', '5 Completely yes', df$C0JS)
  df$C0JS <- ifelse(df$C0JS == '5 _ Completely yes', '5 Completely yes', df$C0JS)
  df$C0JS <- ifelse(df$C0JS == '5_completely_yes', '5 Completely yes', df$C0JS)
  
  df$C0JS <- ifelse(df$C0JS == 'don_t_know', "6 Don't know", df$C0JS)
  df$C0JS <- ifelse(df$C0JS == 'Dont know', "6 Don't know", df$C0JS)
  df$C0JS <- ifelse(df$C0JS == "Don\u0090t know", "6 Don't know", df$C0JS)
  
  df$C0JS <- ifelse(df$C0JS == 'refused', "7 Refused", df$C0JS)
  df$C0JS <- ifelse(df$C0JS == 'Refused', "7 Refused", df$C0JS)
  
  #
  #  Cleaning C1JS
  #
  df$C1JS <- as.character(df$C1JS)
  for (i in 1:nrow(df)) {
    entry = df$C1JS[i]
    df$C1JS[i] <- capitalize(gsub("_", " ", entry))
    df$C1JS[i] <- gsub("  ", " / ", df$C1JS[i])
  }
  df$C1JS <- ifelse(df$C1JS == 'How to get healthcare psycholo', 'How to get healthcare psychological support', df$C1JS)
  df$C1JS <- ifelse(df$C1JS == 'How to get healthcare psychological support', 'How to get healthcare psychosocial support', df$C1JS)
  df$C1JS <- ifelse(df$C1JS == 'How to get healthcare/psychological support', 'How to get healthcare psychosocial support', df$C1JS)
  
  df$C1JS <- ifelse(df$C1JS == 'How to register for access sup', 'How to register for access support', df$C1JS)
  df$C1JS <- ifelse(df$C1JS == 'How to register for access support', 'How to register for access support', df$C1JS)
  df$C1JS <- ifelse(df$C1JS == 'How to register for/access support', 'How to register for access support', df$C1JS)
  
  df$C1JS <- ifelse(df$C1JS == 'How to replace personal docume', 'How to replace personal documentation', df$C1JS)
  
  df$C1JS <- ifelse(df$C1JS == 'News about government decision', 'News about government decisions', df$C1JS)
  df$C1JS <- ifelse(df$C1JS == 'Other', 'Others', df$C1JS)
  
  #
  #  Cleaning D0JS
  #
  df$D0JS <- as.character(df$D0JS)
  df$D0JS <- ifelse(df$D0JS == '1___not_at_all', '1 Not at all', df$D0JS)
  df$D0JS <- ifelse(df$D0JS == '1 _ Not at all', '1 Not at all', df$D0JS)
  df$D0JS <- ifelse(df$D0JS == '1_not_at_all', '1 Not at all', df$D0JS)
  
  df$D0JS <- ifelse(df$D0JS == '2___very_little', '2 Very little', df$D0JS)
  df$D0JS <- ifelse(df$D0JS == '2 _ Not very much', '2 Very little', df$D0JS)
  df$D0JS <- ifelse(df$D0JS == '2_not_very_much', '2 Very little', df$D0JS)
  
  df$D0JS <- ifelse(df$D0JS == '3___neutral', '3 Neutral', df$D0JS)
  df$D0JS <- ifelse(df$D0JS == '3 _ Neutral', '3 Neutral', df$D0JS)
  df$D0JS <- ifelse(df$D0JS == '3_neutral', '3 Neutral', df$D0JS)
  
  df$D0JS <- ifelse(df$D0JS == '4_Mostly yes', '4 Mostly yes', df$D0JS)
  df$D0JS <- ifelse(df$D0JS == '4___mostly_yes', '4 Mostly yes', df$D0JS)
  df$D0JS <- ifelse(df$D0JS == '4_ Mostly yes', '4 Mostly yes', df$D0JS)
  df$D0JS <- ifelse(df$D0JS == '4_somewhat_yes', '4 Mostly yes', df$D0JS)
  
  df$D0JS <- ifelse(df$D0JS == '5___completely_yes', '5 Completely yes', df$D0JS)
  df$D0JS <- ifelse(df$D0JS == '5 _ Completely yes', '5 Completely yes', df$D0JS)
  df$D0JS <- ifelse(df$D0JS == '5_completely_yes', '5 Completely yes', df$D0JS)
  
  df$D0JS <- ifelse(df$D0JS == 'don_t_know', "6 Don't know", df$D0JS)
  df$D0JS <- ifelse(df$D0JS == 'Dont know', "6 Don't know", df$D0JS)
  df$D0JS <- ifelse(df$D0JS == "Don\u0090t know", "6 Don't know", df$D0JS)
  
  df$D0JS <- ifelse(df$D0JS == 'refused', "7 Refused", df$D0JS)
  df$D0JS <- ifelse(df$D0JS == 'Refused', "7 Refused", df$D0JS)
  
  
  #
  #  Cleaning E0JS
  #
  df$E0JS <- as.character(df$E0JS)
  df$E0JS <- ifelse(df$E0JS == '1___not_at_all', '1 Not at all', df$E0JS)
  df$E0JS <- ifelse(df$E0JS == '1 _ Not at all', '1 Not at all', df$E0JS)
  df$E0JS <- ifelse(df$E0JS == '1_not_at_all', '1 Not at all', df$E0JS)
  
  df$E0JS <- ifelse(df$E0JS == '2___very_little', '2 Very little', df$E0JS)
  df$E0JS <- ifelse(df$E0JS == '2 _ Not very much', '2 Very little', df$E0JS)
  df$E0JS <- ifelse(df$E0JS == '2_not_very_much', '2 Very little', df$E0JS)
  
  df$E0JS <- ifelse(df$E0JS == '3___neutral', '3 Neutral', df$E0JS)
  df$E0JS <- ifelse(df$E0JS == '3 _ Neutral', '3 Neutral', df$E0JS)
  df$E0JS <- ifelse(df$E0JS == '3_neutral', '3 Neutral', df$E0JS)
  
  df$E0JS <- ifelse(df$E0JS == '4_Mostly yes', '4 Mostly yes', df$E0JS)
  df$E0JS <- ifelse(df$E0JS == '4___mostly_yes', '4 Mostly yes', df$E0JS)
  df$E0JS <- ifelse(df$E0JS == '4_ Mostly yes', '4 Mostly yes', df$E0JS)
  df$E0JS <- ifelse(df$E0JS == '4_somewhat_yes', '4 Mostly yes', df$E0JS)
  
  df$E0JS <- ifelse(df$E0JS == '5___completely_yes', '5 Completely yes', df$E0JS)
  df$E0JS <- ifelse(df$E0JS == '5 _ Completely yes', '5 Completely yes', df$E0JS)
  df$E0JS <- ifelse(df$E0JS == '5_completely_yes', '5 Completely yes', df$E0JS)
  
  df$E0JS <- ifelse(df$E0JS == 'don_t_know', "6 Don't know", df$E0JS)
  df$E0JS <- ifelse(df$E0JS == 'Dont know', "6 Don't know", df$E0JS)
  df$E0JS <- ifelse(df$E0JS == "Don\u0090t know", "6 Don't know", df$E0JS)
  
  df$E0JS <- ifelse(df$E0JS == 'refused', "7 Refused", df$E0JS)
  df$E0JS <- ifelse(df$E0JS == 'Refused', "7 Refused", df$E0JS)
  
  
  
  #
  #  Cleaning date.
  #
  df$Date <- as.character(df$Date)
  df$Date <- ifelse(is.na(ymd(df$Date)) == FALSE, format(ymd(df$Date), '%Y-%m-%d'), as.character(df$Date))
  df$Date <- ifelse(is.na(dmy(df$Date)) == FALSE, format(dmy(df$Date), '%Y-%m-%d'), as.character(df$Date))
  
  df$Month <- month(ymd(df$Date))
  
  
  #
  #  Gender.
  #
  df$Gender <- capitalize(as.character(df$Gender))
  
  return(df)
}

makeCheck <- function(df) {
  total_records = nrow(round1) + nrow(round2) + nrow(round3) + nrow(round4) + nrow(round5) + nrow(round6)
  if (nrow(df) != total_records) {
    print(paste('Total records:', total_records))
    print(paste('Records processed: ', nrow(df)))
    stop('Records do not match. Something is out of place.')
  } else {
    print('Check PASSED!')
  }
}

d <- cleanAndSelect(mergeAll())
makeCheck(d)

write.csv(d, 'data/all_rounds_merged.csv', row.names=FALSE)
 