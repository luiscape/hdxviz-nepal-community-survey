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
library(Hmisc)
library(lubridate)

#
#  Load data form orignal CSVs.
#
round1 <- read.csv('data/preprocessing/round1.csv', na.strings='n/a')
round2 <- read.csv('data/preprocessing/round2.csv', na.strings='n/a')
round3 <- read.csv('data/preprocessing/round3.csv')
round4 <- read.csv('data/preprocessing/round4.csv')

#
#  Adding round label.
#
round1$Round <- 1
round2$Round <- 2
round3$Round <- 3
round4$Round <- 4

#
#  Helper function. Helps identify
#  what columns match and what are
#  missing.
#
makeAssessment <- function () {
  as.numeric(summary(names(round1) %in% names(round2))[[3]]) / length(names(round1))
  as.numeric(summary(names(round2) %in% names(round3))[[3]]) / length(names(round2))
  as.numeric(summary(names(round3) %in% names(round4))[[3]]) / length(names(round3))
  
  name_analysis <- data.frame(table_names = names(round1), in_round2 = names(round1) %in% names(round2))
  name_analysis2 <- data.frame(table_names = names(round2), in_round3 = names(round2) %in% names(round3))
  name_analysis3 <- data.frame(table_names = names(round3), in_round4 = names(round3) %in% names(round4))
}

#
#  Merge all results into a
#  single data.frame.
#
mergeAll <- function() {
  intermediate <- rbind(round1, round2)
  intermediate <- rbind(intermediate, round3)
  
  select_names <- data.frame(table_names = names(round4), intermediate = names(round4) %in% names(intermediate))
  select_columns <- filter(select_names, intermediate == TRUE)$table_names
  
  #
  #  Selecting only relevant columns
  #  from rounds 1 to 3.
  #
  select_intermediate <- select(intermediate, 
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
  
  results <- rbind(select_intermediate, select_round4)
  return(results)
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
  df$Occupation <- ifelse(df$Occupation == 'farmer_laborer', 'Farmer / Laborer', df$Occupation)
  df$Occupation <- ifelse(df$Occupation == 'government_service__i_e__teach', 'Government Services (i.e. Teacher)', df$Occupation)
  df$Occupation <- ifelse(df$Occupation == 'skilled_worker__i_e__carpenter', 'Skilled Worker (i.e. Carpenter)', df$Occupation)
  df$Occupation <- ifelse(df$Occupation == 'skilled_worker', 'Skilled Worker (i.e. Carpenter)', df$Occupation)
  df$Occupation <- ifelse(df$Occupation == 'government_ser', 'Government Servant', df$Occupation)
  df$Occupation <- ifelse(df$Occupation == 'other', 'Other', df$Occupation)
  
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
  df$A0JS <- ifelse(df$A0JS == '1___not_at_all', 'Not at all', df$A0JS)
  df$A0JS <- ifelse(df$A0JS == '2___very_little', 'Very little', df$A0JS)
  df$A0JS <- ifelse(df$A0JS == '3___neutral', 'Neutral', df$A0JS)
  df$A0JS <- ifelse(df$A0JS == '4___mostly_yes', 'Mostly yes', df$A0JS)
  df$A0JS <- ifelse(df$A0JS == '5___completely_yes', 'Completely yes', df$A0JS)
  df$A0JS <- ifelse(df$A0JS == 'don_t_know', "Don't know", df$A0JS)
  df$A0JS <- ifelse(df$A0JS == 'refused', "Refused", df$A0JS)
  
  
  #
  #  Cleaning A1JS
  #
  df$A1JS <- as.character(df$A1JS)
  for (i in 1:nrow(df)) {
    entry = df$A1JS[i]
    df$A1JS[i] <- capitalize(gsub("_", " ", entry))
    df$A1JS[i] <- gsub("  ", " / ", df$A1JS[i])
  }
  
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
  df$B0JS <- ifelse(df$B0JS == '1___not_at_all', 'Not at all', df$B0JS)
  df$B0JS <- ifelse(df$B0JS == '2___very_little', 'Very little', df$B0JS)
  df$B0JS <- ifelse(df$B0JS == '3___neutral', 'Neutral', df$B0JS)
  df$B0JS <- ifelse(df$B0JS == '4___mostly_yes', 'Mostly yes', df$B0JS)
  df$B0JS <- ifelse(df$B0JS == '5___completely_yes', 'Completely yes', df$B0JS)
  df$B0JS <- ifelse(df$B0JS == 'don_t_know', "Don't know", df$B0JS)
  df$A0JS <- ifelse(df$B0JS == 'refused', "Refused", df$B0JS)
  
  #
  #  Cleaning C0JS
  #
  df$C0JS <- as.character(df$C0JS)
  df$C0JS <- ifelse(df$C0JS == '1___not_at_all', 'Not at all', df$C0JS)
  df$C0JS <- ifelse(df$C0JS == '2___very_little', 'Very little', df$C0JS)
  df$C0JS <- ifelse(df$C0JS == '3___neutral', 'Neutral', df$C0JS)
  df$C0JS <- ifelse(df$C0JS == '4___mostly_yes', 'Mostly yes', df$C0JS)
  df$C0JS <- ifelse(df$C0JS == '5___completely_yes', 'Completely yes', df$C0JS)
  df$C0JS <- ifelse(df$C0JS == 'don_t_know', "Don't know", df$C0JS)
  df$A0JS <- ifelse(df$C0JS == 'refused', "Refused", df$C0JS)
  
  #
  #  Cleaning C1JS
  #
  df$C1JS <- as.character(df$C1JS)
  for (i in 1:nrow(df)) {
    entry = df$C1JS[i]
    df$C1JS[i] <- capitalize(gsub("_", " ", entry))
    df$C1JS[i] <- gsub("  ", " / ", df$C1JS[i])
  }
  
  #
  #  Cleaning C2JS
  #
#   df$C1JS <- as.character(df$C2JS)
#   for (i in 1:nrow(df)) {
#     entry = df$C2JS[i]
#     df$C2JS[i] <- capitalize(gsub("_", " ", entry))
#     df$C2JS[i] <- gsub("  ", " / ", df$C2JS[i])
#   }
  
  #
  #  Cleaning D0JS
  #
  df$D0JS <- as.character(df$D0JS)
  df$D0JS <- ifelse(df$D0JS == '1___not_at_all', 'Not at all', df$D0JS)
  df$D0JS <- ifelse(df$D0JS == '2___very_little', 'Very little', df$D0JS)
  df$D0JS <- ifelse(df$D0JS == '3___neutral', 'Neutral', df$D0JS)
  df$D0JS <- ifelse(df$D0JS == '4___mostly_yes', 'Mostly yes', df$D0JS)
  df$D0JS <- ifelse(df$D0JS == '5___completely_yes', 'Completely yes', df$D0JS)
  df$D0JS <- ifelse(df$D0JS == 'don_t_know', "Don't know", df$D0JS)
  df$A0JS <- ifelse(df$D0JS == 'refused', "Refused", df$D0JS)
  
  #
  #  Cleaning E0JS
  #
  df$E0JS <- as.character(df$E0JS)
  df$E0JS <- ifelse(df$E0JS == '1___not_at_all', 'Not at all', df$E0JS)
  df$E0JS <- ifelse(df$E0JS == '2___very_little', 'Very little', df$E0JS)
  df$E0JS <- ifelse(df$E0JS == '3___neutral', 'Neutral', df$E0JS)
  df$E0JS <- ifelse(df$E0JS == '4___mostly_yes', 'Mostly yes', df$E0JS)
  df$E0JS <- ifelse(df$E0JS == '5___completely_yes', 'Completely yes', df$E0JS)
  df$E0JS <- ifelse(df$E0JS == 'don_t_know', "Don't know", df$E0JS)
  
  
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
  total_records = nrow(round1) + nrow(round2) + nrow(round3) + nrow(round4)
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
 