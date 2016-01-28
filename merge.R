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
  
  
  if (as.numeric(summary(names(select_data) %in% names(intermediate))[[3]]) / length(names(select_data)) == 0) {
    results <- rbind(select_intermediate, select_round4)
    return(results)
  } else {
    stop('Checking for the same column names on the latest survey failed.')
  }
}

#
# Clean the dataset and select variables.
#
cleanAndSelect <- function(df) {
  df <- select(df, Round, Date, District, Ward, Age, Ethnicity, Ethnicity_Other, Occupation, Occupation_Other, Do_you_have_any_health_problem, A0JS, A1JS, B0JS, C0JS, C1JS, D0JS, E0JS)
  
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
  
  df$Do_you_have_any_health_problem <- as.character(df$Do_you_have_any_health_problem)
  df$Do_you_have_any_health_problem <- ifelse(df$Do_you_have_any_health_problem == 'no_difficulty', 'No difficulty.', df$Do_you_have_any_health_problem)
  df$Do_you_have_any_health_problem <- ifelse(df$Do_you_have_any_health_problem == 'yes__some_diff', 'Yes, some difficulty.', df$Do_you_have_any_health_problem)
  df$Do_you_have_any_health_problem <- ifelse(df$Do_you_have_any_health_problem == 'yes__a_lot_of_', 'Yes, a lot of difficulty.', df$Do_you_have_any_health_problem)
  df$Do_you_have_any_health_problem <- ifelse(df$Do_you_have_any_health_problem == 'cannot_do_at_a', 'Cannot do at all.', df$Do_you_have_any_health_problem)
  
  df$Date <- as.character(df$Date)
  df$Date <- ifelse(is.na(ymd(df$Date)) == FALSE, format(ymd(df$Date), '%Y-%m-%d'), as.character(df$Date))
  df$Date <- ifelse(is.na(dmy(df$Date)) == FALSE, format(dmy(df$Date), '%Y-%m-%d'), as.character(df$Date))
  
  df$Month <- month(ymd(df$Date))
  
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
 