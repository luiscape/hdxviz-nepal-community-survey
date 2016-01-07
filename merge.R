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

#
#  Load data form orignal CSVs.
#
round1 <- read.csv('data/preprocessing/round1.csv', na.strings='n/a')
round2 <- read.csv('data/preprocessing/round2.csv', na.strings='n/a')
round3 <- read.csv('data/preprocessing/round3.csv')
round4 <- read.csv('data/preprocessing/round4.csv')

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
  select_data <- intermediate[select_columns, ]
  
  if (as.numeric(summary(names(select_data) %in% names(intermediate))[[3]]) / length(names(select_data)) == 0) {
    intermediate_result <- rbind(intermediate, select_data)
    select_names <- data.frame(table_names = names(select_data), intermediate = names(intermediate_result) %in% names(intermediate_result))
    select_columns <- filter(select_names, intermediate == TRUE)$table_names
    final_result <- intermediate_result
    return(final_result)
  } else {
    stop('Checking for the same column names on the latest survey failed.')
  }
}

write.csv(mergeAll(), 'data/all_rounds_merged.csv', row.names=FALSE)
