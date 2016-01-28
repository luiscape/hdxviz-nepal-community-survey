# --------------------------------
#
#  CLEAN:
#  -----
#
#  Cleaning variable values to
#  make them ready for analysis.
#
# --------------------------------

library(Hmisc)
library(dplyr)
library(lubridate)

data <- read.csv('data/all_rounds_merged.csv')
data$Date <- as.character(data$Date)

#
#  Cleaning dates.
#
data$Date <- ifelse(is.na(format(dmy(data$Date), '%Y-%m-%d')) == FALSE, format(dmy(data$Date), '%Y-%m-%d'), data$Date)
data$Date <- as.factor(data$Date)

#
#  Cleaning ward names.
#
class(data$Ward)
data <- filter(data, Ward > 0)

#
#  Capitalizing unique entries.
#
data$District <- as.factor(capitalize(as.character(data$District)))
data$Ethnicity <- as.factor(capitalize(as.character(data$Ethnicity)))
data$Gender <- as.factor(capitalize(as.character(data$Gender)))
data$Agency <- as.factor(capitalize(as.character(data$Agency)))

#
#  Writting results.
#
write.csv(data, 'data/visualization_data.csv', na="", row.names=FALSE)
