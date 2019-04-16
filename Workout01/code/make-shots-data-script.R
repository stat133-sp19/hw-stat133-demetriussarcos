# Title: Preparing and Manipulating Shots Data
# Description: Imports all 5 .csv files and combines them into a global table for data visualization. Also performs
#              data manipulation to make more descriptive values and add some new variables through recycling
# Input(s): andre-igoudala.csv
#         draymond-green.csv
#         kevin-durant.csv
#         klay-thompson.csv
#         stephen-curry.csv
# Output(s): A global table of all containing all 5 players statistics, shots-data.csv,
#         and several summary statistics as text files for the global table and the individual imported files
# Author: Demetrius Sarcos
# Date: 03-13-2019
#=============================================================================================================

#Reading the Data into R
iguodala <- read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)
green <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
durant <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
thompson <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)
curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)

# adding "name" column to individual data frames 
iguodala["name"] <- "Andre Igoudala"
green["name"] <- "Draymond Green"
durant["name"] <- "Kevin Durant"
thompson["name"] <- "Klay Thompson"
curry["name"] <- "Stephen Curry"

# Manipulating shot_made_flag values to be more descriptive
iguodala$shot_made_flag <- ifelse(iguodala$shot_made_flag == "n", "shot_no", "shot_yes")
green$shot_made_flag <- ifelse(green$shot_made_flag == "n", "shot_no", "shot_yes")
durant$shot_made_flag <- ifelse(durant$shot_made_flag == "n", "shot_no", "shot_yes")
thompson$shot_made_flag <- ifelse(thompson$shot_made_flag == "n", "shot_no", "shot_yes")
curry$shot_made_flag <- ifelse(curry$shot_made_flag == "n", "shot_no", "shot_yes")

# Mutating Data Frames: Adding "minute" column
iguodala["minute"] <- (iguodala$period * 12) - iguodala$minutes_remaining
green["minute"] <- (green$period * 12) - green$minutes_remaining
durant["minute"] <- (durant$period * 12) - durant$minutes_remaining
thompson["minute"] <- (thompson$period * 12) - thompson$minutes_remaining
curry["minute"] <- (curry$period * 12) - curry$minutes_remaining

# Exporting Summaries of Imported Data Frames
sink(file = "../output/andre-iguodala-summary.txt")
summary(iguodala)
sink()

sink(file = "../output/draymond-green-summary.txt")
summary(green)
sink()

sink(file = "../output/kevin-durant-summary.txt")
summary(durant)
sink()

sink(file = "../output/klay-thompson-summary.txt")
summary(thompson)
sink()

sink(file = "../output/stephen-curry-summary.txt")
summary(curry)
sink()

# Stacking the Data into a Single Data Frame
gsw <- rbind(curry, durant, green, iguodala, thompson)

# Exporting "GSW" and its Summary Statistics
write.csv(gsw, file = "../data/shots-data.csv", row.names = FALSE)

sink(file = "../output/shots-data-summary.txt")
summary(gsw)
sink()