######
#
# Create a single file containing all student data and also masks student names/IDs
#
######

library(stringr)
library(dplyr)
library(anonymizer)

setwd("~/Dropbox/PER/AHS Phys 2018-19/Analysis/FCI_Analysis_2018-2019")

pre_data = read.csv("FCI Pre.csv", header = TRUE)
post1_data = read.csv("FCI Post1.csv", header = TRUE)
lawson_data = read.csv("Lawson Test.csv", header = TRUE)

# inner join, only can compare if all data avaliable
# should be no NAs
pre_post1 = merge(pre_data,post1_data, by = "Username", all = FALSE)
#head(pre_post1)
#summary(pre_post1)

summary(pre_post1["X11.x"])
summary(pre_post1["X11.y"])

# columns to drop
drops <- c("Timestamp.x","Timestamp.y","Total.score.x","Total.score.y","Last.Name.x","Last.Name.y","First.Name.x","First.Name.y","Period.y","Gender.y")

pre_post1 = pre_post1[ , !(names(pre_post1) %in% drops)]
#summary(pre_post1)

# create student data aliases
pre_post1_anon <- pre_post1
pre_post1_anon[,"Username"] <- anonymize(pre_post1[,"Username"])
#tail(pre_post1_anon)

write.csv(pre_post1_anon, file = "pre_post1.csv")

## all scores in one file
all_data = merge(pre_post1,lawson_data, by = "Username", all = FALSE)

# create student data aliases
all_data_anon <- all_data
all_data_anon[,"Username"] <- anonymize(all_data_anon[,"Username"])
#head(all_data_anon)


write.csv(all_data_anon, file = "all_data.csv")
