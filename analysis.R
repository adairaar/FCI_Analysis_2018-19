######################
#
# Investigate what variables best explain the data
# The period of class should be the best explainer of results if the teaching in
# G period (the experimental group) proved to be better.
# Also will consider mitigating factors: race, education of parents, and
# scientific reasoning (measured by the Lawson test).
#
######################


library(stringr)
library(dplyr)
library(stats)

df <- read.csv("all_data_scored.csv", header = TRUE)

# to revise to make cleaner and see scaling effect of parental education
model <- lm(g_ind~lawson_result+Period.x+Race+Highest.level.of.education.of.parent.guardian, data = df)
summary(model)
### as is, Period G had the biggest explanatory value and only one that passed statistical significance (p < 0.05)
### also, need to combine E and F periods into one, as both part of control group

df$Highest.level.of.education.of.parent.guardian = factor(df$Highest.level.of.education.of.parent.guardian, levels = c("High School Graduate/GED","Some college","Bachelor's (4-year) degree","Graduate school--no degree earned","Graduate school--Masters","Graduate school--PhD"))

model <- glm(g_ind~lawson_result+Period.x+Race+Highest.level.of.education.of.parent.guardian, data = df)
summary(model)

df$Race = factor(df$Race, levels = c("White/Caucasian","Black/African American","Hispanic/Latinx","Asian"))

model <- glm(g_ind~lawson_result+Period.x+Race+Highest.level.of.education.of.parent.guardian, data = df)
summary(model)

df$Period.x[df$Period.x=="F"] <- "E"

model <- glm(g_ind~lawson_result+Period.x+Race+Highest.level.of.education.of.parent.guardian, data = df)
summary(model)

model <- lm(g_ind~factor(Period.x), family=gaussian, data = df)
summary(model)

#############################################
## model without superfuous variables, as indicated by prior analyses above
## appears that Period.x explains the most data
## scientific reasoning (measured by lawson_result) fails significance test
# ordering of categorical levels

df$Highest.level.of.education.of.parent.guardian = factor(df$Highest.level.of.education.of.parent.guardian, levels = c("High School Graduate/GED","Some college","Bachelor's (4-year) degree","Graduate school--no degree earned","Graduate school--Masters","Graduate school--PhD"))
df$Race = factor(df$Race, levels = c("White/Caucasian","Black/African American","Hispanic/Latinx","Asian"))
df$Period.x[df$Period.x=="F"] <- "E" # make E and F same group, both part of control group

model_pre <- lm(pre_result~lawson_result+Period.x+Race+Highest.level.of.education.of.parent.guardian, data = df)
summary(model_pre)
anova(model_pre)

model_post <- lm(g_ind~lawson_result+Period.x, data = df)
summary(model_post)
anova(model_post)
plot(residuals(model_post))
plot(model_post)

save(model_pre,file="model_pre.RData")
save(model_post,file="model_post.RData")
#############################################


