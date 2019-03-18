##################
#
# Perform after pre_post_combo
#
##################

library(stringr)
library(dplyr)

df <- read.csv("all_data.csv", header = TRUE)
key <- read.csv("FCI_key.csv", header = TRUE)

head(df)
head(key)

toString(df[1,"X3.x"])
df$X1.x[1]
key[[1,"X1"]]

pre_result <- vector(mode = "numeric", length = nrow(df))
post1_result <- vector(mode = "numeric", length = nrow(df))

## scoring
# pretest (.x)
for (student in 1:nrow(df)){
  for (q in 1:30){
    qu = ""
    answer = ""
    qu = paste("X",q, sep = "")
    answer = paste("X",q,".x", sep = "")
    if(toString(df[student,answer]) == toString(key[1,qu])) { 
      pre_result[student] = pre_result[student] + 1
      }
  }
}

# posttest (.y)
for (student in 1:nrow(df)){
  for (q in 1:30){
    qu = ""
    answer = ""
    qu = paste("X",q, sep = "")
    answer = paste("X",q,".y", sep = "")
    if(toString(df[student,answer]) == toString(key[1,qu])) { 
      post1_result[student] = post1_result[student] + 1
    }
  }
}


mean(pre_result)
mean(post1_result)

hist(pre_result)
hist(post1_result)

## normalized gains
g_ind <- vector(mode = "numeric", length = nrow(df))

for (i in 1:nrow(df)){
  g_ind[i] <- (post1_result[i] - pre_result[i])/(30 - pre_result[i])
}

mean(g_ind)

## effect size
# s is pooled standard dev, defined by Cohen (1988)
s = sqrt(((length(pre_result)-1)*sd(pre_result)^2+(length(post1_result)-1)*sd(post1_result)^2)/(length(pre_result)+length(post1_result)-2))
d = (mean(post1_result) - mean(pre_result))/s


##################################################################
### breaking into experimental and control groups
## Periods E and F control section
## Period G experimental section
##################################################################

pre_cont <- vector(mode = "numeric", length = nrow(df) - sum(df$Period.x == "G"))
post1_cont <- vector(mode = "numeric", length = nrow(df) - sum(df$Period.x == "G"))
pre_exp <- vector(mode = "numeric", length = sum(df$Period.x == "G"))
post1_exp <- vector(mode = "numeric", length = sum(df$Period.x == "G"))

## scoring
# pretest (.x)
i = 1
j = 1

for (student in 1:nrow(df)){
  score = 0
  
  for (q in 1:30){
    qu = ""
    answer = ""
    qu = paste("X",q, sep = "")
    answer = paste("X",q,".x", sep = "")
    if(toString(df[student,answer]) == toString(key[1,qu])) { 
      score = score + 1
    }
  }
  
  if(toString(df[student,"Period.x"]) == "G"){
    pre_exp[i] = pre_exp[i] + score
    i = i + 1
  } else {
    pre_cont[j] = pre_cont[j] + score
    j = j + 1
  }
}


## scoring
# posttest (.y)
i = 1
j = 1

for (student in 1:nrow(df)){
  score = 0
  
  for (q in 1:30){
    qu = ""
    answer = ""
    qu = paste("X",q, sep = "")
    answer = paste("X",q,".y", sep = "")
    if(toString(df[student,answer]) == toString(key[1,qu])) { 
      score = score + 1
    }
  }
  
  if(toString(df[student,"Period.x"]) == "G"){
    post1_exp[i] = post1_exp[i] + score
    i = i + 1
  } else {
    post1_cont[j] = post1_cont[j] + score
    j = j + 1
  }
}

mean(pre_exp)
mean(post1_exp)
mean(pre_cont)
mean(post1_cont)

se = sqrt(sd(pre_exp)^2/length(pre_exp) + sd(pre_cont)^2/length(pre_cont))
degf = se^2/((sd(pre_exp)^2/length(pre_exp))^2/(length(pre_exp)-1) + (sd(pre_cont)^2/length(pre_cont))^2/(length(pre_cont)-1))
t = (mean(pre_exp) - mean(pre_cont))/se

## normalized gains
g_exp <- vector(mode = "numeric", length = sum(df$Period.x == "G"))

for (i in 1:sum(df$Period.x == "G")){
  g_exp[i] <- (post1_exp[i] - pre_exp[i])/(30 - pre_exp[i])
}

mean(g_exp)

g_cont <- vector(mode = "numeric", length = nrow(df) - sum(df$Period.x == "G"))

for (i in 1:(nrow(df)-sum(df$Period.x == "G"))){
  g_cont[i] <- (post1_cont[i] - pre_cont[i])/(30 - pre_cont[i])
}

mean(g_cont)

hist(pre_exp)
## effect size
# s is pooled standard dev, defined by Cohen (1988)
s = sqrt(((length(pre_exp)-1)*sd(pre_exp)^2+(length(post1_exp)-1)*sd(post1_exp)^2)/(length(pre_exp)+length(post1_exp)-2))
d_exp = (mean(post1_exp) - mean(pre_exp))/s

s = sqrt(((length(pre_cont)-1)*sd(pre_cont)^2+(length(post1_cont)-1)*sd(post1_cont)^2)/(length(pre_cont)+length(post1_cont)-2))
d_cont = (mean(post1_cont) - mean(pre_cont))/s


# Lawson scoring
lawson_result = vector(mode = "numeric", length = nrow(all_data_anon))
for (student in 1:nrow(df)){
  for (q in 1:24){
    qu = ""
    answer = ""
    qu = paste("X",q, sep = "")
    answer = paste("X",q, sep = "")
    if(toString(all_data_anon[student,answer]) == toString(key[3,qu])) { 
      lawson_result[student] = lawson_result[student] + 1
    }
  }
}

lawson_result
df = cbind(df,pre_result,post1_result,g_ind,lawson_result)


## saving data
save(pre_result, file="pre_result.RData")
save(post1_result, file="post1_result.RData")
save(g_ind, file="g_ind.Rdata")
save(g_exp, file="g_exp.Rdata")
save(g_cont, file="g_cont.Rdata")
save(pre_exp, file="pre_exp.RData")
save(pre_cont, file="pre_cont.RData")
save(post1_exp, file="post1_exp.RData")
save(post1_cont, file="post1_cont.RData")
write.csv(df, file="all_data_scored.csv")

