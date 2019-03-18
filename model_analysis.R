##############
#
# Based on student answers, determine what mental model(s) they use
#
##############

library(stringr)
library(dplyr)
#library(matlib)

# loading in data
df <- read.csv("pre_post1.csv", header = TRUE)
key <- read.csv("FCI_key.csv", header = TRUE)
# key includes correct answers and common misconceptions

# only want to look at FCI questions related to force misconceptions
# key has missing elements for questions not related to impetus-like forces
pre_model <- matrix(0,nrow(df),3)
post1_model <- matrix(0,nrow(df),3)
num_miscon <- sum(is.na(key[2,])==FALSE)

# pretest
for (student in 1:nrow(df)){
  miscon = 0
  correct = 0
  for (q in 1:30){
    qu = ""
    answer = ""
    qu = paste("X",q, sep = "")
    answer = paste("X",q,".x", sep = "")
    if (grepl(toString(df[student,answer]),toString(key[2,qu])) && is.na(key[2,qu])==FALSE){
      miscon = miscon + 1
    }
    if (toString(df[student,answer]) == toString(key[1,qu]) && is.na(key[2,qu])==FALSE){
      correct = correct + 1
    }
  }
  pre_model[student,1] = correct
  pre_model[student,2] = miscon
  pre_model[student,3] = num_miscon - miscon - correct
  # if (toString(df[student,"Period.x"]) == "G"){
  #   pre_model[student,3] = 1
  # }
}

# posttest1
for (student in 1:nrow(df)){
  miscon = 0
  correct = 0
  for (q in 1:30){
    qu = ""
    answer = ""
    qu = paste("X",q, sep = "")
    answer = paste("X",q,".y", sep = "")
    if (grepl(toString(df[student,answer]),toString(key[2,qu])) && is.na(key[2,qu])==FALSE){
      miscon = miscon + 1
    }
    if (toString(df[student,answer]) == toString(key[1,qu]) && is.na(key[2,qu])==FALSE){
      correct = correct + 1
    }
  }
  post1_model[student,1] = correct
  post1_model[student,2] = miscon
  post1_model[student,3] = num_miscon - miscon - correct
  # if (toString(df[student,"Period.x"]) == "G"){
  #   post1_model[student,3] = 1
  # }
}


# normalize matrices 
pre_model = pre_model/num_miscon/nrow(df)
post1_model = post1_model/num_miscon/nrow(df)

u_pre_model = t(t(sqrt(colSums(pre_model))))
u_post1_model = t(t(sqrt(colSums(post1_model))))

# model matrices
D_pre_model = (u_pre_model %*% t(u_pre_model))
D_post1_model = (u_post1_model %*% t(u_post1_model))

pre_eigen = eigen(D_pre_model)
post1_eigen = eigen(D_post1_model)

###############################
#
# The above analysis provides the eigenvectors that represent the 
# entire cohort of students. We will repeat the above but will
# seperate out students into the experimental and control groups.
#
###############################

pre_exp_model <- matrix(0,nrow(df[df$Period.x=="G",]),3)
post1_exp_model <- matrix(0,nrow(df[df$Period.x=="G",]),3)
pre_cont_model <- matrix(0,nrow(df[df$Period.x!="G",]),3)
post1_cont_model <- matrix(0,nrow(df[df$Period.x!="G",]),3)
num_miscon <- sum(is.na(key[2,])==FALSE)

## for efficiency, creating fuctions
grading_pre <- function(X){
  df_trun <- df[df$Period.x==X,]
  pre_m = matrix(0,nrow(df_trun),3)
  
  for (student in 1:nrow(df_trun)){
    miscon = 0
    correct = 0
    for (q in 1:30){
      qu = ""
      answer = ""
      qu = paste("X",q, sep = "")
      answer = paste("X",q,".x", sep = "")
      if (grepl(toString(df_trun[student,answer]),toString(key[2,qu])) && is.na(key[2,qu])==FALSE){
        miscon = miscon + 1
      }
      if (toString(df_trun[student,answer]) == toString(key[1,qu]) && is.na(key[2,qu])==FALSE){
        correct = correct + 1
      }
    }
    pre_m[student,1] = correct
    pre_m[student,2] = miscon
    pre_m[student,3] = num_miscon - miscon - correct
  }
  return(pre_m)
}

grading_post1 <- function(X){ 
  df_trun <- df[df$Period.x==X,]
  post1_m = matrix(0,nrow(df_trun),3)
  
  for (student in 1:nrow(df_trun)){
    miscon = 0
    correct = 0
    for (q in 1:30){
      qu = ""
      answer = ""
      qu = paste("X",q, sep = "")
      answer = paste("X",q,".y", sep = "")
      if (grepl(toString(df_trun[student,answer]),toString(key[2,qu])) && is.na(key[2,qu])==FALSE){
        miscon = miscon + 1
      }
      if (toString(df_trun[student,answer]) == toString(key[1,qu]) && is.na(key[2,qu])==FALSE){
        correct = correct + 1
      }
    }
    post1_m[student,1] = correct
    post1_m[student,2] = miscon
    post1_m[student,3] = num_miscon - miscon - correct
  }
  return(post1_m)
}

# create eigenvectors and eigenvalues for model
eigen_model <- function(M){
  #normalize matrix elements
  Mnorm = M/nrow(M)/num_miscon
  
  #transpose, make into prob distribution vector
  u = t(t(sqrt(colSums(Mnorm))))
  
  #density matrix
  D = u %*% t(u)
  
  return(eigen(D))
}




# pass in period, only Period G is experimental group
pre_exp_model <- grading_pre("G")
pre_cont_model <- rbind(grading_pre("E"), grading_pre("F")) # rbind together both control periods
post1_exp_model <- grading_post1("G")
post1_cont_model <- rbind(grading_post1("E"), grading_post1("F"))


pre_exp_eigen = eigen_model(pre_exp_model)
pre_cont_eigen = eigen_model(pre_cont_model)
post1_exp_eigen = eigen_model(post1_exp_model)
post1_cont_eigen = eigen_model(post1_cont_model)


# saving files for graphing and documentation
save(pre_exp_eigen,file="pre_exp_eigen.RData")
save(pre_cont_eigen,file="pre_cont_eigen.RData")
save(post1_exp_eigen,file="post1_exp_eigen.RData")
save(post1_cont_eigen,file="post1_cont_eigen.RData")


