####################
#
# Using the MIRT package to group students based on FCI answers
# mirt = Multidimensional Item Response Theory
#
####################

library(mirt)

set.seed(123)

df <- read.csv("old_pretest_data.csv", header = TRUE)
#df <- na.omit(df)



# vector for all possible question answers
make_qa_vect_1q <- function(x, row_num, test_type = 'pre', qnum){
  nvec <- vector(mode = "numeric", length = 5)
  qnum = qnum
  for (i in 1){    # 1 question
    answer = ""
    if (test_type == "pre"){
      answer = paste("X",i-1+qnum,".x", sep = "")
    }
    if (test_type == "post1"){
      answer = paste("X",i-1+qnum,".y", sep = "")
    }

    if (toString(x[row_num,answer]) == "A"){
      nvec[i*5 - 4] = nvec[i*5 - 4] + 1
    }
    if (toString(x[row_num,answer]) == "B"){
      nvec[i*5 - 3] = nvec[i*5 - 3] + 1
    }
    if (toString(x[row_num,answer]) == "C"){
      nvec[i*5 - 2] = nvec[i*5 - 2] + 1
    }
    if (toString(x[row_num,answer]) == "D"){
      nvec[i*5 - 1] = nvec[i*5 - 1] + 1
    }
    if (toString(x[row_num,answer]) == "E"){
      nvec[i*5] = nvec[i*5] + 1
    }
  }

  return(nvec)
}

# vector for all students
vect_class_1q <- function(x, test_type = 'pre',qnum){
  vclass <- vector()
  for (j in 1:nrow(x)){
    vclass <- rbind(vclass,make_qa_vect_1q(x,j,test_type,qnum))
  }
  return(vclass)
}



df_vect = 0
df_vect = vect_class_1q(df,'pre',19)
head(df_vect)
colnames(df_vect) <- c("A", "B", "C", "D", "E")
mod2 <- mirt(df_vect,1, method="MHRM",draws=50000)

#itemplot(mod2,3)

plot(mod2, type = 'trace')
plot(mod2, type = 'trace', facet_items=FALSE)

