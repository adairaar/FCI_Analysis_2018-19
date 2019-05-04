##############
#
# Using k-means clustering to find if students fall into identifiable
# categories/types.
#
##############

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(FactoMineR) # PCA analysis
library(nFactors)   # factor analysis
library(psych)


set.seed(123)

#df <- read.csv("all_data_scored.csv", header = TRUE)
df <- read.csv("old_pretest_data.csv", header = TRUE)

#df <- na.omit(df)


#############
# need to turn turn answers to MC questions into vector with
# numerical values. Possibility: create 150-column vector for all answers
# students could have given (30 Qs, 5 possibilities each)
# then run k-means on that.
############

####################################################
#################### FUNCTIONS #####################
####################################################

# vector for all possible question answers
make_qa_vect <- function(x, row_num, test_type = 'pre'){
  nvec <- vector(mode = "numeric", length = 150)
  for (i in 1:30){    # 30 questions
    answer = ""
    if (test_type == "pre"){
      answer = paste("X",i,".x", sep = "")
    }
    if (test_type == "post1"){
      answer = paste("X",i,".y", sep = "")
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
vect_class <- function(x, test_type = 'pre'){
  vclass <- vector()
  for (j in 1:nrow(x)){
    vclass <- rbind(vclass,make_qa_vect(x,j,test_type))
  }
  return(vclass)
}


# vector for all possible question answers
make_qa_vect_impetus <- function(x, row_num, test_type = 'pre'){
  nvec <- vector(mode = "numeric", length = 150)
  for (i in cbind(4,5,11,13,15,17,18,25,28,30)){    # 10 questions
    answer = ""
    if (test_type == "pre"){
      answer = paste("X",i,".x", sep = "")
    }
    if (test_type == "post1"){
      answer = paste("X",i,".y", sep = "")
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
vect_class_impetus <- function(x, test_type = 'pre'){
  vclass <- vector()
  for (j in 1:nrow(x)){
    vclass <- rbind(vclass,make_qa_vect_impetus(x,j,test_type))
  }
  vclass <- vclass[, colSums(vclass == 0) != nrow(vclass)] #drop empty columns
  return(vclass)
}


####################################################
####################################################
####################################################
####################################################


####################################################
#################### Processing ####################
####################################################


df_vect = vect_class_impetus(df,'pre')
df_vect <- scale(df_vect)
df_vect <- df_vect[, colSums(is.na(df_vect)) != nrow(df_vect)] # avoids unscalable columns/rows
head(df_vect)


# Pricipal Components Analysis
# entering raw data and extracting PCs
# from the correlation matrix
fit <- princomp(df_vect, cor=TRUE)
summary(fit) # print variance accounted for
loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
fit$scores # the principal components
biplot(fit)

fit <- principal(df_vect, nfactors=4, rotate="varimax")
fit # print results


# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors,
# with varimax rotation
fit <- factanal(df_vect, 5, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2
load <- fit$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(df_vect),cex=.7) # add variable names



# Determine Number of Factors to Extract
ev <- eigen(cor(df_vect)) # get eigenvalues
ap <- parallel(subject=nrow(df_vect),var=ncol(df_vect),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)



# distance graph
#distance <- get_dist(df_vect)
#fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

## clustering
# 2 and 3 clusters assumed to be best num of clusters
k2 <- kmeans(df_vect, centers = 2, nstart = 25)
str(k2)
k2
fviz_cluster(k2, data = df_vect)

k3 <- kmeans(df_vect, centers = 3, nstart = 25)
str(k3)
k3
fviz_cluster(k3, data = df_vect)

k6 <- kmeans(df_vect, centers = 6, nstart = 25)
str(k6)
k6
fviz_cluster(k6, data = df_vect)

# choosing best number of clusters
fviz_nbclust(df_vect, kmeans, method = "silhouette")
gap_stat <- clusGap(df_vect, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)






