##############
#
# Using model_analysis.R results, creage graphs showing probabilities
# of using mental models and how that changes between groups over time
#
##############

library(ggplot2)

# loading eigenvectors/eigenvalues
load("pre_exp_eigen.RData")
load("pre_cont_eigen.RData")
load("post1_exp_eigen.RData")
load("post1_cont_eigen.RData")

###
#
# first eigenvalue largest, is the dominant model
# eigenvector[1,1] is prob density of using model 1 (Newtonian)
# eigenvector[2,1] is prob density of using model 2 (pre-Newtonian)
# eigenvector[3,1] is prob density of using random answer
#
###


# probability of using model i: eigval[1]^2 * eigvec[i,1]^2
a1=pre_exp_eigen$values[1]^2*pre_exp_eigen$vectors[1,1]^2 # y-value
b1=pre_exp_eigen$values[1]^2*pre_exp_eigen$vectors[2,1]^2 # x_value
c1=post1_exp_eigen$values[1]^2*post1_exp_eigen$vectors[1,1]^2 # y-value
d1=post1_exp_eigen$values[1]^2*post1_exp_eigen$vectors[2,1]^2 # x-value

a2=pre_cont_eigen$values[1]^2*pre_cont_eigen$vectors[1,1]^2
b2=pre_cont_eigen$values[1]^2*pre_cont_eigen$vectors[2,1]^2
c2=post1_cont_eigen$values[1]^2*post1_cont_eigen$vectors[1,1]^2
d2=post1_cont_eigen$values[1]^2*post1_cont_eigen$vectors[2,1]^2

# pre_exp_eigen$values[1]^2*pre_exp_eigen$vectors[1,1]^2 + pre_exp_eigen$values[1]^2*pre_exp_eigen$vectors[2,1]^2
# post1_exp_eigen$values[1]^2*post1_exp_eigen$vectors[1,1]^2 + post1_exp_eigen$values[1]^2*post1_exp_eigen$vectors[2,1]^2
# pre_cont_eigen$values[1]^2*pre_cont_eigen$vectors[1,1]^2 + pre_cont_eigen$values[1]^2*pre_cont_eigen$vectors[2,1]^2
# post1_cont_eigen$values[1]^2*post1_cont_eigen$vectors[1,1]^2 + post1_cont_eigen$values[1]^2*post1_cont_eigen$vectors[2,1]^2

plot(0:1, 0:1, type="n", xlab="Prob(Naive Model)", ylab="Prob(Newtonian Model)", frame.plot=FALSE,xaxs = "i",yaxs="i")
title(main="Model Analysis \n Newtonian vs Naive Model")
abline(1,-1)
abline(0.4,-1)
legend(0.55,0.91, legend=c("Experimental","Control"),col=c("blue","red"),lty=1, cex=0.8)
legend(0.55,0.7, legend=c("Pretest","Posttest"), pch=c(3,24), cex=0.8)
#abline(0,1/3)
#abline(0,3)
points(b1,a1, pch=3, col = "black")
points(d1,c1, pch=24, col = "black")
arrows(b1,a1, d1, c1, col = "blue", length = 0.1)

points(b2,a2, pch=3, col = "black")
points(d2,c2,pch=24, col = "black")
arrows(b2,a2, d2,c2, col = "red", length = 0.1)

dev.copy(png,'modelplot.png')
dev.off()

