##############
#
# Running decision tree machine learning strategies to see if certain
# question answers predict others, thus showing students fall into
# certain categories. The decision trees may also find unexpected
# correlations.
#
##############

library(rpart)
library(rpart.plot)

df <- read.csv("all_data_scored.csv", header = TRUE)


head(df)


fit <- rpart(g_ind~Period.x+pre_result+lawson_result+Gender.x+Race, method="anova", data=df)
printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# create additional plots
par(mfrow=c(1,1)) # two plots on one page
rsq.rpart(fit) # visualize cross-validation results

# plot tree
plot(fit, uniform=TRUE,
     main="Regression Tree of Normalized Gains")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

rpart.plot(fit,box.palette=0, branch.col='black', extra=1,
           type=5, branch.lwd=3, col="black",
           main="Breakdown of Student Normalized Gains \n Decision Tree")


post(fit, file = "tree_FCI.ps",
     title = "Regression Tree of Normalized Gains")



# prune the tree
pfit<- prune(fit, cp=0.056) # from cptable

# plot the pruned tree
plot(pfit, uniform=TRUE,
     main="Pruned Regression Tree of Normalized Gains")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "ptree_FCI.ps",
     title = "Pruned Regression Tree of Normalized Gains")

rpart.plot(pfit,box.palette=0, branch.col='black', extra=1,
           type=5, branch.lwd=3, col="black",
           main="Breakdown of Student Normalized Gains \n Decision Tree")

dev.copy(png,'decisiontree.png')
dev.off()
