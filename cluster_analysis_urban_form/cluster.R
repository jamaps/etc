# cluster anlaysis of census linked urban form variables

library(ggplot2)
library(ggthemes)
library(dplyr)
library(RColorBrewer)
library(reshape2)
library(questionr)

# setting up variables to input
dfcluster <- subset(df, df$tpov1 > 0.5)
dfcluster <- dfcluster[,c("comp_car","comp_transit","pop_density","II_in_LICO","h_app","b_since2000","mov_p_nonmovers")]

dfcluster <- subset(dfcluster, dfcluster$pop_density < 20000)

dfcluster <- na.omit(dfcluster) # listwise deletion of missing
dfcluster <- scale(dfcluster) # standardize variables

# Determine number of clusters
wss <- (nrow(dfcluster)-1)*sum(apply(dfcluster,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(dfcluster,
                                     centers=i)$withinss)

ggplot() + geom_point(aes(1:15, wss)) + geom_line(aes(1:15, wss)) + xlab("number of clusters") + ylab("within groups sum of squares") +
  scale_x_continuous(breaks = 1:15) +
  theme_minimal()  +
  theme(text = element_text(size=10))


# K-Means Cluster Analysis
fit <- kmeans(dfcluster, 2) # n clusters


# summarizing some of the results
dfclustero <- subset(df, df$tpov1 > 0.5)
dfclustero <- dfclustero[,c("dauid","comp_car","comp_transit","pop_density","II_in_LICO","h_app","b_since2000","mov_p_nonmovers","population","topv1cat")]
dfclustero <- subset(dfclustero, dfclustero$pop_density < 20000)

dfclustero <- na.omit(dfclustero) # listwise deletion of missing

dfclustero <- data.frame(dfclustero, fit$cluster)


aggregate(dfclustero,by=list(fit$cluster),FUN=mean)
table(fit$cluster)


ggplot() + geom_point(aes(x = dfcluster$comp_ratio,y = dfcluster$pop_density,color = as.character(dfcluster$fit.cluster)))


aggregate(dfcluster$population,by=list(dfcluster$fit.cluster),FUN=sum)
