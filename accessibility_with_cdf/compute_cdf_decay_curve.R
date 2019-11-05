library(tidyverse)
library(reshape2)
library(Hmisc)

setwd("~/Dropbox/work/OntarioLine")


# bring in the travel times!

# load
times_nc <- read.csv("fromMX/DATA FOR UofT/perceived_tt_BAU.csv") # , row.names = 1 
times_nc <- subset(times_nc,times_nc$X != "e+20") # remove the weird rows

# add in row names
rownames(times_nc) <- paste("X",times_nc$X,sep= "")
times_nc$X <- NULL

# add 5 min travel time to internal zone trips
times_nc <- times_nc + diag(3530) * 5

# round to save space
times_nc <- round(times_nc,digits = 0)

times_nc[times_nc > 3000] <- 3000

# just gtha
times_nc <- times_nc[0:2265,0:2265]


times_nc_melt <- melt(as.matrix(times_nc))

times_nc_melt$odid <- paste(times_nc_melt$Var1, times_nc_melt$Var2, sep ="")
times_nc_melt$Var1 <- NULL
times_nc_melt$Var2 <- NULL
colnames(times_nc_melt) <- c("time","odid")


# bring in the demand!

# load
demand_nc <- read.csv("fromMX/DATA FOR UofT/demand_do_nothing.csv", row.names = 1)
# rownames
rownames(demand_nc) <- paste("X",rownames(demand_nc),sep= "")


# round to save space
demand_nc <- round(demand_nc,digits = 5)


demand_nc <- demand_nc[0:2265,0:2265]

demand_nc_melt <- melt(as.matrix(demand_nc))

demand_nc_melt$odid <- paste(demand_nc_melt$Var1, demand_nc_melt$Var2, sep ="")
demand_nc_melt$Var1 <- NULL
demand_nc_melt$Var2 <- NULL
colnames(demand_nc_melt) <- c("demand","odid")




# # # # # #


dfm <- merge(times_nc_melt,demand_nc_melt,by = "odid")

# dfm$time[dfm$time > 300] <- 300

ggplot() + geom_histogram(aes(x = dfm$time, y = ..density.., weight = dfm$demand),binwidth = 5) +
  scale_x_continuous(limits = c(0,500))


# try estimating

dfmt <- dfm %>% group_by(time) %>%
  summarise(
    sum(demand)
  )
colnames(dfmt) <- c("time","trips")

ggplot() + geom_point(aes(x = dfmt$time, y = dfmt$trips))

# create a CDF

dfcdf <- as.data.frame(wtd.Ecdf(x = dfmt$time, w = dfmt$trips))
# flip the side were are cumulating towards
dfcdf$c <- 1 - dfcdf$ecdf

colnames(dfcdf) <- c("travel_time","cdf","one_minus_cdf")

# plot
ggplot() + geom_point(aes(x = dfcdf$travel_time, y = dfcdf$one_minus_cdf), size = 0.5) + scale_x_continuous(limits = c(0,300)) + xlab("\nX = Perceived Travel Time (minutes)") + ylab("Proportion of trips greater than X\n") + theme_minimal() + theme(text = element_text(size=10))  


# 4

# write.csv(dfcdf,"spatial_data/cdf_traveltimes.csv")



# cdf Toronto








# # # # # same for OL for testing / or just for Toronto


# load
times_nc <- read.csv("fromMX/DATA FOR UofT/perceived_tt_BAU.csv") # , row.names = 1 
times_nc <- subset(times_nc,times_nc$X != "e+20") # remove the weird rows

# add in row names
rownames(times_nc) <- paste("X",times_nc$X,sep= "")
times_nc$X <- NULL

# add 5 min travel time to internal zone trips
times_nc <- times_nc + diag(3530) * 5

# round to save space
times_nc <- round(times_nc,digits = 0)
  
times_nc[times_nc > 3000] <- 3000

# just gtha
times_nc <- times_nc[0:631,0:631]


times_nc_melt <- melt(as.matrix(times_nc))

times_nc_melt$odid <- paste(times_nc_melt$Var1, times_nc_melt$Var2, sep ="")
times_nc_melt$Var1 <- NULL
times_nc_melt$Var2 <- NULL
colnames(times_nc_melt) <- c("time","odid")


# bring in the demand!

# load
demand_nc <- read.csv("fromMX/DATA FOR UofT/demand_do_nothing.csv", row.names = 1)
# rownames
rownames(demand_nc) <- paste("X",rownames(demand_nc),sep= "")


# round to save space
demand_nc <- round(demand_nc,digits = 5)


demand_nc <- demand_nc[0:631,0:631]

demand_nc_melt <- melt(as.matrix(demand_nc))

demand_nc_melt$odid <- paste(demand_nc_melt$Var1, demand_nc_melt$Var2, sep ="")
demand_nc_melt$Var1 <- NULL
demand_nc_melt$Var2 <- NULL
colnames(demand_nc_melt) <- c("demand","odid")




# # # # # #


dfm <- merge(times_nc_melt,demand_nc_melt,by = "odid")

weighted.mean(dfm$time,dfm$demand)

ggplot() + geom_histogram(aes(x = dfm$time, y = ..density.., weight = dfm$demand),binwidth = 5) +
  scale_x_continuous(limits = c(0,500))


# try estimating

dfmt <- dfm %>% group_by(time) %>%
  summarise(
    sum(demand)
  )
colnames(dfmt) <- c("time","trips")

ggplot() + geom_point(aes(x = dfmt$time, y = dfmt$trips))

# create a CDF

dfcdf_ol <- as.data.frame(wtd.Ecdf(x = dfmt$time, w = dfmt$trips))
# flip the side were are cumulating towards
dfcdf_ol$c <- 1 - dfcdf_ol$ecdf

# plot
ggplot() + geom_point(aes(x = dfcdf_ol$x, y = dfcdf_ol$c)) + scale_x_continuous(limits = c(0,300)) + xlab("travel time (min)") + ylab("CDF") + theme_minimal()


write.csv(dfcdf_ol,"spatial_data/cdf_traveltimes_tor.csv")

# plot both
ggplot() + 
  geom_point(aes(x = dfcdf_ol$x, y = dfcdf_ol$c), size = 0.5) + 
  geom_point(aes(x = dfcdf$travel_time, y = dfcdf$one_minus_cdf), color = "red", size = 0.5) +
  scale_x_continuous(limits = c(0,300)) + xlab("travel time (min)") + ylab("CDF") + theme_minimal()
