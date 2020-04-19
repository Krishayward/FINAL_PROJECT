#libraries
library(ggplot2) #used for plotting
library(car) #used for standardizing data
library(dplyr) #used for manipulating dataframes

#import data from current directory
dat <- read.csv("step1_output_MEG.csv", header = T)
dat<- dat %>% na.omit()
#force timestamp info to character class for ease of use later.
dat$LocalDateTime <- as.character(dat$LocalDateTime)

#get variable names
var.names <- unique(dat$VariableCode)

#initialize list variables to store histograms, information from symbox() regarding best transformations.
histograms.list <- list()
transforms.list <- list()

#Histogram of data values before any cleaning is done.
qplot(dat$DataValue) + geom_histogram()

#plot histograms of each variable to assess normality
for (i in 1:length(var.names)) {
  #subset data by variable code and print variable name
  var <- var.names[i]
  print(var)
  #Create temporary dataframe to perform analysis of data for a single variable
  temp.dat <- dat[dat$VariableCode == var, ]
  #Print dimensions of temporary dataframe
  print(dim(temp.dat))
  #Calculate and print the mean and standard deviation of the variable measured, with any NA values removed
  temp.mean <- mean(temp.dat$DataValue, na.rm = T)
  print(temp.mean)
  temp.sd <- sd(temp.dat$DataValue, na.rm = T)
  print(temp.sd)
  #plot histogram and save as part of a variable
  histograms.list[[i]] <- qplot(DataValue, data = temp.dat) + geom_histogram() +
    xlab(paste(var, "Value", sep = " ")) + ylab("Count") + theme_bw() + 
    stat_function(fun = dnorm,
                   args = list(mean = temp.mean,
                               sd = temp.sd), 
                   colour = "plum4", 
                   lwd = 2)
  #Store output of a series of 
  transforms.list[[i]] <- car::symbox(~DataValue, data = temp.dat)
  }

#' *No data is normally distributed*

#determine appropriate transformations for non-normal data & perform them.
#' visual check of the graphical output of car::symbox(~DataValue, data = temp.dat) on line 35 provide 
#' the following suggestions for transformations (raising to the power of x)
#' *DSRad_10minAvg* x = 0.5
#' *WatTmp_10minAvg_0.1m* x = 1 (no transformation)
#' *Cond_10minAvg_1m* x = 1 (no transformation)
#' *pH_10minAvg_1m* x = 1 (no transformation)
#' *Chla_10minAvg_1m* log transform 
#' *ChlRFU_10minAvg_1m* log transform

#Transform data
dat.DSRad <- mutate(dat[dat$VariableCode == "DSRad_10minAvg", ], trans_varb = dat$DataValue[dat$VariableCode == "DSRad_10minAvg"]^0.5)
dat.WatTmp <- mutate(dat[dat$VariableCode == "WatTmp_10minAvg_0.1m", ], trans_varb = dat$DataValue[dat$VariableCode == "WatTmp_10minAvg_0.1m"]^1)
dat.Cond <- mutate(dat[dat$VariableCode == "Cond_10minAvg_1m", ], trans_varb = dat$DataValue[dat$VariableCode == "Cond_10minAvg_1m"]^1)
dat.pH <- mutate(dat[dat$VariableCode == "pH_10minAvg_1m", ], trans_varb = dat$DataValue[dat$VariableCode == "pH_10minAvg_1m"]^1)
dat.Chla<- mutate(dat[dat$VariableCode == "Chla_10minAvg_1m",], trans_varb = log(dat$DataValue[dat$VariableCode == "Chla_10minAvg_1m"]) + 1)
dat.ChlRFU <- mutate(dat[dat$VariableCode == "ChlRFU_10minAvg_1m", ], trans_varb = log(dat$DataValue[dat$VariableCode == "ChlRFU_10minAvg_1m"]) + 1)

#Merge into one dataframe
dat.full <- rbind(dat.ChlRFU, dat.DSRad, dat.WatTmp, dat.Cond, dat.pH, dat.Chla)

#Get timestamps affiliated with each variable
datestamps1 <- as.character(dat.full$LocalDateTime[dat.full$VariableCode == "DSRad_10minAvg"])
datestamps2 <- as.character(dat.full$LocalDateTime[dat.full$VariableCode == "WatTmp_10minAvg_0.1m"])
datestamps3 <- as.character(dat.full$LocalDateTime[dat.full$VariableCode == "Cond_10minAvg_1m"])
datestamps4 <- as.character(dat.full$LocalDateTime[dat.full$VariableCode == "pH_10minAvg_1m"])
datestamps5 <- as.character(dat.full$LocalDateTime[dat.full$VariableCode == "ChlRFU_10minAvg_1m"])
datestamps6 <- as.character(dat.full$LocalDateTime[dat.full$VariableCode == "Chla_10minAvg_1m"])

#Produce a vector of timestamps which are associated with observations for all variables
full.timestamps <- Reduce(intersect, list(datestamps1, datestamps2, datestamps3, datestamps4, datestamps5, datestamps6))

#Produce a clean dataset which contains only observations assocated with the timestamps identified in the code immediately above
dat.clean <- dat.full[which(dat.full$LocalDateTime %in% full.timestamps), ]

#Influence check - we decided this would make more sense after model selection 

#Write .csv file with tranformed dataset
write.csv(dat.clean,"step2_output_ERIC.csv", row.names = T)
