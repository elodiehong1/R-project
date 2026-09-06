library(ggplot2)
## Read the file into R - the filepath will need to be changed.
## Setting your working directory to the folder you're saving the output
## files in will make this simpler.
## Naming your files and objects well can also help as you try to distinguish 
## between many similar plots and files
exp_anywhere_ip6_prob06 = read.csv("Epidemic_ABM_RHO experiment-table1", skip=6, header=T)
## A quick way to check that the data has imported correctly (and to see
## what the columns are) is to use 'summary'
summary(exp_anywhere_ip6_prob06) 
## Rename the columns of interest to be easier to work with
## These will likely be:
## - X.run.number
## - X.step
## - count.turtles.with...Status..I
## - count.turtles.with...Status..R
## but may include others #
## The following line selects the columns of interest and renames them
names(exp_anywhere_ip6_prob06)[c(1,9,10,11)] = c("runID", "TimeStep", "CountInfected", "CountRecovered")
summary(exp_anywhere_ip6_prob06)
## Plot [in this case] the CountInfected data against time
## the 'group' argument means that the plot shows each runID as a separate line
## but doesn't colour them or make a legend
ggplot(data=exp_anywhere_ip6_prob06, aes(x=TimeStep, y=CountInfected, group = runID)) + geom_path()

# create a vector of runIDs in the dataset
runIDs = unique(exp_anywhere_ip6_prob06$runID)
# Create a vector the same length as runIDs
# This vector will hold the time of maximum for each run
times_maxI = rep(NA, length(runIDs))
# For each i in runIDs
for (i in runIDs){
  ## create a dataframe containing that run's data
  exp_i = exp_anywhere_ip6_prob06[exp_anywhere_ip6_prob06$runID == i,]
  ## find the maximum of the infected values
  max_inf_i = max(exp_i$CountInfected)
  ## find the time step at which that occurs
  time_maxinf_i = exp_i$TimeStep[exp_i$CountInfected == max_inf_i] 
  ## record the time step of maximum infection in the times_maxI vector
  ## Sometimes there will be more than one timestep for which the infectious value
  ## is at its highest, so we use 'min' to choose the first of these
  times_maxI[i] = min(time_maxinf_i)
}
# Plot this as a histogram
hist(times_maxI, breaks=20)

