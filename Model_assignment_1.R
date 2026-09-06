#Setup package
library(tidyverse)
library(dslabs)
library(deSolve)
library(ggplot2)
library(lhs)
library(outbreaks)
library(optimization)


#Import data
load("assignment1_df_2026.RData")
#Update dataset name to mumps_sav in 20260202.
mumps_sav <- assignment1_df %>% 
  rename(time_days = time)

## Refer to the SIR model code as in lectures 1 & 2.
##Update, add Death and alpha in 20260202
sird_model <- function(t, y, params){
  
  S <- y[1]
  I <- y[2]
  R <- y[3]
  D <- y[4]  
  N <- S + I + R + D
  
  beta  <- params["beta"]
  gamma <- params["gamma"]
  alpha <- params["alpha"] 
  lambda <- 1 - alpha
  
  dS <- -(beta * S * I) / N
  dI <- (beta * S * I) / N - gamma * I
  dR <- alpha * gamma * I
  dD <- lambda * gamma * I
  
  list(c(dS, dI, dR, dD))
}

# ---- Initial conditions ----
par_init = c(beta=1, gamma=1, alpha=1 ) # Some fairly arbitrary starting values

### - to include init_sav
### - to include the times_sav

init_sav = c(S=1199, I=1, R=0, D=0)
times_sav = mumps_sav$time_days

opt_fun_sav = function(params){
  
  ## The following lines prevent 'optim_nm' from wandering into negative parameter space
  if (params[1]<0){
    params[1] = -params[1]
  } 
  if(params[2]<0){
    params[2] = -params[2]
  }
  if(params[3]<0){
    params[3] = -params[3]
  }  
  ## Run the SIR model with the current parameter values 
  output_sav_try = ode(
    y=init_sav, 
    times = times_sav, 
    func = sird_model, 
    parms= c(
      beta = params[1], 
      gamma=params[2],
      alpha = params[3]
    )
  )
  
  output_savtry_df = as.data.frame(output_sav_try) # coerce to a data.frame object

  n_obs = nrow(mumps_sav)
  
  ## Create a data frame with observed and model prediction data for Savoonga
  sav_rmse_df = data.frame(
    time = mumps_sav$time_days,
    obs = mumps_sav$D_obs,
    # the following lines find the cumulative I prediction for each time in the mummps data
    pred = sapply(1:n_obs, function(i){output_savtry_df$D[
      output_savtry_df$time == mumps_sav$time_days[i]
    ]})
  )
  # Calculate the RMSE for this prediction
  rmse_try = sqrt((1/n_obs)*(sum((sav_rmse_df$pred - sav_rmse_df$obs)^2)))  
  
  return(rmse_try)
  
}
# Run Nelder-Mead optimisation to find the point in parameter space leading to the smallest RMSE
optnm_result=optim_nm(opt_fun_sav,start=c(0.6,0.6,0.6),k=3, trace=T)
optnm_result

### Run the SIR model at the values found by optim_nm

output_sav_nm = ode (y=init_sav, times = times_sav, func = sird_model,  
                     parms= c(
                       beta = optnm_result$par[1], 
                       gamma=optnm_result$par[2],
                       alpha = optnm_result$par[3]
                     ))

output_savnm_df = as.data.frame(output_sav_nm) # coerce to a data.frame object

# Reformat to 'long'
output_savnm_tidy = pivot_longer(output_savnm_df, cols = c("S", "I", "R", "D"), names_to = "Compartment")

# Create a data frame with corresponding outbreak data from Savoonga and SIR model data
n_obs = nrow(mumps_sav)

sav_nm_pred = data.frame(
  time = mumps_sav$time_days,
  obs = mumps_sav$D_obs,
  pred = sapply(1:n_obs, function(i){output_savnm_df$D[
    output_savnm_df$time == mumps_sav$time_days[i]
  ]})
)
# Calculate the RMSE from this model fit
rmse = sqrt((1/n_obs)*(sum((sav_nm_pred$pred - sav_nm_pred$obs)^2)))

ggplot() +
  # Plot the observed data in blue (with points and a ribbon)
  geom_point(data=mumps_sav, aes(x=time_days, y=D_obs), col="blue") +
  geom_ribbon(data=mumps_sav, aes(x=time_days, ymin=0, ymax=D_obs), fill="blue", alpha=0.25) +
  # Plot cumulative I from the SIR model output as a line
  geom_line(data=output_savnm_tidy[output_savnm_tidy$Compartment=="D",], aes(x=time, y=value)) +
  # Label axes and title
  ylab("Cumulative deaths") + xlab("Time (days)") + 
  ggtitle(sprintf("beta = %g, gamma = %g, alpha = %g, RMSE = %g",
                  optnm_result$par[1], optnm_result$par[2], optnm_result$par[3], rmse))

R0 <- optnm_result$par[1] / optnm_result$par[2]
R0
