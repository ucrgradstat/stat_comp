## Date-Time ####
Date_Time <- format(Sys.time(),"%Y-%m-%d-%H-%M") #Used as a unique identifier


## Setting WD ####
dir.create("~/rwork")
setwd("~/rwork") # Setting working directory to rwork, were all the data is saved


## Download Essential Files ####

download.file("https://bit.ly/3gZJBsy", "slurm.tmpl")
download.file("https://bit.ly/3nvSNHA", ".batchtools.conf.R")

## Install R Packages ####

# install.packages("batchtools")
# install.packages("RenvModule")



## Loading R Packages ####
library(batchtools)
library(RenvModule)
library(mvtnorm)


## Functions ####

data_set_sim <- function(seed, nobs, beta, sigma, xmeans, xsigs){ # Simulates the data set
  set.seed(seed) # Sets a seed
  xrn <- rmvnorm(nobs, mean = xmeans, sigma = xsigs) # Simulates Predictors
  xped <- cbind(rep(1,nobs),xrn) # Creating Design Matrix
  y <- xped %*% beta + rnorm(nobs ,0, sigma) # Simulating Y
  df <- data.frame(x=xrn, y=y) # Creating Data Frame
  return(df)
}

data_sim <- function(data){ # Simulates the data set
  df_list <- lapply(1:data$N, data_set_sim,
                    nobs = data$nobs, beta = data$beta, sigma = data$sigma,
                    xmeans = data$xmeans, xsigs = data$xsigs)
  return(list(df_list = df_list, formula = data$formula))
}


parallel_lm<-function(data){
  ll<-length(data$df_list)
  lm_coef <- function(formula, data){ # Applying a Ordinary Least Squares to a data frame and formula
    lm_res <- lm(formula, data = data) # Find OLS Estimates
    return(as.vector(coef(lm_res)))# Obtaining 
  }
  results <- parallel::mclapply(data$df_list, lm_coef, formula = data$formula,
                                mc.cores = 8)
  mat<-matrix(unlist(results), nrow = ll, byrow = T)
  return(mat)
}

## Simulation Parameters ####
sim_list <- list(list(N = 500, # Number of Data sets
                      nobs = 200, # Number of observations
                      beta = c(20, 30), # beta parameters
                      xmeans = c(0), # Means for predictors
                      xsigs = diag(rep(1, 1)), # Variance for predictor
                      sigma = 3, # Variance for error term
                      formula = y ~ x),
                 list(N = 500, # Number of Data sets
                      nobs = 200, # Number of observations
                      beta = c(5, -8, 2), # beta parameters
                      xmeans = c(-2, 0), # Means for predictors
                      xsigs = diag(rep(1, 2)), # Variance for predictor
                      sigma = 3, # Variance for error term
                      formula = y ~ x.1 + x.2),
                 list(N = 500, # Number of Data sets
                      nobs = 200, # Number of observations
                      beta = c(5, 4, -5, -3), # beta parameters
                      xmeans = c(-2, 0, 2), # Means for predictors
                      xsigs = diag(rep(1, 3)), # Variance for predictor
                      sigma = 3, # Variance for error term
                      formula = y ~ x.1 + x.2 + x.3),
                 list(N = 500, # Number of Data sets
                      nobs = 200, # Number of observations
                      beta = c(5, 4, -5, -3, 6), # beta parameters
                      xmeans = c(-2, 0, 2, 8), # Means for predictors
                      xsigs = diag(rep(1, 4)), # Variance for predictor
                      sigma = 3, # Variance for error term
                      formula = y ~ x.1 + x.2 + x.3 + x.4)
)

## Simulating Data ####

standard_data <- lapply(sim_list, data_sim) # Using data_sim function to simulate N data sets


## Obtaining Estimates ####
module('load','slurm')
reg <- makeRegistry(file.dir="myregdir", conf.file=".batchtools.conf.R")
res <- list(partition="statsdept", walltime=120, ntasks=1, ncpus=8, memory=1024)

submission_id <- batchMap(parallel_lm, data = standard_data) # Prepares to submit each element of standard data as a job 

done <- submitJobs(submission_id, reg=reg, resources=res) # Submits job from submission_id

getStatus() # Provides the status of each job

## Average results ####

parallel_results <- lapply(1:length(standard_data), loadResult) #obtains the results of each job adds them as an element in a list
lapply(parallel_results, colMeans) # Averages each matrix

## Saving Results ####
results <- list(results = parallel_results, data = standard_data, # Combining list
                parameters = sim_list, Date_Time = Date_Time)

save_dir <- paste("Results_", Date_Time, ".RData", sep="") # Creating file name, contains date-time
save(results, file = save_dir, version = 2) # Saving RData file, recommend using version 2

## Clear Registry
clearRegistry()

## Delete Registry
removeRegistry(wait=0, reg=reg) 
