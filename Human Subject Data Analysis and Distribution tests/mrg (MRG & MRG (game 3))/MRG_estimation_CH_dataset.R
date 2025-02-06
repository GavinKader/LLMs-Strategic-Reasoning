# -Initialize--------------------------------------------------------------------
rm(list=ls())
library(msm)
library(parallel)
library(pbapply)
library(alabama)
library(ggplot2)
library(extrafont) 
library(maxLik)
#setwd("") Set working directory
num_cores <- detectCores() - 1
llm<-"mrg"

# Defining the log-likelihood function -----------------
log_likelihood_ch_mrg <- function(params, x) {
  exp_params <- exp(params[1])
  p0 <- exp(-exp_params[1])
  p1 <- p0 * exp_params[1] / 1
  p2 <- p1 * exp_params[1] / 2
  p3 <- p2 * exp_params[1] / 3
  p4 <- p3 * exp_params[1] / 4
  p5 <- 1 - p0 - p1 - p2 - p3 - p4
  sf <- 0.0001#scaling factor for MLE
  
  b0 <- 20
  b1 <- 19
  b2 <- round((p1 * b1 + p0 * b0) / (p1 + p0)) - 1
  b3 <- round((p2 * b2 + p1 * b1 + p0 * b0) / (p2 + p1 + p0)) - 1
  b4 <- round((p3 * b3 + p2 * b2 + p1 * b1 + p0 * b0) / (p3 + p2 + p1 + p0)) - 1
  
  f0 <- 1/10
  f1 <- 1*(x==b0)
  f2 <- 1*(x==b1)
  f3 <- 1*(x==b2)
  f4 <- 1*(x==b3)
  f5 <- 1*(x==b4)
  
  l <- p0* f0 + p1 * f1 + p2 * f2 + p3 * f3 + p4 * f4 + p5 * f5
  logl <- log(l)
  return(sum(logl))
}

# Maximise Likelihood -----------------
i=0
filename_data <- paste0("game1_1120.csv")
beauty_data_full<-read.csv(filename_data, header = TRUE)
beauty_data_full$y <- beauty_data_full$response
beauty_data_full$treatment_combinations <- "game1"
  unique_treatments <- unique(beauty_data_full$treatment_combinations)
  output_game1 <- matrix(NA,length(unique_treatments),6)
  colnames(output_game1)<-c("LLM", "tau","treatment","temperature","ci_lower_tau", "ci_upper_tau")
  sf <- 0.0001
for (treatment in unique_treatments) {
  i=i+1
  beauty_data <- beauty_data_full[beauty_data_full$treatment_combinations == treatment,]
  initial_params <- c(1)
  result <- maxLik(logLik = log_likelihood_ch_mrg, start = initial_params, x = beauty_data$y, method = "BFGS")
  optimized_params <- result$estimate
  params <- exp(optimized_params)
  results<-matrix(0,1,1)
  output_game1[i,1]<-llm
  output_game1[i,2]<-params
  output_game1[i,3]<-treatment
  output_game1[i,4]<-"NA"
  n_bootstrap <- 1000
  bootstrap_estimates <- matrix(NA, nrow = n_bootstrap, ncol = length(optimized_params))

  # Bootstrap for SEs
  bootstrap_estimates <- pblapply(1:n_bootstrap, function(b) {
    bootstrap_sample <- beauty_data[sample(nrow(beauty_data), replace = TRUE), ]
    tryCatch({
      bootstrap_result <- maxLik(logLik = log_likelihood_ch_mrg, start = initial_params, x = bootstrap_sample$y, method = "BFGS")
      return(bootstrap_result$estimate)
    }, error = function(e) {
      return(rep(NA, length(optimized_params)))
    })
  }, cl = num_cores)
  
  bootstrap_estimates <- do.call(rbind, bootstrap_estimates)
  tau_estimates <- exp(bootstrap_estimates[, 1])
  
  ci_lower_tau <- quantile(tau_estimates, probs = 0.05, na.rm = TRUE)
  ci_upper_tau <- quantile(tau_estimates, probs = 0.95, na.rm = TRUE)
  output_game1[i, "ci_lower_tau"] <- ci_lower_tau
  output_game1[i, "ci_upper_tau"] <- ci_upper_tau
  
}
  filename_table <- paste0("/Users/gavin/Library/CloudStorage/Dropbox/Current Research/Bounded Rationality of AI/Estimation/Datasets from papers/Datasets/mrg/output_game1_ch.csv")
  write.csv(output_game1, file = filename_table, row.names = TRUE)
  i=0
  
  print("done game 1")
  
  
  # Maximise Likelihood -----------------
  i=0
  filename_data <- paste0("game3_1120.csv")
  beauty_data_full<-read.csv(filename_data, header = TRUE)
  beauty_data_full$y <- beauty_data_full$response
  beauty_data_full$treatment_combinations <- "game3"
  unique_treatments <- unique(beauty_data_full$treatment_combinations)
  output_game3 <- matrix(NA,length(unique_treatments),6)
  colnames(output_game3)<-c("LLM", "tau","treatment","temperature","ci_lower_tau", "ci_upper_tau")
  sf <- 0.0001
  for (treatment in unique_treatments) {
    i=i+1
    beauty_data <- beauty_data_full[beauty_data_full$treatment_combinations == treatment,]
    initial_params <- c(1)
    result <- maxLik(logLik = log_likelihood_ch_mrg, start = initial_params, x = beauty_data$y, method = "BFGS")
    optimized_params <- result$estimate
    params <- exp(optimized_params)
    results<-matrix(0,1,1)
    output_game3[i,1]<-llm
    output_game3[i,2]<-params
    output_game3[i,3]<-treatment
    output_game3[i,4]<-"NA"
    n_bootstrap <- 1000
    bootstrap_estimates <- matrix(NA, nrow = n_bootstrap, ncol = length(optimized_params))
    
    # Bootstrap for SEs
    bootstrap_estimates <- pblapply(1:n_bootstrap, function(b) {
      bootstrap_sample <- beauty_data[sample(nrow(beauty_data), replace = TRUE), ]
      tryCatch({
        bootstrap_result <- maxLik(logLik = log_likelihood_ch_mrg, start = initial_params, x = bootstrap_sample$y, method = "BFGS")
        return(bootstrap_result$estimate)
      }, error = function(e) {
        return(rep(NA, length(optimized_params)))
      })
    }, cl = num_cores)
    
    bootstrap_estimates <- do.call(rbind, bootstrap_estimates)
    tau_estimates <- exp(bootstrap_estimates[, 1])
    
    ci_lower_tau <- quantile(tau_estimates, probs = 0.05, na.rm = TRUE)
    ci_upper_tau <- quantile(tau_estimates, probs = 0.95, na.rm = TRUE)
    output_game3[i, "ci_lower_tau"] <- ci_lower_tau
    output_game3[i, "ci_upper_tau"] <- ci_upper_tau
    
  }
  #filename_table <- paste0("") choose directory
  write.csv(output_game3, file = filename_table, row.names = TRUE)
  i=0
  
  print("done game 3")
  
  
  
  
  


