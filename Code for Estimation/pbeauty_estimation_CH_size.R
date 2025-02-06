# -Initialize--------------------------------------------------------------------
rm(list=ls())
library(msm)
library(parallel)
library(pbapply)
library(alabama)
library(ggplot2)
library(extrafont) 
library(maxLik)

#setwd("") // Set working directory of your choosing
LLMs<-c("chatgpt4","chatgpto1","claude2","claude3","gemini1","gemini2")
num_cores <- detectCores() - 1

# Defining the log-likelihood function -----------------

log_likelihood_ch <- function(params, x) {
  
  exp_params <- exp(params[1])
  
  p0 <- exp(-exp_params[1])
  p1 <- p0 * exp_params[1] / 1
  p2 <- p1 * exp_params[1] / 2
  p3 <- p2 * exp_params[1] / 3
  p4 <- p3 * exp_params[1] / 4
  p5 <- 1 - p0 - p1 - p2 - p3 - p4
  sf <- 0.0001 # scaling factor for MLE
  
  target_proportion <- 2/3
  
  b0 <- 50
  b1 <- target_proportion * b0
  b2 <- target_proportion * (p1 * b1 + p0 * b0) / (p1 + p0)
  b3 <- target_proportion * (p2 * b2 + p1 * b1 + p0 * b0) / (p2 + p1 + p0)
  b4 <- target_proportion * (p3 * b3 + p2 * b2 + p1 * b1 + p0 * b0) / (p3 + p2 + p1 + p0)
  n <- exp(sf*params[2])+b1
  
  f0 <- 1/101
  f1 <- dbinom(x, round(n), round(50*(2/3)^1)/round(n))
  f2 <- dbinom(x, round(n), round(50*(2/3)^2)/round(n))
  f3 <- dbinom(x, round(n), round(50*(2/3)^3)/round(n))
  f4 <- dbinom(x, round(n), round(50*(2/3)^4)/round(n))
  f5 <- dbinom(x, round(n), 1/(n))
  
  l <- p0 * f0 + p1 * f1 + p2 * f2 + p3 * f3 + p4 * f4 + p5 * f5
  logl <- log(l)
  return(sum(logl))
}

# Maximise Likelihood -----------------
i=0
for (llm in LLMs){
  print(llm)
  filename_data <- paste0("Data files/bcg_1shot_size_",llm, ".csv")
  beauty_data_full<-read.csv(filename_data, header = TRUE)
  beauty_data_full$y <- beauty_data_full$response
  beauty_data_full$treatment_combinations <- paste(beauty_data_full$size, beauty_data_full$temp, sep = "_")
  unique_treatments <- unique(beauty_data_full$treatment_combinations)
  output_size <- matrix(NA,length(unique_treatments),14)
  colnames(output_size)<-c("LLM","tau","var1", "var2", "var3", "var4", "var5", "n", "treatment","temperature","ci_lower_tau", "ci_upper_tau", "ci_lower_n", "ci_upper_n")
  sf <- 0.0001
for (treatment in unique_treatments) {
  i=i+1
  beauty_data <- beauty_data_full[beauty_data_full$treatment_combinations == treatment,]
  initial_params <- c(0.1,0)
  result <- maxLik(logLik = log_likelihood_ch, start = initial_params, x = beauty_data$y, method = "BFGS")
  optimized_params <- result$estimate
  params <- exp(optimized_params[1])
  target_proportion <- 2/3
  p0 <- exp(-params[1])
  p1 <- p0 * params[1] / 1
  p2 <- p1 * params[1] / 2
  p3 <- p2 * params[1] / 3
  p4 <- p3 * params[1] / 4
  b0 <- 50
  b1 <- target_proportion * b0
  b2 <- target_proportion * (p1 * b1 + p0 * b0) / (p1 + p0)
  b3 <- target_proportion * (p2 * b2 + p1 * b1 + p0 * b0) / (p2 + p1 + p0)
  b4 <- target_proportion * (p3 * b3 + p2 * b2 + p1 * b1 + p0 * b0) / (p3 + p2 + p1 + p0)
  n <- exp(sf*optimized_params[2])+round(b1)
  var1 <- n * (round(b1)/(n)) * (1-round(b1)/(n))
  var2 <- n * (round(b2)/(n)) * (1-round(b2)/(n))
  var3 <- n * (round(b3)/(n)) * (1-round(b3)/(n))
  var4 <- n * (round(b4)/(n)) * (1-round(b4)/(n))
  var5 <- n * (1/n) * (1 - 1/n)
  results<-matrix(0,1,7)
  results[1,]<-c(params,var1, var2,var3,var4,var5,n)
  colnames(results)<-c("tau","var1", "var2", "var3", "var4", "var5", "n")
  output_size[i,1]<-llm
  output_size[i,2:8]<-results
  output_size[i,9]<-treatment
  output_size[i,10]<-unique(beauty_data$temp)
  n_bootstrap <- 1000
  bootstrap_estimates <- matrix(NA, nrow = n_bootstrap, ncol = length(optimized_params))

  # Bootstrap for SEs
  bootstrap_estimates <- pblapply(1:n_bootstrap, function(b) {
    bootstrap_sample <- beauty_data[sample(nrow(beauty_data), replace = TRUE), ]
    tryCatch({
      bootstrap_result <- maxLik(logLik = log_likelihood_ch, start = initial_params, x = bootstrap_sample$y, method = "BFGS")
      return(bootstrap_result$estimate)
    }, error = function(e) {
      return(rep(NA, length(optimized_params)))
    })
  }, cl = num_cores)
  
  bootstrap_estimates <- do.call(rbind, bootstrap_estimates)
  tau_estimates <- exp(bootstrap_estimates[, 1])
  n_estimates <- exp(sf * bootstrap_estimates[, 2]) + round(b1) 
  
  ci_lower_tau <- quantile(tau_estimates, probs = 0.05, na.rm = TRUE)
  ci_upper_tau <- quantile(tau_estimates, probs = 0.95, na.rm = TRUE)
  ci_lower_n <- quantile(n_estimates, probs = 0.05, na.rm = TRUE)
  ci_upper_n <- quantile(n_estimates, probs = 0.95, na.rm = TRUE)
  
  output_size[i, "ci_lower_tau"] <- ci_lower_tau
  output_size[i, "ci_upper_tau"] <- ci_upper_tau
  output_size[i, "ci_lower_n"] <- ci_lower_n
  output_size[i, "ci_upper_n"] <- ci_upper_n
  
}
#filename_table <- paste0("", llm, "/size/output_size_CH.csv") Set directory of your own choosing
write.csv(output_size, file = filename_table, row.names = TRUE)
i=0
}
print("estimation completed")

# Combine CSV Files
combined_output <- NULL 
for (llm in LLMs) {
  #filename_table <- paste0("", llm, "/size/output_size_CH.csv")Set directory of your own choosing
  llm_data <- read.csv(filename_table, header = TRUE)
  if (is.null(combined_output)) {
    combined_output <- llm_data
  } else {
    combined_output <- rbind(combined_output, llm_data)
  }
}

#combined_filename <- "" Set directory of your own choosing
write.csv(combined_output, file = combined_filename, row.names = FALSE)


