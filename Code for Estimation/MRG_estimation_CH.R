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
num_cores <- detectCores() - 1

# Defining the log-likelihood function -----------------
log_likelihood_ch_mrg <- function(params, x) {
  exp_params <- exp(params[1])
  p0 <- exp(-exp_params[1])
  p1 <- p0 * exp_params[1] / 1
  p2 <- p1 * exp_params[1] / 2
  p3 <- p2 * exp_params[1] / 3
  p4 <- p3 * exp_params[1] / 4
  p5 <- 1 - p0 - p1 - p2 - p3 - p4
  
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

# Maximise Likelihood for Game 3-----------------
i=0
LLMs<-c("chatgpt4","chatgpto1","claude2","claude3","gemini1","gemini2")
for (llm in LLMs){
  print(llm)
  filename_data <- paste0("Data files/game3_1120_",llm, ".csv")
  beauty_data_full<-read.csv(filename_data, header = TRUE)
  beauty_data_full$y <- beauty_data_full$response
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$response != "uncomfortable")
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$response != "gender bias")
  beauty_data_full$treatment_combinations <- paste(beauty_data_full$act_as, beauty_data_full$play_against, beauty_data_full$temp, sep = "_")
  unique_treatments <- unique(beauty_data_full$treatment_combinations)
  output_game3 <- matrix(NA,length(unique_treatments),6)
  colnames(output_game3)<-c("LLM", "tau","treatment","temperature","ci_lower_tau", "ci_upper_tau")
  sf <- 0.0001
for (treatment in unique_treatments) {
  i=i+1
  beauty_data <- beauty_data_full[beauty_data_full$treatment_combinations == treatment,]
  initial_params <- c(0.1)
  result <- maxLik(logLik = log_likelihood_ch_mrg, start = initial_params, x = beauty_data$y, method = "BFGS")
  optimized_params <- result$estimate
  params <- exp(optimized_params)
  results<-matrix(0,1,1)
  output_game3[i,1]<-llm
  output_game3[i,2]<-params
  output_game3[i,3]<-treatment
  output_game3[i,4]<-unique(beauty_data$temp)
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
  #filename_table <- paste0("", llm, "/output_game3_CH.csv") choose own directory
  write.csv(output_game3, file = filename_table, row.names = TRUE)
  i=0
  }
print("done")

# Combine CSV Files
combined_output <- NULL 
for (llm in LLMs) {
  #filename_table <- paste0("", llm, "/output_game3_CH.csv") choose own directory
  llm_data <- read.csv(filename_table, header = TRUE)
  if (is.null(combined_output)) {
    combined_output <- llm_data
  } else {
    combined_output <- rbind(combined_output, llm_data)
  }
}

#combined_filename <- "" choose own directory
write.csv(combined_output, file = combined_filename, row.names = FALSE)


# Maximise Likelihood for Game 1-----------------
i=0
LLMs<-c("chatgpt4","chatgpto1","claude2","claude3","gemini1","gemini2")
for (llm in LLMs){
  print(llm)
  filename_data <- paste0("Data files/game1_1120_",llm, ".csv")
  beauty_data_full<-read.csv(filename_data, header = TRUE)
  beauty_data_full$y <- beauty_data_full$response
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$response != "uncomfortable")
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$response != "gender bias")
  beauty_data_full$treatment_combinations <- paste(beauty_data_full$act_as, beauty_data_full$play_against, beauty_data_full$temp, sep = "_")
  unique_treatments <- unique(beauty_data_full$treatment_combinations)
  output_game1 <- matrix(NA,length(unique_treatments),6)
  colnames(output_game1)<-c("LLM", "tau","treatment","temperature","ci_lower_tau", "ci_upper_tau")
  sf <- 0.0001
  for (treatment in unique_treatments) {
    i=i+1
    beauty_data <- beauty_data_full[beauty_data_full$treatment_combinations == treatment,]
    initial_params <- c(0.1)
    result <- maxLik(logLik = log_likelihood_ch_mrg, start = initial_params, x = beauty_data$y, method = "BFGS")
    optimized_params <- result$estimate
    params <- exp(optimized_params)
    results<-matrix(0,1,1)
    output_game1[i,1]<-llm
    output_game1[i,2]<-params
    output_game1[i,3]<-treatment
    output_game1[i,4]<-unique(beauty_data$temp)
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
  # filename_table <- paste0("", llm, "/output_game1_CH.csv") choose own directory
  write.csv(output_game1, file = filename_table, row.names = TRUE)
  i=0
}
print("done")

# Combine CSV Files
combined_output <- NULL 
for (llm in LLMs) {
  #filename_table <- paste0("", llm, "/output_game1_CH.csv") choose own directory
  llm_data <- read.csv(filename_table, header = TRUE)
  if (is.null(combined_output)) {
    combined_output <- llm_data
  } else {
    combined_output <- rbind(combined_output, llm_data)
  }
}

#combined_filename <- "" choose own directory
write.csv(combined_output, file = combined_filename, row.names = FALSE)

# Create summary of results

games <- c("game1","game3")
summary <- NULL 
for (game in games) {
  #filename_table <- paste0("//",game,"/output_",game,"_CH_allLLMs.csv") choose own directory
  llm_data <- read.csv(filename_table, header = TRUE)
  llm_data <- cbind(game,llm_data)
  if (is.null(summary)) {
    summary <- llm_data
  } else {
    summary <- rbind(summary, llm_data)
  }
}

#combined_filename <- "" choose own directory
write.csv(summary, file = combined_filename, row.names = FALSE)





