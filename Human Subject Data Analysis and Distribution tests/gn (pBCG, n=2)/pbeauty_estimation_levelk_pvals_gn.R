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

# Defining the log-likelihood function -----------------
log_likelihood_abs_23 <- function(params, x) {
  exp_params <- exp(params[1:5])
  p1 <- exp_params[1] / (1 + sum(exp_params))
  p2 <- exp_params[2] / (1 + sum(exp_params))
  p3 <- exp_params[3] / (1 + sum(exp_params))
  p4 <- exp_params[4] / (1 + sum(exp_params))
  p5 <- exp_params[5] / (1 + sum(exp_params))
  sf <- 0.0001  # scaling factor for MLE
  n <- exp(sf*params[6])+round(50*(2/3)^1)
  
  f0 <- 1/101
  f1 <- dbinom(x, round(n), round(50*(2/3)^1)/round(n))
  f2 <- dbinom(x, round(n), round(50*(2/3)^2)/round(n))
  f3 <- dbinom(x, round(n), round(50*(2/3)^3)/round(n))
  f4 <- dbinom(x, round(n), round(50*(2/3)^4)/round(n))
  f5 <- dbinom(x, round(n), 1/(n))
  
  l <- (1 - p1 - p2 - p3 - p4 - p5) * f0 + p1 * f1 + p2 * f2 + p3 * f3 + p4 * f4 + p5 * f5
  logl <- log(l)
  return(sum(logl))
}

log_likelihood_abs_12 <- function(params, x) {
  exp_params <- exp(params[1:5])
  p1 <- exp_params[1] / (1 + sum(exp_params))
  p2 <- exp_params[2] / (1 + sum(exp_params))
  p3 <- exp_params[3] / (1 + sum(exp_params))
  p4 <- exp_params[4] / (1 + sum(exp_params))
  p5 <- exp_params[5] / (1 + sum(exp_params))
  sf <- 0.0001
  n <- exp(sf*params[6])+round(50*(1/2)^1)
  
  f0 <- 1/101
  f1 <- dbinom(x, round(n), round(50*(1/2)^1)/round(n))
  f2 <- dbinom(x, round(n), round(50*(1/2)^2)/round(n))
  f3 <- dbinom(x, round(n), round(50*(1/2)^3)/round(n))
  f4 <- dbinom(x, round(n), round(50*(1/2)^4)/round(n))
  f5 <- dbinom(x, round(n), 1/(n))
  
  l <- (1 - p1 - p2 - p3 - p4 - p5) * f0 + p1 * f1 + p2 * f2 + p3 * f3 + p4 * f4 + p5 * f5
  logl <- log(l)
  return(sum(logl))
}

log_likelihood_abs_43 <- function(params, x) {
  exp_params <- exp(params[1:3])
  p1 <- exp_params[1] / (1 + sum(exp_params))
  p2 <- exp_params[2] / (1 + sum(exp_params))
  p3 <- exp_params[3] / (1 + sum(exp_params))
  sf <- 0.0001
  n <- exp(sf*params[4])+100
  
  f0 <- 1/101
  f1 <- dbinom(x, round(n), round(50*(4/3)^1)/round(n))
  f2 <- dbinom(x, round(n), round(50*(4/3)^2)/round(n))
  f3 <- dbinom(x, round(n), min(round(50*(4/3)^3),100)/round(n))
  
  l <- (1 - p1 - p2 - p3) * f0 + p1 * f1 + p2 * f2 + p3 * f3 
  logl <- log(l)
  return(sum(logl))
}

# Maximise Likelihood -----------------
  llm <- "gn"
  i<-0
  filename_data <- paste0("distribution_tests/BCG_gn.csv")
  beauty_data_full<-read.csv(filename_data, header = TRUE)
  beauty_data_full$response<-round(beauty_data_full$response)
  beauty_data_full$y <- beauty_data_full$response
  beauty_data_full$p_val[beauty_data_full$p_val=="2-Jan"] <- "onehalf"
  beauty_data_full$p_val[beauty_data_full$p_val=="3-Feb"] <- "twothirds"
  beauty_data_full$p_val[beauty_data_full$p_val=="3-Apr"] <- "fourthirds"
  beauty_data_full$p_val[beauty_data_full$p_val=="1/2"] <- "onehalf"
  beauty_data_full$p_val[beauty_data_full$p_val=="2/3"] <- "twothirds"
  beauty_data_full$p_val[beauty_data_full$p_val=="4/3"] <- "fourthirds"
  beauty_data_full$treatment_combinations <- paste(beauty_data_full$p_val, beauty_data_full$temp, sep = "_")
  unique_treatments <- unique(beauty_data_full$treatment_combinations)
  output_pvals <- matrix(NA,length(unique_treatments),29)
  colnames(output_pvals)<-c("dataset", "p0","p1","p2","p3","p4","p5","var1", "var2", "var3", "var4", "var5", "n", "treatment","temperature","ci_lower_p0","ci_upper_p0","ci_lower_p1","ci_upper_p1","ci_lower_p2","ci_upper_p2","ci_lower_p3","ci_upper_p3","ci_lower_p4","ci_upper_p4","ci_lower_p5","ci_upper_p5","ci_lower_n","ci_upper_n")
  sf <- 0.0001
  for (treatment in unique_treatments) {
  i=i+1
    beauty_data <- beauty_data_full[beauty_data_full$treatment_combinations == treatment,]
    if (unique(beauty_data$p_val)=="twothirds"){
      initial_params <- c(0.1, 0.1, 0.1, 0.1, 0.1,0)
      result <- maxLik(logLik = log_likelihood_abs_23, start = initial_params, x = beauty_data$y, method = "BFGS")
      optimized_params <- result$estimate
      params <- exp(optimized_params[1:5]) / (1 + sum(exp(optimized_params[1:5])))
      n <- round(exp(optimized_params[6])+round(50*(2/3)^1))
      var1 <- n * (round(50*(2/3)^1)/(n)) * (1-round(50*(2/3)^1)/(n))
      var2 <- n * (round(50*(2/3)^2)/(n)) * (1-round(50*(2/3)^2)/(n))
      var3 <- n * (round(50*(2/3)^3)/(n)) * (1-round(50*(2/3)^3)/(n))
      var4 <- n * (round(50*(2/3)^4)/(n)) * (1-round(50*(2/3)^4)/(n))
      var5 <- n * (1/n) * (1 - 1/n)
      p0 <- 1 - sum(params)
      results<-matrix(0,1,12)
      results[1,]<-c(p0,params,var1, var2,var3,var4,var5,n)
      colnames(results)<-c("p0","p1","p2","p3","p4","p5","var1", "var2", "var3", "var4", "var5", "n")
      output_pvals[i,1]<-llm
      output_pvals[i,2:13]<-results
      output_pvals[i,14]<-treatment
      output_pvals[i,15]<-"NA"
      
      
      n_bootstrap <- 1000
      bootstrap_estimates <- matrix(NA, nrow = n_bootstrap, ncol = length(optimized_params))
      
      # Bootstrap for SEs
      bootstrap_estimates <- pblapply(1:n_bootstrap, function(b) {
        bootstrap_sample <- beauty_data[sample(nrow(beauty_data), replace = TRUE), ]
        tryCatch({
          bootstrap_result <- maxLik(logLik = log_likelihood_abs_23, start = initial_params, x = bootstrap_sample$y, method = "BFGS")
          return(bootstrap_result$estimate)
        }, error = function(e) {
          return(rep(NA, length(optimized_params)))
        })
      }, cl = num_cores)
      
      bootstrap_estimates <- do.call(rbind, bootstrap_estimates)
      p1_estimates <- exp(bootstrap_estimates[,1]) / (1 + rowSums(exp(bootstrap_estimates[,1:5])))
      p2_estimates <- exp(bootstrap_estimates[,2]) / (1 + rowSums(exp(bootstrap_estimates[,1:5])))
      p3_estimates <- exp(bootstrap_estimates[,3]) / (1 + rowSums(exp(bootstrap_estimates[,1:5])))
      p4_estimates <- exp(bootstrap_estimates[,4]) / (1 + rowSums(exp(bootstrap_estimates[,1:5])))
      p5_estimates <- exp(bootstrap_estimates[,5]) / (1 + rowSums(exp(bootstrap_estimates[,1:5])))
      p1to5_estimates <- cbind(p1_estimates,p2_estimates,p3_estimates,p4_estimates,p5_estimates)
      p0_estimates <- 1 - rowSums(p1to5_estimates)
      n_estimates <- exp(sf*bootstrap_estimates[6])+round(50*(2/3)^1)
      
      ci_lower_p1 <- quantile(p1_estimates, probs = 0.05, na.rm = TRUE)
      ci_upper_p1 <- quantile(p1_estimates, probs = 0.95, na.rm = TRUE)
      ci_lower_p2 <- quantile(p2_estimates, probs = 0.05, na.rm = TRUE)
      ci_upper_p2 <- quantile(p2_estimates, probs = 0.95, na.rm = TRUE)
      ci_lower_p3 <- quantile(p3_estimates, probs = 0.05, na.rm = TRUE)
      ci_upper_p3 <- quantile(p3_estimates, probs = 0.95, na.rm = TRUE)
      ci_lower_p4 <- quantile(p4_estimates, probs = 0.05, na.rm = TRUE)
      ci_upper_p4 <- quantile(p4_estimates, probs = 0.95, na.rm = TRUE)
      ci_lower_p5 <- quantile(p5_estimates, probs = 0.05, na.rm = TRUE)
      ci_upper_p5 <- quantile(p5_estimates, probs = 0.95, na.rm = TRUE)
      ci_lower_p0 <- quantile(p0_estimates, probs = 0.05, na.rm = TRUE)
      ci_upper_p0 <- quantile(p0_estimates, probs = 0.95, na.rm = TRUE)
      ci_lower_n <- quantile(n_estimates, probs = 0.05, na.rm = TRUE)
      ci_upper_n <- quantile(n_estimates, probs = 0.95, na.rm = TRUE)
      
      output_pvals[i, "ci_lower_p1"] <- ci_lower_p1
      output_pvals[i, "ci_upper_p1"] <- ci_upper_p1
      output_pvals[i, "ci_lower_p2"] <- ci_lower_p2
      output_pvals[i, "ci_upper_p2"] <- ci_upper_p2
      output_pvals[i, "ci_lower_p3"] <- ci_lower_p3
      output_pvals[i, "ci_upper_p3"] <- ci_upper_p3
      output_pvals[i, "ci_lower_p4"] <- ci_lower_p4
      output_pvals[i, "ci_upper_p4"] <- ci_upper_p4
      output_pvals[i, "ci_lower_p5"] <- ci_lower_p5
      output_pvals[i, "ci_upper_p5"] <- ci_upper_p5
      output_pvals[i, "ci_lower_p0"] <- ci_lower_p0
      output_pvals[i, "ci_upper_p0"] <- ci_upper_p0
      output_pvals[i, "ci_lower_n"] <- ci_lower_n
      output_pvals[i, "ci_upper_n"] <- ci_upper_n
      # Computing posterior probabilities based on the optimized parameters
      compute_posteriors <- function(p, x) {
        p1 <- p[2]
        p2 <- p[3]
        p3 <- p[4]
        p4 <- p[5]
        p5 <- p[6]
        n <- p[12]
        
        f0 <- 1/101
        f1 <- dbinom(x, round(n), round(50*(2/3)^1)/round(n))
        f2 <- dbinom(x, round(n), round(50*(2/3)^2)/round(n))
        f3 <- dbinom(x, round(n), round(50*(2/3)^3)/round(n))
        f4 <- dbinom(x, round(n), round(50*(2/3)^4)/round(n))
        f5 <- dbinom(x, round(n), 1/(n))
        
        l <- (1 - p1 - p2 - p3 - p4 - p5) * f0 + p1 * f1 + p2 * f2 + p3 * f3 + p4 * f4 + p5 * f5
        
        postp0 <- (1 - p1 - p2 - p3 - p4 - p5) * f0 / l
        postp1 <- p1 * f1 / l
        postp2 <- p2 * f2 / l
        postp3 <- p3 * f3 / l
        postp4 <- p4 * f4 / l
        postp5 <- p5 * f5 / l
        
        return(list(postp0 = postp0, postp1 = postp1, postp2 = postp2, 
                    postp3 = postp3, postp4 = postp4, postp5 = postp5))
      }
      posteriors <- compute_posteriors(results, beauty_data$response)
      posterior <- matrix()
      posterior <- cbind(beauty_data$response,as.matrix(posteriors$postp0),as.matrix(posteriors$postp1),as.matrix(posteriors$postp2),as.matrix(posteriors$postp3),as.matrix(posteriors$postp4),as.matrix(posteriors$postp5))
      colnames(posterior)<-c("response", "p0","p1","p2","p3","p4","p5")
      
      posterior_df <- as.data.frame(posterior)
      ggplot(posterior_df, aes(x = response)) +
        geom_line(aes(y = p0, color = "p0", linetype = "p0")) +
        geom_point(aes(y = p0, color = "p0", shape = "p0"), size= 1.1) +
        geom_line(aes(y = p1, color = "p1", linetype = "p1")) +
        geom_point(aes(y = p1, color = "p1", shape = "p1"), size= 1.1) +
        geom_line(aes(y = p2, color = "p2", linetype = "p2")) +
        geom_point(aes(y = p2, color = "p2", shape = "p2"), size= 1.1) +
        geom_line(aes(y = p3, color = "p3", linetype = "p3")) +
        geom_point(aes(y = p3, color = "p3", shape = "p3"), size= 1.1) +
        geom_line(aes(y = p4, color = "p4", linetype = "p4")) +
        geom_point(aes(y = p4, color = "p4", shape = "p4"), size= 1.1) +
        geom_line(aes(y = p5, color = "p5", linetype = "p5")) +
        geom_point(aes(y = p5, color = "p5", shape = "p5"), size= 1.1) +
        labs(y = "Posterior Probability", x = "Responses", color = "", linetype = "", shape = "") +
        scale_color_manual(values = c("p0" = "black", "p1" = "red", "p2" = "blue", "p3" = "forestgreen", "p4" = "orange", "p5" = "purple")) +
        scale_linetype_manual(values = c("p0" = "solid", "p1" = "solid", "p2" = "solid", "p3" = "solid", "p4" = "solid", "p5" = "solid")) +
        scale_shape_manual(values = c("p0" = 1, "p1" = 2, "p2" = 8, "p3" = 4, "p4" = 5, "p5" = 6)) +
        scale_x_continuous(breaks = seq(0, 100, by = 5), limits = c(0, 100), expand = c(0.01, 0.01)) +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.10), expand = c(0.005, 0.005)) +
        theme_minimal() +
        theme(panel.grid.major.x = element_line(), panel.grid.minor.x = element_blank(), text = element_text(size= 12, family = "LM Roman 10"), legend.position = c(.95, .5),legend.box.background = element_rect(color=rgb(0,0,0,0), fill = rgb(0,0,0,0.1), size=0.6),legend.box.margin = margin(-16-5, -5, -5, -5), plot.margin = margin(t = 20, r = 20, b = 20, l = 20),  axis.title.x = element_text(margin = margin(t = 10)),
              axis.title.y = element_text(margin = margin(r = 10)),)
      
      filename_graph <- paste0("gn/BCG_gn_",treatment, ".pdf")
      ggsave(filename_graph, width = 8, height = 4, units = "in", device = "pdf")
    }
    if (unique(beauty_data$p_val)=="onehalf"){
      initial_params <- c(0.1, 0.1, 0.1, 0.1, 0.1,0)
      result <- maxLik(logLik = log_likelihood_abs_12, start = initial_params, x = beauty_data$y, method = "BFGS")
      optimized_params <- result$estimate
      params <- exp(optimized_params[1:5]) / (1 + sum(exp(optimized_params[1:5])))
      n <- round(exp(optimized_params[6])+round(50*(1/2)^1))
      var1 <- n * (round(50*(1/2)^1)/(n)) * (1-round(50*(1/2)^1)/(n))
      var2 <- n * (round(50*(1/2)^2)/(n)) * (1-round(50*(1/2)^2)/(n))
      var3 <- n * (round(50*(1/2)^3)/(n)) * (1-round(50*(1/2)^3)/(n))
      var4 <- n * (round(50*(1/2)^4)/(n)) * (1-round(50*(1/2)^4)/(n))
      var5 <- n * (1/n) * (1 - 1/n)
      p0 <- 1 - sum(params)
      results<-matrix(0,1,12)
      results[1,]<-c(p0,params,var1, var2,var3,var4,var5,n)
      colnames(results)<-c("p0","p1","p2","p3","p4","p5","var1", "var2", "var3", "var4", "var5", "n")
      output_pvals[i,1]<-llm
      output_pvals[i,2:13]<-results
      output_pvals[i,14]<-treatment
      output_pvals[i,15]<-"NA"
      
      n_bootstrap <- 1000
      bootstrap_estimates <- matrix(NA, nrow = n_bootstrap, ncol = length(optimized_params))
      
      # Bootstrap for SEs
      bootstrap_estimates <- pblapply(1:n_bootstrap, function(b) {
        bootstrap_sample <- beauty_data[sample(nrow(beauty_data), replace = TRUE), ]
        tryCatch({
          bootstrap_result <- maxLik(logLik = log_likelihood_abs_12, start = initial_params, x = bootstrap_sample$y, method = "BFGS")
          return(bootstrap_result$estimate)
        }, error = function(e) {
          return(rep(NA, length(optimized_params)))
        })
      }, cl = num_cores)
      
      bootstrap_estimates <- do.call(rbind, bootstrap_estimates)
      p1_estimates <- exp(bootstrap_estimates[,1]) / (1 + rowSums(exp(bootstrap_estimates[,1:5])))
      p2_estimates <- exp(bootstrap_estimates[,2]) / (1 + rowSums(exp(bootstrap_estimates[,1:5])))
      p3_estimates <- exp(bootstrap_estimates[,3]) / (1 + rowSums(exp(bootstrap_estimates[,1:5])))
      p4_estimates <- exp(bootstrap_estimates[,4]) / (1 + rowSums(exp(bootstrap_estimates[,1:5])))
      p5_estimates <- exp(bootstrap_estimates[,5]) / (1 + rowSums(exp(bootstrap_estimates[,1:5])))
      p1to5_estimates <- cbind(p1_estimates,p2_estimates,p3_estimates,p4_estimates,p5_estimates)
      p0_estimates <- 1 - rowSums(p1to5_estimates)
      n_estimates <- exp(sf*bootstrap_estimates[6])+round(50*(1/2)^1)
      
      ci_lower_p1 <- quantile(p1_estimates, probs = 0.05, na.rm = TRUE)
      ci_upper_p1 <- quantile(p1_estimates, probs = 0.95, na.rm = TRUE)
      ci_lower_p2 <- quantile(p2_estimates, probs = 0.05, na.rm = TRUE)
      ci_upper_p2 <- quantile(p2_estimates, probs = 0.95, na.rm = TRUE)
      ci_lower_p3 <- quantile(p3_estimates, probs = 0.05, na.rm = TRUE)
      ci_upper_p3 <- quantile(p3_estimates, probs = 0.95, na.rm = TRUE)
      ci_lower_p4 <- quantile(p4_estimates, probs = 0.05, na.rm = TRUE)
      ci_upper_p4 <- quantile(p4_estimates, probs = 0.95, na.rm = TRUE)
      ci_lower_p5 <- quantile(p5_estimates, probs = 0.05, na.rm = TRUE)
      ci_upper_p5 <- quantile(p5_estimates, probs = 0.95, na.rm = TRUE)
      ci_lower_p0 <- quantile(p0_estimates, probs = 0.05, na.rm = TRUE)
      ci_upper_p0 <- quantile(p0_estimates, probs = 0.95, na.rm = TRUE)
      ci_lower_n <- quantile(n_estimates, probs = 0.05, na.rm = TRUE)
      ci_upper_n <- quantile(n_estimates, probs = 0.95, na.rm = TRUE)
      
      output_pvals[i, "ci_lower_p1"] <- ci_lower_p1
      output_pvals[i, "ci_upper_p1"] <- ci_upper_p1
      output_pvals[i, "ci_lower_p2"] <- ci_lower_p2
      output_pvals[i, "ci_upper_p2"] <- ci_upper_p2
      output_pvals[i, "ci_lower_p3"] <- ci_lower_p3
      output_pvals[i, "ci_upper_p3"] <- ci_upper_p3
      output_pvals[i, "ci_lower_p4"] <- ci_lower_p4
      output_pvals[i, "ci_upper_p4"] <- ci_upper_p4
      output_pvals[i, "ci_lower_p5"] <- ci_lower_p5
      output_pvals[i, "ci_upper_p5"] <- ci_upper_p5
      output_pvals[i, "ci_lower_p0"] <- ci_lower_p0
      output_pvals[i, "ci_upper_p0"] <- ci_upper_p0
      output_pvals[i, "ci_lower_n"] <- ci_lower_n
      output_pvals[i, "ci_upper_n"] <- ci_upper_n
      # Computing posterior probabilities based on the optimized parameters
      compute_posteriors <- function(p, x) {
        p1 <- p[2]
        p2 <- p[3]
        p3 <- p[4]
        p4 <- p[5]
        p5 <- p[6]
        n <- p[12]
        
        f0 <- 1/101
        f1 <- dbinom(x, round(n), round(50*(1/2)^1)/round(n))
        f2 <- dbinom(x, round(n), round(50*(1/2)^2)/round(n))
        f3 <- dbinom(x, round(n), round(50*(1/2)^3)/round(n))
        f4 <- dbinom(x, round(n), round(50*(1/2)^4)/round(n))
        f5 <- dbinom(x, round(n), 1/(n))
        
        l <- (1 - p1 - p2 - p3 - p4 - p5) * f0 + p1 * f1 + p2 * f2 + p3 * f3 + p4 * f4 + p5 * f5
        
        postp0 <- (1 - p1 - p2 - p3 - p4 - p5) * f0 / l
        postp1 <- p1 * f1 / l
        postp2 <- p2 * f2 / l
        postp3 <- p3 * f3 / l
        postp4 <- p4 * f4 / l
        postp5 <- p5 * f5 / l
        
        return(list(postp0 = postp0, postp1 = postp1, postp2 = postp2, 
                    postp3 = postp3, postp4 = postp4, postp5 = postp5))
      }
      posteriors <- compute_posteriors(results, beauty_data$response)
      posterior <- matrix()
      posterior <- cbind(beauty_data$response,as.matrix(posteriors$postp0),as.matrix(posteriors$postp1),as.matrix(posteriors$postp2),as.matrix(posteriors$postp3),as.matrix(posteriors$postp4),as.matrix(posteriors$postp5))
      colnames(posterior)<-c("response", "p0","p1","p2","p3","p4","p5")
      
      posterior_df <- as.data.frame(posterior)
      ggplot(posterior_df, aes(x = response)) +
        geom_line(aes(y = p0, color = "p0", linetype = "p0")) +
        geom_point(aes(y = p0, color = "p0", shape = "p0"), size= 1.1) +
        geom_line(aes(y = p1, color = "p1", linetype = "p1")) +
        geom_point(aes(y = p1, color = "p1", shape = "p1"), size= 1.1) +
        geom_line(aes(y = p2, color = "p2", linetype = "p2")) +
        geom_point(aes(y = p2, color = "p2", shape = "p2"), size= 1.1) +
        geom_line(aes(y = p3, color = "p3", linetype = "p3")) +
        geom_point(aes(y = p3, color = "p3", shape = "p3"), size= 1.1) +
        geom_line(aes(y = p4, color = "p4", linetype = "p4")) +
        geom_point(aes(y = p4, color = "p4", shape = "p4"), size= 1.1) +
        geom_line(aes(y = p5, color = "p5", linetype = "p5")) +
        geom_point(aes(y = p5, color = "p5", shape = "p5"), size= 1.1) +
        labs(y = "Posterior Probability", x = "Responses", color = "", linetype = "", shape = "") +
        scale_color_manual(values = c("p0" = "black", "p1" = "red", "p2" = "blue", "p3" = "forestgreen", "p4" = "orange", "p5" = "purple")) +
        scale_linetype_manual(values = c("p0" = "solid", "p1" = "solid", "p2" = "solid", "p3" = "solid", "p4" = "solid", "p5" = "solid")) +
        scale_shape_manual(values = c("p0" = 1, "p1" = 2, "p2" = 8, "p3" = 4, "p4" = 5, "p5" = 6)) +
        scale_x_continuous(breaks = seq(0, 100, by = 5), limits = c(0, 100), expand = c(0.01, 0.01)) +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.10), expand = c(0.005, 0.005)) +
        theme_minimal() +
        theme(panel.grid.major.x = element_line(), panel.grid.minor.x = element_blank(), text = element_text(size= 12, family = "LM Roman 10"), legend.position = c(.95, .5),legend.box.background = element_rect(color=rgb(0,0,0,0), fill = rgb(0,0,0,0.1), size=0.6),legend.box.margin = margin(-16-5, -5, -5, -5), plot.margin = margin(t = 20, r = 20, b = 20, l = 20),  axis.title.x = element_text(margin = margin(t = 10)),
              axis.title.y = element_text(margin = margin(r = 10)),)
      
      filename_graph <- paste0("gn/BCG_gn_",treatment, ".pdf")
      ggsave(filename_graph, width = 8, height = 4, units = "in", device = "pdf")
    }
    if (unique(beauty_data$p_val)=="fourthirds"){
      initial_params <- c(0.2, 0.2, 0.2,0)
      result <- maxLik(logLik = log_likelihood_abs_43, start = initial_params, x = beauty_data$y, method = "BFGS")
      optimized_params <- result$estimate
      params <- exp(optimized_params[1:3]) / (1 + sum(exp(optimized_params[1:3])))
      n <- round(exp(optimized_params[4])+100)
      var1 <- n * (round(50*(4/3)^1)/(n)) * (1-round(50*(4/3)^1)/(n))
      var2 <- n * (round(50*(4/3)^2)/(n)) * (1-round(50*(4/3)^2)/(n))
      var3 <- n * min(round(50*(4/3)^3),100)/round(n) * (1-min(round(50*(4/3)^3),100)/round(n))
      p0 <- 1 - sum(params)
      results<-matrix(0,1,12)
      results[1,]<-c(p0,params,0,0,var1, var2,var3,0,0,n)
      colnames(results)<-c("p0","p1","p2","p3","p4","p5","var1", "var2", "var3", "var4", "var5", "n")
      output_pvals[i,1]<-llm
      output_pvals[i,2:13]<-results
      output_pvals[i,14]<-treatment
      output_pvals[i,15]<-"NA"
      
      n_bootstrap <- 1000
      bootstrap_estimates <- matrix(NA, nrow = n_bootstrap, ncol = length(optimized_params))
      
      # Bootstrap for SEs
      bootstrap_estimates <- pblapply(1:n_bootstrap, function(b) {
        bootstrap_sample <- beauty_data[sample(nrow(beauty_data), replace = TRUE), ]
        tryCatch({
          bootstrap_result <- maxLik(logLik = log_likelihood_abs_43, start = initial_params, x = bootstrap_sample$y, method = "BFGS")
          return(bootstrap_result$estimate)
        }, error = function(e) {
          return(rep(NA, length(optimized_params)))
        })
      }, cl = num_cores)
      
      bootstrap_estimates <- do.call(rbind, bootstrap_estimates)
      p1_estimates <- exp(bootstrap_estimates[,1]) / (1 + rowSums(exp(bootstrap_estimates[,1:3])))
      p2_estimates <- exp(bootstrap_estimates[,2]) / (1 + rowSums(exp(bootstrap_estimates[,1:3])))
      p3_estimates <- exp(bootstrap_estimates[,3]) / (1 + rowSums(exp(bootstrap_estimates[,1:3])))
      p1to3_estimates <- cbind(p1_estimates,p2_estimates,p3_estimates)
      p0_estimates <- 1 - rowSums(p1to3_estimates)
      n_estimates <- exp(sf*bootstrap_estimates[4])+100
      
      ci_lower_p1 <- quantile(p1_estimates, probs = 0.05, na.rm = TRUE)
      ci_upper_p1 <- quantile(p1_estimates, probs = 0.95, na.rm = TRUE)
      ci_lower_p2 <- quantile(p2_estimates, probs = 0.05, na.rm = TRUE)
      ci_upper_p2 <- quantile(p2_estimates, probs = 0.95, na.rm = TRUE)
      ci_lower_p3 <- quantile(p3_estimates, probs = 0.05, na.rm = TRUE)
      ci_upper_p3 <- quantile(p3_estimates, probs = 0.95, na.rm = TRUE)
      ci_lower_p4 <- quantile(p4_estimates, probs = 0.05, na.rm = TRUE)
      ci_upper_p4 <- quantile(p4_estimates, probs = 0.95, na.rm = TRUE)
      ci_lower_p5 <- quantile(p5_estimates, probs = 0.05, na.rm = TRUE)
      ci_upper_p5 <- quantile(p5_estimates, probs = 0.95, na.rm = TRUE)
      ci_lower_p0 <- quantile(p0_estimates, probs = 0.05, na.rm = TRUE)
      ci_upper_p0 <- quantile(p0_estimates, probs = 0.95, na.rm = TRUE)
      ci_lower_n <- quantile(n_estimates, probs = 0.05, na.rm = TRUE)
      ci_upper_n <- quantile(n_estimates, probs = 0.95, na.rm = TRUE)
      
      output_pvals[i, "ci_lower_p1"] <- ci_lower_p1
      output_pvals[i, "ci_upper_p1"] <- ci_upper_p1
      output_pvals[i, "ci_lower_p2"] <- ci_lower_p2
      output_pvals[i, "ci_upper_p2"] <- ci_upper_p2
      output_pvals[i, "ci_lower_p3"] <- ci_lower_p3
      output_pvals[i, "ci_upper_p3"] <- ci_upper_p3
      output_pvals[i, "ci_lower_p4"] <- ci_lower_p4
      output_pvals[i, "ci_upper_p4"] <- ci_upper_p4
      output_pvals[i, "ci_lower_p5"] <- ci_lower_p5
      output_pvals[i, "ci_upper_p5"] <- ci_upper_p5
      output_pvals[i, "ci_lower_p0"] <- ci_lower_p0
      output_pvals[i, "ci_upper_p0"] <- ci_upper_p0
      output_pvals[i, "ci_lower_n"] <- ci_lower_n
      output_pvals[i, "ci_upper_n"] <- ci_upper_n
      # Computing posterior probabilities based on the optimized parameters
      compute_posteriors <- function(p, x) {
        p1 <- p[2]
        p2 <- p[3]
        p3 <- p[4]
        n <- p[12]
        
        f0 <- 1/101
        f1 <- dbinom(x, round(n), round(50*(4/3)^1)/round(n))
        f2 <- dbinom(x, round(n), round(50*(4/3)^2)/round(n))
        f3 <- dbinom(x, round(n), min(round(50*(4/3)^3),100)/round(n))
        
        l <- (1 - p1 - p2 - p3) * f0 + p1 * f1 + p2 * f2 + p3 * f3 
        
        postp0 <- (1 - p1 - p2 - p3) * f0 / l
        postp1 <- p1 * f1 / l
        postp2 <- p2 * f2 / l
        postp3 <- p3 * f3 / l
        postp4 <- l / l -1
        postp5 <- l / l -1
        
        return(list(postp0 = postp0, postp1 = postp1, postp2 = postp2, 
                    postp3 = postp3, postp4 = postp4, postp5 = postp5))
      }
      posteriors <- compute_posteriors(results, beauty_data$response)
      posterior <- matrix()
      posterior <- cbind(beauty_data$response,as.matrix(posteriors$postp0),as.matrix(posteriors$postp1),as.matrix(posteriors$postp2),as.matrix(posteriors$postp3),as.matrix(posteriors$postp4),as.matrix(posteriors$postp5))
      colnames(posterior)<-c("response", "p0","p1","p2","p3","p4","p5")
      
      posterior_df <- as.data.frame(posterior)
      ggplot(posterior_df, aes(x = response)) +
        geom_line(aes(y = p0, color = "p0", linetype = "p0")) +
        geom_point(aes(y = p0, color = "p0", shape = "p0"), size= 1.1) +
        geom_line(aes(y = p1, color = "p1", linetype = "p1")) +
        geom_point(aes(y = p1, color = "p1", shape = "p1"), size= 1.1) +
        geom_line(aes(y = p2, color = "p2", linetype = "p2")) +
        geom_point(aes(y = p2, color = "p2", shape = "p2"), size= 1.1) +
        geom_line(aes(y = p3, color = "p3", linetype = "p3")) +
        geom_point(aes(y = p3, color = "p3", shape = "p3"), size= 1.1) +
        labs(y = "Posterior Probability", x = "Responses", color = "", linetype = "", shape = "") +
        scale_color_manual(values = c("p0" = "black", "p1" = "red", "p2" = "blue", "p3" = "forestgreen", "p4" = "orange", "p5" = "purple")) +
        scale_linetype_manual(values = c("p0" = "solid", "p1" = "solid", "p2" = "solid", "p3" = "solid", "p4" = "solid", "p5" = "solid")) +
        scale_shape_manual(values = c("p0" = 1, "p1" = 2, "p2" = 8, "p3" = 4, "p4" = 5, "p5" = 6)) +
        scale_x_continuous(breaks = seq(0, 100, by = 5), limits = c(0, 100), expand = c(0.01, 0.01)) +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.10), expand = c(0.005, 0.005)) +
        theme_minimal() +
        theme(panel.grid.major.x = element_line(), panel.grid.minor.x = element_blank(), text = element_text(size= 12, family = "LM Roman 10"), legend.position = c(.95, .5),legend.box.background = element_rect(color=rgb(0,0,0,0), fill = rgb(0,0,0,0.1), size=0.6),legend.box.margin = margin(-16-5, -5, -5, -5), plot.margin = margin(t = 20, r = 20, b = 20, l = 20),  axis.title.x = element_text(margin = margin(t = 10)),
              axis.title.y = element_text(margin = margin(r = 10)),)
      
      filename_graph <- paste0("gn/BCG_gn_",treatment, ".pdf")
      ggsave(filename_graph, width = 8, height = 4, units = "in", device = "pdf")
    }
  }
  # filename_table <- paste0("") choose directory
  write.csv(output_pvals, file = filename_table, row.names = TRUE)
print("done")


