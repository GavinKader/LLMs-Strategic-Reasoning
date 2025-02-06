# -Initialize--------------------------------------------------------------------
rm(list=ls())
library(msm)
library(parallel)
library(pbapply)
library(alabama)
library(ggplot2)
library(extrafont) 
library(maxLik)
library(ggplot2)
library(dplyr)
library(tidyr)
#setwd("") Set working directory
num_cores <- detectCores() - 1
llm<-"mrg"

# Defining the log-likelihood function -----------------
log_likelihood_gg <- function(params) {
  #responses
  {
    order <- c(1,3,5,7,9,11,13,15,2,4,6,8,10,12,14,16)
    lower_limits_you<-   as.numeric(list("100","100","100","300","300","300","300","300","100","100","300","100","300","100","100","100"))
    upper_limits_you<-   as.numeric(list("500","900","900","500","500","900","900","900","900","500","900","900","500","900","500","500"))
    targets_you<-        as.numeric(list("0.7","0.5","0.5","0.7","1.5","1.3","1.3","1.3","0.5","1.5","0.7","1.3","0.7","1.5","0.7","1.5"))
    lower_limits_hisher<-as.numeric(list("100","100","300","100","300","300","300","300","100","100","100","300","100","300","100","100"))
    upper_limits_hisher<-as.numeric(list("900","500","500","900","900","500","900","900","500","900","900","900","900","500","500","500"))
    targets_hisher<-     as.numeric(list("0.5","0.7","0.7","0.5","1.3","1.5","1.3","1.3","1.5","0.5","1.3","0.7","1.5","0.7","1.5","0.7"))
    lower_limits_you_order <- lower_limits_you[order]
    upper_limits_you_order<-   upper_limits_you[order]
    L1_responses_you <- as.numeric(targets_you)*(as.numeric(lower_limits_hisher)+as.numeric(upper_limits_hisher))/2
    for (i in 1:16) {L1_responses_you[i]<-min(max(as.numeric(lower_limits_you[i]),L1_responses_you[i]),as.numeric(upper_limits_you[i]))}
    L1_responses_hisher <- as.numeric(targets_hisher)*(as.numeric(lower_limits_you)+as.numeric(upper_limits_you))/2
    for (i in 1:16) {L1_responses_hisher[i]<-min(max(as.numeric(lower_limits_hisher[i]),L1_responses_hisher[i]),as.numeric(upper_limits_hisher[i]))}
    L2_responses_you <- as.numeric(targets_you)*L1_responses_hisher
    for (i in 1:16) {L2_responses_you[i]<-min(max(as.numeric(lower_limits_you[i]),L2_responses_you[i]),as.numeric(upper_limits_you[i]))}
    L2_responses_hisher <- as.numeric(targets_hisher)*L1_responses_you
    for (i in 1:16) {L2_responses_hisher[i]<-min(max(as.numeric(lower_limits_hisher[i]),L2_responses_hisher[i]),as.numeric(upper_limits_hisher[i]))}
    L3_responses_you <- as.numeric(targets_you)*L2_responses_hisher
    for (i in 1:16) {L3_responses_you[i]<-min(max(as.numeric(lower_limits_you[i]),L3_responses_you[i]),as.numeric(upper_limits_you[i]))}
    L3_responses_hisher <- as.numeric(targets_hisher)*L2_responses_you
    for (i in 1:16) {L3_responses_hisher[i]<-min(max(as.numeric(lower_limits_hisher[i]),L3_responses_hisher[i]),as.numeric(upper_limits_hisher[i]))}
    L4_responses_you <- as.numeric(targets_you)*L3_responses_hisher
    for (i in 1:16) {L4_responses_you[i]<-min(max(as.numeric(lower_limits_you[i]),L4_responses_you[i]),as.numeric(upper_limits_you[i]))}
    equil_responses_you <- as.matrix(c(100,100,150,300,500,650,900,900,100,150,300,390,500,750,350,500))
    levelkresponses <- cbind(L1_responses_you,L2_responses_you,L3_responses_you,L4_responses_you,equil_responses_you)
    levelkresponses <- levelkresponses[order,]
  }
  #variables
  {
  exp_params <- exp(params[1:5])
  p1 <- exp_params[1] / (1 + sum(exp_params))
  p2 <- exp_params[2] / (1 + sum(exp_params))
  p3 <- exp_params[3] / (1 + sum(exp_params))
  p4 <- exp_params[4] / (1 + sum(exp_params))
  p5 <- exp_params[5] / (1 + sum(exp_params))
  sf <- 0.1#scaling factor for MLE
  n_round1 <- exp(sf*params[6])+upper_limits_you_order[1]
  n_round2 <- exp(sf*params[7])+upper_limits_you_order[2]
  n_round3 <- exp(sf*params[8])+upper_limits_you_order[3]
  n_round4 <- exp(sf*params[9])+upper_limits_you_order[4]
  n_round5 <- exp(sf*params[10])+upper_limits_you_order[5]
  n_round6 <- exp(sf*params[11])+upper_limits_you_order[6]
  n_round7 <- exp(sf*params[12])+upper_limits_you_order[7]
  n_round8 <- exp(sf*params[13])+upper_limits_you_order[8]
  n_round9 <- exp(sf*params[14])+upper_limits_you_order[9]
  n_round10 <- exp(sf*params[15])+upper_limits_you_order[10]
  n_round11 <- exp(sf*params[16])+upper_limits_you_order[11]
  n_round12 <- exp(sf*params[17])+upper_limits_you_order[12]
  n_round13 <- exp(sf*params[18])+upper_limits_you_order[13]
  n_round14 <- exp(sf*params[19])+upper_limits_you_order[14]
  n_round15 <- exp(sf*params[20])+upper_limits_you_order[15]
  n_round16 <- exp(sf*params[21])+upper_limits_you_order[16]
}
  #f0
  { f0_round1 <- 1/(as.numeric(upper_limits_you_order[1])-as.numeric(lower_limits_you_order[1])+1)
    f0_round2 <- 1/(as.numeric(upper_limits_you_order[2])-as.numeric(lower_limits_you_order[2])+1)
    f0_round3 <- 1/(as.numeric(upper_limits_you_order[3])-as.numeric(lower_limits_you_order[3])+1)
    f0_round4 <- 1/(as.numeric(upper_limits_you_order[4])-as.numeric(lower_limits_you_order[4])+1)
    f0_round5 <- 1/(as.numeric(upper_limits_you_order[5])-as.numeric(lower_limits_you_order[5])+1)
    f0_round6 <- 1/(as.numeric(upper_limits_you_order[6])-as.numeric(lower_limits_you_order[6])+1)
    f0_round7 <- 1/(as.numeric(upper_limits_you_order[7])-as.numeric(lower_limits_you_order[7])+1)
    f0_round8 <- 1/(as.numeric(upper_limits_you_order[8])-as.numeric(lower_limits_you_order[8])+1)
    f0_round9 <- 1/(as.numeric(upper_limits_you_order[9])-as.numeric(lower_limits_you_order[9])+1)
    f0_round10 <- 1/(as.numeric(upper_limits_you_order[10])-as.numeric(lower_limits_you_order[10])+1)
    f0_round11 <- 1/(as.numeric(upper_limits_you_order[11])-as.numeric(lower_limits_you_order[11])+1)
    f0_round12 <- 1/(as.numeric(upper_limits_you_order[12])-as.numeric(lower_limits_you_order[12])+1)
    f0_round13 <- 1/(as.numeric(upper_limits_you_order[13])-as.numeric(lower_limits_you_order[13])+1)
    f0_round14 <- 1/(as.numeric(upper_limits_you_order[14])-as.numeric(lower_limits_you_order[14])+1)
    f0_round15 <- 1/(as.numeric(upper_limits_you_order[15])-as.numeric(lower_limits_you_order[15])+1)
    f0_round16 <- 1/(as.numeric(upper_limits_you_order[16])-as.numeric(lower_limits_you_order[16])+1)
    if (x[1]==round(levelkresponses[1,5])) {f0_round1=0}
    if (x[2]==round(levelkresponses[2,5])) {f0_round2=0}
    if (x[3]==round(levelkresponses[3,5])) {f0_round3=0} 
    if (x[4]==round(levelkresponses[4,5])) {f0_round4=0} 
    if (x[5]==round(levelkresponses[5,5])) {f0_round5=0}
    if (x[6]==round(levelkresponses[6,5])) {f0_round6=0}
    if (x[7]==round(levelkresponses[7,5])) {f0_round7=0} 
    if (x[8]==round(levelkresponses[8,5])) {f0_round8=0} 
    if (x[9]==round(levelkresponses[9,5])) {f0_round9=0}
    if (x[10]==round(levelkresponses[10,5])) {f0_round10=0}
    if (x[11]==round(levelkresponses[11,5])) {f0_round11=0} 
    if (x[12]==round(levelkresponses[12,5])) {f0_round12=0} 
    if (x[13]==round(levelkresponses[13,5])) {f0_round13=0}
    if (x[14]==round(levelkresponses[14,5])) {f0_round14=0}
    if (x[15]==round(levelkresponses[15,5])) {f0_round15=0} 
    if (x[16]==round(levelkresponses[16,5])) {f0_round16=0}
    }
  #f1
  { f1_round1 <- dbinom(x[1], round(n_round1), round(levelkresponses[1,1])/round(n_round1))
    f1_round2 <- dbinom(x[2], round(n_round2), round(levelkresponses[2,1])/round(n_round2))
    f1_round3 <- dbinom(x[3], round(n_round3), round(levelkresponses[3,1])/round(n_round3))
    f1_round4 <- dbinom(x[4], round(n_round4), round(levelkresponses[4,1])/round(n_round4))
    f1_round5 <- dbinom(x[5], round(n_round5), round(levelkresponses[5,1])/round(n_round5))
    f1_round6 <- dbinom(x[6], round(n_round6), round(levelkresponses[6,1])/round(n_round6))
    f1_round7 <- dbinom(x[7], round(n_round7), round(levelkresponses[7,1])/round(n_round7))
    f1_round8 <- dbinom(x[8], round(n_round8), round(levelkresponses[8,1])/round(n_round8))
    f1_round9 <- dbinom(x[9], round(n_round9), round(levelkresponses[9,1])/round(n_round9))
    f1_round10 <- dbinom(x[10], round(n_round10), round(levelkresponses[10,1])/round(n_round10))
    f1_round11 <- dbinom(x[11], round(n_round11), round(levelkresponses[11,1])/round(n_round11))
    f1_round12 <- dbinom(x[12], round(n_round12), round(levelkresponses[12,1])/round(n_round12))
    f1_round13 <- dbinom(x[13], round(n_round13), round(levelkresponses[13,1])/round(n_round13))
    f1_round14 <- dbinom(x[14], round(n_round14), round(levelkresponses[14,1])/round(n_round14))
    f1_round15 <- dbinom(x[15], round(n_round15), round(levelkresponses[15,1])/round(n_round15))
    f1_round16 <- dbinom(x[16], round(n_round16), round(levelkresponses[16,1])/round(n_round16))
    if (x[1]==round(levelkresponses[1,5])) {f1_round1=0}
    if (x[2]==round(levelkresponses[2,5])) {f1_round2=0}
    if (x[3]==round(levelkresponses[3,5])) {f1_round3=0} 
    if (x[4]==round(levelkresponses[4,5])) {f1_round4=0} 
    if (x[5]==round(levelkresponses[5,5])) {f1_round5=0}
    if (x[6]==round(levelkresponses[6,5])) {f1_round6=0}
    if (x[7]==round(levelkresponses[7,5])) {f1_round7=0} 
    if (x[8]==round(levelkresponses[8,5])) {f1_round8=0} 
    if (x[9]==round(levelkresponses[9,5])) {f1_round9=0}
    if (x[10]==round(levelkresponses[10,5])) {f1_round10=0}
    if (x[11]==round(levelkresponses[11,5])) {f1_round11=0} 
    if (x[12]==round(levelkresponses[12,5])) {f1_round12=0} 
    if (x[13]==round(levelkresponses[13,5])) {f1_round13=0}
    if (x[14]==round(levelkresponses[14,5])) {f1_round14=0}
    if (x[15]==round(levelkresponses[15,5])) {f1_round15=0} 
    if (x[16]==round(levelkresponses[16,5])) {f1_round16=0} 
    }
  #f2
  { f2_round1 <- dbinom(x[1], round(n_round1), round(levelkresponses[1,2])/round(n_round1))
    f2_round2 <- dbinom(x[2], round(n_round2), round(levelkresponses[2,2])/round(n_round2))
    f2_round3 <- dbinom(x[3], round(n_round3), round(levelkresponses[3,2])/round(n_round3))
    f2_round4 <- dbinom(x[4], round(n_round4), round(levelkresponses[4,2])/round(n_round4))
    f2_round5 <- dbinom(x[5], round(n_round5), round(levelkresponses[5,2])/round(n_round5))
    f2_round6 <- dbinom(x[6], round(n_round6), round(levelkresponses[6,2])/round(n_round6))
    f2_round7 <- dbinom(x[7], round(n_round7), round(levelkresponses[7,2])/round(n_round7))
    f2_round8 <- dbinom(x[8], round(n_round8), round(levelkresponses[8,2])/round(n_round8))
    f2_round9 <- dbinom(x[9], round(n_round9), round(levelkresponses[9,2])/round(n_round9))
    f2_round10 <- dbinom(x[10], round(n_round10), round(levelkresponses[10,2])/round(n_round10))
    f2_round11 <- dbinom(x[11], round(n_round11), round(levelkresponses[11,2])/round(n_round11))
    f2_round12 <- dbinom(x[12], round(n_round12), round(levelkresponses[12,2])/round(n_round12))
    f2_round13 <- dbinom(x[13], round(n_round13), round(levelkresponses[13,2])/round(n_round13))
    f2_round14 <- dbinom(x[14], round(n_round14), round(levelkresponses[14,2])/round(n_round14))
    f2_round15 <- dbinom(x[15], round(n_round15), round(levelkresponses[15,2])/round(n_round15))
    f2_round16 <- dbinom(x[16], round(n_round16), round(levelkresponses[16,2])/round(n_round16))
    if (x[1]==round(levelkresponses[1,5])) {f2_round1=0}
    if (x[2]==round(levelkresponses[2,5])) {f2_round2=0}
    if (x[3]==round(levelkresponses[3,5])) {f2_round3=0} 
    if (x[4]==round(levelkresponses[4,5])) {f2_round4=0} 
    if (x[5]==round(levelkresponses[5,5])) {f2_round5=0}
    if (x[6]==round(levelkresponses[6,5])) {f2_round6=0}
    if (x[7]==round(levelkresponses[7,5])) {f2_round7=0} 
    if (x[8]==round(levelkresponses[8,5])) {f2_round8=0} 
    if (x[9]==round(levelkresponses[9,5])) {f2_round9=0}
    if (x[10]==round(levelkresponses[10,5])) {f2_round10=0}
    if (x[11]==round(levelkresponses[11,5])) {f2_round11=0} 
    if (x[12]==round(levelkresponses[12,5])) {f2_round12=0} 
    if (x[13]==round(levelkresponses[13,5])) {f2_round13=0}
    if (x[14]==round(levelkresponses[14,5])) {f2_round14=0}
    if (x[15]==round(levelkresponses[15,5])) {f2_round15=0} 
    if (x[16]==round(levelkresponses[16,5])) {f2_round16=0}
    }
  #f3
  { f3_round1 <- dbinom(x[1], round(n_round1), round(levelkresponses[1,3])/round(n_round1))
    f3_round2 <- dbinom(x[2], round(n_round2), round(levelkresponses[2,3])/round(n_round2))
    f3_round3 <- dbinom(x[3], round(n_round3), round(levelkresponses[3,3])/round(n_round3))
    f3_round4 <- dbinom(x[4], round(n_round4), round(levelkresponses[4,3])/round(n_round4))
    f3_round5 <- dbinom(x[5], round(n_round5), round(levelkresponses[5,3])/round(n_round5))
    f3_round6 <- dbinom(x[6], round(n_round6), round(levelkresponses[6,3])/round(n_round6))
    f3_round7 <- dbinom(x[7], round(n_round7), round(levelkresponses[7,3])/round(n_round7))
    f3_round8 <- dbinom(x[8], round(n_round8), round(levelkresponses[8,3])/round(n_round8))
    f3_round9 <- dbinom(x[9], round(n_round9), round(levelkresponses[9,3])/round(n_round9))
    f3_round10 <- dbinom(x[10], round(n_round10), round(levelkresponses[10,3])/round(n_round10))
    f3_round11 <- dbinom(x[11], round(n_round11), round(levelkresponses[11,3])/round(n_round11))
    f3_round12 <- dbinom(x[12], round(n_round12), round(levelkresponses[12,3])/round(n_round12))
    f3_round13 <- dbinom(x[13], round(n_round13), round(levelkresponses[13,3])/round(n_round13))
    f3_round14 <- dbinom(x[14], round(n_round14), round(levelkresponses[14,3])/round(n_round14))
    f3_round15 <- dbinom(x[15], round(n_round15), round(levelkresponses[15,3])/round(n_round15))
    f3_round16 <- dbinom(x[16], round(n_round16), round(levelkresponses[16,3])/round(n_round16))
    if (x[1]==round(levelkresponses[1,5])) {f3_round1=0}
    if (x[2]==round(levelkresponses[2,5])) {f3_round2=0}
    if (x[3]==round(levelkresponses[3,5])) {f3_round3=0} 
    if (x[4]==round(levelkresponses[4,5])) {f3_round4=0} 
    if (x[5]==round(levelkresponses[5,5])) {f3_round5=0}
    if (x[6]==round(levelkresponses[6,5])) {f3_round6=0}
    if (x[7]==round(levelkresponses[7,5])) {f3_round7=0} 
    if (x[8]==round(levelkresponses[8,5])) {f3_round8=0} 
    if (x[9]==round(levelkresponses[9,5])) {f3_round9=0}
    if (x[10]==round(levelkresponses[10,5])) {f3_round10=0}
    if (x[11]==round(levelkresponses[11,5])) {f3_round11=0} 
    if (x[12]==round(levelkresponses[12,5])) {f3_round12=0} 
    if (x[13]==round(levelkresponses[13,5])) {f3_round13=0}
    if (x[14]==round(levelkresponses[14,5])) {f3_round14=0}
    if (x[15]==round(levelkresponses[15,5])) {f3_round15=0} 
    if (x[16]==round(levelkresponses[16,5])) {f3_round16=0} 
    }
  #f4
  { f4_round1 <- dbinom(x[1], round(n_round1), round(levelkresponses[1,4])/round(n_round1))
    f4_round2 <- dbinom(x[2], round(n_round2), round(levelkresponses[2,4])/round(n_round2))
    f4_round3 <- dbinom(x[3], round(n_round3), round(levelkresponses[3,4])/round(n_round3))
    f4_round4 <- dbinom(x[4], round(n_round4), round(levelkresponses[4,4])/round(n_round4))
    f4_round5 <- dbinom(x[5], round(n_round5), round(levelkresponses[5,4])/round(n_round5))
    f4_round6 <- dbinom(x[6], round(n_round6), round(levelkresponses[6,4])/round(n_round6))
    f4_round7 <- dbinom(x[7], round(n_round7), round(levelkresponses[7,4])/round(n_round7))
    f4_round8 <- dbinom(x[8], round(n_round8), round(levelkresponses[8,4])/round(n_round8))
    f4_round9 <- dbinom(x[9], round(n_round9), round(levelkresponses[9,4])/round(n_round9))
    f4_round10 <- dbinom(x[10], round(n_round10), round(levelkresponses[10,4])/round(n_round10))
    f4_round11 <- dbinom(x[11], round(n_round11), round(levelkresponses[11,4])/round(n_round11))
    f4_round12 <- dbinom(x[12], round(n_round12), round(levelkresponses[12,4])/round(n_round12))
    f4_round13 <- dbinom(x[13], round(n_round13), round(levelkresponses[13,4])/round(n_round13))
    f4_round14 <- dbinom(x[14], round(n_round14), round(levelkresponses[14,4])/round(n_round14))
    f4_round15 <- dbinom(x[15], round(n_round15), round(levelkresponses[15,4])/round(n_round15))
    f4_round16 <- dbinom(x[16], round(n_round16), round(levelkresponses[16,4])/round(n_round16))
    if (x[1]==round(levelkresponses[1,5])) {f4_round1=0}
    if (x[2]==round(levelkresponses[2,5])) {f4_round2=0}
    if (x[3]==round(levelkresponses[3,5])) {f4_round3=0} 
    if (x[4]==round(levelkresponses[4,5])) {f4_round4=0} 
    if (x[5]==round(levelkresponses[5,5])) {f4_round5=0}
    if (x[6]==round(levelkresponses[6,5])) {f4_round6=0}
    if (x[7]==round(levelkresponses[7,5])) {f4_round7=0} 
    if (x[8]==round(levelkresponses[8,5])) {f4_round8=0} 
    if (x[9]==round(levelkresponses[9,5])) {f4_round9=0}
    if (x[10]==round(levelkresponses[10,5])) {f4_round10=0}
    if (x[11]==round(levelkresponses[11,5])) {f4_round11=0} 
    if (x[12]==round(levelkresponses[12,5])) {f4_round12=0} 
    if (x[13]==round(levelkresponses[13,5])) {f4_round13=0}
    if (x[14]==round(levelkresponses[14,5])) {f4_round14=0}
    if (x[15]==round(levelkresponses[15,5])) {f4_round15=0} 
    if (x[16]==round(levelkresponses[16,5])) {f4_round16=0}
    }
  #f5
  { f5_round1 <- dbinom(x[1], round(n_round1), round(levelkresponses[1,5])/round(n_round1))
    f5_round2 <- dbinom(x[2], round(n_round2), round(levelkresponses[2,5])/round(n_round2))
    f5_round3 <- dbinom(x[3], round(n_round3), round(levelkresponses[3,5])/round(n_round3))
    f5_round4 <- dbinom(x[4], round(n_round4), round(levelkresponses[4,5])/round(n_round4))
    f5_round5 <- dbinom(x[5], round(n_round5), round(levelkresponses[5,5])/round(n_round5))
    f5_round6 <- dbinom(x[6], round(n_round6), round(levelkresponses[6,5])/round(n_round6))
    f5_round7 <- dbinom(x[7], round(n_round7), round(levelkresponses[7,5])/round(n_round7))
    f5_round8 <- dbinom(x[8], round(n_round8), round(levelkresponses[8,5])/round(n_round8))
    f5_round9 <- dbinom(x[9], round(n_round9), round(levelkresponses[9,5])/round(n_round9))
    f5_round10 <- dbinom(x[10], round(n_round10), round(levelkresponses[10,5])/round(n_round10))
    f5_round11 <- dbinom(x[11], round(n_round11), round(levelkresponses[11,5])/round(n_round11))
    f5_round12 <- dbinom(x[12], round(n_round12), round(levelkresponses[12,5])/round(n_round12))
    f5_round13 <- dbinom(x[13], round(n_round13), round(levelkresponses[13,5])/round(n_round13))
    f5_round14 <- dbinom(x[14], round(n_round14), round(levelkresponses[14,5])/round(n_round14))
    f5_round15 <- dbinom(x[15], round(n_round15), round(levelkresponses[15,5])/round(n_round15))
    f5_round16 <- dbinom(x[16], round(n_round16), round(levelkresponses[16,5])/round(n_round16))}
  #likelihood
  {
  l <- (1/1)*((1 - p1 - p2 - p3 - p4 - p5) * f0_round1 + p1 * f1_round1 + p2 * f2_round1 + p3 * f3_round1 + p4 * f4_round1 + p5 * f5_round1)*
       (1/1)*((1 - p1 - p2 - p3 - p4 - p5) * f0_round2 + p1 * f1_round2 + p2 * f2_round2 + p3 * f3_round2 + p4 * f4_round2 + p5 * f5_round2)*
       (1/1)*((1 - p1 - p2 - p3 - p4 - p5) * f0_round3 + p1 * f1_round3 + p2 * f2_round3 + p3 * f3_round3 + p4 * f4_round3 + p5 * f5_round3)*
       (1/1)*((1 - p1 - p2 - p3 - p4 - p5) * f0_round4 + p1 * f1_round4 + p2 * f2_round4 + p3 * f3_round4 + p4 * f4_round4 + p5 * f5_round4)*
       (1/1)*((1 - p1 - p2 - p3 - p4 - p5) * f0_round5 + p1 * f1_round5 + p2 * f2_round5 + p3 * f3_round5 + p4 * f4_round5 + p5 * f5_round5)*
       (1/1)*((1 - p1 - p2 - p3 - p4 - p5) * f0_round6 + p1 * f1_round6 + p2 * f2_round6 + p3 * f3_round6 + p4 * f4_round6 + p5 * f5_round6)*
       (1/1)*((1 - p1 - p2 - p3 - p4 - p5) * f0_round7 + p1 * f1_round7 + p2 * f2_round7 + p3 * f3_round7 + p4 * f4_round7 + p5 * f5_round7)*
       (1/1)*((1 - p1 - p2 - p3 - p4 - p5) * f0_round8 + p1 * f1_round8 + p2 * f2_round8 + p3 * f3_round8 + p4 * f4_round8 + p5 * f5_round8)*
       (1/1)*((1 - p1 - p2 - p3 - p4 - p5) * f0_round9 + p1 * f1_round9 + p2 * f2_round9 + p3 * f3_round9 + p4 * f4_round9 + p5 * f5_round9)*
       (1/1)*((1 - p1 - p2 - p3 - p4 - p5) * f0_round10 + p1 * f1_round10 + p2 * f2_round10 + p3 * f3_round10 + p4 * f4_round10 + p5 * f5_round10)*
       (1/1)*((1 - p1 - p2 - p3 - p4 - p5) * f0_round11 + p1 * f1_round11 + p2 * f2_round11 + p3 * f3_round11 + p4 * f4_round11 + p5 * f5_round11)*
       (1/1)*((1 - p1 - p2 - p3 - p4 - p5) * f0_round12 + p1 * f1_round12 + p2 * f2_round12 + p3 * f3_round12 + p4 * f4_round12 + p5 * f5_round12)*
       (1/1)*((1 - p1 - p2 - p3 - p4 - p5) * f0_round13 + p1 * f1_round13 + p2 * f2_round13 + p3 * f3_round13 + p4 * f4_round13 + p5 * f5_round13)*
       (1/1)*((1 - p1 - p2 - p3 - p4 - p5) * f0_round14 + p1 * f1_round14 + p2 * f2_round14 + p3 * f3_round14 + p4 * f4_round14 + p5 * f5_round14)*
       (1/1)*((1 - p1 - p2 - p3 - p4 - p5) * f0_round15 + p1 * f1_round15 + p2 * f2_round15 + p3 * f3_round15 + p4 * f4_round15 + p5 * f5_round15)*
       (1/1)*((1 - p1 - p2 - p3 - p4 - p5) * f0_round16 + p1 * f1_round16 + p2 * f2_round16 + p3 * f3_round16 + p4 * f4_round16 + p5 * f5_round16)
  }
  
  logl <- log(l)
  return(sum(logl))
}

#responses
{
  order <- c(1,3,5,7,9,11,13,15,2,4,6,8,10,12,14,16)
  lower_limits_you<-   as.numeric(list("100","100","100","300","300","300","300","300","100","100","300","100","300","100","100","100"))
  upper_limits_you<-   as.numeric(list("500","900","900","500","500","900","900","900","900","500","900","900","500","900","500","500"))
  targets_you<-        as.numeric(list("0.7","0.5","0.5","0.7","1.5","1.3","1.3","1.3","0.5","1.5","0.7","1.3","0.7","1.5","0.7","1.5"))
  lower_limits_hisher<-as.numeric(list("100","100","300","100","300","300","300","300","100","100","100","300","100","300","100","100"))
  upper_limits_hisher<-as.numeric(list("900","500","500","900","900","500","900","900","500","900","900","900","900","500","500","500"))
  targets_hisher<-     as.numeric(list("0.5","0.7","0.7","0.5","1.3","1.5","1.3","1.3","1.5","0.5","1.3","0.7","1.5","0.7","1.5","0.7"))
  L1_responses_you <- as.numeric(targets_you)*(as.numeric(lower_limits_hisher)+as.numeric(upper_limits_hisher))/2
  for (i in 1:16) {L1_responses_you[i]<-min(max(as.numeric(lower_limits_you[i]),L1_responses_you[i]),as.numeric(upper_limits_you[i]))}
  L1_responses_hisher <- as.numeric(targets_hisher)*(as.numeric(lower_limits_you)+as.numeric(upper_limits_you))/2
  for (i in 1:16) {L1_responses_hisher[i]<-min(max(as.numeric(lower_limits_hisher[i]),L1_responses_hisher[i]),as.numeric(upper_limits_hisher[i]))}
  L2_responses_you <- as.numeric(targets_you)*L1_responses_hisher
  for (i in 1:16) {L2_responses_you[i]<-min(max(as.numeric(lower_limits_you[i]),L2_responses_you[i]),as.numeric(upper_limits_you[i]))}
  L2_responses_hisher <- as.numeric(targets_hisher)*L1_responses_you
  for (i in 1:16) {L2_responses_hisher[i]<-min(max(as.numeric(lower_limits_hisher[i]),L2_responses_hisher[i]),as.numeric(upper_limits_hisher[i]))}
  L3_responses_you <- as.numeric(targets_you)*L2_responses_hisher
  for (i in 1:16) {L3_responses_you[i]<-min(max(as.numeric(lower_limits_you[i]),L3_responses_you[i]),as.numeric(upper_limits_you[i]))}
  L3_responses_hisher <- as.numeric(targets_hisher)*L2_responses_you
  for (i in 1:16) {L3_responses_hisher[i]<-min(max(as.numeric(lower_limits_hisher[i]),L3_responses_hisher[i]),as.numeric(upper_limits_hisher[i]))}
  L4_responses_you <- as.numeric(targets_you)*L3_responses_hisher
  for (i in 1:16) {L4_responses_you[i]<-min(max(as.numeric(lower_limits_you[i]),L4_responses_you[i]),as.numeric(upper_limits_you[i]))}
  equil_responses_you <- as.matrix(c(100,100,150,300,500,650,900,900,100,150,300,390,500,750,350,500))
  levelkresponses <- cbind(L1_responses_you,L2_responses_you,L3_responses_you,L4_responses_you,equil_responses_you)
  levelkresponses <- levelkresponses[order,]
  colnames(levelkresponses) <- NULL
  lower_limits_you_order <- lower_limits_you[order]
  upper_limits_you_order<-   upper_limits_you[order]
}

# Maximise Likelihood -----------------
i=-1
  filename_data <- paste0("SubjectsGuesses.csv")
  GG_data_full<-read.csv(filename_data, header = TRUE)
  GG_data_full[,2:17]<-round(GG_data_full[,2:17])
  GG_data_full$temp <- "NA"
  GG_data_full$treatment_combinations <- paste(GG_data_full$avg, GG_data_full$temp, sep = "_")
  unique_treatments <- unique(GG_data_full$treatment_combinations)
  output <- matrix(NA,length(unique_treatments)*nrow(GG_data_full),70)
  sd_list<- vector("list",nrow(GG_data_full))
  colnames(output)<-c("subject", "LLM", "p0","p1","p2","p3","p4","p5","n_round1","n_round2","n_round3","n_round4","n_round5","n_round6","n_round7","n_round8","n_round9","n_round10","n_round11","n_round12","n_round13","n_round14","n_round15","n_round16", "treatment","temperature","ci_lower_p0","ci_upper_p0","ci_lower_p1","ci_upper_p1","ci_lower_p2","ci_upper_p2","ci_lower_p3","ci_upper_p3","ci_lower_p4","ci_upper_p4","ci_lower_p5","ci_upper_p5","ci_lower_n_round1","ci_upper_n_round1","ci_lower_n_round2","ci_upper_n_round2","ci_lower_n_round3","ci_upper_n_round3","ci_lower_n_round4","ci_upper_n_round4","ci_lower_n_round5","ci_upper_n_round5","ci_lower_n_round6","ci_upper_n_round6","ci_lower_n_round7","ci_upper_n_round7","ci_lower_n_round8","ci_upper_n_round8","ci_lower_n_round9","ci_upper_n_round9","ci_lower_n_round10","ci_upper_n_round10","ci_lower_n_round11","ci_upper_n_round11","ci_lower_n_round12","ci_upper_n_round12","ci_lower_n_round13","ci_upper_n_round13","ci_lower_n_round14","ci_upper_n_round14","ci_lower_n_round15","ci_upper_n_round15","ci_lower_n_round16","ci_upper_n_round16")
  sf <- 0.1

  for (treatment in unique_treatments) {
    i=i+1
    for (j in 1:nrow(GG_data_full)){
    print(paste("subject:",j))
    GG_data <- GG_data_full[GG_data_full$treatment_combinations == treatment,]
    x <- as.numeric(GG_data[j,2:17])
    initial_params <- c(0.1, 0.1, 0.1, 0.1, 0.1,c(rep(0,16)))
    result <- maxLik(logLik = log_likelihood_gg, start = initial_params, method = "BFGS")
    optimized_params <- result$estimate
    params <- exp(optimized_params[1:5]) / (1 + sum(exp(optimized_params[1:5])))
    n_round1 <- exp(sf*optimized_params[6])+upper_limits_you_order[1]
    n_round2 <- exp(sf*optimized_params[7])+upper_limits_you_order[2]
    n_round3 <- exp(sf*optimized_params[8])+upper_limits_you_order[3]
    n_round4 <- exp(sf*optimized_params[9])+upper_limits_you_order[4]
    n_round5 <- exp(sf*optimized_params[10])+upper_limits_you_order[5]
    n_round6 <- exp(sf*optimized_params[11])+upper_limits_you_order[6]
    n_round7 <- exp(sf*optimized_params[12])+upper_limits_you_order[7]
    n_round8 <- exp(sf*optimized_params[13])+upper_limits_you_order[8]
    n_round9 <- exp(sf*optimized_params[14])+upper_limits_you_order[9]
    n_round10 <- exp(sf*optimized_params[15])+upper_limits_you_order[10]
    n_round11 <- exp(sf*optimized_params[16])+upper_limits_you_order[11]
    n_round12 <- exp(sf*optimized_params[17])+upper_limits_you_order[12]
    n_round13 <- exp(sf*optimized_params[18])+upper_limits_you_order[13]
    n_round14 <- exp(sf*optimized_params[19])+upper_limits_you_order[14]
    n_round15 <- exp(sf*optimized_params[20])+upper_limits_you_order[15]
    n_round16 <- exp(sf*optimized_params[21])+upper_limits_you_order[16]
    n <- c(n_round1,n_round2,n_round3,n_round4,n_round5,n_round6,n_round7,n_round8,n_round9,n_round10,n_round11,n_round12,n_round13,n_round14,n_round15,n_round16)
    sd<-matrix(0,16,5)
    for (s in 1:5){ sd[,s] <- sqrt(levelkresponses[,s]* (1- (levelkresponses[,s]/n)))}
    p0 <- 1 - sum(params)
    results<-matrix(0,1,22)
    results[1,]<-c(p0,params,n)
    colnames(results)<-c("p0","p1","p2","p3","p4","p5","n_round1","n_round2","n_round3","n_round4","n_round5","n_round6","n_round7","n_round8","n_round9","n_round10","n_round11","n_round12","n_round13","n_round14","n_round15","n_round16")
    output[i+j,1]<-GG_data_full[j,1]
    output[i+j,2]<-llm
    output[i+j,3:24]<-results
    output[i+j,25]<-treatment
    output[i+j,26]<-"NA"
    sd_list[[j]]<-sd
    }
    
    n_bootstrap <- 1000
    bootstrap_estimates <- matrix(NA, nrow = n_bootstrap, ncol = length(optimized_params))
    
    # Bootstrap for SEs

      GG_data_rounds <- GG_data[,2:17]
      bootstrap_sample <- apply(GG_data_rounds, 2, function(col) {
        sample(col, n_bootstrap, replace = TRUE)
      })
      
      bootstrap_estimates <- pblapply(1:n_bootstrap, function(b) {
        x <- bootstrap_sample[b, ]
        tryCatch({
          environment(log_likelihood_gg) <- list2env(list(x = x), parent = environment(log_likelihood_gg)) # Assign 'x' in the environment so it is accessible by the log-likelihood function
          bootstrap_result <- maxLik(logLik = log_likelihood_gg, start = initial_params, method = "BFGS")
          return(bootstrap_result$estimate)
        }, error = function(e) {
          return(rep(NA, length(initial_params)))
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
    n_round1_estimates <- exp(sf*bootstrap_estimates[,6])+upper_limits_you_order[1]
    n_round2_estimates <- exp(sf*bootstrap_estimates[,7])+upper_limits_you_order[2]
    n_round3_estimates <- exp(sf*bootstrap_estimates[,8])+upper_limits_you_order[3]
    n_round4_estimates <- exp(sf*bootstrap_estimates[,9])+upper_limits_you_order[4]
    n_round5_estimates <- exp(sf*bootstrap_estimates[,10])+upper_limits_you_order[5]
    n_round6_estimates <- exp(sf*bootstrap_estimates[,11])+upper_limits_you_order[6]
    n_round7_estimates <- exp(sf*bootstrap_estimates[,12])+upper_limits_you_order[7]
    n_round8_estimates <- exp(sf*bootstrap_estimates[,13])+upper_limits_you_order[8]
    n_round9_estimates <- exp(sf*bootstrap_estimates[,14])+upper_limits_you_order[9]
    n_round10_estimates <- exp(sf*bootstrap_estimates[,15])+upper_limits_you_order[10]
    n_round11_estimates <- exp(sf*bootstrap_estimates[,16])+upper_limits_you_order[11]
    n_round12_estimates <- exp(sf*bootstrap_estimates[,17])+upper_limits_you_order[12]
    n_round13_estimates <- exp(sf*bootstrap_estimates[,18])+upper_limits_you_order[13]
    n_round14_estimates <- exp(sf*bootstrap_estimates[,19])+upper_limits_you_order[14]
    n_round15_estimates <- exp(sf*bootstrap_estimates[,20])+upper_limits_you_order[15]
    n_round16_estimates <- exp(sf*bootstrap_estimates[,21])+upper_limits_you_order[16]
    
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
    ci_lower_n_round1_estimates <- quantile(n_round1_estimates, probs = 0.05, na.rm = TRUE)
    ci_upper_n_round1_estimates <- quantile(n_round1_estimates, probs = 0.95, na.rm = TRUE)
    ci_lower_n_round2_estimates <- quantile(n_round2_estimates, probs = 0.05, na.rm = TRUE)
    ci_upper_n_round2_estimates <- quantile(n_round2_estimates, probs = 0.95, na.rm = TRUE)
    ci_lower_n_round3_estimates <- quantile(n_round3_estimates, probs = 0.05, na.rm = TRUE)
    ci_upper_n_round3_estimates <- quantile(n_round3_estimates, probs = 0.95, na.rm = TRUE)
    ci_lower_n_round4_estimates <- quantile(n_round4_estimates, probs = 0.05, na.rm = TRUE)
    ci_upper_n_round4_estimates <- quantile(n_round4_estimates, probs = 0.95, na.rm = TRUE)
    ci_lower_n_round5_estimates <- quantile(n_round5_estimates, probs = 0.05, na.rm = TRUE)
    ci_upper_n_round5_estimates <- quantile(n_round5_estimates, probs = 0.95, na.rm = TRUE)
    ci_lower_n_round6_estimates <- quantile(n_round6_estimates, probs = 0.05, na.rm = TRUE)
    ci_upper_n_round6_estimates <- quantile(n_round6_estimates, probs = 0.95, na.rm = TRUE)
    ci_lower_n_round7_estimates <- quantile(n_round7_estimates, probs = 0.05, na.rm = TRUE)
    ci_upper_n_round7_estimates <- quantile(n_round7_estimates, probs = 0.95, na.rm = TRUE)
    ci_lower_n_round8_estimates <- quantile(n_round8_estimates, probs = 0.05, na.rm = TRUE)
    ci_upper_n_round8_estimates <- quantile(n_round8_estimates, probs = 0.95, na.rm = TRUE)
    ci_lower_n_round9_estimates <- quantile(n_round9_estimates, probs = 0.05, na.rm = TRUE)
    ci_upper_n_round9_estimates <- quantile(n_round9_estimates, probs = 0.95, na.rm = TRUE)
    ci_lower_n_round10_estimates <- quantile(n_round10_estimates, probs = 0.05, na.rm = TRUE)
    ci_upper_n_round10_estimates <- quantile(n_round10_estimates, probs = 0.95, na.rm = TRUE)
    ci_lower_n_round11_estimates <- quantile(n_round11_estimates, probs = 0.05, na.rm = TRUE)
    ci_upper_n_round11_estimates <- quantile(n_round11_estimates, probs = 0.95, na.rm = TRUE)
    ci_lower_n_round12_estimates <- quantile(n_round12_estimates, probs = 0.05, na.rm = TRUE)
    ci_upper_n_round12_estimates <- quantile(n_round12_estimates, probs = 0.95, na.rm = TRUE)
    ci_lower_n_round13_estimates <- quantile(n_round13_estimates, probs = 0.05, na.rm = TRUE)
    ci_upper_n_round13_estimates <- quantile(n_round13_estimates, probs = 0.95, na.rm = TRUE)
    ci_lower_n_round14_estimates <- quantile(n_round14_estimates, probs = 0.05, na.rm = TRUE)
    ci_upper_n_round14_estimates <- quantile(n_round14_estimates, probs = 0.95, na.rm = TRUE)
    ci_lower_n_round15_estimates <- quantile(n_round15_estimates, probs = 0.05, na.rm = TRUE)
    ci_upper_n_round15_estimates <- quantile(n_round15_estimates, probs = 0.95, na.rm = TRUE)
    ci_lower_n_round16_estimates <- quantile(n_round16_estimates, probs = 0.05, na.rm = TRUE)
    ci_upper_n_round16_estimates <- quantile(n_round16_estimates, probs = 0.95, na.rm = TRUE)
    
    output[, "ci_lower_p1"] <- ci_lower_p1
    output[, "ci_upper_p1"] <- ci_upper_p1
    output[, "ci_lower_p2"] <- ci_lower_p2
    output[, "ci_upper_p2"] <- ci_upper_p2
    output[, "ci_lower_p3"] <- ci_lower_p3
    output[, "ci_upper_p3"] <- ci_upper_p3
    output[, "ci_lower_p4"] <- ci_lower_p4
    output[, "ci_upper_p4"] <- ci_upper_p4
    output[, "ci_lower_p5"] <- ci_lower_p5
    output[, "ci_upper_p5"] <- ci_upper_p5
    output[, "ci_lower_p0"] <- ci_lower_p0
    output[, "ci_upper_p0"] <- ci_upper_p0
    output[, "ci_lower_n_round1"] <- ci_lower_n_round1_estimates
    output[, "ci_upper_n_round1"] <- ci_upper_n_round1_estimates
    output[, "ci_lower_n_round2"] <- ci_lower_n_round2_estimates
    output[, "ci_upper_n_round2"] <- ci_upper_n_round2_estimates
    output[, "ci_lower_n_round3"] <- ci_lower_n_round3_estimates
    output[, "ci_upper_n_round3"] <- ci_upper_n_round3_estimates
    output[, "ci_lower_n_round4"] <- ci_lower_n_round4_estimates
    output[, "ci_upper_n_round4"] <- ci_upper_n_round4_estimates
    output[, "ci_lower_n_round5"] <- ci_lower_n_round5_estimates
    output[, "ci_upper_n_round5"] <- ci_upper_n_round5_estimates
    output[, "ci_lower_n_round6"] <- ci_lower_n_round6_estimates
    output[, "ci_upper_n_round6"] <- ci_upper_n_round6_estimates
    output[, "ci_lower_n_round7"] <- ci_lower_n_round7_estimates
    output[, "ci_upper_n_round7"] <- ci_upper_n_round7_estimates
    output[, "ci_lower_n_round8"] <- ci_lower_n_round8_estimates
    output[, "ci_upper_n_round8"] <- ci_upper_n_round8_estimates
    output[, "ci_lower_n_round9"] <- ci_lower_n_round9_estimates
    output[, "ci_upper_n_round9"] <- ci_upper_n_round9_estimates
    output[, "ci_lower_n_round10"] <- ci_lower_n_round10_estimates
    output[, "ci_upper_n_round10"] <- ci_upper_n_round10_estimates
    output[, "ci_lower_n_round11"] <- ci_lower_n_round11_estimates
    output[, "ci_upper_n_round11"] <- ci_upper_n_round11_estimates
    output[, "ci_lower_n_round12"] <- ci_lower_n_round12_estimates
    output[, "ci_upper_n_round12"] <- ci_upper_n_round12_estimates
    output[, "ci_lower_n_round13"] <- ci_lower_n_round13_estimates
    output[, "ci_upper_n_round13"] <- ci_upper_n_round13_estimates
    output[, "ci_lower_n_round14"] <- ci_lower_n_round14_estimates
    output[, "ci_upper_n_round14"] <- ci_upper_n_round14_estimates
    output[, "ci_lower_n_round15"] <- ci_lower_n_round15_estimates
    output[, "ci_upper_n_round15"] <- ci_upper_n_round15_estimates
    output[, "ci_lower_n_round16"] <- ci_lower_n_round16_estimates
    output[, "ci_upper_n_round16"] <- ci_upper_n_round16_estimates
    
  }
  filename_table <- paste0("output_gg_lk.csv")
  write.csv(output, file = filename_table, row.names = TRUE)
  filename_table <- paste0("output_gg_sd_lk.csv")
  write.csv(sd_list, file = filename_table, row.names = TRUE)
  i=0

print("done")




