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
LLMs<-c("chatgpt4","chatgpto1","claude2","claude3","gemini1","gemini2")

# -MRG-1--------------------------------------------------------------------

filename_data <- paste0("game1_1120.csv")
paper_data<-read.csv(filename_data, header = TRUE)
j=0
tests <- matrix(0,96,9)
colnames(tests)<- c("LLM","Game","two-sided","CDF_llm not below CDF_real","CDF_llm not above CDF_real","two-sided","less","greater","More rational" )
for (llm in LLMs){
  j=j+1
  print(llm)
  filename_data <- paste0("llm_data/game1_1120_",llm, ".csv")
  mrg_data_full<-read.csv(filename_data, header = TRUE)
  mrg_data_full$y <- mrg_data_full$response
  mrg_data_full <- subset(mrg_data_full, mrg_data_full$response != "uncomfortable")
  mrg_data_full <- subset(mrg_data_full, mrg_data_full$response != "gender bias")
  mrg_data_full <- subset(mrg_data_full, mrg_data_full$act_as == "human decision maker")

  mrg_data_full <- subset(mrg_data_full, mrg_data_full$temp == 0.5)
  if (llm=="chatgpto1"){mrg_data_full<-read.csv(filename_data, header = TRUE)}
  tests[j,1] <- llm
  tests[j,2] <- "MRG-1"
  tests[j,3]<-round(ks.test(as.numeric(mrg_data_full$response),paper_data$response,alternative="two.sided", exact=TRUE)$p.value,3)
  tests[j,4]<-round(ks.test(as.numeric(mrg_data_full$response),paper_data$response,alternative="less", exact=TRUE)$p.value,3)
  tests[j,5]<-round(ks.test(as.numeric(mrg_data_full$response),paper_data$response,alternative="greater", exact=TRUE)$p.value,3)
  tests[j,6]<-ifelse(tests[j,3]<0.05,"Reject","Not Reject")
  tests[j,7]<-ifelse(tests[j,4]<0.05,"Reject","Not Reject")
  tests[j,8]<-ifelse(tests[j,5]<0.05,"Reject","Not Reject")
  tests[j,9]<-ifelse(tests[j,5]<0.05,"LLM","Human")
  if (llm=="claude3"){
    tests[j,3]<-round(ks.test(as.numeric(mrg_data_full$response[mrg_data_full$response>15]),paper_data$response,alternative="two.sided", exact=TRUE)$p.value,3)
    tests[j,4]<-round(ks.test(as.numeric(mrg_data_full$response[mrg_data_full$response>15]),paper_data$response,alternative="less", exact=TRUE)$p.value,3)
    tests[j,5]<-round(ks.test(as.numeric(mrg_data_full$response[mrg_data_full$response>15]),paper_data$response,alternative="greater", exact=TRUE)$p.value,3)
    tests[j,6]<-ifelse(tests[j,3]<0.05,"Reject","Not Reject")
    tests[j,7]<-ifelse(tests[j,4]<0.05,"Reject","Not Reject")
    tests[j,8]<-ifelse(tests[j,5]<0.05,"Reject","Not Reject")
    tests[j,9]<-ifelse(tests[j,5]<0.05,"LLM","Human")
  }
}

# -MRG-3--------------------------------------------------------------------

filename_data <- paste0("game3_1120.csv")
paper_data<-read.csv(filename_data, header = TRUE)
for (llm in LLMs){
  j=j+1
  print(llm)
  filename_data <- paste0("llm_data/game3_1120_",llm, ".csv")
  mrg_data_full<-read.csv(filename_data, header = TRUE)
  mrg_data_full$y <- mrg_data_full$response
  mrg_data_full <- subset(mrg_data_full, mrg_data_full$response != "uncomfortable")
  mrg_data_full <- subset(mrg_data_full, mrg_data_full$response != "gender bias")
  mrg_data_full <- subset(mrg_data_full, mrg_data_full$act_as == "human decision maker")

  mrg_data_full <- subset(mrg_data_full, mrg_data_full$temp == 0.5)
  if (llm=="chatgpto1"){mrg_data_full<-read.csv(filename_data, header = TRUE)}
  tests[j,1] <- llm
  tests[j,2] <- "MRG-2"
  tests[j,3]<-round(ks.test(as.numeric(mrg_data_full$response),paper_data$response,alternative="two.sided", exact=TRUE)$p.value,3)
  tests[j,4]<-round(ks.test(as.numeric(mrg_data_full$response),paper_data$response,alternative="less", exact=TRUE)$p.value,3)
  tests[j,5]<-round(ks.test(as.numeric(mrg_data_full$response),paper_data$response,alternative="greater", exact=TRUE)$p.value,3)
  tests[j,6]<-ifelse(tests[j,3]<0.05,"Reject","Not Reject")
  tests[j,7]<-ifelse(tests[j,4]<0.05,"Reject","Not Reject")
  tests[j,8]<-ifelse(tests[j,5]<0.05,"Reject","Not Reject")
  tests[j,9]<-ifelse(tests[j,5]<0.05,"LLM","Human")
  if (llm=="claude3"){
    tests[j,3]<-round(ks.test(as.numeric(mrg_data_full$response[mrg_data_full$response>15]),paper_data$response,alternative="two.sided", exact=TRUE)$p.value,3)
    tests[j,4]<-round(ks.test(as.numeric(mrg_data_full$response[mrg_data_full$response>15]),paper_data$response,alternative="less", exact=TRUE)$p.value,3)
    tests[j,5]<-round(ks.test(as.numeric(mrg_data_full$response[mrg_data_full$response>15]),paper_data$response,alternative="greater", exact=TRUE)$p.value,3)
    tests[j,6]<-ifelse(tests[j,3]<0.05,"Reject","Not Reject")
    tests[j,7]<-ifelse(tests[j,4]<0.05,"Reject","Not Reject")
    tests[j,8]<-ifelse(tests[j,5]<0.05,"Reject","Not Reject")
    tests[j,9]<-ifelse(tests[j,5]<0.05,"LLM","Human")
  }
}

# - Median - HS-------------------------------------------------------------------

filename_data <- paste0("BCG_hs.csv")
paper_data<-read.csv(filename_data, header = TRUE)
for (llm in LLMs){
  j=j+1
  print(llm)
  filename_data <- paste0("llm_data/bcg_1shot_avgmed_",llm, ".csv")
  beauty_data_full<-read.csv(filename_data, header = TRUE)
  beauty_data_full$y <- beauty_data_full$response
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$response != "uncomfortable")
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$response != "gender bias")
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$act_as == "human decision maker")
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$avg == "median")

  beauty_data_full <- subset(beauty_data_full, beauty_data_full$temp == 0.5)
  if (llm=="chatgpto1"){beauty_data_full<-read.csv(filename_data, header = TRUE)}
  tests[j,1] <- llm
  tests[j,2] <- "pBCG - Median"
  tests[j,3]<-round(ks.test(as.numeric(beauty_data_full$response),paper_data$response,alternative="two.sided", exact=TRUE)$p.value,3)
  tests[j,4]<-round(ks.test(as.numeric(beauty_data_full$response),paper_data$response,alternative="less", exact=TRUE)$p.value,3)
  tests[j,5]<-round(ks.test(as.numeric(beauty_data_full$response),paper_data$response,alternative="greater", exact=TRUE)$p.value,3)
  tests[j,6]<-ifelse(tests[j,3]<0.05,"Reject","Not Reject")
  tests[j,7]<-ifelse(tests[j,4]<0.05,"Reject","Not Reject")
  tests[j,8]<-ifelse(tests[j,5]<0.05,"Reject","Not Reject")
  tests[j,9]<-ifelse(tests[j,5]<0.05,"LLM","Human")
}

# - Unspecified n - Bosch-------------------------------------------------------------------

filename_data <- paste0("BCG_bosch.csv")
paper_data<-read.csv(filename_data, header = TRUE)
for (llm in LLMs){
  j=j+1
  print(llm)
  filename_data <- paste0("llm_data/bcg_1shot_size_",llm, ".csv")
  beauty_data_full<-read.csv(filename_data, header = TRUE)
  beauty_data_full$y <- beauty_data_full$response
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$response != "uncomfortable")
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$response != "gender bias")
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$act_as == "human decision maker")
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$size == "unknown")

  beauty_data_full <- subset(beauty_data_full, beauty_data_full$temp == 0.5)
  if (llm=="chatgpto1"){beauty_data_full<-read.csv(filename_data, header = TRUE)}
  tests[j,1] <- llm
  tests[j,2] <- "pBCG - n=unspecified"
  tests[j,3]<-round(ks.test(as.numeric(beauty_data_full$response),paper_data$response,alternative="two.sided", exact=TRUE)$p.value,3)
  tests[j,4]<-round(ks.test(as.numeric(beauty_data_full$response),paper_data$response,alternative="less", exact=TRUE)$p.value,3)
  tests[j,5]<-round(ks.test(as.numeric(beauty_data_full$response),paper_data$response,alternative="greater", exact=TRUE)$p.value,3)
  tests[j,6]<-ifelse(tests[j,3]<0.05,"Reject","Not Reject")
  tests[j,7]<-ifelse(tests[j,4]<0.05,"Reject","Not Reject")
  tests[j,8]<-ifelse(tests[j,5]<0.05,"Reject","Not Reject")
  tests[j,9]<-ifelse(tests[j,5]<0.05,"LLM","Human")
}

# - n=2 - Grosskopf and Nagel -------------------------------------------------------------------

filename_data <- paste0("BCG_gn.csv")
paper_data<-read.csv(filename_data, header = TRUE)
for (llm in LLMs){
  j=j+1
  print(llm)
  filename_data <- paste0("llm_data/bcg_1shot_size_",llm, ".csv")
  beauty_data_full<-read.csv(filename_data, header = TRUE)
  beauty_data_full$y <- beauty_data_full$response
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$response != "uncomfortable")
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$response != "gender bias")
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$act_as == "human decision maker")
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$size == "2")
  
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$temp == 0.5)
  if (llm=="chatgpto1"){beauty_data_full<-read.csv(filename_data, header = TRUE)}
  tests[j,1] <- llm
  tests[j,2] <- "pBCG - n=2"
  tests[j,3]<-round(ks.test(as.numeric(beauty_data_full$response),paper_data$response,alternative="two.sided", exact=TRUE)$p.value,3)
  tests[j,4]<-round(ks.test(as.numeric(beauty_data_full$response),paper_data$response,alternative="less", exact=TRUE)$p.value,3)
  tests[j,5]<-round(ks.test(as.numeric(beauty_data_full$response),paper_data$response,alternative="greater", exact=TRUE)$p.value,3)
  tests[j,6]<-ifelse(tests[j,3]<0.05,"Reject","Not Reject")
  tests[j,7]<-ifelse(tests[j,4]<0.05,"Reject","Not Reject")
  tests[j,8]<-ifelse(tests[j,5]<0.05,"Reject","Not Reject")
  tests[j,9]<-ifelse(tests[j,5]<0.05,"LLM","Human")
}

# - p-vals (1/2) -Nagel-------------------------------------------------------------------

filename_data <- paste0("BCG_nagel.csv")
paper_data<-read.csv(filename_data, header = TRUE)
paper_data <- subset(paper_data, paper_data$p_val == "onehalf")
for (llm in LLMs){
  j=j+1
  print(llm)
  filename_data <- paste0("llm_data/bcg_1shot_pvals_",llm, ".csv")
  beauty_data_full<-read.csv(filename_data, header = TRUE)
  beauty_data_full$y <- beauty_data_full$response
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$response != "uncomfortable")
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$response != "gender bias")
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$act_as == "human decision maker")
  beauty_data_full$p_val[beauty_data_full$p_val=="2-Jan"] <- "onehalf"
  beauty_data_full$p_val[beauty_data_full$p_val=="3-Feb"] <- "twothirds"
  beauty_data_full$p_val[beauty_data_full$p_val=="3-Apr"] <- "fourthirds"
  beauty_data_full$p_val[beauty_data_full$p_val=="1/2"] <- "onehalf"
  beauty_data_full$p_val[beauty_data_full$p_val=="2/3"] <- "twothirds"
  beauty_data_full$p_val[beauty_data_full$p_val=="4/3"] <- "fourthirds"
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$p_val == "onehalf")

  beauty_data_full <- subset(beauty_data_full, beauty_data_full$temp == 0.5)
  if (llm=="chatgpto1"){beauty_data_full<-read.csv(filename_data, header = TRUE)}
  tests[j,1] <- llm
  tests[j,2] <- "pBCG - p=1/2"
  tests[j,3]<-round(ks.test(as.numeric(beauty_data_full$response),paper_data$response,alternative="two.sided", exact=TRUE)$p.value,3)
  tests[j,4]<-round(ks.test(as.numeric(beauty_data_full$response),paper_data$response,alternative="less", exact=TRUE)$p.value,3)
  tests[j,5]<-round(ks.test(as.numeric(beauty_data_full$response),paper_data$response,alternative="greater", exact=TRUE)$p.value,3)
  tests[j,6]<-ifelse(tests[j,3]<0.05,"Reject","Not Reject")
  tests[j,7]<-ifelse(tests[j,4]<0.05,"Reject","Not Reject")
  tests[j,8]<-ifelse(tests[j,5]<0.05,"Reject","Not Reject")
  tests[j,9]<-ifelse(tests[j,5]<0.05,"LLM","Human")
}

# - p-vals (2/3) -Nagel-------------------------------------------------------------------

filename_data <- paste0("BCG_nagel.csv")
paper_data<-read.csv(filename_data, header = TRUE)
paper_data <- subset(paper_data, paper_data$p_val == "twothirds")
for (llm in LLMs){
  j=j+1
  print(llm)
  filename_data <- paste0("llm_data/bcg_1shot_pvals_",llm, ".csv")
  beauty_data_full<-read.csv(filename_data, header = TRUE)
  beauty_data_full$y <- beauty_data_full$response
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$response != "uncomfortable")
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$response != "gender bias")
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$act_as == "human decision maker")
  beauty_data_full$p_val[beauty_data_full$p_val=="2-Jan"] <- "onehalf"
  beauty_data_full$p_val[beauty_data_full$p_val=="3-Feb"] <- "twothirds"
  beauty_data_full$p_val[beauty_data_full$p_val=="3-Apr"] <- "fourthirds"
  beauty_data_full$p_val[beauty_data_full$p_val=="1/2"] <- "onehalf"
  beauty_data_full$p_val[beauty_data_full$p_val=="2/3"] <- "twothirds"
  beauty_data_full$p_val[beauty_data_full$p_val=="4/3"] <- "fourthirds"
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$p_val == "twothirds")

  beauty_data_full <- subset(beauty_data_full, beauty_data_full$temp == 0.5)
  if (llm=="chatgpto1"){beauty_data_full<-read.csv(filename_data, header = TRUE)}
  tests[j,1] <- llm
  tests[j,2] <- "pBCG - p=2/3"
  tests[j,3]<-round(ks.test(as.numeric(beauty_data_full$response),paper_data$response,alternative="two.sided", exact=TRUE)$p.value,3)
  tests[j,4]<-round(ks.test(as.numeric(beauty_data_full$response),paper_data$response,alternative="less", exact=TRUE)$p.value,3)
  tests[j,5]<-round(ks.test(as.numeric(beauty_data_full$response),paper_data$response,alternative="greater", exact=TRUE)$p.value,3)
  tests[j,6]<-ifelse(tests[j,3]<0.05,"Reject","Not Reject")
  tests[j,7]<-ifelse(tests[j,4]<0.05,"Reject","Not Reject")
  tests[j,8]<-ifelse(tests[j,5]<0.05,"Reject","Not Reject")
  tests[j,9]<-ifelse(tests[j,5]<0.05,"LLM","Human")
}

# - p-vals (4/3) -Nagel-------------------------------------------------------------------

filename_data <- paste0("BCG_nagel.csv")
paper_data<-read.csv(filename_data, header = TRUE)
paper_data <- subset(paper_data, paper_data$p_val == "fourthirds")
for (llm in LLMs){
  j=j+1
  print(llm)
  filename_data <- paste0("llm_data/bcg_1shot_pvals_",llm, ".csv")
  beauty_data_full<-read.csv(filename_data, header = TRUE)
  beauty_data_full$y <- beauty_data_full$response
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$response != "uncomfortable")
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$response != "gender bias")
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$act_as == "human decision maker")
  beauty_data_full$p_val[beauty_data_full$p_val=="2-Jan"] <- "onehalf"
  beauty_data_full$p_val[beauty_data_full$p_val=="3-Feb"] <- "twothirds"
  beauty_data_full$p_val[beauty_data_full$p_val=="3-Apr"] <- "fourthirds"
  beauty_data_full$p_val[beauty_data_full$p_val=="1/2"] <- "onehalf"
  beauty_data_full$p_val[beauty_data_full$p_val=="2/3"] <- "twothirds"
  beauty_data_full$p_val[beauty_data_full$p_val=="4/3"] <- "fourthirds"
  beauty_data_full <- subset(beauty_data_full, beauty_data_full$p_val == "fourthirds")

  beauty_data_full <- subset(beauty_data_full, beauty_data_full$temp == 0.5)
  if (llm=="chatgpto1"){beauty_data_full<-read.csv(filename_data, header = TRUE)}
  tests[j,1] <- llm
  tests[j,2] <- "pBCG - p=4/3"
  tests[j,3]<-round(ks.test(as.numeric(beauty_data_full$response),paper_data$response,alternative="two.sided", exact=TRUE)$p.value,3)
  tests[j,4]<-round(ks.test(as.numeric(beauty_data_full$response),paper_data$response,alternative="less", exact=TRUE)$p.value,3)
  tests[j,5]<-round(ks.test(as.numeric(beauty_data_full$response),paper_data$response,alternative="greater", exact=TRUE)$p.value,3)
  tests[j,6]<-ifelse(tests[j,3]<0.05,"Reject","Not Reject")
  tests[j,7]<-ifelse(tests[j,4]<0.05,"Reject","Not Reject")
  tests[j,8]<-ifelse(tests[j,5]<0.05,"Reject","Not Reject")
  tests[j,9]<-ifelse(tests[j,5]<0.05,"Human","LLM")
}

# - GG-------------------------------------------------------------------

LLMs<-c("chatgpto1","claude3","gemini2")
filename_data <- paste0("SubjectsGuesses.csv")
paper_data<-read.csv(filename_data, header = TRUE)
paper_data<-paper_data[,c(1,2,10,3,11,4,12,5,13,6,14,7,15,8,16,9,17)]
paper_data<-paper_data[,c(1,1+c(6,15,14,10,9,2,12,3,16,11,4,13,8,1,7,5))]
targets_you<-as.numeric(list("0.7","0.5","0.5","0.7","1.5","1.3","1.3","1.3","0.5","1.5","0.7","1.3","0.7","1.5","0.7","1.5"))
targets_you<-targets_you[c(6,15,14,10,9,2,12,3,16,11,4,13,8,1,7,5)]
equil_responses_you <- as.matrix(c(100,100,150,300,500,650,900,900,100,150,300,390,500,750,350,500))
equil_responses_you <-equil_responses_you[c(6,15,14,10,9,2,12,3,16,11,4,13,8,1,7,5)]
for (llm in LLMs){
for (r in 2:17){
  j=j+1
  filename_data <- paste0("llm_data/gg_nofeedback_",llm, ".csv")
  beauty_data_full<-read.csv(filename_data, header = TRUE)
  tests[j,1] <- llm
  tests[j,2] <- paste("GG - ","Round ",r-1,sep="")
  tests[j,3]<-round(ks.test(as.numeric(beauty_data_full[,r]),paper_data[,r],alternative="two.sided", exact=TRUE)$p.value,3)
  tests[j,4]<-""
  tests[j,5]<-""
  tests[j,6]<-ifelse(tests[j,3]<0.05,"Reject","Not Reject")
  tests[j,7]<-""
  tests[j,8]<-""
  tests[j,9]<-ifelse(sum(as.numeric(beauty_data_full[,r])==equil_responses_you[r-1])>sum(as.numeric(paper_data[,r])==equil_responses_you[r-1]),"LLM","Human")
}
}

# - If 1-sided tests have same conclusion, then inconclusive evidence for more rational subjects --------------------------

exclude_rows <- paste0("GG - Round ", 1:16)
for (i in which(!(tests[, 2] %in% exclude_rows))) {
  if (tests[i, 7] == tests[i, 8]) {
    tests[i, 9] <- ""
  }
}

# - Make table for export --------------------------

llm_nice<-c('ChatGPT-4', 'ChatGPT-o1', 'Claude-2.1', 'Claude-3.5 (Sonnet)', 'Gemini-1.5 Flash', 'Gemini-2.0 Flash (Thinking)')
LLMs<-c("chatgpt4","chatgpto1","claude2","claude3","gemini1","gemini2")
llm_mapping <- setNames(llm_nice, LLMs)
tests[,1] <- llm_mapping[tests[,1]]
export_results <- tests
colnames(export_results)<- c("LLM","Game","H0: Two-sided","H0: Not Less","H0: Not Greater","two-sided","less","greater","More rational" )
export_results <- export_results[,c(1:5,9)]
index <- match(export_results[,1], llm_nice)
export_results <- export_results[order(index), ]

write.csv(export_results, file = "dist_tests.csv", row.names = TRUE)