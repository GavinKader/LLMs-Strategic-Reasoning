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
library(readxl)
library(ggpattern)
library(grid)
library(gridpattern)
library(Cairo)
library(data.table)
library(readr)
library(haven)
library(gridExtra)
library(patchwork)
#setwd("") Set working directory

# ------------------------------ BCG combined (normal temp) ------------------------------
# Load the dataset
file_path <- "Figures and Estimates/p-beauty/Summary of Results (one-shot) - pb - lk.csv"
df <- read.csv(file_path, header = TRUE)

# Filter dataset for 'alternative' graph
df_alternative <- df %>% filter(alternative == 1 | baseline ==1)
df_alternative$treatment[which(df_alternative$treatment=="2_1")]<-"2_0.5"
df_alternative$treatment[which(df_alternative$treatment=="unknown_1")]<-"unknown_0.5"
df_alternative$treatment[which(df_alternative$treatment=="median_1")]<-"median_0.5"
df_alternative$treatment[which(df_alternative$treatment=="onehalf_1")]<-"onehalf_0.5"
df_alternative$treatment[which(df_alternative$treatment=="fourthirds_1")]<-"fourthirds_0.5"
df_alternative$treatment[which(df_alternative$treatment=="twothirds_1")]<-"twothirds_0.5"

# Calculate the averages for p0 to p5 for each LLM and treatment
averages_alternative <- df_alternative %>%
  group_by(LLM, treatment) %>% summarise(across(p0:p5, mean, na.rm = TRUE)) %>% ungroup()

# Reshape data for ggplot
averages_long_alternative <- averages_alternative %>%
  pivot_longer(cols = p0:p5, names_to = "Parameter", values_to = "Average") %>%
  complete(LLM, treatment, Parameter, fill = list(Average = 0))

# Reverse the order of levels
averages_long_alternative$Parameter <- factor(averages_long_alternative$Parameter, levels = rev(unique(averages_long_alternative$Parameter)))
averages_long_alternative[,4][averages_long_alternative[,4]<1e-4] <- 0

labels_treatment <- c('2_0.5' = 'n=2', 'unknown_0.5' = 'n = Unspecified', 'median_0.5' = 'Median', 'onehalf_0.5' = 'p=1/2', 'fourthirds_0.5' = 'p=4/3', 'twothirds_0.5' = 'p=2/3 (baseline)')

# Reorder LLM and Treatment for plotting
averages_long_alternative$LLM <- factor(averages_long_alternative$LLM, levels = rev(unique(averages_long_alternative$LLM)))
averages_long_alternative$treatment <- factor(averages_long_alternative$treatment, levels = c('twothirds_0.5','onehalf_0.5', 'fourthirds_0.5','2_0.5', 'unknown_0.5', 'median_0.5'))

# Define labels for parameters
parameter_labels <- c('p0' = 'L0', 'p1' = 'L1', 'p2' = 'L2', 'p3' = 'L3', 'p4' = 'L4', 'p5' = expression('L' * infinity))
labels_llm <- rev(c('ChatGPT-4', 'ChatGPT-o1', 'Claude-2.1', 'Claude-3.5 (Sonnet)', 'Gemini-1.5 Flash', 'Gemini-2.0 Flash (Thinking)'))

# Plot the bar chart for each treatment
graph_lk_alternative <- ggplot(averages_long_alternative, aes(x = Average, y = Parameter)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(), aes(pattern_fill = LLM, pattern_type = LLM, pattern_spacing = LLM, pattern_angle = LLM, pattern_size = LLM), pattern_key_scale_factor = 0.5, pattern = 'polygon_tiling') +
  facet_wrap(~ treatment, scales = "free_y", ncol = 3, nrow = 2, labeller = labeller(treatment = labels_treatment)) +
  scale_pattern_fill_manual(values = rev(c('lightblue', 'darkorange', 'orchid1','grey', 'gold', 'green3')), labels = labels_llm, drop = TRUE) +
  scale_pattern_type_manual(values = rev(c('truncated_trihexagonal', 'hexagonal', 'square', '2*.2**.2*.2**', 'triangular', '18.18.3*')), labels = labels_llm) +
  scale_pattern_spacing_manual(values = rev(1.5*c(0.03,0.018,0.025,0.035,0.030,0.025)), labels = labels_llm) +
  scale_pattern_angle_manual(values = rev(c(0.02,0.022,0.05,0.05,90,0.025)), labels = labels_llm) +
  scale_pattern_size_manual(values = c(rep(0.2,6)), labels = labels_llm) +
  scale_y_discrete(labels = parameter_labels, limits = rev(names(parameter_labels))) +
  geom_hline(yintercept = seq(0.5, length(averages_long_alternative$LLM), by = 1), color="gray", size=.5, alpha=.5) +
  labs(x = "Average Proportions", y = NULL) +
  theme_minimal() +  
  theme(
    legend.position = "bottom",
    axis.title.y = element_text(margin = margin(t = 15)), 
    axis.title.x = element_text(margin = margin(t = 8)), 
    legend.key.size = unit(1, 'cm'), 
    legend.title = element_blank(),
    text = element_text(family = "LM Roman 10", size = 18),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 90)
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), expand = c(0.02, 0.02))

# Save and print the graph
ggsave(filename = "Figures and Estimates/Graphs/lk_pb.pdf", plot = graph_lk_alternative, height = 8 , width = 10, units = "in", dpi = 300, device=cairo_pdf)
#system("open 'Figures and Estimates/Graphs/lk_pb.pdf'")

# ------------------------------ BCG combined (low temp) ------------------------------
# Load the dataset
file_path <- "Figures and Estimates/p-beauty/Summary of Results (one-shot) - pb - lk.csv"
df <- read.csv(file_path, header = TRUE)

# Filter dataset for 'alternative' graph
df_alternative_templow <- df %>% filter(templow_alternative == 1 | templow_baseline ==1)
df_alternative_templow$treatment[which(df_alternative_templow$treatment=="2_1")]<-"2_0.25"
df_alternative_templow$treatment[which(df_alternative_templow$treatment=="unknown_1")]<-"unknown_0.25"
df_alternative_templow$treatment[which(df_alternative_templow$treatment=="median_1")]<-"median_0.25"
df_alternative_templow$treatment[which(df_alternative_templow$treatment=="onehalf_1")]<-"onehalf_0.25"
df_alternative_templow$treatment[which(df_alternative_templow$treatment=="fourthirds_1")]<-"fourthirds_0.25"
df_alternative_templow$treatment[which(df_alternative_templow$treatment=="twothirds_1")]<-"twothirds_0.25"
# Calculate the averages for p0 to p5 for each LLM and treatment
averages_alternative_templow <- df_alternative_templow %>%
  group_by(LLM, treatment) %>% summarise(across(p0:p5, mean, na.rm = TRUE)) %>% ungroup()

# Reshape data for ggplot
averages_long_alternative_templow <- averages_alternative_templow %>%
  pivot_longer(cols = p0:p5, names_to = "Parameter", values_to = "Average") %>%
  complete(LLM, treatment, Parameter, fill = list(Average = 0))

# Reverse the order of levels
averages_long_alternative_templow$Parameter <- factor(averages_long_alternative_templow$Parameter, levels = rev(unique(averages_long_alternative_templow$Parameter)))
averages_long_alternative_templow[,4][averages_long_alternative_templow[,4]<1e-4] <- 0

labels_treatment <- c('2_0.25' = 'n=2', 'unknown_0.25' = 'n = Unspecified', 'median_0.25' = 'Median', 'onehalf_0.25' = 'p=1/2', 'fourthirds_0.25' = 'p=4/3','twothirds_0.25' = 'p=2/3  (baseline)')

# Reorder LLM and Treatment for plotting
averages_long_alternative_templow$LLM <- factor(averages_long_alternative_templow$LLM, levels = rev(unique(averages_long_alternative_templow$LLM)))
averages_long_alternative_templow$treatment <- factor(averages_long_alternative_templow$treatment, levels = c('twothirds_0.25','onehalf_0.25', 'fourthirds_0.25','2_0.25', 'unknown_0.25', 'median_0.25'))

# Define labels for parameters
parameter_labels <- c('p0' = 'L0', 'p1' = 'L1', 'p2' = 'L2', 'p3' = 'L3', 'p4' = 'L4', 'p5' = expression('L' * infinity))
labels_llm <- rev(c('ChatGPT-4', 'ChatGPT-o1', 'Claude-2.1', 'Claude-3.5 (Sonnet)', 'Gemini-1.5 Flash', 'Gemini-2.0 Flash (Thinking)'))

# Plot the bar chart for each treatment
graph_lk_alternative_templow <- ggplot(averages_long_alternative_templow, aes(x = Average, y = Parameter)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(), aes(pattern_fill = LLM, pattern_type = LLM, pattern_spacing = LLM, pattern_angle = LLM, pattern_size = LLM), pattern_key_scale_factor = 0.5, pattern = 'polygon_tiling') +
  facet_wrap(~ treatment, scales = "free_y", ncol = 3, nrow = 2, labeller = labeller(treatment = labels_treatment)) +
  scale_pattern_fill_manual(values = rev(c('lightblue', 'darkorange', 'orchid','grey', 'gold', 'green3')), labels = labels_llm, drop = TRUE) +
  scale_pattern_type_manual(values = rev(c('truncated_trihexagonal', 'hexagonal', 'square', '2*.2**.2*.2**', 'triangular', '18.18.3*')), labels = labels_llm) +
  scale_pattern_spacing_manual(values = rev(1.5*c(0.03,0.018,0.025,0.035,0.030,0.025)), labels = labels_llm) +
  scale_pattern_angle_manual(values = rev(c(0.02,0.022,0.05,0.05,90,0.025)), labels = labels_llm) +
  scale_pattern_size_manual(values = c(rep(0.2,6)), labels = labels_llm) +
  scale_y_discrete(labels = parameter_labels, limits = rev(names(parameter_labels))) +
  geom_hline(yintercept = seq(0.5, length(averages_long_alternative_templow$LLM), by = 1), color="gray", size=.5, alpha=.5) +
  labs(x = "Average Proportions", y = NULL) +
  theme_minimal() +  
  theme(
    legend.position = "bottom",
    axis.title.y = element_text(margin = margin(t = 15)), 
    axis.title.x = element_text(margin = margin(t = 8)), 
    legend.key.size = unit(1, 'cm'), 
    legend.title = element_blank(),
    text = element_text(family = "LM Roman 10", size = 18),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 90)
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), expand = c(0.02, 0.02))

# Save and print the graph
ggsave(filename = "Figures and Estimates/Graphs/lk_pb_templow.pdf", plot = graph_lk_alternative_templow, height = 8 , width = 10, units = "in", dpi = 300, device=cairo_pdf)
system("open 'Figures and Estimates/Graphs/lk_pb_templow.pdf'")

# ------------------------------ BCG combined (high temp) ------------------------------
# Load the dataset
file_path <- "Figures and Estimates/p-beauty/Summary of Results (one-shot) - pb - lk.csv"
df <- read.csv(file_path, header = TRUE)

# Filter dataset for 'alternative' graph
df_alternative_temphigh <- df %>% filter(temphigh_alternative == 1 | temphigh_baseline ==1)
df_alternative_temphigh$treatment[which(df_alternative_temphigh$treatment=="2_1")]<-"2_0.75"
df_alternative_temphigh$treatment[which(df_alternative_temphigh$treatment=="unknown_1")]<-"unknown_0.75"
df_alternative_temphigh$treatment[which(df_alternative_temphigh$treatment=="median_1")]<-"median_0.75"
df_alternative_temphigh$treatment[which(df_alternative_temphigh$treatment=="onehalf_1")]<-"onehalf_0.75"
df_alternative_temphigh$treatment[which(df_alternative_temphigh$treatment=="fourthirds_1")]<-"fourthirds_0.75"
df_alternative_temphigh$treatment[which(df_alternative_temphigh$treatment=="twothirds_1")]<-"twothirds_0.75"

# Calculate the averages for p0 to p5 for each LLM and treatment
averages_alternative_temphigh <- df_alternative_temphigh %>%
  group_by(LLM, treatment) %>% summarise(across(p0:p5, mean, na.rm = TRUE)) %>% ungroup()

# Reshape data for ggplot
averages_long_alternative_temphigh <- averages_alternative_temphigh %>%
  pivot_longer(cols = p0:p5, names_to = "Parameter", values_to = "Average") %>%
  complete(LLM, treatment, Parameter, fill = list(Average = 0))

# Reverse the order of levels
averages_long_alternative_temphigh$Parameter <- factor(averages_long_alternative_temphigh$Parameter, levels = rev(unique(averages_long_alternative_temphigh$Parameter)))
averages_long_alternative_temphigh[,4][averages_long_alternative_temphigh[,4]<1e-4] <- 0

labels_treatment <- c('2_0.75' = 'n=2', 'unknown_0.75' = 'n = Unspecified', 'median_0.75' = 'Median', 'onehalf_0.75' = 'p=1/2', 'fourthirds_0.75' = 'p=4/3','twothirds_0.75' = 'p=2/3 (baseline)')

# Reorder LLM and Treatment for plotting
averages_long_alternative_temphigh$LLM <- factor(averages_long_alternative_temphigh$LLM, levels = rev(unique(averages_long_alternative_temphigh$LLM)))
averages_long_alternative_temphigh$treatment <- factor(averages_long_alternative_temphigh$treatment, levels = c('twothirds_0.75','onehalf_0.75', 'fourthirds_0.75','2_0.75', 'unknown_0.75', 'median_0.75'))

# Define labels for parameters
parameter_labels <- c('p0' = 'L0', 'p1' = 'L1', 'p2' = 'L2', 'p3' = 'L3', 'p4' = 'L4', 'p5' = expression('L' * infinity))
labels_llm <- rev(c('ChatGPT-4', 'ChatGPT-o1', 'Claude-2.1', 'Claude-3.5 (Sonnet)', 'Gemini-1.5 Flash', 'Gemini-2.0 Flash (Thinking)'))

# Plot the bar chart for each treatment
graph_lk_alternative_temphigh <- ggplot(averages_long_alternative_temphigh, aes(x = Average, y = Parameter)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(), aes(pattern_fill = LLM, pattern_type = LLM, pattern_spacing = LLM, pattern_angle = LLM, pattern_size = LLM), pattern_key_scale_factor = 0.5, pattern = 'polygon_tiling') +
  facet_wrap(~ treatment, scales = "free_y", ncol = 3, nrow = 2, labeller = labeller(treatment = labels_treatment)) +
  scale_pattern_fill_manual(values = rev(c('lightblue', 'darkorange', 'orchid','grey', 'gold', 'green3')), labels = labels_llm, drop = TRUE) +
  scale_pattern_type_manual(values = rev(c('truncated_trihexagonal', 'hexagonal', 'square', '2*.2**.2*.2**', 'triangular', '18.18.3*')), labels = labels_llm) +
  scale_pattern_spacing_manual(values = rev(1.5*c(0.03,0.018,0.025,0.035,0.030,0.025)), labels = labels_llm) +
  scale_pattern_angle_manual(values = rev(c(0.02,0.022,0.05,0.05,90,0.025)), labels = labels_llm) +
  scale_pattern_size_manual(values = c(rep(0.2,6)), labels = labels_llm) +
  scale_y_discrete(labels = parameter_labels, limits = rev(names(parameter_labels))) +
  geom_hline(yintercept = seq(0.5, length(averages_long_alternative_temphigh$LLM), by = 1), color="gray", size=.5, alpha=.5) +
  labs(x = "Average Proportions", y = NULL) +
  theme_minimal() +  
  theme(
    legend.position = "bottom",
    axis.title.y = element_text(margin = margin(t = 15)), 
    axis.title.x = element_text(margin = margin(t = 8)), 
    legend.key.size = unit(1, 'cm'), 
    legend.title = element_blank(),
    text = element_text(family = "LM Roman 10", size = 18),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 90)
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), expand = c(0.02, 0.02))

# Save and print the graph
ggsave(filename = "Figures and Estimates/Graphs/lk_pb_temphigh.pdf", plot = graph_lk_alternative_temphigh, height = 8 , width = 10, units = "in", dpi = 300, device=cairo_pdf)
#system("open 'Figures and Estimates/Graphs/lk_pb_temphigh.pdf'")


# ------------------------------ Combined Plot for Multiple Rounds (baseline) ------------------------------

# Set working directory
setwd("Figures and Estimates")

# Import data
data <- read.csv("p-beauty/Summary of Results (multiple).csv")

# Clean and prepare data
data <- data %>%
  mutate(
    p_val = case_when(
      p_val == "2/3" ~ "twothirds",
      p_val == "1/2" ~ "onehalf",
      p_val == "4/3" ~ "fourthirds",
      p_val == "3-Feb" ~ "twothirds",
      p_val == "2-Jan" ~ "onehalf",
      p_val == "3-Apr" ~ "fourthirds",
      TRUE ~ p_val
    ),
    LLM = case_when(
      LLM == "chatgpt4" ~ "ChatGPT-4",
      LLM == "chatgpt-o1" ~ "ChatGPT-o1",
      LLM == "claude2" ~ "Claude-2.1",
      LLM == "claude3" ~ "Claude-3.5 (Sonnet)",
      LLM == "gemini1" ~ "Gemini-1.5 Flash", 
      LLM == "gemini2" ~ "Gemini-2.0 Flash (Thinking)",
      TRUE ~ LLM
    ),
    temp = ifelse(LLM == "ChatGPT-o1", 0.5, temp)
  )

# Filter data for plotting (p = 2/3)
plot_data_23 <- data %>%
  filter(feedback == "feedback", random == "fixed", p_val == "twothirds", size == 11, temp == "0.5") %>%
  group_by(LLM) %>%
  summarise(across(starts_with("round"), list(mean = mean, sem = ~ sd(.) / sqrt(n())), .names = "{fn}_{col}")) %>%
  pivot_longer(cols = starts_with("mean_round"), names_to = "round", values_to = "mean_round") %>%
  pivot_longer(cols = starts_with("sem_round"), names_to = "sem_round_name", values_to = "sem_round") %>%
  filter(gsub("mean_round", "", round) == gsub("sem_round", "", sem_round_name)) %>%
  select(-sem_round_name)

plot_data_23$round_number <- as.numeric(gsub("mean_round", "", plot_data_23$round))
plot_data_23$upper_bound <- plot_data_23$mean_round + plot_data_23$sem_round
plot_data_23$lower_bound <- plot_data_23$mean_round - plot_data_23$sem_round

# Filter data for plotting (p = 4/3)
plot_data_43 <- data %>%
  filter(feedback == "feedback", random == "fixed", p_val == "fourthirds", size == 11, temp == "0.5") %>%
  group_by(LLM) %>%
  summarise(across(starts_with("round"), list(mean = mean, sem = ~ sd(.) / sqrt(n())), .names = "{fn}_{col}")) %>%
  pivot_longer(cols = starts_with("mean_round"), names_to = "round", values_to = "mean_round") %>%
  pivot_longer(cols = starts_with("sem_round"), names_to = "sem_round_name", values_to = "sem_round") %>%
  filter(gsub("mean_round", "", round) == gsub("sem_round", "", sem_round_name)) %>%
  select(-sem_round_name)

plot_data_43$round_number <- as.numeric(gsub("mean_round", "", plot_data_43$round))
plot_data_43$upper_bound <- plot_data_43$mean_round + plot_data_43$sem_round
plot_data_43$lower_bound <- plot_data_43$mean_round - plot_data_43$sem_round

# Shapes and colors
shapes <- c(16, 17, 15, 18, 3, 4)
colors <- rev(c('lightblue', 'darkorange', 'orchid', 'grey', 'gold', 'green3'))

# Plotting the first graph (p = 2/3)
p1 <- ggplot(plot_data_23, aes(x = round_number, y = mean_round, group = LLM)) +
  geom_point(aes(shape = LLM, color = LLM), size = 2) +
  geom_line(aes(color = LLM), linetype = "solid") +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound, color = LLM), width = 0.2) +
  scale_shape_manual(values = shapes, breaks = c('Gemini-2.0 Flash (Thinking)', 'Gemini-1.5 Flash','Claude-3.5 (Sonnet)', 'Claude-2.1', 'ChatGPT-o1', 'ChatGPT-4')) +
  scale_color_manual(values = colors, breaks = c('Gemini-2.0 Flash (Thinking)', 'Gemini-1.5 Flash','Claude-3.5 (Sonnet)', 'Claude-2.1', 'ChatGPT-o1', 'ChatGPT-4')) +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  labs(x = "Round", y = "Average") + ggtitle("p = 2/3") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 15)),
        legend.position = "none",
        text = element_text(family = "LM Roman 10", size = 15))

# Plotting the second graph (p = 4/3)
p2 <- ggplot(plot_data_43, aes(x = round_number, y = mean_round, group = LLM)) +
  geom_point(aes(shape = LLM, color = LLM), size = 2) +
  geom_line(aes(color = LLM), linetype = "solid") +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound, color = LLM), width = 0.2) +
  scale_shape_manual(values = shapes, breaks = c('Gemini-2.0 Flash (Thinking)', 'Gemini-1.5 Flash','Claude-3.5 (Sonnet)', 'Claude-2.1', 'ChatGPT-o1', 'ChatGPT-4')) +
  scale_color_manual(values = colors, breaks = c('Gemini-2.0 Flash (Thinking)', 'Gemini-1.5 Flash','Claude-3.5 (Sonnet)', 'Claude-2.1', 'ChatGPT-o1', 'ChatGPT-4')) +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  labs(x = "Round", y = "Average") + ggtitle("p = 4/3") +
  theme_minimal() +
  theme(
    text = element_text(family = "LM Roman 10", size = 15),
    axis.title.x = element_text(margin = margin(t = 15)),
    legend.position = "bottom",
    legend.justification = c(1, 0),
    legend.title = element_blank(),
    legend.box.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(hjust = 0.5),
    legend.key.size = unit(1.5, "lines"))+
  guides(
    color = guide_legend(override.aes = list(size = 3.5)),  # Increase the line width in the legend
    shape = guide_legend(override.aes = list(size = 3.5))   # Optionally, increase the point size in the legend
  )

# Combine the plots

combined_plot <- p1 + p2 + plot_layout(ncol = 2, guides = "collect") + plot_annotation(theme = theme(legend.position = "bottom"))

# Save the combined plot
ggsave("combined_feedback_plots_baseline.pdf", plot = combined_plot, path = "Figures and Estimates/Graphs", width = 10, height = 6, device = cairo_pdf)
system("open 'Figures and Estimates/Graphs/combined_feedback_plots_baseline.pdf'")



# ------------------------------ Combined Plot (low temp) ------------------------------

# Set working directory
setwd("Figures and Estimates")

# Import data
data <- read.csv("p-beauty/Summary of Results (multiple).csv")

# Clean and prepare data
data <- data %>%
  mutate(
    p_val = case_when(
      p_val == "2/3" ~ "twothirds",
      p_val == "1/2" ~ "onehalf",
      p_val == "4/3" ~ "fourthirds",
      p_val == "3-Feb" ~ "twothirds",
      p_val == "2-Jan" ~ "onehalf",
      p_val == "3-Apr" ~ "fourthirds",
      TRUE ~ p_val
    ),
    LLM = case_when(
      LLM == "chatgpt4" ~ "ChatGPT-4",
      LLM == "chatgpt-o1" ~ "ChatGPT-o1",
      LLM == "claude2" ~ "Claude-2.1",
      LLM == "claude3" ~ "Claude-3.5 (Sonnet)",
      LLM == "gemini1" ~ "Gemini-1.5 Flash", 
      LLM == "gemini2" ~ "Gemini-2.0 Flash (Thinking)",
      TRUE ~ LLM
    ),
    temp = ifelse(LLM == "ChatGPT-o1", 0.25, temp)
  )

# Filter data for plotting (p = 2/3)
plot_data_23 <- data %>%
  filter(feedback == "feedback", random == "fixed", p_val == "twothirds", size == 11, temp == "0.25") %>%
  group_by(LLM) %>%
  summarise(across(starts_with("round"), list(mean = mean, sem = ~ sd(.) / sqrt(n())), .names = "{fn}_{col}")) %>%
  pivot_longer(cols = starts_with("mean_round"), names_to = "round", values_to = "mean_round") %>%
  pivot_longer(cols = starts_with("sem_round"), names_to = "sem_round_name", values_to = "sem_round") %>%
  filter(gsub("mean_round", "", round) == gsub("sem_round", "", sem_round_name)) %>%
  select(-sem_round_name)

plot_data_23$round_number <- as.numeric(gsub("mean_round", "", plot_data_23$round))
plot_data_23$upper_bound <- plot_data_23$mean_round + plot_data_23$sem_round
plot_data_23$lower_bound <- plot_data_23$mean_round - plot_data_23$sem_round

# Filter data for plotting (p = 4/3)
plot_data_43 <- data %>%
  filter(feedback == "feedback", random == "fixed", p_val == "fourthirds", size == 11, temp == "0.25") %>%
  group_by(LLM) %>%
  summarise(across(starts_with("round"), list(mean = mean, sem = ~ sd(.) / sqrt(n())), .names = "{fn}_{col}")) %>%
  pivot_longer(cols = starts_with("mean_round"), names_to = "round", values_to = "mean_round") %>%
  pivot_longer(cols = starts_with("sem_round"), names_to = "sem_round_name", values_to = "sem_round") %>%
  filter(gsub("mean_round", "", round) == gsub("sem_round", "", sem_round_name)) %>%
  select(-sem_round_name)

plot_data_43$round_number <- as.numeric(gsub("mean_round", "", plot_data_43$round))
plot_data_43$upper_bound <- plot_data_43$mean_round + plot_data_43$sem_round
plot_data_43$lower_bound <- plot_data_43$mean_round - plot_data_43$sem_round

# Shapes and colors
shapes <- c(16, 17, 15, 18, 3, 4)
colors <- rev(c('lightblue', 'darkorange', 'orchid', 'grey', 'gold', 'green3'))

# Plotting the first graph (p = 2/3)
p1 <- ggplot(plot_data_23, aes(x = round_number, y = mean_round, group = LLM)) +
  geom_point(aes(shape = LLM, color = LLM), size = 2) +
  geom_line(aes(color = LLM), linetype = "solid") +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound, color = LLM), width = 0.2) +
  scale_shape_manual(values = shapes, breaks = c('Gemini-2.0 Flash (Thinking)', 'Gemini-1.5 Flash','Claude-3.5 (Sonnet)', 'Claude-2.1', 'ChatGPT-o1', 'ChatGPT-4')) +
  scale_color_manual(values = colors, breaks = c('Gemini-2.0 Flash (Thinking)', 'Gemini-1.5 Flash','Claude-3.5 (Sonnet)', 'Claude-2.1', 'ChatGPT-o1', 'ChatGPT-4')) +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  labs(x = "Round", y = "Average") + ggtitle("p = 2/3") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 15)),
        legend.position = "none",
        text = element_text(family = "LM Roman 10", size = 15))

# Plotting the second graph (p = 4/3)
p2 <- ggplot(plot_data_43, aes(x = round_number, y = mean_round, group = LLM)) +
  geom_point(aes(shape = LLM, color = LLM), size = 2) +
  geom_line(aes(color = LLM), linetype = "solid") +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound, color = LLM), width = 0.2) +
  scale_shape_manual(values = shapes, breaks = c('Gemini-2.0 Flash (Thinking)', 'Gemini-1.5 Flash','Claude-3.5 (Sonnet)', 'Claude-2.1', 'ChatGPT-o1', 'ChatGPT-4')) +
  scale_color_manual(values = colors, breaks = c('Gemini-2.0 Flash (Thinking)', 'Gemini-1.5 Flash','Claude-3.5 (Sonnet)', 'Claude-2.1', 'ChatGPT-o1', 'ChatGPT-4')) +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  labs(x = "Round", y = "Average") + ggtitle("p = 4/3") +
  theme_minimal() +
  theme(
    text = element_text(family = "LM Roman 10", size = 15),
    axis.title.x = element_text(margin = margin(t = 15)),
    legend.position = "bottom",
    legend.justification = c(1, 0),
    legend.title = element_blank(),
    legend.box.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(hjust = 0.5))+
  guides(
    color = guide_legend(override.aes = list(size = 3.5)),  # Increase the line width in the legend
    shape = guide_legend(override.aes = list(size = 3.5))   # Optionally, increase the point size in the legend
  )

# Combine the plots

combined_plot <- p1 + p2 + plot_layout(ncol = 2, guides = "collect") + plot_annotation(theme = theme(legend.position = "bottom"))

# Save the combined plot
ggsave("combined_feedback_plots_templow.pdf", plot = combined_plot, path = "Figures and Estimates/Graphs", width = 10, height = 6, device = cairo_pdf)
#system("open 'Figures and Estimates/Graphs/combined_feedback_plots_templow.pdf'")

# ------------------------------ Combined Plot (low temp) ------------------------------

# Set working directory
setwd("Figures and Estimates")

# Import data
data <- read.csv("p-beauty/Summary of Results (multiple).csv")

# Clean and prepare data
data <- data %>%
  mutate(
    p_val = case_when(
      p_val == "2/3" ~ "twothirds",
      p_val == "1/2" ~ "onehalf",
      p_val == "4/3" ~ "fourthirds",
      p_val == "3-Feb" ~ "twothirds",
      p_val == "2-Jan" ~ "onehalf",
      p_val == "3-Apr" ~ "fourthirds",
      TRUE ~ p_val
    ),
    LLM = case_when(
      LLM == "chatgpt4" ~ "ChatGPT-4",
      LLM == "chatgpt-o1" ~ "ChatGPT-o1",
      LLM == "claude2" ~ "Claude-2.1",
      LLM == "claude3" ~ "Claude-3.5 (Sonnet)",
      LLM == "gemini1" ~ "Gemini-1.5 Flash", 
      LLM == "gemini2" ~ "Gemini-2.0 Flash (Thinking)",
      TRUE ~ LLM
    ),
    temp = ifelse(LLM == "ChatGPT-o1", 0.75, temp)
  )

# Filter data for plotting (p = 2/3)
plot_data_23 <- data %>%
  filter(feedback == "feedback", random == "fixed", p_val == "twothirds", size == 11, temp == "0.75") %>%
  group_by(LLM) %>%
  summarise(across(starts_with("round"), list(mean = mean, sem = ~ sd(.) / sqrt(n())), .names = "{fn}_{col}")) %>%
  pivot_longer(cols = starts_with("mean_round"), names_to = "round", values_to = "mean_round") %>%
  pivot_longer(cols = starts_with("sem_round"), names_to = "sem_round_name", values_to = "sem_round") %>%
  filter(gsub("mean_round", "", round) == gsub("sem_round", "", sem_round_name)) %>%
  select(-sem_round_name)

plot_data_23$round_number <- as.numeric(gsub("mean_round", "", plot_data_23$round))
plot_data_23$upper_bound <- plot_data_23$mean_round + plot_data_23$sem_round
plot_data_23$lower_bound <- plot_data_23$mean_round - plot_data_23$sem_round

# Filter data for plotting (p = 4/3)
plot_data_43 <- data %>%
  filter(feedback == "feedback", random == "fixed", p_val == "fourthirds", size == 11, temp == "0.75") %>%
  group_by(LLM) %>%
  summarise(across(starts_with("round"), list(mean = mean, sem = ~ sd(.) / sqrt(n())), .names = "{fn}_{col}")) %>%
  pivot_longer(cols = starts_with("mean_round"), names_to = "round", values_to = "mean_round") %>%
  pivot_longer(cols = starts_with("sem_round"), names_to = "sem_round_name", values_to = "sem_round") %>%
  filter(gsub("mean_round", "", round) == gsub("sem_round", "", sem_round_name)) %>%
  select(-sem_round_name)

plot_data_43$round_number <- as.numeric(gsub("mean_round", "", plot_data_43$round))
plot_data_43$upper_bound <- plot_data_43$mean_round + plot_data_43$sem_round
plot_data_43$lower_bound <- plot_data_43$mean_round - plot_data_43$sem_round

# Shapes and colors
shapes <- c(16, 17, 15, 18, 3, 4)
colors <- rev(c('lightblue', 'darkorange', 'orchid', 'grey', 'gold', 'green3'))

# Plotting the first graph (p = 2/3)
p1 <- ggplot(plot_data_23, aes(x = round_number, y = mean_round, group = LLM)) +
  geom_point(aes(shape = LLM, color = LLM), size = 2) +
  geom_line(aes(color = LLM), linetype = "solid") +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound, color = LLM), width = 0.2) +
  scale_shape_manual(values = shapes, breaks = c('Gemini-2.0 Flash (Thinking)', 'Gemini-1.5 Flash','Claude-3.5 (Sonnet)', 'Claude-2.1', 'ChatGPT-o1', 'ChatGPT-4')) +
  scale_color_manual(values = colors, breaks = c('Gemini-2.0 Flash (Thinking)', 'Gemini-1.5 Flash','Claude-3.5 (Sonnet)', 'Claude-2.1', 'ChatGPT-o1', 'ChatGPT-4')) +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  labs(x = "Round", y = "Average") + ggtitle("p = 2/3") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 15)),
        legend.position = "none",
        text = element_text(family = "LM Roman 10", size = 15))

# Plotting the second graph (p = 4/3)
p2 <- ggplot(plot_data_43, aes(x = round_number, y = mean_round, group = LLM)) +
  geom_point(aes(shape = LLM, color = LLM), size = 2) +
  geom_line(aes(color = LLM), linetype = "solid") +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound, color = LLM), width = 0.2) +
  scale_shape_manual(values = shapes, breaks = c('Gemini-2.0 Flash (Thinking)', 'Gemini-1.5 Flash','Claude-3.5 (Sonnet)', 'Claude-2.1', 'ChatGPT-o1', 'ChatGPT-4')) +
  scale_color_manual(values = colors, breaks = c('Gemini-2.0 Flash (Thinking)', 'Gemini-1.5 Flash','Claude-3.5 (Sonnet)', 'Claude-2.1', 'ChatGPT-o1', 'ChatGPT-4')) +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  labs(x = "Round", y = "Average") + ggtitle("p = 4/3") +
  theme_minimal() +
  theme(
    text = element_text(family = "LM Roman 10", size = 15),
    axis.title.x = element_text(margin = margin(t = 15)),
    legend.position = "bottom",
    legend.justification = c(1, 0),
    legend.title = element_blank(),
    legend.box.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(hjust = 0.5))+
  guides(
    color = guide_legend(override.aes = list(size = 3.5)),  # Increase the line width in the legend
    shape = guide_legend(override.aes = list(size = 3.5))   # Optionally, increase the point size in the legend
  )

# Combine the plots

combined_plot <- p1 + p2 + plot_layout(ncol = 2, guides = "collect") + plot_annotation(theme = theme(legend.position = "bottom"))

# Save the combined plot
ggsave("combined_feedback_plots_temphigh.pdf", plot = combined_plot, path = "Figures and Estimates/Graphs", width = 10, height = 6, device = cairo_pdf)
#system("open 'Figures and Estimates/Graphs/combined_feedback_plots_temphigh.pdf'")


# -----------------MRG (combined) -----------------------
# Load the dataset
file_path <- "Figures and Estimates/MRG/Summary of Results (one-shot) - mrg - lk.csv"
df <- read.csv(file_path, header = TRUE)

# Filter dataset for 'baseline == 1', 'templow_baseline == 1', and 'temphigh_baseline == 1'
df_baseline_game1 <- df %>% filter(game == "game1", temperature == 0.5 | temperature==1)
df_baseline_game3 <- df %>% filter(game == "game3", temperature == 0.5 | temperature==1)

df_templow_game1 <- df %>% filter(game == "game1", temperature == 0.25 | temperature==1)
df_templow_game3 <- df %>% filter(game == "game3", temperature == 0.25 | temperature==1)

df_temphigh_game1 <- df %>% filter(game == "game1", temperature == 0.75 | temperature==1)
df_temphigh_game3 <- df %>% filter(game == "game3", temperature == 0.75 | temperature==1)


# Function to generate graphs for different filters
create_graphs <- function(df_game1, df_game3, filename) {
  # Calculate the averages for p0 to p5 for each LLM for game1 and game3
  averages_game1 <- df_game1 %>%
    group_by(LLM) %>% summarise(across(p0:p5, mean, na.rm = TRUE)) %>% ungroup()
  
  averages_game3 <- df_game3 %>%
    group_by(LLM) %>% summarise(across(p0:p5, mean, na.rm = TRUE)) %>% ungroup()
  
  # Reshape data for ggplot
  averages_long_game1 <- averages_game1 %>%
    pivot_longer(cols = p0:p5, names_to = "Parameter", values_to = "Average") %>% complete(LLM, Parameter, fill = list(Average = 0))
  averages_long_game1[,3][averages_long_game1[,3] < 1e-4] <- 0
  
  averages_long_game3 <- averages_game3 %>%
    pivot_longer(cols = p0:p5, names_to = "Parameter", values_to = "Average") %>% complete(LLM, Parameter, fill = list(Average = 0))
  averages_long_game3[,3][averages_long_game3[,3] < 1e-4] <- 0
  
  # Reorder LLM for plotting
  averages_long_game1$LLM <- factor(averages_long_game1$LLM, levels = rev(unique(averages_long_game1$LLM)))
  averages_long_game3$LLM <- factor(averages_long_game3$LLM, levels = rev(unique(averages_long_game3$LLM)))
  
  # Define labels for parameters
  parameter_labels <- c('p0' = 'Random', 'p1' = 'Level-0', 'p2' = 'Level-1', 'p3' = 'Level-2', 'p4' = 'Level-3', 'p5' = 'Level-4')
  labels_llm <- rev(c('ChatGPT-4', 'ChatGPT-o1', 'Claude-2.1', 'Claude-3.5 (Sonnet)', 'Gemini-1.5 Flash', 'Gemini-2.0 Flash (Thinking)'))
  
  # Plot the bar chart for game1
  plot_game1 <- ggplot(averages_long_game1, aes(x = Average, y = Parameter)) +
    geom_bar_pattern(stat = "identity", position = position_dodge(), aes(pattern_fill = LLM, pattern_type = LLM, pattern_spacing = LLM, pattern_angle = LLM, pattern_size = LLM), pattern_key_scale_factor = 0.8, pattern = 'polygon_tiling') +
    scale_pattern_fill_manual(values = rev(c('lightblue', 'darkorange', 'orchid', 'grey', 'gold', 'green3')), labels = labels_llm, drop = FALSE, guide = "legend") +
    scale_pattern_type_manual(values = rev(c('truncated_trihexagonal', 'hexagonal', 'square', '2*.2**.2*.2**', 'triangular', '18.18.3*')), labels = labels_llm) +
    scale_pattern_spacing_manual(values = rev(c(0.03, 0.018, 0.025, 0.03, 0.018, 0.025)), labels = labels_llm) +
    scale_pattern_angle_manual(values = rev(c(0.02, 0.022, 0.05, 0.05, 90, 0.025)), labels = labels_llm) +
    scale_pattern_size_manual(values = c(rep(0.2, 6)), labels = labels_llm) +
    scale_y_discrete(labels = parameter_labels, limits = rev(levels(factor(averages_long_game1$Parameter)))) +
    labs(y = "", x = "Average Proportions") +
    theme_minimal() +  
    theme(
      axis.title.y = element_text(margin = margin(t = 15)), 
      axis.title.x = element_text(margin = margin(t = 8)), 
      legend.position = "none",
      text = element_text(family = "LM Roman 10", size = 20),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), expand = c(0, 0)) +
    geom_hline(yintercept = seq(0.5, length(averages_long_game1$LLM), by = 1), color = "gray", size = .5, alpha = .5)
  
  # Plot the bar chart for game3
  plot_game3 <- ggplot(averages_long_game3, aes(x = Average, y = Parameter)) +
    geom_bar_pattern(stat = "identity", position = position_dodge(), aes(pattern_fill = LLM, pattern_type = LLM, pattern_spacing = LLM, pattern_angle = LLM, pattern_size = LLM), pattern_key_scale_factor = 0.8, pattern = 'polygon_tiling') +
    scale_pattern_fill_manual(values = rev(c('lightblue', 'darkorange', 'orchid', 'grey', 'gold', 'green3')), labels = labels_llm, drop = FALSE, guide = "legend") +
    scale_pattern_type_manual(values = rev(c('truncated_trihexagonal', 'hexagonal', 'square', '2*.2**.2*.2**', 'triangular', '18.18.3*')), labels = labels_llm) +
    scale_pattern_spacing_manual(values = rev(c(0.03, 0.018, 0.025, 0.03, 0.018, 0.025)), labels = labels_llm) +
    scale_pattern_angle_manual(values = rev(c(0.02, 0.022, 0.05, 0.05, 90, 0.025)), labels = labels_llm) +
    scale_pattern_size_manual(values = c(rep(0.2, 6)), labels = labels_llm) +
    scale_y_discrete(labels = parameter_labels, limits = rev(levels(factor(averages_long_game3$Parameter)))) +
    labs(y = "", x = "Average Proportions") +
    theme_minimal() +  
    theme(
      axis.title.y = element_text(margin = margin(t = 15)), 
      axis.title.x = element_text(margin = margin(t = 8)), 
      legend.position = "none",
      text = element_text(family = "LM Roman 10", size = 20),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), expand = c(0, 0)) +
    geom_hline(yintercept = seq(0.5, length(averages_long_game3$LLM), by = 1), color = "gray", size = .5, alpha = .5)
  
  # Combine the two plots side-by-side with a shared legend using patchwork
  final_plot <- (plot_game1 | plot_game3) +
    plot_layout(ncol = 2, guides = "collect") &
    theme(legend.position = "bottom", legend.title = element_blank(), legend.key.size = unit(1, 'cm'))
  
  # Save the combined plot
  ggsave(filename = filename, plot = final_plot, height = 8, width = 16, units = "in", dpi = 300, device = cairo_pdf)
  #system(paste("open", shQuote(filename)))
}

# Generate graphs for baseline, templow, and temphigh (keep temperature games combined) but MRG is needed for main text (game 1)
create_graphs(df_baseline_game1, df_baseline_game3, "Figures and Estimates/Graphs/lk_combined_baseline_mrg.pdf")
create_graphs(df_templow_game1, df_templow_game3, "Figures and Estimates/Graphs/lk_combined_templow_mrg.pdf")
create_graphs(df_temphigh_game1, df_temphigh_game3, "Figures and Estimates/Graphs/lk_combined_temphigh_mrg.pdf")


# -----------------MRG (game 1) -----------------------
# Load the dataset
file_path <- "Figures and Estimates/MRG/Summary of Results (one-shot) - mrg - lk.csv"
df <- read.csv(file_path, header = TRUE)

# Filter dataset for 'baseline == 0.5'
df_baseline_game1 <- df %>% filter(game == "game1", temperature == 0.5 | temperature==1)

  # Calculate the averages for p0 to p5 for each LLM for game1 and game3
  averages_game1 <- df_baseline_game1 %>%
    group_by(LLM) %>% summarise(across(p0:p5, mean, na.rm = TRUE)) %>% ungroup()

  # Reshape data for ggplot
  averages_long_game1 <- averages_game1 %>%
    pivot_longer(cols = p0:p5, names_to = "Parameter", values_to = "Average") %>% complete(LLM, Parameter, fill = list(Average = 0))
  averages_long_game1[,3][averages_long_game1[,3] < 1e-4] <- 0
  
  # Reorder LLM for plotting
  averages_long_game1$LLM <- factor(averages_long_game1$LLM, levels = rev(unique(averages_long_game1$LLM)))
  
  # Define labels for parameters
  parameter_labels <- c('p0' = 'Random', 'p1' = 'Level-0', 'p2' = 'Level-1', 'p3' = 'Level-2', 'p4' = 'Level-3', 'p5' = 'Level-4')
  labels_llm <- rev(c('ChatGPT-4', 'ChatGPT-o1', 'Claude-2.1', 'Claude-3.5 (Sonnet)', 'Gemini-1.5 Flash', 'Gemini-2.0 Flash (Thinking)'))
  
  # Plot the bar chart for game1
  plot_game1 <- ggplot(averages_long_game1, aes(x = Average, y = Parameter)) +
    geom_bar_pattern(stat = "identity", position = position_dodge(), aes(pattern_fill = LLM, pattern_type = LLM, pattern_spacing = LLM, pattern_angle = LLM, pattern_size = LLM), pattern_key_scale_factor = 0.8, pattern = 'polygon_tiling') +
    scale_pattern_fill_manual(values = rev(c('lightblue', 'darkorange', 'orchid', 'grey', 'gold', 'green3')), labels = labels_llm, drop = FALSE, guide = "legend") +
    scale_pattern_type_manual(values = rev(c('truncated_trihexagonal', 'hexagonal', 'square', '2*.2**.2*.2**', 'triangular', '18.18.3*')), labels = labels_llm) +
    scale_pattern_spacing_manual(values = rev(c(0.03, 0.018, 0.025, 0.03, 0.018, 0.025)), labels = labels_llm) +
    scale_pattern_angle_manual(values = rev(c(0.02, 0.022, 0.05, 0.05, 90, 0.025)), labels = labels_llm) +
    scale_pattern_size_manual(values = c(rep(0.2, 6)), labels = labels_llm) +
    scale_y_discrete(labels = parameter_labels, limits = rev(levels(factor(averages_long_game1$Parameter)))) +
    labs(y = "", x = "Average Proportions") +
    theme_minimal() +  
    theme(
      axis.title.y = element_text(margin = margin(t = 15)), 
      axis.title.x = element_text(margin = margin(t = 8)), 
      legend.key.size = unit(1, 'cm'), 
      legend.title = element_blank(),
      text = element_text(family = "LM Roman 10", size = 20),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), expand = c(0.02, 0.02)) +
    geom_hline(yintercept = seq(0.5, length(averages_long_game1$LLM), by = 1), color = "gray", size = .5, alpha = .5)
  
ggsave(filename = "Figures and Estimates/Graphs/lk_baseline_mrg1.pdf", plot = plot_game1, height = 6, width = 10, units = "in", dpi = 300, device=cairo_pdf)

# -----------------MRG (game 3) -----------------------
# Load the dataset
file_path <- "Figures and Estimates/MRG/Summary of Results (one-shot) - mrg - lk.csv"
df <- read.csv(file_path, header = TRUE)

# Filter dataset for 'baseline == 0.5'
df_baseline_game3 <- df %>% filter(game == "game3", temperature == 0.5 | temperature==1)

# Calculate the averages for p0 to p5 for each LLM for game3 and game3
averages_game3 <- df_baseline_game3 %>%
  group_by(LLM) %>% summarise(across(p0:p5, mean, na.rm = TRUE)) %>% ungroup()

# Reshape data for ggplot
averages_long_game3 <- averages_game3 %>%
  pivot_longer(cols = p0:p5, names_to = "Parameter", values_to = "Average") %>% complete(LLM, Parameter, fill = list(Average = 0))
averages_long_game3[,3][averages_long_game3[,3] < 1e-4] <- 0

# Reorder LLM for plotting
averages_long_game3$LLM <- factor(averages_long_game3$LLM, levels = rev(unique(averages_long_game3$LLM)))

# Define labels for parameters
parameter_labels <- c('p0' = 'Random', 'p1' = 'Level-0', 'p2' = 'Level-1', 'p3' = 'Level-2', 'p4' = 'Level-3', 'p5' = 'Level-4')
labels_llm <- rev(c('ChatGPT-4', 'ChatGPT-o1', 'Claude-2.1', 'Claude-3.5 (Sonnet)', 'Gemini-1.5 Flash', 'Gemini-2.0 Flash (Thinking)'))

# Plot the bar chart for game3
plot_game3 <- ggplot(averages_long_game3, aes(x = Average, y = Parameter)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(), aes(pattern_fill = LLM, pattern_type = LLM, pattern_spacing = LLM, pattern_angle = LLM, pattern_size = LLM), pattern_key_scale_factor = 0.8, pattern = 'polygon_tiling') +
  scale_pattern_fill_manual(values = rev(c('lightblue', 'darkorange', 'orchid', 'grey', 'gold', 'green3')), labels = labels_llm, drop = FALSE, guide = "legend") +
  scale_pattern_type_manual(values = rev(c('truncated_trihexagonal', 'hexagonal', 'square', '2*.2**.2*.2**', 'triangular', '18.18.3*')), labels = labels_llm) +
  scale_pattern_spacing_manual(values = rev(c(0.03, 0.018, 0.025, 0.03, 0.018, 0.025)), labels = labels_llm) +
  scale_pattern_angle_manual(values = rev(c(0.02, 0.022, 0.05, 0.05, 90, 0.025)), labels = labels_llm) +
  scale_pattern_size_manual(values = c(rep(0.2, 6)), labels = labels_llm) +
  scale_y_discrete(labels = parameter_labels, limits = rev(levels(factor(averages_long_game3$Parameter)))) +
  labs(y = "", x = "Average Proportions") +
  theme_minimal() +  
  theme(
    axis.title.y = element_text(margin = margin(t = 15)), 
    axis.title.x = element_text(margin = margin(t = 8)), 
    legend.key.size = unit(1, 'cm'), 
    legend.title = element_blank(),
    text = element_text(family = "LM Roman 10", size = 20),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), expand = c(0.02, 0.02)) +
  geom_hline(yintercept = seq(0.5, length(averages_long_game3$LLM), by = 1), color = "gray", size = .5, alpha = .5)

ggsave(filename = "Figures and Estimates/Graphs/lk_baseline_mrg3.pdf", plot = plot_game3, height = 6, width = 10, units = "in", dpi = 300, device=cairo_pdf)

# -----------------GG -----------------------

# Load the dataset
file_path <- "Figures and Estimates/GG/output_lk_allLLMs.csv"
df <- read.csv(file_path)

# Filter dataset for 'baseline' graph (assuming it requires a specific filtering step)
df_baseline <- df  # Modify filtering if needed, based on dataset specifics

# Calculate the averages for p0 to p5 for each LLM
averages_baseline <- df_baseline %>%
  group_by(LLM) %>% summarise(across(p0:p5, mean, na.rm = TRUE)) %>% ungroup()

# Extract confidence intervals from the first row of each LLM subset
ci_values_chatgpto1 <- df_baseline %>% filter(LLM == "chatgpto1") %>% slice(1) %>% select(starts_with("ci_"))
ci_values_claude3 <- df_baseline %>% filter(LLM == "claude3") %>% slice(1) %>% select(starts_with("ci_"))
ci_values_gemini2 <- df_baseline %>% filter(LLM == "gemini2") %>% slice(1) %>% select(starts_with("ci_"))

# Combine confidence intervals into a single dataframe
ci_long_baseline <- bind_rows(
  data.frame(
    LLM = "chatgpto1",
    Parameter = c("p0", "p1", "p2", "p3", "p4", "p5"),
    ci_lower = as.numeric(ci_values_chatgpto1[c("ci_lower_p0", "ci_lower_p1", "ci_lower_p2", "ci_lower_p3", "ci_lower_p4", "ci_lower_p5")]),
    ci_upper = as.numeric(ci_values_chatgpto1[c("ci_upper_p0", "ci_upper_p1", "ci_upper_p2", "ci_upper_p3", "ci_upper_p4", "ci_upper_p5")])
  ),
  data.frame(
    LLM = "claude3",
    Parameter = c("p0", "p1", "p2", "p3", "p4", "p5"),
    ci_lower = as.numeric(ci_values_claude3[c("ci_lower_p0", "ci_lower_p1", "ci_lower_p2", "ci_lower_p3", "ci_lower_p4", "ci_lower_p5")]),
    ci_upper = as.numeric(ci_values_claude3[c("ci_upper_p0", "ci_upper_p1", "ci_upper_p2", "ci_upper_p3", "ci_upper_p4", "ci_upper_p5")])
  ),
  data.frame(
    LLM = "gemini2",
    Parameter = c("p0", "p1", "p2", "p3", "p4", "p5"),
    ci_lower = as.numeric(ci_values_gemini2[c("ci_lower_p0", "ci_lower_p1", "ci_lower_p2", "ci_lower_p3", "ci_lower_p4", "ci_lower_p5")]),
    ci_upper = as.numeric(ci_values_gemini2[c("ci_upper_p0", "ci_upper_p1", "ci_upper_p2", "ci_upper_p3", "ci_upper_p4", "ci_upper_p5")])
  )
)

# Reshape data for ggplot
averages_long_baseline <- averages_baseline %>%
  pivot_longer(cols = p0:p5, names_to = "Parameter", values_to = "Average") %>% complete(LLM, Parameter, fill = list(Average = 0))
averages_long_baseline[,3][averages_long_baseline[,3]<1e-4] <- 0

# Merge confidence intervals with averages data
averages_long_baseline <- averages_long_baseline %>%
  left_join(ci_long_baseline, by = c("LLM", "Parameter"))

# Reorder LLM for plotting
averages_long_baseline$LLM <- factor(averages_long_baseline$LLM, levels = rev(unique(averages_long_baseline$LLM)))

# Define labels for parameters
parameter_labels <- c('p0' = 'Level-0', 'p1' = 'Level-1', 'p2' = 'Level-2', 'p3' = 'Level-3', 'p4' = 'Level-4', 'p5' = expression('Level-' * infinity))
labels_llm <- rev(unique(averages_long_baseline$LLM))
labels_llm <- ifelse(labels_llm == 'chatgpto1', 'ChatGPT-o1', ifelse(labels_llm == 'claude3', 'Claude-3.5 (Sonnet)', ifelse(labels_llm == 'gemini2', 'Gemini-2.0 Flash (Thinking)', labels_llm)))  # Update labels based on unique LLM names in the dataset

# Plot the bar chart for baseline
graph_lk_baseline <- ggplot(averages_long_baseline, aes(x = Average, y = Parameter)) +
  geom_bar_pattern(stat = "identity", width = 0.7, position = position_dodge(width=0.95), aes(pattern_fill = LLM, pattern_type = LLM, pattern_spacing = LLM, pattern_angle = LLM, pattern_size = LLM), pattern_key_scale_factor = 1, pattern = 'polygon_tiling') +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper, group = LLM), width = 0.2, position = position_dodge(width=0.95), color = 'black') +
  scale_pattern_fill_manual(values = rev(c('darkorange', 'grey', 'green3')), labels = labels_llm, drop = FALSE, guide = "legend") +
  scale_pattern_type_manual(values = rev(c('hexagonal','2*.2**.2*.2**', '18.18.3*')), labels = labels_llm) +
  scale_pattern_spacing_manual(values = rev(c(0.018, 0.03,0.025)), labels = labels_llm) +
  scale_pattern_angle_manual(values = rev(c(0.05, 0.025, 0.025)), labels = labels_llm) +
  scale_pattern_size_manual(values = rev(c(0.2,0.2,0.2)), labels = labels_llm) +
  scale_y_discrete(labels = parameter_labels, limits = rev(levels(factor(averages_long_baseline$Parameter)))) +
  labs(y = "", x = "Average Proportions") +
  theme_minimal() +  
  theme(
    axis.title.y = element_text(margin = margin(t = 15)), 
    axis.title.x = element_text(margin = margin(t = 8)), 
    legend.key.size = unit(1, 'cm'), 
    legend.title = element_blank(),
    text = element_text(family = "LM Roman 10", size = 20),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), expand = c(0, 0)) +
  geom_hline(yintercept = seq(0.5, length(averages_long_baseline$LLM), by = 1), color="gray", size=.5, alpha=.5)

ggsave(filename = "/lk_baseline_gg.pdf", plot = graph_lk_baseline, height = 6, width = 8, units = "in", dpi = 300, device=cairo_pdf)



