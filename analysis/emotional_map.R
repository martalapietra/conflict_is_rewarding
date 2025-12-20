# title: "The Experience of Cognitive Conflict is Intrinsically Rewarding (MANUSCRIPT)"
# author of the analysis script: Marta La Pietra
# date of creation: August 28, 2025
# data of update: December 11, 2025

#----------------------------------------------------------------------
# Install packages
install.packages("kableExtra")
install.packages("purrr")
install.packages("AICcmodavg")
install.packages("broom.mixed")
install.packages("ggrepel")
install.packages("rstatix")
install.packages("ggridges")
install.packages("here")
install.packages("tidytext")
install.packages("flextable")
install.packages("expss")
install.packages("openxlsx")
install.packages("xlsx")

library(here)         # relative paths
library(tidyverse)    # tidy functions
library(knitr)        # knit functions
library(kableExtra)   # extra markdown functions
library(purrr)        # map functions
library(lme4)         # mixed-effects regressions
library(lmerTest)     # mixed-effects regressions
library(AICcmodavg)   # predictSE()
library(broom.mixed)  # tidy()
library(ggrepel)      # geom_text_repel
library(sjPlot)       # tab_model
library(rstatix)      # cohen's d
library(ggridges)     # density plot
library(tidytext)
library(readxl)
library(flextable)
library(expss)
library(openxlsx)
library(xlsx)
library(dplyr)
library(rstatix)

knitr::opts_chunk$set(echo = FALSE, warnings = FALSE, message = FALSE)
load_models <- TRUE   # if you want to load the pre-trained models

## Data
# Specify relative paths
dir_analysis <- ("/GitHub/data/") # change according to your directory
dir_parent <- str_remove(dir_analysis, "/analysis")
dir_graphs <- str_c(dir_parent, "/graphs")

# Load
df_emotion <- read_excel(str_c(dir_analysis, "experiments_emotional_map.xlsx"))
df_experiment <- read_excel(str_c(dir_analysis, "experiments_emotions.xlsx"))

# Glimpse of data
head(df_emotion) %>%
  kable(caption = "Emotion Classification Glimpse") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

head(df_experiment) %>%
  kable(caption = "Experiment Glimpse") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# Emotion list
emotion_list <- c("neutral", "surprised", "aroused", "peppy", "enthusiastic", "happy", 
                  "satisfied", "relaxed", "calm", "sleepy", "still", "quiet", 
                  "sluggish", "sad", "disappointed", "disgusted", "annoyed", 
                  "angry", "afraid", "nervous")

plot_width = 6
plot_height = 6

fig0_data <- df_emotion %>%
  mutate(emotion = fct_relevel(emotion, emotion_list))

emotion_theme <- theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1, 
        axis.text.x = element_text(size = 22), 
        axis.text.y = element_text(size = 22),
        axis.title = element_text(size = 24), 
        legend.position = "none")

# fig0a - All points
fig0a_plot <- ggplot(df_emotion, aes(x = valence , y = arousal, color = emotion)) + 
  geom_point(alpha = .6) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  coord_fixed() + # square
  scale_y_continuous(name = "Arousal", breaks = seq(-200, 200, by = 100)) +
  scale_x_continuous(name = "Pleasantness", breaks = seq(-200, 200, by = 100)) +
  emotion_theme
fig0a_plot


fig0b_data <- df_emotion %>% 
  group_by(emotion) %>%
  dplyr::summarise(meanValence = mean(valence), meanArousal = mean(arousal),
                   sdValence = sd(valence), sdArousal = sd(arousal), N = n(), 
                   seValence = sdValence / sqrt(N), seArousal = sdArousal / sqrt(N)) %>%
  mutate(ciLowerValence = meanValence - qt(1 - (0.05 / 2), N - 1) * seValence,
         ciUpperValence = meanValence + qt(1 - (0.05 / 2), N - 1) * seValence, 
         ciLowerArousal = meanArousal - qt(1 - (0.05 / 2), N - 1) * seArousal,
         ciUpperArousal = meanArousal + qt(1 - (0.05 / 2), N - 1) * seArousal)

fig0b_plot <- ggplot(fig0_data, aes(x = valence, y = arousal, color = emotion)) + geom_point(alpha = .1) + # all data points
  geom_point(data = fig0b_data, aes(x = meanValence, y = meanArousal, color = emotion), size = 2) + # averages
  geom_errorbar(data = fig0b_data, aes(x = meanValence, y = meanArousal, ymin = ciLowerArousal, ymax = ciUpperArousal)) + # CIs
  geom_errorbarh(data = fig0b_data, aes(x = meanValence, y = meanArousal, xmin = ciLowerValence, xmax = ciUpperValence)) + # CIs
  geom_text_repel(data = fig0b_data, aes(x = meanValence, y = meanArousal, color = emotion, label = emotion), segment.colour = NA, force = 10, show.legend = FALSE, size = 5) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  coord_fixed() +
  scale_y_continuous(name = "Arousal", breaks = seq(-200, 200, by = 100)) +
  scale_x_continuous(name = "Pleasantness", breaks = seq(-200, 200, by = 100)) +
  emotion_theme
fig0b_plot

### fig 0 A & B
fig0_plot <- cowplot::plot_grid(fig0a_plot, fig0b_plot, labels = c("A", "B"), nrow = 1, rel_widths = c(3, 3))
ggsave(filename=str_c(dir_graphs, "/figure0/fig0a_b.pdf"), fig0_plot, width = 10, height = 5, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/figure0/fig0a_b.png"), fig0_plot, width = 10, height = 5)
fig0_plot


# If you want the colours used
colorfig <- ggplot(fig0b_data, aes(x = meanValence, y = meanArousal, color = emotion)) + 
  geom_point()
color <- ggplot_build(colorfig)
colorLabels <- unique(color$data[[1]]["colour"])

# fig0c - CONFLICT LEVELS
fig0c_data <- df_experiment %>%
  mutate(choice_fct = Conflict_Level)  # Directly assigning if it exists

fig0c_data$choice_fct <- factor(fig0c_data$choice_fct, levels = c("High","Medium","Low"))

fig0c_plot <- ggplot(fig0c_data, aes(x = valence, y = arousal, color = choice_fct)) + 
  geom_point(alpha = .5) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  coord_fixed() + # square
  scale_y_continuous(name = "Arousal", breaks = seq(-200, 200, by = 100)) +
  scale_x_continuous(name = "Pleasantness", breaks = seq(-200, 200, by = 100)) +
  scale_color_manual(values = c("#00BFC4",  "#FFB205", "#F64436")) +  #High, Medium, Low
  emotion_theme
fig0c_plot


fig0d <- df_experiment %>% 
  group_by(Conflict_Level) %>%
  dplyr::summarise(meanValence = mean(valence), meanArousal = mean(arousal),
                   sdValence = sd(valence), sdArousal = sd(arousal), N = n(), 
                   seValence = sdValence / sqrt(N), seArousal = sdArousal / sqrt(N)) %>%
  mutate(ciLowerValence = meanValence - qt(1 - (0.05 / 2), N - 1) * seValence,
         ciUpperValence = meanValence + qt(1 - (0.05 / 2), N - 1) * seValence, 
         ciLowerArousal = meanArousal - qt(1 - (0.05 / 2), N - 1) * seArousal,
         ciUpperArousal = meanArousal + qt(1 - (0.05 / 2), N - 1) * seArousal)

fig0d$Conflict_Level <- factor(fig0d$Conflict_Level, levels = c("High","Medium","Low"))

# Create the plot
fig0d_plot <- ggplot(fig0c_data, aes(x = valence, y = arousal, color = Conflict_Level)) + 
  geom_point(alpha = .1) +  # Plot all data points with low opacity
  geom_point(data = fig0d, aes(x = meanValence, y = meanArousal, color = Conflict_Level), size = 3) +  # Plot mean points
  geom_errorbar(data = fig0d, aes(x = meanValence, y = meanArousal, ymin = ciLowerArousal, ymax = ciUpperArousal), width = 0.1) +  # Vertical error bars (arousal)
  geom_errorbarh(data = fig0d, aes(x = meanValence, y = meanArousal, xmin = ciLowerValence, xmax = ciUpperValence), height = 0.1) +  # Horizontal error bars (valence)
  geom_text_repel(data = fig0d, aes(x = meanValence, y = meanArousal, label = Conflict_Level, color = Conflict_Level), 
                  segment.colour = NA, force = 10, show.legend = FALSE, size = 5) +  # Add labels for Conflict Levels
  geom_hline(yintercept = 0) +  # Add horizontal reference line
  geom_vline(xintercept = 0) +  # Add vertical reference line
  coord_fixed() +  # Ensure aspect ratio is square
  scale_y_continuous(name = "Arousal", breaks = seq(-200, 200, by = 100)) +  # Customize y-axis
  scale_x_continuous(name = "Pleasantness", breaks = seq(-200, 200, by = 100)) +  # Customize x-axis
  scale_color_manual(values = c("#00BFC4",  "#FFB205", "#F64436")) +  # Custom colors for Conflict Levels
  emotion_theme  # Apply custom theme
fig0d_plot

ggsave(filename = str_c(dir_graphs, "/figure0/fig0a.pdf"), plot=fig0a_plot, width = plot_width, height = plot_height)
ggsave(filename = str_c(dir_graphs, "/figure0/fig0a.png"), plot=fig0a_plot, width = plot_width, height = plot_height)
ggsave(filename = str_c(dir_graphs, "/figure0/fig0b.pdf"), plot=fig0b_plot, width = plot_width, height = plot_height)
ggsave(filename = str_c(dir_graphs, "/figure0/fig0b.png"), plot=fig0b_plot, width = plot_width, height = plot_height)
ggsave(filename = str_c(dir_graphs, "/figure0/fig0c.pdf"), plot=fig0c_plot, width = plot_width, height = plot_height)
ggsave(filename = str_c(dir_graphs, "/figure0/fig0c.png"), plot=fig0c_plot, width = plot_width, height = plot_height)
ggsave(filename=str_c(dir_graphs, "/figure0/fig0c_legend.png"), fig0c_plot + theme(legend.position = "right") + labs(color = "Incongruence Levels"), width = 6, height = 6)
ggsave(filename = str_c(dir_graphs, "/figure0/fig0d.png"), plot=fig0d_plot, width = plot_width, height = plot_height)
ggsave(filename = str_c(dir_graphs, "/figure0/fig0d.pdf"), plot=fig0d_plot, width = plot_width, height = plot_height)

################# MACHINE LEARNING APPROACHES
## Evaluate overall model accuracy (loading models takes a long time)
nn_fit <- readRDS(str_c(dir_analysis, "/nn_model.RDS"))
knn_fit <- readRDS(str_c(dir_analysis, "/knn_model.RDS"))
svm_fit <- readRDS(str_c(dir_analysis, "/svm_model.RDS"))

df_train <- read_csv(str_c(dir_analysis, "/df_train.csv"))
df_test <- read_csv(str_c(dir_analysis, "/df_train.csv"))

# Calculating out of sample accuracy
df_test_nn <- df_test %>%
  mutate(nn_class = predict(nn_fit, newdata = df_test, type = "raw"))
nn_cm <- caret::confusionMatrix(table(df_test_nn$nn_class, df_test_nn$emotion))

df_test_svm <- df_test %>%
  mutate(svm_class = predict(svm_fit, newdata = df_test, type = "raw"))
svm_cm <- caret::confusionMatrix(table(df_test_svm$svm_class, df_test_svm$emotion))

df_test_knn <- df_test %>%
  mutate(knn_class = predict(knn_fit, newdata = df_test, type = "raw"))
knn_cm <- caret::confusionMatrix(table(df_test_knn$knn_class, df_test_knn$emotion))

# Accuracy
nn_cm$overall
svm_cm$overall
knn_cm$overall

#####################################################
# Supplementary figure 4A (fig S4A): NN model view
nn_emo_space <- expand.grid(valence = seq(-250, 250, by = 1),
                            arousal = seq(-250, 250, by = 1))
nn_emo_space$class <- predict(nn_fit, newdata = nn_emo_space, type = "raw") # emo class

figS4a_plot <- ggplot() +
  geom_point(data = nn_emo_space, aes(x = valence, y = arousal, color = class), size = 1, alpha = 1) +
  scale_x_continuous(name = "Pleasantness", limits = c(-250, 250)) +
  scale_y_continuous(name = "Arousal", limits = c(-250, 250)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_color_discrete(name = "Predicted Emotion") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(), text = element_text(size = 14), aspect.ratio = 1)
figS4a_plot

ggsave(filename=str_c(dir_graphs, "/supplementary/model_vision/figureS4/figS4a_nn.pdf"), figS4a_plot + theme(legend.position = "none"), width = 6, height = 6, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/supplementary/model_vision/figureS4/figS4a_nn.png"), figS4a_plot + theme(legend.position = "none"), width = 6, height = 6)
ggsave(filename=str_c(dir_graphs, "/supplementary/model_vision/figureS4/figS4a_nn_legend.png"), figS4a_plot + theme(legend.position = "right"), width = 6, height = 8)


#rm(nn_fit) # optional

# Supplementary figure 4B (fig S4B):KNN model view
knn_emo_space <- expand.grid(valence = seq(-250, 250, by = 1), 
                             arousal = seq(-250, 250, by = 1))
knn_emo_space$class <- predict(knn_fit, newdata = knn_emo_space, type = "raw") # emo class

figS4b_plot <- ggplot() + 
  geom_point(data = knn_emo_space, aes(x = valence, y = arousal, color = class), size = 1, alpha = 1) + 
  scale_x_continuous(name = "Pleasantness", limits = c(-250, 250)) + 
  scale_y_continuous(name = "Arousal", limits = c(-250, 250)) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  scale_color_discrete(name = "Predicted Emotion") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.ticks = element_blank(), axis.text = element_blank(), text = element_text(size = 14), aspect.ratio = 1)
figS4b_plot

ggsave(filename=str_c(dir_graphs, "/supplementary/model_vision/figureS4/figS4b_knn.pdf"), figS4b_plot + theme(legend.position = "none"), width = 6, height = 6, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/supplementary/model_vision/figureS4/figS4b_knn.png"), figS4b_plot + theme(legend.position = "none"), width = 6, height = 6)
ggsave(filename=str_c(dir_graphs, "/supplementary/model_vision/figureS4/figS4b_knn_legend.png"), figS4b_plot + theme(legend.position = "right"), width = 6, height = 6)

# Supplementary figure 4C (fig S4C):SVM model view
svm_emo_space <- expand.grid(valence = seq(-250, 250, by = 1),
                             arousal = seq(-250, 250, by = 1))
svm_emo_space$class <- predict(svm_fit, newdata = svm_emo_space, type = "raw") # emo class

figS4c_plot <- ggplot() +
  geom_point(data = svm_emo_space, aes(x = valence, y = arousal, color = class), size = 1, alpha = 1) +
  scale_x_continuous(name = "Pleasantness", limits = c(-250, 250)) +
  scale_y_continuous(name = "Arousal", limits = c(-250, 250)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_color_discrete(name = "Predicted Emotion") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(), text = element_text(size = 14), aspect.ratio = 1)
figS4c_plot

ggsave(filename=str_c(dir_graphs, "/supplementary/model_vision/figureS4/figS4c_svm.pdf"), figS4c_plot + theme(legend.position = "none"), width = 6, height = 6, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/supplementary/model_vision/figureS4/figS4c_svm.png"), figS4c_plot + theme(legend.position = "none"), width = 6, height = 6)
ggsave(filename=str_c(dir_graphs, "/supplementary/model_vision/figureS4/figS4c_svm_legend.png"), figS4c_plot + theme(legend.position = "right"), width = 6, height = 6)

# All models
figS4_plot <- cowplot::plot_grid(figS4a_plot+ theme(legend.position = "none"), figS4b_plot+ theme(legend.position = "none"), figS4c_plot + theme(legend.position = "none"), labels = c("A", "B", "C"), nrow = 1, rel_widths = c(6, 6))
ggsave(filename=str_c(dir_graphs, "/supplementary/figureS4/figS4.pdf"), figS4_plot, width = 15, height = 5, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/supplementary/figureS4/figS4.png"), figS4_plot, width = 15, height = 5)
figS4_plot

#########################################################################
# Aesthetics
emotion_list <- c("neutral", "surprised", "aroused", "peppy", "enthusiastic", "happy", 
                   "satisfied", "relaxed", "calm", "sleepy", "still", "quiet", 
                   "sluggish", "sad", "disappointed", "disgusted", "annoyed", 
                   "angry", "afraid", "nervous")
                  
emotion_theme <- theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        aspect.ratio = 1, 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 24), 
        legend.position = "none")

# fig00 data
fig00_data <- df_emotion %>%
  mutate(emotion = fct_relevel(emotion, emotion_list)) %>% ungroup()

# figure00a - Contour Plots (separate density levels per plot)
fig00a_list <- list()

# Loop through each emotion dataset and make a contour plot
for (i in 1:length(emotion_list)) {
  # Filter data
  plot_data <- fig00_data %>% filter(emotion == emotion_list[i]) 
  
  # Plot
  plot <- ggplot(plot_data,  aes(x = valence, y = arousal, color = emotion)) + 
    geom_density_2d() + 
    geom_hline(yintercept = 0, color = "black") + 
    geom_vline(xintercept = 0, color = "black") +
    scale_color_manual(values = colorLabels$colour[i]) +  # pull colors from figure 1
    coord_fixed(ratio = 1) +
    scale_y_continuous(limits = c(-250, 250)) + 
    scale_x_continuous(limits = c(-250, 250)) + 
    #scale_y_continuous(name = "Arousal", breaks = seq(-200, 200, by = 100)) +
    #scale_x_continuous(name = "Pleasantness", breaks = seq(-200, 200, by = 100)) +
    ggtitle(emotion_list[i]) + 
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(-.1, -.1, -.1, -.1), "cm"),  # plot margins (top, right, bottom, left)
          plot.title = element_text(hjust = 0.5, vjust = -1.5),   # center title, put on top of graph
          axis.text = element_blank(), 
          axis.ticks = element_blank(),
          axis.title = element_blank(), 
          legend.position = "none")
  
  # Add to list
  fig00a_list[[i]] <- ggplotGrob(plot)
}

fig00a_plot <- cowplot::plot_grid(plotlist=fig00a_list, ncol=4)
fig00a_plot
ggsave(filename = str_c(dir_graphs, "/figure0/fig0a.pdf"), plot=fig00a_plot, width = 6, height = 6)

# figure00b - 1D density
fig00b_plot <- ggplot(fig00_data %>% pivot_longer(cols = c(valence, arousal), names_to = "measure", values_to = "value"),
                     aes(x = value, y = emotion, fill = emotion)) +
  ggridges::geom_density_ridges(scale = 3, alpha = 3/4, bandwidth = 18) +  # tweek to visualize
  scale_x_continuous() +
  facet_wrap(~measure) +
  theme_bw(base_size=9) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
fig00b_plot
ggsave(filename = str_c(dir_graphs, "/figure00/fig00b.pdf"), plot=fig00b_plot, width = 6, height = 8)


### fig00A & fig00B
fig00a_b_plot <- cowplot::plot_grid(fig00a_plot, fig00b_plot, labels = c("A", "B"), nrow = 1, rel_widths = c(9, 7))
ggsave(filename=str_c(dir_graphs, "/figure00/fig00a_b_plot.pdf"), fig00a_b_plot, width = 10, height = 5, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/figure00/fig00a_b_plot.png"), fig00a_b_plot, width = 10, height = 5)
fig00a_b_plot

#############################################################
# figure 6 - NN Emotion Classifications
# Requires model 
dir_analysis_models <- ("/GitHub/models/")
nn_fit <- readRDS(str_c(dir_analysis_models, "/nn_model.RDS"))
experiment_nn <- predict(nn_fit, newdata = df_experiment, type = "prob")
df_experiment_probs_nn <- df_experiment %>% bind_cols(experiment_nn)
rm(nn_fit) # helpful for memory

df_experiment_probs_nn$Conflict_Level <- factor(df_experiment_probs_nn$Conflict_Level, levels = c("High","Medium","Low"))

# levels(df_experiment_probs_nn$Conflict_Level) <- c("High conflict", "Medium conflict", "Low conflict")


# fig6 data
fig6a_nn_data <- df_experiment_probs_nn %>%
  select(sub, Conflict_Level, afraid:surprised) %>%  # Include Conflict_Level in the selection
  pivot_longer(cols = -c(sub, Conflict_Level), names_to = "nn_emotion", values_to = "nn_prob") %>%
  group_by(Conflict_Level, nn_emotion, sub) %>%  # Group by Conflict_Level 
  summarise(mean_prob_sub = mean(nn_prob), .groups = 'drop_last') %>%
  group_by(Conflict_Level, nn_emotion) %>%  # Group by Conflict_Level 
  summarise(mean_prob = mean(mean_prob_sub), sd_prob = sd(mean_prob_sub), N = n(), se_prob = sd_prob / sqrt(N), .groups = 'drop') %>%
  mutate(lwr = mean_prob - qt(1 - (0.05 / 2), N - 1) * se_prob,
         upr = mean_prob + qt(1 - (0.05 / 2), N - 1) * se_prob) %>%
  # Add numeric labels
  mutate(label = str_c(as.character(round(mean_prob*100, 1)), "%"), 
         y_label = mean_prob + .04)

# Filter the data and calculate the mean and standard deviation, rounded to 2 decimals
stats <- fig6a_nn_data %>%
  filter(Conflict_Level == "Medium") %>%
  filter(nn_emotion == "peppy") %>%
  summarize(
    mean = round(mean(mean_prob, na.rm = TRUE), 4),
    sd = round(mean(sd_prob, na.rm = TRUE), 4)
  )

# Extract and print the values individually
mean <- stats$mean*100
sd <- stats$sd*100

# Print with two decimals
cat("Mean:", mean, "\n")
cat("Standard Deviation:", sd, "\n")

#### figure 6a
# fig6a_nn plot - probabilities by choice
label_size <- 4
label_color <- "black"
fig6a_nn_plot <- ggplot(fig6a_nn_data, aes(x = tidytext::reorder_within(nn_emotion, mean_prob, Conflict_Level), y = mean_prob, fill = nn_emotion)) + 
  geom_col(show.legend = FALSE) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .12) + 
  geom_text(aes(label = label, y = y_label), color = label_color, size = label_size) + 
  facet_wrap(~Conflict_Level, scales = "free_y",
             labeller = as_labeller(c(
               "High" = "High conflict",
               "Medium" = "Medium conflict",
               "Low" = "Low conflict"
             ))) +   
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L), name = "Model Likelihood", limits = c(0, .20)) +
  coord_flip() + 
  tidytext::scale_x_reordered(name = "Neural Network Emotion") + 
  theme_classic() + 
  theme(text = element_text(size = 14))
fig6a_nn_plot

ggsave(filename=str_c(dir_graphs, "/figure6/fig6a_nn.pdf"), fig6a_nn_plot, width = 11, height = 4, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/figure6/fig6a_nn.png"), fig6a_nn_plot, width = 11, height = 4)

# -------------------------------- figure 6B 
# DELTA HIGH VS. LOW CONFLICT
fig6b_nn_data <- df_experiment_probs_nn %>%
  select(sub, Conflict_Level, afraid:surprised) %>%
  pivot_longer(cols = -c(sub, Conflict_Level), names_to = "nn_emotion", values_to = "nn_prob") %>%
  group_by(Conflict_Level, nn_emotion, sub) %>%
  summarise(mean_prob_sub = mean(nn_prob)) %>%
  group_by(Conflict_Level, nn_emotion) %>% 
  summarise(mean_prob = mean(mean_prob_sub), sd_prob = sd(mean_prob_sub), N = n(), se_prob = sd_prob / sqrt(N)) %>%
  mutate(lwr = mean_prob - qt(1 - (0.05 / 2), N - 1) * se_prob,
         upr = mean_prob + qt(1 - (0.05 / 2), N - 1) * se_prob) %>%
  # Delta
  select(Conflict_Level, nn_emotion, mean_prob) %>%
  pivot_wider(names_from = Conflict_Level, values_from = mean_prob) %>% 
  group_by(nn_emotion) %>% 
  mutate(delta = High - Low) %>%
  # Add numeric labels
  mutate(label = str_c(as.character(round(delta*100, 1)), "%"), 
         y_label = if_else(delta >= 0, delta + .01, delta - .01)) #play with this to change the position of the %

fig6b_nn_data <- df_experiment_probs_nn %>%
  select(sub, Conflict_Level, afraid:surprised) %>%
  pivot_longer(cols = -c(sub, Conflict_Level), names_to = "nn_emotion", values_to = "nn_prob") %>%
  group_by(Conflict_Level, nn_emotion, sub) %>%
  summarise(mean_prob_sub = mean(nn_prob)) %>%
  group_by(Conflict_Level, nn_emotion) %>% 
  summarise(mean_prob = mean(mean_prob_sub), sd_prob = sd(mean_prob_sub), N = n(), se_prob = sd_prob / sqrt(N)) %>%
  mutate(lwr = mean_prob - qt(1 - (0.05 / 2), N - 1) * se_prob,
         upr = mean_prob + qt(1 - (0.05 / 2), N - 1) * se_prob) %>%
  # Delta
  select(Conflict_Level, nn_emotion, mean_prob) %>%
  pivot_wider(names_from = Conflict_Level, values_from = mean_prob) %>% 
  group_by(nn_emotion) %>% 
  mutate(delta = High - Low) %>%
  # Add numeric labels
  mutate(label = str_c(as.character(round(delta*100, 1)), "%"), 
         y_label = if_else(delta >= 0, delta + .01, delta - .01)) #play with this to change the position of the %

# fig 6b Plot
fig6b_nn_plot <- ggplot(fig6b_nn_data, aes(x = reorder(nn_emotion, delta), y = delta, fill = nn_emotion)) + 
  geom_col(show.legend = FALSE, position = position_dodge(.9)) + 
  geom_text(aes(label = label, y = y_label), color = label_color, size = label_size) + 
  # geom_errorbar(aes(ymin = lwr, ymax = upr), width = .12) +
  scale_y_continuous(labels = scales::percent, name = "Δ Model Likelihood (High - Low Conflict)", expand = c(.03, .03)) +
  xlab("NN Emotion") + 
  theme_classic() + 
  coord_flip() + 
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    text = element_text(size = 16)  # Move legend to the top
  )
fig6b_nn_plot

ggsave(filename=str_c(dir_graphs, "/figure6/fig6b_nn.pdf"), fig6b_nn_plot, width = 11, height = 4, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/figure6/fig6b_nn.png"), fig6b_nn_plot, width = 6, height = 6)

# WITH SEM
fig6b_nn_data_SEM <- df_experiment_probs_nn %>%
  select(sub, Conflict_Level, afraid:surprised) %>%
  pivot_longer(cols = -c(sub, Conflict_Level), names_to = "nn_emotion", values_to = "nn_prob") %>%
  # First, calculate subject-level means
  group_by(Conflict_Level, nn_emotion, sub) %>%
  summarise(mean_prob_sub = mean(nn_prob), .groups = "drop") %>%
  # Then calculate group statistics (preserving N)
  group_by(Conflict_Level, nn_emotion) %>%
  summarise(
    mean_prob = mean(mean_prob_sub),
    sd_prob = sd(mean_prob_sub),
    N = n(),
    se_prob = sd_prob / sqrt(N),
    .groups = "drop"
  ) %>%
  # Pivot wider while keeping all necessary columns
  pivot_wider(
    names_from = Conflict_Level,
    values_from = c(mean_prob, sd_prob, N, se_prob),
    names_sep = "."
  ) %>%
  # Calculate delta and its SEM
  mutate(
    delta = mean_prob.High - mean_prob.Low,
    delta_se = sqrt(se_prob.High^2 + se_prob.Low^2),
    N_combined = min(N.High, N.Low),  # Use the smaller N for t-distribution
    delta_lwr = delta - qt(1 - (0.05 / 2), N_combined - 1) * delta_se,
    delta_upr = delta + qt(1 - (0.05 / 2), N_combined - 1) * delta_se,
    # Add labels
    label = str_c(as.character(round(delta*100, 1)), "%"),
    y_label = if_else(delta >= 0, delta + 0.028, delta - 0.025)
  )

fig6b_nn_plot_SEM <- ggplot(fig6b_nn_data_SEM, aes(x = reorder(nn_emotion, delta), y = delta, fill = nn_emotion)) + 
  geom_col(show.legend = FALSE) + 
  geom_errorbar(aes(ymin = delta_lwr, ymax = delta_upr), width = 0.2, color = "black") +
  geom_text(aes(label = label, y = y_label), color = label_color, size = label_size) + 
  scale_y_continuous(labels = scales::percent, name = "Δ Model Likelihood (High - Low Conflict)") +
  xlab("Neural Network Emotion") + 
  coord_flip() +
  theme_classic() + 
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black", margin = margin(t = 0, r = 15, b = 0, l = 0)),
    text = element_text(size = 16)  # Move legend to the top
  )
fig6b_nn_plot_SEM

ggsave(filename=str_c(dir_graphs, "/figure6/fig6b_nn_SEM.pdf"), fig6b_nn_plot_SEM, width = 11, height = 4, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/figure6/fig6b_nn_SEM_1.png"), fig6b_nn_plot_SEM, width = 7, height = 6)

# Filter the data and calculate the mean and standard deviation, rounded to 2 decimals
stats <- fig6b_nn_data %>%
  filter(nn_emotion == "quiet") %>%
  summarize(
    mean = round(mean(delta, na.rm = TRUE), 4),
    sd = round(sd(delta, na.rm = TRUE), 4)
  )

# Extract and print the values individually
mean <- stats$mean*100

# Print with two decimals
cat("Delta %:", mean, "\n")

#--------------------------------------------------------------------------
# DELTA HIGH VS. MEDIUM
### Delta (High - Medium) by Emotion
fig6c_nn_data <- df_experiment_probs_nn %>%
  select(sub, Conflict_Level, afraid:surprised) %>%
  pivot_longer(cols = -c(sub, Conflict_Level), names_to = "nn_emotion", values_to = "nn_prob") %>%
  group_by(Conflict_Level, nn_emotion, sub) %>%
  summarise(mean_prob_sub = mean(nn_prob)) %>%
  group_by(Conflict_Level, nn_emotion) %>% 
  summarise(mean_prob = mean(mean_prob_sub), sd_prob = sd(mean_prob_sub), N = n(), se_prob = sd_prob / sqrt(N)) %>%
  mutate(lwr = mean_prob - qt(1 - (0.05 / 2), N - 1) * se_prob,
         upr = mean_prob + qt(1 - (0.05 / 2), N - 1) * se_prob) %>%
  # Delta
  select(Conflict_Level, nn_emotion, mean_prob) %>%
  pivot_wider(names_from = Conflict_Level, values_from = mean_prob) %>% 
  group_by(nn_emotion) %>% 
  mutate(delta = High - Medium) %>%
  # Add numeric labels
  mutate(label = str_c(as.character(round(delta*100, 1)), "%"), 
         y_label = if_else(delta >= 0, delta + .01, delta - .01)) #play with this to change the position of the %

# PLOT
fig6c_nn_plot <- ggplot(fig6c_nn_data, aes(x = reorder(nn_emotion, delta), y = delta, fill = nn_emotion)) + 
  geom_col(show.legend = FALSE, position = position_dodge(.9)) + 
  geom_text(aes(label = label, y = y_label), color = label_color, size = label_size) + 
  # geom_errorbar(aes(ymin = lwr, ymax = upr), width = .12) +
  scale_y_continuous(labels = scales::percent, name = "Δ Model Likelihood (High - Medium Conflict)", expand = c(.03, .03)) +
  xlab("NN Emotion") + 
  theme_classic() + 
  coord_flip() + 
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    text = element_text(size = 16)  # Move legend to the top
  )
fig6c_nn_plot

ggsave(filename=str_c(dir_graphs, "/figure6/fig6c_nn.pdf"), fig6c_nn_plot, width = 11, height = 4, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/figure6/fig6c_nn.png"), fig6c_nn_plot, width = 6, height = 6)

# WITH SEM
fig6c_nn_data_SEM <- df_experiment_probs_nn %>%
  select(sub, Conflict_Level, afraid:surprised) %>%
  pivot_longer(cols = -c(sub, Conflict_Level), names_to = "nn_emotion", values_to = "nn_prob") %>%
  # First calculate subject-level means
  group_by(Conflict_Level, nn_emotion, sub) %>%
  summarise(mean_prob_sub = mean(nn_prob), .groups = "drop") %>%
  # Then calculate group statistics (preserving N)
  group_by(Conflict_Level, nn_emotion) %>%
  summarise(
    mean_prob = mean(mean_prob_sub),
    sd_prob = sd(mean_prob_sub),
    N = n(),
    se_prob = sd_prob / sqrt(N),
    .groups = "drop"
  ) %>%
  # Pivot wider while keeping all necessary columns
  pivot_wider(
    names_from = Conflict_Level,
    values_from = c(mean_prob, sd_prob, N, se_prob),
    names_sep = "."
  ) %>%
  # Calculate delta and its SEM
  mutate(
    delta = mean_prob.High - mean_prob.Medium,
    delta_se = sqrt(se_prob.High^2 + se_prob.Medium^2),
    N_combined = min(N.High, N.Medium),  # Use the smaller N for t-distribution
    delta_lwr = delta - qt(1 - (0.05 / 2), N_combined - 1) * delta_se,
    delta_upr = delta + qt(1 - (0.05 / 2), N_combined - 1) * delta_se,
    # Add labels
    label = str_c(as.character(round(delta*100, 1)), "%"),
    y_label = if_else(delta >= 0, delta + 0.03, delta - 0.025) # here to change the position of the percentage values
  )

fig6c_nn_plot_SEM <- ggplot(fig6c_nn_data_SEM, aes(x = reorder(nn_emotion, delta), y = delta, fill = nn_emotion)) + 
  geom_col(show.legend = FALSE) + 
  geom_errorbar(aes(ymin = delta_lwr, ymax = delta_upr), width = 0.2, color = "black") +
  geom_text(aes(label = label, y = y_label), color = label_color, size = label_size) + 
  scale_y_continuous(labels = scales::percent, name = "Δ Model Likelihood (High - Medium Conflict)") +
  xlab("Neural Network Emotion") + 
  coord_flip() +
  theme_classic() + 
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black", margin = margin(t = 0, r = 15, b = 0, l = 0)), # Questo aggiunge spazio nelle labels delle emozioni!
    text = element_text(size = 16)  # Move legend to the top
  )
fig6c_nn_plot_SEM

ggsave(filename=str_c(dir_graphs, "/figure6/fig6c_nn_SEM.pdf"), fig6c_nn_plot_SEM, width = 7, height = 6, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/figure6/fig6c_nn_SEM_1.png"), fig6c_nn_plot_SEM, width = 7, height = 6)

#--------------------------------------------------------------------------
# DELTA MEDIUM vs LOW
### Delta (Medium - Low) by Emotion
fig6d_nn_data <- df_experiment_probs_nn %>%
  select(sub, Conflict_Level, afraid:surprised) %>%
  pivot_longer(cols = -c(sub, Conflict_Level), names_to = "nn_emotion", values_to = "nn_prob") %>%
  group_by(Conflict_Level, nn_emotion, sub) %>%
  summarise(mean_prob_sub = mean(nn_prob)) %>%
  group_by(Conflict_Level, nn_emotion) %>% 
  summarise(mean_prob = mean(mean_prob_sub), sd_prob = sd(mean_prob_sub), N = n(), se_prob = sd_prob / sqrt(N)) %>%
  mutate(lwr = mean_prob - qt(1 - (0.05 / 2), N - 1) * se_prob,
         upr = mean_prob + qt(1 - (0.05 / 2), N - 1) * se_prob) %>%
  # Delta
  select(Conflict_Level, nn_emotion, mean_prob) %>%
  pivot_wider(names_from = Conflict_Level, values_from = mean_prob) %>% 
  group_by(nn_emotion) %>% 
  mutate(delta = Medium - Low) %>%
  # Add numeric labels
  mutate(label = str_c(as.character(round(delta*100, 1)), "%"), 
         y_label = if_else(delta >= 0, delta + .01, delta - .01)) #play with this to change the position of the %

# PLOT
fig6d_nn_plot <- ggplot(fig6d_nn_data, aes(x = reorder(nn_emotion, delta), y = delta, fill = nn_emotion)) + 
  geom_col(show.legend = FALSE, position = position_dodge(.9)) + 
  geom_text(aes(label = label, y = y_label), color = label_color, size = label_size) + 
  # geom_errorbar(aes(ymin = lwr, ymax = upr), width = .12) +
  scale_y_continuous(labels = scales::percent, name = "Δ Model Likelihood (Medium - Low Conflict)", expand = c(.03, .03)) +
  xlab("NN Emotion") + 
  theme_classic() + 
  coord_flip() + 
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    text = element_text(size = 16)  # Move legend to the top
  )
fig6d_nn_plot

ggsave(filename=str_c(dir_graphs, "/figure6/fig6d_nn.pdf"), fig6d_nn_plot, width = 11, height = 4, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/figure6/fig6d_nn.png"), fig6d_nn_plot, width = 6, height = 6)

# WITH SEM
fig6d_nn_data_SEM <- df_experiment_probs_nn %>%
  select(sub, Conflict_Level, afraid:surprised) %>%
  pivot_longer(cols = -c(sub, Conflict_Level), names_to = "nn_emotion", values_to = "nn_prob") %>%
  # First calculate subject-level means
  group_by(Conflict_Level, nn_emotion, sub) %>%
  summarise(mean_prob_sub = mean(nn_prob), .groups = "drop") %>%
  # Then calculate group statistics (preserving N)
  group_by(Conflict_Level, nn_emotion) %>%
  summarise(
    mean_prob = mean(mean_prob_sub),
    sd_prob = sd(mean_prob_sub),
    N = n(),
    se_prob = sd_prob / sqrt(N),
    .groups = "drop"
  ) %>%
  # Pivot wider while keeping all necessary columns
  pivot_wider(
    names_from = Conflict_Level,
    values_from = c(mean_prob, sd_prob, N, se_prob),
    names_sep = "."
  ) %>%
  # Calculate delta and its SEM
  mutate(
    delta = mean_prob.Medium - mean_prob.Low,
    delta_se = sqrt(se_prob.Medium^2 + se_prob.Low^2),
    N_combined = min(N.Medium, N.Low),  # Use the smaller N for t-distribution
    delta_lwr = delta - qt(1 - (0.05 / 2), N_combined - 1) * delta_se,
    delta_upr = delta + qt(1 - (0.05 / 2), N_combined - 1) * delta_se,
    # Add labels
    label = str_c(as.character(round(delta*100, 1)), "%"),
    y_label = if_else(delta >= 0, delta + 0.03, delta - 0.03)
  )

fig6d_nn_plot_SEM <- ggplot(fig6d_nn_data_SEM, aes(x = reorder(nn_emotion, delta), y = delta, fill = nn_emotion)) + 
  geom_col(show.legend = FALSE) + 
  geom_errorbar(aes(ymin = delta_lwr, ymax = delta_upr), width = 0.2, color = "black") +
  geom_text(aes(label = label, y = y_label), color = label_color, size = label_size) + 
  scale_y_continuous(labels = scales::percent, name = "Δ Model Likelihood (Medium - Low Conflict)") +
  xlab("Neural Network Emotion") + 
  coord_flip() +
  theme_classic() + 
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Add axis lines
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black", margin = margin(t = 0, r = 15, b = 0, l = 0)), # Questo aggiunge spazio nelle labels delle emozioni!
    text = element_text(size = 16)  # Move legend to the top
  )
fig6d_nn_plot_SEM

ggsave(filename=str_c(dir_graphs, "/figure6/fig6d_nn_SEM.pdf"), fig6d_nn_plot_SEM, width = 11, height = 4, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/figure6/fig6d_nn_SEM_1.png"), fig6d_nn_plot_SEM, width = 7, height = 6)

all_deltas <- cowplot::plot_grid(fig6b_nn_plot_SEM, fig6c_nn_plot_SEM,fig6d_nn_plot_SEM,  labels = c("B", "C","D"), nrow = 1, rel_widths = c(3, 3))
ggsave(filename=str_c(dir_graphs, "/all_deltas.pdf"), all_deltas, width = 18, height =6 , useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/all_deltas.png"), all_deltas, width = 18, height = 6)
all_deltas

fig6a <- cowplot::plot_grid(fig6a_nn_plot,  labels = c("A"), nrow = 1, rel_widths = c(3, 3))
ggsave(filename=str_c(dir_graphs, "/first_plot.pdf"), first_plot, width = 15, height = 6, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/first_plot.png"), first_plot, width = 15, height = 6)
fig6a

#------------------------------------------------------------
# DELTA PER SUBJECTS
# High vs. low
fig6b_nn_data_sub <- df_experiment_probs_nn %>%
  select(sub, Conflict_Level, afraid:surprised) %>%
  pivot_longer(cols = -c(sub, Conflict_Level), names_to = "nn_emotion", values_to = "nn_prob") %>%
  group_by(sub, Conflict_Level, nn_emotion) %>%
  summarise(mean_prob_sub = mean(nn_prob), .groups = "drop") %>%
  pivot_wider(names_from = Conflict_Level, values_from = mean_prob_sub) %>%
  mutate(delta = High - Low) # Delta per subject

# High vs. medium
 fig6c_nn_data_sub <- df_experiment_probs_nn %>%
  select(sub, Conflict_Level, afraid:surprised) %>%
  pivot_longer(cols = -c(sub, Conflict_Level), names_to = "nn_emotion", values_to = "nn_prob") %>%
  group_by(sub, Conflict_Level, nn_emotion) %>%
  summarise(mean_prob_sub = mean(nn_prob), .groups = "drop") %>%
  pivot_wider(names_from = Conflict_Level, values_from = mean_prob_sub) %>%
  mutate(delta = High - Medium) # Delta per subject

# Medium vs. low
 fig6d_nn_data_sub <- df_experiment_probs_nn %>%
  select(sub, Conflict_Level, afraid:surprised) %>%
  pivot_longer(cols = -c(sub, Conflict_Level), names_to = "nn_emotion", values_to = "nn_prob") %>%
  group_by(sub, Conflict_Level, nn_emotion) %>%
  summarise(mean_prob_sub = mean(nn_prob), .groups = "drop") %>%
  pivot_wider(names_from = Conflict_Level, values_from = mean_prob_sub) %>%
  mutate(delta = Medium - Low) # Delta per subject


# Filter the data and calculate the mean and standard deviation, rounded to 2 decimals
stats <- fig6c_nn_data_sub %>% # fig6c_nn_data_sub # fig6d_nn_data_sub
  group_by(nn_emotion) %>%
  filter(nn_emotion == "enthusiastic") %>%
  summarize(
    mean = round(mean(delta, na.rm = TRUE), 4),
    sd = round(sd(delta, na.rm = TRUE), 4)
  )

# Extract and print the values individually
mean <- stats$mean*100

# Print with two decimals
cat("Delta %:", mean, "\n")

#-------------------------------------------------- Statistical test comparing each emotion against a theoretical baseline of zero 
# Function to perform one-sample two-tailed t-tests and Cohen's d
compute_tests <- function(df) {
  emotion <- unique(df$nn_emotion) # Extract the emotion label
  
  t_test_result <- t_test(df, delta ~ 1, mu = 0) %>% add_significance()
  cohens_d_result <- cohens_d(df, delta ~ 1, mu = 0) %>% select(effsize)
  
  # Combine results and add emotion label
  bind_cols(t_test_result, cohens_d_result) %>% mutate(nn_emotion = emotion)
}

# Delta High vs. Low Conflict
fig6a_nn_tests_zero <- fig6b_nn_data_sub %>%
  group_split(nn_emotion) %>%
  map_dfr(compute_tests)
fig6a_nn_tests_zero

write.xlsx(fig6a_nn_tests_zero, "/GitHub/results/delta_high_low.xlsx")

# Delta High vs. Medium Conflict
fig6c_nn_tests_zero <- fig6c_nn_data_sub %>%
  group_split(nn_emotion) %>%
  map_dfr(compute_tests)
fig6c_nn_tests_zero

write.xlsx(fig6c_nn_tests_zero, "/GitHub/results/delta_high_medium.xlsx")

# Delta Medium vs. Low Conflict
fig6d_nn_tests_zero <- fig6d_nn_data_sub %>%
  group_split(nn_emotion) %>%
  map_dfr(compute_tests)
fig6d_nn_tests_zero

write.xlsx(fig6d_nn_tests_zero, "/GitHub/results/delta_medium_low.xlsx")

