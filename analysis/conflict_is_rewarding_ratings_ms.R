# This analysis script pertains to the Research Project "The Sweet Spot of Cognitive Conflict (SweetC)" 
# Project Number: (PID2020-114717RA-I00 /AEI/ 10.13039/501100011033 to Ruzzoli M) 
# funded by the Ministerio de Ciencia e Innovación (MICIIN) and the Agencia Estatal de Investigación (AEI)
# and by the Basque Government through the BERC 2022-2025 program and the Spanish State Research Agency 
# through BCBL Severo Ochoa excellence accreditation CEX2020-001010-S.

# Title of the manuscript: "The Experience of Cognitive conflict is intrinsically rewarding"
# doi: https://doi.org/10.31234/osf.io/b83mn_v3
# Authors: La Pietra, M., Vives, M. L., Molinaro, N., Ruzzoli, M. (2025)

# Author of the analysis script: Marta La Pietra (she/her/hers)

# date of creation: August 28, 2025
# data of update: March 9, 2026

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
install.packages("ordinal")
install.packages("marginaleffects")

# Libraries
library(tidyverse)    # tidy functions
library(tidytext)
library(readxl)
library(sjPlot)       # tab_model
library(ggeffects)
library(ggplot2)
library(ordinal)
library(lmerTest)     # mixed-effects regressions
library(lme4)
library(sjmisc)
library(marginaleffects)
library(VGAM)
library(MASS)
library(brant)

#--------------------------------------- 
# "High levels of conflict are effortful and enjoyable"
dir_analysis <- ("/GitHub/data/") # change according to your directory
dir_parent <- str_remove(dir_analysis, "/analysis")
dir_graphs <- str_c(dir_parent, "/graphs")

ratings <- read_excel(str_c(dir_analysis, "experiments_ratings.xlsx")) # Choose if you want to analyse the experiments or the pilots: pilots_ratings.xlsx OR experiments_ratings.xlsx

# Choose the experiment you want to analyse
ratings_experiment <- ratings[ratings$Experiment == "Simon", ] # "Simon" OR "Stroop" # CHANGE THE EXPERIMENT NAME HERE

# Plot theme
conflict_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"), # Add axis lines
        panel.border = element_blank(),
        aspect.ratio = 1,
        axis.text.x = element_text(size = 22,color = "black"),
        axis.text.y = element_text(size = 22,color = "black"),
        axis.title = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 5)),  # t = top margin
        axis.title.y = element_text(margin = margin(r = 5)),   # r = right margin
        legend.position = "none")

# Example palette (customize as needed)
custom_colors <- c("#023e8a","#00b4d8", "#90e0ef", "#f0f3bd", "#faa307", "#dc2f02", "#370617")  # Or use any hex codes or named colors

#--------------------------------- Effort
# Check the variable type for the rating
str(ratings_experiment$Effort)
# Check the distribution of the ratings
table(ratings_experiment$Effort)
# Convert the ratings into factors
ratings_experiment$Effort <- factor(as.character(ratings_experiment$Effort),
                                       levels = as.character(1:7),
                                       ordered = TRUE)
# Check if the conversion happened correctly
str(ratings_experiment$Effort)

# Cumulative Logit Model with the number of incongruent trials chosen
model_effort <- clm(Effort ~ Conflict_chosen, data = ratings_experiment, link = "logit")
summary(model_effort)
confint(model_effort, level = 0.95)

model_effort_assumption <- polr(Effort ~ Conflict_chosen, data = ratings_experiment, Hess=TRUE)
brant(model_effort_assumption)

#----------------------------- For experiment 2 where assumptions of proportional odds are violated (χ²(5) = 14.87, p < 0.001)
ratings_experiment <- ratings[ratings$Experiment == "Stroop", ] 
# Check the variable type for the rating
str(ratings_experiment$Effort)
# Check the distribution of the ratings
table(ratings_experiment$Effort)
# Convert the ratings into factors
ratings_experiment$Effort <- factor(as.character(ratings_experiment$Effort),
                                    levels = as.character(1:7),
                                    ordered = TRUE)
# Check if the conversion happened correctly
str(ratings_experiment$Effort)

# Fit the partial proportional odds model
model_vgam <- vglm(Effort ~ Conflict_chosen, 
                   family = cumulative(parallel = FALSE), 
                   data = ratings_experiment)
summary(model_vgam)

#---------------------------------------------- Prepare data for plots
pred_effort <- ggpredict(model_effort, terms = "Conflict_chosen[all]")
pred_effort$group <- factor(pred_effort$response.level)
unique(pred_effort$x)
unique(pred_effort$group) 
head(pred_effort)
str(pred_effort)

# Assign descriptive labels
pred_effort$group <- factor(pred_effort$response.level,
                     levels = 1:7,
                     labels = c("Very Low", "Low", "Moderately Low", "Neutral",
                                "Moderately High", "High", "Very High"))

# Plot the data
############### !Comment/Uncomment depending on which task you are analysing! #######################

# ------ EXPERIMENT 1: SIMON
fig5a_plot_simon <- ggplot(pred_effort, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = 0, ymax = predicted, fill = group), alpha = 0.15, color = NA) +  # fills under the curve
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, .30),
                     name = "Predicted Probability") +
  scale_x_continuous(name = "Conflict Chosen") +
  labs(title = " ",
       color = "Effort Rating",
       fill = "Effort Rating") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE),
         fill = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  conflict_theme
fig5a_plot_simon

# Save the plots in the "graph" directory
ggsave(filename=str_c(dir_graphs, "/figure5/fig5a_Simon.pdf"), fig5a_plot_simon, width = 10, height = 8, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/figure5/fig5a_Simon.png"), fig5a_plot_simon, width = 10, height = 8)
ggsave(filename=str_c(dir_graphs, "/figure5/fig5a_legend_Simon.png"),fig5a_plot_simon + theme(legend.position = "top",
                                                                                    legend.title   = element_text(size = 20),
                                                                                    legend.text    = element_text(size = 16),
                                                                                    plot.margin    = margin(10, 20, 10, 10)), width = 15, height = 8)

# #----- EXPERIMENT 2: STROOP TASK
# fig5a_plot_stroop <- ggplot(pred_effort, aes(x = x, y = predicted, color = group, fill = group)) +
#   geom_line(size = 1.2) +
#   geom_ribbon(aes(ymin = 0, ymax = predicted, fill = group), alpha = 0.15, color = NA) +  # fills under the curve
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, .30), 
#                      name = "Predicted Probability") +
#   scale_x_continuous(name = "Conflict Chosen") +
#   labs(title = " ",
#        color = "Effort Rating",
#        fill = "Effort Rating") +
#   guides(color = guide_legend(nrow = 2, byrow = TRUE),
#          fill = guide_legend(nrow = 2, byrow = TRUE)) +
#   scale_color_manual(values = custom_colors) +
#   scale_fill_manual(values = custom_colors) +
#   conflict_theme
# fig5a_plot_stroop
# 
# # Save the plots in the "graph" directory
# ggsave(filename=str_c(dir_graphs, "/figure5/fig5a_Stroop.pdf"), fig5a_plot_stroop, width = 10, height = 8, useDingbats=F)
# ggsave(filename=str_c(dir_graphs, "/figure5/fig5a_Stroop.png"), fig5a_plot_stroop, width = 10, height = 8)
# ggsave(filename=str_c(dir_graphs, "/figure5/fig5a_legend_Stroop.png"),fig5a_plot_stroop + theme(legend.position = "top",
#                                                                                       legend.title   = element_text(size = 20),
#                                                                                       legend.text    = element_text(size = 16),
#                                                                                       plot.margin    = margin(10, 20, 10, 10)), width = 15, height = 8)


# Fit a cumultive logit model with the Normalised Accuracy/Speed values
model_performance_effort <- clm(Effort~Conflict_chosen * Norm_AccuracySpeed, data = ratings_experiment, link = "logit")
summary(model_performance_effort)
confint(model_performance_effort, level = 0.95)

# Check which one of the two models better fits the data
anova (model_effort, model_performance_effort)
 
#--------------------------------------- Enjoyment
str(ratings_experiment$Enjoyment)
table(ratings_experiment$Enjoyment)
ratings_experiment$Enjoyment <- factor(as.character(ratings_experiment$Enjoyment),
                                       levels = as.character(1:7),
                                       ordered = TRUE)
str(ratings_experiment$Enjoyment)

model_enjoyment <- clm(Enjoyment ~ Conflict_chosen, data = ratings_experiment, link = "logit")
summary(model_enjoyment)
confint(model_enjoyment, level = 0.95)

# Test assumption of proportional odds
model_enjoyment_assumption <- polr(Enjoyment ~ Conflict_chosen, data = ratings_experiment, Hess=TRUE)
brant(model_enjoyment_assumption)

#---------------- Prepare data for plots
pred_enjoyment <- ggpredict(model_enjoyment, terms = "Conflict_chosen[all]")
pred_enjoyment$group <- factor(pred_enjoyment$response.level)
unique(pred_enjoyment$x)
unique(pred_enjoyment$group) 
head(pred_enjoyment)
str(pred_enjoyment)

# Assign descriptive labels
pred_enjoyment$group <- factor(pred_enjoyment$response.level,
                     levels = 1:7,
                     labels = c("Very Low", "Low", "Moderately Low", "Neutral",
                                "Moderately High", "High", "Very High"))


levels(pred_enjoyment$group)

############### !Comment/Uncomment depending on which task you are analysing! #######################

#----- EXPERIMENT 1: SIMON TASK
fig5b_plot_simon <- ggplot(pred_enjoyment, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = 0, ymax = predicted, fill = group), alpha = 0.15, color = NA) +  # fills under the curve
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, .30), 
                     name = "Predicted Probability") +
  scale_x_continuous(name = "Conflict Chosen") +
  labs(title = " ",
       color = "Enjoyment Rating",
       fill = "Enjoyment Rating") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE),
         fill = guide_legend(nrow = 1, byrow = TRUE)) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  conflict_theme
fig5b_plot_simon

ggsave(filename=str_c(dir_graphs, "/figure5/fig5b_Simon.pdf"), fig5b_plot_simon, width = 10, height = 8, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/figure5/fig5b_Simon.png"), fig5b_plot_simon, width = 10, height = 8)
ggsave(filename=str_c(dir_graphs, "/figure5/fig5b_legend_Simon.png"),fig5b_plot_simon + theme(legend.position = "top",
                                                                                        legend.title   = element_text(size = 20),
                                                                                        legend.text    = element_text(size = 16),
                                                                                        plot.margin    = margin(10, 20, 10, 10)), width = 15, height = 8)


# #----- EXPERIMENT 2: STROOP TASK
# fig5b_plot_stroop <- ggplot(pred_enjoyment, aes(x = x, y = predicted, color = group, fill = group)) +
#   geom_line(size = 1.2) +
#   geom_ribbon(aes(ymin = 0, ymax = predicted, fill = group), alpha = 0.15, color = NA) +  # fills under the curve
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, .30), 
#                      name = "Predicted Probability") +
#   scale_x_continuous(name = "Conflict Chosen") +
#   labs(title = " ",
#        color = "Enjoyment Rating",
#        fill = "Enjoyment Rating") +
#   guides(color = guide_legend(nrow = 1, byrow = TRUE),
#          fill = guide_legend(nrow = 1, byrow = TRUE)) +
#   scale_color_manual(values = custom_colors) +
#   scale_fill_manual(values = custom_colors) +
#   conflict_theme
# fig5b_plot_stroop
# 
# ggsave(filename=str_c(dir_graphs, "/figure5/fig5b_Stroop.pdf"), fig5b_plot_stroop, width = 10, height = 8, useDingbats=F)
# ggsave(filename=str_c(dir_graphs, "/figure5/fig5b_Stroop.png"), fig5b_plot_stroop, width = 10, height = 8)
# ggsave(filename=str_c(dir_graphs, "/figure5/fig5b_legend_Stroop.png"),fig5b_plot_stroop + theme(legend.position = "top",
#                                                                                         legend.title   = element_text(size = 20),
#                                                                                         legend.text    = element_text(size = 16),
#                                                                                         plot.margin    = margin(10, 20, 10, 10)), width = 15, height = 8)

model_performance_enjoyment <- clm(Enjoyment~Conflict_chosen * Norm_AccuracySpeed, data = ratings_experiment, link = "logit")
summary(model_performance_enjoyment)
confint(model_performance_enjoyment, level = 0.95)

# Likelihood ratio test
anova (model_enjoyment, model_performance_enjoyment)

#-------------------- SAVE PLOTS
############### !Comment/Uncomment depending on which task you are analysing! #######################

fig5_plot_simon <- cowplot::plot_grid(fig5a_plot_simon,fig5b_plot_simon, labels = c("Effort", "Enjoyment"),  label_size  = 22, label_fontface = "bold", nrow = 1, rel_widths = c(2, 2))
fig5_plot_simon

ggsave(filename=str_c(dir_graphs, "/figure5/fig5a_b_Simon.pdf"), fig5_plot_simon, width = 9, height = 5.5, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/figure5/fig5a_b_Simon.png"), fig5_plot_simon, width = 10, height = 5.5)
 
# fig5_plot_stroop <- cowplot::plot_grid(fig5a_plot_stroop,fig5b_plot_stroop, labels = c("Effort", "Enjoyment"),  label_size  = 22, label_fontface = "bold", nrow = 1, rel_widths = c(2, 2))
# fig5_plot_stroop
# 
# ggsave(filename=str_c(dir_graphs, "/figure5/fig5a_b_Stroop.pdf"), fig5_plot_stroop, width = 9, height = 5.5, useDingbats=F)
# ggsave(filename=str_c(dir_graphs, "/figure5/fig5a_b_Stroop.png"), fig5_plot_stroop, width = 10, height = 5.5)

#------------ If you want the data of both experiments plotted together
fig5a_plot_both <- cowplot::plot_grid(fig5a_plot_simon,fig5a_plot_stroop, labels = c("Effort", " "),  label_size  = 22, label_fontface = "bold", nrow = 1, rel_widths = c(2, 2))
fig5a_plot_both

ggsave(filename=str_c(dir_graphs, "/figure5/fig5a_Simon_Stroop.pdf"), fig5a_plot_both, width = 9, height = 5, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/figure5/fig5a_Simon_Stroop.png"), fig5a_plot_both, width = 10, height = 5.5)

fig5b_plot_both <- cowplot::plot_grid(fig5b_plot_simon, fig5b_plot_stroop, labels = c("Enjoyment", " "),  label_size  = 22, label_fontface = "bold", nrow = 1, rel_widths = c(2, 2))
fig5b_plot_both

ggsave(filename=str_c(dir_graphs, "/figure5/fig5b_Simon_Stroop.pdf"), fig5b_plot_both, width = 9, height = 5, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/figure5/fig5b_Simon_Stroop.png"), fig5b_plot_both, width = 10, height = 5.5)

#------------------------------------------------------------------------
# "The temporal dynamics of seeking and experiencing cognitive conflict"
time_data <- read_excel(str_c(dir_analysis, "experiments_performance_previous_block.xlsx")) 
# Choose the experiment you want to analyse
time_data <- time_data[time_data$Experiment == "Stroop", ] #"Stroop" OR "Simon" # CHANGE THE EXPERIMENT NAME HERE

vars_to_shift <- c(
  "Reaction_Time", "RTsCong", "RTsIncong", "ConflictEffectRTs", "Accuracy", "Accuracy_Congruent", "Accuracy_Incongruent", "ConflictEffectAccuracy", "AccuracySpeed", "Norm_AccuracySpeed",
  "Conflict_chosen", "Conflict_Level", "Effort", "Enjoyment", "Pleasantness", "Arousal"
)

# Cumulative Link Mixed Model: Effort/Enjoyment ~ Block_Number progression
time_data$Enjoyment <- ordered(time_data$Enjoyment)
time_data$Effort <- ordered(time_data$Effort)

model_affect_time_effort = clmm(Effort ~ Block_Number + (1| Participant), data=time_data)
summary(model_affect_time_effort)
confint(model_affect_time_effort, level = 0.95)

model_affect_time_enjoyment = clmm(Enjoyment ~ Block_Number + (1| Participant), data=time_data)
summary(model_affect_time_enjoyment)
confint(model_affect_time_enjoyment, level = 0.95)

anova(model_affect_time, model_affect_time_effort)

#-------------------- PREVIOUS BLOCK EFFECT 
time_data_df <- time_data %>%
  arrange(Participant, Block_Number) %>%
  group_by(Participant) %>%
  mutate(across(all_of(vars_to_shift), ~lag(.x, 1), .names = "Previous_{.col}")) %>%
  ungroup()
time_data_df <- na.omit(time_data_df)

model_choice_affect = lmer(scale(Conflict_chosen) ~ scale(Previous_Conflict_chosen) * scale(Previous_Effort) * scale(Previous_Enjoyment) + (1 + Block_Number| Participant), data=time_data_df, REML = FALSE, control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=10000000)))
summary(model_choice_affect)
confint(model_choice_affect)

# with NormAccuracySpeed
model_choice_withAccSpeed = lmer(scale(Conflict_chosen) ~ scale(Previous_Conflict_chosen) * scale(Previous_Effort) * scale(Previous_Enjoyment) * scale(Previous_Norm_AccuracySpeed) + (1 + Block_Number| Participant), data=time_data_df, REML = FALSE, control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=10000000)))
summary(model_choice_withAccSpeed)
confint(model_choice_withAccSpeed)

