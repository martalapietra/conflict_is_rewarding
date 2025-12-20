# title: "The Experience of Cognitive Conflict is Intrinsically Rewarding (MANUSCRIPT)"
# author of the analysis script: Marta La Pietra
# date of creation: August 28, 2025
# data of update: December 11, 2025

#----------------------------------------------------------------------
# Install packages
install.packages("ggbeeswarm")
install.packages("ggsignif")
install.packages("ggeffects")
install.packages("ggplot2")
install.packages("devtools")
install.packages("lmerTest")
install.packages("readxl")
install.packages("emmeans")
install.packages("devtools")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyverse")    # tidy functions
install.packages("here")         # relative paths
install.packages("knitr")        # knit functions
install.packages("kableExtra")   # extra markdown functions
install.packages("purrr")        # map functions
install.packages("lme4")         # mixed-effects regressions
install.packages("lmerTest")     # mixed-effects regressions
install.packages("AICcmodavg")   # predictSE()
install.packages("broom.mixed")  # tidy()
install.packages("ggrepel")      # geom_text_repel
install.packages("sjPlot")       # tab_model
install.packages("rstatix")      # cohen's d
install.packages("ggridges")     # density plot
install.packages("tidytext")

# Libraries
library(ggbeeswarm)
library(ggsignif)
library(ggeffects)
library(ggplot2)
library("devtools")
library("lmerTest")
library(readxl)
library(emmeans)
library(devtools)
library(ggplot2)
library(dplyr)
library(tidyverse)    # tidy functions
library(here)         # relative paths
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


# Specify relative paths
dir_analysis <- ("/GitHub/data/") # change according to your directory
dir_parent <- str_remove(dir_analysis, "/analysis")
dir_graphs <- str_c(dir_parent, "/graphs")

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


## CONFLICT TASK PERFORMANCE
# Load
data <- read_excel(str_c(dir_analysis, "experiments_reaction_times.xlsx"))
data <- data[data$Experiment == "Simon", ] # Choose the experiment you want to analyse: "Stroop" OR "Simon"

data$Conflict_Level <- factor(data$Conflict_Level, levels = c("Low","Medium","High"))
data$Congruency <- factor(data$Congruency, levels = c("Congruent", "Incongruent"))

# Filter the data and calculate the mean and standard deviation, rounded to 2 decimals
stats_RTs <- data %>%
  filter(Congruency == "Incongruent") %>%
  summarize(
    mean_RTs = round(mean(Reaction_Time, na.rm = TRUE), 2),
    sd_RTs = round(sd(Reaction_Time, na.rm = TRUE), 2)
  )

# Extract and print the values individually
mean_RTs <- stats_RTs$mean_RTs
sd_RTs <- stats_RTs$sd_RTs

# Print with two decimals
cat("Mean RTs:", mean_RTs, "\n")
cat("Standard Deviation RTs:", sd_RTs, "\n")


# Model with the number of incongruent trials chosen
data$log_RT <- log(data$Reaction_Time)
model_log = lmer(log_RT ~ Conflict_chosen * Congruency + (1 + Block_Number |Participant), data=data, REML = FALSE, control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=10000000)))
summary(model_log)


# PLOT
# Create a data frame for predictions
new_data <- expand.grid(Conflict_chosen = seq(min(data$Conflict_chosen), max(data$Conflict_chosen), length.out = 100),
                        Congruency = unique(data$Congruency))

# Add predicted Reaction_Time values
new_data$log_RT <- predict(model_log, newdata = new_data, re.form = NA)

# Supplementary Figure 1
figS1_plot <- ggplot(new_data, aes(x = Conflict_chosen, y = log_RT, color = Congruency)) +
  geom_line(size = 1) +  # Plot the predicted lines
  scale_x_continuous(limits = c(min(data$Conflict_chosen), max(data$Conflict_chosen))) +  # Set x-axis limits
  scale_y_continuous(limits = c(6.40,6.8)) +    # Set y-axis limits
  scale_color_manual(values = c("#43aa8b", "#FFB205")) +
  labs(x = "Incongruent trials", y = "log(Reaction Time)", color = "Congruency") +
  conflict_theme
figS1_plot

ggsave(filename=str_c(dir_graphs, "/supplementary/figS1.pdf"), figS1_plot, width = 10, height = 5, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/supplementary/figS1.png"), figS1_plot, width = 6, height = 6)
ggsave(filename=str_c(dir_graphs, "/supplementary/figS1_legend.png"), figS1_plot + theme(legend.position = "right"), width = 6, height = 5)

######################################################
# INDIVIDUAL SLIDER
# Load
slider_all <- read_excel(str_c(dir_analysis, "experiments_conflict_counts_reversed.xlsx")) # REVERSED
slider <- slider_all[slider_all$Experiment == "Simon", ]  # CHANGE THE EXPERIMENT NAME HERE: "Stroop" OR "Simon"

# Supplementary Figure 2
plot_width = 6
plot_height = 6

total_possible_responses <- 100 * 10  

# Summarize the data: Calculate total count per Response and Slider
figS2_data <- slider %>%
  group_by(Response, Slider) %>%
  summarise(Total_Count = sum(Count), .groups = "drop")%>%
  mutate(Percentage = (Total_Count / total_possible_responses) * 100)  # Calculate percentage

total_count_sum <- sum(figS2_data$Total_Count)
# Print the result
print(total_count_sum)

# Supplementary Figure 2
# Fig S2A (Simon), 2C (Stroop)
figS2_plot <- ggplot(figS2_data, aes(x = Response, y = Percentage, fill = Slider)) +
  geom_bar(stat = "identity", position = "dodge") +  # Dodge to separate Slider types
  scale_x_continuous(breaks = NULL, name = "Conflict level chosen") +  # Ensure x-axis covers 0 to 30
  scale_y_continuous(limits = c(-0.5,15), name = "Time chosen",labels = scales::number_format(scale = 1, suffix = "%", accuracy = 0.1)) +    # Set y-axis limits
  scale_fill_manual(values = c("#43aa8b", "#FFB205")) + 
  labs(fill = "Slider question") +
  theme_classic()+  # Use a minimal theme
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    # axis.line = element_line(color = "black"),  # Add axis lines
    axis.line.x = element_line(color = "black"),  # Add bottom border
    axis.line.y = element_line(color = "black"),  # Add left border
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "black"),
    text = element_text(size = 16),
    legend.position = "none"  # Move legend to the top
  )+
  annotate("segment", 
           x = min(figS2_data$Response), xend = max(figS2_data$Response), 
           y = -0.4, yend = -0.4,
           color = "black", size = 0.7, 
           arrow = arrow(type = "closed", length = unit(0.15, "inches"))) # Adj ust y if necessary
figS2_plot

ggsave(filename=str_c(dir_graphs, "/supplementary/figS2_reversed.pdf"), figS2_plot + guides(fill=guide_legend(title="Slider Type")), width = 8, height = 4, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/supplementary/figS2_reversed.png"), figS2_plot + guides(fill=guide_legend(title="Level of incongruence")) + theme(legend.title = element_text(size=10)), width = 8, height = 5)
ggsave(filename=str_c(dir_graphs, "/supplementary/figS2_reversed_legend.png"), figS2_plot + theme(legend.position = "right"), width = 10, height = 5)


# GROUPBY LEVELS
figS2b_data <- slider %>%
  group_by(Conflict_Level, Slider) %>%
  summarise(Total_Count = sum(Count), .groups = "drop")%>%
  mutate(Percentage = (Total_Count / total_possible_responses) * 100)  # Calculate percentage

total_count_sum <- sum(figS2b_data$Total_Count)
# Print the result
print(total_count_sum)

figS2b_data$Conflict_Level <- factor(figS2b_data$Conflict_Level, levels = c("Low","Medium","High"))

# Fig S2B (Simon), 2D (Stroop)
figS2b_plot <- ggplot(figS2b_data, aes(x = Conflict_Level, y = Percentage, fill = Slider)) +
  geom_bar(stat = "identity", position = "dodge") +  # Dodge to separate Slider types
  scale_x_discrete(name = "Conflict level chosen") +  # Ensure x-axis covers 0 to 30
  scale_y_continuous(limits = c(0,25), name = "Time chosen",labels = scales::number_format(scale = 1, suffix = "%", accuracy = 0.1)) +    # Set y-axis limits
  scale_fill_manual(values = c("#43aa8b", "#FFB205")) + 
  labs(fill = "Slider question") +
  theme_classic()+  # Use a minimal theme
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    # axis.line = element_line(color = "black"),  # Add axis lines
    axis.line.x = element_line(color = "black"),  # Add bottom border
    axis.line.y = element_line(color = "black"),  # Add left border
    axis.text.x = element_text(color = "black", size = 16),
    axis.text.y = element_text(color = "black"),
    text = element_text(size = 16),
    legend.position = "none"  # Move legend to the top
  )
figS2b_plot

ggsave(filename=str_c(dir_graphs, "/supplementary/figS2b.pdf"), figS2b_plot + guides(fill=guide_legend(title="Slider Type")), width = 8, height = 4, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/supplementary/figS2b.png"), figS2b_plot + guides(fill=guide_legend(title="Level of incongruence")) + theme(legend.title = element_text(size=10)), width = 8, height = 5)
ggsave(filename=str_c(dir_graphs, "/supplementary/figS2b_legend.png"), figS2b_plot + theme(legend.position = "right"), width = 10, height = 5)

########################################################
# For pilot data: Supplementary Figure 7
dir_analysis <- ("/GitHub/data/") # change according to your directory
dir_parent <- str_remove(dir_analysis, "/analysis")
dir_graphs <- str_c(dir_parent, "/graphs")
# CONFLICT PROPORTIONS PILOTS
data <- read_excel(str_c(dir_analysis, "pilots_conflict_proportions.xlsx"))

plot_width = 6
plot_height = 6

# Define the color palette
conflict <- c("Low", "Medium", "High")
# Define the color palette
palette <- c("#F64436","#FFB205", "#00BFC4")

data$Conflict_Level <- factor(data$Conflict_Level, levels = c("Low","Medium","High"))

figS7_data <- data %>%
  mutate(Conflict_Level = fct_relevel(Conflict_Level, conflict))

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


# Calculate means and SEMs
means <- figS7_data %>%
  group_by(Conflict_Level) %>%
  summarise(mean = mean(Proportion), .groups = 'drop')

sems <- figS7_data %>%
  group_by(Conflict_Level) %>%
  summarise(sem = sd(Proportion) / sqrt(n()), .groups = 'drop')

# Combine means and SEMs
summary_data <- means %>%
  left_join(sems, by = "Conflict_Level")

# Create the plot
figS7_plot <- ggplot(figS7_data, aes(x = Conflict_Level, y = Proportion, color = Conflict_Level)) +
  # Violin plot
  geom_violin(aes(fill = Conflict_Level), alpha = 0.5, trim = TRUE, scale = "width", width = 0.5) +
  # Swarm plot
  geom_jitter(color = "grey", alpha = 0.3, width = 0.2) +
  # SEM error bars
  geom_errorbar(data = summary_data, aes(x = Conflict_Level, y = mean, ymin = mean - sem, ymax = mean + sem),
                width = 0.1, color = "black", size = 0.8) +
  # Mean points
  geom_point(data = summary_data, aes(x = Conflict_Level, y = mean), color = "black", size = 3) +
  # Chance level line
  geom_hline(yintercept = 0.33, color = "#d90429", linetype = "dashed", size = 1) +
  # Customize colors
  scale_fill_manual(values = palette) +
  scale_color_manual(values = c("#F64436", "#FFB205", "#00BFC4")) +  # Custom colors for Conflict Levels
  scale_y_continuous(name = "Proportion of choices", breaks = seq(0, 1, by = 0.2))+#,labels = scales::number_format(scale = 1, suffix = "%", accuracy = 0.1)) +  # Customize y-axis
  labs(y = "Proportion", x = "Conflict Level") +
  conflict_theme
figS7_plot

ggsave(filename=str_c(dir_graphs, "/supplementary/figS7.pdf"), figS7_plot, width = 6, height = 5, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/supplementary/figS7.png"), figS7_plot, width = 6, height = 7)

#--------------------------
# ONE-WAY ANOVA
# Compute the analysis of variance
res.aov <- aov(Proportion ~ Conflict_Level, data = figS7_data)
# Summary of the analysis
summary(res.aov)

# Reshape the data to wide format
wide_data <- figS7_data %>%
  pivot_wider(names_from = Conflict_Level, values_from = Proportion)

# Perform the paired t-test
t_test_result <- t.test(wide_data$High, wide_data$Low, paired = TRUE)
# View the result
print(t_test_result)
#------------------------------

# Perform pairwise t-tests
pairwise_tests <- pairwise.t.test(
  data$Proportion,
  data$Conflict_Level,
  p.adjust.method = "bonferroni"  # Adjust p-values for multiple comparisons
)

# Display the results
print(pairwise_tests)

# Extract p-values
p_values <- pairwise_tests$p.value

# Define significance levels
significance_level <- 0.05

# Identify significant comparisons
significant_comparisons <- which(p_values < significance_level, arr.ind = TRUE)

# Define significance annotations
figS7_plot_sign <- figS7_plot +
  geom_signif(comparisons = list(c("Medium", "Low"), c("High", "Medium"), c("High", "Low")),
              annotations = c("n.s.",  "n.s.", "n.s."),  # Adjust based on p-values
              color = "black",
              y_position = c(1.0, 1.10, 1.20),  # Adjust y positions to avoid overlap
              tip_length = 0.01,
              vjust = 0.1,
              size = 0.5,  textsize = 8)
figS7_plot_sign

ggsave(filename=str_c(dir_graphs, "/supplementary/figS7_sign.pdf"), figS7_plot_sign, width = 7, height = 7, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/supplementary/figS7_sign.png"), figS7_plot_sign, width = 7, height = 7)

#-----------------------------------------------------------
# Set chance level
chance_level <- 0.33

# Get unique conditions
conditions <- unique(figS7_data$Conflict_Level)

# Initialize an empty list to store p-values
p_values <- list()

# Perform one-sample t-tests for each condition
p_values <- sapply(conditions, function(cond) {
  subset_data <- subset(figS7_data, Conflict_Level == cond)
  t.test(subset_data$Proportion, mu = chance_level)$p.value
})

# Generate significance labels
signif_labels <- sapply(p_values, function(p) {
  if (p < 0.001) "***"
  else if (p < 0.01) "**"
  else if (p < 0.05) "*"
  else "n.s."
})

# Print for reference
print(data.frame(Condition = conditions, P_Value = p_values, Label = signif_labels))

# Add chance line
figS7_plot_sign_chance <- figS7_plot_sign +
  geom_text(
    data = data.frame(
      Conflict_Level = conditions,
      y_pos = c(-0.1, -0.08, -0.1),  # Adjust as needed
      label = signif_labels
    ),
    aes(x = Conflict_Level, y = y_pos, label = label),
    size = 8, color = "red"
  )
figS7_plot_sign_chance

ggsave(filename=str_c(dir_graphs, "/supplementary/figS7_sign_chance.pdf"), figS7_plot_sign_chance, width = 7, height = 7, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/supplementary/figS7_sign_chance.png"), figS7_plot_sign_chance, width = 7, height = 7)

