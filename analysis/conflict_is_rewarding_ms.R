# title: "Cognitive conflict is intrisically rewarding (MANUSCRIPT)"
# author: "Marta La Pietra"

# Libraries
library(tidyverse)    # tidy functions
library(tidytext)
library(readxl)
library(sjPlot)       # tab_model
library(ggeffects)
library(ggplot2)
# install.packages("ggbeeswarm")
library(ggbeeswarm)
# install.packages("ggsignif")
library(ggsignif)
# install.packages("beeswarm")
library(beeswarm)

## Data
# Specify relative paths
dir_analysis <- ("Github/data/") # change according to your directory
dir_parent <- str_remove(dir_analysis, "/analysis")
dir_graphs <- str_c(dir_parent, "/graphs")

#-------------------------- Analysis of the proportions of participants' choices for the level of conflict
# Load data
data <- read_excel(str_c(dir_analysis, "experiments_conflict_proportions.xlsx")) #pilots_conflict_proportions #experiments_conflict_proportions

# Choose the experiment you want to analyse
data <- data[data$Experiment == "Simon", ] #"Stroop" OR "Simon"

plot_width = 6
plot_height = 6

# Define the color palette
conflict <- c("Low", "Medium", "High")
# Define the color palette
palette <- c("#F64436","#FFB205", "#00BFC4")

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


data$Conflict_Level <- factor(data$Conflict_Level, levels = c("Low","Medium","High"))

fig2_data <- data %>%
  mutate(Conflict_Level = fct_relevel(Conflict_Level, conflict))

# Calculate means and SEMs
means <- fig2_data %>%
  group_by(Conflict_Level) %>%
  summarise(mean = mean(Proportion), .groups = 'drop')

sems <- fig2_data %>%
  group_by(Conflict_Level) %>%
  summarise(sem = sd(Proportion) / sqrt(n()), .groups = 'drop')

# Combine means and SEMs
summary_data <- means %>%
  left_join(sems, by = "Conflict_Level")

# Create the plot
fig2_plot <- ggplot(fig2_data, aes(x = Conflict_Level, y = Proportion, color = Conflict_Level)) +
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
fig2_plot

ggsave(filename=str_c(dir_graphs, "/figure2/fig2.pdf"), fig2_plot, width = 6, height = 5, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/figure2/fig2.png"), fig2_plot, width = 6, height = 7)

#--------------------------
# ONE-WAY ANOVA
# Compute the analysis of variance
res.aov <- aov(Proportion ~ Conflict_Level, data = fig2_data)
# Summary of the analysis
summary(res.aov)

# Reshape the data to wide format
wide_data <- fig2_data %>%
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
rfig2_plot_sign <- fig2_plot +
  geom_signif(comparisons = list(c("Medium", "Low"), c("High", "Medium"), c("High", "Low")),
              annotations = c("*",  "***", "n.s."),  # Adjust based on p-values
              color = "black",
              y_position = c(1.0, 1.10, 1.20),  # Adjust y positions to avoid overlap
              tip_length = 0.01,
              vjust = 0.1,
              size = 0.5,  textsize = 8)

fig2_plot_sign

ggsave(filename=str_c(dir_graphs, "/figure2/fig2_sign.pdf"), fig2_plot_sign, width = 6, height = 5, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/figure2/fig2_sign.png"), fig2_plot_sign, width = 6, height = 6)

#-----------------------------------------------------------
# Set chance level
chance_level <- 0.33

# Get unique conditions
conditions <- unique(fig2_data$Conflict_Level)

# Initialize an empty list to store p-values
p_values <- list()

# Perform one-sample t-tests for each condition
p_values <- sapply(conditions, function(cond) {
  subset_data <- subset(fig2_data, Conflict_Level == cond)
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
fig2_plot_sign_chance <- fig2_plot_sign +
  geom_text(
    data = data.frame(
      Conflict_Level = conditions,
      y_pos = c(-0.1, -0.1, -0.08),  # Adjust as needed
      label = signif_labels
    ),
    aes(x = Conflict_Level, y = y_pos, label = label),
    size = 8, color = "red"
  )
fig2_plot_sign_chance

ggsave(filename=str_c(dir_graphs, "/figure2/fig2_sign_chance.pdf"), fig2_plot_sign_chance, width = 6, height = 5, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/figure2/fig2_sign_chance.png"), fig2_plot_sign_chance, width = 6, height = 6)

#---------------------------------------------------------
# Experiment 2: Final choice for the Stroop task
dir_analysis <- ("Github/data/") # change according to your directory
dir_parent <- str_remove(dir_analysis, "/analysis")
dir_graphs <- str_c(dir_parent, "/graphs")
final_slider <- read_excel(str_c(dir_analysis, "experiment2_final_choice_conflict.xlsx"))

final_slider$Conflict_Level <- factor(final_slider$Conflict_Level, levels = c("Low","Medium","High"))

total_possible_responses <- 100

plot_width = 6
plot_height = 6

# Define the color palette
conflict <- c("Low", "Medium", "High")
# Define the color palette
palette <- c("#F64436","#FFB205", "#00BFC4")

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

# Summarize the data: Calculate total count per Response and Slider
fig3_data <- final_slider %>%
  group_by(Conflict_Level) %>%
  summarise(Total_Count = sum(Count), .groups = "drop")%>%
  mutate(Proportion = (Total_Count / total_possible_responses))  # Calculate percentage

total_count_sum <- sum(fig3_data$Total_Count)
# Print the result
print(total_count_sum)

final_slider_prop <- read_excel(str_c(dir_analysis, "experiment2_final_choice_conflict_proportions.xlsx"))
final_slider_prop$Conflict_Level <- factor(final_slider_prop$Conflict_Level, levels = c("Low","Medium","High"))

fig3b_data <- final_slider_prop %>%
  mutate(Conflict_Level = fct_relevel(Conflict_Level, conflict))

# Calculate means and SEMs
means <- fig3b_data %>%
  group_by(Conflict_Level) %>%
  summarise(mean = mean(Proportion), .groups = 'drop')

sems <- fig3b_data %>%
  group_by(Conflict_Level) %>%
  summarise(sem = sd(Proportion) / sqrt(n()), .groups = 'drop')

# Combine means and SEMs
summary_data <- means %>%
  left_join(sems, by = "Conflict_Level")

#----------------------------------
# Violin plot
fig3b_data <- final_slider_prop %>%
  mutate(Conflict_Level = fct_relevel(Conflict_Level, conflict))

# Calculate means and SEMs
means <- fig3b_data %>%
  group_by(Conflict_Level) %>%
  summarise(mean = mean(Proportion), .groups = 'drop')

sems <- fig3b_data %>%
  group_by(Conflict_Level) %>%
  summarise(sem = sd(Proportion) / sqrt(n()), .groups = 'drop')

# Combine means and SEMs
summary_data <- means %>%
  left_join(sems, by = "Conflict_Level")

# Create the plot
fig3b_plot <- ggplot(fig3b_data, aes(x = Conflict_Level, y = Proportion, color = Conflict_Level)) +
  # Violin plot
  geom_violin(aes(fill = Conflict_Level), alpha = 0.5, trim = TRUE, scale = "width", width = 0.5) +
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
  scale_y_continuous(name = "Proportion of choices", breaks = seq(0, 1, by = 0.2)) +  # Customize y-axis
  labs(y = "Proportion", x = "Conflict Level") +
  conflict_theme
fig3b_plot


ggsave(filename=str_c(dir_graphs, "/figure3/fig3.pdf"), fig3b_plot, width = 6, height = 5, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/figure3/fig3.png"), fig3b_plot, width = 6, height = 6)

#-----------------------------------------------------------
# Set chance level
chance_level <- 0.33

# Get unique conditions
conditions <- unique(final_slider_prop$Conflict_Level)

# Initialize an empty list to store p-values
p_values <- list()

# Perform one-sample t-tests for each condition
p_values <- sapply(conditions, function(cond) {
  subset_data <- subset(final_slider_prop, Conflict_Level == cond)
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
fig3b_plot_sign <- fig3b_plot +
  geom_text(
    data = data.frame(
      Conflict_Level = conditions,
      y_pos = c(-0.1, -0.1, -0.08),  # Adjust as needed
      label = signif_labels
    ),
    aes(x = Conflict_Level, y = y_pos, label = label),
    size = 8, color = "red"
  )
fig3b_plot_sign

ggsave(filename=str_c(dir_graphs, "/figure3/fig3_sign.pdf"), fig3b_plot_sign, width = 6, height = 5, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/figure3/fig3_sign.png"), fig3b_plot_sign, width = 6, height = 6)

