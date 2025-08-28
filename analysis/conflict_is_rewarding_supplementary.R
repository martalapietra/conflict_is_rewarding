# title: "Conflict is fun (Supplement)"
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

library(ggeffects)
library(ggplot2)
library("devtools")
library("lmerTest")
library(readxl)
library(emmeans)
library(devtools)
library(sjPlot)
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
dir_analysis <- ("Github/data/") # change according to your directory
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
data <- data[data$Experiment == "Simon", ] #"Stroop" OR "Simon"

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
data$z_RT <- scale(data$Reaction_Time)
data$log_RT <- log(data$Reaction_Time)

model_zscore = lmer(z_RT ~ Conflict_chosen * Congruency + (1 |Participant), data=data, REML = FALSE, control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=10000000)))
summary(model_zscore)

# We chose this one
model_log = lmer(log_RT ~ Conflict_chosen * Congruency + (1 |Participant), data=data, REML = FALSE, control = lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=10000000)))
summary(model_log)
confint(model_log)

AIC(model_zscore, model_log)

# Confronto residui
plot(resid(model_log), main = "Residui modello log")
plot(resid(model_zscore), main = "Residui modello z-score")
# sjPlot:: tab_model(model_numb)

# PLOT
# Create a data frame for predictions
new_data <- expand.grid(Conflict_chosen = seq(min(data$Conflict_chosen), max(data$Conflict_chosen), length.out = 100),
                        Congruency = unique(data$Congruency))

# Add predicted Reaction_Time values
new_data$log_RT <- predict(model_log, newdata = new_data, re.form = NA)

# Plot using ggplot2
figS1_plot <- ggplot(new_data, aes(x = Conflict_chosen, y = log_RT, color = Congruency)) +
  geom_line(size = 1) +  # Plot the predicted lines
  scale_x_continuous(limits = c(min(data$Conflict_chosen), max(data$Conflict_chosen))) +  # Set x-axis limits
  scale_y_continuous(limits = c(6.40,6.8)) +    # Set y-axis limits
  scale_color_manual(values = c("#43aa8b", "#FFB205")) +
  labs(x = "Incongruent trials", y = "log(Reaction Time)", color = "Congruency") +
  conflict_theme
  # theme_minimal()+  # Use a minimal theme
  # theme(
  #   panel.grid.major = element_blank(),  # Remove major grid lines
  #   panel.grid.minor = element_blank(),  # Remove minor grid lines
  #   axis.line = element_line(color = "black"),  # Add axis lines
  #   text = element_text(size = 16), aspect.ratio = 1,
  #   legend.position = "none"  # Move legend to the top
  # )
figS1_plot

ggsave(filename=str_c(dir_graphs, "/supplementary/figS1.pdf"), figS1_plot, width = 10, height = 5, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/supplementary/figS1.png"), figS1_plot, width = 6, height = 6)
ggsave(filename=str_c(dir_graphs, "/supplementary/figS1_legend.png"), figS1_plot + theme(legend.position = "right"), width = 6, height = 5)

######################################################
# INDIVIDUAL SLIDER
# Load
# slider <- read_excel(str_c(dir_analysis, "TotCount_BothSliders.xlsx")) # NOT REVERSED CONGRUENT
slider <- read_excel(str_c(dir_analysis, "TotCount_BothSliders_Reversed.xlsx")) # REVERSED 


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

# Create the bar plot
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

# Create the bar plot
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



###################################################
# INDIVIDUAL CHOICES
slider_blocks <- read_excel(str_c(dir_analysis, "Slider_Responses_Count.xlsx"))

# Filter the data for Block Number 1
block_1_data <- slider_blocks %>% filter(Block_Number == 1)

# Calculate the first choice values for each participant in Block 1
first_choices <- block_1_data %>%
  group_by(Participant) %>%
  summarise(First_Choice = first(Response)) %>%
  arrange(First_Choice)  # Sort participants based on first choice values

# Merge sorted participants back to main data
sorted_data <- slider_blocks %>%
  filter(Participant %in% first_choices$Participant) %>%
  mutate(Participant = factor(Participant, levels = first_choices$Participant))  # Ensure correct order

# Step 1: Reset the 'sub' number (they all performed different tasks)
sorted_data <- sorted_data %>%
  # group_by(study) %>%
  mutate(new_sub = dense_rank(Participant)) %>%
  # ungroup() %>%
  mutate(continuos_sub = group_indices(.,new_sub))

# Step 2: Move 'continuos_sub' to the first column
sorted_data <- sorted_data %>%
  select(continuos_sub, everything())

# Step 3: Drop the original 'sub' and the 'new_sub' columns
sorted_data <- sorted_data %>%
  select(-Participant)
sorted_data <- sorted_data %>%
  select(-new_sub)

# Step 4: Rename 'continuos_sub' to 'sub'
sorted_data <- sorted_data %>%
  rename(sub = continuos_sub)

# Create the plot
figS6_plot <- ggplot(sorted_data, aes(x = Block_Number, y = Response, fill = as.factor(Block_Number))) +
  # geom_bar(stat = "identity", position = "identity", width = 0.8) +  # Adjust bar width
  geom_point(size = 2, alpha = 1.5, aes(colour=factor(Block_Number))) +  # Use points instead of bars
  facet_wrap(~sub, ncol = 10) +  # Arrange participants into rows/columns
  scale_y_continuous(breaks = seq(0, 35, 10)) +  # Y-axis from 0 to 30 for Stroops, change it for Simon50: (0,16)
  ylim(-3, 36) + # Y-axis from 0 to 30 for Stroops, change it for Simon50: (-3,18)
  scale_x_continuous(breaks = seq(1, 10, 1)) +  # X-axis for Block Numbers
  labs(x = "Block Number", y = "Slider Response") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # axis.line.x = element_line(color = "black"),  # Add bottom border
        # axis.line.y = element_line(color = "black"),  # Add left border
        panel.border = element_rect(color = "black", fill = NA, linewidth=0.2),  # Add borders to all facets
        axis.ticks = element_line(linewidth = 0.5),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        text = element_text(size = 16),
        strip.text = element_text(size = 12, face = "bold"))
figS6_plot

ggsave(filename=str_c(dir_graphs, "/supplementary/figS6.pdf"), figS6_plot, width = 15, height = 10, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/supplementary/figS6.png"), figS6_plot, width = 15, height = 10)
# ggsave(filename=str_c(dir_graphs, "/supplementary/figS1_legend.png"), figS1_plot + theme(legend.position = "right"), width = 6, height = 5)

########################################################
########################################################
dir_analysis <- ("C:/Users/Marta/Nextcloud/Shared_SweetC/Experiments/ExpPrefer/ExpPref_Analysis/ExpPref_SummaryResults/Stroop/") # Stroop, Simon
dir_parent <- str_remove(dir_analysis, "/analysis")
dir_graphs <- str_c(dir_parent, "/graphs")
# CONFLICT PROPORTIONS PILOTS
data <- read_excel(str_c(dir_analysis, "ConflictProportions.xlsx"))

plot_width = 6
plot_height = 6

# Define the color palette
conflict <- c("Low", "Medium", "High")
# Define the color palette
palette <- c("#F64436","#FFB205", "#00BFC4")

data$Conflict_Level <- factor(data$Conflict_Level, levels = c("Low","Medium","High"))

fig2_data <- data %>%
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
fig2_plot_sign <- fig2_plot +
  geom_signif(comparisons = list(c("Medium", "Low"), c("High", "Medium"), c("High", "Low")),
              annotations = c("*",  "n.s.", "**"),  # Adjust based on p-values
              color = "black",
              y_position = c(1.0, 1.10, 1.20),  # Adjust y positions to avoid overlap
              tip_length = 0.01,
              vjust = 0.1,
              size = 0.5,  textsize = 8)
fig2_plot_sign

ggsave(filename=str_c(dir_graphs, "/figure2/fig2_sign.pdf"), fig2_plot_sign, width = 7, height = 7, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/figure2/fig2_sign.png"), fig2_plot_sign, width = 7, height = 7)

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
      y_pos = c(-0.1, -0.08, -0.1),  # Adjust as needed
      label = signif_labels
    ),
    aes(x = Conflict_Level, y = y_pos, label = label),
    size = 8, color = "red"
  )
fig2_plot_sign_chance

ggsave(filename=str_c(dir_graphs, "/figure2/fig2_sign_chance.pdf"), fig2_plot_sign_chance, width = 7, height = 7, useDingbats=F)
ggsave(filename=str_c(dir_graphs, "/figure2/fig2_sign_chance.png"), fig2_plot_sign_chance, width = 7, height = 7)

