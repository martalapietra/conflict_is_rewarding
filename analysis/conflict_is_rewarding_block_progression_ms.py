# -*- coding: utf-8 -*-
"""
# title: "The Experience of Cognitive conflict is intrisically rewarding (MANUSCRIPT)"
# author of the analysis script: Marta La Pietra
# date of creation: August 28, 2025
# data of update: December 11, 2025
"""
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import os

file_excel = 'C:/Users/Marta/Nextcloud/Shared_SweetC/Experiments/ExpPrefer/GitHub/data/experiments_block_differences.xlsx'
df = pd.read_excel(file_excel)

experiment = df[df['Experiment'] == "Stroop"] ## CHANGE THE EXPERIMENT NAME HERE: Simon or Stroop
experiment = experiment.drop(columns=["Experiment"])

### Figure 4: Differences in choices for each participant during the experimental progression in experiment 1 (Simon, A) and experiment 2 (Stroop, B). 
# Create a DataFrame suitable for a heatmap
heatmap_data = experiment.set_index('Participant').T
participant_order = heatmap_data.loc['1-10'].sort_values().index
heatmap_data = heatmap_data[participant_order]

# Increase figure size for better readability
plt.figure(figsize=(30, 15))
palette = ["#E1C13F","#E4A84B", "#C56478", "#95318B"]
# Plot heatmap
ax = sns.heatmap(heatmap_data, cmap="coolwarm", cbar=True,  fmt=".1f", linewidths=.5, linecolor="white",cbar_kws={'pad': 0.02,'aspect': 20})  # Ridimensiona la colorbar)
# Ensure all participants are labeled by setting xticks manually
plt.xticks(ticks=np.arange(len(heatmap_data.columns)) + 0.5, labels=heatmap_data.columns, fontsize=0)
plt.yticks(ticks=np.arange(len(heatmap_data.index)) + 0.5, labels=heatmap_data.index, fontsize=25, family = 'Arial')
plt.yticks(fontsize=25)
# Set labels and title
plt.xlabel('Participant', fontsize=25, family = 'Arial')
plt.ylabel('Block Comparison', fontsize=25, family = 'Arial')
# Aumenta la dimensione dei numeri nella colorbar
cbar = ax.collections[0].colorbar
cbar.ax.tick_params(labelsize=25)
for tick in cbar.ax.get_yticklabels():
    tick.set_fontname('Arial')

# Save the plot
dir_graphs = "C:/Users/Marta/Nextcloud/Shared_SweetC/Experiments/ExpPrefer/GitHub/graphs/"
fig_folder = os.path.join(dir_graphs, "figure4")
os.makedirs(fig_folder, exist_ok=True)

############### !Comment/Uncomment depending on which task you are analysing! #######################
# # If results from Experiment 1 are analysed and plotted
# fig_path_pdf = os.path.join(fig_folder, "fig4A.pdf")
# plt.savefig(fig_path_pdf, format='pdf', bbox_inches='tight')
# fig_path_png = os.path.join(fig_folder, "fig4A.png")
# plt.savefig(fig_path_png, format='png', dpi=300, bbox_inches='tight')

# If results from Experiment 2 are analysed and plotted
fig_path_pdf = os.path.join(fig_folder, "fig4B.pdf")
plt.savefig(fig_path_pdf, format='pdf', bbox_inches='tight')
fig_path_png = os.path.join(fig_folder, "fig4B.png")
plt.savefig(fig_path_png, format='png', dpi=300, bbox_inches='tight')

