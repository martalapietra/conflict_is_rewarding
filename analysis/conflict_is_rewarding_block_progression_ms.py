# -*- coding: utf-8 -*-
"""
# This analysis script pertains to the Research Project "The Sweet Spot of Cognitive Conflict (SweetC)" 
# Project Number: (PID2020-114717RA-I00 /AEI/ 10.13039/501100011033 to Ruzzoli M) 
# funded by the Ministerio de Ciencia e Innovación (MICIIN) and the Agencia Estatal de Investigación (AEI)
# and by the Basque Government through the BERC 2022-2025 program and the Spanish State Research Agency 
# through BCBL Severo Ochoa excellence accreditation CEX2020-001010-S.

# Title of the manuscript: "The Experience of Cognitive Conflict is Intrinsically Rewarding"
# doi: https://doi.org/10.31234/osf.io/b83mn_v3
# Authors: La Pietra, M., Vives, M. L., Molinaro, N., Ruzzoli, M. (2025)

# Author of the analysis script: Marta La Pietra (she/her/hers)

# date of creation: August 28, 2025
# data of update: March 03, 2026
"""
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import os

file_excel = '/GitHub/data/experiments_block_differences.xlsx'
df = pd.read_excel(file_excel)

experiment = df[df['Experiment'] == "Stroop"] ## CHANGE THE EXPERIMENT NAME HERE: Simon or Stroop
experiment = experiment.drop(columns=["Experiment"])

column_order = ['Participant', '1-2', '2-3', '3-4', '4-5', '5-6','6-7','7-8','8-9','9-10','1-10']
experiment = experiment[column_order]
only_block1_10 = experiment[['Participant','1-10']]
experiment = experiment.rename(columns={'1-2': '2-1','2-3': '3-2','3-4': '4-3',
                                        '4-5': '5-4','5-6': '6-5','6-7': '7-6',
                                        '7-8': '8-7','8-9': '10-9','1-10': '10-1'})


### Figure 4: Differences in choices for each participant during the experimental progression in experiment 1 (Simon, A) and experiment 2 (Stroop, B). 
# Create a DataFrame suitable for a heatmap
heatmap_data = experiment.set_index('Participant').T
participant_order = heatmap_data.loc['10-1'].sort_values().index
heatmap_data = heatmap_data[participant_order]

# Create figure and main axes
fig, ax = plt.subplots(figsize=(8, 15))

# Create a separate axes for the colorbar at the top
cbar_ax = fig.add_axes([0.15, 0.90, 0.7, 0.03])  # [left, bottom, width, height] — posiziona in alto

# Plot heatmap with TRANSPOSED data
sns.heatmap(
    heatmap_data.T,  # Trasponi: Partecipanti → righe (asse Y), Block Comparison → colonne (asse X)
    cmap="coolwarm",
    cbar=True,
    fmt=".1f",
    linewidths=.5,
    linecolor="white",
    cbar_ax=cbar_ax,
    cbar_kws={'orientation': 'horizontal', 'pad': 0.02, 'aspect': 20},
    ax=ax
)

# ———— ASSE Y: Partecipanti ————
# Mostra ogni 3° partecipante per evitare sovrapposizioni
step_y = 3
yticks = np.arange(len(heatmap_data.columns)) + 0.5
ax.set_yticks(yticks[::step_y])
ax.set_yticklabels(
    heatmap_data.columns[::step_y],  # Etichette dei partecipanti
    fontsize=0,
    family='Arial',
    rotation=0,
    ha='right'
)

# ———— ASSE X: Block Comparison ————
xticks = np.arange(len(heatmap_data.index)) + 0.5
ax.set_xticks(xticks)
# Crea le etichette
labels = list(heatmap_data.index)
# Imposta tutte le etichette
tick_labels = ax.set_xticklabels(
    labels,
    fontsize=19,
    family='Arial',
    ha='center'
)
# Evidenzia solo l'ultima etichetta (indice -1)
tick_labels[-1].set_color('black')      # Colore 
tick_labels[-1].set_fontweight('bold')  # Grassetto
tick_labels[-1].set_fontsize(24)    # Opzionale: aumenta leggermente la dimensione

# Set labels
ax.set_xlabel('Δ Choices Across Blocks', fontsize=25, family='Arial')
ax.set_ylabel('Participant', fontsize=25, family='Arial')

# Customise colorbar
cbar = ax.collections[0].colorbar
cbar.set_ticks([cbar.vmin, cbar.vmax])
cbar.set_ticklabels(
    ['Less Conflict Chosen', 'More Conflict Chosen'],
    fontname='Arial',
    fontsize=20,
    fontweight='normal'
)
cbar.ax.tick_params(labelsize=20)
for tick in cbar.ax.get_xticklabels():
    tick.set_fontname('Arial')

# Adjust layout
plt.subplots_adjust(top=0.85, bottom=0.3, left=0.15)  # Spazio in basso per X, a sinistra per Y


# Save the plot
dir_graphs = "C:/Users/Marta/Nextcloud/Shared_SweetC/Experiments/ExpPrefer/GitHub/graphs/"
fig_folder = os.path.join(dir_graphs, "figure4")
os.makedirs(fig_folder, exist_ok=True)

# ############### !Comment/Uncomment depending on which task you are analysing! #######################
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



