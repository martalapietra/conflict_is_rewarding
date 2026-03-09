This ReadME pertains to the Research Project "The Sweet Spot of Cognitive Conflict (SweetC)" Project Number: (PID2020-114717RA-I00 /AEI/ 10.13039/501100011033 to Ruzzoli M) funded by the Ministerio de Ciencia e Innovación (MICIIN) and the Agencia Estatal de Investigación (AEI) and by the Basque Government through the BERC 2022-2025 program and the Spanish State Research Agency through BCBL Severo Ochoa excellence accreditation CEX2020-001010-S.

Title of the manuscript: "The Experience of Cognitive conflict is intrinsically rewarding"
doi: https://doi.org/10.31234/osf.io/b83mn_v3
Authors: La Pietra, M., Vives, M. L., Molinaro, N., Ruzzoli, M. (2025)

Author of this ReadME, of the analysis scripts and responsible for data sharing: Marta La Pietra (she/her/hers)
__________________________________________________________________

Structure of the data files and where they are used.

Datascript: conflict_is_rewarding_ms.R

Name of the file: "experiments_conflict_proportions.xlsx"
This file contains the proportions of choices during the test phase for each participant and each level of conflict.
Columns:
1. Participant: contains Participants' numbers (N=100 each experiment);
2. Conflict_Level: whether the number of incongruent trials chosen was mapped into the High, Medium or Low level of conflict;
3. Proportion: the number of times each conflict level was chosen throughout the ten experimental blocks (see the Data Analysis section in the manuscript for details);
4. Experiment: which experiments the data were collected from (exp1: Simon, exp2: Stroop).

Name of the file: "experiment2_final_choice_conflict.xlsx"
This file contains the data from the ultimate choice for conflict level in experiment 2.
Columns:
1. Participant: contains Participants' numbers (N=100 each experiment);
2. Conflict_Level: whether the ultimate choice was for a High, Medium or Low level of conflict;
3. Response: the exact number of incongruent trials the participant would have chosen to perform one last time if allowed;
4. Count: always 1.

Name of the file: "experiment2_final_choice_conflict_proportions.xlsx"
This file contains the data from the ultimate choice for conflict level in experiment 2 expressed in proportions for analysis purposes.
Columns:
1. Participant: contains Participants' numbers (N=100 each experiment);
2. Conflict_Level: whether the ultimate choice was for a High, Medium or Low level of conflict;
3. Proportion: either 1 or 0, since participants could choose just once;
4. Experiment: Stroop.

Name of the file: "experiments_performance_previous_block.xlsx"
This file contains the data from the test phase. number of incongruent trials chosen, levels of conflict, subjetive ratings, , RTs to Congruent trials, RTs to incongruent trials, Conflict Effect (incongruent RTs - congruent RTs), accuracy, accuracy to congruent trials, accuracy to incongreunt 
Columns:
1. Participant: contains Participants' numbers (N=100 each experiment);
2. Block_Number: from 1 to 10 experimental blocks;
3. Conflict_chosen: the number of incongruent trials chosen in each experimental block;
4. Slider: the slider each participant was assigned to, either to choose the level of incongruence or the level of congruence (reversed);
5. Conflict_Level: whether the number of incongruent trials chosen was mapped into the High, Medium or Low level of conflict;
6. Effort: reported mental effort from 1 (low) to 7 (high); 
7. Enjoyment: reported enjoyment from 1 (low) to 7 (high); 
8. valence: the reported valence (from negative to positive, unpleasant-pleasant);
9. arousal: the reported arousal (from negative to positive, low-high);
10. Reaction_Time: average Reaction Times (RTs) at that block;
11. RTsCong: average RTs to Congruent trials at that block;
12. RTsIncong: average RTs to Incongruent trials at that block;
13. ConflictEffectRTs: The conflict Effect (incongruent RTs - congruent RTs) at that block;
14. Accuracy: mean accuracy (%) at at block;
15. Accuracy_Congruent: average Accuracy to Congruent trials at that block;
16. Accuracy_Incongruent: average Accuracy to Incongruent trials at that block;
17. ConflictEffectAccuracy: The conflict effect (incongruent accuracy - congruent accuracy) at that block;
18. AccuracySpeed: Accuracy/Reaction Times
19. Norm_AccuracySpeed: z-scores of accuracy (%) over response speed (ms).
20. Experiment: which experiments the data were collected from (exp1: Simon, exp2: Stroop).

-----------------------------------------------------
Datascript: conflict_is_rewarding_ratings_ms.R

Name of the file: "experiments_ratings.xlsx"
This file contains the data on effort and enjoyment ratings, on valence and arousal, and the z-scores of accuracy (%) over speed (ms) for each experimental block and participant.
Columns:
1. Participant: contains Participants' numbers (N=100 each experiment);
2. Block_Number: from 1 to 10 experimental blocks;
3. choice: the number of incongruent trials chosen in each experimental block;
4. Conflict_Level: whether the number of incongruent trials chosen was mapped into the High, Medium or Low level of conflict;
5. Effort: reported mental effort from 1 (low) to 7 (high); 
6. Enjoyment: reported enjoyment from 1 (low) to 7 (high); 
7. valence: the reported valence (from negative to positive, unpleasant-pleasant);
8. arousal: the reported arousal (from negative to positive, low-high);
9. study: which experiments the data were collected from (exp1: Simon, exp2: Stroop).
10. Norm_AccuracySpeed: z-scores of accuracy (%) over response speed (ms).

Name of the file: "experiments_performance_previous_block.xlsx"
This file contains the data from the test phase. number of incongruent trials chosen, levels of conflict, subjetive ratings, , RTs to Congruent trials, RTs to incongruent trials, Conflict Effect (incongruent RTs - congruent RTs), accuracy, accuracy to congruent trials, accuracy to incongreunt 
Columns:
1. Participant: contains Participants' numbers (N=100 each experiment);
2. Block_Number: from 1 to 10 experimental blocks;
3. Conflict_chosen: the number of incongruent trials chosen in each experimental block;
4. Slider: the slider each participant was assigned to, either to choose the level of incongruence or the level of congruence (reversed);
5. Conflict_Level: whether the number of incongruent trials chosen was mapped into the High, Medium or Low level of conflict;
6. Effort: reported mental effort from 1 (low) to 7 (high); 
7. Enjoyment: reported enjoyment from 1 (low) to 7 (high); 
8. valence: the reported valence (from negative to positive, unpleasant-pleasant);
9. arousal: the reported arousal (from negative to positive, low-high);
10. Reaction_Time: average Reaction Times (RTs) at that block;
11. RTsCong: average RTs to Congruent trials at that block;
12. RTsIncong: average RTs to Incongruent trials at that block;
13. ConflictEffectRTs: The conflict Effect (incongruent RTs - congruent RTs) at that block;
14. Accuracy: mean accuracy (%) at at block;
15. Accuracy_Congruent: average Accuracy to Congruent trials at that block;
16. Accuracy_Incongruent: average Accuracy to Incongruent trials at that block;
17. ConflictEffectAccuracy: The conflict effect (incongruent accuracy - congruent accuracy) at that block;
18. AccuracySpeed: Accuracy/Reaction Times
19. Norm_AccuracySpeed: z-scores of accuracy (%) over response speed (ms).
20. Experiment: which experiments the data were collected from (exp1: Simon, exp2: Stroop).

-----------------------------------------------------
Datascript: machine_learning_models.R

Name of the file: "emotional_map_experiments_pilots.xlsx"
This file gathers the data from the emotional task where participants used a 500x500-pixel arousal-pleasantness grid to classify 20 emotions. 
Columns:
1. sub: contains Participants' numbers (N=100 each experiment);
2. trial: contains information regarding the trials (from 1 to 39);
3. emotion: afraid, angry, annoyed, aroused, calm, happy, disgusted, enthusiastic, disappointed, nervous, neutral, peppy, quiet, relaxed, sad, satisfied, sleepy, sluggish, still, surprised;
4. valence: the reported valence (from negative to positive, unpleasant-pleasant) for each emotion;
5. arousal: the reported arousal (from negative to positive, low-high) for each emotion;
6. study: whether the data were collected during the experiments (exp1: Simon, exp2: Stroop), or during the pilots (pilot1: SimonPilot, pilot2: StroopPilot).

-----------------------------------------------------
Datascript: emotional_map.R

Name of the file: "experiments_emotional_map.xlsx"
This file gathers the data from the emotional task where participants used a 500x500-pixel arousal-pleasantness grid to classify 20 emotions. 
Columns:
1. sub: contains Participants' numbers (N=100 each experiment);
2. trial: contains information regarding the trials (from 1 to 39);
3. emotion: afraid, angry, annoyed, aroused, calm, happy, disgusted, enthusiastic, disappointed, nervous, neutral, peppy, quiet, relaxed, sad, satisfied, sleepy, sluggish, still, surprised;
4. valence: the reported valence (from negative to positive, unpleasant-pleasant) for each emotion;
5. arousal: the reported arousal (from negative to positive, low-high) for each emotion;
6. study: whether the data were collected during the experiments (exp1: Simon, exp2: Stroop).

Name of the file: "experiments_emotions.xlsx"
This file contains the data on effort and enjoyment ratings, on valence and arousal, and the z-scores of accuracy (%) over speed (ms) for each experimental block and participant.
Columns:
1. Participant: contains Participants' numbers (N=100 each experiment);
2. Block_Number: from 1 to 10 experimental blocks;
3. choice: the number of incongruent trials chosen in each experimental block;
4. Conflict_Level: whether the number of incongruent trials chosen was mapped into the High, Medium or Low level of conflict;
5. Effort: reported mental effort from 1 (low) to 7 (high); 
6. Enjoyment: reported enjoyment from 1 (low) to 7 (high); 
7. valence: the reported valence (from negative to positive, unpleasant-pleasant);
8. arousal: the reported arousal (from negative to positive, low-high);
9. study: which experiments the data were collected from (exp1: Simon, exp2: Stroop).
10. Norm_AccuracySpeed: z-scores of accuracy (%) over response speed (ms).

-----------------------------------------------------
Datascript: conflict_is_rewarding_supplementary.R

Name of the file: "experiments_reaction_times.xlsx"
This file contains the data from the conflict tasks during the test phase. 
Columns:
1. Participant: contains Participants' numbers (N=100 each experiment);
2. Block_Number: from 1 to 10 experimental blocks;
3. Conflict_chosen: the number of incongruent trials each participant choose to perform in that block (Simon: from 0 to 16; Stroop: from 0 to 30);
4. Trial_Number: contains information regarding the trials, without the first trial of the block;
5. Reaction_Time: all single reaction times;
6. Congruence: whether the trial was congruent (1) or incongruent (0);
7. Congruency: whether the trial was congruent (Congruent) or incongruent (Incongruent) for the linear mixed-effects model;
8. Conflict_Level: whether the number of incongruent trials chosen was mapped into the High, Medium or Low level of conflict;
9. study: which experiments the data were collected from (exp1: Simon, exp2: Stroop).

Name of the file: "experiments_conflict_counts_reversed.xlsx"
This file contains the data from the free-choice sliders during the test phase merged together, with the choices for the congruence level reversed for interpretability.
Columns:
1. Participant: contains Participants' numbers (N=100 each experiment);
2. Responses: the number of incongruent trials chosen in each experiment;
3. Count: the number of times each number of incongruent trials was chosen (from 1 to 10);
4. Slider: the slider each participant was assigned to, either to choose the level of incongruence or the level of congruence (then reversed); 
5. Conflict_Level: whether the number of incongruent trials chosen was mapped into the High, Medium or Low level of conflict;
6. Experiment: which experiments the data were collected from (exp1: Simon, exp2: Stroop).

Name of the file: "pilots_conflict_proportions.xlsx"
This file contains the proportions of choices during the test phase for each participant and each level of conflict.
Columns:
1. Participant: contains Participants' numbers (N=100 each experiment);
2. Conflict_Level: whether the number of incongruent trials chosen was mapped into the High, Medium or Low level of conflict;
3. Proportion: the number of times each conflict level was chosen throughout the ten experimental blocks (see the Data Analysis section in the manuscript for details);
4. Experiment: which experiments the data were collected from (pilot1: Simon, pilot2: Stroop).

-----------------------------------------------------
Datascript: conflict_is_rewarding_block_progression_ms.py

Name of the file: "experiments_block_differences.xlsx"
This file contains the differences in choices across blocks in each experiment.
Columns:
1. Participant: contains Participants' numbers (N=100 each experiment);
From column 2 to column 11: deltas of choices from block N to N-1;
12. Experiment: which experiments the data were calculated from (exp1: Simon, exp2: Stroop).
