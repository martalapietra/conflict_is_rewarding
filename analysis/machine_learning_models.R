# Libraries
install.packages("kableExtra")
install.packages("purrr")
install.packages("AICcmodavg")
install.packages("broom.mixed")
install.packages("ggrepel")
install.packages("rstatix")
install.packages("ggridges")
install.packages("here")
install.packages("tidytext")
install.packages("snow")
install.packages("caret")
install.packages("doParallel")
install.packages("readxl")
install.packages("tidyverse")
install.packages("tidyverse")

library(tidyverse)
library(caret)
library(doParallel)
library(snow)
library(readxl)
library(dplyr)

# Load data
data_dir <- ("Github/data/") # change according to your directory
df_emotion <- read_excel(str_c(data_dir, "emotional_map_experiments_pilots.xlsx"))

# Step 1: Reset the 'sub' number (they all performed different tasks)
df_emotion <- df_emotion %>%
  group_by(study) %>%
  mutate(new_sub = dense_rank(sub)) %>%
  ungroup() %>%
  mutate(continuos_sub = group_indices(., study, new_sub))

# Step 2: Move 'continuos_sub' to the first column
df_emotion <- df_emotion %>%
  select(continuos_sub, everything())

# Step 3: Drop the original 'sub' and the 'new_sub' columns
df_emotion <- df_emotion %>%
  select(-sub)
df_emotion <- df_emotion %>%
  select(-new_sub)

# Step 4: Rename 'continuos_sub' to 'sub'
df_emotion <- df_emotion %>%
  rename(sub = continuos_sub)

# Pre-processing
## Train-test split
seed <- 123
set.seed(seed)
index_train <- as.numeric(createDataPartition(y = df_emotion$emotion, p = 0.75, list = FALSE, times = 1))
df_train_raw <- df_emotion[index_train, ]
df_test_raw <- df_emotion[-index_train, ]

## Seeds for neural network
# List should have B + 1 elements where B is the number of resamples (10 in our case)
# The first B elements of the list should be vectors of integers of length M where
# M is the number of models being evaluated (due to tuning parameters)s

set_seeds <- function(method = "cv", folds = 10, tunes = NULL, seed = 234) {
  set.seed(seed = seed)
  seeds <- vector(mode = "list", length = folds)
  seeds <- lapply(seeds, function(x) sample.int(n = 1000000, size = folds + ifelse(is.null(tunes), 0, tunes)))
  seeds[[length(seeds) + 1]] <- sample.int(n = 1000000, size = 1) # for final model
  return(seeds)
}

# Neural network
df_train_nn <- df_train_raw
df_test_nn <- df_test_raw

nn_tune_grid <- expand.grid(size = seq(20, 50, by = 1), decay = seq(0, .1, by = .005))  # tuning parameters chosen with trial and error
nn_seeds <- set_seeds(method = "cv", folds = 10, tunes = nrow(nn_tune_grid))
nn_train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE, 
                                 savePredictions = TRUE, seeds = nn_seeds)

num_cores <- detectCores() - 1 # never want to use all cores 
cl <- makeCluster(num_cores, type = "SOCK")
registerDoParallel(cl)
set.seed(1)

## Model
nn_fit <- train(emotion ~ valence + arousal, 
                data = df_train_nn, 
                method = "nnet", 
                trControl = nn_train_control,
                preProcess = c("center", "scale"), 
                tuneGrid = nn_tune_grid, 
                trace = FALSE,  # no console output
                linout = 0)  # classification
stopCluster(cl)  # stop cluster after training
saveRDS(nn_fit, str_c(data_dir, "/nn_model.rds"))

## Evaluate
df_test_nn$nn_class <- predict(nn_fit, newdata = df_test_nn, type = "raw")
nn_cm <- confusionMatrix(table(df_test_nn$nn_class, df_test_nn$emotion))
nn_cm$overall

# KNN
df_train_knn <- df_train_raw
df_test_knn <- df_test_raw

knn_tune_grid <- expand.grid(k = seq(50, 300, by = 1)) # tuning range chosen with trial and error
knn_seeds <- set_seeds(method = "cv", folds = 10, tunes = nrow(knn_tune_grid))
knn_train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE, 
                                  savePredictions = TRUE, seeds = knn_seeds)

num_cores <- detectCores() - 1 # never want to use all cores 
cl <- makeCluster(num_cores, type = "SOCK")
registerDoParallel(cl)
set.seed(1)

## Model
knn_fit <- train(emotion ~ valence + arousal, 
                 data = df_train_knn, 
                 method = "knn", 
                 trControl = knn_train_control,
                 preProcess = c("center", "scale"), 
                 tuneGrid = knn_tune_grid)
parallel::stopCluster(cl) # stop cluster after training
saveRDS(knn_fit, str_c(data_dir, "/knn_model.rds"))

## Evaluate
df_test_knn$knn_class <- predict(knn_fit, newdata = df_test_knn, type = "raw")
knn_cm <- confusionMatrix(table(df_test_knn$knn_class, df_test_knn$emotion))
knn_cm$overall

# SVM 
df_train_svm <- df_train_raw
df_test_svm <- df_test_raw

svm_tune_grid <- expand.grid(C = seq(0, 2, by = .01))
svm_seeds <- set_seeds(method = "cv", folds = 10, tunes = nrow(svm_tune_grid))
svm_train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE, 
                                  savePredictions = TRUE, seeds = svm_seeds)

num_cores <- detectCores() - 1 # never want to use all cores 
cl <- makeCluster(num_cores, type = "SOCK")
registerDoParallel(cl)
set.seed(1)

## Model
svm_fit <- train(emotion ~ valence + arousal, 
                 data = df_train_svm, 
                 method = "svmLinear", 
                 trControl = svm_train_control,
                 preProcess = c("center", "scale"), 
                 tuneGrid = svm_tune_grid)
parallel::stopCluster(cl) # stop cluster after training
saveRDS(svm_fit, str_c(data_dir, "/svm_model.rds"))

## Evaluate
df_test_svm$svm_class <- predict(svm_fit, newdata = df_test_svm, type = "raw")
svm_cm <- confusionMatrix(table(df_test_svm$svm_class, df_test_svm$emotion))
svm_cm$overall

# K-Means
kmeans_fit <- kmeans(x = df_emotion[,c("valence", "arousal")], centers = 9, nstart = 100)
saveRDS(kmeans_fit, str_c(data_dir, "/kmeans_model.rds"))

##############################################################
# Output
write_csv(df_train_nn, str_c(data_dir, "/df_train.csv"))
write_csv(df_test_nn, str_c(data_dir, "/df_test.csv"))

# Session Info
writeLines(capture.output(sessionInfo()), str_c(data_dir, "/sessionInfo.txt")) # session info

