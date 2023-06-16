# This script calculates the first set of results in the paper based on the proximal period
# compiled_data_lite.RData contains the windowed motion feature data for all sessions
# The data are partitioned into training and testing sets
# Group models and individual ("split") models are trained, and then predictions are made on the testing set
# Agreement metrics are saved to /data to be reported in the RMarkdown manuscript
# Depending on hardware, this script can take several hours to run

library(here)
library(randomForest)
library(cvms)
library(caret)
library(tidyverse)
library(rstatix)

#LOAD DATA
load( here("group_split_comparison","compiled_data_lite.RData"))
ntree = 550
mtry = 44
not_all_na <- function(x) any(!is.na(x))

total_time <- session %>% mutate(coded_time = end_time_coded - start_time_coded) %>% arrange(coded_time)
total_samples <- slide_filt %>% drop_na(code) %>% group_by(id) %>% summarize(n_samples = n()) %>% arrange(n_samples)

training <- slide_filt %>% group_by(id, code) %>% slice_head(prop = .75) %>% ungroup 
testing <- slide_filt %>% group_by(id, code) %>% slice_tail(prop = .25) %>% ungroup

group_model <- function(temp_id, training, ntree = 50) {
  print(temp_id)
  temp_group_training <- training %>% 
    filter(id != temp_id) %>% 
    select_if(not_all_na) %>% 
    mutate(code = fct_drop(code)) %>% 
    select(-id)
  group <- randomForest(code ~ ., data = temp_group_training, 
                        localImp = TRUE, proximity = FALSE, ntree = ntree, mtry = mtry)
}

split_model <- function(temp_id, training, ntree = 50) {
  temp_split_training <- training %>% 
    filter(id == temp_id) %>% 
    select_if(not_all_na) %>% 
    mutate(code = fct_drop(code)) %>% 
    select(-id)
  split <- randomForest(code ~ ., data = temp_split_training, 
                        localImp = TRUE, proximity = FALSE, ntree = ntree, mtry = mtry)
}

ids <- unique(training$id)
group_mods <-map(ids, ~group_model(.x, training)) %>% set_names(ids)
split_mods <- map(ids, ~split_model(.x, training)) %>% set_names(ids)

metrics <- function(rfmodel, testing) {
  predictions <- predict(rfmodel, testing, type = "class")
  u <- union(predictions, testing$code)
  res <- confusion_matrix(factor(testing$code, u),factor(predictions, u))
}

res_group <- map2(group_mods, ids, ~metrics(.x, filter(testing, id == .y))) %>% 
  map_dfr(~ select(.x, `Overall Accuracy`:Kappa)) %>% 
  add_column(ids) %>% add_column(model = "group")
res_group %>% get_summary_stats(-ids)

res_split <- map2(split_mods, ids, ~metrics(.x, filter(testing, id == .y))) %>% 
  map_dfr(~ select(.x, `Overall Accuracy`:Kappa)) %>% 
  add_column(ids) %>% add_column(model = "split")
res_split %>% get_summary_stats(-ids)
res_split %>% arrange(`Overall Accuracy`)

ds <- bind_rows(res_split, res_group) 
write_csv(ds, file = "data/group_split_metrics.csv")

res_group_class <-  map2(group_mods, ids, ~metrics(.x, filter(testing, id == .y))) %>%  
  map2_dfr(ids, ~ .x$`Class Level Results`[[1]] %>% 
                          select(-(Support:Table)) %>% 
                          mutate(id = .y, model = "group"))
res_split_class <-  map2(split_mods, ids, ~metrics(.x, filter(testing, id == .y))) %>%  
  map2_dfr(ids, ~ .x$`Class Level Results`[[1]] %>% 
             select(-(Support:Table)) %>% 
             mutate(id = .y, model = "split"))

ds_class <- bind_rows(res_split_class, res_group_class) 
write_csv(ds_class, file = "data/group_split_metrics_class.csv")
