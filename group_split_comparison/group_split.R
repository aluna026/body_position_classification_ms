library(here)
library(randomForest)
library(cvms)
library(caret)
library(tidyverse)
library(rstatix)

#LOAD DATA
load( here("group_split_comparison","compiled_data_lite.RData"))
ntree = 50
not_all_na <- function(x) any(!is.na(x))

training <- slide_filt %>% group_by(id, code) %>% slice_head(prop = .75) %>% ungroup 
testing <- slide_filt %>% group_by(id, code) %>% slice_tail(prop = .25) %>% ungroup

temp_id <- 9908
bad_ids <- c(9909, 10204, 10802)

group_model <- function(temp_id, training, ntree = 50, bad_ids = NULL) {
  print(temp_id)
  temp_group_training <- training %>% 
    filter(id != temp_id, !(id %in% bad_ids)) %>% 
    select_if(not_all_na) %>% 
    mutate(code = fct_drop(code)) %>% 
    select(-id)
  group <- randomForest(code ~ ., data = temp_group_training, 
                        localImp = TRUE, proximity = FALSE, ntree = ntree)
}

split_model <- function(temp_id, training, ntree = 50) {
  temp_split_training <- training %>% 
    filter(id == temp_id) %>% 
    select_if(not_all_na) %>% 
    mutate(code = fct_drop(code)) %>% 
    select(-id)
  split <- randomForest(code ~ ., data = temp_split_training, 
                        localImp = TRUE, proximity = FALSE, ntree = ntree)
}

ids <- unique(training$id)
group_mods <- map(ids, ~group_model(.x, training, ntree = 50)) %>% set_names(ids)
group_select_mods <- map(ids, ~group_model(.x, training, ntree = 50, bad_ids = bad_ids)) %>% set_names(ids)
split_mods <- map(ids, ~split_model(.x, training, ntree = 50)) %>% set_names(ids)

metrics <- function(rfmodel, testing) {
  predictions <- predict(rfmodel, testing, type = "class")
  u <- union(predictions, testing$code)
  res <- confusion_matrix(factor(testing$code, u),factor(predictions, u))
}
res_split <- map2(split_mods, ids, ~metrics(.x, filter(testing, id == .y))) %>% set_names(ids)
res_split %>% map_dbl(~ .x$`Balanced Accuracy`) %>% sort(.)

