library(here)
library(randomForest)
library(cvms)
library(caret)
library(tidyverse)
library(rstatix)
library(furrr)

plan("multisession", workers = 9)
plan("sequential")

#LOAD DATA
load( here("group_split_comparison","compiled_data_lite.RData"))
ntree = 150
not_all_na <- function(x) any(!is.na(x))

training <- slide_filt %>% group_by(id, code) %>% slice_head(prop = .75) %>% ungroup 
testing <- slide_filt %>% group_by(id, code) %>% slice_tail(prop = .25) %>% ungroup

bad_ids <- c(9909, 9911, 10204, 10802, 11104, 11501)

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
group_mods <- future_map(ids, ~group_model(.x, training)) %>% set_names(ids)
group_select_mods <- future_map(ids, ~group_model(.x, training, bad_ids = bad_ids)) %>% set_names(ids)
split_mods <- future_map(ids, ~split_model(.x, training)) %>% set_names(ids)

# save(group_select_mods, split_mods, file = "group_split_comparison/models.RData")
# load( here("group_split_comparison","models.RData"))

metrics <- function(rfmodel, testing) {
  predictions <- predict(rfmodel, testing, type = "class")
  u <- union(predictions, testing$code)
  res <- confusion_matrix(factor(testing$code, u),factor(predictions, u))
}

res_group <- map2(group_mods, ids, ~metrics(.x, filter(testing, id == .y))) %>% 
  map_dfr(~ select(.x, `Overall Accuracy`:Kappa)) %>% 
  add_column(ids) %>% add_column(model = "group")
res_group %>% get_summary_stats(-ids)

res_select <- map2(group_select_mods, ids, ~metrics(.x, filter(testing, id == .y))) %>% 
  map_dfr(~ select(.x, `Overall Accuracy`:Kappa)) %>% 
  add_column(ids) %>% add_column(model = "select")
res_select %>% get_summary_stats(-ids)

res_split <- map2(split_mods, ids, ~metrics(.x, filter(testing, id == .y))) %>% 
  map_dfr(~ select(.x, `Overall Accuracy`:Kappa)) %>% 
  add_column(ids) %>% add_column(model = "split")
res_split %>% get_summary_stats(-ids)
res_split %>% arrange(`Overall Accuracy`)

ds <- bind_rows(res_split, res_group) %>% bind_rows(res_select)
write_csv(ds, file = "data/group_split_metrics.csv")

ggplot(ds, aes(x = model, y = `Overall Accuracy`)) + geom_boxplot()
ggplot(ds, aes(x = model, y = `Balanced Accuracy`)) + geom_boxplot()
ggplot(ds, aes(x = model, y = `Kappa`)) + geom_boxplot()

