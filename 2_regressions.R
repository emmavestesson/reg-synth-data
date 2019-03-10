library(tidyverse)
library("survival")
library(xlsx)

df <- readRDS(here::here('data', 'df.rds'))

predictors <- c("v1_5", "v1_6", "v2_1_1", "v2_1_2", "v2_1_3", "v2_1_4", "age_group", "v2_5")

surv_data <- Surv(time=df$time, event=df$event, type="right")
crude <- map(predictors, ~coxph(as.formula(paste("surv_data ~ ", .x, sep="+")),
                                data=df)) %>% 
  set_names(., paste0('crude_',predictors))

adjusted <- map(predictors, ~coxph(as.formula(paste("surv_data ~ age_group + v1_6 ", .x, sep="+")),
                                   data=df)) %>% 
  set_names(., paste0('adj_',predictors))

  crude_tidy <- map(crude, ~broom::tidy(.x, exponentiate=TRUE)) %>% 
  bind_rows(.id="id")

adj_tidy <- map(adjusted, ~broom::tidy(.x, exponentiate=TRUE)) %>% 
  bind_rows(.id="id")

adj_glance <- map(adjusted, ~broom::glance(.x)) %>% 
  bind_rows(.id="id")
# Save models in excel files
# Crude models
write.xlsx2(crude_tidy, here::here('crude_models.xlsx'), sheetName = 'all_models')
crude_tidy %>% 
  mutate(nested_var=id) %>% 
  group_by(nested_var) %>% 
  nest() %>% 
  mutate(sheet_new = map(data, ~write.xlsx2(.x, here::here('crude_models.xlsx'), append= TRUE, sheetName =.x$id[[1]])))

# Adjusted models
write.xlsx2(adj_tidy, here::here('adj_models.xlsx'), sheetName = 'all_models')
adj_tidy %>% 
  mutate(nested_var=id) %>% 
  group_by(nested_var) %>% 
  nest() %>% 
  mutate(sheet_new = map(data, ~write.xlsx2(.x, here::here('adj_models.xlsx'), append= TRUE, sheetName =.x$id[[1]])))


