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
