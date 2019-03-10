library(tidyverse)
library(rms)
library(lubridate)
library(tableone)
library(simstudy)

def <- defData(varname = "v1_6", dist = "binary", formula = 0.6, id = "idnum", link = "logit") %>% 
  defData(varname = "v1_5", dist = "uniformInt", formula = "18;110") %>% 
 defData(varname = "v2_1_1", dist = "binary", formula = 0.053) %>% 
 defData(varname = "v2_1_2", dist = "binary", formula = 0.544) %>% 
 defData(varname = "v2_1_3", dist = "binary", formula = 0.223) %>% 
 defData(varname = "v2_1_4", dist = "binary", formula = 0.257) %>% 
 defData(varname = "v2_1_5", dist = "binary", formula = 0.187) %>% 
 defData(varname = "v2_5", dist = "binary", formula = "0.4 + 0.1*v2_1_1") %>% 
 defData(varname = "v7_4", dist = "binary", formula = 0.5) %>% 
 defData(varname = "event", dist = "binary", formula = "0.1 + 0.1*v1_6 + v2_1_1*0.05 + v2_1_2*0.01 + v2_1_3*0.15") %>% 
 defData(varname = "died", dist = "binary", formula = 0.5) %>% 
 defData(varname = "NIHSS_15", dist = "uniformInt", formula = "0;42", link="identity") %>% 
 defData(varname = "age_group", dist = "categorical", formula = "0.5;0.2;0.3", link="identity")
# Define survival data part
sdef <- defSurv(varname = "time", formula = "v1_5/2", scale = 20, 
                shape = "v2_5*1 + (1-v2_5)*1.5")
sdef <- defSurv(sdef, varname = "censorTime", scale = 80, shape = 1)
# Create data
df <- genData(5000, def)
df <- genSurv(df, sdef)

df <- df %>% 
  mutate_at("age_group", as.factor)

saveRDS(df, here::here('data', 'df.rds'))

