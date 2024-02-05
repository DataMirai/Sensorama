if(!require("pacman")){install.packages("pacman")}
pacman::p_load(tidyverse, # to arrange all stuff and functional programming
               jsonlite, # to read the data with FromJSON
               utils, BiocManager, 
               ChemmineR, ChemmineOB, 
               grid, ggplotify, rcdk, furrr,
               FactoMineR,FactoClass,factoextra,Rcpp,broom,pander,corrplot,gridExtra)



df_sensorama_Completo <- read_rds('Data/DF_Sensorama.rds')

# Sensorama de ejemplo ----

df_sensorama_ejemplo_juguete <- df_sensorama_Completo %>% 
  select(-c(canonical_sdf,parsed_smile)) %>%   
  unnest(cols = c(odour, descriptors)) 

df_sensorama_ejemplo_juguete  <-   df_sensorama_ejemplo_juguete  %>% 
  mutate(
    Invent_var_1 = rpois(dim(df_sensorama_ejemplo_juguete)[1],50),
    Invent_var_2 = rpois(dim(df_sensorama_ejemplo_juguete)[1],200),
    Invent_var_3 = rnorm(dim(df_sensorama_ejemplo_juguete)[1],100,40),
    Invent_var_4 = rnorm(dim(df_sensorama_ejemplo_juguete)[1],200,71)) %>% 
  select(cid, level, name, starts_with('Invent_var'), xlogp ) %>% 
  rename(Invent_var_5= xlogp)

# Multilistas ----


df_sensorama_ejemplo_juguete_lista_niveles <- df_sensorama_ejemplo_juguete %>% 
  group_split(level) %>% 
  set_names(paste('nivel',1:8) )


df_sensorama_ejemplo_juguete_lista_niveles


# Correspondencia nivel 1 - 2 ----

unique(df_sensorama_ejemplo_juguete$name)


