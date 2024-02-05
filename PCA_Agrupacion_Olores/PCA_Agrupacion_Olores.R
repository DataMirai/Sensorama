# Este codigo esta hech a partir de los siguientes posts:
## https://rpubs.com/Joaquin_AR/310338



if(!require("pacman")){install.packages("pacman")}
pacman::p_load(tidyverse, # to arrange all stuff and functional programming
               jsonlite, # to read the data with FromJSON
               utils, BiocManager, 
               ChemmineR, ChemmineOB, 
               grid, ggplotify, rcdk, furrr,
               cluster, FactoMineR,FactoClass,factoextra,Rcpp,broom,pander,corrplot,gridExtra)

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


lista_df_scaled_ByLevels <- df_sensorama_ejemplo_juguete %>% 
  mutate_at(vars(starts_with('Invent')), scale) %>% 
  group_split(level) %>% 
  set_names(paste0('Level_',1:8))

# clusters_por_nivel <- fviz_nbclust(x = df_transformado, FUNcluster = pam, method = "wss", k.max = 20,
#              diss = dist(df_transformado, method = "manhattan"))

clusters_por_nivel <- lista_df_scaled_ByLevels %>% 
  map(~ pam(x = .x %>%  select(starts_with('Invent')), k = 10, metric = "manhattan"))

clusters_por_nivel

# saveRDS(clusters_por_nivel, 'Data/clusters_por_nivel.rds')


map2(
  clusters_por_nivel,lista_df_scaled_ByLevels,
  ~ fviz_cluster(object = .x, data = .y, ellipse.type = "t", repel = TRUE) +
    theme_bw() +
    labs(title = "Resultados clustering PAM") +
    theme(legend.position = "none"))


pam(x = df_transformado %>%  select(starts_with('Invent')), k = 10, metric = "manhattan")
km_clusters <- pam(x = df_transformado, k = 16, metric = "manhattan")


