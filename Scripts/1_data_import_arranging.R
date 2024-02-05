if(!require("pacman")){install.packages("pacman")}
pacman::p_load(tidyverse, # to arrange all stuff and functional programming
               jsonlite, # to read the data with FromJSON
               utils, 
               BiocManager,
               ChemmineR,
               ChemmineOB, 
               grid, 
               ggplotify,
               rcdk,
               #ingerprint,
               furrr)

# Data importing -------------------------------------------------------------------------------
  # In this case the data is already prepared and only required its importation straightforwardly,
  # This data has been provided from the UAB bioinformatics team


compounds <- read.csv("Data/compounds.csv") %>%  as_tibble()

odour <- fromJSON("Data/odor_relations.json", flatten = T) %>%  as_tibble() %>% 
  mutate(
    name= str_replace_all(name,'almond bitter almond','bitter almond'),
    name= str_replace_all(name,'peanut roasted peanut','roasted peanut'),
    name= str_replace_all(name,'rose dried rose','dried rose'),
    name= str_replace_all(name,'apple green apple','green apple'),
    name= str_replace_all(name,'fruit overripe fruit','overripe fruit'),
    name= str_replace_all(name,'fruit tropical fruit','tropical fruit'),
    name= str_replace_all(name,'coffee roasted coffee','roasted coffee'),
    name= str_replace_all(name,'pepper black pepper','black pepper'),
    name= str_replace_all(name,'cherry maraschino cherry','maraschino cherry'),
    name= str_replace_all(name,'chocolate dark chocolate','dark chocolate'),
    name= str_replace_all(name,'cherry maraschino cherry','maraschino cherry'),
    name= str_replace_all(name,'licorice black licorice','black licorice'),
    name= str_replace_all(name,'potato raw potato','raw potato'),
    name= str_replace_all(name,'meaty roasted meaty','roasted meaty'),
    name= str_replace_all(name,'fruit ripe fruit','ripe fruit'),
    name= str_replace_all(name,'banana unripe banana','unripe banana'),
    name= str_replace_all(name,'currant black currant','black currant'),
    name= str_replace_all(name,'cloth laundered cloth','laundered cloth'),
    name= str_replace_all(name,'hazelnut roasted hazelnut','roasted hazelnut'),
    name= str_replace_all(name,'barley roasted barley','roasted barley'),
    name= str_replace_all(name,'grain toasted grain','grain toasted grain'),
    name= str_replace_all(name,'currant bud black currant bud','black currant bud'),
    name= str_replace_all(name,'pepper bell pepper','bell pepper'),
    name= str_replace_all(name,'almond roasted almond','roasted almond'),
    name= str_replace_all(name,'cheesy limburger cheese','limburger cheese'),
    name= str_replace_all(name,'sugar burnt sugar','burnt sugar'),
    name= str_replace_all(name,'tea black tea','black tea'),
    name= str_replace_all(name,'sugar brown sugar','brown sugar'),
    name= str_replace_all(name,'onion cooked onion','cooked onion'),
    name= str_replace_all(name,'apple cooked apple','cooked apple'),
    name= str_replace_all(name,'banana ripe banana','ripe banana'),
    name= str_replace_all(name,'sausage smoked sausage','smoked sausage'),
    name= str_replace_all(name,'potato baked potato','baked potato'),
    name= str_replace_all(name,'bean green bean','green bean'),
    name= str_replace_all(name,'pea green pea','green pea'),
    name= str_replace_all(name,'chicken roasted chicken','roasted chicken'),
    name= str_replace_all(name,'beefy roasted beefy','roasted beefy'),
    name= str_replace_all(name,'cheesy parmesan cheese','parmesan cheese'),
    name= str_replace_all(name,'woody burnt wood','burnt wood'),
    name= str_replace_all(name,'almond toasted almond','toasted almond'),
    name= str_replace_all(name,'tea green tea','green tea'),
    name= str_replace_all(name,'cheesy feta cheese','feta cheese'),
    name= str_replace_all(name,'cheesy bleu cheese','blue cheese'),
    name= str_replace_all(name,'orange bitter orange','bitter orange'))

## Data checks --------------------------------------------------------------------------------------
  # Seems to be duplicated data cases, just point to control

# compounds %>%
#   mutate(cid= as.character(cid)) %>%
#   inner_join(odour, by=c("cid"="cid") ) %>%
#   head(2) # Seem to be duplicated data
# 
# compounds %>%
#   mutate(cid= as.character(cid)) %>%
#   inner_join(odour, by=c("cid"="cid") ) %>%
#   group_by_all() %>%
#   tally() %>%
#   as.data.frame()

# Data arranging and nesting -------------------------------------------------------------------
  # The datasets will be worked without thes duplicities

df <- compounds %>%
  mutate(cid= as.character(cid)) %>%
  inner_join(odour, by=c("cid"="cid") ) %>%
  distinct() %>%
  mutate( 
    level = case_when( level == "first"  ~ '1',
                       level == "second" ~ '2',
                       level == "third"  ~ '3',
                       level == "fourth" ~ '4', 
                       level == "fifth"  ~ '5', 
                       level == "sixth"  ~ '6',
                       level == "seventh"~ '7',
                       level == "eighth" ~ '8')) %>%
  mutate(cid= as.integer(cid) ) %>%
  arrange(cid, level) %>%
  mutate(cid= as.character(cid)) %>%
  select(cid, canonical_smiles, xlogp, level, name ) %>%
  filter(!cid %in% c("222","14923", "25519") )

df_nested <- df %>%
  group_by(cid, canonical_smiles, xlogp) %>%
  nest() %>%
  ungroup() %>%
  rename(odour = "data")

# microbenchmark::microbenchmark(
#   df_nested %>%
#     mutate("canonical_sdf" = future_map(canonical_smiles, smiles2sdf ),
#            "parsed_smile"  = future_map(canonical_smiles, parse.smiles)) %>%
#     mutate("descriptors"= future_map(parsed_smile, ~ eval.desc(.x, nombre_metodos  ))) 
#   ,
#   df_nested %>%
#     mutate("canonical_sdf" = map(canonical_smiles, smiles2sdf ),
#            "parsed_smile"  = map(canonical_smiles, parse.smiles)) %>%
#     mutate("descriptors"= map(parsed_smile, ~ eval.desc(.x, nombre_metodos  ))),
#   times= 20
# )

nombre_metodos <- get.desc.names()

future::plan(multisession)

df_nested <- df_nested %>%
  mutate("canonical_sdf" = future_map(canonical_smiles, smiles2sdf, .progress = TRUE ),
         "parsed_smile"  = future_map(canonical_smiles, parse.smiles, .progress = TRUE)) %>%
  mutate("descriptors"= future_map(parsed_smile, ~ eval.desc(.x, nombre_metodos  ),.progress = TRUE)) 

# df_nested %>%
#   select(-c(molecular_weight, canonical_sdf,parsed_smile)) %>% 
#   toJSON() %>%
#   write("Data/Sensorama_JSON.json")

df_nested

rm(list=setdiff(ls(), "df_nested"))

saveRDS(df_nested, 'Data/DF_Sensorama.rds')
