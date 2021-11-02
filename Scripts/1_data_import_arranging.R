if(!require("pacman")){install.packages("pacman")}
pacman::p_load(tidyverse, jsonlite , utils,BiocManager,ChemmineR,ChemmineOB, grid, ggplotify)

# Data importing -------------------------------------------------------------------------------
  # In this case the data is already prepared and only required its importation straightforwardly,
  # This data has been provided from the UAB bioinformatics team

odour <- fromJSON("Data/odor_relations.json", flatten = T)
compounds <- read.csv("Data/compounds.csv")

## Data checks --------------------------------------------------------------------------------------
  # Seems to be duplicated data cases, just point to control

head(compounds)

compounds %>%
  mutate(cid= as.character(cid)) %>%
  inner_join(odour, by=c("cid"="cid") ) %>%
  head(2) # Seem to be duplicated data

compounds %>%
  mutate(cid= as.character(cid)) %>%
  inner_join(odour, by=c("cid"="cid") ) %>%
  group_by_all() %>%
  tally() %>%
  as.data.frame()

# Data arranging and nesting -------------------------------------------------------------------
  # The datasets will be worked without thes duplicities

df <- compounds %>%
  mutate(cid= as.character(cid)) %>%
  inner_join(odour, by=c("cid"="cid") ) %>%
  distinct() %>%
  mutate( cid= as.numeric(cid),
          level = case_when( level == "first"  ~ 1,
                             level == "second" ~ 2,
                             level == "third"  ~ 3,
                             level == "fourth" ~ 4, 
                             level == "fifth"  ~ 5, 
                             level == "sixth"  ~ 6,
                             level == "seventh"~ 7,
                             level == "eighth" ~ 8)) %>%
  arrange(cid, level) %>%
  select(cid, canonical_smiles, xlogp, molecular_weight, level, name )


df_nested <- df %>%
  group_by(cid, canonical_smiles, xlogp, molecular_weight) %>%
  nest() %>%
  ungroup() %>%
  rename(odour = "data") %>%
  mutate(cid= as.character(cid) )

df_nested <- df_nested %>%
  filter(!cid %in% c("222","14923", "25519") ) %>%
  mutate("canonical_sdf" = map(canonical_smiles, smiles2sdf))

rm("compounds","odour", "df")
