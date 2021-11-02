source("Scripts/1_data_import_arranging.r")

df_nested 

fingerprint_OB(obmolRefs = obmol(df_nested$canonical_sdf[[100]]), "FP4" )

test1 <- fingerprint_OB(obmolRefs = obmol(df_nested$canonical_sdf[[100]]), "FP4" )
length(test1)

obmol(df_nested$canonical_sdf[[1]] )

forEachMol("SMILES",df_nested$canonical_smiles[[1]])



# /////////////////////////////////////////////////
lapply(df_nested$canonical_sdf,MW)
lapply(df_nested$canonical_sdf, bondblock )
lapply(df_nested$canonical_sdf, bonds)
lapply(df_nested$canonical_sdf, rings)
