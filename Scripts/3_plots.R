source("Scripts/1_data_import_arranging.r")

df_nested
df_nested_2 <- df_nested %>%
  mutate(string_length= map_int(canonical_smiles, ~ length(unlist(strsplit(.x, ""))))) %>%
  arrange(desc(string_length))

# ////////////////////////////////////////////////////////////////////////////

# plot_gen_rds <- function(df_nested_row){
#   plot(df_nested_row$canonical_sdf[[1]])
#   test<- recordPlot()
#   saveRDS(test, paste("Plots/RDS/",df_nested_row$cid,"_recordplot.rds",sep = ""))
# }

# apply(df_nested, 1 , plot_gen_rds)

plot_molecule <- function(cid){
  test<-readRDS(paste("Plots/RDS/",cid,"_recordplot.rds", sep = ""))
  return(test)

}


for(cid in head(df_nested$cid)){
  plot_molecule(cid)
}


test_cid <- 4

plot_molecule(test_cid)
dev.copy(png, filename= paste("Plots/PNG/",test_cid,"_plot.png", sep = "")) 
dev.off()


