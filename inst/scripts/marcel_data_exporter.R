
export_marcels <- function(output_dir="../data", 
                           output_file="marcels.RData", 
                           stats_to_project=c("X1B", "X2B", "X3B", "HR", 
                                              "BB", "HBP", 
                                              "SB", "CS", 
                                              "SO", "SH", "SF")) {
  
  a <- get_batting_stats()
  b <- tbl_df(append_previous_years(a %>% filter(POS!="P")))
  
  mcl <- tbl_df(apply_marcel(b, "HR"))
  marcels <- mcl %>% select(playerID, yearID, proj_pa)
  for (stat_name in stats_to_project) {
    mcl <- tbl_df(apply_marcel(b, stat_name))
    mcl %>% select(playerID, yearID, proj_pa, proj_value)
    marcels[[stat_name]] <- mcl$proj_value
  }
  save(marcels, file=paste(output_dir, output_file, sep='/'))
}
