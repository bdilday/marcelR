
export_marcels <- function(output_dir="../data", 
                           output_file="marcels.RData", 
                           stats_to_project_batting=c("X1B", "X2B", "X3B", "HR", 
                                              "BB", "HBP", 
                                              "SB", "CS", 
                                              "SO", "SH", "SF"),
                           stats_to_project_pitching=c("H", "HR")
                           ) {
  

  a <- get_batting_stats()
  b <- tbl_df(append_previous_years(a %>% filter(POS!="P")))
  
  mcl <- tbl_df(apply_marcel(b, "HR"))
  marcels_batting <- mcl %>% select(playerID, yearID, proj_pa)
  for (stat_name in stats_to_project_batting) {
    mcl <- tbl_df(apply_marcel(b, stat_name))
    mcl %>% select(playerID, yearID, proj_pa, proj_value)
    marcels_batting[[stat_name]] <- mcl$proj_value
  }


  a <- get_pitching_stats()
  b <- tbl_df(append_previous_years(a %>% filter(POS=="P")))
  
  mcl <- tbl_df(apply_marcel(b, "H"))
  marcels_pitching <- mcl %>% select(playerID, yearID, proj_pa)
  for (stat_name in stats_to_project_pitching) {
    mcl <- tbl_df(apply_marcel(b, stat_name))
    mcl %>% select(playerID, yearID, proj_pa, proj_value)
    marcels_pitching[[stat_name]] <- mcl$proj_value
  }
  
  marcels <- list(Pitching=marcels_pitching, Batting=marcels_batting)
  save(marcels, file=paste(output_dir, output_file, sep='/'))
}
