
#' Export Marcels
#' 
#' Computes Marcels for batting, pitching, and teams, and exports to an
#' R data object.
#' 
#' @import dplyr
#' @param output_dir Directory to write RData file
#' @param output_file Name of RData file
#' @param stats_to_project_batting Array of stats to project for batters
#' @param stats_to_project_pitching Array of stats to project for pitchers
#' 
export_marcels <- function(output_dir="../data", 
                           output_file="marcels.RData", 
                           stats_to_project_batting=c("X1B", "X2B", "X3B", "HR", 
                                              "BB", "HBP", 
                                              "SB", "CS", 
                                              "SO", "SH", "SF"),
                           stats_to_project_pitching=c("H", "HR", 
                                                       "ER", "BB", "SO", "HBP",
                                                       "R"),
                           upcoming_season=2020
                           ) {
  

  a <- get_batting_stats()
  b <- dplyr::tbl_df(append_previous_years(a %>% filter(POS!="P"), 
                                           get_seasonal_averages_batting, 
                                           previous_years = 3))
  
  mcl <- dplyr::tbl_df(apply_marcel_batting(b, "HR", age_adjustment))
  marcels_batting <- mcl %>% 
    dplyr::select(playerID, yearID=projectedYearID, proj_pa) 
  
  for (stat_name in stats_to_project_batting) {
    if ( stat_name %in% c("SO", "CS") ) {
      age_fun <- age_adjustment_reciprocal
    } else {
      age_fun <- age_adjustment
    }
    mcl <- dplyr::tbl_df(apply_marcel_batting(b, stat_name, age_fun))
    mcl %>% dplyr::select(playerID, yearID=projectedYearID, proj_pa, proj_value)
    marcels_batting[[stat_name]] <- mcl$proj_value
    }


  a <- get_pitching_stats()
  b <- dplyr::tbl_df(append_previous_years(a %>% filter(POS=="P"), 
              get_seasonal_averages_pitching, 
              previous_years=3))
  
  mcl <- dplyr::tbl_df(apply_marcel_pitching(b, "SO", age_adjustment))
  marcels_pitching <- mcl %>% 
    dplyr::select(playerID, yearID=projectedYearID, proj_pt)
  for (stat_name in stats_to_project_pitching) {
    if ( stat_name %in% c("SO") ) {
      age_fun <- age_adjustment
    } else {
      age_fun <- age_adjustment_reciprocal
    }
    mcl <- dplyr::tbl_df(apply_marcel_pitching(b, stat_name, age_fun))
    mcl %>% select(playerID, yearID=projectedYearID, proj_pt, proj_value)
    marcels_pitching[[stat_name]] <- mcl$proj_value
  }
  
  roster_batting <- rbind(
    Lahman::Batting %>% filter(stint==1) %>% select(playerID, yearID, stint, teamID),
    get_roster_batting_X(upcoming_season)
  )
  roster_pitching <- rbind(
    Lahman::Pitching %>% filter(stint==1) %>% select(playerID, yearID, stint, teamID),
    get_roster_pitching_X(upcoming_season)
  )

  team_mapping <- rbind(
    Lahman::Teams,
    Lahman::Teams %>% filter(yearID==2019) %>% mutate(yearID=2020)
  )
  
  marcels_teams <- dplyr::tbl_df(
    get_team_projected_wins(marcels_batting=marcels_batting, 
                            roster_batting = roster_batting,
                            marcels_pitching=marcels_pitching,
                            roster_pitching = roster_pitching, 
                            team_mapping = team_mapping)
  )
  marcels <- list(Pitching=marcels_pitching, Batting=marcels_batting, Teams=marcels_teams)
  save(marcels, file=paste(output_dir, output_file, sep='/'))
}
