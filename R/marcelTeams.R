
#' Get the typical PA for a team for a season.
#' 
#' @param year integer
#' @param pa_per_game numeric
#' The average number of PA per game. Defaults to 38.
#' 
#' @seealso \code{\link{get_ipouts_for_year}} \code{\link{get_game_for_year}}
#' @export
get_pa_for_year <- function(year, pa_per_game=38) {
  pa_per_game * get_games_for_year(year)
}


#' Get the typical IPouts for a team for a year.
#' 
#' @param year integer
#' @param ipouts_per_game numeric
#' The average number of PA per game. Defaults to 27.
#' 
#' @seealso \code{\link{get_pa_for_year}} \code{\link{get_game_for_year}}
#' @export
get_ipouts_for_year <- function(year, ipouts_per_game=27) {
  ipouts_per_game * get_games_for_year(year)
}

#' Get the expected number of games for a season.
#' 
#' Get the expected number of games for a season. This function does a very
#' basic switch-case to distinguish 3 eras; pre-1902 (140 game), 
#' pre-expansion (154 games), and present day (162 games). 
#'
#' @param year integer
#' 
#' @seealso \code{\link{get_pa_for_year}} \code{\link{get_ipouts_for_year}}
#' @export
get_games_for_year <- function(year) {
  if(year < 1902) {
    140
  } else if (year < 1961) {
    154
  } else {
    162
  }
}

#' Get team projected batting
#' 
#' @return A data frame with projected totals aggregated at the team-season level. 
#' Roster is taken from players that began the following year with the team.
#' @export
get_team_projected_batting <- function() {

  # TODO: use actual IBB. currently estimated as 8% BB
  # TODO: include GIDP in base runs formula
  
  projected_batting <- Lahman::Batting %>% mutate(yearID=yearID+1) %>%
    filter(stint==1) %>% select(playerID, yearID, teamID) %>%
    merge(marcels$Batting, by=c("playerID","yearID"))
  
  team_projected_batting <- projected_batting %>%
    tidyr::gather(key, value, -playerID, -yearID, -teamID) %>%
    group_by(yearID, teamID, key) %>%
    summarise(v=sum(value)) %>% tidyr::spread(key, v) %>%
    mutate(H=X1B+X2B+X3B+HR,
           OBP=(H+BB+HBP)/(proj_pa-SH),
           TB=X1B + 2*X2B + 3*X3B + 4*HR,
           AB=proj_pa-BB-HBP-SH-SF,
           SLG=TB/AB,
           BR_A=H+BB+HBP-(0.5*0.08*BB)-HR,
           BR_B=1.1*(1.4*TB - 0.6*H - 3*HR + 0.1*(BB+HBP-0.08*BB) + 0.9*(SB-CS)),
           BR_C=proj_pa-BB-SF-SH-HBP+CS,
           BR_D=HR,
           BSR=(BR_A*BR_B)/(BR_B+BR_C) + BR_D
    ) %>% group_by(yearID) %>% mutate(LG_BSR_PA=sum(BSR)/sum(proj_pa))
}

#' Get team projected pitching
#' 
#' @return A data frame with projected totals aggregated at the team-season level. 
#' Roster is taken from players that began the *following year* with the team.
#' 
#' @export
get_team_projected_pitching <- function(projected_pitching) {
  projected_pitching <- Lahman::Pitching %>% mutate(yearID=yearID+1) %>%
    filter(stint==1) %>% select(playerID, yearID, teamID) %>%
    merge(marcels$Pitching, by=c("playerID","yearID"))

team_projected_pitching <- projected_pitching %>%
  tidyr::gather(key, value, -playerID, -yearID, -teamID) %>%
  group_by(yearID ,teamID,key) %>%
  summarise(v=sum(value)) %>% tidyr::spread(key, v) %>%
  mutate(RA9=27*R/proj_pt) %>% group_by(yearID) %>% mutate(LG_RA_IPOUT=sum(R)/sum(proj_pt))
}


#' Get team projected wins
#' 
#' @return A data frame with projected win total aggregated at the team-season level. 
#' 
#' @seealso \code{\link{get_team_projected_pitching}, \link{get_team_projected_batting}}
#' @export
get_team_projected_wins <- function(team_projected_batting=NULL, team_projected_pitching=NULL) {
  
  if (is.null(team_projected_batting)) {
    team_projected_batting <- get_team_projected_batting()
  }
  
  if (is.null(team_projected_pitching)) {
    team_projected_pitching <- get_team_projected_pitching()
  }
  
  team_projections <- team_projected_batting %>%
    merge(team_projected_pitching,
          by=c("yearID", "teamID"), suffixes=c("_batting", "_pitching"))
  
  team_runs <- team_projections %>%
    select(yearID, teamID, BSR, LG_BSR_PA, proj_pa, R, LG_RA_IPOUT, proj_pt)
  team_runs$target_pa <-  sapply(team_projections$yearID, get_pa_for_year)
  team_runs$target_ipouts <-  sapply(team_projections$yearID, get_ipouts_for_year)
  
  team_runs <- team_runs %>% mutate(dx=target_pa-proj_pa,
                                    dx=ifelse(dx<0, 0, dx),
                                    z=dx*LG_BSR_PA,
                                    y=ifelse(target_pa>proj_pa, BSR+z, BSR*target_pa/proj_pa),
                                    CORRECTED_BSR=y) %>% select(-dx, -z, -y)
  
  team_runs <- team_runs %>% mutate(dx=target_ipouts-proj_pt,
                                    dx=ifelse(dx<0, 0, dx),
                                    z=dx*LG_RA_IPOUT,
                                    y=ifelse(target_ipouts>proj_pt, R+z, R*target_ipouts/proj_pt),
                                    CORRECTED_R=y) %>% select(-dx, -z, -y)
  
  team_wins <- team_runs %>%
    group_by(yearID) %>%
    mutate(x1=sum(CORRECTED_BSR), x2=sum(CORRECTED_R), ff=x2/x1) %>%
    mutate(CORRECTED_R=CORRECTED_R/ff) %>%
    select(yearID, teamID, BSR, CORRECTED_BSR, R, CORRECTED_R) %>%
    mutate(z=(CORRECTED_R/CORRECTED_BSR)**2, wpct=1/(1+z)) %>%
    group_by(yearID) %>%
    mutate(x1=mean(wpct), x2=0.5, ff=x2/x1) %>%
    mutate(wpct=wpct*ff)
  
  team_wins$games <- sapply(team_wins$yearID, get_games_for_year)
  team_wins <- team_wins %>% mutate(wins=as.integer(0.5 + games*wpct), losses=games-wins) %>% 
    select(yearID, teamID, BSR, CORRECTED_BSR, R, CORRECTED_R, games, wins, losses, wpct)
  
  Lahman::Teams %>% select(yearID, teamID, lgID, divID) %>% merge(team_wins)
  
}


standings_pretty_print_div <- function(standings, lg_id, div_id) {
 
  div_lookup <- list(E='East', W='West', C='Central')
  if (is.na(div_id)) {
    div_str <- " "
  } else {
    div_str <- div_lookup[[div_id]]
  }
  
  if (is.na(div_id)) {
    div_standings <- standings %>% filter(lgID==lg_id) 
  } else {
    div_standings <- standings %>% filter(lgID==lg_id, divID==div_id) 
  }
  
  datum <- div_standings[1,]
  s <- '--------------- \n'
  cat(s)
  cat(sprintf(' %s %s %d \n', lg_id, div_str, datum$yearID))
  cat('--------------- \n')
  s= sprintf('    | team |    W |    L |  Wpct |    R |   RA\n')
  cat(s)
  s <-        '----------------------------------------------\n'
  cat(s)
  for (idx in 1:nrow(div_standings)) {
    datum <- div_standings[idx,]
    s= sprintf(' %2d | %4s | %4d | %4d | %.3f | %4d | %4d\n', 
               datum$standing, datum$teamID, datum$wins, datum$losses, 
               datum$wpct, as.integer(datum$CORRECTED_BSR), as.integer(datum$CORRECTED_R)
               )
    cat(s)
  }
}

get_standings <- function(marcels_team, season) {
  standings <- marcels_team %>% 
    filter(yearID==season) %>% 
    group_by(lgID, divID) %>% 
    arrange(-wins, teamID) %>% 
    mutate(standing=row_number()) %>% 
    ungroup() 
}

standings_pretty_print <- function(marcels_team, season) {
  standings <- get_standings(marcels_team, season)
  
  lg_divs <- standings %>% group_by(lgID, divID) %>% summarise()
  for (idx in 1:nrow(lg_divs)) {
    lg_id <- lg_divs[idx,]$lgID
    div_id <- lg_divs[idx,]$divID
  standings_pretty_print_div(standings, lg_id, div_id)
  }

}
