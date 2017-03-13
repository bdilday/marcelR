
get_primary_pos <- function() {
  PrimaryPosition <- Lahman::Fielding %>%
    group_by(playerID, yearID, POS) %>%
    summarise(n.game = sum(G)) %>%
    arrange(playerID, yearID, desc(n.game)) %>%
    mutate(rr=row_number()) %>%
    filter(rr==1) %>%
    select(-rr) %>%
    ungroup()
}

get_pitching_stats <- function(PrimaryPosition=NULL) {
  if (is.null(PrimaryPosition)) {
    PrimaryPosition <- marcelR:::get_primary_pos()
  }
  
  # TODO: combine stints properly for pitchers
  PitchingStats <- Lahman::Pitching %>% filter(stint==1)
  PitchingStats
}


get_batting_stats <- function(PrimaryPosition=NULL) {
  if (is.null(PrimaryPosition)) {
    PrimaryPosition <- marcelR:::get_primary_pos()
  }
  
  BattingStats <- marcelR:::combine_batter_stints(Lahman::battingStats()) %>% 
    merge(PrimaryPosition %>%
            select(playerID, yearID, POS),
          by=c("playerID", "yearID"))
  
  BattingStats$Age <- get_age(BattingStats)
  BattingStats
}


append_previous_years <- function(data, previous_years=3) {
  data$key <- paste0(data$playerID, data$yearID)
  all_data <- data 
  df_list <- list()
  for (year_offset in 1:previous_years) {
    tmp <- data %>% dplyr::mutate(yearID = yearID+year_offset-1)
    tmp$key <- paste0(tmp$playerID, tmp$yearID)
    df_list[[year_offset]] <- tmp
  }

  seasonal_averages <- get_seasonal_averages(data)
  for(idx in 1:3) {
    this_df = df_list[[idx]]
    all_data <- merge(all_data, this_df,
                    by="key",
                    suffixes = c("",sprintf(".%d", idx)), 
                    all.x=TRUE)
    
    this_sa <- get_seasonal_averages(this_df)
    seasonal_averages <- merge(seasonal_averages, this_sa,
                by="yearID",
                suffixes = c("",sprintf(".%d", idx)), 
                all.x=TRUE)
    
  }
  all_data$age_adj <- sapply(all_data$Age+1, age_adjustment)

  all_data %>% merge(seasonal_averages, 
                     by="yearID", 
                     suffixes=c("", ".SA"))
}

age_adjustment <- function(age) {
  if (is.na(age)) {
    1
  } else if (age <= 0) { #johnsbi01 ?
    1
  } else if (age > 29) {
    1 / (1 + 0.006 * (age-29))
  } else if (age<29) {
    1 + 0.003 * (29-age)
  } else {
    1
  }
}

apply_marcel <- function(metric_av_df, metric, 
                         ww=c(5, 4, 3), 
                         pa_weights=c(0.5, 0.1, 0)) {
  sw <- sum(ww)
 
  x_metric = 0
  x_pa = 0
  x_av = 0
  proj_pa = 200

  for (idx in seq_along(ww)) {
    metric_key = sprintf('%s.%d', metric, idx)
    metric_av_key = paste0(metric_key, ".SA")
    pa_key = sprintf('%s.%d', "PA", idx)
    pa <- na.zero(metric_av_df[[pa_key]])
    sa_value <- na.zero(metric_av_df[[metric_av_key]])
    
    x_metric <- x_metric + na.zero(metric_av_df[[metric_key]]) * ww[idx]
    x_pa <- x_pa + pa * ww[idx]
    x_av <- x_av + (sa_value * ww[idx])
    proj_pa <- proj_pa + pa_weights[[idx]] * pa
  }

  x_av <- x_av / sw
  data.frame(playerID=metric_av_df$playerID,
             yearID=metric_av_df$yearID,
             projectedYearID=metric_av_df$yearID+1,
             age_adj=metric_av_df$age_adj,
             x_metric=x_metric,
             x_pa=x_pa, x_av=x_av, proj_pa=proj_pa) %>%
    mutate(num=x_av*100*sw+x_metric, 
           denom=x_pa+100*sw,
           proj_rate_raw=num/denom,
           proj_rate=age_adj*proj_rate_raw,
           proj_value=proj_pa*proj_rate)

}


