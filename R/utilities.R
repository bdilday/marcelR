#' @import Lahman
#' 
na.zero <- function(x) {
  x[is.na(x)] <- 0
  x
}

age_adjustment <- function(age) {
  if (is.na(age)) {
    1
  } else if (age <= 0) { #johnsbi01 ?
    1
  } else if (age > 29) {
    1 /(1 + 0.003 * (age-29))
  } else if (age<29) {
    1 + 0.006 * (29-age)
  } else {
    1
  }
}

#' Age adjustment - re
age_adjustment_reciprocal <- function(age) {
  if (is.na(age)) {
    1
  } else if (age <= 0) { #johnsbi01 ?
    1
  } else if (age > 29) {
    1 + 0.003 * (age-29)
  } else if (age<29) {
    1/(1 + 0.006 * (29-age))
  } else {
    1
  }
}

#' Get primary position
#' 
#' Gets primary position based on `Fielding` table from the 
#' `Lahman` package. Uses the categories from the Lahman Fielding table 
#' so all OF positons are collapsed into `OF`. Main use is to filter
#' pitchers before computing seasonal averages batting stats.
#' 
#' @return A data frame containing playerID, yearID, primary position, 
#' and number of games at primary position.
#' @import dplyr
#' @export
#' @examples 
#' primary_pos <- get_primary_pos()
#' primary_pos %>% group_by(POS) %>% summarise(n=n())
get_primary_pos <- function(year=NULL) {
  
  if (is.null(year)) {
    fielding <- Lahman::Fielding
  } else {
    fielding <- Lahman::Fielding %>% filter(yearID==year) 
  }
  
  PrimaryPosition <- fielding %>%
    group_by(playerID, yearID, POS) %>%
    summarise(n.game = sum(G)) %>%
    arrange(playerID, yearID, desc(n.game)) %>%
    mutate(rr=row_number()) %>%
    filter(rr==1) %>%
    select(-rr) %>%
    ungroup()
}

sum_stints <- function(data, columns_to_sum) {
  grouped_data <- data %>% dplyr::group_by(playerID, yearID)
  for (column in columns_to_sum) {
    tmp <- grouped_data %>% 
      dplyr::mutate_(var = lazyeval::interp(~sum(var, na.rm = TRUE), 
                                           var = as.name(column)))
    grouped_data[[column]] <- tmp$var
  }
  grouped_data
}

get_age <- function(data) {
  
  tmp <- data %>%
    merge(Lahman::Master %>% select(playerID, birthYear), by="playerID") %>%
    mutate(age = yearID-birthYear) %>%
    select(-birthYear)
  
  tmp$age
} 

append_previous_years <- function(data, average_fn, previous_years=3) {
  data$key <- paste0(data$playerID, data$yearID)
  all_data <- data 
  df_list <- list()
  for (year_offset in 1:previous_years) {
    tmp <- data %>% dplyr::mutate(yearID = yearID+year_offset-1)
    tmp$key <- paste0(tmp$playerID, tmp$yearID)
    df_list[[year_offset]] <- tmp
  }
  
  seasonal_averages <- average_fn(data)
  for(idx in 1:3) {
    this_df = df_list[[idx]]
    all_data <- merge(all_data, this_df,
                      by="key",
                      suffixes = c("",sprintf(".%d", idx)), 
                      all.x=TRUE)
    
    this_sa <- average_fn(this_df)
    seasonal_averages <- merge(seasonal_averages, this_sa,
                               by="yearID",
                               suffixes = c("",sprintf(".%d", idx)), 
                               all.x=TRUE)
    
  }
  
  all_data %>% merge(seasonal_averages, 
                     by="yearID", 
                     suffixes=c("", ".SA"))
}

get_target_value <- function(projections, metric, metric_weights) {
  
}

