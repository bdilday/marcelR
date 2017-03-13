
na.zero <- function(x) {
  x[is.na(x)] <- 0
  x
}

get_seasonal_averages <- function(data) {
  
  data %>%
    dplyr::select(-stint, -lgID) %>%
    tidyr::gather(key, value, -playerID, -yearID, -teamID, -PA) %>%
    dplyr::mutate(value=as.numeric(value)) %>% group_by(key, yearID) %>%
    dplyr::summarise(n=n(),
                     mm=mean(value, na.rm=TRUE),
                     ss=sd(value, na.rm=TRUE),
                     zz=sum(value, na.rm=TRUE),
                     all.pa=sum(PA, na.rm=TRUE),
                     lgAv = zz/all.pa
    ) %>%
    dplyr::ungroup() %>% 
    dplyr::select(yearID, key, lgAv) %>% 
    tidyr::spread(key, lgAv)
}



combine_batter_stints <- function(data) {
  
  columns.to.sum <- c("G","PA","AB",
                      "H","X2B","X3B","HR",
                      "R","RBI"
                      ,"SB","CS","BB","SO","IBB","HBP","SH","SF","GIDP")
  
  grouped_data <- data %>% dplyr::group_by(playerID, yearID)
  for (column in columns.to.sum) {
    tmp <- grouped_data %>% dplyr::mutate_(var = lazyeval::interp(~sum(var, na.rm = TRUE), 
                                                                  var = as.name(column)))
    grouped_data[[column]] <- tmp$var
  }
  
  grouped_data %>% dplyr::mutate(OB = OBP * (PA-SH), 
                                 BIP=AB-SO-HR+SF, 
                                 HOBIP=H-HR, 
                                 OBP=sum(OB)/sum(PA-SH), 
                                 SLG=sum(TB)/sum(AB),
                                 BABIP=sum(HOBIP)/sum(BIP))
  
  grouped_data %>% dplyr::ungroup() %>% dplyr::filter(stint==1)
}

get_age <- function(data) {
  
  tmp <- data %>%
    merge(Lahman::Master %>% select(playerID, birthYear), by="playerID") %>%
    mutate(age = yearID-birthYear) %>%
    select(-birthYear)
  
  tmp$age
} 

merge.fit.for.metric <- function(in.data.stats,
                                 in.data.averages,
                                 metric) {
  
  df1 <- in.data.averages %>%
    dplyr::filter(key==metric) %>%
    dplyr::mutate(lgAv=mm/all.pa) %>%
    dplyr::select(yearID, lgAv)
  
  idx.list <- c(which(grepl(sprintf('^%s$', metric), names(in.data.stats))),
                which(grepl(sprintf('^%s\\.', metric), names(in.data.stats))),
                which(grepl('^yearID', names(in.data.stats))),
                which(grepl('^PA', names(in.data.stats))),
                which(grepl('^age', names(in.data.stats))),
                which(grepl('^playerID', names(in.data.stats)))
  )
  
  df2 <-  merge(in.data.stats[,idx.list], df1, by="yearID")
  
}


fit_projections <- function(in.data, metric) {
  
  ans <- lm( OBP ~ . + I(age**2) - PA.1 - PA.2-PA.3-PA - age.1 - age.2 - age.3 - yearID-1,
             data=kk %>% mutate(lgAv=lgAv*600),
             subset = which(in.data$yearID>=1970))
  
}
