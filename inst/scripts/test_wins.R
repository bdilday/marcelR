
tt0 = Lahman::Teams %>% mutate(Wpct=W/(W+L)) %>% select(yearID, teamID, W, Wpct)
tt1 = tt0 %>% mutate(yearID=yearID+1)
tt2 = tt0 %>% mutate(yearID=yearID+2)
tt3 = tt0 %>% mutate(yearID=yearID+3)

tt_all <- tt0 %>% 
  merge(tt1, by=c("yearID", "teamID"), suffixes=c("", "1")) %>% 
  merge(tt2, by=c("yearID", "teamID"), suffixes=c("", "2")) %>%
  merge(tt3, by=c("yearID", "teamID"), suffixes=c("", "3"))

tt_all$wav = 0.5 * tt_all$W/tt_all$Wpct 
tt_all$pav = 0.5 