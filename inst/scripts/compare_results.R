
get_tango_results <- function() {
  a <- readr::read_csv('extdata/BattingMarcel2004.csv')
  b <- readr::read_csv('extdata/PitchingMarcel2004.csv')
  list(tangoBatting=a, tangoPitching=b)
}

compare_results <- function(pid) {
  tango_results <- get_tango_results()
  tmp <- marcels$Batting %>% 
    filter(yearID==2004) %>% 
    merge(tango_results$tangoBatting, by="playerID") %>% 
    filter(grepl(pid, playerID)) 

  tmp1 <- tmp %>% mutate(H=X1B+X2B+X3B+HR) %>% select(playerID, yearID, 
                         mPA=proj_pa, mH=H, m2B=X2B, m3B=X3B, mHR=HR, mSB=SB, mCS=CS,mSO=SO )
  tmp2 <- tmp %>% select(playerID, yearID, 
                         mPA, mH, m2B, m3B, mHR, mSB, mCS, mSO)
 
  xx = rbind(tmp1, tmp2) 
  row.names(xx) <- c("BD", "Tango")
  xx
  
}
                         

compare_results_p <- function(pid='glavito02') {
  tango_results <- get_tango_results()
  tmp <- marcels$Pitching %>% 
    filter(yearID==2004) %>% 
    merge(tango_results$tangoPitching, by="playerID") %>% 
    filter(grepl(pid, playerID)) 
  
  tmp1 <- tmp %>% mutate(mIP=proj_pt/3) %>% select(playerID, yearID, 
                         mIP, mH=H, mER=ER, mBB=BB, mSO=SO)
  tmp2 <- tmp %>% select(playerID, yearID, 
                         mIP, mH, mER, mBB, mSO)
  
  xx = rbind(tmp1, tmp2) 
  row.names(xx) <- c("BD", "Tango")
  xx
}

