# marcelR

This package generates Marcel projections, using data from the `Lahman` package. 

## Brief introduction to Marcels

Marcels describes a projection system for baseball, first developed by Tom Tango. It is often described as the most basic projection system. It weights a players last three seasons, regresses this to the mean, and applies an age adjustment.

## Installing 

Since this is not on CRAN, it needs to be installed from github,

``` {r}
> library(devtools)
> install_github('bdilday/marcelR')
> library(marcelR)
```

As of this writing the `Lahman` package has not been updated to include the 2017+ seasons. I created an updated version, however, that can be used for generating projections for the 2018-2020 seasons. It can be installed from github as well,

``` {r}
> install_github('bdilday/Lahman')
> max(Lahman::Batting$yearID)
[1] 2019
```

## Marcel data

This package includes the marcels as a set of data frames.

``` {r}
> data(marcels)
> names(marcels)
[1] "Pitching" "Batting"  "Teams"   

> nrow(marcels$Batting)
[1] 56622

> nrow(marcels$Pitching)
[1] 42777

> nrow(marcels$Teams)
[1] 2877

```

### Batting

Here's an example of a projection, illustrated with Carlos Beltran for 2004.

``` {r}
> library(dplyr)
> marcels$Batting %>% 
     filter(yearID==2004, playerID=='beltrca01') %>% 
     print.data.frame()
   playerID yearID proj_pa    X1B      X2B      X3B       HR       BB      HBP       SB
1 beltrca01   2004   573.2 93.154 25.37286 7.483096 22.70545 58.44974 3.351604 29.40546
        CS      SO       SH       SF
1 3.721516 92.3098 1.338349 5.604223
```

The highest projected HR,

``` {r}
> marcels$Batting %>% arrange(-HR) %>% select(playerID, yearID, HR)
# A tibble: 54,730 × 3
    playerID yearID       HR
      <fctr>  <dbl>    <dbl>
1  mcgwima01   1999 52.27053
2  mcgwima01   2000 51.93934
3  bondsba01   2002 49.70076
4   sosasa01   2002 48.08998
5  mcgwima01   1998 47.17453
6   sosasa01   2001 46.87696
7   sosasa01   2000 46.38852
8  griffke02   1999 45.72925
9  bondsba01   2003 45.71398
10  sosasa01   2003 43.79848
# ... with 54,720 more rows
```

### Pitching

Lowest projected RA9 since 1950.

``` {r}
> marcels$Pitching %>% 
   mutate(RA9=27*R/proj_pt) %>% 
   arrange(RA9) %>% 
   filter(yearID>=1950) %>% 
   select(playerID, yearID, RA9)
   # A tibble: 29,054 × 3
    playerID yearID      RA9
      <fctr>  <dbl>    <dbl>
1  gibsobo01   1970 2.409111
2  goodedw01   1986 2.417237
3  gibsobo01   1969 2.439491
4  koufasa01   1965 2.451923
5  koufasa01   1967 2.473709
6  kershcl01   2015 2.480347
7  koufasa01   1966 2.525069
8  kershcl01   2016 2.525205
9  kimbrcr01   2014 2.529273
10 maddugr01   1996 2.530020
# ... with 28,830 more rows
```

### Teams

Highest projected winning percentage since 1913,
``` {r}
> marcels$Teams %>% filter(yearID>=1913) %>% arrange(-wpct) %>% select(yearID, teamID, wpct)
# A tibble: 2,290 × 3
   yearID teamID      wpct
    <dbl> <fctr>     <dbl>
1    1940    NYA 0.6175461
2    1928    NYA 0.6119175
3    1952    NY1 0.6096779
4    1913    NY1 0.6089000
5    1953    BRO 0.6082399
6    1934    CHN 0.6039637
7    2017    CHN 0.6039189
8    2004    BOS 0.6038871
9    1921    NYA 0.6037286
10   1941    NYA 0.6033883
# ... with 2,280 more rows
```

As of this writing, the Batting and Pitching stats have been updated to 2019, but the team projectison have not (waiting on creation of updated rosters).

## Marcel computations

### Data exporting

The marcel data is exported in the `marcel_data_exporter.R` script. The low-level functions to compute the marcels are also included, however. Examples are given below.

### Batting

For batting stats, the weights given to the previous three seasons are 5, 4, and 3, and the amount of regression is 100 PA. 

An example of computing marcels for batting stats,


``` {r}
> a <- get_batting_stats()
> b <- dplyr::tbl_df(marcelR:::append_previous_years(a %>% filter(POS!="P"), 
                                           get_seasonal_averages_batting, 
                                           previous_years = 3))
> mcl <- dplyr::tbl_df(apply_marcel_batting(b, "HR", marcelR:::age_adjustment))
> mcl %>% filter(projectedYearID==2004, playerID=='beltrca01') %>% print.data.frame()
   playerID yearID projectedYearID age_adj x_metric x_pa       x_av proj_pa metric_target
1 beltrca01   2003            2004   1.012      318 7938 0.02867754   573.2    0.02868739
      num denom proj_rate_raw  proj_rate proj_value metric_agg proj_value_floating
1 352.413  9138    0.03856566 0.03902845   22.70545 0.02826497            22.37111
  metric_multiplier
1          1.014945
```

### Pitching

For pitching stats, the weights given to the previous three seasons are 3, 2, and 1, and the amount of regression is 134 Outs, or about 44.2 innings.

An example of computing marcels for pitching stats,

``` {r}
> a <- get_pitching_stats()
> b <- dplyr::tbl_df(marcelR:::append_previous_years(a %>% filter(POS=="P"), 
              get_seasonal_averages_pitching, 
              previous_years=3))
> mcl <- dplyr::tbl_df(apply_marcel_pitching(b, "R", marcelR:::age_adjustment_reciprocal))
> mcl %>% filter(projectedYearID==2017) %>% mutate(RA9=27*proj_value/proj_pt) %>% arrange(RA9) %>% head(4) %>% print.data.frame()
   playerID yearID projectedYearID age_adj x_metric x_pt    x_lgav proj_pt metric_target
1 kershcl01   2016            2017   1.000      259 3332 0.1607082   473.3     0.1616625
2 brittza01   2016            2017   1.003       70 1226 0.1614394   195.2     0.1616625
3 daviswa01   2016            2017   1.009       51 1010 0.1602990   160.2     0.1616625
4 millean01   2016            2017   1.009       87 1226 0.1621735   205.0     0.1616625
       num denom proj_rate_raw  proj_rate proj_value metric_agg proj_value_floating
1 388.2094  4136    0.09386108 0.09386108   44.42786  0.1616501            44.42445
2 199.7973  2030    0.09842229 0.09871756   19.27115  0.1616501            19.26967
3 179.8804  1814    0.09916231 0.10005477   16.03000  0.1616501            16.02877
4 217.3875  2030    0.10708744 0.10805123   22.15220  0.1616501            22.15050
  metric_multiplier      RA9
1          1.000077 2.534444
2          1.000077 2.665579
3          1.000077 2.701686
4          1.000077 2.917607
```
  
### Teams

Team win projections aren't strictly a part of the marcel specification. In this package, marcels are used in the following way to project wins.

* Specify a roster of batters and pitchers. In practice this comes from the players that actually played in the subsequent season, based on `Lahman` data.

* Given the assumed roster, aggregate batting and pitching stats based on projected playing time.

* Apply Base Runs to estimate the number of runs scored on offense.

* Use estimated RA9 from the pitching projections directly for estimating runs allowed.

* Adjust the estimated runs and runs-allowed to a common number of PA.

* Apply the Pythagorean win formula to these adjusted runs estimates.

