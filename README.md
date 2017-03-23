# marcelR

This package generates Marcel projections, using data from the `Lahman` package. 

## Brief introduction to Marcels

Marcels are a projection system for baseball. It is often described as the most basic projection system. It weights a players last three seasons, regresses this to the mean, and applies an age adjustment.

## Marcel data

This package includes the marcels as a set of data frames.

``` {r}
> library(marcelR)
> data(marcels)
> names(marcels)
[1] "Pitching" "Batting"  "Teams"   

> nrow(marcels$Batting)
[1] 53570

> nrow(marcels$Pitching)
[1] 40516

> nrow(marcels$Teams)
[1] 2675

```

### Batting

Here's an example of a projection, illustrated with Carlos Beltran for 2004.

``` {r}
library(dplyr)
marcels$Batting %>% 
 filter(yearID==2003, playerID=='beltrca01') %>% 
 print.data.frame()
   playerID yearID proj_pa      X1B      X2B      X3B       HR       BB      HBP       SB       CS       SO
1 beltrca01   2003   573.2 92.69494 25.20517 7.579383 22.23753 57.08782 3.342505 28.80157 3.744408 94.67932
        SH       SF
1 1.381183 5.493292
```

The highest projected HR,

``` {r}
> marcels$Batting %>% arrange(-HR) %>% select(playerID, yearID, HR)
# A tibble: 53,570 × 3
    playerID yearID       HR
      <fctr>  <int>    <dbl>
1  mcgwima01   1998 50.54582
2  mcgwima01   1999 50.48674
3  bondsba01   2001 47.66849
4   sosasa01   2001 46.64636
5   sosasa01   2000 46.01002
6  mcgwima01   1997 45.93436
7   sosasa01   1999 45.73958
8  griffke02   1998 44.99480
9  bondsba01   2002 43.52437
10 rodrial01   2002 42.50002
```

### Pitching

Lowest projected RA9 since 1950.

``` {r}
> marcels$Pitching %>% 
   mutate(RA9=27*R/proj_pt) %>% 
   arrange(RA9) %>% 
   filter(yearID>=1950) %>% 
   select(playerID, yearID, RA9)
# A tibble: 28,840 × 3
    playerID yearID      RA9
      <fctr>  <int>    <dbl>
1  gibsobo01   1968 2.394492
2  koufasa01   1964 2.415229
3  goodedw01   1985 2.447113
4  koufasa01   1966 2.451025
5  gibsobo01   1969 2.457587
6  kershcl01   2014 2.461541
7  koufasa01   1965 2.488301
8  maddugr01   1995 2.509565
9  kimbrcr01   2013 2.521602
10 kershcl01   2015 2.526198
# ... with 28,830 more rows
```

### Teams

Highest projected winning percentage since 1913,
``` {r}
> marcels$Teams %>% filter(yearID>=1913) %>% arrange(-wpct) %>% select(yearID, teamID, wpct)
# A tibble: 2,221 × 3
   yearID teamID      wpct
    <int> <fctr>     <dbl>
1    1944    SLN 0.6202040
2    1928    NYA 0.6176024
3    1942    NYA 0.6116650
4    1998    ATL 0.6096477
5    1932    NYA 0.6086274
6    1939    NYA 0.6081185
7    1913    NY1 0.6018390
8    1970    BAL 0.6011749
9    1938    NYA 0.6010655
10   1943    SLN 0.5987925
# ... with 2,211 more rows
```

## Marcel computations

### Data exporting

The marcel data is exported in the `marcel_data_exporter.R` script. The low-level functions to compute the marcels are also included, however. Examples are given below.

### Batting

For batting stats, the weights given to the previous three seasons are 5, 4, and 3, and the amount of regression is 100 PA. 

An example of computing marcels for batting stats,


``` {r}
> a <- get_batting_stats()
> b <- dplyr::tbl_df(append_previous_years(a %>% filter(POS!="P"), 
                                           get_seasonal_averages_batting, 
                                           previous_years = 3))
> mcl <- dplyr::tbl_df(apply_marcel_batting(b, "HR", age_adjustment))
> mcl %>% filter(yearID==2003, playerID=='beltrca01') %>% print.data.frame()
   playerID yearID projectedYearID age_adj x_metric x_pa       x_av proj_pa      num denom proj_rate_raw
1 beltrca01   2003            2004   1.006      318 7938 0.02866513   573.2 352.3982  9138    0.03856403
   proj_rate proj_value
1 0.03879542   22.23753

```

### Pitching

For pitching stats, the weights given to the previous three seasons are 3, 2, and 1, and the amount of regression is 134 Outs, or about 44.2 innings.

An example of computing marcels for pitching stats,

``` {r}
> a <- get_pitching_stats()
> b <- dplyr::tbl_df(append_previous_years(a %>% filter(POS=="P"), 
              get_seasonal_averages_pitching, 
              previous_years=3))
> mcl <- dplyr::tbl_df(apply_marcel_pitching(b, "H", age_adjustment))
> mcl  %>% tail(1) %>% print.data.frame()
   playerID yearID projectedYearID age_adj x_metric x_pt    x_lgav proj_pt      num denom proj_rate_raw
1 richaga01   2016            2017       1      579 2062 0.3222663   294.2 838.1021  2866     0.2924292
  proj_rate proj_value
1 0.2924292   86.03268
```
  
### Teams

Team win projections aren't strictly a part of the marcel specification. Marcels are used in the follwing way to project wins.

* Specify a roster of batters and pitchers. In practice this comes from the players that actually played in the subsequent season, based on `Lahman` data.

* Given the assumed roster, aggregate batting and pitching stats based on projected playing time.

* Apply Base Runs to estimate the number of runs scored on offense.

* Use estimated RA9 from the pitching projections directly for estimating runs allowed.

* Adjust the estimated runs and runs-allowed to a common number of PA.

* Apply the Pythagorean win formula to these adjusted runs estimates.

