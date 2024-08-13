Week 3 NYPD Shootings Rmd
================
M. Aragon
2024-08-12

``` r
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggplot2)
```

# Week 3 NYPD Shooting Analysis

### Objective

While reviewing the data for this project, I’d like to answer the
following questions:

- What trend over time, if any, exists within the data? I’d like to
  understand when shootings generally occur;
- What does the data tell me about each borough? Based on the shootings,
  where would be the safest place to visit based on the data?;
- After answering the above, where would be the optimal place to visit
  and what time of year?

### How to retrieve the NYPD Shooting data

1.  You’ll want to add the tidyverse library to your session. I’ve done
    this at the top of this rmd.

2.  Obtain the correct URL for the file. Take a look at this and ensure
    it is what you need.
    <https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD>

3.  Call the read_csv import, passing it the URL from Step 2. Place this
    into a variable.

- I’m also creating a variable to pass to my visualizations to help with
  the presentation of each viz.

``` r
shootings <- read_csv(file="https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD")
```

    ## Rows: 28562 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (12): OCCUR_DATE, BORO, LOC_OF_OCCUR_DESC, LOC_CLASSFCTN_DESC, LOCATION...
    ## dbl   (7): INCIDENT_KEY, PRECINCT, JURISDICTION_CODE, X_COORD_CD, Y_COORD_CD...
    ## lgl   (1): STATISTICAL_MURDER_FLAG
    ## time  (1): OCCUR_TIME
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
breaksOverTheWholeRange = 12
```

### Review and transform the data

1.  Review the data, outputting the results of the variable from the
    prior step. In the data set, I can see:

- What appears to be a unique value for each record in the data,
- When the shooting took place,
- Where the shooting took place (sometimes this data is missing),
- What general values were captured regarding the shooting (perpetrator
  information as well as victim information), and,
- Whether or not the shooting resulted in a murder.

``` r
shootings
```

    ## # A tibble: 28,562 × 21
    ##    INCIDENT_KEY OCCUR_DATE OCCUR_TIME BORO      LOC_OF_OCCUR_DESC PRECINCT
    ##           <dbl> <chr>      <time>     <chr>     <chr>                <dbl>
    ##  1    244608249 05/05/2022 00:10      MANHATTAN INSIDE                  14
    ##  2    247542571 07/04/2022 22:20      BRONX     OUTSIDE                 48
    ##  3     84967535 05/27/2012 19:35      QUEENS    <NA>                   103
    ##  4    202853370 09/24/2019 21:00      BRONX     <NA>                    42
    ##  5     27078636 02/25/2007 21:00      BROOKLYN  <NA>                    83
    ##  6    230311078 07/01/2021 23:07      MANHATTAN <NA>                    23
    ##  7    229224142 06/07/2021 19:55      QUEENS    <NA>                   113
    ##  8    231246224 07/22/2021 01:47      BROOKLYN  <NA>                    77
    ##  9    228559720 05/22/2021 18:39      BRONX     <NA>                    48
    ## 10    238210279 12/22/2021 23:17      BRONX     <NA>                    49
    ## # ℹ 28,552 more rows
    ## # ℹ 15 more variables: JURISDICTION_CODE <dbl>, LOC_CLASSFCTN_DESC <chr>,
    ## #   LOCATION_DESC <chr>, STATISTICAL_MURDER_FLAG <lgl>, PERP_AGE_GROUP <chr>,
    ## #   PERP_SEX <chr>, PERP_RACE <chr>, VIC_AGE_GROUP <chr>, VIC_SEX <chr>,
    ## #   VIC_RACE <chr>, X_COORD_CD <dbl>, Y_COORD_CD <dbl>, Latitude <dbl>,
    ## #   Longitude <dbl>, Lon_Lat <chr>

2.  Tidy and Transform the data

- Rename fields to make them easier to read/display
- Modify the OccurrenceDate (from the prior step) to be a date field
  rather than a character field
- Remove fields that we won’t use
- Place null or NA values into the UNKNOWN perpetrator race category
- Review the revised data set

``` r
shootings <- shootings %>% 
rename(OccurrenceDate = OCCUR_DATE, OccurrenceTime = OCCUR_TIME, Borough = BORO, Location = LOCATION_DESC, Murder = STATISTICAL_MURDER_FLAG, PerpetratorRace = PERP_RACE, VictimRace = VIC_RACE, VictimSex = VIC_SEX) %>%
mutate(OccurrenceDate = mdy(OccurrenceDate)) %>%
mutate(PerpetratorRace = case_when(PerpetratorRace == "AMERICAN INDIAN/ALASKAN NATIVE" ~ "AMERICAN INDIAN/ALASKAN NATIVE",
                                  PerpetratorRace == "ASIAN / PACIFIC ISLANDER" ~ "ASIAN / PACIFIC ISLANDER",
                                  PerpetratorRace == "BLACK" ~ "BLACK",
                                  PerpetratorRace == "BLACK HISPANIC" ~ "BLACK HISPANIC",
                                  PerpetratorRace == "WHITE" ~ "WHITE",
                                  PerpetratorRace == "WHITE HISPANIC" ~ "WHITE HISPANIC",
                                  TRUE ~ "UNKNOWN")) %>%
select(-c(INCIDENT_KEY, LOC_OF_OCCUR_DESC, Lon_Lat, X_COORD_CD, Y_COORD_CD, JURISDICTION_CODE, LOC_CLASSFCTN_DESC, PERP_AGE_GROUP, Latitude, Longitude, Location, PERP_AGE_GROUP, PERP_SEX))

shootings
```

    ## # A tibble: 28,562 × 9
    ##    OccurrenceDate OccurrenceTime Borough   PRECINCT Murder PerpetratorRace
    ##    <date>         <time>         <chr>        <dbl> <lgl>  <chr>          
    ##  1 2022-05-05     00:10          MANHATTAN       14 TRUE   BLACK          
    ##  2 2022-07-04     22:20          BRONX           48 TRUE   UNKNOWN        
    ##  3 2012-05-27     19:35          QUEENS         103 FALSE  UNKNOWN        
    ##  4 2019-09-24     21:00          BRONX           42 FALSE  UNKNOWN        
    ##  5 2007-02-25     21:00          BROOKLYN        83 FALSE  BLACK          
    ##  6 2021-07-01     23:07          MANHATTAN       23 FALSE  UNKNOWN        
    ##  7 2021-06-07     19:55          QUEENS         113 TRUE   UNKNOWN        
    ##  8 2021-07-22     01:47          BROOKLYN        77 FALSE  UNKNOWN        
    ##  9 2021-05-22     18:39          BRONX           48 FALSE  UNKNOWN        
    ## 10 2021-12-22     23:17          BRONX           49 TRUE   WHITE HISPANIC 
    ## # ℹ 28,552 more rows
    ## # ℹ 3 more variables: VIC_AGE_GROUP <chr>, VictimSex <chr>, VictimRace <chr>

3.  Make a new column of data to allow us to summarize Murders over time

``` r
shootings$Murders <- with(shootings, ifelse(Murder == TRUE, 1, 0))
```

4.  Do the same for shootings. Note: Everything in the file is a
    shooting, we just want a number to be able to summarize it.

``` r
shootings$Shootings <- with(shootings, 1)
```

5.  Add a column for the YearQuarter and Quarter and Year. Note: As I
    worked with the data, I tried many variables to see how the data
    would be best presented. So, yes, I am carrying some extra,
    unnecessary variables, which can be removed.

``` r
shootings$YearQuarter <- with(shootings, quarter(OccurrenceDate, with_year = T))
shootings$Quarter <- with(shootings, quarter(OccurrenceDate, with_year = F))
shootings$Year <- with(shootings, year(OccurrenceDate))
shootings$YearMonth <-with(shootings, as.numeric(format(OccurrenceDate, '%Y%m')))
```

6.  Assign color to the various perpetrator races. Note: This is one
    variable that I could not get wired into the visualizations.

``` r
shootings$PerpetratorRaceColor <- 
  with(shootings, case_when(
                        PerpetratorRace == "AMERICAN INDIAN/ALASKAN NATIVE" ~ "green",
                        PerpetratorRace == "ASIAN / PACIFIC ISLANDER" ~ "red",
                        PerpetratorRace == "BLACK" ~ "black",
                        PerpetratorRace == "BLACK HISPANIC" ~ "brown",
                        PerpetratorRace == "WHITE" ~ "white",
                        PerpetratorRace == "WHITE HISPANIC" ~ "tan",
                        PerpetratorRace == "UNKNOWN" ~ "lightgrey"))
```

7.  Review the data

``` r
shootings %>%
  select(OccurrenceDate, Murders, Shootings, YearQuarter, Quarter, Year, PerpetratorRace, PerpetratorRaceColor, YearMonth)
```

    ## # A tibble: 28,562 × 9
    ##    OccurrenceDate Murders Shootings YearQuarter Quarter  Year PerpetratorRace
    ##    <date>           <dbl>     <dbl>       <dbl>   <int> <dbl> <chr>          
    ##  1 2022-05-05           1         1       2022.       2  2022 BLACK          
    ##  2 2022-07-04           1         1       2022.       3  2022 UNKNOWN        
    ##  3 2012-05-27           0         1       2012.       2  2012 UNKNOWN        
    ##  4 2019-09-24           0         1       2019.       3  2019 UNKNOWN        
    ##  5 2007-02-25           0         1       2007.       1  2007 BLACK          
    ##  6 2021-07-01           0         1       2021.       3  2021 UNKNOWN        
    ##  7 2021-06-07           1         1       2021.       2  2021 UNKNOWN        
    ##  8 2021-07-22           0         1       2021.       3  2021 UNKNOWN        
    ##  9 2021-05-22           0         1       2021.       2  2021 UNKNOWN        
    ## 10 2021-12-22           1         1       2021.       4  2021 WHITE HISPANIC 
    ## # ℹ 28,552 more rows
    ## # ℹ 2 more variables: PerpetratorRaceColor <chr>, YearMonth <dbl>

8.  Summarize the data by quarter

``` r
ShootingsOverTime <- shootings %>%
 group_by(YearQuarter) %>%
 summarize(Murders = sum(Murders), Shootings = sum(Shootings))
```

### Now, let’s visualize the data and see what stands out

1.  Display the data in a line chart. NOTE: To reduce the number of data
    points, I have summed up the data at the Year-Quarter level.

``` r
ShootingsOverTime %>%
 filter(Shootings > 0) %>%
 ggplot(aes(x = YearQuarter, y = Shootings)) +
 geom_line(aes(color = "Shootings", alpha = 0.5)) +
 geom_point(aes(color = "Shootings")) + 
 geom_line(aes(y = Murders, color = "Murders", alpha = 0.5)) +
 geom_point(aes(y = Murders, color = "Murders")) +
 geom_smooth(method = "lm") +
 scale_color_manual(values = c("red", "blue")) +
 scale_x_continuous(n.breaks = breaksOverTheWholeRange) +
 labs(
     title = paste("Shootings and murders over time"), 
     x = "By Quarter & Year",
     y = "Number of shootings/murders"
 )
```

    ## `geom_smooth()` using formula = 'y ~ x'

<img src="Week-3-NYPD-Shooting-Analysis_files/figure-gfm/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

2.  After reviewing the data, I noticed an interesting trend in
    shootings where Q2 and Q3 typically increase while Q4 and Q1
    decrease. This raises the idea that people in the New York area must
    be outdoors more in the warmer months, leading to the higher number
    of shootings, versus the cooler/colder months of the year.  

- After doing a Google search for the criteria “average monthly
  temperature in New York boroughs from 2006 to 2023”, I came across
  this National Centers for Environmental Information
  [site](https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/city/time-series).
  Within the site I was able to construct an average monthly temperature
  for New York’s Central Park for the desired date range.

- I did this by selecting the following paramters, reviewing the Plot
  feature to ensure it was what I wanted, then took a copy of the
  Download “CSV” button. Here are my parameters:

- Parameter: Average Temperature

- Time Scale: 1-Month

- Month: All Months

- Start Year: 2006

- End Year: 2023

- State: New York

- City: New York (Central Park)

- Next, I read the data in to a variable and reviewed the data. Note:
  This took a couple of tries as I had to figure out how many rows to
  skip to get to the values.

- In addition, I renamed variables to make them easier to use and more
  meaningful.

``` r
noaaAvgTemps <- read.csv(file="https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/city/time-series/USW00094728/tavg/1/0/2006-2023.csv?base_prd=true&begbaseyear=2006&endbaseyear=2023", skip = 4)

noaaAvgTemps <- noaaAvgTemps %>%
  rename(YearMonth = Date, MonthlyAvgTemp = Value)

head(noaaAvgTemps, 10)
```

    ##    YearMonth MonthlyAvgTemp Anomaly
    ## 1     200601           41.1     6.5
    ## 2     200602           36.0    -0.1
    ## 3     200603           43.3    -0.3
    ## 4     200604           55.9     1.7
    ## 5     200605           63.3    -0.4
    ## 6     200606           71.2    -0.9
    ## 7     200607           78.2    -0.1
    ## 8     200608           76.0    -0.4
    ## 9     200609           66.8    -3.1
    ## 10    200610           56.5    -2.5

3.  Bring the NOAA data in with the shooting data. Summarize the
    shootings by month, join the monthly average temperatures, then plot
    the data to see how shootings look compared to temperature. NOTE:
    Because of the large number of months, I chose to narrow the date
    range to the last 8 years of data.

``` r
# MonthlyShootingsWithTemp <- shootings
shootingsByMonth <- shootings %>%
  filter(Year >= 2016) %>%
  group_by(YearMonth, Year) %>%
  summarize(Shootings = sum(Shootings))
```

    ## `summarise()` has grouped output by 'YearMonth'. You can override using the
    ## `.groups` argument.

``` r
shootingsByMonth <- inner_join(shootingsByMonth, noaaAvgTemps, by = "YearMonth")
# shootingsByMonth
sortedTime <- sort(unique(shootingsByMonth$YearMonth), decreasing = FALSE)

# view(shootingsByMonth %>% filter(Year >= 2019))

for(yr in unique(shootingsByMonth$Year)){
  
  print(
  shootingsByMonth %>%
    filter(Year == yr) %>%
    ggplot(aes(factor(reorder(YearMonth, Shootings, sum), sortedTime), y = Shootings, alpha = 0.5)) +
    geom_bar(mapping = aes(x = YearMonth, y = Shootings), stat = "identity") +
    geom_line(mapping = aes(x = YearMonth, y = MonthlyAvgTemp)) +
    scale_x_continuous(
      breaks = scales::pretty_breaks(n = 12)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, .05)),
      sec.axis = sec_axis(
                    ~./1, 
                    name = "Avg Temperature", 
                    breaks = seq(
                        0, 110, by = 10))) +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(
      title = paste(yr, " shootings by all races"),
      x = "Month over month"
    )
  
  
  )
}
```

<img src="Week-3-NYPD-Shooting-Analysis_files/figure-gfm/unnamed-chunk-12-1.png" style="display: block; margin: auto;" /><img src="Week-3-NYPD-Shooting-Analysis_files/figure-gfm/unnamed-chunk-12-2.png" style="display: block; margin: auto;" /><img src="Week-3-NYPD-Shooting-Analysis_files/figure-gfm/unnamed-chunk-12-3.png" style="display: block; margin: auto;" /><img src="Week-3-NYPD-Shooting-Analysis_files/figure-gfm/unnamed-chunk-12-4.png" style="display: block; margin: auto;" /><img src="Week-3-NYPD-Shooting-Analysis_files/figure-gfm/unnamed-chunk-12-5.png" style="display: block; margin: auto;" /><img src="Week-3-NYPD-Shooting-Analysis_files/figure-gfm/unnamed-chunk-12-6.png" style="display: block; margin: auto;" /><img src="Week-3-NYPD-Shooting-Analysis_files/figure-gfm/unnamed-chunk-12-7.png" style="display: block; margin: auto;" /><img src="Week-3-NYPD-Shooting-Analysis_files/figure-gfm/unnamed-chunk-12-8.png" style="display: block; margin: auto;" />

4.  With the average monthly temperature overlayed on the monthly number
    of shootings, we can see that the summertime months are the peak
    time in the number of shootings over each of the years presented.

5.  Let’s see how the data looks for each Borough.

- Summarize the data by Borough
- Then plot by Borough

``` r
ShootingsOverTimeByBorough <- shootings %>%
  group_by(Borough, YearQuarter) %>%
  summarize(Murders = sum(Murders), Shootings = sum(Shootings))
```

    ## `summarise()` has grouped output by 'Borough'. You can override using the
    ## `.groups` argument.

``` r
for(boro in unique(ShootingsOverTimeByBorough$Borough)) {
  # print(boro)
  
  print(
    ShootingsOverTimeByBorough %>%
     filter(Shootings > 0 & Borough == boro) %>%
     ggplot(aes(x = YearQuarter, y = Shootings)) +
     geom_line(aes(color = "Shootings", alpha = 0.5)) +
     geom_point(aes(color = "Shootings")) + 
     geom_line(aes(y = Murders, color = "Murders", alpha = 0.5)) +
     geom_point(aes(y = Murders, color = "Murders")) +
     geom_smooth(method = "lm") +
     scale_color_manual(values = c("red", "blue")) + 
     scale_x_continuous(n.breaks = breaksOverTheWholeRange) +
     theme_minimal() +
     labs(
       title = paste(boro, " shootings and murders over time"), 
       x = "By Quarter & Year",
       y = "Number of shootings/murders")
    )
}
```

    ## `geom_smooth()` using formula = 'y ~ x'

<img src="Week-3-NYPD-Shooting-Analysis_files/figure-gfm/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

    ## `geom_smooth()` using formula = 'y ~ x'

<img src="Week-3-NYPD-Shooting-Analysis_files/figure-gfm/unnamed-chunk-13-2.png" style="display: block; margin: auto;" />

    ## `geom_smooth()` using formula = 'y ~ x'

<img src="Week-3-NYPD-Shooting-Analysis_files/figure-gfm/unnamed-chunk-13-3.png" style="display: block; margin: auto;" />

    ## `geom_smooth()` using formula = 'y ~ x'

<img src="Week-3-NYPD-Shooting-Analysis_files/figure-gfm/unnamed-chunk-13-4.png" style="display: block; margin: auto;" />

    ## `geom_smooth()` using formula = 'y ~ x'

<img src="Week-3-NYPD-Shooting-Analysis_files/figure-gfm/unnamed-chunk-13-5.png" style="display: block; margin: auto;" />

6.  Based on these charts, if I were to visit the New York area, I’d
    visit between the months of October and March, and travel to the
    safest location, which happens to be Staten Island (it has the
    fewest shootings and is trending downward across time).

### Next, let’s visualize the data based on race over the different boroughs

1.  Group up the data based on the Borough, Year, and Perpetrator Race.
    Then, plot the shootings over time.

``` r
ShootingsOverTimeByBoroughandRace <- shootings %>%
  group_by(BoroughYear = paste(Borough, " ", Year), Borough, PerpetratorRace, PerpetratorRaceColor) %>%
  summarize(Murders = sum(Murders), Shootings = sum(Shootings))
```

    ## `summarise()` has grouped output by 'BoroughYear', 'Borough',
    ## 'PerpetratorRace'. You can override using the `.groups` argument.

``` r
TotalShootingOverTimeByBorough <- shootings %>%
  group_by(BoroughYear = paste(Borough, " ", Year)) %>%
  summarize(TotalShootings = sum(Shootings))

ShootingsOverTimeByBoroughandRace <- left_join(ShootingsOverTimeByBoroughandRace, TotalShootingOverTimeByBorough, by = "BoroughYear")

ShootingsOverTimeByBoroughandRace <- ShootingsOverTimeByBoroughandRace %>%
  mutate(PercentOfTotalShootings = (Shootings / TotalShootings)*100)


for(boro in unique(ShootingsOverTimeByBoroughandRace$Borough)){
  print(boro)
  
  boroData <- ShootingsOverTimeByBoroughandRace %>%
      filter(Shootings > 0 & Borough == boro)
  
  sortedTime <- sort(unique(boroData$BoroughYear), decreasing = FALSE)
  
  print(
        ggplot(boroData, aes(factor(reorder(BoroughYear, Shootings, sum), sortedTime), Shootings, fill = PerpetratorRace)) +
          geom_col() +
          geom_text(
            aes(label = after_stat(y), group = BoroughYear),
            stat = 'summary', fun = sum, vjust = -1
          ) +
          theme(axis.text.x = element_text(angle = 90)) +
          labs(
            title = paste(boro, " shootings by perpetrator race"),
            x = "Shootings over the years"
          ) + scale_fill_manual(values = c("#d6d2d2", "#f25757", "#faa381", "#fae588", "#b4ebca", "#83bcff", "#8a84e2"))
          
          
  )
   # scale_color_manual( values = c('red','blue','green'))
  
  # colors but mis-assigns ...
  # 
  
}
```

    ## [1] "BRONX"

<img src="Week-3-NYPD-Shooting-Analysis_files/figure-gfm/unnamed-chunk-14-1.png" style="display: block; margin: auto;" />

    ## [1] "BROOKLYN"

<img src="Week-3-NYPD-Shooting-Analysis_files/figure-gfm/unnamed-chunk-14-2.png" style="display: block; margin: auto;" />

    ## [1] "MANHATTAN"

<img src="Week-3-NYPD-Shooting-Analysis_files/figure-gfm/unnamed-chunk-14-3.png" style="display: block; margin: auto;" />

    ## [1] "QUEENS"

<img src="Week-3-NYPD-Shooting-Analysis_files/figure-gfm/unnamed-chunk-14-4.png" style="display: block; margin: auto;" />

    ## [1] "STATEN ISLAND"

<img src="Week-3-NYPD-Shooting-Analysis_files/figure-gfm/unnamed-chunk-14-5.png" style="display: block; margin: auto;" />

2.  When reviewing the data based on the borough, over time, and broken
    out by the perpetrator race, it concerns me that there is such a
    large portion of shootings in every borough where the perpetrator
    information is either not collected/NA or UNKNOWN. Because of this,
    it makes it very difficult to accurately state which race may be the
    cause of the most shootings. However, if the UNKNOWN category is
    ignored the perpetrator race of BLACK appears to lead the number of
    shootings in every borough in every year of the data set.

3.  Here’s an alternate view of the data over time, excluding the
    UNKNOWN values

``` r
shootings %>%
  group_by(Year, Borough, PerpetratorRace) %>%
  summarize(TotalShootings = sum(Shootings)) %>%
  
  ggplot(aes(Year, TotalShootings)) +
  geom_point(aes(colour = PerpetratorRace,
                 size = TotalShootings,
                 alpha = 0.5)) +
  facet_wrap(~Borough, nrow = 5) +
  theme_bw()
```

    ## `summarise()` has grouped output by 'Year', 'Borough'. You can override using
    ## the `.groups` argument.

<img src="Week-3-NYPD-Shooting-Analysis_files/figure-gfm/unnamed-chunk-15-1.png" style="display: block; margin: auto;" />

### Conclusions

While reviewing the NYPD Shootings data set, I attempted to plot the
data as-is, with very little modification as well as personal bias. I
did, however, group NA and (null) Perpetrator Races in with the UNKNOWN
Perpetrator Race category given that, to me, UNKNOWN is UNKNOWN. In
addition, I did a number of searches looking for why shootings may
increase in New York and I kept coming back to the same thought -
temperature/weather. In doing so, I was able to find a government
agency, National Centers for Environmental Information, which happened
to store average temperatures for multiple New York areas over the span
of our data set. By layering the temperature data in with the shooting
data, it became very apparent that shootings in all of the boroughs of
New York increase during the summertime months.

While I wanted to delve deeper into the perpetrator race, the UNKNOWN
data makes this a very tricky topic. It is not fair to assume that all
of the UNKNOWN perpetrators were black. Nor is it fair to assume they
are White or Hispanic or any other race for that matter. So, the most I
could do is simply report the facts:

- Shootings are related to weather in the New York area. Summertime
  appears to be the most active time for shootings.
- Perpetrators with a race of Black appear to contribute to a large
  number (majority) of the shootings across the 5 boroughs but we cannot
  say definitively that they are THE race who cause the most shootings.
  In order to do that, we’d need to accurately determine the UNKNOWN
  perpetrator race.
- Because of the trends in the data, I’ll be sure to only visit the New
  York area between October and March and it may very well be (sadly)
  Staten Island. I say “sadly” because visiting the Empire State
  building along with the skyscrapers of Manhattan has long been a dream
  of mine.

By sticking to just the facts, I believe I have kept my own personal
bias out of my analysis as well as limited the bias of my source data,
by only using the NYPD Shooting data set and data from a government
agency.
