Explore\_markdown
================
Felix
27 5 2021

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

``` r
# create 

df <- df %>% mutate(Tripmonth = month(`Trip Start Timestamp`),
                    Tripdate = date(`Trip Start Timestamp`),
                    Triphour = hour(`Trip Start Timestamp`)
    )

# Code from old, not cleaned file on Drive
# df <- df %>% mutate(Tripmonth = as.factor(month((parse_date_time(df$`Trip Start Timestamp`, "mdYHMS%p")))),
#                     Triphour = hour(parse_date_time(df$`Trip Start Timestamp`, "mdYHMS%p"))
#     
# )

# Trips per dayhour for each month
ggplot(df,aes(Triphour, fill = Tripmonth))+
  geom_bar()+
  facet_wrap( ~ Tripmonth)
```

![](RMarkdown_figs/markdown-taxi_trip%202-1.png)<!-- -->

``` r
# mean trip length per dayhour
df %>% group_by(Triphour) %>% summarize(mean_trip_length_in_seconds = mean(`Trip Seconds`, na.rm = TRUE)) %>%
  ungroup() %>% 
  ggplot(data = ., aes(y= mean_trip_length_in_seconds, x = Triphour))+
  geom_col()
```

![](RMarkdown_figs/markdown-taxi_trip%203-1.png)<!-- -->

longer trips during commuting time (mornings and afternoons). longer
trips or more
traffic?

``` r
df %>% group_by(Triphour) %>% summarize(mean_trip_length_in_miles = mean(`Trip Miles`, na.rm = TRUE)) %>%
  ungroup() %>% 
  ggplot(data = ., aes(y= mean_trip_length_in_miles, x = Triphour))+
  geom_col() 
```

![](RMarkdown_figs/markdown-taxi_trip%204-1.png)<!-- -->

only long distance trips in the morning. Hence longer time in the
afternoon should be due to high traffic.

Where does the traffic appear in the
morning?

``` r
Pickup <- df %>% filter(!is.na(`Pickup Centroid Latitude`)) %>% select(`Trip ID`, `Pickup Centroid Latitude`, `Pickup Centroid Longitude`, Triphour, Tripmonth) %>% rename(Lat = `Pickup Centroid Latitude`,
                      Long = `Pickup Centroid Longitude`,
                      ) %>% mutate(Status = "Pickup")

Dropoff <- df %>% filter(!is.na(`Pickup Centroid Latitude`)) %>% select(`Trip ID`, `Dropoff Centroid Latitude`, `Dropoff Centroid Longitude`, Triphour, Tripmonth) %>% rename(Lat = `Dropoff Centroid Latitude`,
                      Long = `Dropoff Centroid Longitude`,
                      ) %>% mutate(Status = "Dropoff")

Pickup <- Pickup %>% filter(Triphour > 4 & Triphour < 9 & Tripmonth == "2") # restricting to morning commuting in Februrary
Dropoff <- Dropoff %>% filter(Triphour > 4 & Triphour < 9 & Tripmonth == "2") # restricting to morning commuting in Februrary
# 
# sampleInd = sample(x = c(FALSE,TRUE), size = nrow(Pickup), prob = c(.3, .7), replace = TRUE) # randomly draw 30 % of the sample
# Pickup = Pickup[!sampleInd,]
# Dropoff = Dropoff[!sampleInd,]

Triplocation <- bind_rows(Pickup, Dropoff)
Triplocation <- Triplocation[!is.na(Triplocation$Lat),]

coordinates(Triplocation) <- ~Long+Lat

#Pickup<-st_as_sf(Pickup)


tmap_mode("plot")
```

    ## tmap mode set to plotting

``` r
tm_shape(Triplocation) +
 tm_dots(col = "Status")
```

    ## Warning: Currect projection of shape Triplocation unknown. Long-lat (WGS84) is
    ## assumed.

![](RMarkdown_figs/markdown-taxi_trip%205-1.png)<!-- --> Dropoff and
Pick up during morning hours in February

couldn t plot in view mode for .md file. Only works with html. This only
looks lame…

Create Prediction data Set

Daily
    periods:

``` r
unique(df$PU_H3)
```

    ##  [1] "872c2119cffffff" "872c21cd8ffffff" "872c21016ffffff" "872c21cd2ffffff"
    ##  [5] "872c21191ffffff" "872c2118bffffff" "872c2118affffff" "872c2119dffffff"
    ##  [9] NA                "872c21010ffffff" "872c21196ffffff" "872c21190ffffff"
    ## [13] "872c21cd9ffffff" "872c21565ffffff" "872c21015ffffff" "872c21ccbffffff"
    ## [17] "872c21002ffffff" "872c21cdeffffff" "872c21014ffffff" "872c21564ffffff"
    ## [21] "872c21cd3ffffff" "872c21525ffffff" "872c21011ffffff" "872c21ccaffffff"
    ## [25] "872c21c99ffffff" "872c21524ffffff" "872c21003ffffff" "872c21c98ffffff"

``` r
# some rides dont have a H3 ID
df <- df[complete.cases(df$PU_H3), ]
df <- df[complete.cases(df$DO_H3), ]
# delete those rides with NA 

# Make tolls NA equla 0
df$Tolls[is.na(df$Tolls)] <- 0

# get prediction data set
df_pred <- df %>% group_by(Tripdate, PU_H3) %>% summarize(number_of_taxirides = n(),
                                                          number_of_taxis = length(unique(`Taxi ID`)),
                                                          number_of_companies = length(unique(Company)),
                                                          avg_trip_seconds = mean(`Trip Seconds`,na.rm = TRUE),
                                                          avg_trip_miles = mean(`Trip Miles`,na.rm = TRUE),
                                                          avg_trip_fare = mean(Fare,na.rm = TRUE),
                                                          avg_tips = mean(Tips),
                                                          avg_tolls = mean(Tolls),
                                                          avg_extras = mean(Extras),
                                                          tip_rate = sum(as.integer(Tips>0))/n(),
                                                          toll_rate = sum(as.integer(Tolls>0))/n(),
                                                          extras_rate = sum(as.integer(Extras>0))/n(),
                                                          cash_rate = sum(as.integer(`Payment Type`== "Cash"))/n(), # one could also seperate different payment types, however most of the payment is done with either cash or credit card.
                                                          total_trip_seconds = sum(`Trip Seconds`,na.rm = TRUE),
                                                          total_trip_miles = sum(`Trip Miles`,na.rm = TRUE),
                                                          dropoff_rate_within_same_H3 = sum(as.integer(PU_H3 == DO_H3, na.rm= TRUE))/n()
                                                          ) %>% ungroup()
```

    ## `summarise()` has grouped output by 'Tripdate'. You can override using the `.groups` argument.

``` r
# Now these numbers of observations, averages and sums stem from the observations of a prediction unit. It would be cheap to use them as features. 
# create averages on a weekday bases 
df_pred <- df_pred %>% mutate(weekday = wday(Tripdate, label = TRUE, abbr = FALSE)) 

df_pred_grouped_averages <- df_pred %>% group_by(weekday, PU_H3) %>% summarise(avg_number_of_taxirides = mean(number_of_taxirides),
                                                          avg_number_of_taxis = mean(number_of_taxis),
                                                          avg_number_of_companies = mean(number_of_companies),
                                                          avg_trip_seconds = mean(avg_trip_seconds),
                                                          avg_trip_miles = mean(avg_trip_miles),
                                                          avg_trip_fare = mean(avg_trip_fare),
                                                          avg_tips = mean(avg_tips),
                                                          avg_tolls = mean(avg_tolls),
                                                          avg_extras = mean(avg_extras),
                                                          avg_tip_rate = mean(tip_rate),
                                                          avg_toll_rate = mean(toll_rate),
                                                          avg_extras_rate = mean(extras_rate),
                                                          avg_cash_rate = mean(cash_rate),
                                                          avg_total_trip_seconds = mean(total_trip_seconds),
                                                          avg_total_trip_miles = mean(total_trip_miles),
                                                          avg_dropoff_rate_within_same_H3 = mean(dropoff_rate_within_same_H3)
                                                          ) %>% ungroup()
```

    ## `summarise()` has grouped output by 'weekday'. You can override using the `.groups` argument.

``` r
df_pred <- left_join(df_pred %>% select(number_of_taxirides, Tripdate, PU_H3, weekday), df_pred_grouped_averages)
```

    ## Joining, by = c("PU_H3", "weekday")

``` r
print(head(df_pred))
```

    ## # A tibble: 6 x 20
    ##   number_of_taxir~ Tripdate   PU_H3 weekday avg_number_of_t~ avg_number_of_t~
    ##              <int> <date>     <chr> <ord>              <dbl>            <dbl>
    ## 1                2 2017-01-01 872c~ Sonntag             2.89             2.89
    ## 2               19 2017-01-01 872c~ Sonntag            17.3             17.2 
    ## 3                3 2017-01-01 872c~ Sonntag             1.37             1.37
    ## 4                4 2017-01-01 872c~ Sonntag             2.67             2.67
    ## 5                5 2017-01-01 872c~ Sonntag             4.27             4.21
    ## 6               16 2017-01-01 872c~ Sonntag            13.1             13.0 
    ## # ... with 14 more variables: avg_number_of_companies <dbl>,
    ## #   avg_trip_seconds <dbl>, avg_trip_miles <dbl>, avg_trip_fare <dbl>,
    ## #   avg_tips <dbl>, avg_tolls <dbl>, avg_extras <dbl>, avg_tip_rate <dbl>,
    ## #   avg_toll_rate <dbl>, avg_extras_rate <dbl>, avg_cash_rate <dbl>,
    ## #   avg_total_trip_seconds <dbl>, avg_total_trip_miles <dbl>,
    ## #   avg_dropoff_rate_within_same_H3 <dbl>
