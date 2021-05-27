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

df <- df %>% mutate(Tripmonth = as.factor(month((parse_date_time(df$`Trip Start Timestamp`, "mdYHMS%p")))),
                    Triphour = hour(parse_date_time(df$`Trip Start Timestamp`, "mdYHMS%p"))
    
)

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

longer trips during commuting time. longer trips or more
traffic?

``` r
df %>% group_by(Triphour) %>% summarize(mean_trip_length_in_miles = mean(`Trip Miles`, na.rm = TRUE)) %>%
  ungroup() %>% 
  ggplot(data = ., aes(y= mean_trip_length_in_miles, x = Triphour))+
  geom_col() 
```

![](RMarkdown_figs/markdown-taxi_trip%204-1.png)<!-- -->

only long distance trips in the morning. Hence longer time in the
afternoon should be due to traffic.