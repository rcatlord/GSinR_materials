#### https://twitter.com/JustinWolfers/status/844953412986982401


```r
df <- read.csv("US_passports.csv", header = T)
head(df)

library(ggplot2) ; library(hrbrthemes) ; library(ggrepel)
ggplot(df, aes(percent_voted_trump, percent_own_passport)) +
  geom_text_repel(aes(percent_voted_trump, percent_own_passport, label = state)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x="voting Trump (2016)", y="with passport (2010)",
       title="Passports in US states that voted Trump",
       caption="http://www.theexpeditioner.com/2010/02/17/how-many-americans-have-a-passport-2/
       http://uselectionatlas.org") + 
  theme_ipsum_rc()
  ```
