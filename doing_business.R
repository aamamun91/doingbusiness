
# set directory -----------------------------------------------------------
setwd("~/Data Incubator")

# load packages -----------------------------------------------------------

require(tidyverse)
require(foreign)
require(readr)
require(broom)
require(lubridate)
require(dplyr)
require(ggplot2)
require(ggmap)
require(maps)


# load data ---------------------------------------------------------------

doing_business <- read_csv("doing_business.csv") %>% 
                    gather(key="year", value="value", -c(1:2)) %>% 
                    filter(value!="..") %>% 
                    mutate(year=as.integer(year)) %>% 
                    rename('country'='Country Name',
                            'series'='Series Name') %>% 
                    mutate(value=as.numeric(value))
              

ease_doingbusiness <- doing_business %>% 
                      filter(series
                              =="Ease of doing business index (1=easiest to 185=most difficult)")
                    
total_tax <- doing_business %>% 
               filter(series=="Total tax rate (% of profit)")
                
invest_frnd <- ease_doingbusiness %>% 
                bind_rows(total_tax)

ease_2015 <-  ease_doingbusiness %>% 
              filter(year==2015) %>% 
              rename('score'='value', 'ease'='series') %>% 
              select(country, ease, score)

tax_rate_2015 <- total_tax %>% 
                  filter(year ==2015) %>% 
                  rename('tax_rate'='value', 'tax'='series') %>% 
                  select(country, tax, tax_rate) 

ease_tax_wide <-   ease_2015 %>% 
              left_join(tax_rate_2015, by="country") %>% 
              arrange(score) %>% 
              mutate(breaks=ntile(score, 4), 
              ranking=rank(score, ties.method='first')) %>% 
              mutate(breaks=factor(breaks, levels=c(1,2,3,4),
                            labels=c("top 40 countries", "41-80th", "81-120th", 
                                   "121-158th"))) %>% 
              mutate(country=as.factor(country))
              
View(ease_tax_wide)

ease_2015 <- ease_2015 %>% 
              rename('value'='score', 'series'='ease') %>% 
              arrange(value)
tax_rate_2015 <- tax_rate_2015 %>% 
                  rename('value'='tax_rate', 'series'='tax')
ease_tax_long <- ease_2015 %>% 
                  bind_rows(tax_rate_2015)
View(ease_tax_long)

# plots -------------------------------------------------------------------

ggplot(ease_tax_wide, aes(x=score, y=tax_rate))+
  geom_point(size=1.5, aes(color=country))+
  theme(legend.position = "none")+
  facet_wrap(~breaks, ncol = 2, scales='free')+
  geom_smooth(method=lm, se=FALSE)+
  labs(x="Ease of doing business: easiest to difficult", 
        y="Total tax rate as % of profit")

ggplot(subset(ease_tax_wide, ranking %in% c(1:25)), 
        aes(x=reorder(country, score), y=score, fill=country))+
        geom_bar(stat = "identity")+
        theme(legend.position="none")+
        coord_flip()


# ggplot(subset(ease_tax_wide, ranking %in% c(1:25)), 
  #     aes(x=score, y=tax_rate))+
   #     geom_point(size=1.5, aes(color=country))+
    #  theme(legend.position = "none")+
    # geom_smooth(method=lm, se=FALSE)





              
             




