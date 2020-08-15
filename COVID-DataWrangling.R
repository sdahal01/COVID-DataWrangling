library(tidyverse)

death <- read_csv("C:/Users/samprayash/OneDrive/Desktop/deaths.csv")
death <- death %>% gather(key="Date", value="Deaths", -`Province/State`, -`Country/Region`, -`Lat`, -`Long`) 
death <- death %>% select(`Country` = `Country/Region`, `Date`, `Deaths`) 
death <- death %>% filter(Country != "US") 
death <- death  %>% mutate(Country = replace(Country, Country == "Republic of Korea", "South Korea"))

confirm <- read_csv("C:/Users/samprayash/OneDrive/Desktop/confirmed.csv")
confirm <- confirm %>% gather(key="Date", value="Confirmed", -`Province/State`, -`Country/Region`, -Lat, -Long) 
 confirm = subset(confirm, select = -c(Lat, Long))
 confirm = subset(confirm, select = -c(1))
 confirm <- confirm %>% select(`Country` = `Country/Region`, `Date`, `Confirmed`)
 confirm <- confirm %>% mutate(Country = replace(Country, Country == "Republic of Korea", "South Korea"))
 confirm <- confirm %>% filter(Country != "US")

 beds <- read_csv("C:/Users/samprayash/OneDrive/Desktop/hospitalbeds.csv")
 beds <- beds %>% filter(Year == 2015) 
  beds <- beds %>% select(`Country`, `Beds` = `Hospital beds (per 10 000 population)`)
  beds <- beds %>% mutate(Country = replace(Country, Country == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom"))
  beds <- beds %>% mutate(Country = replace(Country, Country == "Iran (Islamic Republic of)", "Iran"))
  beds <- beds %>% mutate(Country = replace(Country, Country == "Republic of Korea", "South Korea"))
 
  Demo <- read_csv("C:/Users/samprayash/OneDrive/Desktop/demographics.csv")      
  demo <- demo%>% select(`Country Name`, `Series Code`, `YR2015`) 
   demo <- demo %>% spread(key = `Series Code`, value = `YR2015`) 
   demo <- demo %>% replace(is.na(.), 0) 
   demo <- demo %>% mutate(`MORT RATE` = reduce(select(., starts_with("SP.DYN.AMRT")), `+`)) 
   demo <- demo %>% mutate(`POP 0TO14` = reduce(select(., starts_with("SP.POP.0014")), `+`))
   demo <- demo %>% mutate(`POP 15TO65` = reduce(select(., starts_with("SP.POP.1564")), `+`)) 
   demo <- demo %>% mutate(`POP 65UP` = reduce(select(., starts_with("SP.POP.65UP")), `+`)) 
  Demo <- demo %>% mutate(`POP 80UP` = reduce(select(., starts_with("SP.POP.80UP")), `+`)) 
   demo <- demo %>% select(`Country` = `Country Name`,`LIFE EXT` = `SP.DYN.LE00.IN`, `MORT RATE`,  `POP TOTAL` = `SP.POP.TOTL`, `POP URBAN` = `SP.URB.TOTL`, `POP 0TO14`, `POP 15TO65`, `POP 65UP`, `POP 80UP`)
  demo <- demo  %>% mutate(Country = replace(Country, Country == "Korea, Dem. Peopleâ???Ts Rep.", "South Korea"))
   demo <- demo %>% mutate(Country = replace(Country, Country == "Korea, Rep.", "South Korea"))
   demo <- demo %>% mutate(Country = replace(Country, Country == "Iran, Islamic Rep.", "Iran"))
   demo <- demo %>% filter(Country != "United States")
   #code to join dataset to one table
   confirm_death <- confirm %>% inner_join(death)
   demo_bed <- demo %>% inner_join(beds)
   main <- demo_bed %>% inner_join(confirm_death)
   
   #Confirmed~Deaths
   good <- lm(data = main, Confirmed~Deaths)
   cf <- coef(good)
    ggplot(data = main)+geom_point(mapping = aes(x = Deaths, y = Confirmed))+geom_abline(slope = cf[2], intercept = cf[1])
   
   #Beds~Deaths
   hos_beds <- lm(data = main, Beds~Deaths)
    cf_beds <- coef(hos_beds)
   ggplot(data = main)+geom_point(mapping = aes(x = Deaths, y = Beds))+geom_abline(slope = cf_beds[2], intercept = cf_beds[1])
   
   #MORT RATE~Deaths
   mortality <- lm(data = main, main$`MORT RATE`~main$Deaths)
    cf_mor <- coef(mortality)
   ggplot(data = main)+geom_point(mapping = aes(x = Deaths, y = main$`MORT RATE`))+geom_abline(slope = cf_mor[2], intercept = cf_mor[1])
   
   #POPTOTAL~Deaths 
   pop <- lm(data = main, main$`POP TOTAL`~Deaths)
    cf_pop <- coef(pop)
    ggplot(data = main)+geom_point(mapping = aes(x = Deaths, y = main$`POP TOTAL`))+geom_abline(slope = cf_pop[2], intercept = cf_pop[1])
   
   #POP80UP~Deaths
   UP80<- lm(data = main, main$`POP 80UP`~Deaths)
    cf_80 <- coef(UP80)
    ggplot(data = main)+geom_point(mapping = aes(x = Deaths, y = main$`POP 80UP`))+geom_abline(slope = cf_80[2], intercept = cf_80[1])
   
   
   #LIFE EXT~Deaths
   life <- lm(data = main, main$`LIFE EXT`~Deaths)
    cf_life <- coef(life)
    ggplot(data = main)+geom_point(mapping = aes(x = Deaths, y = main$`LIFE EXT`))+geom_abline(slope = cf_life[2], intercept = cf_life[1])
   