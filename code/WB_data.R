library(tidyverse)
library(wbstats)
library(imputeTS)
library(janitor)
#import WB datasets-----
##Poverty headcount @ USD 2.15/ day----
povertyin <- wb_data(indicator = "SI.POV.DDAY",
               country = "countries_only",
               start_date = 2000,
               end_date = 2022)

##access to basic water----
waterin <- wb_data(indicator = "SH.H2O.BASW.ZS",
                 country = "countries_only",
                 start_date = 2000,
                 end_date = 2022)

##country list for income status----
countriesin <- wb_countries()

#interpolate TS data for poverty-----
poverty_complete <- povertyin |> 
  na_kalman()

#calculate no access to water -----
water_no <- waterin |> 
  mutate(basw_noaccess = 100-SH.H2O.BASW.ZS)

#filter GDP and water for only LIC and only 2000 & 2022-----
poverty_lic <- poverty_complete |> left_join(countriesin |> 
                              select(iso3c, income_level_iso3c), join_by(iso3c)) |> 
#  filter(income_level_iso3c =="LIC",
#         date %in% c(2000,2022))
filter(income_level_iso3c =="LIC")


water_lic <- water_no |> left_join(countriesin |> 
                                         select(iso3c, income_level_iso3c), join_by(iso3c)) |> 
#  filter(income_level_iso3c =="LIC",
#         date %in% c(2000,2022))
filter(income_level_iso3c =="LIC")

#join tables for plotting-----
poverty_water <- poverty_lic |> left_join(water_lic  |> 
                                        select(iso3c, date, basw_noaccess), join_by(iso3c, date)) |> 
  janitor::clean_names()
  
#plot!-----
ggplot(data = poverty_water, mapping = aes(x = date)) +
         geom_line(aes(y = basw_noaccess, color = "basw_noaccess"))+
         geom_line(aes(y = si_pov_dday, color= "si+pov_dday")) +
        facet_wrap(~country)
