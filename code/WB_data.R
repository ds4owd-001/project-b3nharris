library(tidyverse)
library(wbstats)
library(imputeTS)
library(janitor)
#import WB datasets-----
##Poverty headcount @ USD 2.15/ day----
povertyLICin <- wb_data(indicator = "SI.POV.DDAY",
                     country = "countries_only",
                     start_date = 2000,
                     end_date = 2022)

##Poverty headcount @ USD 2.15/ day----
povertyLMICin <- wb_data(indicator = "SI.POV.LMIC",
                     country = "countries_only",
                     start_date = 2000,
                     end_date = 2022)

##access to basic water----
waterin <- wb_data(indicator = "SH.H2O.BASW.ZS",
                   country = "countries_only",
                   start_date = 2000,
                   end_date = 2022)

##access to basic sanitation----
sanin <- wb_data(indicator = "SH.STA.BASS.ZS",
                   country = "countries_only",
                   start_date = 2000,
                   end_date = 2022)

##country list for income status----
countriesin <- wb_countries()

#interpolate TS data for poverty as some years have no values-----
povertyLICcomplete <- povertyLICin |> 
  na_kalman()
povertyLMICcomplete <- povertyLMICin |>
  na_kalman()

#calculate no access to water and sanitation -----
water_no <- waterin |> 
  mutate(basw_noaccess = 100-SH.H2O.BASW.ZS)
san_no <- sanin |> 
  mutate(bass_noaccess = 100-SH.STA.BASS.ZS)

#join tables for plotting-----
poverty_water_san <- povertyLICcomplete |>
  left_join(povertyLMICcomplete  |> 
              select(iso3c, date, SI.POV.LMIC), join_by(iso3c, date)) |> 
  left_join(water_no  |> 
              select(iso3c, date, basw_noaccess), join_by(iso3c, date)) |> 
  left_join(san_no  |> 
              select(iso3c, date, bass_noaccess), join_by(iso3c, date)) |>
  left_join(countriesin |> 
              select(iso3c, income_level_iso3c), join_by(iso3c))|>
  janitor::clean_names() |>
  select(-c(unit, obs_status, footnote, last_updated))
