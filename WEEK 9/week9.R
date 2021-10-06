library(haven)
library(tidyverse)
library(dplyr)
library(pool)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
library(tidycensus)
library(sf)
install.packages("leaflet")
library(leaflet)
source("api-keys.R")
census_api_key(census_api_key, install = TRUE)
U_S_Religion_Census_Religious_Congregations_and_Membership_Study_2010_County_File_ <- read_sav("U.S. Religion Census Religious Congregations and Membership Study, 2010 (County File).SAV")
View(U_S_Religion_Census_Religious_Congregations_and_Membership_Study_2010_County_File_)

U_S_Religion_Census_Religious_Congregations_and_Membership_Study_2010_County_File_  %>% rename(region = FIPS, value = TOTRATE)  %>% county_choropleth()
U_S_Religion_Census_Religious_Congregations_and_Membership_Study_2010_County_File_  %>% rename(region = FIPS, value = EVANRATE)  %>% county_choropleth()

U_S_Religion_Census_Religious_Congregations_and_Membership_Study_2010_County_File_  %>% rename(region = FIPS, value = OTHRATE)  %>% county_choropleth(state_zoom = "new jersey")

nj_rental <-
  get_acs(
    geography = "tract",
    variables = c(medrental = "B25064_001"),
    state = "NJ",
    geometry = TRUE
  )

pal2 <- colorQuantile("Greens", domain = nj_rental$estimate, n = 9)

nj_rental %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet() %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ paste0("$",format(estimate, big.mark = ",")),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.5,
              color = ~ pal2(estimate)) %>%
  addLegend("bottomright", 
            pal = pal2, 
            values = ~ estimate,
            title = "Median Rent",
            opacity = 1)