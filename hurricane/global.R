library(dplyr)
library(rgdal)
library(sp)
library(leaflet)
library(leaflet.extras)

# Read data
smas <- read.csv('data/summaries.csv', stringsAsFactors = FALSE) 
detail <- read.csv('data/detail.csv', stringsAsFactors = FALSE) 

# choose data we need, to reduce amount of data and string 

smas <- smas %>% 
    select(
        state_name = state, fipsStateCode, fipsCountyCode, fyDeclared, 
        declarationTitle, disasterNumber, designatedArea
    ) %>% 
    filter(!(fipsCountyCode == 0)) %>% 
    filter(!(fipsStateCode %in% c(78, 60)))
detail <- detail %>% 
    mutate(fyDeclared = as.numeric(substr(declarationDate, 1, 4))) %>% 
    select(fyDeclared, disasterNumber, 
           fipsStateCode = stateNumberCode, 
           fipsCountyCode = countyCode, federalShareObligated, projectAmount) %>% 
    group_by(fyDeclared, disasterNumber, fipsStateCode, fipsCountyCode) %>% 
    summarise(
        federalShareObligated = sum(federalShareObligated, na.rm = TRUE), 
        projectAmount = sum(projectAmount, na.rm = TRUE)
    )

# connect data for map 
plot.data <- smas %>%
    left_join(detail, by = c('fipsStateCode', 'fipsCountyCode', 'fyDeclared', 'disasterNumber'))

# prepare data for shiny 
years <- sort(unique(plot.data$fyDeclared))
# declarationTitles <- sort(unique(plot.data$declarationTitle))
# designatedAreas <- sort(unique(plot.data$designatedArea))

# Prepare for America map data
us_map <- readOGR('data/gz_2010_us_050_00_5m/gz_2010_us_050_00_5m.shp')
us_map@data <- us_map@data %>% 
    mutate(
        fipsStateCode = as.numeric(STATE),
        fipsCountyCode = as.numeric(COUNTY)
    )

# smas %>% select(fipsStateCode) %>% filter(!(fipsStateCode %in% us_map@data$fipsStateCode)) %>% unique()
# smas %>% select(fipsCountyCode) %>% filter(!(fipsCountyCode %in% us_map@data$fipsCountyCode)) %>% unique()
