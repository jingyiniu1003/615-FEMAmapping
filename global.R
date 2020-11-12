library(dplyr)
library(rgdal)
library(sp)
library(leaflet)
library(leaflet.extras)

# 读取数据
smas <- read.csv('data/summaries.csv', stringsAsFactors = FALSE) 
detail <- read.csv('data/detail.csv', stringsAsFactors = FALSE) 

# 筛选只需要用到的数据, 缩减字段和数据量

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

# 关联数据 - 画图用
plot.data <- smas %>%
    left_join(detail, by = c('fipsStateCode', 'fipsCountyCode', 'fyDeclared', 'disasterNumber'))

# 准备shiny筛选的数据
years <- sort(unique(plot.data$fyDeclared))
# declarationTitles <- sort(unique(plot.data$declarationTitle))
# designatedAreas <- sort(unique(plot.data$designatedArea))

# 美国地图数据准备
us_map <- readOGR('data/gz_2010_us_050_00_20m/gz_2010_us_050_00_20m.shp')
us_map@data <- us_map@data %>% 
    mutate(
        fipsStateCode = as.numeric(STATE),
        fipsCountyCode = as.numeric(COUNTY)
    )

# smas %>% select(fipsStateCode) %>% filter(!(fipsStateCode %in% us_map@data$fipsStateCode)) %>% unique()
# smas %>% select(fipsCountyCode) %>% filter(!(fipsCountyCode %in% us_map@data$fipsCountyCode)) %>% unique()
