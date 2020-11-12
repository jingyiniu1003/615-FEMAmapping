library(shiny)
library(leaflet)
library(leaflet.extras)

source('global.R', encoding = 'UTF-8')

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Hurricane"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput('year', 'Choice Year: ', years), 
            uiOutput('hurricane'), 
            uiOutput('hurricane_sta')
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("usMapPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    values <- reactiveValues(
        year_data         = NULL, # 依据年份筛选的数据
        declarationTitleS = NULL, # 飓风筛选器数据, 依据年份变化
        year_state        = NULL, # 依据年份筛选的数据 得到的州数据
        hurricane_state   = NULL  # 州选择器, 依据年份和飓风筛选而来
    )
    
    observeEvent(input$year, {
        values$year_data <- plot.data %>% filter(fyDeclared == input$year) 
        values$declarationTitleS <- sort(unique(values$year_data$declarationTitle)) 
        output$hurricane <- renderUI({
            radioButtons(
                inputId = "declarationTitle", 
                label = "Choice Hurricane: ", 
                c('ALL Hurricane', values$declarationTitleS), 
                'ALL Hurricane', inline = FALSE
            )
        })
    })
    
    observeEvent(input$declarationTitle, {
        if (input$declarationTitle != 'ALL Hurricane') {
            values$year_state <- values$year_data %>% 
                filter(declarationTitle == input$declarationTitle) %>% 
                select(state_name, fipsStateCode) %>% unique()
            
            if (length(unique(values$year_state$state_name)) > 1) {
                values$hurricane_state <- sort(unique(values$year_state$state_name))
                output$hurricane_sta <- renderUI({
                    radioButtons(
                        inputId = "STATE_NAME",
                        label = "Choice State Name: ",
                        c('ALL STA', values$hurricane_state), 
                        'ALL STA', inline = TRUE
                    )
                })
            } else {
                values$hurricane_state <- c(sort(unique(values$year_state$state_name)))
                output$hurricane_sta <- renderUI({
                    radioButtons(
                        inputId = "STATE_NAME",
                        label = "State Name: ",
                        values$hurricane_state, inline = TRUE
                    )
                })
            }
        }
    })
    
    observeEvent(input$declarationTitle, {
        # All County 地图
        output$usMapPlot <- renderLeaflet({
            if (input$declarationTitle == 'ALL Hurricane') {
                map <- us_map
                # 为区分州准备数据
                map_sta_boder <- aggregate(
                    map[, "STATE"], by = list(ID = map@data$STATE), FUN = unique, dissolve = T
                )
                # 实际数据
                map_data <- values$year_data %>%
                    group_by(state_name, fipsStateCode, fipsCountyCode, designatedArea) %>%
                    summarise(
                        federalShareObligated = sum(federalShareObligated, na.rm = TRUE),
                        projectAmount = sum(projectAmount, na.rm = TRUE)
                    )
                
                map@data <- map@data %>%
                    left_join(map_data, by = c('fipsStateCode', 'fipsCountyCode')) %>%
                    mutate(
                        TYPE = ifelse(is.na(designatedArea), 'No Hurricane', ifelse(
                            is.na(projectAmount) | projectAmount <= 0, 'Hurricane No PA', 'Hurricane PA'))
                    )
                
                map@data$text <- sprintf(
                    "<strong>County: %s</strong><br/>TYPE: %s<br/>",
                    map@data$NAME, map@data$TYPE
                ) %>% lapply(htmltools::HTML)
                
                pal <- colorFactor(
                    palette = c('#998ec3', '#f1a340', '#f7f7f7'),
                    domain = map@data$TYPE
                )
                leaflet(map) %>%
                    setView(-96, 37.8, 4) %>%
                    setMapWidgetStyle(list(background = "#f7f7f7")) %>%
                    addPolygons(
                        color = 'Grey',
                        opacity = 0.2, smoothFactor = 0.3,
                        fillOpacity = 1,
                        weight = 1,
                        fillColor = ~ pal(map@data$TYPE),
                        label = map@data$text
                    ) %>%
                    addPolylines(data = map_sta_boder, color = "DarkGrey", opacity = 1, weight = 0.8)
            }
        })
    })
    
    observeEvent(input$STATE_NAME, {
        # 特定飓风地图
        output$usMapPlot <- renderLeaflet({
            if (input$STATE_NAME == 'ALL STA') {
                map <- us_map[us_map@data$fipsStateCode %in% values$year_state$fipsStateCode, ]
                
                map_data <- values$year_data %>% 
                    filter(declarationTitle == input$declarationTitle)
            } else {
                map <- us_map[us_map@data$fipsStateCode == values$year_state[
                    values$year_state$state_name == input$STATE_NAME, ]$fipsStateCode, 
                ]
                map_data <- values$year_data %>%
                    filter(declarationTitle == input$declarationTitle) %>% 
                    filter(fipsStateCode == values$year_state[
                        values$year_state$state_name == input$STATE_NAME, ]$fipsStateCode
                    )
            }
            map_sta_boder <- aggregate(
                map[, "STATE"], by = list(ID = map@data$STATE), FUN = unique, dissolve = T
            )
            
            map@data <- map@data %>%
                left_join(map_data, by = c('fipsStateCode', 'fipsCountyCode')) 
            
            # 地图显示信息
            
            map@data$text <- sprintf(
                "<strong>County: %s</strong><br/>federalShareObligated: %s<br/>projectAmount: %s",
                map@data$NAME, map@data$federalShareObligated, map@data$projectAmount
            ) %>% lapply(htmltools::HTML)
            
            colorpal <- colorBin("Blues", map@data$projectAmount, 
                                 na.color = "#f7f7f7", 6, pretty = TRUE)
            
            leaflet(map) %>%
                setMapWidgetStyle(list(background = "#f7f7f7")) %>%
                addPolygons(
                    color = "grey", opacity = 0.2, smoothFactor = 0.3, fillOpacity = 1, weight = 1,
                    fillColor = ~ colorpal(map@data$projectAmount),
                    label = map@data$text
                ) %>%
                addPolylines(data = map_sta_boder, color = "DarkGrey", opacity = 1, weight = 0.8) %>%
                addLegend(
                    pal = colorpal, values = map@data$projectAmount, opacity = 0.9, na.label = "No Hurricane"
                )  
        })
    })
    
    observeEvent(input$declarationTitle, {
        if (input$declarationTitle == 'ALL Hurricane') {
            output$hurricane_sta <- NULL
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
