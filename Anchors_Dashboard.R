library(shiny)
library(sf)
library(leaflet)
library(viridis)
library(rgdal)
library(shinycssloaders)
library(tictoc)
library(tidyverse)
library(readxl)
library(htmltools)
library(shinydashboard)
library(shinythemes)
library(htmlwidgets)
library(DT)

indicator_scale <- tibble(sheet = unique(readRDS('map_data.rds')$sheet))


map_data <- readRDS('map_data.rds') %>% 
  left_join(indicator_scale) %>%
  mutate(value = ifelse(scale_order == 'desc', value*-1, value)) %>% 
  group_by(sheet) %>% 
  #filter(!is.na(value)) %>% 
  mutate(group_min = min(value, na.rm = T),
         group_max = max(value, na.rm = T),
         group_range = group_max-group_min,
         bin = case_when(group_min <= value & value < group_range*.2+group_min ~ paste('1st quintile (Worst)'),
                         group_range*.2+group_min <= value & value < group_range*.4+group_min ~ paste('2nd quintile'),
                         group_range*.4+group_min <= value & value < group_range*.6+group_min ~ paste('3rd quintile'),
                         group_range*.6+group_min <= value & value < group_range*.8+group_min ~ paste('4th quintile'),
                         group_range*.8+group_min <= value ~ '5th quintile (Best)'),
         bin = factor(bin)
         
         # bin_sort = case_when(group_min <= value & value < group_max*.2 ~ 'group1',
         #                      group_max*.2 <= value & value < group_max*.4 ~ 'group2',
         #                      group_max*.4 <= value & value < group_max*.6 ~ 'group3',
         #                      group_max*.6 <= value & value < group_max*.8 ~ 'group4',
         #                      group_max*.8 <= value ~ 'group5')
         ) %>% 
  ungroup() %>%
  mutate(label = case_when(id == '91133' ~ paste0(signif(abs(value), 2), ' per 1,000 pop.'),
                           id == '92924' ~ paste0(signif(abs(value), 2), ' micrograms per cubic metre'),
                           val_type %in% c('percentage', 'rate') ~ paste0(signif(abs(value), 2), '%'),
                           val_type == 'number' ~ paste0(paste0('£',signif(abs(value), 2)))
                           
                           ))
  #arrange(bin_sort) %>% 
  #mutate(bin = fct_reorder(bin, value))


 icb_bounds <- readRDS('icb_bounds.rds') #%>%
#   left_join(map_data,
#             by = c(ICB22CD = 'ICB22ons')) %>% 
#   nest(data = -sheet)

interventions <- read_excel('Anchor interventions evidence collection.xlsx',
                            sheet = 'Interventions_Case Studies',
                            range =  'D2:N109') %>%
  filter(!is.na(`Description of intervention`),
         !`Anchor Intervention category` == 'NEW') %>%
  group_by(`Anchor Intervention category`,
           `Sub Category`,
           Intervention,
           `Delivered by`) %>%
  summarise(intervention_header = paste0('<h4 style = "padding-top: 1.2em;
  padding-bottom: 0.2em;
  border-bottom: 2px solid #f9bf07;">',
                                         `Anchor Intervention category`,
                                         ' - ',
                                         `Sub Category`,
                                         ': ',
                                         Intervention,
                                         '</h4>',
                                         #'<br>',
                                         `Description of intervention`,
                                         '<br>'),
            overview_text = paste0('<h4 style = "padding-top: 1.2em;
  padding-bottom: 0.2em;
  border-bottom: 2px solid #f9bf07;">',
                                   `Anchor Intervention category`,
                                   ' - ',
                                   `Sub Category`,
                                   ': ',
                                   Intervention,
                                   ' (',
                                   `Delivered by`,
                                   ')',
                                   '</h4>'),
            intervention_text = `Description of intervention`)#,
#intervention_text = `Description of intervention`)
 
  
  category_text <- read_excel('Anchor interventions evidence collection.xlsx',
                              sheet = 'Interventions_Case Studies',
                              range =  'D2:N109') %>%
    filter(!is.na(`Description of intervention`),
           !`Anchor Intervention category` == 'NEW') %>%
    #mutate()
     group_by(`Anchor Intervention category`,
            `Sub Category`,
            Intervention) %>%
    mutate(across(everything(), ~ifelse(is.na(.x), "", .x))) %>% 
    #summarise(test = n())
    summarise(intervention_text = paste0(#'<h4 style = "padding-top: 1.2em; padding-bottom: 0.2em;
                                         #border-bottom: 2px solid #f9bf07;">',
                                         #`Anchor Intervention category`,
                                         #' - ',
                                         #`Sub Category`,
                                         #': ',
                                         #Intervention,
                                         #'</h4>',
                                         #'<br>',
                                         `Description of intervention`,
                                         '<br>',
                                         '<b>', 
                                         `Delivered by`,
                                         ' (', `Year of publication`, ') - ',
                                         '</b>',
                                         'For more information click ', '<a href = "', URL, '" target=\"_blank\"', '>', 'here', '</a>',
                                         '<br>')) %>%
    group_by(`Anchor Intervention category`,
             `Sub Category`,
             Intervention) %>% 
    summarise(intervention_text = paste0(#'<h4 style = "padding-top: 1.2em; padding-bottom: 0.2em;
                                         #border-bottom: 2px solid #f9bf07;">',
                                         #Intervention,
                                         #'</h4>',
                                         intervention_text, collapse = '<br>')) %>%
    mutate(intervention_text = paste0('<h4 style = "padding-top: 1.2em; padding-bottom: 0.2em;
                                      border-bottom: 2px solid #f9bf07;">',
                                      Intervention,
                                      '</h4>',
                                      intervention_text)) %>% 
    group_by(`Anchor Intervention category`,
             `Sub Category`,
             Intervention) %>% 
    summarise(intervention_text = paste0(intervention_text, collapse = '<br>')) %>% 
    select('category' = 1,
           'subcategory' = 2,
           Intervention,
           intervention_text)
    #mutate(intervention_text = paste(intervention_text, collapse=" "))
    #nest(intervention_subcat = -`Anchor Intervention category`) %>% 

  category1_text <- category_text |> 
    filter(category == 'Employment practices')|>
    ungroup() |> 
    select(-category) |> 
    group_by(subcategory) |>
    group_nest() |>
    deframe() |>
    map(deframe)
  
  category2_text <- category_text |> 
    filter(category == 'Estates management')|>
    ungroup() |> 
    select(-category) |> 
    group_by(subcategory) |>
    group_nest() |>
    deframe() |>
    map(deframe)
  
  category3_text <- category_text |> 
    filter(category == 'Service models')|>
    ungroup() |> 
    select(-category) |> 
    group_by(subcategory) |>
    group_nest() |>
    deframe() |>
    map(deframe)
  
  category4_text <- category_text |> 
    filter(category == 'Spending policies')|>
    ungroup() |> 
    select(-category) |> 
    group_by(subcategory) |>
    group_nest() |>
    deframe() |>
    map(deframe)
  
  

# cats <- list()
# 
# #for(i in 1:nrow(category_text)){
#   cats <- 
#     c(
#       category_text$`Anchor Intervention category`,
#       purrr::pmap(category_text, \(`Sub Category`, intervention_text) {
#         tabPanel(`Sub Category`, intervention_text)
#       }))
      #purrr::map(category_text$intervention_subcat[[i]]$`Sub Category`, tabPanel))
#}
  cats <- c(
    purrr::pmap(category_text, \(category, subcategory, Intervention, intervention_text) {
      category
      tabPanel(subcategory, HTML(intervention_text))
    })
  )

# for(i in nrow(interventions)){
#   for(j in nrow(interventions$intervention_subcat)){
#   paste(interventions$`Anchor Intervention category`[i],
#         interventions$intervention_subcat)
#   }
# }

#tmp <- interventions[1,]

# for(j in nrow(tmp$intervention_subcat)){
# paste(tmp$`Anchor Intervention category`,
#       tmp$)
# 
# }

#interventions[1,]$intervention_subcat[[1]]$intervention_text

  
ui <- fluidPage(
  theme = shinytheme('yeti'),
  tagList(
  navbarPage('Anchors Dashboard', id = 'navbar',
             tags$script(HTML(
             "var header = $('.navbar > .container-fluid');
header.append('<div style=\"float:right\"><a href=\"https://www.strategyunitwm.nhs.uk/\", target=\"_blank\"><img src=\"https://connect.strategyunitwm.nhs.uk/content/23295017-5f95-4e9f-b5d8-7ce25c84afe8/logo_yellow.svg\"style=\"float:right;width:50px;height:50px;padding-top:0px;\"> </div>');
    console.log(header)
             ")),
             selected = 'introduction',
             tabPanel(value = 'introduction', 'Introduction',
                      HTML(
                        '<h2> Welcome to the interactive map for anchor institutions and health inequalities</h2> 
                        <h3 style = "padding-top: 1.2em; padding-bottom: 0.2em;
                                      border-bottom: 2px solid #f9bf07;"> How to use the map </h3>
                        To view the map click on the \'Map\' tab at the top of this page. The map shows each Integrated Commissioning Board (ICB) in England with the current performance level for the selected indicator. ICBs are shaded based on the indictor value and the quintile into which it this places it. Hovering over an ICB with the mouse will show the name of the ICB and its indicator value. 
                        Different indictors are available to pick from the drop down under ‘Select an indictor to view on map’. Once you select an indicator the map will update, as will the information below it describing the indictor, the period the indictor relates to and its source. More information on the source data can also be found by clicking where it says ‘For more information click here’.
                        You can zoom in or out of the map as required.
                        
                        <h3 style = "padding-top: 1.2em; padding-bottom: 0.2em;
                                      border-bottom: 2px solid #f9bf07;"> How to access the data behind the map </h3>
                        The ‘Data Explorer’ tab at the top of the page will take you to the ICB level data by behind the map.
                        
                        <h3 style = "padding-top: 1.2em; padding-bottom: 0.2em;
                                      border-bottom: 2px solid #f9bf07;"> Evidence base </h3>
                        The aim of the map is to not just help organisations within ICBs understand current levels of inequality, but to also provide evidence-based insight which can be utilised to help identify, prioritise and plan effective interventions. The ‘Evidence Base tab’ box lists the intervention categories (e.g., Employment practices) with their sub-categories (e.g., Being a good employer, Building the future workforce and Widening workforce participation). Then, within each sub-category individual evidenced interventions are shown. Clicking on these will take you straight to the detailed information on the nature of the intervention alongside where it took place and the rationale behind it.
                        ')),
             
             tabPanel(value = 'mapsPanel', 'Map', 
                      fluidRow(
                      column(
                      dashboardBody(
                        tags$style(
                          type = "text/css", "#map {height: calc(100vh - 80px) !important;}",
                                   "#SelectInterventionsText{height: calc(100vh - 80px) !important;}",
                          HTML(".leaflet-left .leaflet-control{
                            visibility: hidden;
                          }")
                          ),
               
                        
                        #uiOutput("references")),
                        leafletOutput("map"#, width = "100%", height = "100%"
                        ) %>%
                          withSpinner(color="#0dc5c1"),
                        )
                      , width = 9
                      ),
                      #uiOutput('InterventionsOverview', style = "overflow-y: scroll"), 
                      absolutePanel(
                        id = "controls",
                        class = "panel panel-default",
                        draggable = F,
                        cursor = 'default',
                        top = 66,
                        left = "auto",
                        right = 0,
                        bottom = "auto",
                        width = 330,
                        height = "auto",
                        h3("Select an indicator to view on map"),
                        selectInput('indicatorDropdown', '',
                                    choices = unique(map_data$`Indicator Name`)),
                        uiOutput('indicator_info'),
                        style = "background-color: white;
                          padding: 0 20px 20px 20px;
                          cursor: move;
                          opacity: 1;"),
                      width = 4)
                      ),
                        
                                    #)
                
             tabPanel(value = 'interventionsPanel', 'Evidence Base',
                      h3('Employment practices'),
                      category1_text |>
                        imap(\(.x1, .i1) {
                          div(
                            
                            tags$ul(tags$li(h4(.i1)),
                                    tags$ul(
                                      imap(.x1, \(.x2, .i2) {
                                        name <- paste0(.i1, "-", .i2) |>
                                          stringr::str_replace_all(" ", "_") |>
                                          stringr::str_to_lower()
                                        tags$li(
                                          actionLink(name, .i2)
                                        )
                                      })
                                    )
                            )
                          )
                        }),
                      h3('Estates management'),
                      category2_text |>
                        imap(\(.x1, .i1) {
                          div(
                            
                            tags$ul(tags$li(h4(.i1)),
                                    tags$ul(
                                      imap(.x1, \(.x2, .i2) {
                                        name <- paste0(.i1, "-", .i2) |>
                                          stringr::str_replace_all(" ", "_") |>
                                          stringr::str_to_lower()
                                        tags$li(
                                          actionLink(name, .i2)
                                        )
                                      })
                                    )
                            )
                          )
                        }),
                      h3('Service models'),
                      category3_text |>
                        imap(\(.x1, .i1) {
                          div(
                            
                            tags$ul(tags$li(h4(.i1)),
                                    tags$ul(
                                      imap(.x1, \(.x2, .i2) {
                                        name <- paste0(.i1, "-", .i2) |>
                                          stringr::str_replace_all(" ", "_") |>
                                          stringr::str_to_lower()
                                        tags$li(
                                          actionLink(name, .i2)
                                        )
                                      })
                                    )
                            )
                          )
                        }),
                      h3('Spending policies'),
                      category4_text |>
                        imap(\(.x1, .i1) {
                          div(
                            
                            tags$ul(tags$li(h4(.i1)),
                                    tags$ul(
                                      imap(.x1, \(.x2, .i2) {
                                        name <- paste0(.i1, "-", .i2) |>
                                          stringr::str_replace_all(" ", "_") |>
                                          stringr::str_to_lower()
                                        tags$li(
                                          actionLink(name, .i2)
                                        )
                                      })
                                    )
                            )
                          )
                        })
                      #purrr::lift_dl(navlistPanel, id = 'test')(cats)#,
                      #uiOutput('interventionsHeader'),
                      #uiOutput('interventionsText')
                      ),
             
             tabPanel(value = 'data_tab', 'Data Explorer',  DT::dataTableOutput('data_explorer'))
             )
  )
)


# output$appdx_houses <- renderUI({
#   lapply(1:nrow(interventions), function(i) {
#     renderUI(HTML(output_vals$appdxHouses[[i]]))
#   })

server <- function(input, output) {
  
  # output$InterventionsOverview <- renderUI(HTML(paste0('<h2 style = "
  # padding-bottom: 0.4em"> Evidence base for indicator rationale and improvement', paste0(interventions$overview_text, collapse = ''))))
  #output$interventionsHeader <- renderUI(HTML(paste0(interventions$intervention_header)))
  
  #output$interventionsText <- renderUI(HTML(paste0(interventions$intervention_text[1])))
  
  category1_text |>
    imap(\(.x1, .i1) {
      imap(.x1, \(.x2, .i2) {
        name <- paste0(.i1, "-", .i2) |>
          stringr::str_replace_all(" ", "_") |>
          stringr::str_to_lower()
        header <- HTML(str_extract(.x2, "<h4(.|\n)*/h4>"))
        text <- HTML(str_split(.x2, "(?<=/h4>)", simplify = T) [,2])
        observeEvent(input[[name]], {
          modal <- modalDialog(
            title = header,
            HTML(text),
            easyClose = T,
            footer = tagList(
              modalButton("Close")
            )
          )
          showModal(modal)
        })
      })
    })
  
  category2_text |>
    imap(\(.x1, .i1) {
      imap(.x1, \(.x2, .i2) {
        name <- paste0(.i1, "-", .i2) |>
          stringr::str_replace_all(" ", "_") |>
          stringr::str_to_lower()
        header <- HTML(str_extract(.x2, "<h4(.|\n)*/h4>"))
        text <- HTML(str_split(.x2, "(?<=/h4>)", simplify = T) [,2])
        observeEvent(input[[name]], {
          modal <- modalDialog(
            title = header,
            HTML(text),
            easyClose = T,
            footer = tagList(
              modalButton("Close")
            )
          )
          showModal(modal)
        })
      })
    })
  
  category3_text |>
    imap(\(.x1, .i1) {
      imap(.x1, \(.x2, .i2) {
        name <- paste0(.i1, "-", .i2) |>
          stringr::str_replace_all(" ", "_") |>
          stringr::str_to_lower()
        header <- HTML(str_extract(.x2, "<h4(.|\n)*/h4>"))
        text <- HTML(str_split(.x2, "(?<=/h4>)", simplify = T) [,2])
        observeEvent(input[[name]], {
          modal <- modalDialog(
            title = header,
            HTML(text),
            easyClose = T,
            footer = tagList(
              modalButton("Close")
            )
          )
          showModal(modal)
        })
      })
    })
  
  category4_text |>
    imap(\(.x1, .i1) {
      imap(.x1, \(.x2, .i2) {
        name <- paste0(.i1, "-", .i2) |>
          stringr::str_replace_all(" ", "_") |>
          stringr::str_to_lower()
        header <- HTML(str_extract(.x2, "<h4(.|\n)*/h4>"))
        text <- HTML(str_split(.x2, "(?<=/h4>)", simplify = T) [,2])
        observeEvent(input[[name]], {
          modal <- modalDialog(
            title = header,
            HTML(text),
            easyClose = T,
            footer = tagList(
              modalButton("Close")
            )
          )
          showModal(modal)
        })
      })
    })
  
  output$data_explorer <- renderDataTable(
    datatable(readRDS('map_data.rds') %>%
                ungroup() %>% 
                select('ICB' = ICB22NM, `Indicator Name`, value) %>% 
                mutate(value = round(value, digits = 2)) %>% 
                pivot_wider(names_from = `Indicator Name`, 
                            values_from = value),
              options = list(dom = 'ft',
                             'pageLength' = 15))
  )
  
  
  observeEvent(input$indicatorDropdown, {
    
    indicator_info_df <- (map_data %>% 
                          filter(`Indicator Name` == input$indicatorDropdown) %>%
                            mutate(Period = paste0(' (data from ', Period, ')'),
                                   Source = paste0('<a href = "', Source, '" target=\"_blank\"', '">', 'here', '</a>')))[1,]
    
    output$indicator_info <- renderText(HTML(
      paste0('Notes: ', indicator_info_df$description[1], 
      indicator_info_df$Period[1], '<br>',
      'For more information click ', indicator_info_df$Source[1], '<br>',
      'Copyright: ', indicator_info_df$Copyright[1])
    ))
    
    tmp_map_data <- icb_bounds %>%
                       left_join(map_data %>% select(-ICB22NM) %>%  filter(`Indicator Name` == input$indicatorDropdown),
                                 by = c(ICB22CD = 'ICB22ons')) 
                       
                       
    
    pal_bin <- colorFactor(
      palette = "viridis",
      domain = tmp_map_data$bin,
      alpha = 0.6)
    
    #m <- leafletProxy('map')
    
    
    output$map <- renderLeaflet(
      leaflet(tmp_map_data) %>%
        addProviderTiles("CartoDB.Positron") %>%
        fitBounds(min(icb_bounds$LONG), min(icb_bounds$LAT), max(icb_bounds$LONG), max(icb_bounds$LAT)) %>% 
        addPolygons(fillColor = "#808080",
                    color = 'white') %>% 
        addPolygons(fillColor = #'pink',
                      #viridis::viridis_pal(),
                      ~pal_bin(bin),
                    fillOpacity = 1, opacity = 1,
                    color = 'white',
                    weight = 1,
                    label = ~paste0(ICB22NM, ', ', label),
                    layerId = ~ICB22CD) %>%
        addLegend(pal = pal_bin, values = ~bin, position = 'bottomright', title = F, opacity = 1) %>%
        onRender(
          "function(el, x) {
          L.control.zoom({
            position:'bottomright'
          }).addTo(this);
        }")
      )
    
    })
  
  
}

shinyApp(ui = ui, server = server)  


