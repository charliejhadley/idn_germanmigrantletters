library(shiny)
library(leaflet)
library(shinyjs)

shinyUI(
  navbarPage(
    useShinyjs(),
    theme = "animate.min.css",
    
    # includeCSS("www/custom_leaflet_legend.css"),
    "",
    tabPanel(
      "US Choropleth",
      fluidPage(
        useShinyjs(),
        shiny::tags$head(
          shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom_leaflet_legend.css"),
          shiny::tags$link(rel = "stylesheet", type = "text/css", href = "loading.css")
        ),
        sidebarLayout(
          sidebarPanel(
            radioButtons("choropleth_boundaries_to_show",
                         label = "Boundaries to show",
                         choices = list(
                           "States only" = "states",
                           "States and Counties" = "counties",
                           "States and Congressional Districts" = "congressional districts"),
                         selected = "counties"
                         ),
            selectInput(
              "choropleth_how_tally",
              "Tally which?",
              choices = list(
                "Send location" = "sender",
                "Receive location" = "receiver",
                "Both" = "both"
              ),
              selected = "both"
            ),
            uiOutput("choropleth_checkbox_datefilter_UI"),
            uiOutput("choropleth_date_slider_ui")
          ),
          mainPanel(
            div(id = "loading-choropleth",
                fluidPage(
                  h2(class = "animated infinite pulse", "Loading data...")
                  # HTML("<img src=images/cruk-logo.png width='50%'></img>")
                )),
            leafletOutput("us_states_choropleth")
          )
        )
      )
    ),
    tabPanel("Letter Journeys",
             fluidPage(
               # runcodeUI(code = "shinyjs::alert('Hello!')", width = "100%", height = "400px"),
               sidebarLayout(
                 sidebarPanel(
                   uiOutput("journeys_checkbox_datefilter_UI"),
                   uiOutput("journeys_date_slider_ui"),
                   checkboxInput("highlight_selected_families",
                                 "Highlight selected families?",
                                 value = FALSE)
                 ),
                 mainPanel(
                   div(id = "loading-journeys",
                       fluidPage(
                         h2(class = "animated infinite pulse", "Loading data...")
                         # HTML("<img src=images/cruk-logo.png width='50%'></img>")
                       )),
                   leafletOutput("letter_journeys_map")
                 )
               )
               
             )),
    tabPanel("ggplot2 Specific Family Map",
             fluidPage(
               wellPanel(uiOutput("selected_family_date_range_UI")),
               # plotOutput("selected_family_ggplot_map"),
               leafletOutput("selected_family_leaflet_map")
             ))
    # tabPanel("Specific Family History",
    #          fluidPage(
    #            # runcodeUI(code = "shinyjs::alert('Hello!')", width = "100%", height = "400px"),
    #            fluidRow(
    #              column(
    #                wellPanel(
    #                  uiOutput("selected_family_UI"),
    #                  uiOutput("selected_family_checkbox_datefilter_UI"),
    #                  uiOutput("selected_family_date_slider_ui")
    #                ),
    #                uiOutput("selected_family_click_summary"),
    #                width = 4
    #              ),
    #              column(
    #                div(id = "loading-selected-family",
    #                    fluidPage(
    #                      h2(class = "animated infinite pulse", "Loading data...")
    #                      # HTML("<img src=images/cruk-logo.png width='50%'></img>")
    #                    )),
    #                leafletOutput("selected_family_letters_journeys_map"),
    #                width = 8
    #              )
    #            ),
    #            uiOutput("selected_family_letter_viewers_UI")
    #          ))
  ))