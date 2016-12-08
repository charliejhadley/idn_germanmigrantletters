library(shiny)
library(leaflet)
library(shinyjs)

loadingCSS <- "
#loading-content {
position: absolute;
background: #FFFFFF;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #000000;
}"

customLegend <- "
.leaflet .legend i{
border-radius: 50%;
width: 10px;
height: 10px;
margin-top: 4px;
}"

shinyUI(navbarPage(
  theme = "animate.min.css",
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "custom_leaflet_legend.css")
  # ),
  # includeCSS("www/custom_leaflet_legend.css"),
  inlineCSS(loadingCSS),
  "",
  tabPanel(
    "US Choropleth",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "choropleth_how_tally",
            "Tally which?",
            choices = list(
              "Send location" = "sender",
              "Receive location" = "receiver",
              "Both" = "both"
            )
          ),
          uiOutput("choropleth_checkbox_datefilter_UI"),
          uiOutput("choropleth_date_slider_ui")
        ),
        mainPanel(
          div(
            id = "loading-choropleth",
            fluidPage(
              h2(class = "animated infinite pulse","Loading data...")
              # HTML("<img src=images/cruk-logo.png width='50%'></img>")
            )
          ),
          leafletOutput("us_states_choropleth")
        )
      )
    )
  ),
  tabPanel(
    "Letter Journeys",
    fluidPage(
      inlineCSS(customLegend),
      useShinyjs(),
      # runcodeUI(code = "shinyjs::alert('Hello!')", width = "100%", height = "400px"),
      sidebarLayout(
        sidebarPanel(
          uiOutput("journeys_checkbox_datefilter_UI"),
          uiOutput("journeys_date_slider_ui")
        ),
        mainPanel(
          div(
            id = "loading-journeys",
            fluidPage(
              h2(class = "animated infinite pulse","Loading data...")
              # HTML("<img src=images/cruk-logo.png width='50%'></img>")
            )
          ),
          leafletOutput("letter_journeys_map")
        )
      )
      
    )
  )
  

  
))