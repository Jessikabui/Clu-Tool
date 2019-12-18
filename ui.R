# version 7
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(rpivotTable)
library(Cairo)

dashboardPage(skin = "purple",
  dashboardHeader(title = "Clu-Tool Visualization",
                  titleWidth = 250
                ),
      
  ## Sidebar content
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Spatio-Temporal Visualization", tabName = "visual", icon = icon("area-chart")),
      menuItem("Descriptive S-T Clustering", tabName = "visual2", icon = icon("chart-bar")),
      menuItem("Pivot table", tabName = "pivot", icon = icon("sliders-h")),
      menuItem("Table S-T Clustering", tabName = "tabel", icon = icon("table"))
    ) 
  ),
  
  
  
  ## Body content
  dashboardBody(
    tags$head(
      tags$style(
        HTML("
            #myScrollBox{ 
              overflow-y: scroll; 
              overflow-x: scroll; 
              height:1200px;
            }
             ")
      )
    ),
    tabItems(
      # First tab content
      tabItem(tabName = "visual",
              fluidRow(
                box(
                  width = 12,
                  title = "Import Data", solidHeader = TRUE, collapsible = TRUE,
                  fileInput("file1", "Select a ';' delimited CSV file with: 'latitude', 'longitude' and 'date' (in numeric format)",
                            multiple = FALSE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv"))
                ),

                # cluster map
                box(
                  width = 9,height = 650,
                  solidHeader = TRUE,
                  selectInput("size","Size", choices = "" ),
                  h3(textOutput("caption"), align = "center"),
                  leafletOutput("plot"),
                  sliderInput("range", "Time", 0, 1000, value=c(10,800))
                ),

                box(
                  width = 3,
                  solidHeader = TRUE,
                  selectInput("distance","Distance", choices = c("euclidean",
                                                                 "maximum",
                                                                 "canberra",
                                                                 #"binary",
                                                                 "minkowski",
                                                                 "manhattan"
                                                                 ) )
                  
                ),
                box(
                  width = 3,
                  title = "EPS 1", status = "primary",  collapsible = TRUE,
                  sliderInput("eps1", "Spatial", 0, 1.0, 0.35)
                ),
                box(
                  width = 3,
                  title = "EPS 2" , status = "primary", collapsible = TRUE,
                  sliderInput("eps2", "Temporal", 1, 100, 50)
                ),
                box(
                  width = 3,
                  title = "MinPts", status = "primary", collapsible = TRUE,
                  sliderInput("minPts", "", 1, 12, 9)
                ),
                
              
                valueBoxOutput("total", width = 3),

                valueBoxOutput("bigCluster", width = 3),

                valueBoxOutput("smallCluster", width = 3),

                valueBoxOutput("noise", width = 3)

               )
       ),
      
      # Second tab content "Tabla con clusters ST"
      tabItem(tabName = "tabel",
              fluidRow(
                column(12,
                       dataTableOutput('table')
                ),
                downloadButton("downloadData", "Download Data")
              )
       ),
      # Third tab content "histograma clusters ST"
       tabItem(tabName = "visual2",
               fluidRow(
                 box(
                   width=12,solidHeader = TRUE,
                     column(
                       width = 6,
                       solidHeader = TRUE,
                       plotOutput("cluplot", 
                                  brush = brushOpts(id="plot_brush",resetOnNew = TRUE), 
                                  height = 400)),
                     column(
                       width = 6,
                       plotOutput("plot3", height = 400))
                     ),
                       #verbatimTextOutput("info")
                   box(
                       width = 12,height = 400,
                       solidHeader = TRUE,
                       DT::dataTableOutput("info"),
                       style = "height:400px; overflow-y: scroll;overflow-x: scroll;"
                     ),
                    valueBoxOutput("total2", width = 6),
                    valueBoxOutput("noise2", width = 6),
                    box( 
                      width=12,solidHeader = TRUE,
                  #histogram  dates
                      column(
                           width = 6,
                           solidHeader = TRUE,
                           plotOutput("hist", height = 390)
                            ),
                  #boxplot    
                  column(
                       width = 6,
                       solidHeader = TRUE,
                       selectInput("metric","Please select a column:", choices=NULL),
                       plotOutput("boxplot", height = 335)
                       )
                     ),


               )
               
        ),
      # Fourth tab content "Pivot table"
      tabItem(tabName = "pivot",
             fluidRow(
               div(id="myScrollBox",
                 rpivotTableOutput("OverallPivot", width = "100%", height = "500px"))
               
             )
        )
     

    )
  )
)




