library(shiny)
library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
library(DT)

shinyUI(fluidPage(
    # Some custom CSS
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    navbarPage(
        "Data Visualiser",
        tabPanel("Data",
             sidebarLayout(
                 sidebarPanel(
                     checkboxInput("smooth", "Use Example Dataset"),
                     conditionalPanel(
                         condition = "input.smooth == true",
                         uiOutput("choose_dataset")
                     ),
                                      fileInput('file1', 'Choose CSV File',
                                                accept=c('text/csv', 
                                                         'text/comma-separated-values,text/plain', 
                                                         '.csv')),
                                      tags$hr(),
                                      checkboxInput('header', 'Header', TRUE),
                                      radioButtons('sep', 'Separator',
                                                   c(Comma=',',
                                                     Semicolon=';',
                                                     Tab='\t'),
                                                   ','),
                                      radioButtons('quote', 'Quote',
                                                   c(None='',
                                                     'Double Quote'='"',
                                                     'Single Quote'="'"),
                                                   '"')
                 ),
                 mainPanel(
                     tableOutput('contents')                 
                     )
             )
        ),
        tabPanel("Plot",
            sidebarLayout(
                sidebarPanel(
                   uiOutput("x_axis"),
                   uiOutput("y_axis"),
                   selectInput("ggplot_scaletype", "Scale type",
                                c("normal" = "normal",
                                  "reverse (scale_*_reverse())" = "reverse",
                                  "log10 (scale_*_log10())" = "log10",
                                  "log2 (scale_*_continuous( trans=log2_trans()))" = "log2",
                                  "log10 (coord_trans())" = "log10_trans",
                                  "log2 (coord_trans())" = "log2_trans",
                                  "coord_cartesian()" = "coord_cartesian",
                                  "coord_flip()" = "coord_flip",
                                  "coord_polar() (doesn't work)" = "coord_polar",
                                  "x factor" = "x_factor")
                    ),
                    selectInput("ggplot_facet", "Facet",
                                c("none" = "none",
                                  "wrap" = "wrap",
                                  "grid x" = "grid_x",
                                  "grid y" = "grid_y",
                                  "grid xy" = "grid_xy",
                                  "grid xy free" = "grid_xy_free"
                                )
                    ),
                    div(class = "option-header", "Brush"),
                    radioButtons("brush_dir", "Direction(s)",
                                c("xy", "x", "y"), inline = TRUE),
                    checkboxInput("brush_reset", "Reset on new image")
                 ),
                mainPanel(
                    uiOutput("plotui"),
                    downloadButton('downloadPlot')
                )
            ),
            fluidRow(
                column(width = 9,
                       wellPanel(width = 9,
                                 h4("Points selected"),
                                 DT::dataTableOutput("plot_brushed_points")
                       )
                )
            )
        )
    )
))