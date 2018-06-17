library(shiny)
library(dygraphs)
library(d3heatmap)
library(metricsgraphics)

shinyUI(fluidPage(titlePanel(
  img(
    src = "Rahul.jpg", height = 80, width = 150
  )
),

sidebarLayout(
  #content on the sidebar
  sidebarPanel(fluidRow(
    column(
      10,h4("Input data"),
      fileInput(
        "file", 'Choose CSV File',
        accept = c('sheetName', 'header'), multiple =
          FALSE
      ),
      #fluidRow(
      selectInput("Xvar", "Variable 1: ",""),
      selectInput("Tvar", "Variable 2: ",""),
      selectInput("Yvar", "Category: ",""),
      selectInput("Zvar", "Group by: ",""),
      #),
      br(),
      br(),
      p(
        "Visualize data through various charts. For
        customization as per your data:"
      ),
      br(),
      h4("Contact us:"),
      p(span("info@", style = "color:blue")),
      br(),
      p("For Demo only. Not for commercial use.")
      )
  )),
  
  #content on the center page
  mainPanel(
    h2("Dashboards for Retail stores"),
    p(
      "Select an excel file with numeric value in the first sheet.
      Visualize data through different charts"
    ),
    #create tabs to show different outputs/charts.
    tabsetPanel(
      tabPanel("Data",
               dataTableOutput("tabl")),
      tabPanel(
        "Analysis: one parameter",
        h5(
          "Select ", span("Variable 1, Category", style = "color:blue"),
          " and ", span("Group by", style = "color: blue"), "variables"
        ),
        fluidRow(column(
          6,plotOutput("barplot1",width = "450px", height = "300px")
        ),
        column(
          6,plotOutput("barplot2",width = "450px", height = "300px")
        ))
      ),
      tabPanel(
        "Analysis: two parameters",
        h5(
          "Select ", span("Variable 1, Variable 2", style = "color:blue"),
          " and ", span("Group by", style = "color: blue"), "variable"
        ),
        metricsgraphicsOutput("scatplot",width = "700px", height = "300px"),
        tableOutput("scattext")
      ),
      tabPanel(
        "Analysis: all parameters",
        h5("Select ", span("Group by", style = "color:blue"), " variables"),
        #fluidRow(
        d3heatmapOutput("corelplot2",width = "850px", height = "550px")
        #br(), br(), br(), br(),
        #plotOutput("corelplot1",width = "700px", height = "300px"),
        #),
        #fluidRow(
        #column(4,tableOutput("coreltext"))
        #)
      ),
      tabPanel(
        "Forecasting",
        h5(
          "Select ", span("Variable 1", style = "color:blue"),
          " and ", span("Group by", style = "color: blue"), "variables"
        ),
        fluidRow(
          column(
            3,selectInput(
              "txinp", label = "Data Type",
              choices = c("month","quarter"),
              selected = "month"
            )
          ),
          column(
            3,dateInput(
              "dtfrm", label = "Date From",
              value = "2010-01-01", min = "1975-03-14", max = "9999-01-01",
              format = "yyyy-mm-dd", startview = "month",
              weekstart = 1, language = "en"
            )
          ),
          column(
            3,numericInput(
              "unpd", label = "Units to Predict",
              value = 6, min = 6, max = 144, step = 6
            )
          ),
          column(
            3,selectInput(
              "interval", label = "Prediction Interval",
              choices = c("0.80", "0.90", "0.95", "0.99"),
              selected = "0.95"
            )
          )
        ),
        #hr(),
        #div(strong("From: "), textOutput("from", inline = TRUE)),
        #div(strong("To: "), textOutput("to", inline = TRUE)),
        #br(),
        #helpText("Click and drag to zoom in (double click to zoom back out)."),
        dygraphOutput("dygraph",width = "800px", height = "300px")
      ),
      tabPanel(
        "Recomm",
        h5(
          "Select ", span("Variable 1, Category", style = "color:blue"),
          " and ", span("Target", style = "color: blue"), ":"
        ),
        numericInput(
          "target", label = "Set target",
          value = 1, min = 1, max = 144, step = .5
        ),
        tableOutput("fourth")
      )
    )
    )
)))