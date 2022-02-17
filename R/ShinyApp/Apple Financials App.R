#Homework 3 - Krischelle Joyner

library(shiny)
library(ggplot2)
library(lubridate)
library(dplyr)
library(haven)
library(hrbrthemes)
ui <- fluidPage(
  titlePanel("Apple Financials"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      fileInput("dt", "Upload SAS Data:"),
      selectInput("xvar",
                  "X-Axis Variable:",
                  choices = c("Sales",
                              "Cash",
                              "Assets",
                              "Profits",
                              "R&D",
                              "SG&A")),
      selectInput("yvar",
                  "Y-Axis Variable:",
                  choices = c("Sales",
                              "Cash",
                              "Assets",
                              "Profits",
                              "R&D",
                              "SG&A"),
                  'R&D'),
      selectInput("scale",
                  "Choose the Scale:",
                  choices = c("Levels",
                              "Log 10"),
                  "Levels"),
      radioButtons("model",
                   "Choose the Model:",
                   choices = c("Linear Model",
                               "LOESS",
                               "None"),
                   "LOESS"),
      checkboxInput("ribbon","Standard Error Ribbon", TRUE)
    ),
    mainPanel(plotOutput("plot"))
  )
)
server <- function(input, output){
  output$plot <- renderPlot({
    intro = 'Please upload a SAS data file (sas7bdat extension)
 Make sure that it has the following variables:
 SALEQ, CHEQ, ATQ, OIADPQ, XRDQ, XSGAQ'
    validate(need(input$dt != "", intro))
    aapl <- read_sas(input$dt$datapath)
    yaxis <- switch(input$yvar,
                   "Sales" = "SALEQ",
                   "Cash" = "CHEQ",
                   "Assets" = "ATQ",
                   "Profits" = "OIADPQ",
                   "R&D" = "XRDQ",
                   "SG&A" = "XSGAQ")
    xaxis <- switch(input$xvar,
                   "Sales" = "SALEQ",
                   "Cash" = "CHEQ",
                   "Assets" = "ATQ",
                   "Profits" = "OIADPQ",
                   "R&D" = "XRDQ",
                   "SG&A" = "XSGAQ")
    ytitle <- switch(input$yvar,
                     "Sales" = "Sales (million $)",
                     "Cash" = "Cash (million $)",
                     "Assets" = "Assets (million $)",
                     "Profits" = "Profits (million $)",
                     "R&D" = "R&D (million $)",
                     "SG&A" = "SG&A (million $)")
    xtitle <- switch(input$xvar,
                     "Sales" = "Sales (million $)",
                     "Cash" = "Cash (million $)",
                     "Assets" = "Assets (million $)",
                     "Profits" = "Profits (million $)",
                     "R&D" = "R&D (million $)",
                     "SG&A" = "SG&A (million $)")
    scaletype = switch(input$scale,
                    "Levels" = "identity",
                    "Log 10" = "log10")
    error = switch(input$ribbon,
                      "Standard Error Ribbon" = TRUE)
    modeltype = switch(input$model,
                       "Linear Model" = geom_smooth(method = 'lm', color='white', se = input$ribbon),
                       "LOESS" = geom_smooth(method = 'loess', color='white', se = input$ribbon))
    ggplot(aapl, aes_string(xaxis, yaxis)) +
      geom_point() +
      labs(x = xtitle, y = ytitle) +
      hrbrthemes::theme_modern_rc() +
      scale_x_continuous(trans = scaletype) +
      scale_y_continuous(trans = scaletype) +
      modeltype
  })
}
shinyApp(ui = ui, server = server)