library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("RecommendR"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      helpText("RecommendR is an app that suggests R packages you might", 
               "be interested in based on packages you are already considering for a project."),
      tags$style(type='text/css', 
                 ".selectize-input { font-size: 1.25em; line-height: 1.25em;} .control-label { font-size: 1.25em; } .radio { font-size: 1.1em}"),
      selectizeInput(
        'in_pkgs', 'Packages I like/use', choices = NULL, multiple = TRUE
      ),
      radioButtons("method","Recommend:",
                   choiceNames = list(tagList("Complementary packages",
                                              actionLink("als_help","", icon = icon("question-circle-o"))),
                                      tagList("Similar packages",
                                              actionLink("content_help","", icon = icon("question-circle-o")))), 
                   choiceValues = c("ALS","CONTENT"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      dataTableOutput("packages")
    )
  ),
  theme = shinytheme("flatly")
))
