library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("RecommendR"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      helpText("RecommendR is an app that suggests other R packages you might", 
               "be interested in based on packages you are already using/considering for a project."),
      selectizeInput(
        'in_pkgs', 'Packages I like/use', choices = NULL, multiple = TRUE
      ),
      textInput('gh_repo',
                tagList("Github repository",
                        actionLink("gh_help","", icon = icon("question-circle-o"))),
                placeholder = "e.g. r-lib/gh"),
      actionButton("button", "Read packages from Github", width = "100%"),
      br(),br(),
      radioButtons("method","Recommendation based on:",
                   choiceNames = list(tagList("Similar Package Use",
                                              actionLink("als_help","", icon = icon("question-circle-o"))),
                                      tagList("Similar Package Content",
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
