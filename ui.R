library(shiny)

#pkgs <- available.packages()[,"Package"]

shinyUI(fluidPage(
  
  # Application title
  titlePanel("RecommendR"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        'in_pkgs', 'Packages I like/use', choices = NULL, multiple = TRUE
      ),
      textInput('gh_repo',"Github Repository"),
      actionButton("button", "Read packages from Github", width = "100%"),
      br(),br(),
      numericInput("n","Number of packages to recommend", 
                   value = 8, min = 1, max = 50, step = 1),
      radioButtons("method","Recommendation based on:",
                   choiceNames = c("Similar Users and Packages","Similar Users",
                                   "Similar Package Content"), choiceValues = c("ALS","UBCF","CONTENT"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      verbatimTextOutput("packages")
    )
  )
))
