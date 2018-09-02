# This is the server logic for a Shiny web application.
library(shiny)
library(Matrix)
library(stringr)
library(tidyverse)
library(pkgrecommendr)


## Reading in Data ---------------------------------------------------------------------

recommenders <- readRDS("recommenders.Rds")

all_pkgs <- rownames(recommenders$CONTENT@tfidf)
cf_pkgs <- colnames(recommenders$UBCF@train)

pkg_table <- readRDS("package_table.Rds")

check_pkgs <- function(pkgs, method){
  if (method != "CONTENT" && !any(cf_pkgs %in% pkgs)){
    return(FALSE)
  }
  return(TRUE)
}

make_pred <- function(pkgs, method, n){
  if (method == "CONTENT"){
    ix <- which(all_pkgs %in% pkgs)
    if (length(ix) == 0) return(NULL)
    tmpmat <- sparseMatrix(i = rep(1,length(ix)), j = ix, x = 1, dims = c(1, length(all_pkgs)),
                           dimnames = list(c("test"), all_pkgs))
    recs <- all_pkgs[recommend_items(recommenders[[method]], tmpmat, n)]
    
  } else {
    ix <- which(cf_pkgs %in% pkgs)
    if (length(ix) == 0) return(NULL)
    tmpmat <- sparseMatrix(i = rep(1,length(ix)), j = ix, x = 1, dims = c(1, length(cf_pkgs)),
                           dimnames = list(c("test"),cf_pkgs))
    recs <- cf_pkgs[recommend_items(recommenders[[method]], tmpmat, n)]
  }
  return(recs)
}

## Server function -----------------------------------------------------------------------------------------

n_recs <- 100

shinyServer(function(input, output, session) {
  
  
  #choices = all_pkgs,
  updateSelectizeInput(session, 'in_pkgs', choices = all_pkgs,  server = TRUE)
  
  output$packages <- renderDataTable({
    
    if (length(input$in_pkgs) > 0 && !check_pkgs(input$in_pkgs, input$method)){
      showModal(modalDialog(
        title = "Insufficient data!",
        "Not enough R files used in training the model called the input package(s).",
        "Try the \"Similar Package Content\" recommendation method, which does not require",
        "any minimum level of observed use.",
        easyClose = TRUE
      ))
    } else if (length(input$in_pkgs) > 0){
      # get recommendation
      recs <- data_frame(rank = seq_len(n_recs), package = make_pred(input$in_pkgs, input$method, n_recs))
      out <- left_join(recs, pkg_table)
      return(out)
    } else{
      return(NULL)
    }
  },options = list(
    pageLength = 10,
    lengthMenu = c(5, 10, 25)
  ))
  
  
  observeEvent(input$als_help,{
    showModal(modalDialog(
      title = "Recommending complementary packages",
      "Compelementary packages are recommended based on package use in Github repositories.",
      "Matrix factorization via alternating least squares is used",
      "to decompose the input repository-package matrix into a matrix of repositories and",
      "latent factors and packages and latent factors. The latent factors take into account",
      "the similarity between users and the similarity between packages.",
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$content_help,{
    showModal(modalDialog(
      title = "Recommending similar packages",
      "Similar packages are recommended based on similarity between the",
      "documentation for each package.",
      "Documentation in the 'man' folder for each package was concatenated & cleaned prior to",
      "computing the TF-IDF (Term Frequency-Inverse Document Frequency).",
      "When providing multiple input packages, TF-IDF",
      "vector is averaged across input packages. Package recommendations are based",
      "on the most similar packages using cosine similarity of the TF-IDF.",
      "One advantage of a content based approach is that it will recommend packages",
      "that are very new or have not been used in one of the github repos analyzed.",
      easyClose = TRUE
    ))
  })
  
  
  
})
