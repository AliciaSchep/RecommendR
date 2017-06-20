# This is the server logic for a Shiny web application.
library(shiny)
library(Matrix)
library(rvest)
library(stringr)
library(tidyverse)
library(pkgrecommendr)

## HELPER FUNCTIONS -----------------------------------------------------------------------

'%ni%' = Negate('%in%')

regex_libraries <- str_c(c(map_chr(c("library","require","loadNamespace","requireNamespace"), 
                                   function(f) str_c("(?<=",f,"\\()[^[\\)]]+(?=\\))")),
                           "[[:alnum:]\\.]+(?=\\:\\:)"), collapse = "|")

get_github <- function(repo){
  repo_split <- str_split(repo,"/")[[1]]
  if (length(repo_split) != 2) return(NULL)
  res <- gh::gh("/repos/:owner/:repo/contents/:path", 
                owner = repo_split[1], repo = repo_split[2], path = ".")
  file_names <- map_chr(res, function(x) x$name)
  if ("DESCRIPTION" %in% file_names){
    d <- read_file(res[[which(file_names == "DESCRIPTION")]]$download_url)
    parsed_desc <- desc::desc(text = d)
    imports <- str_trim(str_split(parsed_desc$get("Imports")[[1]],",")[[1]])
    depends <- str_trim(str_split(parsed_desc$get("Depends")[[1]],",")[[1]])
    pkgs <- str_extract(c(imports, depends),"^[[:alnum:]\\.]+")
  } else{
    file_types <- map_chr(res, function(x) x$type)
    file_urls <- c(map_chr(res[file_types == "file"], function(x) x$download_url),
                   unlist(map(map_chr(res[file_types == "dir"], function(x) x$path), 
                              gh_recurse, repo_split[1], repo_split[2])))
    r_files <- map_chr(str_subset(file_urls,"\\.([[Rr]]|Rmd)$"), 
                       function(x){Sys.sleep(0.1); read_file(x)})
    pkgs <- data_frame(content = r_files) %>% 
      mutate(lib_content = str_extract_all(content,regex_libraries),
             content = NULL) %>%
      unnest(lib_content) %>% 
      mutate(lib_content = str_trim(lib_content)) %>%
      mutate(lib_content = str_replace_all(lib_content,'\\"','')) %>%
      mutate(lib_content = str_replace_all(lib_content,'\\,.*','')) %>%
      mutate(lib_content = str_replace_all(lib_content,"\\'|\\`",'')) %>%
      distinct() %>% select(lib_content) %>% unlist(use.names = FALSE) 
  }
  pkgs <- pkgs[pkgs %ni% c("R","stats","graphics","grDevices",
                           "utils","datasets","methods","base","knitr")]
  return(pkgs)
}

gh_recurse <- function(path, repo_owner, repo_name){
  res <- gh::gh("/repos/:owner/:repo/contents/:path", 
                owner = repo_owner, repo = repo_name, path = path)
  file_types <- map_chr(res, function(x) x$type)
  file_urls <- map_chr(res[file_types == "file"], function(x) x$download_url)
  if (any(file_types == "dir")){
    file_urls <- c(file_urls, 
                   unlist(map(map_chr(res[file_types == "dir"], function(x) x$path), 
                              gh_recurse, repo_owner, repo_name)))
  }
  return(file_urls)
}

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
      recs <- data_frame(rank = seq_len(input$n), package = make_pred(input$in_pkgs, input$method, input$n))
      out <- left_join(recs, pkg_table)
      print(out)
      return(out)
    } else{
      return(NULL)
    }
  })
  
  observeEvent(input$button,{
    gh_pkgs <- tryCatch(get_github(input$gh_repo), error = function(e){print(e);return(NULL)})
    updateSelectizeInput(session, 'in_pkgs',
                         selected = gh_pkgs,
                         choices = all_pkgs,  
                         server = TRUE)
  })
 
  observeEvent(input$gh_help,{
    showModal(modalDialog(
      title = "Getting packages from Github",
      "Rather than inputting packages one by one, you can import all the packages used by a",
      "public Github repository.",
      "If the repository is an R package, R packages used will be extracted",
      "from the Imports and Depends fields of the DESCRIPTION file,",
      "removing base packages. Otherwise, packages are extracted from",
      "R and R markdown files",
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$als_help,{
    showModal(modalDialog(
      title = "Recommending based on similar users and packages",
      "This algorithm uses matrix factorization via alternating least squares",
      "to decompose the input repository-package matrix into a matrix of repositories and",
      "latent factors and packages and latent factors. The latent factors take into account",
      "the similarity between users and the similarity between packages.",
      easyClose = TRUE
    ))
  })
   
  observeEvent(input$ubcf_help,{
    showModal(modalDialog(
      title = "Recommending based on similar users",
      "This algorithm finds users with a similar profile of packages as your input list.",
      "Recommendations are based on what other packages those users employed.",
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$content_help,{
    showModal(modalDialog(
      title = "Recommending based on package content",
      "This algorithm finds packages with similar content to your input.",
      "Package \"content\" is determined using package documentation.",
      "Documentation in `man` folder was concatenated & cleaned prior to",
      "computing the TF-IDF (Term Frequency-Inverse Document Frequency).",
      "When providing multiple input packages, TF-IDF",
      "vector is averaged across input packages. Package recommendations are based",
      "on most similar packages using cosine similarity.",
      "The advantage of a content based approach is that it will recommend packages",
      "that are very new or have not been used in one of the github repos analyzed.",
      easyClose = TRUE
    ))
  })
  
  
  
})
