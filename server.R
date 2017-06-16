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

shinyServer(function(input, output, session) {
  
  
  #choices = all_pkgs,
  updateSelectizeInput(session, 'in_pkgs', choices = all_pkgs,  server = TRUE)
  
  output$packages <- renderText({
    
    # text recommendation here
    str_c(make_pred(input$in_pkgs, input$method, input$n), collapse = "\n")
    
  })
  
  observeEvent(input$button,{
    gh_pkgs <- tryCatch(get_github(input$gh_repo), error = function(e){print(e);return(NULL)})
    updateSelectizeInput(session, 'in_pkgs',
                         selected = gh_pkgs,
                         choices = all_pkgs,  
                         server = TRUE)
  })
  
})
