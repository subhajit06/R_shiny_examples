### Nov 21, 2019 Subhajit Sengupta
### https://rdrr.io/cran/dqshiny/man/autocomplete_input.html
### adding synonym search

library(data.table)
library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(dqshiny)

#topN = 30
#totalCnt = 26060 #### these are the total number of unique persons from condition_occurrence table

setwd("~/Desktop/ToDo/CPG/EHR_data_integration/concept_hierarchy/OMOP_SQL/hierarchy/")

concept_and_person_count_first_level_desc_ordered_short_df = 
  fread("./concept_and_person_count_first_level_desc_ordered_short_df.tsv")

concept_and_synonym_person_count_ordered_short_df = 
  fread("./concept_and_synonym_person_count_ordered_short_df.tsv")

## get concept_id and sysnonym
concept_and_synonym = concept_and_synonym_person_count_ordered_short_df %>%
  dplyr::select(concept_id, concept_name, concept_synonym_name, concept_code, count)

df0 = concept_and_synonym

get_a_vector <- function(id_set_str){
  v_arr = as.numeric(str_split(id_set_str,",")[[1]])
  return(v_arr)
}

# get_first_level_child_info <- function(c_id){
#   #### there will be multiple but just take the first one as everything is same except the concept_synonym_name 
#   id = which(concept_synonym_person_count_first_level_desc_ordered_short_df$concept_id == c_id)[1]
#   child_indx_vec_str = 
#     concept_synonym_person_count_first_level_desc_ordered_short_df$first_order_descendants_indx_str[id]
#   
#   indx_vec = get_a_vector(child_indx_vec_str)
#   info_list = concept_synonym_person_count_first_level_desc_ordered_short_df$full_info_str[indx_vec]
#   
# }

#non_empty_first_order_child_idx = which(
#  concept_synonym_person_count_first_level_desc_ordered_short_df$first_order_descendants_indx_str != "")

#df = concept_synonym_person_count_first_level_desc_ordered_short_df[non_empty_first_order_child_idx,]

#######################
df = concept_and_person_count_first_level_desc_ordered_short_df

# Choices for drop-downs
var_1 = df0$concept_synonym_name

if (interactive()) {

  ui = fluidPage(
    titlePanel("Helper for Searching Conditions in GHI Cohort"),
    fluidRow(
      hr(),
      column(10, h4("Input:") ),
      #column(10, autocomplete_input("syn_name", "Enter your favourite condition", var_1) ),
      column(10, selectizeInput("syn_name", "Enter your favourite condition", var_1) ),
      headerPanel(""),
      
      column(10, h4("Output:") ),
      column(10, tags$label("OMOP Concept Name (used in ICEGO)"), verbatimTextOutput("c_name", placeholder = TRUE)),
      #column(2, tags$label("Count"), verbatimTextOutput("cnt", placeholder = TRUE)),
      column(5, tags$label("OMOP Concept ID", verbatimTextOutput("c_id", placeholder = TRUE)))
      #column(2, tags$label("SNOMED Code", verbatimTextOutput("snomed_c_code", placeholder = TRUE))),       
      #hr(),
      #column(11, h3(paste0("First-level children of the chosen condition (Top ",topN, ")"))),
      #hr(),
      #column(12,plotOutput("cHist"))
    )
  ) ### ui
  
  server = function(input, output, session) {
    
    output$c_name <- renderText(as.character(df0$concept_name[which(df0$concept_synonym_name == input$syn_name)]))
    #output$cnt <- renderText(paste0(df0$count[which(df0$concept_synonym_name == input$syn_name)],
    #                                " (",round(df0$count[which(df0$concept_synonym_name == input$syn_name)]*100/totalCnt,digits = 2),"%)")) 
    output$c_id <- renderText(as.character(df0$concept_id[which(df0$concept_synonym_name == input$syn_name)]))     
    output$snomed_c_code <- renderText(as.character(df0$concept_code[which(df0$concept_synonym_name == input$syn_name)]))
    
    observe({
      
      idx_in_syn_table = which(df0$concept_synonym_name == input$syn_name)
      c_id_for_that_syn = df0$concept_id[idx_in_syn_table]
      
      idx = which(df$concept_id == c_id_for_that_syn)
      
      cat(df$concept_name[idx],"\n")
      
    }) #### observe
    
  } ### server
  
} #### interactive


#### Run the search app 
condition_app_with_search = shinyApp(ui = ui, server = server, options = list(width=200, height=50))
runApp(condition_app_with_search)
#runApp(condition_app_with_search,launch.browser = TRUE, host = '127.0.0.1', port = 11354)

