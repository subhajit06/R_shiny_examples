### Nov 21, 2019 Subhajit Sengupta
### https://rdrr.io/cran/dqshiny/man/autocomplete_input.html
### adding synonym search

library(data.table)
library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(dqshiny)

topN = 30
totalCnt = 26060 #### these are the total number of unique persons from condition_occurrence table

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
    titlePanel("Total Count for Different Conditions in GHI Cohort"),
    fluidRow(
      hr(),
      column(4, autocomplete_input("syn_name", "Search Condition Synonym", var_1) ),
      column(4, tags$label("OMOP Concept Name"), verbatimTextOutput("c_name", placeholder = TRUE)),
      column(2, tags$label("Count"), verbatimTextOutput("cnt", placeholder = TRUE)),
      column(2, tags$label("OMOP Concept ID", verbatimTextOutput("c_id", placeholder = TRUE))),
      #column(2, tags$label("SNOMED Code", verbatimTextOutput("snomed_c_code", placeholder = TRUE))),       
      #hr(),
      column(11, h3(paste0("First-level children of the chosen condition (Top ",topN, ")"))),
      hr(),
      column(12,plotOutput("cHist"))
    )
  ) ### ui
  
  server = function(input, output, session) {
    
    output$c_name <- renderText(as.character(df0$concept_name[which(df0$concept_synonym_name == input$syn_name)]))
    output$cnt <- renderText(paste0(df0$count[which(df0$concept_synonym_name == input$syn_name)],
                                    " (",round(df0$count[which(df0$concept_synonym_name == input$syn_name)]*100/totalCnt,digits = 2),"%)")) 
    output$c_id <- renderText(as.character(df0$concept_id[which(df0$concept_synonym_name == input$syn_name)]))     
    output$snomed_c_code <- renderText(as.character(df0$concept_code[which(df0$concept_synonym_name == input$syn_name)]))
    
    observe({
      
      idx_in_syn_table = which(df0$concept_synonym_name == input$syn_name)
      c_id_for_that_syn = df0$concept_id[idx_in_syn_table]
      
      idx = which(df$concept_id == c_id_for_that_syn)
      
      cat(df$concept_name[idx],"\n")
      if(length(idx) > 0){
        v_arr = get_a_vector(df$first_order_descendants_indx_str[idx])
        df1 = df[v_arr,]
        cat("Dim of data:", dim(df1)[1],"x",dim(df1)[2],"\n")
        if(dim(df1)[1] > topN){
          df1 = df1[1:topN,]
        }
        
        
        output$cHist <- renderPlot({
          
          df1 = df1[order(-count),]
          df1$concept_name = factor(df1$concept_name, levels=df1$concept_name)
          
          ggplot(df1, aes(x = concept_name, y = count))+
            geom_bar(stat = "identity",width = 0.6,fill = "#56B4E9",color='black')+
            geom_text(aes(label=count), vjust=-0.4)+
            theme(axis.text.x = element_text(angle = 90, hjust = 1),
                  axis.text=element_text(size=13),
                  axis.title=element_text(size=16,face="bold"))+
            xlab("Condition Name")+
            ylab("Participant Count")
          
          
        },height = 750, width = 1100)
      }
    }) #### observe
    
  } ### server
  
} #### interactive


#### Run the search app 
condition_app_with_search = shinyApp(ui = ui, server = server)
runApp(condition_app_with_search)
#runApp(condition_app_with_search,launch.browser = TRUE, host = '127.0.0.1', port = 11354)

