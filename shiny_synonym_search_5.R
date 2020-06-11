
## Only run examples in interactive R sessions
library(data.table)
library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(dqshiny)
library(shinyWidgets)
setwd("~/Desktop/ToDo/CPG/EHR_data_integration/concept_hierarchy/OMOP_SQL/hierarchy/")

df_cond_syn = readRDS("./df_cond_syn.Rds")
df_syn_filtered = readRDS("./df_syn_filtered.Rds")
df_cond = readRDS("./df_cond.Rds")


get_delimited_str <- function(str_arr){
  temp_arr = NULL
  for(i in 1:length(str_arr)){
    v_arr = str_split(str_arr[i],";")[[1]]
    temp_arr = c(temp_arr,v_arr)
  }
  return(temp_arr)
}


return_c_id <-function(s){
  c_id = df_syn_filtered$concept_id[which(df_syn_filtered$concept_synonym_name == s)]
  return(as.character(c_id))
}

return_c_name <-function(s){
  c_id = df_syn_filtered$concept_id[which(df_syn_filtered$concept_synonym_name == s)]
  return(df_cond$concept_name[df_cond$concept_id == c_id])
}

# return_c_id <-function(s){
#   c_id = df_cond_syn$concept_id[which(df_cond_syn$concept_synonym_names == s)]
#   return(as.character(c_id))
# }
# 
# return_c_name <-function(s){
#   c_name = df_cond_syn$concept_name[which(df_cond_syn$concept_synonym_names == s)]
#   return(c_name)
# }


# getStrWithKw <- function(input_kw_str){
#   x = ""
#   if(str_length(input_kw_str) > 0){
#     tmp_words = unlist(str_split(input_kw_str," ")) 
#     s_str = paste(lapply(tmp_words,function(x){paste0("(?=.*",tolower(x),")")}), collapse = "")
#     #x = df_cond_syn$concept_synonym_names[
#     #  str_detect(df_cond_syn$concept_synonym_names_tolower, s_str)]
#     x = str_subset(df_cond_syn$concept_synonym_names_tolower, s_str)
#   }
#   return(x)
# }

getStrWithKw_nested <- function(input_kw_str){
  x = ""
  if(str_length(input_kw_str) > 0){
    tmp_words = tolower(unlist(str_split(input_kw_str," ")) )
    x = df_cond_syn$concept_synonym_names
    for(i in 1:length(tmp_words)){
      #x = str_subset(x, tmp_words[i])
      x = as.vector(x[str_detect(x, fixed(tmp_words[i],ignore_case = TRUE))])
    }
  }
  return(x)
}


# getRegExFromKw <- function(input_kw_str){
#   if(str_length(input_kw_str) > 0){
#     tmp_words = unlist(str_split(input_kw_str," ")) 
#     s_str = paste(lapply(tmp_words,function(x){paste0("(?=.*",tolower(x),")")}), collapse = "")
#   }else{
#     s_str = ""
#   }
#   return(s_str) 
# }

setwd("~/Desktop/ToDo/CPG/EHR_data_integration/concept_hierarchy/OMOP_SQL/hierarchy/")

# df_concept=readRDS("../concept_OMOP_table.rds")
# df_syn = readRDS("../concept_synonym_OMOP_table.rds") %>% select(concept_id, concept_synonym_name)
# 
# df_cond = df_concept %>% select(concept_id, concept_name, domain_id,
#                                 standard_concept, valid_end_date) %>%
#   
#   filter(domain_id == "Condition" & standard_concept == 'S' &
#            valid_end_date == '2099-12-31') %>%
#   
#   select(concept_id, concept_name)
# 
# df_cond = as.data.frame(df_cond)
# 
# df_syn_filtered = df_syn %>% filter(concept_id %in% df_cond$concept_id)
# 
# #df_cond_syn_filtered = inner_join(df_cond, df_syn_filtered, by='concept_id')
# #df = df_cond_syn_filtered %>% group_by(concept_id, concept_name) %>% 
# #            mutate(collapsed_syn_names = paste0(concept_synonym_name, collapse = ";"))
# 
# collapsed_syn_str = unlist(lapply(df_cond$concept_id, function(x){
#                 paste(df_syn_filtered$concept_synonym_name[which(df_syn_filtered$concept_id == x)],
#                       collapse=";")}))
# 
# df_cond_syn = NULL
# df_cond_syn$concept_id = df_cond$concept_id
# df_cond_syn$concept_name = df_cond$concept_name
# df_cond_syn$concept_synonym_names = collapsed_syn_str
# ##### everything has to be lower
# df_cond_syn$concept_synonym_names_tolower = tolower(df_cond_syn$concept_synonym_names)
# 
# df_cond_syn = as.data.frame(df_cond_syn)
# 
# saveRDS(df_cond_syn,"./df_cond_syn.Rds")



# df_str = NULL
# df_str$id = c(1,2,3,4,5,6)
# df_str$name = c('A',"K","O","B","P","W")
# df_str$syn_name = c("apple","kiwi","orange","banana","papaya","Watermelon")
# 
# df_str = as.data.frame(df_str)

#stringr::str_subset(df_cond_syn$concept_synonym_name, "(?=.*small)(?=.*cell)(?=.*lung)")

if (interactive()) {
  
  ui <- fluidPage(
    
    titlePanel("Helper Interface for Searching Conditions in GHI Cohort"),
    fluidRow(
      hr(),
      column(7, h3(span("Input", style = "color:blue"))),
      column(5, h3(span("Output", style = "color:blue"))),
      column(7, textInput("search_kw", "Search Multiple Keywords", '')),
      
      column(5, tags$label("OMOP Concept ID", verbatimTextOutput("c_id", placeholder = TRUE))),
      
      
      #column(5, h4("Regex:") ),
      #column(8, tags$label("Search regex pattern "), verbatimTextOutput("reg_ex",placeholder = TRUE)),
      #column(12, tags$label("Matched String", verbatimTextOutput("matched_str",placeholder = TRUE))),
      
      headerPanel(""),
      #column(5, h4("Choices:") ),
      column(7, tags$label("", selectInput("select_group_choice","grp_choices",choices = ""))),
      column(5, tags$label("OMOP Concept Name (used in ICEGO)", verbatimTextOutput("c_name",placeholder = TRUE))),
      column(7, tags$label("", selectInput("select_choice","Choices",choices = "")))
      
      #headerPanel(""),
      #column(5, h4("OMOP:") ),
      
      
      
    )
  )
  
  server <- function(input, output, session) {
    
    output$c_id = renderText({ 
      if(str_length(input$search_kw) > 0){
        get_c_id()
      }else{
        return()
      }
    })

    
    output$c_name = renderText({ 
      if(str_length(input$search_kw) > 0){
        get_c_name()
      }else{
        return()
      }
    })
    
    #output$matched_str = renderText({ 
    #  return(input$select_group_choice)
    #})
    
    #output$in_str = renderText({ return(as.character(input$search_kw)) })
    
    get_c_id <- reactive({
      return_c_id(input$select_choice)
    })
    get_c_name <- reactive({
      return_c_name(input$select_choice)
    })
    
    getGroupSearchStr_reactive <- reactive({
      return(getStrWithKw_nested(input$search_kw))
    })
    
    getSearchStr_reactive <- reactive({
      return(get_delimited_str(input$select_group_choice))
    })
    
    
    #getRegExFromKw_reactive <-  reactive({
    #  return(getRegExFromKw(input$search_kw))
    #})
    
    #output$reg_ex = renderText({ getRegExFromKw_reactive() })
    #   if(str_length(input$search_kw) > 0){
    #     tmp_words = unlist(str_split(input$search_kw," ")) 
    #     s_str = paste(lapply(tmp_words,function(x){paste0("(?=.*",tolower(x),")")}), collapse = "")
    #     #x = str_subset(df_cond_syn$concept_synonym_name_tolower, s_str)
    #     x = df_cond_syn$concept_synonym_names[
    #               str_detect(df_cond_syn$concept_synonym_names_tolower, s_str)]
    #     #x = str_subset(df_cond_syn$concept_synonym_name_tolower, s_str)
    #     
    #   }else{
    #     x = ""
    #   }
    #   return(x)
    # })
    
    # output$reg_ex = renderText({ 
    #   if(str_length(input$search_kw) > 0){
    #     tmp_words = unlist(str_split(input$search_kw," ")) 
    #     s_str = paste(lapply(tmp_words,function(x){paste0("(?=.*",tolower(x),")")}), collapse = "")
    #   }else{
    #     s_str = ""
    #   }
    #   return(s_str) 
    # })
    
    observe({
      
      # Can use character(0) to remove all choices
      #if (is.null(x))
      #  x <- character(0)
      
      # Can also set the label and select items
      updateSelectInput(session, "select_group_choice",
                        label = "Select the group of synonyms",
                        choices = getGroupSearchStr_reactive() )
    
      
      #updateSelectInput(session, "select_choice",
      #                  label = "Select",
      #                  choices = get_delimited_str(input$select_group_choice) )
      #updateSelectInput(session, "select_choice",
      #                  label = "Select",
      #                  choices = get_delimited_str(getStrWithKw(input$search_kw)))
      
    })
    
    observe({
      
      updateSelectInput(session, "select_choice",
                        label = "Select the synonym",
                        choices = getSearchStr_reactive() )
      #updateSelectInput(session, "select_choice",
      #                  label = "Select",
      #                  choices = get_delimited_str(getStrWithKw(input$search_kw)))
      
    })
  }
}

app_search = shinyApp(ui = ui, server = server, options = list(width=200, height=50))
#runApp(app_search)
runApp(app_search,launch.browser = TRUE, host = '127.0.0.1', port = 13457)
