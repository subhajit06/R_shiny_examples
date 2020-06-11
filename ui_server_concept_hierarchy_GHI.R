##### this is for genomic summary for GHI
library(data.table)
library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)

topN = 30

setwd("~/Desktop/ToDo/CPG/EHR_data_integration/concept_hierarchy/OMOP_SQL/hierarchy/")

concept_and_person_count_first_level_desc_ordered_short_df = fread("./concept_and_person_count_first_level_desc_ordered_short_df.tsv")
arr_v = 1:dim(concept_and_person_count_first_level_desc_ordered_short_df)[1]

concept_and_person_count_first_level_desc_ordered_short_df$info_str = unlist(lapply(arr_v, function(x){
      paste0(concept_and_person_count_first_level_desc_ordered_short_df$concept_name[x]," (",
             concept_and_person_count_first_level_desc_ordered_short_df$count[x],")")}))


get_a_vector <- function(id_set_str){
  v_arr = as.numeric(str_split(id_set_str,",")[[1]])
  return(v_arr)
}

get_first_level_child_info <- function(c_id){
  id = which(concept_and_person_count_first_level_desc_ordered_short_df$concept_id == c_id)
  child_indx_vec_str = concept_and_person_count_first_level_desc_ordered_short_df$first_order_descendants_indx_str[id]
  indx_vec = get_a_vector(child_indx_vec_str)
  info_list = concept_and_person_count_first_level_desc_ordered_short_df$full_info_str[indx_vec]
  
}

non_empty_first_order_child_idx = which(concept_and_person_count_first_level_desc_ordered_short_df$first_order_descendants_indx_str != "")

df = concept_and_person_count_first_level_desc_ordered_short_df[non_empty_first_order_child_idx,]

#######################

# Choices for drop-downs
var_1 = df$info_str ### All is for all the nodes having a non-empty set of child

ui <- fluidPage(
  
  titlePanel("Total Count for Different Conditions in GHI Cohort"),
  sidebarLayout(
    
    sidebarPanel( width = 5,
        selectInput("info_str", "Conditions with count", var_1)
      ), ### sidebar
    
    #### Main panel
    mainPanel(width = 10,
              hr(),
              h3(paste0("Top first-level children of the selected condition (max ",topN, ")")),
              plotOutput("cHist")
    ) ### main panel
  ) ### sidebarLayout
) ### fluidPage

server <- function(input, output, session) {
  
  observe({
    
    idx = which(df$info_str == input$info_str)
    v_arr = get_a_vector(df$first_order_descendants_indx_str[idx])
   
    df1 = concept_and_person_count_first_level_desc_ordered_short_df[v_arr,]
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
        
        
     },height = 750, width = 1200)

  }) ### observe 
}

#### Run the app 
condition_app = shinyApp(ui = ui, server = server)
runApp(condition_app)
#runApp(condition_app,launch.browser = TRUE, host = '127.0.0.1', port = 7354)
