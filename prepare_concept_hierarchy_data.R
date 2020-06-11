library(data.table)
library(stringr)
library(dplyr)

concept_and_person_count_ordered_short_df = fread("./concept_and_person_count_ordered_short_df.tsv")

concept_ancestor_df = readRDS(file="../concept_ancestor_OMOP_table.rds")
concept_ancestor_filtered_df = concept_ancestor_df %>% filter( (ancestor_concept_id %in% concept_and_person_count_ordered_short_df$concept_id) |  (descendant_concept_id %in% concept_and_person_count_ordered_short_df$concept_id) )

find_first_order_descendants_indx <-function(c_id){
  
  d_id_arr = concept_ancestor_filtered_df$descendant_concept_id[which(
          concept_ancestor_filtered_df$ancestor_concept_id == c_id & 
          concept_ancestor_filtered_df$max_levels_of_separation==1)]
  
  d_id_arr_in_GHI = intersect(d_id_arr, concept_and_person_count_ordered_short_df$concept_id)
  
  id_set = unlist(lapply(d_id_arr_in_GHI, function(x){which(concept_and_person_count_ordered_short_df$concept_id == x)}))
   
  id_set_reordered = id_set[order(-concept_and_person_count_ordered_short_df$count[id_set])]
                           
  id_set_str = paste(id_set_reordered, collapse = ",")
  
  return(id_set_str)
}

get_a_vector <- function(id_set_str){
  v_arr= as.numeric(str_split(id_set_str,",")[[1]])
  return(v_arr)
}


concept_and_person_count_df = concept_and_person_count_ordered_short_df 

arr_v = seq(1, dim(concept_and_person_count_df)[1],by=1)


concept_and_person_count_df$full_info_str = unlist(lapply(arr_v, function(x){
                                          	paste0(concept_and_person_count_df$concept_name[x]," [",
                                                 concept_and_person_count_df$concept_id[x],":",
                                                 concept_and_person_count_df$concept_code[x],
                                                 "] : Count = ",
                                                 concept_and_person_count_df$count[x])}))
                                                 
  
concept_and_person_count_df$first_order_descendants_indx_str = 
                  unlist(lapply(concept_and_person_count_df$concept_id, function(x){
                                  cat("done ",parent.frame()$i,"\n"); return(find_first_order_descendants_indx(x))}))



saveRDS(concept_and_person_count_df,
        file="concept_and_person_count_first_level_desc_ordered_short_df.rds")
fwrite(concept_and_person_count_df,
       file="concept_and_person_count_first_level_desc_ordered_short_df.tsv",sep="\t")

###### adding synonym to the structure
concept_and_person_count_first_level_desc_ordered_short_df = 
  fread("./concept_and_person_count_first_level_desc_ordered_short_df.tsv")

concept_and_synonym_person_count_ordered_short_df = 
  fread("./concept_and_synonym_person_count_ordered_short_df.tsv")

## get concept_id and sysnonym
concept_id_and_synonym = concept_and_synonym_person_count_ordered_short_df %>%
                          dplyr::select(concept_id,concept_synonym_name)
