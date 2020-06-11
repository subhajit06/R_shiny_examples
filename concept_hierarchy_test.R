######

library(data.table)
library(stringr)
library(dplyr)

filter_concept_df = fread(file="../filtered_concept_df.csv")
condition_concept_id_vector = filter_concept_df$concept_id

person_condition_df_all = readRDS("../condition_occurrence_OMOP_table.rds")

person_condition_df = person_condition_df_all %>% dplyr::select(person_id, condition_concept_id)

list_of_conditions_per_person = lapply(unique(person_condition_df$person_id),function(x){return (as.vector(sort(setdiff(unique(person_condition_df$condition_concept_id[which(person_condition_df$person_id == x)]),'0'))))} )


arr_of_conditions_per_person = unlist(lapply(list_of_conditions_per_person,function(x){return(paste(x,collapse=','))}))

person_cond_arr = NULL
person_cond_arr$person_id = unique(person_condition_df$person_id)
person_cond_arr$condition_concept_id_arr = arr_of_conditions_per_person
person_cond_arr = as.data.frame(person_cond_arr)


##### to revert back to an integer concept_id list
##### as.numeric(as.vector(str_split(person_cond_arr$condition_concept_id_arr[1],',')[[1]]))
#fwrite(person_cond_arr,file="./person_cond_arr.tsv",sep="\t")
saveRDS(person_cond_arr,file="./person_cond_arr.rds")

###################

concept_ancestor_df = readRDS(file="../concept_ancestor_OMOP_table.rds")
concept_df = readRDS(file="../concept_OMOP_table.rds")


#### Few concept_id
#### Pain in limb = 138525
#### Pain in upper limb = 4009890

#### We need to find all the ancestors from concept_ancestor table for a given descendent_concept_id
#### This is equivalent to find reverse path to root (unordered)

### now all these ancester concept ids i.e. filtered_concept$concept_id should get 1 
###  1) if they exists in 7692 long concept set ### may be we may not have to check it ??
###  2) if the target_id has a 1

#### closure is the operation I am calling that would add all the ancester concept to an exiting set
build_concept_set_closure <- function(input_concept_id){

	### find all the concepts from the concept_ancestor table where input_concept_id is a descendant_conceppt_id 
	ancestor_concept_id = concept_ancestor_df$ancestor_concept_id[
										which(concept_ancestor_df$descendant_concept_id == input_concept_id)]

	### take those concept_ids which satisfies the following conditions
	filtered_concept_id  = concept_df %>% 
							filter(concept_id %in% ancestor_concept_id) %>%
							filter( (domain_id == "Condition") &
									(vocabulary_id == "SNOMED") &
									(concept_class_id == "Clinical Finding") &
									(standard_concept == "S") &
									(valid_end_date == "2099-12-31") ) %>%
							dplyr::select(concept_id)

	closure_set_concept_id = unique(filtered_concept_id$concept_id)

	return(closure_set_concept_id)

}

### run with list_concept_id = filter_concept_df$concept_id and ancestor list also keeps the originating concept_id
build_larger_set_of_concept_id <-function(list_concept_id){
	
	list_of_ancestor_concept_id_per_concept_id = lapply(list_concept_id,function(x){return (sort(build_concept_set_closure(x)))})
	arr_of_ancestor_concept_id_per_concept_id = unlist(lapply(list_of_ancestor_concept_id_per_concept_id,function(x){return(paste(x,collapse=','))}))

	ancestor_concept_df = NULL
	ancestor_concept_df$concept_id = list_concept_id 
	ancestor_concept_df$concept_id_arr = arr_of_ancestor_concept_id_per_concept_id
	ancestor_concept_df = as.data.frame(ancestor_concept_df)

	return(ancestor_concept_df)

}

list_concept_id = filter_concept_df$concept_id
ancestor_concept_df = build_larger_set_of_concept_id(list_concept_id)
saveRDS(ancestor_concept_df,file="./ancestor_concept_df.rds")
ancestor_concept_df = readRDS(file="./ancestor_concept_df.rds")

get_all_concept_id_int <- function(x){
	return(as.numeric(as.vector(
		str_split(ancestor_concept_df$concept_id_arr[which(ancestor_concept_df$concept_id == x)],',')[[1]])))
}

### get all the existing condition for each person in a string (comma separated concept_id_list) and for each concept_id 
### find all ancerstor_concept_id and make a bigger list, find the unique one and sort in ascending order 

find_closure_for_concept_id_set <-function(condition_arr_str){

	if(str_length(condition_arr_str) > 0){
		concept_id_vec = as.numeric(as.vector(str_split(condition_arr_str,',')[[1]]))	
		sorted_all_concept_id_with_ancestor = sort(unique(unlist(lapply(concept_id_vec,function(x){get_all_concept_id_int(x)}))))
		return(paste(sorted_all_concept_id_with_ancestor,collapse=','))
	}else{
		return("")
	}
}

get_all_person_id_in_a_str <- function(c_id){

	binary_list_of_person_for_c_id = as.numeric(unlist(lapply(list_of_concept_id_per_person, function(x){c_id %in% x})))
	person_id_list = person_cond_arr_with_ancestor_df$person_id[which(binary_list_of_person_for_c_id == 1)]		
	person_id_list_str = paste(person_id_list,collapse=',')
	return(person_id_list_str)
}


person_cond_arr_with_ancestor_tmp = NULL
person_cond_arr_with_ancestor_tmp = person_cond_arr[-which(str_length(person_cond_arr$condition_concept_id_arr) == 0),]
person_cond_arr_with_ancestor = NULL

person_cond_arr_with_ancestor$person_id = person_cond_arr_with_ancestor_tmp$person_id
person_cond_arr_with_ancestor$condition_concept_id_arr = as.vector(person_cond_arr_with_ancestor_tmp$condition_concept_id_arr)
person_cond_arr_with_ancestor$condition_concept_id_with_ancestor_arr = unlist(lapply(as.vector(person_cond_arr_with_ancestor_tmp$condition_concept_id_arr),function(x){cat("processed :", parent.frame()$i,"\n");return(find_closure_for_concept_id_set(x))}))
person_cond_arr_with_ancestor =  as.data.frame(person_cond_arr_with_ancestor)
saveRDS(person_cond_arr_with_ancestor,file="./person_cond_arr_with_ancestor.rds")
	
full_list_of_concept_id = sort(unique(unlist(lapply(person_cond_arr$condition_concept_id_arr,function(x){as.numeric(as.vector(str_split(x,',')[[1]]))}))))

cat("Base concept_id set size = ",length(full_list_of_concept_id),"\n")

full_list_of_concept_id_with_ancestors = sort(unique(unlist(lapply(person_cond_arr_with_ancestor$condition_concept_id_with_ancestor_arr,function(x){as.numeric(as.vector(str_split(x,',')[[1]]))}))))

cat("After exanding to_cover all ancestor concept_id the set size = ",length(full_list_of_concept_id_with_ancestors),"\n")


#######################
#### now flipping the concept_id and person_id matrix's row's and column
#### we need to find all the person_ids that have each of the condition 
#### For all 13399 unique concept_id we need a string that has a list of person_id that has that condition
#### So we will have a data.frame of 2 columns, first column is concept_id and second column 
#### is a list (comma separated string) of all person_id that has that concept_id
#### we can generate count of persons for each concept_id from this data.frame

person_cond_arr_with_ancestor_df = readRDS("./person_cond_arr_with_ancestor.rds")
list_of_concept_id_per_person = lapply(person_cond_arr_with_ancestor_df$condition_concept_id_with_ancestor_arr,function(x){as.numeric(as.vector(str_split(x,',')[[1]]))})
full_concept_id_list = sort(unique(unlist(list_of_concept_id_per_person))) 

concept_id_and_person_arr_df = NULL

concept_id_and_person_arr_df$concept_id = full_concept_id_list
concept_id_and_person_arr_df$person_id_arr = unlist(lapply(full_concept_id_list,function(x){get_all_person_id_in_a_str(x)})) 
concept_id_and_person_arr_df = as.data.frame(concept_id_and_person_arr_df)
saveRDS(concept_id_and_person_arr_df,file = "./concept_id_and_person_arr_df.rds")

############## count the number of persons for each concept_id
count_n_person_id_for_each_concept_id <-function(){
	concept_id_and_person_arr_df = readRDS("concept_id_and_person_arr_df.rds")
	p_list = lapply(concept_id_and_person_arr_df$person_id_arr, function(x) {as.numeric(as.vector(str_split(x,',')[[1]]))})
	count_vector = unlist(lapply(p_list,function(x){length(x)}))
	c_id = concept_id_and_person_arr_df$concept_id

	concept_id_person_count = NULL
	concept_id_person_count$concept_id = c_id
	concept_id_person_count$count = count_vector
	concept_id_person_count = as.data.frame(concept_id_person_count)

	concept_synonym_OMOP_df = readRDS("../concept_synonym_OMOP_table.rds")
	concept_OMOP_df = readRDS("../concept_OMOP_table.rds")

	concept_and_person_count_df = merge(concept_id_person_count,concept_OMOP_df,by='concept_id')
	
	concept_and_person_count_ordered_df = concept_and_person_count_df[order(-concept_and_person_count_df$count),]
	saveRDS(concept_and_person_count_ordered_df,file="./concept_and_person_count_ordered_df.rds")
	fwrite(concept_and_person_count_ordered_df[,c(1,3,8,2)],file="./concept_and_person_count_ordered_short_df.tsv",sep="\t")
	
	concept_and_synonym_person_count_df = merge(concept_and_person_count_df,concept_synonym_OMOP_df,
															by='concept_id')
	concept_and_synonym_person_count_ordered_df = concept_and_synonym_person_count_df[
														order(-concept_and_synonym_person_count_df$count),]

	saveRDS(concept_and_synonym_person_count_ordered_df,file="./concept_and_synonym_person_count_ordered_df.rds")
	fwrite(concept_and_synonym_person_count_ordered_df[,c(1,3,12,8,2)],
							file="./concept_and_synonym_person_count_ordered_short_df.tsv",sep="\t")
	return(concept_and_synonym_person_count_ordered_df)	

}

concept_and_synonym_person_count_ordered_df = count_n_person_id_for_each_concept_id()


##############
 
								
