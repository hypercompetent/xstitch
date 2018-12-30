library(xstitch)
library(dplyr)
options(stringsAsFactors = FALSE)
array_dims <- c(67, 41, 58)

ont <- aba_get_ontology() %>%
  aba_flatten_ontology()

target_level <- 6

res <- data.frame(id = numeric(length = nrow(ont)),
                  merge_id = numeric(length = nrow(ont)),
                  acronym = character(length = nrow(ont)),
                  name = character(length = nrow(ont)),
                  st_level = numeric(length = nrow(ont)))

for(i in 1:nrow(ont)) {
  if(ont$st_level[i] > target_level) {
    parent_id <- ont$parent_structure_id[i]
    parent_st_level <- ont$st_level[ont$id == parent_id]

    while(parent_st_level > target_level) {
      parent_id <- ont$parent_structure_id[ont$id == parent_id]
      parent_st_level <- ont$st_level[ont$id == parent_id]
    }

    final_vals <- ont[ont$id == parent_id,,drop=FALSE]
  } else {
    final_vals <- ont[i,,drop = FALSE]
  }

  res$id[i] <- ont$id[i]
  res$merge_id[i] <- final_vals$id
  res$acronym[i] <- final_vals$acronym
  res$name[i] <- final_vals$name
  res$st_level[i] <- final_vals$st_level

}

merged_ont <- res %>%
  select(-id) %>%
  rename(id = merge_id) %>%
  unique()

anno_arr <- aba_get_annotation_array()
merged_arr <- array(res$merge_id[match(anno_arr, res$id)], dim = array_dims)

saveRDS(merged_arr,"inst/shiny-examples/aba/aba_merged_annotation_id_array.RData")
saveRDS(merged_ont,"inst/shiny-examples/aba/aba_merged_annotation_df.RData")
