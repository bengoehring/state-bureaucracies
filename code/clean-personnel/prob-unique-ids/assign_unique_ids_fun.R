# This function probabalistically assigns unique identifiers to state employees
#   using fastLink::fastLink(). It is meant to be run on the cluster. 
options(warn=1)
link_by_block <- function(dataframe, # original dataframe to dedup
                          blocking_object, #blocking object from fastlink::blockData()
                          block_i, # block number, character (e.g. 'block.1')
                          blocking_variable, # name of variable that blocking is happening on
                          vars_to_join, # all variables to join on
                          vars_to_string_dist_match, #variables to string distance match on
                          blocking = TRUE,# allows function to also dedupe if not blocking
                          n_cores = 36) 
{
  if(blocking){
    # for each block, get the actual name of the blocking variable value
    indices <- blocking_object[[block_i]][[1]]
    
    block_real_name <- pull(unique(dataframe[indices, blocking_variable]))
    
    cat("-----------------\n")
    cat("Calculating block: ", block_real_name, "\n")
    cat("-----------------\n")
    
    if(length(block_real_name) != 1) {
      stop('The blocking variable value is displaying as more than 1.')
    }
    
    # run fastlink on blocked data and get matches
    blocked_data <- dataframe[indices,]
    
    fl_out <- fastLink(
      dfA = blocked_data, 
      dfB = blocked_data,
      varnames = vars_to_join,
      stringdist.match = vars_to_string_dist_match,
      partial.match = vars_to_string_dist_match,
      threshold.match = .90,
      n.cores = n_cores)
    
    matches_out <- getMatches(
      dfA = blocked_data, 
      dfB = blocked_data,
      fl.out = fl_out,
      threshold.match = .90
    )
    
    # add block name to create inter-block unique id
    matches_out <- matches_out %>% 
      mutate(dedupe.ids = str_c(block_real_name,
                                "_",
                                dedupe.ids))
    
    # save fl object and dataframe, for the block
    fl_out_name <- str_c(block_real_name, 
                         "_", 
                         block_i, 
                         "_", 
                         "fl_out")
    dedupe_df_name <- str_c(block_real_name, 
                            "_", 
                            block_i, 
                            "_", 
                            "dataframe")
    
    all_out <- list(fl_out_name = fl_out, 
                    dedupe_df_name = matches_out)
    
    return(all_out)
  } else {
    # not blocking 
    fl_out <- fastLink(
      dfA = dataframe, 
      dfB = dataframe,
      varnames = vars_to_join,
      stringdist.match = vars_to_string_dist_match,
      partial.match = vars_to_string_dist_match,
      threshold.match = .90,
      n.cores = n_cores)
    
    matches_out <- getMatches(
      dfA = dataframe, 
      dfB = dataframe,
      fl.out = fl_out,
      threshold.match = .90
    )
    
    all_out <- list(fl_out, 
                    matches_out)
    
    return(all_out)
  }
}




