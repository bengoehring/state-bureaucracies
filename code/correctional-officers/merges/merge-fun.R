# This function is set up to merge voter data with personnel data. 
options(warn=1)

merge_by_block <- function(personnel_data,
                           voter_data,
                           blocking_object, #blocking object from fastlink::blockData()
                           block_i, # block number, character (e.g. 'block.1')
                           vars_to_join, # all variables to join on
                           vars_to_string_dist_match, #variables to string distance match on
                           blocking = TRUE,
                           n_cores = 36,
                           threshold_level) 
{
  if(blocking){
    cat("-----------------\n")
    cat("Calculating: ", block_i, "\n")
    cat("-----------------\n")
    
    # run fastlink on blocked data and get matches
    blocked_personnel_data <- personnel_data[blocking_object[[block_i]][['dfA.inds']], ]
    blocked_voter_data <- voter_data[blocking_object[[block_i]][['dfB.inds']], ]
    
    fl_out <- fastLink(
      dfA = blocked_personnel_data, 
      dfB = blocked_voter_data,
      varnames = vars_to_join,
      stringdist.match = vars_to_string_dist_match,
      partial.match = vars_to_string_dist_match,
      threshold.match = threshold_level,
      n.cores = n_cores)
    
    matches_out <- getMatches(
      dfA = blocked_personnel_data, 
      dfB = blocked_voter_data,
      fl.out = fl_out,
      threshold.match = threshold_level
    )
    
    # save fl object and dataframe, for the block
    fl_out_name <- str_c(block_i, 
                         "_", 
                         "fl_out")
    dedupe_df_name <- str_c(block_i, 
                            "_", 
                            "dataframe")
    
    all_out <- list(fl_out, 
                    matches_out)
    names(all_out) <- c(fl_out_name, 
                        dedupe_df_name)
    
    return(all_out)
  } else {
    
    # not blocking 
    fl_out <- fastLink(
      dfA = personnel_data, 
      dfB = voter_data,
      varnames = vars_to_join,
      stringdist.match = vars_to_string_dist_match,
      partial.match = vars_to_string_dist_match,
      threshold.match = threshold_level,
      n.cores = n_cores)
    
    matches_out <- getMatches(
      dfA = personnel_data, 
      dfB = voter_data,
      fl.out = fl_out,
      threshold.match = threshold_level
    )
    
    all_out <- list(fl_out, 
                    matches_out)
    
    return(all_out)
  }
}




