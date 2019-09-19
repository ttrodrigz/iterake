# first function does the core weighting presented in a for loop
# one pass of this will equal one set of tabs in devon's excel sheet
one_pass <- function(data, design, wgt.lim = 3) {
    
    # define these out of loop, only have to set them once
    wgt_names <- design[["wgt_cat"]]
    data$wgt <- 1
    N <- nrow(data)

    # this will pass through each of the weight categories once
    for (i in seq_along(wgt_names)) {
        
        # data.table versions of:
        # - data being weighted, i'th weighting column as key
        # - i'th dataset (targets) of design
        DT_data   <- data.table(data, key = wgt_names[i])
        DT_design <- data.table(design[["data"]][[i]])
        
        # start with original data.table-ized object
        DT_merge <- 
            
            # original everything is getting merged to
            DT_data[
                
                # DT object to be merged with the original
                # ideal becuase it uses the same key for merging
                DT_data[, 
                        # calcs the "haves" by the i'th weighting category
                        .(act_prop = sum(wgt) / N),
                        by = c(wgt_names[i])
                        
                        # merge haves with wants
                        ][DT_design
                          
                          ][, 
                            # calculate wants over haves by the i'th weighting category
                            .(wgt_temp = ifelse(act_prop == 0, 0, targ_prop / act_prop)), 
                            by = c(wgt_names[i])
                            ]
                ]
        
        # creates the weight factor, max out at the weight limit
        data <- DT_merge[, wgt := ifelse(wgt * wgt_temp > wgt.lim, wgt.lim, wgt * wgt_temp)]
        
        # temporary weight no longer needed
        data[, wgt_temp := NULL]
        
    }
    
    data
}


one_pass(df, design = design)
