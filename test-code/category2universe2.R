category2 <- function(name, buckets, targets, sum.1 = FALSE) {
    
    # adjust if specified
    if (sum.1) {
        targets <- targets / sum(targets)
    }
    
    # list structure
    out <- list(
        category = name,
        buckets = buckets,
        targ_prop = targets
    )

    # assign class
    class(out) <- c(class(out), "category")
    
    return(out)
    
    
}

universe2 <- function(data, ..., N) {
    
    categories <- list(...)
    
    # verify categories specified exist in the data ---------------------------

    # Simply check to make sure that the user is not creating categories which
    # don't exist in the data to be weighted.
    
    df.names <- names(data)
    wgt.cats <- map_chr(categories, pluck, "category")
    bad.cats <- wgt.cats[!wgt.cats %in% df.names]
    
    if (length(bad.cats) > 0) {
        
        stop.message <- glue(
            "
Each name given to a weighting category in `universe()` must have a matching column name in `data`. The following weighting categories have no match:
            ",
            glue_collapse(bad.cats, sep = ", "),
            .sep = "\n"
        )
        
        stop(stop.message)
        
    }
    

    # check that all buckets exist in the data --------------------------------

    # This will check to make sure that the buckets provided are also in the
    # data. One thing to consider here is that there may be missing data in
    # `data`, so when running the checks, we need to drop missing values before
    # the comparison takes place. The model will be ajusted for NA's in a
    # later step.
    
    # this may come in handy later...
    drop_na_vec <- function(x) x[!is.na(x)]

    num.cats    <- length(wgt.cats)
    df.unique   <- map(data[wgt.cats], unique)
    wgt.buckets <- map(categories, pluck, "buckets")
    
    for (i in 1:num.cats) {
        
        # vector class compatibility check here?
        
        # opted for `all.equal()` over `identical()` because `identical()`
        # got hung up on mismatches between numeric/integer
        
        buckets.match <- isTRUE(all.equal(
            
            # note that sort() drops NA's by default
            target  = df.unique[[i]] %>% sort(),
            current = wgt.buckets[[i]] %>% sort()
            
        ))
        
        if (!buckets.match) {
            
            stop.message <- glue(
                "
There are mismatches between the buckets provided, and the unique values of `data` for the '{wgt.cats[[i]]}' weighting category. 
                "
            )
            
            stop(stop.message)
        }
        
    }
    

    # adjust the population model ---------------------------------------------

    # This works by doing the following steps:
    # 1. Find the actual proportions in the data.
    # 2. Create a data frame of the target proportions.
    # 3. Join actual proportions to target full_join(), this ensures that 
    #    correct ordering is maintained, and identifies proportions of NA's 
    #    in the data.
    # 4. Check to see if NA's exist in the data, if so, find proportion.
    # 5. If NA's exist, adjust the targets by target * (1 - prop NA)
    
    # Output from this process will be stored in a new list
    universe <- list()

    for (i in 1:num.cats) {
        
        # 1. Find actual proportions in the data
        wc <- sym(wgt.cats[[i]])
        
        act.prop <-
            data %>%
            count({{ wc }}) %>%
            mutate(act_prop = n / sum(n)) %>%
            select(-n)
        
        # 2. Data frame of targets proportions
        targ.prop <- tibble(
            {{ wc }} := categories[[i]][["buckets"]],
            targ_prop = categories[[i]][["targ_prop"]]
        )
        
        # 3. Join target to actual
        all.prop <- full_join(
            targ.prop,
            act.prop,
            by = wgt.cats[[i]]
        )
        
        # 4. Find proportion missing
        if (any(is.na(all.prop))) {
            
            cat(glue(
                "Missing values were found in {wgt.cats[[i]]}, target proportions are being adjusted.\n"
            ))
            
            p.na <- 
                all.prop %>%
                filter(is.na({{ wc }})) %>%
                pull(act_prop)
            
            # 5. Adjust target proportions to account for p NA
            all.prop <-
                all.prop %>%
                mutate(targ_prop = case_when(
                    is.na(targ_prop) ~ p.na,
                    TRUE ~ targ_prop * (1 - p.na)
                ))
            
        }
        
        # Add finalized data frame to the output list
        universe[[i]] <-
            all.prop %>%
            rename("bucket" = 1) %>%
            add_column(wgt_cat = wgt.cats[[i]], .before = 1)
        
    }
    
    names(universe) <- wgt.cats
    class(universe) <- "universe"
    
    return(universe)
    
}




