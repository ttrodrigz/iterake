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

    num.cats <- length(wgt.cats)
    df.unique <- data[wgt.cats] %>% map(unique)
    wgt.buckets <- categories %>% map(pluck, "buckets")
    
    for (i in 1:num.cats) {
        
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
    # 1. Determine if the unique values in the column of the weighting category
    #    contain missing values
    # 2. If they do, find the proportion of values missing in the data
    # 3. Adjust the targets by target * (1 - prop missing)
    # 4. Add NA to the targets with the value of prop missing from the data
    # 5. Add NA to the buckets
    
    wgt.targets <- map(categories, pluck, "targ_prop")
    
    for (i in 1:num.cats) {
        
        # 1. Do NA's exist?
        if (any(is.na(df.unique[[i]]))) {
            
            # 2. Find proportion missing in data
            wc <- sym(wgt.cats[[i]])
            
            p.na <-
                data %>%
                count({{ wc }}) %>%
                mutate(p = n / sum(n)) %>%
                filter(is.na({{wc}})) %>%
                pull(p)
            
            # 3/4. Re-proportion the targets
            wgt.targets[[i]] <- c(
                wgt.targets[[i]] * (1 - p.na),
                p.na
            )
            
            # 5. Add NA to buckets
            wgt.buckets[[i]] <- c(
                wgt.buckets[[i]],
                NA
            )
            
        }
        
        
    }
    

}

