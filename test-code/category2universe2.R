category2 <- function(name, buckets, targets, sum.1 = FALSE) {
    
    # check to make sure targets sum to 1
    if (sum(targets) != 1 & !sum.1) {
        stop("Input to `targets` must sum to 1. Check proportions used, or force them to sum to 1 by setting sum.1 = TRUE.")
    }
    
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
    

    # up front error checks ---------------------------------------------------

    # make sure data frame is supplied
    if (!is.data.frame(data)) {
        stop("Input to `data` must be a data frame.")
    }
    
    # list object of all unspecified arguments passed to function
    categories <- list(...)
    
    # make sure at least one category was provided
    if (length(categories) < 1) {
        stop("Provide at least one weighting category, use `category()` to construct this.")
    }
    
    # are all inputs to this function category class?
    if (!(all(map_lgl(categories, function(x) "category" %in% class(x))))) {
        stop("Each input to `...` must be of the class 'category'. Use `category()` to construct this input.")
    } 

    # make sure N is good
    if (!missing(N)) {
        if (N <= nrow(data)) {
            stop(glue(
                "
Input to `N` must be a single numeric value larger than the size of your sample ({nrow(data)}).
                "
            ))
        }
    }
    
    # would this work best for doing the expansion factor calculation?
    if (missing(N)) {
        N <- 1
    }
    
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
        
        # vector class compatibility
        df.class  <- class(df.unique[[i]])
        wgt.class <- class(wgt.buckets[[i]])
        
        classes.match <- df.class == wgt.class
        
        if (!classes.match) {
            stop(glue(
                "
Mismatch in variable classes for '{wgt.cats[[i]]}' weighting category.
---------------------------------------------------------------------------
        Class in data: {df.class}
Class in `category()`: {wgt.class}
---------------------------------------------------------------------------
Please reconcile this difference before proceeding.
                "
            ))
        }
        
        # do not allow NA's in bucket
        if (any(is.na(wgt.buckets[[i]]))) {
            stop(glue("
`NA` is not a valid bucket, please review input to `category()` for '{wgt.cats[[i]]}' weighting category.
                 "
            ))
        }
        
        
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
---------------------------------------------------------------------------
        Unique in data: {glue_collapse(sort(df.unique[[i]]), sep = ', ')}
Unique in `category()`: {glue_collapse(sort(wgt.buckets[[i]]), sep = ', ')}
---------------------------------------------------------------------------
Please reconcile this difference before proceeding.
*Note: Missing values (NA) in the data are acceptable.
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
    
    # This is used to print out the weighting categories which were adjusted
    # for missing values
    wgt.cats.adj <- character(0)

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
            
            p.na <- 
                all.prop %>%
                filter(is.na({{ wc }})) %>%
                pull(act_prop)
            
            # collect category name for printing
            wgt.cats.adj <- c(
                wgt.cats.adj, 
                glue("{wgt.cats[[i]]} ({scales::percent(p.na, accuracy = 0.1)})")
            )
            
            
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
            add_column(category = wgt.cats[[i]], .before = 1)
        
    }
    
    warning(glue(
        "
Missing data was found in the following categories:
---------------------------------------------------------------------------
{glue_collapse(wgt.cats.adj, sep = '\n')}
---------------------------------------------------------------------------
Target proportions have been reproportioned to account for missing data.
        "
    ))
    

    # final return ------------------------------------------------------------

    names(universe) <- wgt.cats
    
    out <- list(
        data = data,
        universe = universe,
        N = N
    )
    
    class(out) <- c(class(universe), "universe")
    
    return(out)
    
}




