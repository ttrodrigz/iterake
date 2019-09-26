#' Create universe
#' 
#' Along with the data to be weighted, this combines one or more objects of 
#' class \code{category} to build the universe with known marginal proportions. 
#' It also checks and adjusts the proportions to account for missing values in the data.
#' 
#' @param data Data frame containing data where weights are desired.
#' @param ... One or more output objects from \code{category()}.
#' @param N Size of universe. If supplied, expansion factor will be applied
#' to weights after convergence.
#' 
#' @importFrom glue glue glue_collapse
#' @importFrom purrr map map_lgl map_chr pluck
#' @importFrom dplyr sym pull mutate select filter count full_join case_when rename %>%
#' @importFrom scales percent
#' @importFrom tibble tibble add_column
#' 
#' @return A \code{list} with special class \code{universe}.
#' 
#' @examples 
#' data(demo_data)
#' 
#' universe(
#'     data = demo_data,
#'     
#'     category(
#'         name = "Sex",
#'         buckets = factor(
#'             x = levels(demo_data[["Sex"]]),
#'             levels = levels(demo_data[["Sex"]])
#'         ),
#'         targets = c(0.4, 0.5),
#'         sum.1 = TRUE
#'     ),
#' 
#'     category(
#'         name = "BirthYear",
#'         buckets = c(1986:1990),
#'         targets = rep(0.2, times = 5)
#'     ),
#'     
#'     category(
#'         name = "EyeColor",
#'         buckets = c("brown", "green", "blue"),
#'         targets = c(0.8, 0.1, 0.1)
#'     ),
#'     
#'     category(
#'         name = "HomeOwner",
#'         buckets = c(TRUE, FALSE),
#'         targets = c(3/4, 1/4)
#'     )
#' )
#' 
#' @export
universe <- function(data, ..., N) {
    
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
    } else {
        # would this work best for doing the expansion factor calculation?
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
        
        # there will be some coercion happening further down the line
        # these are scenarios that should skip the stop bit here
        if (!classes.match) {

            if (df.class %in% c("numeric", "integer") & wgt.class %in% c("numeric", "integer")) {
                classes.match <- TRUE
            }
            
            if (df.class %in% c("factor", "character") & wgt.class %in% c("factor", "character")) {
                classes.match <- TRUE
            }
            
        }
        
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
            stop(glue(
"
`NA` is not a valid bucket, please review input to `category()` for '{wgt.cats[[i]]}' weighting category.
"
            ))
        }
        
        # opted for `all.equal()` over `identical()` because `identical()`
        # got hung up on mismatches between numeric/integer

        buckets.match <- isTRUE(all.equal(
            
            # note that sort() drops NA's by default
            # as.character eliminates factor level/character sort issues 
            target  = as.character(df.unique[[i]]) %>% sort(),
            current = as.character(wgt.buckets[[i]]) %>% sort()
            
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
    # 3. Variable coercion for numeric/integer and factor/character
    # 4. Join actual proportions to target full_join(), this ensures that 
    #    correct ordering is maintained, and identifies proportions of NA's 
    #    in the data.
    # 5. Check to see if NA's exist in the data, if so, find proportion.
    # 6. If NA's exist, adjust the targets by target * (1 - prop NA)
    
    # Output from this process will be stored in a new list
    universe <- list()
    
    # This is used to print out the weighting categories which were adjusted
    # for missing values
    wgt.cats.adj <- character(0)
    
    for (i in 1:num.cats) {
        
        # 1. Find actual proportions in the data
        wc <- sym(wgt.cats[[i]])
        
        # remove_labels helps classes match for the full_join
        act.prop <-
            data %>%
            count({{ wc }}) %>%
            mutate(act_prop = n / sum(n)) %>%
            select(-n) %>%
            remove_labels()
        
        # 2. Data frame of targets proportions
        targ.prop <- tibble(
            {{ wc }} := categories[[i]][["buckets"]],
            targ_prop = categories[[i]][["targ_prop"]]
        )
        
        # 3. Check for differing classes - coerce some scenarios if needed for later full_join
        class.act <- class(act.prop[[wgt.cats[i]]])
        class.tar <- class(targ.prop[[wgt.cats[i]]])
        
        if (class.act != class.tar) {
            
            # force target class to match actual/data class - numeric/integer
            if (class.act %in% c("numeric", "integer") & class.tar %in% c("numeric", "integer")) {
            
                if (class.act == "numeric") {
                    
                    targ.prop[[wgt.cats[i]]] <- as.numeric(targ.prop[[wgt.cats[i]]])
                    
                } else if (class.act == "integer") {
                    
                    targ.prop[[wgt.cats[i]]] <- as.integer(targ.prop[[wgt.cats[i]]])
                }

            # force target class to match actual/data class - factor/character
            } else if (class.act %in% c("factor", "character") & class.tar %in% c("factor", "character")) {
             
                if (class.act == "factor") {
                    
                    targ.prop[[wgt.cats[i]]] <- factor(
                        x = targ.prop[[wgt.cats[i]]], 
                        levels = levels(act.prop[[wgt.cats[i]]])
                    )
                    
                } else if (class.act == "character") {
                    
                    targ.prop[[wgt.cats[i]]] <- as.character(targ.prop[[wgt.cats[i]]])
                    
                }
                
            }
            
        }

        # 4. Join target to actual
        all.prop <- full_join(
            targ.prop,
            act.prop,
            by = wgt.cats[[i]]
        )
        
        # 5. Find proportion missing
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
            
            # 6. Adjust target proportions to account for p NA
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
    
    if (length(wgt.cats.adj) > 0) {
        warning(glue(
"
Missing data was found in the following categories:
---------------------------------------------------------------------------
{glue_collapse(wgt.cats.adj, sep = '\n')}
---------------------------------------------------------------------------
Target proportions have been reproportioned to account for missing data.
"
        ))
    }
    
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


utils::globalVariables(c("category", "n"))    