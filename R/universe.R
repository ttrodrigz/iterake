#' Create universe
#' 
#' Along with the data to be weighted, this combines one or more objects of 
#' class \code{category} to build the universe with known marginal proportions. 
#' It also checks and adjusts the proportions to account for missing values in the data.
#' 
#' @param df Data frame containing data where weights are desired.
#' @param ... One or more output objects from \code{category()}.
#' @param N Size of universe. If supplied, expansion factor will be applied
#' to weights after convergence.
#' 
#' @importFrom data.table data.table melt
#' @importFrom purrr map_lgl pmap 
#' @importFrom dplyr anti_join bind_rows pull group_by_ summarise mutate select %>%
#' @importFrom crayon %+%
#' @importFrom tibble tibble
#' 
#' @return A nested \code{tibble} with special class \code{universe}.
#' 
#' @examples 
#' data(weight_me)
#' 
#' universe(
#'     df = weight_me,
#' 
#'     category(
#'         name = "costume",
#'         buckets = c("Bat Man", "Cactus"),
#'         targets = c(0.5, 0.5)),
#' 
#'     category(
#'         name = "seeds",
#'         buckets = c("Tornado", "Bird", "Earthquake"),
#'         targets = c(0.4, 0.3, 0.3))
#' )
#' 
#' @export
universe <- function(df, ..., N) {
    
    # make sure dataframe is supplied
    if (!is.data.frame(df)) {
        stop("Input to `df` must be a data frame.")
    }
    
    # list object of all unspecified arguments passed to function
    category <- list(...)
    
    # make sure at least one category was provided
    if (length(category) < 1) {
        stop("Provide at least one weighting category, use `category()` to construct this.")
    }
    
    # are all inputs to this function category class?
    if (!(all(map_lgl(category, function(x) "category" %in% class(x))))) {
        stop("Each input to `universe()` must be of the class 'category'. Use `category()` to construct this input.")
    } 
    
    # make sure N is good
    if (!missing(N)) {
        if (any(
            N <=0,
            !is.numeric(N),
            N <= nrow(df) 
        )) {
            stop(paste0("Input to `N` must be a single numeric value larger than the size of your sample (",
                        nrow(df),
                        ")."))
        }
    }
        
    # smush 'em together into final form
    out <- bind_rows(category)
    
    # make sure each category in universe has a matching column in df
    df.names  <- names(df)
    
    # category here refers to the variable created in category that identifies a target weighting variable
    wgt.cats <- pull(out, category)
    bad.cats <- wgt.cats[!wgt.cats %in% df.names]
    
    if (length(bad.cats) > 0) {
        stop(
            paste(
                "Each name given to a weighting category in `universe` must have a matching column name in `df`. The following weighting categories have no match:",
                paste(bad.cats, collapse = ", "),
                sep = "\n"
            )
        )
    }
    
    # create function for matching category attributes to data source
    inherit_chr_fct <- function(to, from) {
        
        # to = character, from = factor
        if ((is.character(to) | is.factor(to)) & is.factor(from)) {
            
            to <- factor(x = to, 
                         levels = levels(from))
            
            if (is.ordered(from)) {
                
                to <- factor(to, ordered = TRUE)
                
            }
        }
        
        # to = factor, from = character
        if (is.factor(to) & is.character(from)) {
            
            to <- as.character(to)
        }
        
        to
        
    }

    # check that all buckets exist in data
    missing_buckets <-
        anti_join(
            
            out %>% 
                unnest(data) %>%
                select(-targ_prop),
            
            # using data.table-friendly approaches for speed in case df is large
            unique(
                melt(
                    data.table(df, keep.rownames = TRUE), 
                    id.vars = "rn", 
                    variable.name = "category",
                    variable.factor = FALSE,
                    value.name = "buckets", 
                    measure.vars = c(out$category))[, rn := NULL]),
            
            # the dplyr way
            # weight_me %>%
            #     select(seeds, costume) %>%
            #     gather(category, buckets) %>%
            #     group_by(category, buckets) %>%
            #     filter(row_number() == 1),
            
            by = c("category", "buckets")
            
    )
    
    # check if any rows exist - if so, stop as it means buckets in universe don't
    # exist in the data - and that's bad... mmmkay
    if (nrow(missing_buckets) > 0) {
        stop(
            paste0(
                "Not all buckets in `universe` categories are found in `df`. Edit `category()` calls to remove the following:",
                "\n\n",
                paste(missing_buckets$buckets, "not found in", missing_buckets$category, collapse = "\n")
            )
        )
    }
    
    # adjust targets for any NA in category and adjust attributes as needed
    adjusted_model <- pmap(out, function(..., main_data = df) {
        
        ## create list object out of all unspecified arguments passed from pmap
        ## - this is basically the row being evaluated
        inputList <- list(...)
        
        # get actual proportions to determine existance of NA
        act_props <- 
            main_data %>% 
            group_by_(inputList$category) %>%
            summarise(n = n()) %>%
            mutate(act_prop = n / sum(n)) %>%
            select(-n)
        
        # figure out what row of above has NA if any
        na_val_a <- which(is.na(act_props[1]))
        # determine if targets already have NA bin
        na_val_t <- which(is.na(inputList$data[1]))
        
        # if NA exists in actuals but not in targets - do stuff
        if (length(na_val_a) == 1 & length(na_val_t) == 0) {
            
            # an attempt at notifying when a change happens due to NAs...
            cat('NAs found in ' %+% paste0(inputList$category) %+% '; adjusting targets...\n')
            
            # determine proportion of NAs
            na_prop <- act_props[na_val_a, ]$act_prop
            
            # adjust current targets by a factor of 1 - na_prop
            new_targets <- 
                inputList$data %>%
                mutate(targ_prop = targ_prop * (1 - na_prop))
            
            # insert a new row with the NA info
            new_targets[na_val_a, ] <- c(NA, na_prop)
            # replace existing target model
            inputList$data <- new_targets
        }
        
        # reassign attribute types to match supplied data
        inputList$data$buckets <- inherit_chr_fct(inputList$data$buckets, act_props[[inputList$category]])
        
        # recreate tibble similar to how it's put together in category()
        tibble(
            category = inputList$category,
            data = list(
                tibble(
                    buckets = inputList$data$buckets,
                    targ_prop = inputList$data$targ_prop)))
        
    }) %>%
        
        # bind it all together
        bind_rows()
    
    if (!missing(N)) {
        
        # calc target N's, used later to do expansion factor calcs
        adjusted_model <-
            adjusted_model %>%
            mutate(data = map(data, function(x)
                x %>% mutate(targ_n = targ_prop * N)))
    }
    # assign class
    class(adjusted_model) <- c(class(adjusted_model), "universe")
    
    return(adjusted_model)
    
}


utils::globalVariables(c("category"))    