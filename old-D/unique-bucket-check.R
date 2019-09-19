design_good <- 
    tibble(
        wgt_cat = c("gender", "age"),
        data = list(
            tibble(buckets = c(1, 2),
                   targets = c(0.5, 0.5)),
            tibble(buckets = c("<40", "40+"),
                   targets = c(0.3, 0.7))
        )
    )


df_good <-
    tibble(id = 1:50,
           age    = sample(c("<40", "40+"),     50, TRUE, c(0.35, 0.65)),
           gender = sample(c(1, 2), 50, TRUE, c(0.46, 0.54))
    )


char_conv <- function(df) {
    
    mutate(df, buckets = as.character(buckets))
}

des <- design_good %>%
    mutate(data = map(data, char_conv)) %>%
    mutate(unique = map(data, ~unique(.x$buckets))) %>%
    unnest(unique) %>%
    split(.$wgt_cat) %>%
    lapply(pull, unique) %>%
    lapply(sort)

dat <- df_good %>%
    select(one_of(design_good$wgt_cat)) %>%
    map_df(as.character) %>%
    gather(wgt_cat, unique) %>%
    group_by(wgt_cat, unique) %>%
    distinct() %>%
    split(.$wgt_cat) %>%
    lapply(pull, unique) %>%
    lapply(sort)


check_unique <- function(df, design) {
    
    # step 1: get df and design into same shape
    unique.df <- 
        df %>%
        select(one_of(design_good$wgt_cat)) %>%
        map_df(as.character) %>%
        gather(wgt_cat, unique) %>%
        group_by(wgt_cat, unique) %>%
        distinct() %>%
        split(.$wgt_cat) %>%
        lapply(pull, unique) %>%
        lapply(sort)
    
    unique.design <- 
        design_good %>%
        mutate(data = map(data, char_conv)) %>%
        mutate(unique = map(data, ~unique(.x$buckets))) %>%
        unnest(unique) %>%
        split(.$wgt_cat) %>%
        lapply(pull, unique) %>%
        lapply(sort)
    
    # step 2: iterate thru lists, create df of pass/fails
    out <- tibble(wgt_cat = sort(design_good$wgt_cat),
                  passing = NA)
    
    for (i in 1:nrow(design)) {
        
        check.design <- length(unique.design[[i]][!unique.design[[i]] %in% unique.df[[i]]])
        check.data   <- length(unique.df[[i]][!unique.df[[i]] %in% unique.design[[i]]])
        
        if (sum(check.design, check.data) > 0) {
            out$passing[[i]] <- FALSE
        } else {
            out$passing[[i]] <- TRUE
        }
        
    }
    
    bad.cats <-
        out %>%
        filter(passing == FALSE)
    
    if (nrow(bad.cats > 0)) {
        stop(paste(
            "There are unused buckets in either the weighting categories or the corresponding data for the following variable(s):",
            paste(bad.cats %>% pull(wgt_cat), collapse = ", ")))
             
    }
    
    
}

check_unique(df_good, design_good)

out <- tibble(wgt_cat = sort(design_good$wgt_cat),
              passing = NA)

for (i in 1:length(des)) {
    
    check.design <- length(des[[i]][!des[[i]] %in% dat[[i]]])
    check.data   <- length(dat[[i]][!dat[[i]] %in% des[[i]]])
    
    if (sum(check.design, check.data) > 0) {
        out$passing[[i]] <- FALSE
    } else {
        out$passing[[i]] <- TRUE
    }
    
}
