wgt_stats <- function(weight) {
    
    # one col df's and numeric vectors are permissible
    if (!is.numeric(weight) & !is.data.frame(weight)) {
        
        stop("Input to `weight` must be a data frame with one column or a numeric vector.")
        
    }
    
    # # if one col df, turn into vector
    if (is.data.frame(weight)) {

        if (ncol(weight) == 1) {
            
            weight <- dplyr::pull(weight)
            
            # after vector-ing, make sure numeric
            if (!is.numeric(weight)) {
                
                stop("Column in provided data frame must be numeric.")
                
            }

        } else {

            stop("Input to `weight` must be a data frame with one column or a numeric vector.")

        }

    }
    
    # calculate stats
    uwgt_n <- length(weight)
    wgt_n  <- sum(weight)
    eff_n  <- (sum(weight) ^ 2) / sum(weight ^ 2)
    loss   <- (uwgt_n / eff_n) - 1
    efficiency <- (eff_n / uwgt_n)
    
    # output
    tibble::tibble(
        uwgt_n = uwgt_n,
        wgt_n  = wgt_n,
        eff_n  = eff_n,
        loss   = loss,
        efficiency = efficiency
    )
    
}

# testing
df <- readRDS("./data-for-testing/test_data.rds")

mod <- wgt_design(
    
    df = df,
    
    # age category
    wgt_cat(name = "age",
            buckets = c("18-34", "35-54", "55+"),
            targets = c(0.300, 0.360, 0.340)),
    
    # gender category
    wgt_cat(name = "gender",
            buckets = c("Female", "Male"),
            targets = c(0.500, 0.500)),
    
    # vehicle category
    wgt_cat(name = "vehicle",
            buckets = c("Car", "SUV", "Truck"),
            targets = c(0.400, 0.450, 0.150))
    
)

wtd <- iterake(df, id, mod)

# numeric vector
wgt_stats(wtd$weight)

# non-numeric vector
wgt_stats(wtd$age)

# multi-col df
wgt_stats(wtd)

# numeric one-col df
wgt_stats(dplyr::select(wtd, weight))

# non-numeric one col df
wgt_stats(dplyr::select(wtd, age))
