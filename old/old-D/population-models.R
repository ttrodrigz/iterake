# We need to get the targets from the population model from the user in a
# smart and clean way.

# Proposed methods:

# 1) A function that makes a named list containing a dataframe with the
#    target proportion and corresponding values. This function is used
#    as many times as needed and goes inside a function that wraps them
#    all up and stick them in a nested dataframe.

wgt_cat <- function(name, value, targ_prop) {
    
    if (!is.character(name) || length(name) != 1) {
        stop("name must be a character vector with one element.")
    }
    
    if (length(value) != length(targ_prop)) {
        stop("Must provide equal number of values and target proportions.")
    }
    
    if (name == "") {
        stop("name must be an non-empty character string.")
    }
    
    if (sum(targ_prop) != 1) {
        stop("Target proportions must sum to 1.")
    }
    
    out <- list(tibble(wgt_cat = name,
                       value = value,
                       targ_prop = targ_prop))
    
    names(out) <- name
    return(out)
    
}

pop_model <- function(...) {
    
    wgt_cats <- list(...)
    wgt_cats <- flatten(wgt_cats)
    
    wgt_cats %>%
        bind_rows() %>%
        group_by(wgt_cat) %>%
        nest() %>%
        arrange(wgt_cat)
    
}

pop_model(
    
    # age category
    wgt_cat(name = "age",
            value = c("18-34", "35-54", "55+"),
            targ_prop = c(0.320, 0.350, 0.330)),
    
    # gender category
    wgt_cat(name = "gender",
            value = c("Female", "Male"),
            targ_prop = c(0.54, 0.46)),
    
    # vehicle category
    wgt_cat(name = "vehicle",
            value = c("Car", "SUV", "Truck"),
            targ_prop = c(0.380, 0.470, 0.150))
    
)

# 2) Excel based version of this, first outputs an Excel file where each
#    tab corresponds to a weighting category, each tab contains a header
#    with taget proportion and value. The user fills in the population
#    model and then feeds it back into R, puts it into a nested dataframe.

pop_model_xl_template <- function(cat_names, path) {
    
    n_cats <- length(cat_names)
    
    if (length(n_cats) < 1 || !is.character(cat_names)) {
        stop("cat_names must be a character vector of one more elements corresponding to the weighting categories.")
    }
    
    check_empty <- map_lgl(cat_names, ~identical(., ""))
    
    if (any(check_empty) == TRUE) {
        stop("cat_names must be non-empty character strings.")
    }
    
    base_df <- function() {
        tibble(
            value = integer(),
            targ_prop = double()
        )
    }
    
    to_write <- replicate(
        n = n_cats, 
        expr = base_df(), 
        simplify = FALSE
    )
    
    names(to_write) <- cat_names
    
    writexl::write_xlsx(x = to_write, path = path)
    
}

pop_model_xl <- function(path) {
    
    wgt_cats <- path %>%
        readxl::excel_sheets() %>%
        set_names() %>%
        map(readxl::read_excel, path = path)
    
    for (i in seq_along(wgt_cats)) {
        
        wgt_cats[[i]]$wgt_cat <- names(wgt_cats[i])
    }
    
    out <- wgt_cats %>%
        bind_rows() %>%
        select(wgt_cat, value, targ_prop) %>%
        group_by(wgt_cat) %>%
        nest() %>%
        arrange(wgt_cat)
    
    checks <- out %>%
        mutate(na = map_int(data, ~sum(is.na(.))),
               sum_1 = map_dbl(data, ~sum(.$targ_prop, na.rm = TRUE)))
    
    if (sum(checks$na) > 0) {
        stop("Missing values in Excel file indicate unequal number of values and target proportions.")
    }
    
    if (any(checks$sum_1 < 1)) {
        stop("Target proportions must sum to 1.")
    }
    
    out
    
}

pop_model_xl_template(cat_names = c("age", "gender", "vehicle"), 
                      path = "./output/test.xlsx")

# **** FILL OUT EXCEL FILE **** #

pop_model_xl("./output/test.xlsx")