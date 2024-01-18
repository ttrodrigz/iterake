#' Create Data Frame of Variable or Value Labels.
#'
#' This enables you to inspect labels at the variable or value label.
#' If requesting value labels for a \code{var}, the default is to also
#' tabulate the frequencies and percents. Can provide an optional column
#' of weights to calculated weighted frequencies and percentages.
#'
#' @param data A data frame where variable or value labels are requested.
#' @param var If value labels are requested, provide corresponding column (unquoted) in the data frame.
#' @param freq Logical, whether or not to tabulate frequencies for the value labels.
#' @param weight An unquoted column of weights.
#'
#' @return A data frame of variable or value labels.
#'
#' @examples
#' library(dplyr)
#' library(labelled)
#' library(iterake)
#'
#' df <-
#'   data.frame(
#'     x = sample(c(1, 2), 100, replace = TRUE),
#'     w = rnorm(100, 1, 0.1)
#'   ) %>%
#'   set_variable_labels(
#'     x = "Gender",
#'     w = "Weight"
#'   ) %>%
#'   set_value_labels(
#'     x = c("Male" = 1, "Female" = 2)
#'   )
#'
#' label_table(df)
#' label_table(df, x, freq = FALSE)
#' label_table(df, x)
#' label_table(df, x, weight = w)
#'
#' @importFrom glue glue
#' @importFrom tidyr drop_na
#' @importFrom purrr set_names
#' @importFrom labelled var_label val_labels remove_val_labels
#' @importFrom tibble enframe tibble
#' @importFrom dplyr %>% enquo quo_name pull select bind_rows arrange left_join
#' mutate group_by summarise ungroup filter
#' @importFrom rlang := sym
#' @importFrom crayon green red
#'
#' @export
label_table <- function(data, var, freq = TRUE, weight) {
    
    # create variable label table ---------------------------------------------
    
    # `labelled` can be used to extract variable labels
    # replacing NULL's with NA avoids dropping vars with unlist()
    if (missing(var)) {
        
        var.tbl <- var_label(data)
        var.tbl[sapply(var.tbl, is.null)] <- NA
        var.tbl <- unlist(var.tbl) %>% enframe("var", "lab")
        return(var.tbl)
        
    }
    
    # create value label table ------------------------------------------------
    # crappy check for string - if it is, as.symbol
    check <- try(is.character(var), silent = TRUE)
    
    if (!"try-error" %in% class(check)) {
        if (check) {
            var <- sym(var)
        }
    }
    
    # set up for tidy eval
    var.name      <- enquo(var)
    var.name.chr  <- quo_name(var.name)
    var.label.chr <- paste(var.name.chr, "lab", sep = "_")
    var.label.sym <- sym(var.label.chr)
    
    # extract value labels and early return if NULL
    val.labs <-
        data %>%
        pull({{ var.name }}) %>%
        val_labels()
    
    if (is.null(val.labs)) {
        warning(glue("Value labels do not exist for variable `{var.name.chr}`."))
        return(NULL)
    }
    
    val.tbl <-
        val.labs %>%
        enframe(
            name = var.label.chr,
            value = var.name.chr
        ) %>%
        select(2, 1)
    
    # NA values get dropped by `val_labels()`, need to add those values back in
    in.data   <- data %>% pull({{ var.name }}) %>% unique() %>% sort(na.last = TRUE)
    in.labels <- val.tbl %>% pull(1) %>% unique() %>% sort(na.last = TRUE)
    val.need  <- in.data[! in.data %in% in.labels]
    
    # if there's nothing else to add, just return what we have
    if (!length(val.need) == 0) {
        
        # this gets binded to the existing label table
        need.tbl <- tibble({{ var.name.chr }} := val.need,
                           {{ var.label.chr }} := NA)
        
        val.tbl <-
            val.tbl %>%
            bind_rows(need.tbl) %>%
            arrange({{ var.name.sym }})
    }
    
    
    # if freqs are requested --------------------------------------------------
    
    if (freq) {
        
        N <- nrow(data)
        N.valid <- data %>% select({{ var.name }}) %>% drop_na() %>% nrow()
        
        if (missing(weight)) {
            
            freq.tbl <-
                data %>%
                select({{ var.name }}) %>%
                mutate(weight = 1) %>%
                set_names(var.name.chr, "weight") %>%
                remove_val_labels()
            
        }
        
        if (! missing(weight)) {
            
            weight.name <- enquo(weight)
            weight.name.chr <- quo_name(weight.name)
            weight.is.numeric <- is.numeric(data[[weight.name.chr]])
            
            freq.tbl <-
                data %>%
                select({{ var.name }}, {{ weight.name }}) %>%
                set_names(var.name.chr, "weight") %>%
                remove_val_labels()
            
            # putting this check after the data wrangling, will error out
            # due to not finding the column before moving on to the class check
            
            if (! weight.is.numeric) {
                stop("Input to `weight` must be numeric.")
            }
            
        }
        
        # calc freq and percent
        before.valid.tbl <-
            freq.tbl %>%
            group_by({{ var.name }}) %>%
            summarise(freq = sum(weight)) %>%
            ungroup() %>%
            mutate(pct = freq / N)
        
        # calc valid percent
        with.valid.tbl <-
            before.valid.tbl %>%
            filter(!is.na({{ var.name }})) %>%
            mutate(valid.pct = freq / N.valid) %>%
            select({{ var.name }}, valid.pct)
        
        # warnings due to attribute labels differeing on LHS and RHS of join
        suppressWarnings(
            val.tbl <-
                val.tbl %>%
                remove_val_labels() %>%
                left_join(before.valid.tbl, by = var.name.chr) %>%
                left_join(with.valid.tbl, by = var.name.chr)
        )
    }
    
    if (! missing(var)) {
        
        var.text <-
            data %>%
            select({{ var.name }}) %>%
            var_label() %>%
            unlist()
        
        if(freq) {
            if (!missing(weight)) {
                cat(glue(
                    "Var: {var.name.chr}",
                    "\n",
                    "Lab: {var.text}",
                    "\n",
                    "Tab: {green('Weighted')}",
                    "\n\n"
                ))
            } else {
                cat(glue(
                    "Var: {var.name.chr}",
                    "\n",
                    "Lab: {var.text}",
                    "\n",
                    "Tab: {red('Unweighted')}",
                    "\n\n"
                ))
            }
        } else {
            cat(glue(
                "Var: {var.name.chr}",
                "\n",
                "Lab: {var.text}",
                "\n\n"
            ))
        }
    }
    
    return(val.tbl)
    
}
globalVariables(c("valid.pct", "var.name.sym"))
