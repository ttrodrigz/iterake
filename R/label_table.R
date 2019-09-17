#' Create table of variable or value labels
#'
#' It is often useful to inspect variable or value labels attached to datasets
#' (like with SAV files from SPSS). This function will return a tibble of variables and
#' their associated labels, or, if requested, a variable and its value labels.
#'
#' @param data Data frame.
#' @param var A variable in the dataset where labels are desired.
#' @param freq Boolean, indicates whether frequencies should be included, optional.
#' @param weight A variable in the dataset representing weights to be used 
#' for optional frequency calculation, optional.
#' 
#' @importFrom labelled remove_val_labels val_labels is.labelled
#' @importFrom Hmisc contents
#' @importFrom tibble tibble enframe
#' @importFrom tidyr drop_na
#' @importFrom purrr map_df set_names
#' @importFrom dplyr filter mutate enquo quo_name pull left_join select bind_rows arrange group_by summarise ungroup
#' @importFrom rlang !! sym :=
#'
#' @return By default, will return a dataset of variables and labels of every
#' variable in the dataset. If specified, returns a table of values in a
#' variable and its labels, as well as frequencies.
#'
#' @examples
#' data(demo_data)
#' 
#' label_table(data = demo_data, var = EyeColor)
#'
#' @export
label_table <- function(data, var, freq = FALSE, weight) {

    # when only variable labels are requested ----
    if (missing(var)) {

        # this stops a "1" from being printed where value labels
        # exist on a variable with no variable labels
        # this is an artifact of the contents method of Hmisc
        data <- remove_val_labels(data)

        # do any of the variables have labels?
        var_labels <- contents(data)$contents$Labels

        if (is.null(var_labels)) {
            stop("Data provided contains no variable labels, did you mean to request value labels for a specific variable?")
        }

        # build the table
        out <- tibble(
            var = names(data),
            lab = var_labels
        ) %>%
            # both names and labels should be characters
            map_df(as.character) %>%
            # makes more sense to NA the empty strings
            mutate(lab = ifelse(lab == "", NA, lab))

        return(out)

    }

    # when value labels are requested ----
    if (!missing(var)) {

        var_name      <- enquo(var)
        var_name_chr  <- quo_name(var_name)
        var_label_chr <- paste0(quo_name(var_name), "_lab")

        # does this variable have value lables?
        if (!is.labelled(data %>% pull(!! var_name))) {
            warning(paste0(
                "Value labels do not exist for `",
                quo_name(var_name),
                "`."))

            return(NULL)
        }

        # create a table of the variable levels and their labels
        label_table <-
            data %>%
            pull(!! var_name) %>%
            val_labels() %>%
            enframe(
                name = var_label_chr,
                value = var_name_chr
            ) %>%
            select(2, 1)

        # need to add in extra levels in the data that aren't accounted
        # for by the label attributes

        in.data   <- data %>% pull(!! var_name) %>% unique() %>% sort(na.last = TRUE)
        in.labels <- label_table %>% pull(1) %>% unique() %>% sort(na.last = TRUE)

        needed <- in.data[!in.data %in% in.labels]

        # if there's nothing else to add, just return what we have
        if (!length(needed) == 0) {

            # this gets binded to the existing label table
            need_table <- tibble(!! var_name_chr := needed,
                                 !! var_label_chr := NA)

            label_table <-
                label_table %>%
                # bind what's needed and sort
                bind_rows(need_table) %>%
                arrange(!! sym(var_name_chr))
        }

        label_table
    }

    if (freq) {

        N <- nrow(data)
        N_no_miss <- data %>% select(!! var_name) %>% drop_na() %>% nrow()

        if (missing(weight)) {

            freq_df <-
                data %>%
                select(!! var_name) %>%
                mutate(weight = 1) %>%
                set_names(var_name_chr, "weight") %>%
                remove_val_labels()

        }

        if (!missing(weight)) {
            weight_name <- enquo(weight)

            freq_df <-
                data %>%
                select(!! var_name, !! weight_name) %>%
                set_names(var_name_chr, "weight") %>%
                remove_val_labels()

        }

        # calc freq and percent
        pre_valid <-
            freq_df %>%
            group_by(!! var_name) %>%
            summarise(freq = sum(weight)) %>%
            ungroup() %>%
            mutate(pct = freq / N)

        # calc valid percent
        with_valid <-
            pre_valid %>%
            filter(!is.na(!! var_name)) %>%
            mutate(valid.pct = freq / N_no_miss) %>%
            select(!! var_name, valid.pct)

        if (missing(weight)) {
            cat("Unweighted\n")
        } else {
            cat("Weighted\n")
        }

        # getting rid of warnings, joining LHS and RHS thing
        suppressWarnings(
            out <-
                # join all components
                label_table %>%
                remove_val_labels() %>%
                left_join(pre_valid, by = var_name_chr) %>%
                left_join(with_valid, by = var_name_chr)

        )

        return(out)

    }

    out <- label_table
    out

}

utils::globalVariables(c("lab", "valid.pct", "wgt_temp", "prop_diff", "rn"))