pre_rake <- function(df, pop.model) {
    
    # make sure each wgt_cat in pop.model has a matching column in df
    df.names  <- names(df)
    mod.names <- pull(pop.model, wgt_cat)
    bad.names <- mod.names[!mod.names %in% df.names]
    
    if (length(bad.names) > 0) {
        stop(
            paste(
                "Each weighting category in `pop.model` must have a matching column name in `df`. The following weighting cagegories have no match:",
                paste(bad.names, collapse = ", "),
                sep = "\n"
            )
        )
    }
    
    # do a tally of NA's and tell 'em what's going to happen
    n <- nrow(df)
    
    pct.missing <- df %>%
        select(one_of(pop.model$wgt_cat)) %>%
        map_dbl(~ sum(is.na(.)) / n)
    
    list(pct.missing = pct.missing)
    
    # ----
    # output <- dplyr::left_join(
    #     df %>%
    #         dplyr::select(one_of(pop.model$wgt_cat)) %>%
    #         tidyr::gather(wgt_cat, value) %>%
    #         dplyr::group_by(wgt_cat, value) %>%
    #         dplyr::summarise(n = n()) %>%
    #         dplyr::group_by(wgt_cat) %>%
    #         dplyr::mutate(uwgt_prop = n / sum(n)) %>%
    #         dplyr::ungroup(),
    #     
    #     pop.model %>%
    #         tidyr::unnest(data) %>%
    #         dplyr::ungroup(),
    #     
    #     by = c("wgt_cat", "value")) %>%
    #     
    #     dplyr::mutate(uwgt_diff = targ_prop - uwgt_prop)
    # 
    # print(
    #     output %>%
    #         ggplot(aes(x = as.character(value))) +
    #         geom_errorbar(aes(ymin = targ_prop,
    #                           ymax = targ_prop),
    #                       lty = "longdash",
    #                       color = "#4b4b4b") +
    #         geom_point(aes(y = uwgt_prop),
    #                    size = 3,
    #                    color = "#d10000") +
    #         scale_y_continuous(breaks = pretty,
    #                            limits = c(0, max(output$uwgt_prop, 
    #                                              output$targ_prop))) +
    #         facet_wrap(~wgt_cat, scales = "free_y") +
    #         labs(x = NULL, y = "Proportion",
    #              color = NULL) +
    #         ggtitle("Sample's deviation from population model (unweighted)",
    #                 "Dashed line = target proportion") +
    #         coord_flip() +
    #         theme_bw() +
    #         theme(strip.background = element_rect(fill = "#fff6b5"),
    #               legend.position = "bottom")
    # )
    # 
    # # print to screen - invisible output object
    # title1 <- 'pre-rake deviance'
    # num_dashes <- nchar(title1) + 4
    # rem_dashes <- 80 - num_dashes
    # cat('\n-- ' %+% 
    #         bold(title1) %+% 
    #         ' ' %+%
    #         paste(rep('-', times = rem_dashes), collapse = "") %+%
    #         '\n')
    # print.data.frame(output, row.names = FALSE)
    # invisible(output)
    
    # some ratio of bins vs sample size?
}

pre_rake(df_miss, mod)$data[[2]]


df_miss <- df_1
df_miss[1:19, 3] <- NA

base <-
    tibble(
        value = c("male", "female"),
        targ = c(0.5, 0.5)
    )

fct <-
    base %>%
    mutate(value = as_factor(value))

left_join(fct, base, by = "value")

class(base$value) <- class(fct$value)

magclass::copy.attributes(from = fct$value, to = base$value)


class(base$value) <- NULL
class(base$value) <- "factor"

copy.att <- function(from, to) {
    a <- attributes(from)
    a['class'] <- NULL
    attributes(to) <- c(attributes(to), a)
    to
}

a <- factor(x = c("male", "female"))
b <- c("male", "female")

copy.att(from = a, to = b)


fct <- tibble(v = factor(c("male", "female")),
              y = c(1, 1))

fct.ord <- fct %>%
    mutate(v = factor(v, ordered = TRUE),
           yy = c(0, 1))

fct %>%
    mutate(v = factor(v, levels = fct.ord$v))


# USE THIS TO MAP OVER LIST OF DF'S
calcs <- function(x) {
    
    calc <- x %>%
        group_by(value) %>%
        summarise(n = n()) %>%
        ungroup() %>%
        mutate(uwgt_prop = n / sum(n)) %>%
        ungroup()
    
}

# USE THIS TO MAKE LIST NAME INTO WGT_CAT VARIABLE
add_wgt_cat <- function(df_list) {
    
    for (i in seq_along(df_list)) {
        
        df_list[[i]]$wgt_cat <- names(df_list[i])
        
    }
    
    df_list
}

# THIS IS GOOD RIGHT HERE
a <- df_1 %>%
    select(one_of(mod$wgt_cat)) %>%
    map(as_tibble) %>%
    map(calcs) %>%
    add_wgt_cat() %>%
    map(group_by, wgt_cat) %>%
    map(nest) %>%
    bind_rows()

# THIS IS ALSO GOOD!
(left_join(a, mod, by = "wgt_cat") %>%
    mutate(full = map2(data.x, data.y, left_join, by = "value")))$full[[1]]

# NEED TO APPLY CLASS/LEVELS OF DATA INTO MOD THEN DO STEP ABOVE AND WILL
# ALL JOIN CORRECTLY! HUZZAH!!