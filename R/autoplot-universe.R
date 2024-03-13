#' Visualize `universe()`.
#' 
#' This returns a plot comparing the actual unweighted marginal proportions with
#' the targets of the categories used in constructing the universe.
#' 
#' @param object Output of `universe()`.
#' @param ... Not currently used.
#' 
#' @importFrom dplyr select all_of mutate left_join rename join_by
#' @importFrom forcats fct_inorder fct_rev
#' @importFrom purrr map pluck
#' @importFrom collapse qtab
#' @importFrom tibble enframe
#' @importFrom tidyr unnest
#' @importFrom ggplot2 ggplot aes geom_errorbar geom_point scale_color_manual
#' facet_wrap theme theme_minimal element_rect labs autoplot
#' 
#' @exportS3Method autoplot universe
autoplot.universe <- function(object, ...) {
    
    clr.uwgt <- "#1B8CB5"
    clr.wgt  <- "#F15A24"
    
    
    # What variables are being used in the weighting?
    cat.vars <- names(object$categories)
    
    # What are the actual proportions?
    have <-
        object$data$data |> 
        select(all_of(cat.vars)) |> 
        map(\(x) qtab(x, drop = FALSE, dnn = NULL, sort = TRUE)) |> 
        map(prop.table) |> 
        map(enframe, name = "group", value = "uwgt_p") |> 
        map(mutate, uwgt_p = as.double(uwgt_p)) |> 
        enframe(name = "category", value = "data") |> 
        mutate(category = fct_inorder(category)) |> 
        unnest(data)
    
    
    # What are the target proportions?
    want <-
        object |> 
        pluck("categories") |> 
        map(as_tibble) |> 
        map(rename, group = 1, target = 2) |> 
        enframe(name = "category", value = "data") |> 
        mutate(category = fct_inorder(category)) |> 
        unnest(data) |> 
        mutate(group = as.character(group))
    
    # Join and prep
    joined <-
        have |> 
        left_join(want, by = join_by(category, group)) |> 
        mutate(
            group = fct_inorder(group),
            group = fct_rev(group)
        )
    
    # Calculate delta
    browser()
    
    # Calculate the sum of the absolute difference of the "wants" and "haves"
    delta <- sum(map2_dbl(
        .x = want,
        .y = have,
        .f = \(w, h) sum(abs(w - h))
    ))
    
    w <- want$target
    h <- have$uwgt_p
    
    sum(abs(want$target - have$uwgt_p))

    # Plot
    joined |> 
        mutate(version = "Unweighted") |> 
        ggplot(aes(y = group)) +
        geom_errorbar(
            aes(xmin = target, xmax = target), 
            width = 0.5,
            linewidth = 8/10
        ) +
        geom_point(
            aes(x = uwgt_p, color = version),
            size = 2.75
        ) +
        scale_color_manual(
            values = c("Unweighted" = clr.uwgt, "Weighted" = clr.wgt)
        ) +
        facet_wrap(~category, scales = "free_y") +
        theme_minimal() +
        theme(
            panel.border = element_rect(fill = NA, color = "gray50"),
            plot.title.position = "plot",
            legend.position = "bottom",
            legend.direction = "horizontal"
        ) +
        labs(
            x = "Proportion",
            y = NULL,
            title = "Actual vs Target Marginal Proportions",
            subtitle = "Vertical lines represent the target proportion",
            color = NULL
        )
    
}

#' @export
ggplot2::autoplot

utils::globalVariables(c(
    "data", "group", "target", "uwgt_p"
))