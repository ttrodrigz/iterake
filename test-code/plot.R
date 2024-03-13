



#' @export
ggplot2::autoplot


# u <-
#     universe(
#         data = mtcars,
#         category(
#             name = "cyl",
#             groups = c(4, 6, 8),
#             targets = c(0.3, 0.3, 0.4)
#         ),
#         category(
#             name = "vs",
#             groups = c(0, 1),
#             targets = c(1/2, 1/2)
#         ),
#         category(
#             name = "gear",
#             groups = c(3, 4, 5),
#             targets = c(2/4, 1/4, 1/4)
#         )
#     )
# 
# autoplot(u)
# 
# i <- iterake(u, control = control_iterake(max_iter = 10000))

#' @exportS3Method autoplot universe
autoplot.iterake <- function(object, ...) {
    
    clr.uwgt <- "#1B8CB5"
    clr.wgt  <- "#F15A24"
    
    
    # What variables are being used in the weighting?
    cat.vars <- names(object$universe$categories)
    
    # What are the actual proportions?
    have.uwgt <-
        object$universe$data$data |> 
        select(all_of(cat.vars)) |> 
        map(\(x) qtab(x, drop = FALSE, dnn = NULL, sort = TRUE)) |> 
        map(prop.table) |> 
        map(enframe, name = "group", value = "uwgt_p") |> 
        map(mutate, uwgt_p = as.double(uwgt_p)) |> 
        enframe(name = "category", value = "data") |> 
        mutate(category = fct_inorder(category)) |> 
        unnest(data)
    
    have.wgt <-
        object$universe$data$data |> 
        select(all_of(cat.vars)) |> 
        map(\(x) qtab(x, w = object$results, drop = FALSE, dnn = NULL, sort = TRUE)) |> 
        map(prop.table) |> 
        map(enframe, name = "group", value = "wgt_p") |> 
        map(mutate, wgt_p = as.double(wgt_p)) |> 
        enframe(name = "category", value = "data") |> 
        mutate(category = fct_inorder(category)) |> 
        unnest(data)
    
    
    # What are the target proportions?
    want <-
        object |> 
        pluck("universe") |> 
        pluck("categories") |> 
        map(as_tibble) |> 
        map(rename, group = 1, target = 2) |> 
        enframe(name = "category", value = "data") |> 
        mutate(category = fct_inorder(category)) |> 
        unnest(data) |> 
        mutate(group = as.character(group))
    
    # Join and prep
    joined <-
        have.uwgt |> 
        left_join(have.wgt, by = join_by(category, group)) |> 
        left_join(want, by = join_by(category, group)) |> 
        mutate(
            group = fct_inorder(group),
            group = fct_rev(group)
        ) |> 
        rename(Unweighted = uwgt_p, Weighted = wgt_p) |> 
        pivot_longer(
            cols = c(Unweighted, Weighted),
            names_to = "version",
            values_to = "p"
        ) |> 
        mutate(version = factor(version, levels = c("Unweighted", "Weighted")))
    
    
    
    joined |> 
        ggplot(aes(y = group)) +
        geom_errorbar(
            aes(xmin = target, xmax = target), 
            width = 0.5,
            linewidth = 8/10
        ) +
        geom_point(
            aes(x = p, color = version, group = version),
            size = 2.75,
            position = position_dodge(width = 0.0)
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

autoplot(i)
