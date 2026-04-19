# Helper: remove leading zeros from numeric strings
# e.g. "0.123" -> ".123", "-0.123" -> "-.123"
rmlead0 <- function(x) {
    if (is.numeric(x)) {
        x <- sprintf("%.3f", x)
    }
    x[] <- gsub("^0\\.", ".", x)
    x[] <- gsub("^-0\\.", "-.", x)
    x
}

# Build a formatted tinytable of lavaan parameter estimates
#
# @param fit       A fitted lavaan object.
# @param ops       Character vector of lavaan operators to include.
#                  Defaults to factor loadings and regression paths.
# @param caption   Table caption string.
#
# @return A tinytable object ready to print/render.
sem_table <- function(fit,
                      ops = c("=~", "~"),
                      caption = "Parameter Estimates") {
    est_unst <- lavaan::parameterEstimates(fit) |>
        dplyr::filter(op %in% ops) |>
        dplyr::select(lhs, op, rhs, Est = est, SE = se, pvalue)

    est_std <- lavaan::standardizedSolution(fit) |>
        dplyr::filter(op %in% ops) |>
        dplyr::select(lhs, op, rhs, Std = est.std, SE_Std = se)

    est_combined <- dplyr::left_join(est_unst, est_std,
                                     by = c("lhs", "op", "rhs")) |>
        dplyr::mutate(
            Type = dplyr::case_when(
                op == "=~" ~ "Factor Loading",
                op == "~"  ~ "Regression Path"
            ),
            p = dplyr::case_when(
                pvalue < .001   ~ "< .001",
                is.na(pvalue)   ~ "",
                TRUE            ~ sprintf("%.3f", pvalue)
            ),
            Parameter = dplyr::if_else(
                op == "=~",
                paste(lhs, "\u2192", rhs),   # Factor → Indicator
                paste(rhs, "\u2192", lhs)    # Predictor → Outcome
            )
        ) |>
        dplyr::select(Type, Parameter, Est, SE, Std, SE_Std, p)

    struct_start <- which(est_combined$Type == "Regression Path")[1]

    row_groups <- list("Factor loadings" = 1)
    if (!is.na(struct_start)) {
        row_groups[["Structural coefficients"]] <- struct_start
    }

    est_combined |>
        dplyr::select(-Type) |>
        tinytable::tt(caption = caption) |>
        tinytable::group_tt(
            j = list("Unstandardized" = 2:3, "Standardized" = 4:5)
        ) |>
        tinytable::format_tt(
            j = c("Est", "SE", "Std", "SE_Std"),
            fn = rmlead0
        ) |>
        tinytable::group_tt(i = row_groups) |>
        tinytable::style_tt(j = "Parameter", align = "l") |>
        tinytable::style_tt(
            j = c("Est", "SE", "Std", "SE_Std", "p"),
            align = "c"
        ) |>
        tinytable::style_tt(i = "groupi", underline = TRUE)
}
