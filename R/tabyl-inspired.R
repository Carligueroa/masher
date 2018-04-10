#' A well formatted one variable janitor::tabyl
#'
#' @param .data A data.frame
#' @param .var The variable to count
#' @param show_na Should the NAs be shown?
#' @return A `tabyl` well formatted
table_monovar <- function(.data, .var = NULL, show_na = FALSE) {

  if (is.atomic(.data)) {
    .data <- janitor::tabyl(.data, show_na = show_na)
  } else {
    .var <- rlang::enquo(.var)
    .data <- janitor::tabyl(.data, !!.var, show_na = show_na)
  }

  .data %<>%
    janitor::adorn_pct_formatting()

  if (any(.data[,1] %in% c("Otros", "Other"))) {
    .data %<>%
      dplyr::mutate(to_arrange = ifelse(!!.var == "Otros", -n, n)) %>%
      dplyr::arrange(dplyr::desc(to_arrange)) %>%
      dplyr::select(-to_arrange)
  }

  return(.data)
}

#' A well formatted two variables janitor::tabyl
#'
#' @param .data A data.frame
#' @param .var1 The row variable
#' @param .var2 The column variable
#' @param show_na Should the NAs be shown?
#' @return A two var `tabyl` well formatted
table_duovar <- function(.data, .var1, .var2 = genero, show_na = FALSE) {
  .var1 <- rlang::enquo(.var1)
  .var2 <- rlang::enquo(.var2)

  .data %<>%
    janitor::tabyl(!!.var1, !!.var2, show_na = show_na) %>%
    janitor::adorn_totals("row") %>%
    janitor::adorn_totals("col") %>%
    janitor::adorn_percentages("col") %>%
    janitor::adorn_pct_formatting() %>%
    janitor::adorn_ns()

  return(.data)
}
