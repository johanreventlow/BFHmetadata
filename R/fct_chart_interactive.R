# Interaktiv run chart bygget fra bfh_qic()$qic_data via ggiraph. Punkter er
# klikbare (data_id = ISO-dato) → input$<id>_selected. Median-trin følger 'part'
# (fase), signal-punkter fremhæves. BFHtheme-styling hvor muligt.

#' @param qic_result bfh_qic_result (fra compute_signal()$qic_result)
#' @param selected_date valgt ISO-dato ("YYYY-MM-DD") der fremhæves, el. NULL
#' @param height_svg girafe-højde i tommer
#' @return ggiraph::girafe
#' @noRd
interactive_run_chart <- function(qic_result, selected_date = NULL, height_svg = 4) {
  qd <- qic_result$qic_data
  # Kun endelige y-værdier kan tegnes. Uden dette filter giver degenererede
  # slices (alt-NA y) et plot med tom akse-range (min→Inf) som vælter
  # girafe-bygningen med "diff.default(continuous_range_coord)" — set i
  # produktion under scan. Ingen tegnbare punkter → NULL (kalderen viser
  # venlig besked); en graf-bygning må ALDRIG fejle.
  qd <- qd[is.finite(qd$y), , drop = FALSE]
  if (nrow(qd) == 0) return(NULL)
  # POSIXct → Date-streng (TZ-sikkert) som stabilt punkt-id
  qd$.id <- format(qd$x, "%Y-%m-%d")
  qd$.tooltip <- sprintf("%s: %s", qd$.id, round(qd$y, 2))
  qd$.signal <- isTRUE_vec(qd$anhoej.signal)
  qd$.selected <- !is.null(selected_date) & qd$.id == (selected_date %||% "")

  p <- ggplot2::ggplot(qd, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_line(color = "grey40", linewidth = 0.4)

  # Median-trin pr. fase (cl er konstant inden for hver 'part') — kun når der
  # faktisk ER en median at tegne. qic kan give cl helt NA (fx enkelt-punkts
  # serier); et lag uden endelige værdier bidrager med tom range.
  if (any(is.finite(qd$cl))) {
    p <- p + ggplot2::geom_line(
      data = qd[is.finite(qd$cl), , drop = FALSE],
      ggplot2::aes(y = .data$cl, group = .data$part),
      color = "steelblue", linewidth = 0.6)
  }

  p <- p +
    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = .data$.tooltip, data_id = .data$.id,
                   color = .data$.signal),
      size = 2) +
    ggplot2::scale_color_manual(values = c(`FALSE` = "grey30", `TRUE` = "firebrick"),
                                guide = "none") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal()

  # Fremhæv valgt punkt (ring udenom)
  if (!is.null(selected_date) && any(qd$.selected)) {
    p <- p + ggplot2::geom_point(
      data = qd[qd$.selected, , drop = FALSE],
      shape = 21, size = 4, stroke = 1.1, color = "black", fill = NA)
  }

  ggiraph::girafe(ggobj = p, height_svg = height_svg,
    options = list(
      ggiraph::opts_selection(type = "single", only_shiny = TRUE),
      ggiraph::opts_hover(css = "cursor:pointer;")))
}

#' Robust TRUE-vektor (NA/NULL → FALSE) — qic-signalkolonner kan have NA.
#' @noRd
isTRUE_vec <- function(x) !is.na(x) & x
