plot_missings <- function(DT) {
  vars <- names(DT)
  DT2 <- copy(DT) 
  if (!is.data.table(DT2)) {
    setDT(DT2)
  }
  
  # convert cells to either the class() or NA
  DT2[, (vars) := lapply(.SD, function(x) ifelse(!is.na(x), paste(class(x), collapse = '\n'), NA))]
  DT2[, id_xyz := .I]
  
  # melt data.table (gives warning that not all measure vars are of the same type)
  slag <- suppressWarnings(melt(DT2, id.vars = "id_xyz", measure.vars = vars) )
  
  # plot raster
  ggplot2::ggplot(data = slag, ggplot2::aes_string(x = "variable", y = "id_xyz", text = "value")) +
    ggplot2::geom_raster(ggplot2::aes_string(fill = "value")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "", y = "Rij") +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Type")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))
}
