packages <- c("dplyr", "DBI", "RPostgres", "xml2", "lubridate", "ggplot2", "patchwork", "tidyr")
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

invisible(lapply(packages, install_if_missing))