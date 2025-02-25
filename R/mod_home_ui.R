# R/mod_home_ui.R
#' Home UI
#'
#' @param id Module id
#'
#' @export
mod_home_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        tags$div(
          class = "panel-title-custom",
          "Bem-vindo ao Projeto ReMaP"
        ),
        tags$p("", style = "font-size: 18px; text-align: center;")
      )
    )
  )
}
