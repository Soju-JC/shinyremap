# R/mod_rras_aps_ui.R
#' RRAS APS UI
#'
#' @param id Module id
#' @importFrom magrittr %>%
#'
#' @export
mod_rras_aps_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # Filtros de seleção (mantidos no topo)
    fluidRow(
      column(
        width = 4,
        selectInput(
          inputId = ns("nivel_selection"),
          label = "Selecione o nível:",
          choices = c("ESTADUAL", "RRAS", "DRS", "REGIÃO DE SAÚDE", "MUNICIPIO"),
          selected = "ESTADUAL"
        )
      ),
      column(
        width = 4,
        uiOutput(ns("secondary_filter_ui"))
      )
    ),
    br(),
    # Caixas de totais (dispostas horizontalmente em uma única linha)
    fluidRow(
      column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("summary_box_1")))),
      column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("summary_box_2")))),
      column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("summary_box_3")))),
      column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("summary_box_4"))))
    ),
    br(),
    # Caixas extras para nível MUNICÍPIO
    conditionalPanel(
      condition = sprintf("input['%s'] == 'MUNICIPIO'", ns("nivel_selection")),
      fluidRow(
        column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("extra_summary_box_1")))),
        column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("extra_summary_box_2")))),
        column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("extra_summary_box_3"))))
      )
    ),
    # Cards com gráficos para os demais níveis
    conditionalPanel(
      condition = sprintf("input['%s'] != 'MUNICIPIO'", ns("nivel_selection")),
      fluidRow(
        column(width = 4, uiOutput(ns("card_plot_nascidos_vivos"))),
        column(width = 4, uiOutput(ns("card_plot_ubs"))),
        column(width = 4, uiOutput(ns("card_plot_gestantes_susdependentes")))
      ),
      br(),
      fluidRow(
        column(width = 6, offset = 3, uiOutput(ns("card_plot_nascidos_susdependentes")))
        # ,
        # column(width = 4, uiOutput(ns("card_plot_cobertura_ans"))),
        # column(width = 4, uiOutput(ns("card_plot_cobertura_ab")))
      ),
      # br(),
      # fluidRow(
      #   column(width = 4, uiOutput(ns("card_plot_esf")))
      # ),
      br(),
      br(),
      # Tabelas: cada uma em coluna separada
      fluidRow(
        width = 12,
        column(
          width = 6,
          bs4Dash::bs4Card(
            title  = "Estabelecimentos de Referência para AAE (AGAR)",
            height = "100%",
            width = NULL,
            DT::DTOutput(ns("table_aae"))
          )
        ),
        br(),
        column(
          width = 6,
          bs4Dash::bs4Card(
            title  = "Estabelecimentos de Referência para Parto (Baixo Risco)",
            height = "100%",
            width = NULL,
            DT::DTOutput(ns("table_bxr"))
          )
        )
      )
    )
  )
}
