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
    # Caixas extras para nível MUNICIPIO
    conditionalPanel(
      condition = sprintf("input['%s'] == 'MUNICIPIO'", ns("nivel_selection")),
      fluidRow(
        column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("extra_summary_box_1")))),
        column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("extra_summary_box_2")))),
        column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("extra_summary_box_3"))))
      )
    ),
    # Cards com gráficos para níveis que não sejam MUNICIPIO
    conditionalPanel(
      condition = sprintf("input['%s'] != 'MUNICIPIO'", ns("nivel_selection")),
      # Primeira linha: os 3 primeiros gráficos (nascidos vivos, UBS, gestantes)
      fluidRow(
        column(width = 4, uiOutput(ns("card_plot_nascidos_vivos"))),
        column(width = 4, uiOutput(ns("card_plot_ubs"))),
        column(width = 4, uiOutput(ns("card_plot_gestantes_susdependentes")))
      ),
      br(),
      # Se o nível for ESTADUAL, exibe abaixo, centralizado, o gráfico de Nascidos Vivos SUSdependentes (output único para ESTADUAL)
      conditionalPanel(
        condition = sprintf("input['%s'] == 'ESTADUAL'", ns("nivel_selection")),
        fluidRow(
          column(width = 6, offset = 3, uiOutput(ns("card_plot_nascidos_susdependentes_estadual")))
        )
      ),
      # Se o nível for RRAS, DRS ou REGIÃO DE SAÚDE, exibe a segunda linha com três gráficos e abaixo o de Cobertura AB centralizado;
      # Aqui usamos outro output para o gráfico de Nascidos Vivos SUSdependentes específico para esses níveis.
      conditionalPanel(
        condition = sprintf("input['%s'] == 'RRAS' || input['%s'] == 'DRS' || input['%s'] == 'REGIÃO DE SAÚDE'",
                            ns("nivel_selection"), ns("nivel_selection"), ns("nivel_selection")),
        fluidRow(
          column(width = 4, uiOutput(ns("card_plot_nascidos_susdependentes_outros"))),
          column(width = 4, uiOutput(ns("card_plot_cobertura_ans"))),
          column(width = 4, uiOutput(ns("card_plot_cobertura_esf")))
        ),
        br(),
        fluidRow(
          column(width = 6, offset = 3, uiOutput(ns("card_plot_cobertura_ab")))
        )
      ),
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
