# R/mod_rras_aps_server.R
#' RRAS APS Server
#'
#' @param id Module id
#' @param data_list Lista com os dados carregados em load_data()
#' @importFrom magrittr %>%
#' @import dplyr
#'
#' @export
mod_rras_aps_server <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Função auxiliar para calcular a altura dinâmica com base no número de barras
    calc_dynamic_height <- function(n_bars) {
      if(n_bars <= 10) {
        400
      } else if(n_bars <= 25) {
        500
      } else if(n_bars <= 40) {
        700
      } else if(n_bars <= 55) {
        900
      } else if(n_bars <= 75) {
        1200
      } else {
        2500
      }
    }

    # Função auxiliar para construir o card que conterá o gráfico
    build_plot_card <- function(card_title, plot_output_id, data_to_plot, caption = NULL) {
      n_bars <- nrow(data_to_plot)
      height_val <- calc_dynamic_height(n_bars)
      bs4Dash::bs4Card(
        title  = card_title,
        height = "100%",
        width = NULL,
        tagList(
          # Renderiza o gráfico
          plotly::plotlyOutput(ns(plot_output_id), height = paste0(height_val, "px")),
          # Adiciona o texto extra, se fornecido, com margem para distanciar do eixo x
          if (!is.null(caption)) {
            tags$div(caption, style = "margin-top: 15px; font-size: 12px; color: #555;")
          }
        )
      )
    }

    # Quebra de linha em labels muito grandes nos eixos para função build_bar_plot (orientação v)
    wrap_after_second <- function(text, threshold) {
      if(nchar(text) > threshold) {
        words <- unlist(strsplit(text, "\\s+"))
        if(length(words) > 2) {
          # Junta as duas primeiras palavras, insere <br> e depois junta o restante
          paste(paste(words[1:2], collapse = " "), paste(words[-(1:2)], collapse = " "), sep = "<br>")
        } else {
          text
        }
      } else {
        text
      }
    }

    # Quebra de linha em labels muito grandes nos eixos para função build_bar_plot (orientação h)
    wrap_vertical_title <- function(text) {
      if(nchar(text) <= 15) {
        return(text)
      } else if(nchar(text) > 15 && nchar(text) < 20) {
        words <- unlist(strsplit(text, "\\s+"))
        if(length(words) > 2) {
          # Junta as duas primeiras palavras, insere <br> e junta o restante
          return(paste(paste(words[1:2], collapse = " "), paste(words[-(1:2)], collapse = " "), sep = "<br>"))
        } else {
          return(text)
        }
      } else if(nchar(text) > 20) {
        words <- unlist(strsplit(text, "\\s+"))
        # Junta cada palavra com <br> (cada palavra em uma nova linha)
        return(paste(words, collapse = "<br>"))
      }
    }

    # Função auxiliar para construir o gráfico de barras com orientação dinâmica.
    # Foi adicionado o argumento is_percentage para ajustar o eixo numérico.
    build_bar_plot <- function(data, var_numeric, var_category, is_percentage = FALSE) {
      n_bars <- nrow(data)

      # Nova lógica para decidir a orientação:
      if(n_bars <= 10) {
        orientation <- "v"
      } else {
        tick_names <- as.character(data[[var_category]])
        all_short <- all(nchar(tick_names) <= 12 & sapply(tick_names, function(x) {
          length(unlist(strsplit(x, "\\s+")))
        }) <= 2)
        orientation <- if(all_short) "v" else "h"
      }

      if (orientation == "h") {
        p <- plotly::plot_ly(
          data = data,
          x = as.formula(paste0("~`", var_numeric, "`")),
          y = as.formula(paste0("~`", var_category, "`")),
          type = "bar",
          orientation = "h",
          marker = list(color = "#0A1E3C")
        ) |> plotly::layout(
          xaxis = c(
            list(
              title = list(text = wrap_vertical_title(var_numeric), standoff = 0L),
              tickformat = "d",  # exibe o número completo (ex: 17000)
              tickfont = list(size = 12, color = "#000000")
            ),
            if(is_percentage) list(range = c(0, 100), dtick = 20) else list()
          ),
          yaxis = list(
            title = list(text = var_category, standoff = 0L),
            tickfont = list(size = 12, color = "#000000")
          )
        )
      } else {
        # Para a orientação vertical: ajuste dos rótulos das categorias
        original_categories <- data[[var_category]]
        categories <- ifelse(
          grepl("^[[:alpha:]]+\\s+[[:alpha:]]+$", original_categories),
          sub("\\s+", "<br>", original_categories),
          gsub("^((\\S+\\s+\\S+))\\s+", "\\1<br>", original_categories)
        )
        p <- plotly::plot_ly(
          data = data,
          x = as.formula(paste0("~`", var_category, "`")),
          y = as.formula(paste0("~`", var_numeric, "`")),
          type = "bar",
          marker = list(color = "#0A1E3C")
        ) |> plotly::layout(
          xaxis = list(
            title = list(text = var_category, standoff = 20L),
            tickmode = "array",
            tickvals = original_categories,
            ticktext = categories,
            tickangle = 90,
            automargin = TRUE,
            tickfont = list(size = 12, color = "#000000")
          ),
          yaxis = c(
            list(
              title = list(text = wrap_after_second(var_numeric, threshold = 19), standoff = 20L, size = 1),
              tickfont = list(size = 12, color = "#000000"),
              tickformat = "d"  # exibe o número completo
            ),
            if(is_percentage) list(range = c(0, 100), dtick = 20) else list()
          ),
          margin = list(b = 90)
        )
      }
      p
    }

    # Dados base
    tabela_APS <- data_list$tabela_APS

    # Atualiza o filtro secundário conforme o nível selecionado
    output$secondary_filter_ui <- renderUI({
      if (input$nivel_selection == "ESTADUAL") {
        return(NULL)
      } else {
        shinyWidgets::pickerInput(
          inputId = ns("secondary_filter"),
          label = "Selecione:",
          choices = NULL,
          options = list(`live-search` = TRUE)
        )
      }
    })

    # Atualiza as escolhas do filtro secundário
    observe({
      req(input$nivel_selection)
      level <- input$nivel_selection
      if(level == "RRAS"){
        choices <- unique(tabela_APS$RRAS)
        shinyWidgets::updatePickerInput(session, "secondary_filter", choices = choices, selected = choices[1])
      } else if(level == "DRS"){
        choices <- unique(tabela_APS$DRS)
        shinyWidgets::updatePickerInput(session, "secondary_filter", choices = choices, selected = choices[1])
      } else if(level == "REGIÃO DE SAÚDE"){
        choices <- unique(tabela_APS$`REGIÃO DE SAÚDE`)
        shinyWidgets::updatePickerInput(session, "secondary_filter", choices = choices, selected = choices[1])
      } else if(level == "MUNICIPIO"){
        choices <- unique(tabela_APS$MUNICIPIO)
        shinyWidgets::updatePickerInput(session, "secondary_filter", choices = choices, selected = choices[1])
      }
    })

    # Filtra dados conforme o nível e o filtro secundário
    filtered_data <- reactive({
      req(input$nivel_selection)
      level <- input$nivel_selection
      if(level == "ESTADUAL"){
        tabela_APS
      } else if(level == "RRAS"){
        req(input$secondary_filter)
        tabela_APS[tabela_APS$RRAS == input$secondary_filter, ]
      } else if(level == "DRS"){
        req(input$secondary_filter)
        tabela_APS[tabela_APS$DRS == input$secondary_filter, ]
      } else if(level == "REGIÃO DE SAÚDE"){
        req(input$secondary_filter)
        tabela_APS[tabela_APS$`REGIÃO DE SAÚDE` == input$secondary_filter, ]
      } else if(level == "MUNICIPIO"){
        req(input$secondary_filter)
        tabela_APS[tabela_APS$MUNICIPIO == input$secondary_filter, ]
      }
    })

    # Dados para gráficos: se ESTADUAL, agregação por RRAS; caso contrário, usa os dados filtrados
    plot_data <- reactive({
      if (input$nivel_selection == "ESTADUAL") {
        aggregate(cbind(`Nº NASCIDOS VIVOS`,
                        `NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO`,
                        `Nº DE UBS`,
                        `GESTANTES SUSDEPENDENTES ESTIMADAS/ANO`,
                        `COBERTURA ANS %`,
                        `COBERTURA ESF %`,
                        `COBERTURA AB %`) ~ RRAS,
                  data = tabela_APS, FUN = sum, na.rm = TRUE)
      } else {
        filtered_data()
      }
    })

    # Dados para tabelas de AAE
    filtered_data_aae <- reactive({
      level <- input$nivel_selection

      if (level == "RRAS") {
        req(input$secondary_filter)
        switch(
          input$secondary_filter,
          "RRAS 1" = data_list$tabela_1_APS_AAE,
          "RRAS 2" = data_list$tabela_2_APS_AAE,
          "RRAS 3" = data_list$tabela_3_APS_AAE,
          "RRAS 4" = data_list$tabela_4_APS_AAE,
          "RRAS 5" = data_list$tabela_5_APS_AAE,
          "RRAS 7" = data_list$tabela_7_APS_AAE,
          "RRAS 8" = data_list$tabela_8_APS_AAE,
          "RRAS 9" = data_list$tabela_9_APS_AAE,
          "RRAS 10" = data_list$tabela_10_APS_AAE,
          "RRAS 11" = data_list$tabela_11_APS_AAE,
          "RRAS 12" = data_list$tabela_12_APS_AAE,
          "RRAS 13" = data_list$tabela_13_APS_AAE,
          "RRAS 14" = data_list$tabela_14_APS_AAE,
          "RRAS 15" = data_list$tabela_15_APS_AAE,
          "RRAS 16" = data_list$tabela_16_APS_AAE,
          "RRAS 17" = data_list$tabela_17_APS_AAE,
          "RRAS 18" = data_list$tabela_18_APS_AAE
        )
      } else {
        table_aae_all <- dplyr::bind_rows(
          data_list$tabela_1_APS_AAE,
          data_list$tabela_2_APS_AAE,
          data_list$tabela_3_APS_AAE,
          data_list$tabela_4_APS_AAE,
          data_list$tabela_5_APS_AAE,
          data_list$tabela_7_APS_AAE,
          data_list$tabela_8_APS_AAE,
          data_list$tabela_9_APS_AAE,
          data_list$tabela_10_APS_AAE,
          data_list$tabela_11_APS_AAE,
          data_list$tabela_12_APS_AAE,
          data_list$tabela_13_APS_AAE,
          data_list$tabela_14_APS_AAE,
          data_list$tabela_15_APS_AAE,
          data_list$tabela_16_APS_AAE,
          data_list$tabela_17_APS_AAE,
          data_list$tabela_18_APS_AAE
        )
        if (level == "ESTADUAL") {
          table_aae_all
        } else if (level == "DRS") {
          req(input$secondary_filter)
          dados <- table_aae_all[table_aae_all$DRS == input$secondary_filter, ]
          dados <- dados %>%
            rename("MUNICÍPIO DA DRS" = "MUNICÍPIO DA RRAS")
        } else if (level == "REGIÃO DE SAÚDE") {
          req(input$secondary_filter)
          dados <- table_aae_all[table_aae_all$`REGIÃO DE SAÚDE` == input$secondary_filter, ]
          dados <- dados %>%
            rename("MUNICÍPIO DA REGIÃO DE SAÚDE" = "MUNICÍPIO DA RRAS")
        } else if (level == "MUNICIPIO") {
          req(input$secondary_filter)
          table_aae_all[table_aae_all$`MUNICÍPIO DA RRAS` == input$secondary_filter, ]
        }
      }
    })

    # Dados para tabelas de BAIXO RISCO
    filtered_data_bxr <- reactive({
      level <- input$nivel_selection
      if(level == "RRAS"){
        req(input$secondary_filter)
        switch(
          input$secondary_filter,
          "RRAS 1" = data_list$tabela_1_APS_BXRISCO,
          "RRAS 2" = data_list$tabela_2_APS_BXRISCO,
          "RRAS 3" = data_list$tabela_3_APS_BXRISCO,
          "RRAS 4" = data_list$tabela_4_APS_BXRISCO,
          "RRAS 5" = data_list$tabela_5_APS_BXRISCO,
          "RRAS 7" = data_list$tabela_7_APS_BXRISCO,
          "RRAS 8" = data_list$tabela_8_APS_BXRISCO,
          "RRAS 9" = data_list$tabela_9_APS_BXRISCO,
          "RRAS 10" = data_list$tabela_10_APS_BXRISCO,
          "RRAS 11" = data_list$tabela_11_APS_BXRISCO,
          "RRAS 12" = data_list$tabela_12_APS_BXRISCO,
          "RRAS 13" = data_list$tabela_13_APS_BXRISCO,
          "RRAS 14" = data_list$tabela_14_APS_BXRISCO,
          "RRAS 15" = data_list$tabela_15_APS_BXRISCO,
          "RRAS 16" = data_list$tabela_16_APS_BXRISCO,
          "RRAS 17" = data_list$tabela_17_APS_BXRISCO,
          "RRAS 18" = data_list$tabela_18_APS_BXRISCO
        )
      } else {
        table_bxr_all <- dplyr::bind_rows(
          data_list$tabela_1_APS_BXRISCO,
          data_list$tabela_2_APS_BXRISCO,
          data_list$tabela_3_APS_BXRISCO,
          data_list$tabela_4_APS_BXRISCO,
          data_list$tabela_5_APS_BXRISCO,
          data_list$tabela_7_APS_BXRISCO,
          data_list$tabela_8_APS_BXRISCO,
          data_list$tabela_9_APS_BXRISCO,
          data_list$tabela_10_APS_BXRISCO,
          data_list$tabela_11_APS_BXRISCO,
          data_list$tabela_12_APS_BXRISCO,
          data_list$tabela_13_APS_BXRISCO,
          data_list$tabela_14_APS_BXRISCO,
          data_list$tabela_15_APS_BXRISCO,
          data_list$tabela_16_APS_BXRISCO,
          data_list$tabela_17_APS_BXRISCO,
          data_list$tabela_18_APS_BXRISCO
        )
        if(level == "ESTADUAL"){
          table_bxr_all
        } else if(level == "DRS"){
          req(input$secondary_filter)
          dados <- table_bxr_all[table_bxr_all$DRS == input$secondary_filter, ]
          dados <- dados %>%
            rename("MUNICÍPIO DA DRS" = "MUNICÍPIO DA RRAS")
        } else if(level == "REGIÃO DE SAÚDE"){
          req(input$secondary_filter)
          dados <- table_bxr_all[table_bxr_all$`REGIÃO DE SAÚDE` == input$secondary_filter, ]
          dados <- dados %>%
            rename("MUNICÍPIO DA REGIÃO DE SAÚDE" = "MUNICÍPIO DA RRAS")
        } else if(level == "MUNICIPIO"){
          req(input$secondary_filter)
          table_bxr_all[table_bxr_all$`MUNICÍPIO DA RRAS` == input$secondary_filter, ]
        }
      }
    })

    # Renderiza as tabelas
    output$table_aae <- DT::renderDT({
      data <- filtered_data_aae()
      validate(
        need(!is.null(data) && ncol(data) > 0, "Dados não disponíveis para exibição")
      )
      DT::datatable(
        data,
        options = list(
          pageLength = -1,
          autoWidth  = TRUE,
          scrollX    = TRUE,
          scrollY    = "400px",
          scrollCollapse = TRUE,
          paging     = FALSE,
          dom        = 't',
          columnDefs = list(
            list(className = "dt-center", targets = "_all"),
            list(width = '10%', targets = "_all")
          )
        ),
        rownames = FALSE,
        class = "compact stripe hover nowrap"
      ) |> DT::formatStyle(
        columns = names(filtered_data_aae()),
        `padding-left` = '0px',
        `padding-right` = '0px'
      )
    })

    output$table_bxr <- DT::renderDT({
      data <- filtered_data_bxr()
      validate(
        need(!is.null(data) && ncol(data) > 0, "Dados não disponíveis para exibição")
      )
      DT::datatable(
        data,
        options = list(
          pageLength = -1,
          autoWidth  = TRUE,
          scrollX    = TRUE,
          scrollY    = "400px",
          scrollCollapse = TRUE,
          paging     = FALSE,
          dom        = 't',
          columnDefs = list(
            list(className = "dt-center", targets = "_all"),
            list(width = '10%', targets = "_all")
          )
        ),
        rownames = FALSE,
        class = "compact stripe hover nowrap"
      ) |> DT::formatStyle(
        columns = names(filtered_data_bxr()),
        `padding-left` = '0px',
        `padding-right` = '0px'
      )
    })

    # Caixas resumo principais
    output$summary_box_1 <- renderUI({
      total_nascidos <- round(sum(filtered_data()[["Nº NASCIDOS VIVOS"]], na.rm = TRUE))
      div(
        class = "custom-box box-primary",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center;",
        h4("Total de Nascidos Vivos (2023)"),
        h3(total_nascidos)
      )
    })

    output$summary_box_2 <- renderUI({
      total_sus_nasc <- sum(filtered_data()[["NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO"]], na.rm = TRUE)
      div(
        class = "custom-box box-success",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center;",
        h4("Nascidos vivos SUSdependentes estimados/ano"),
        h3(ceiling(total_sus_nasc))
      )
    })

    output$summary_box_3 <- renderUI({
      total_ubs <- sum(filtered_data()[["Nº DE UBS"]], na.rm = TRUE)
      div(
        class = "custom-box box-danger",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center;",
        h4("Total de UBS"),
        h3(total_ubs)
      )
    })

    output$summary_box_4 <- renderUI({
      total_gestantes <- sum(filtered_data()[["GESTANTES SUSDEPENDENTES ESTIMADAS/ANO"]], na.rm = TRUE)
      div(
        class = "custom-box box-warning",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center;",
        h4("Gestantes SUSdependentes estimadas/ano"),
        h3(ceiling(total_gestantes))
      )
    })

    # Caixas resumo extras para nível MUNICIPIO
    output$extra_summary_box_1 <- renderUI({
      req(input$nivel_selection == "MUNICIPIO")
      data <- filtered_data()
      metric <- round(mean(data$`COBERTURA ANS %`, na.rm = TRUE), 2)
      div(
        class = "custom-box box-primary",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center;",
        h4("Cobertura ANS (%)"),
        h3(metric)
      )
    })

    output$extra_summary_box_2 <- renderUI({
      req(input$nivel_selection == "MUNICIPIO")
      data <- filtered_data()
      metric <- round(mean(data$`COBERTURA ESF %`, na.rm = TRUE), 2)
      div(
        class = "custom-box box-success",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center;",
        h4("Cobertura ESF (%)"),
        h3(metric)
      )
    })

    output$extra_summary_box_3 <- renderUI({
      req(input$nivel_selection == "MUNICIPIO")
      data <- filtered_data()
      metric <- round(mean(data$`COBERTURA AB %`, na.rm = TRUE), 2)
      div(
        class = "custom-box box-warning",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center;",
        h4("Cobertura AB (%)"),
        h3(metric)
      )
    })

    # Renderização dos cards com gráficos, usando a função auxiliar build_plot_card
    output$card_plot_nascidos_vivos <- renderUI({
      req(input$nivel_selection)
      if(input$nivel_selection == "MUNICIPIO") return(NULL)
      build_plot_card("Nascidos Vivos", "plot_nascidos_vivos", plot_data(), caption = "Ano: 2023")
    })
    output$card_plot_ubs <- renderUI({
      req(input$nivel_selection)
      if(input$nivel_selection == "MUNICIPIO") return(NULL)
      build_plot_card("Número de UBS", "plot_ubs", plot_data(), caption = "Ano: 2020")
    })
    output$card_plot_gestantes_susdependentes <- renderUI({
      req(input$nivel_selection)
      if(input$nivel_selection == "MUNICIPIO") return(NULL)
      build_plot_card("Gestantes SUSdependentes", "plot_gestantes_susdependentes", plot_data(), caption = "Ano: 2020")
    })
    # Para nível ESTADUAL
    output$card_plot_nascidos_susdependentes_estadual <- renderUI({
      req(input$nivel_selection)
      if(input$nivel_selection != "ESTADUAL") return(NULL)
      build_plot_card("Nascidos Vivos SUSdependentes", "plot_nascidos_susdependentes_estado", plot_data(), caption = "Ano: 2020")
    })

    # Para níveis RRAS, DRS ou REGIÃO DE SAÚDE
    output$card_plot_nascidos_susdependentes_outros <- renderUI({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      build_plot_card("Nascidos Vivos SUSdependentes", "plot_nascidos_susdependentes_outros", plot_data(), caption = "Ano: 2020")
    })


    # --- NOVOS CARDS DE COBERTURA (apenas para RRAS, DRS e REGIÃO DE SAÚDE) ---
    output$card_plot_cobertura_ans <- renderUI({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      build_plot_card("Cobertura ANS (%)", "plot_cobertura_ans", plot_data(), caption = "Ano: 2020")
    })
    output$card_plot_cobertura_esf <- renderUI({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      build_plot_card("Cobertura ESF (%)", "plot_cobertura_esf", plot_data(), caption = "Ano: 2020")
    })
    output$card_plot_cobertura_ab <- renderUI({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      build_plot_card("Cobertura AB (%)", "plot_cobertura_ab", plot_data(), caption = "Ano: 2020")
    })

    # Renderização dos gráficos utilizando a função auxiliar build_bar_plot
    output$plot_nascidos_vivos <- plotly::renderPlotly({
      req(input$nivel_selection)
      if(input$nivel_selection == "MUNICIPIO") return(NULL)
      # Se estadual, a variável categórica é RRAS; caso contrário, MUNICIPIO
      cat_var <- if(input$nivel_selection == "ESTADUAL") "RRAS" else "MUNICIPIO"
      build_bar_plot(data = plot_data(), var_numeric = "Nº NASCIDOS VIVOS", var_category = cat_var)
    })
    output$plot_ubs <- plotly::renderPlotly({
      req(input$nivel_selection)
      if(input$nivel_selection == "MUNICIPIO") return(NULL)
      cat_var <- if(input$nivel_selection == "ESTADUAL") "RRAS" else "MUNICIPIO"
      build_bar_plot(data = plot_data(), var_numeric = "Nº DE UBS", var_category = cat_var)
    })
    output$plot_gestantes_susdependentes <- plotly::renderPlotly({
      req(input$nivel_selection)
      if(input$nivel_selection == "MUNICIPIO") return(NULL)
      cat_var <- if(input$nivel_selection == "ESTADUAL") "RRAS" else "MUNICIPIO"
      build_bar_plot(data = plot_data(), var_numeric = "GESTANTES SUSDEPENDENTES ESTIMADAS/ANO", var_category = cat_var)
    })
    output$plot_nascidos_susdependentes_estado <- plotly::renderPlotly({
      req(input$nivel_selection)
      if(input$nivel_selection == "MUNICIPIO") return(NULL)
      cat_var <- if(input$nivel_selection == "ESTADUAL") "RRAS" else "MUNICIPIO"
      build_bar_plot(data = plot_data(), var_numeric = "NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO", var_category = cat_var)
    })
    output$plot_nascidos_susdependentes_outros <- plotly::renderPlotly({
      req(input$nivel_selection)
      if(input$nivel_selection == "MUNICIPIO") return(NULL)
      cat_var <- if(input$nivel_selection == "ESTADUAL") "RRAS" else "MUNICIPIO"
      build_bar_plot(data = plot_data(), var_numeric = "NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO", var_category = cat_var)
    })

    # --- GRÁFICOS DE COBERTURA (apenas para RRAS, DRS e REGIÃO DE SAÚDE) ---
    output$plot_cobertura_ans <- plotly::renderPlotly({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      build_bar_plot(data = plot_data(), var_numeric = "COBERTURA ANS %", var_category = "MUNICIPIO", is_percentage = TRUE)
    })
    output$plot_cobertura_esf <- plotly::renderPlotly({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      build_bar_plot(data = plot_data(), var_numeric = "COBERTURA ESF %", var_category = "MUNICIPIO", is_percentage = TRUE)
    })
    output$plot_cobertura_ab <- plotly::renderPlotly({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      build_bar_plot(data = plot_data(), var_numeric = "COBERTURA AB %", var_category = "MUNICIPIO", is_percentage = TRUE)
    })

  })
}
