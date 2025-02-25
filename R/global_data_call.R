# Em R/global_data_call.R
#' Carrega os dados para o app
#'
#' Lê os arquivos Excel da pasta inst/app/data, realiza a transformação (como converter colunas de texto para uppercase e ajustar o tipo das colunas) e retorna uma lista com todas as tabelas.
#'
#' @return Uma lista com os datasets: tabela_APS, tabela_1_APS_AAE, tabela_1_APS_BXRISCO, etc.
#'
#' @importFrom magrittr %>%
#' @noRd
load_data <- function() {

  # Função para converter colunas de texto para uppercase
  to_upper_df <- function(df) {
    df[] <- lapply(df, function(x) if (is.character(x)) toupper(x) else x)
    df
  }

  # Caminhos dos arquivos Excel dentro de inst/app/data
  remap1 <- file.path(app_sys("app", "data"), "remap1.xlsx")
  remap2 <- file.path(app_sys("app", "data"), "remap2.xlsx")
  remap3 <- file.path(app_sys("app", "data"), "remap3.xlsx")
  remap4 <- file.path(app_sys("app", "data"), "remap4.xlsx")
  remap5 <- file.path(app_sys("app", "data"), "remap5.xlsx")
  remap7 <- file.path(app_sys("app", "data"), "remap7.xlsx")
  remap8 <- file.path(app_sys("app", "data"), "remap8.xlsx")
  remap9 <- file.path(app_sys("app", "data"), "remap9.xlsx")
  remap10 <- file.path(app_sys("app", "data"), "remap10.xlsx")
  remap11 <- file.path(app_sys("app", "data"), "remap11.xlsx")
  remap12 <- file.path(app_sys("app", "data"), "remap12.xlsx")
  remap13 <- file.path(app_sys("app", "data"), "remap13.xlsx")
  remap14 <- file.path(app_sys("app", "data"), "remap14.xlsx")
  remap15 <- file.path(app_sys("app", "data"), "remap15.xlsx")
  remap16 <- file.path(app_sys("app", "data"), "remap16.xlsx")
  remap17 <- file.path(app_sys("app", "data"), "remap17.xlsx")
  remap18 <- file.path(app_sys("app", "data"), "remap18.xlsx")
  rras_municipio_path <- file.path(app_sys("app", "data"), "RRAS-MUNICIPIO.xlsx")

  # Leitura das planilhas com transformação para uppercase
  tabela_1_APS         <- to_upper_df(readxl::read_excel(remap1, sheet = "Tabela 1 APS - Dados"))
  tabela_1_APS_AAE     <- to_upper_df(readxl::read_excel(remap1, sheet = "Tabela 1 APS - Referência AAE"))
  tabela_1_APS_BXRISCO <- to_upper_df(readxl::read_excel(remap1, sheet = "Tabela 1 APS - Referência Bx. R"))

  tabela_1_APS_AAE$CNES <- as.numeric(tabela_1_APS_AAE$CNES) # CNES está como character nessa planilha
  tabela_1_APS_BXRISCO$CNES <- as.numeric(tabela_1_APS_BXRISCO$CNES) # CNES está como character nessa planilha

  tabela_2_APS         <- to_upper_df(readxl::read_excel(remap2, sheet = "Tabela 1 APS - Dados"))
  tabela_2_APS_AAE     <- to_upper_df(readxl::read_excel(remap2, sheet = "Tabela 1 APS - Referência AAE"))
  tabela_2_APS_BXRISCO <- to_upper_df(readxl::read_excel(remap2, sheet = "Tabela 1 APS - Referência Bx. R"))

  tabela_2_APS_AAE$CNES <- as.numeric(tabela_2_APS_AAE$CNES) # CNES está como character nessa planilha
  tabela_2_APS_BXRISCO$CNES <- as.numeric(tabela_2_APS_BXRISCO$CNES) # CNES está como character nessa planilha

  tabela_3_APS         <- to_upper_df(readxl::read_excel(remap3, sheet = "Tabela 1 APS - Dados"))
  tabela_3_APS_AAE     <- to_upper_df(readxl::read_excel(remap3, sheet = "Tabela 1 APS - Referência AAE"))
  tabela_3_APS_BXRISCO <- to_upper_df(readxl::read_excel(remap3, sheet = "Tabela 1 APS - Referência Bx. R"))

  tabela_3_APS_AAE$CNES <- as.numeric(tabela_3_APS_AAE$CNES) # CNES está como character nessa planilha
  tabela_3_APS_BXRISCO$CNES <- as.numeric(tabela_3_APS_BXRISCO$CNES) # CNES está como character nessa planilha

  tabela_4_APS         <- to_upper_df(readxl::read_excel(remap4, sheet = "Tabela 1 APS - Dados"))
  tabela_4_APS_AAE     <- to_upper_df(readxl::read_excel(remap4, sheet = "Tabela 1 APS - Referência AAE"))
  tabela_4_APS_BXRISCO <- to_upper_df(readxl::read_excel(remap4, sheet = "Tabela 1 APS - Referência Bx. R"))

  tabela_4_APS_AAE$CNES <- as.numeric(tabela_4_APS_AAE$CNES) # CNES está como character nessa planilha
  tabela_4_APS_BXRISCO$CNES <- as.numeric(tabela_4_APS_BXRISCO$CNES) # CNES está como character nessa planilha

  tabela_5_APS         <- to_upper_df(readxl::read_excel(remap5, sheet = "Tabela 1 APS - Dados"))
  tabela_5_APS_AAE     <- to_upper_df(readxl::read_excel(remap5, sheet = "Tabela 1 APS - Referência AAE"))
  tabela_5_APS_BXRISCO <- to_upper_df(readxl::read_excel(remap5, sheet = "Tabela 1 APS - Referência Bx. R"))

  tabela_5_APS_AAE$CNES <- as.numeric(tabela_5_APS_AAE$CNES) # CNES está como character nessa planilha
  tabela_5_APS_BXRISCO$CNES <- as.numeric(tabela_5_APS_BXRISCO$CNES) # CNES está como character nessa planilha

  tabela_7_APS         <- to_upper_df(readxl::read_excel(remap7, sheet = "Tabela 1 APS - Dados"))
  tabela_7_APS_AAE     <- to_upper_df(readxl::read_excel(remap7, sheet = "Tabela 1 APS - Referência AAE"))
  tabela_7_APS_BXRISCO <- to_upper_df(readxl::read_excel(remap7, sheet = "Tabela 1 APS - Referência Bx. R"))

  tabela_7_APS_AAE$CNES <- as.numeric(tabela_7_APS_AAE$CNES) # CNES está como character nessa planilha
  tabela_7_APS_BXRISCO$CNES <- as.numeric(tabela_7_APS_BXRISCO$CNES) # CNES está como character nessa planilha

  tabela_8_APS         <- to_upper_df(readxl::read_excel(remap8, sheet = "Tabela 1 APS - Dados"))
  tabela_8_APS_AAE     <- to_upper_df(readxl::read_excel(remap8, sheet = "Tabela 1 APS - Referência AAE"))
  tabela_8_APS_BXRISCO <- to_upper_df(readxl::read_excel(remap8, sheet = "Tabela 1 APS - Referência Bx. R"))

  tabela_8_APS_AAE$CNES <- as.numeric(tabela_8_APS_AAE$CNES) # CNES está como character nessa planilha
  tabela_8_APS_BXRISCO$CNES <- as.numeric(tabela_8_APS_BXRISCO$CNES) # CNES está como character nessa planilha

  tabela_9_APS         <- to_upper_df(readxl::read_excel(remap9, sheet = "Tabela 1 APS - Dados"))
  tabela_9_APS_AAE     <- to_upper_df(readxl::read_excel(remap9, sheet = "Tabela 1 APS - Referência AAE"))
  tabela_9_APS_BXRISCO <- to_upper_df(readxl::read_excel(remap9, sheet = "Tabela 1 APS - Referência Bx. R"))

  tabela_9_APS_AAE$CNES <- as.numeric(tabela_9_APS_AAE$CNES) # CNES está como character nessa planilha
  tabela_9_APS_BXRISCO$CNES <- as.numeric(tabela_9_APS_BXRISCO$CNES) # CNES está como character nessa planilha

  tabela_10_APS         <- to_upper_df(readxl::read_excel(remap10, sheet = "Tabela 1 APS - Dados"))
  tabela_10_APS_AAE     <- to_upper_df(readxl::read_excel(remap10, sheet = "Tabela 1 APS - Referência AAE"))
  tabela_10_APS_BXRISCO <- to_upper_df(readxl::read_excel(remap10, sheet = "Tabela 1 APS - Referência Bx. R"))

  tabela_10_APS_AAE$CNES <- as.numeric(tabela_10_APS_AAE$CNES) # CNES está como character nessa planilha
  tabela_10_APS_BXRISCO$CNES <- as.numeric(tabela_10_APS_BXRISCO$CNES) # CNES está como character nessa planilha

  tabela_11_APS         <- to_upper_df(readxl::read_excel(remap11, sheet = "Tabela 1 APS - Dados"))
  tabela_11_APS_AAE     <- to_upper_df(readxl::read_excel(remap11, sheet = "Tabela 1 APS - Referência AAE"))
  tabela_11_APS_BXRISCO <- to_upper_df(readxl::read_excel(remap11, sheet = "Tabela 1 APS - Referência Bx. R"))

  tabela_11_APS_AAE$CNES <- as.numeric(tabela_11_APS_AAE$CNES) # CNES está como character nessa planilha
  tabela_11_APS_BXRISCO$CNES <- as.numeric(tabela_11_APS_BXRISCO$CNES) # CNES está como character nessa planilha

  tabela_12_APS         <- to_upper_df(readxl::read_excel(remap12, sheet = "Tabela 1 APS - Dados"))
  tabela_12_APS_AAE     <- to_upper_df(readxl::read_excel(remap12, sheet = "Tabela 1 APS - Referência AAE"))
  tabela_12_APS_BXRISCO <- to_upper_df(readxl::read_excel(remap12, sheet = "Tabela 1 APS - Referência Bx. R"))

  tabela_12_APS_AAE$CNES <- as.numeric(tabela_12_APS_AAE$CNES) # CNES está como character nessa planilha
  tabela_12_APS_BXRISCO$CNES <- as.numeric(tabela_12_APS_BXRISCO$CNES) # CNES está como character nessa planilha

  tabela_13_APS         <- to_upper_df(readxl::read_excel(remap13, sheet = "Tabela 1 APS - Dados"))
  tabela_13_APS_AAE     <- to_upper_df(readxl::read_excel(remap13, sheet = "Tabela 1 APS - Referência AAE"))
  tabela_13_APS_BXRISCO <- to_upper_df(readxl::read_excel(remap13, sheet = "Tabela 1 APS - Referência Bx. R"))

  tabela_13_APS_AAE$CNES <- as.numeric(tabela_13_APS_AAE$CNES) # CNES está como character nessa planilha
  tabela_13_APS_BXRISCO$CNES <- as.numeric(tabela_13_APS_BXRISCO$CNES) # CNES está como character nessa planilha

  tabela_14_APS         <- to_upper_df(readxl::read_excel(remap14, sheet = "Tabela 1 APS - Dados"))
  tabela_14_APS_AAE     <- to_upper_df(readxl::read_excel(remap14, sheet = "Tabela 1 APS - Referência AAE"))
  tabela_14_APS_BXRISCO <- to_upper_df(readxl::read_excel(remap14, sheet = "Tabela 1 APS - Referência Bx. R"))

  tabela_14_APS_AAE$CNES <- as.numeric(tabela_14_APS_AAE$CNES) # CNES está como character nessa planilha
  tabela_14_APS_BXRISCO$CNES <- as.numeric(tabela_14_APS_BXRISCO$CNES) # CNES está como character nessa planilha

  tabela_15_APS         <- to_upper_df(readxl::read_excel(remap15, sheet = "Tabela 1 APS - Dados"))
  tabela_15_APS_AAE     <- to_upper_df(readxl::read_excel(remap15, sheet = "Tabela 1 APS - Referência AAE"))
  tabela_15_APS_BXRISCO <- to_upper_df(readxl::read_excel(remap15, sheet = "Tabela 1 APS - Referência Bx. R"))

  tabela_15_APS_AAE$CNES <- as.numeric(tabela_15_APS_AAE$CNES) # CNES está como character nessa planilha
  tabela_15_APS_BXRISCO$CNES <- as.numeric(tabela_15_APS_BXRISCO$CNES) # CNES está como character nessa planilha

  tabela_16_APS         <- to_upper_df(readxl::read_excel(remap16, sheet = "Tabela 1 APS - Dados"))
  tabela_16_APS_AAE     <- to_upper_df(readxl::read_excel(remap16, sheet = "Tabela 1 APS - Referência AAE"))
  tabela_16_APS_BXRISCO <- to_upper_df(readxl::read_excel(remap16, sheet = "Tabela 1 APS - Referência Bx. R"))

  tabela_16_APS_AAE$CNES <- as.numeric(tabela_16_APS_AAE$CNES) # CNES está como character nessa planilha
  tabela_16_APS_BXRISCO$CNES <- as.numeric(tabela_16_APS_BXRISCO$CNES) # CNES está como character nessa planilha

  tabela_17_APS         <- to_upper_df(readxl::read_excel(remap17, sheet = "Tabela 1 APS - Dados"))
  tabela_17_APS_AAE     <- to_upper_df(readxl::read_excel(remap17, sheet = "Tabela 1 APS - Referência AAE"))
  tabela_17_APS_BXRISCO <- to_upper_df(readxl::read_excel(remap17, sheet = "Tabela 1 APS - Referência Bx. R"))

  tabela_17_APS_AAE$CNES <- as.numeric(tabela_17_APS_AAE$CNES) # CNES está como character nessa planilha
  tabela_17_APS_BXRISCO$CNES <- as.numeric(tabela_17_APS_BXRISCO$CNES) # CNES está como character nessa planilha

  tabela_18_APS         <- to_upper_df(readxl::read_excel(remap18, sheet = "Tabela 1 APS - Dados"))
  tabela_18_APS_AAE     <- to_upper_df(readxl::read_excel(remap18, sheet = "Tabela 1 APS - Referência AAE"))
  tabela_18_APS_BXRISCO <- to_upper_df(readxl::read_excel(remap18, sheet = "Tabela 1 APS - Referência Bx. R"))

  tabela_18_APS_AAE$CNES <- as.numeric(tabela_18_APS_AAE$CNES) # CNES está como character nessa planilha
  tabela_18_APS_BXRISCO$CNES <- as.numeric(tabela_18_APS_BXRISCO$CNES) # CNES está como character nessa planilha

  # Criando coluna RRAS nas tabelas de APS
  tabela_1_APS$RRAS <- "RRAS 1"
  tabela_2_APS$RRAS <- "RRAS 2"
  tabela_3_APS$RRAS <- "RRAS 3"
  tabela_4_APS$RRAS <- "RRAS 4"
  tabela_5_APS$RRAS <- "RRAS 5"
  tabela_7_APS$RRAS <- "RRAS 7"
  tabela_8_APS$RRAS <- "RRAS 8"
  tabela_9_APS$RRAS <- "RRAS 9"
  tabela_10_APS$RRAS <- "RRAS 10"
  tabela_11_APS$RRAS <- "RRAS 11"
  tabela_12_APS$RRAS <- "RRAS 12"
  tabela_13_APS$RRAS <- "RRAS 13"
  tabela_14_APS$RRAS <- "RRAS 14"
  tabela_15_APS$RRAS <- "RRAS 15"
  tabela_16_APS$RRAS <- "RRAS 16"
  tabela_17_APS$RRAS <- "RRAS 17"
  tabela_18_APS$RRAS <- "RRAS 18"

  # Unindo dados APS
  # tabela_APS <- dplyr::bind_rows(tabela_1_APS, tabela_2_APS, tabela_3_APS, tabela_4_APS, tabela_5_APS)
  tabela_APS <- rbind(
    tabela_1_APS,
    tabela_2_APS,
    tabela_3_APS,
    tabela_4_APS,
    tabela_5_APS,
    tabela_7_APS,
    tabela_8_APS,
    tabela_9_APS,
    tabela_10_APS,
    tabela_11_APS,
    tabela_12_APS,
    tabela_13_APS,
    tabela_14_APS,
    tabela_15_APS,
    tabela_16_APS,
    tabela_17_APS,
    tabela_18_APS
  )

  # Renomeando colunas de APS (garantindo uppercase)
  colnames(tabela_APS) <- toupper(c(
    "DRS",
    "REGIÃO DE SAÚDE",
    "MUNICIPIO",
    "NASCIDOS VIVOS 2023",
    "COBERTURA ANS %",
    "Nº DE UBS",
    "COBERTURA DA ESF/MUNICÍPIO %",
    "COBERTURA DA AB/MUNICÍPIO %",
    "TOTAL DE GESTANTES SUSDEPENDENTES ESTIMADAS",
    "TOTAL DE NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS",
    "RRAS"
  ))

  # Classe das colunas
  tabela_APS <- tabela_APS %>%
    dplyr::mutate(
      DRS = as.character(DRS),
      `REGIÃO DE SAÚDE` = as.character(`REGIÃO DE SAÚDE`),
      MUNICIPIO = as.character(MUNICIPIO),
      RRAS = as.character(RRAS)
    ) %>%
    dplyr::mutate(across(-c(DRS, `REGIÃO DE SAÚDE`, MUNICIPIO, RRAS), ~ as.numeric(as.character(.))))

  # Leitura da planilha RRAS-MUNICIPIO e padronização
  rras_municipio <- to_upper_df(readxl::read_excel(rras_municipio_path))

  # Retorna lista com todos os dados
  list(
    tabela_APS = tabela_APS,
    tabela_1_APS_AAE = tabela_1_APS_AAE,
    tabela_1_APS_BXRISCO = tabela_1_APS_BXRISCO,
    tabela_2_APS_AAE = tabela_2_APS_AAE,
    tabela_2_APS_BXRISCO = tabela_2_APS_BXRISCO,
    tabela_3_APS_AAE = tabela_3_APS_AAE,
    tabela_3_APS_BXRISCO = tabela_3_APS_BXRISCO,
    tabela_4_APS_AAE = tabela_4_APS_AAE,
    tabela_4_APS_BXRISCO = tabela_4_APS_BXRISCO,
    tabela_5_APS_AAE = tabela_5_APS_AAE,
    tabela_5_APS_BXRISCO = tabela_5_APS_BXRISCO,
    tabela_7_APS_AAE = tabela_7_APS_AAE,
    tabela_7_APS_BXRISCO = tabela_7_APS_BXRISCO,
    tabela_8_APS_AAE = tabela_8_APS_AAE,
    tabela_8_APS_BXRISCO = tabela_8_APS_BXRISCO,
    tabela_9_APS_AAE = tabela_9_APS_AAE,
    tabela_9_APS_BXRISCO = tabela_9_APS_BXRISCO,
    tabela_10_APS_AAE = tabela_10_APS_AAE,
    tabela_10_APS_BXRISCO = tabela_10_APS_BXRISCO,
    tabela_11_APS_AAE = tabela_11_APS_AAE,
    tabela_11_APS_BXRISCO = tabela_11_APS_BXRISCO,
    tabela_12_APS_AAE = tabela_12_APS_AAE,
    tabela_12_APS_BXRISCO = tabela_12_APS_BXRISCO,
    tabela_13_APS_AAE = tabela_13_APS_AAE,
    tabela_13_APS_BXRISCO = tabela_13_APS_BXRISCO,
    tabela_14_APS_AAE = tabela_14_APS_AAE,
    tabela_14_APS_BXRISCO = tabela_14_APS_BXRISCO,
    tabela_15_APS_AAE = tabela_15_APS_AAE,
    tabela_15_APS_BXRISCO = tabela_15_APS_BXRISCO,
    tabela_16_APS_AAE = tabela_16_APS_AAE,
    tabela_16_APS_BXRISCO = tabela_16_APS_BXRISCO,
    tabela_17_APS_AAE = tabela_17_APS_AAE,
    tabela_17_APS_BXRISCO = tabela_17_APS_BXRISCO,
    tabela_18_APS_AAE = tabela_18_APS_AAE,
    tabela_18_APS_BXRISCO = tabela_18_APS_BXRISCO,
    rras_municipio = rras_municipio
  )
}
