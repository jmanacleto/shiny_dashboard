# install.packages("lme4")
 library(lme4)

analise_credito_server <- function(input, output, session, dados_modelagem){
  
 # Carregando os dados -----------------------------------------------------------------
  modelo <- readRDS("dados/analise_credito/modelo.rds")
  Tabela <- readRDS("dados/analise_credito/Tabela.rds")
  sen_espCN <- readRDS("dados/analise_credito/sen_espCN.rds")
  
  df_valor_inadimplencia <- readRDS("dados/analise_credito/df_valor_inadimplencia.rds")
  df_clientes_inadimplentes <- readRDS("dados/analise_credito/df_clientes_inadimplentes.rds")
  df_pct_clientes_ativos_inadimplentes <- readRDS("dados/analise_credito/df_pct_clientes_inadimplentes_ativos.rds")

 # Reactives ----------------------------------------------------------------
dados_simulacao <- reactive({
  
  input_uf <- input$input_uf
  input_num_parcelas <- input$input_num_parcelas
  input_prazo_total <- input$input_prazo_total
  input_valor_compra <- input$input_valor_compra
  
  
  ValordaNLOG <- log(input_valor_compra) - mean(log(dados_modelagem$ValordaNF))
  
  NParcelas <- input_num_parcelas - 3
  PrazoTotal <- input_prazo_total - 150
  Descricao <- "PRODUTO XPTO"
  UF <- input_uf
  GrupoF <- "AAA"
  ID <- "AAA"
  NC <- 1
  CompraPMesL <- quantile(dados_modelagem$ComprasPMesL, probs = .15) %>% as.numeric()
  AnoF <- year(Sys.Date())
  Mes <- month(Sys.Date())
  
  BasePredicao <- data.frame(ValordaNLOG)
  
  BasePredicao <- BasePredicao %>%
    mutate(
      NParcela = as.numeric(NParcelas),
      PrazoTotal = as.numeric(PrazoTotal),
      Descricao = Descricao,
      UF = UF,
      GrupoF = GrupoF,
      ID = ID,
      NC =NC,
      CompraPMesL = CompraPMesL,
      AnoF = AnoF,
      Mes = Mes
    )
  
  
    summary_modelo <- summary(modelo)
    Int <- summary_modelo$coefficients[1,1]
    Int_Nparc <- summary_modelo$coefficients[2,1]
    Int_Valor <- summary_modelo$coefficients[3,1]
    Int_PrazoT <- summary_modelo$coefficients[4,1]
    Int_Comp <- summary_modelo$coefficients[5,1]
    Int_Nparc_valor <- summary_modelo$coefficients[6,1]
    
    
    TABRANEF1 <- readRDS("dados/analise_credito/TABRANEF1.rds")
    data_tab1 <- TABRANEF1 %>% filter(IDCNPJ == BasePredicao$ID)
    Int_CNPJ <- ifelse(nrow(data_tab1) > 0, data_tab1$Int_IDCNPJ, 0)
    Int_NC <- ifelse(nrow(data_tab1) > 0, data_tab1$NC_IDCNPJ, 0)
    
    TABRANEF2 <- readRDS("dados/analise_credito/TABRANEF2.rds")
    data_tab2 <- TABRANEF2 %>% filter(IDGC == BasePredicao$GrupoF)
    Int_GC <- ifelse(nrow(data_tab2) > 0, data_tab2$Int_IDGC, 0)
    
    TABRANEF4 <- readRDS("dados/analise_credito/TABRANEF4.rds")
    data_tab4 <- TABRANEF4 %>% filter(IDUF == BasePredicao$UF)
    Int_UF <- ifelse(nrow(data_tab4) > 0, data_tab4$Int_UF, 0)
    
    TABRANEF5 <- readRDS("dados/analise_credito/TABRANEF5.rds")
    data_tab5 <- TABRANEF5 %>% filter(IDDes == BasePredicao$Descricao)
    Int_Des <- ifelse(nrow(data_tab5) > 0, data_tab5$Int_Des, 0)
    
    TABRANEF6 <- readRDS("dados/analise_credito/TABRANEF6.rds")
    data_tab6 <- TABRANEF6 %>% filter(IDMes == BasePredicao$Mes)
    Int_Mes <- ifelse(nrow(data_tab6) > 0, data_tab6$Int_Mes, 0)
    
    TABRANEF7 <- readRDS("dados/analise_credito/TABRANEF7.rds")
    data_tab7 <- TABRANEF7 %>% filter(IDAnoF == BasePredicao$AnoF)
    Int_Ano <- ifelse(nrow(data_tab7) > 0, data_tab7$Int_Ano, 0)
    
    
    CompraPMesL <- BasePredicao$CompraPMesL * Int_Comp
    NParcelas <- BasePredicao$NParcela * Int_Nparc
    PrazoTotal <- BasePredicao$PrazoTotal * Int_PrazoT
    NParc_valor <- NParcelas * BasePredicao$ValordaNLOG
    ValordaNLOG <- BasePredicao$ValordaNLOG * (Int_Valor + Int_GC)
    NC <- BasePredicao$NC * Int_NC
    
    eta <- Int + CompraPMesL + NParcelas + PrazoTotal + NParc_valor + ValordaNLOG +
      Int_CNPJ + Int_UF + Int_Des + Int_Mes + Int_Ano
    
    probabilidade_inadimplencia_predita <- ((exp(eta)/(1+exp(eta)))*100) %>% round(2)
    
    ponto_corte <- Tabela[1,2]
    ponto_corte_log <- log(ponto_corte/(1 - ponto_corte))
    
    valor_max_model_aux <- ponto_corte_log - (Int + CompraPMesL + NParcelas + PrazoTotal + Int_CNPJ + Int_NC + Int_UF + Int_Des + Int_Mes + Int_Ano)
    valor_max_model_aux <- valor_max_model_aux / (Int_Valor + Int_GC + NParc_valor) + mean(log(dados_modelagem$ValordaNF))
    valor_max_model_aux <- exp(valor_max_model_aux)
    
    valor_nf_tot_uf <- dados_modelagem %>%
      filter(UF ==BasePredicao$UF) %>%
      pull(ValordaNF_TOT) %>%
      as.numeric()
    
    valor_min <- quantile(valor_nf_tot_uf, probs = .5)
    valor_max <- quantile(valor_nf_tot_uf, probs = .9)
    
    
    valor_max_model <- case_when(
      valor_max_model_aux < 0 ~ 0,
      valor_max_model_aux > valor_max * 1.2 ~ valor_max * 1.2,
      TRUE ~ valor_max_model_aux
    )
    
    eta_min <- Int + CompraPMesL + NParcelas + PrazoTotal + Int_Nparc_valor *
      BasePredicao$NParcela * (log(valor_min) - mean(log(dados_modelagem$ValordaNF))) +
      (Int_Valor + Int_GC) * (log(valor_min) - mean(log(dados_modelagem$ValordaNF))) +
      Int_CNPJ + NC + Int_UF + Int_Des + Int_Mes + Int_Ano
    
    eta_max <- Int + CompraPMesL + NParcelas + PrazoTotal + Int_Nparc_valor *
      BasePredicao$NParcela * (log(valor_max) - mean(log(dados_modelagem$ValordaNF))) +
      (Int_Valor + Int_GC) * (log(valor_max) - mean(log(dados_modelagem$ValordaNF))) +
      Int_CNPJ + NC + Int_UF + Int_Des + Int_Mes + Int_Ano
      
    eta_max_model <- Int + CompraPMesL + NParcelas + PrazoTotal + Int_Nparc_valor *
      BasePredicao$NParcela * (log(valor_max_model) - mean(log(dados_modelagem$ValordaNF))) +
      (Int_Valor + Int_GC) * (log(valor_max_model) - mean(log(dados_modelagem$ValordaNF))) +
      Int_CNPJ + NC + Int_UF + Int_Des + Int_Mes + Int_Ano
      
    probabilidade_inadimplencia_predita_min <- ((exp(eta_min)/(1+exp(eta_min)))*100)%>% as.numeric() %>% round(2)
    probabilidade_inadimplencia_predita_max <- ((exp(eta_max)/(1+exp(eta_max)))*100)%>% as.numeric() %>% round(2)
    probabilidade_inadimplencia_max_modelo <- ((exp(eta_max_model)/(1+exp(eta_max_model)))*100)%>% as.numeric() %>% round(2)
    
    
    dados_final <- 
      list(
        probabilidade_inadimplencia_predita = probabilidade_inadimplencia_predita,
        valor_max_model = valor_max_model,
        valor_min = valor_min,
        valor_max = valor_max,
        probabilidade_inadimplencia_predita_min = probabilidade_inadimplencia_predita_min,
        probabilidade_inadimplencia_predita_max = probabilidade_inadimplencia_predita_max,
        probabilidade_inadimplencia_max_modelo = probabilidade_inadimplencia_max_modelo,
        ponto_corte = ponto_corte
      )
    
    
  })
  
  
  base_indicador_selecionado <- reactive({
    
    input_indicador <- input$input_indicador
    
    if (input_indicador == "Inadimplencia"){
      dados_indicador <- readRDS("dados/analise_credito/df_clientes_inadimplentes.rds")
    } else if (input_indicador == "Inadimplencia Ativos"){
      dados_indicador <- readRDS("dados/analise_credito/df_clientes_inadimplentes_ativos.rds")
    } else {
      dados_indicador <- readRDS("dados/analise_credito/df_valor_inadimplencia.rds")
    }
    return(dados_indicador)
  })
  
  

  # Output's -------------------------------------
  output$plot_gauge <- renderHighchart({
    
    dados_simulacao <- dados_simulacao()
    probabilidade_inadimplencia_predita <- dados_simulacao[["probabilidade_inadimplencia_predita"]]
    ponto_corte <- dados_simulacao[["ponto_corte"]]
    
    
    highchart() %>%
      hc_add_series(
        data = probabilidade_inadimplencia_predita,
        name = "Prob. Inadimplência",
        type = "gauge"
      )%>%
      hc_pane(
        startAngle = -150,
        endAngle = 150
      )%>%
      hc_add_yAxis(
        min = 0,
        max = 100,
        title = list(text = "Probabilidade"),
        plotBands = list(
          list(from = 0, to = ponto_corte * 100, color = "#065a60"),
          list(from = ponto_corte * 100, to = ponto_corte * 1.4 * 100, color = "#f6bd60"),
          list(from = ponto_corte * 1.4 * 100, to = 100, color = "#e63946")
        )
      )%>%
      hc_tooltip(valueSuffix = "%")
    
    
    
    
  })
  
  
  output$info_credito <- renderInfoBox({

    dados_simulacao <- dados_simulacao()
    probabilidade_inadimplencia_predita <- dados_simulacao[["probabilidade_inadimplencia_predita"]]
    ponto_corte <- dados_simulacao[["ponto_corte"]]

    inadimplencia_predita <- (probabilidade_inadimplencia_predita/100) %>% round (2)
    msg <- ifelse( inadimplencia_predita > ponto_corte, "NÂO CEDER O CREDITO", "CEDER O CRÉDITO")
    cor <- ifelse( inadimplencia_predita > ponto_corte, "red", "green")

    valor_credito_aprovado <- sen_espCN %>%
      filter(ID == inadimplencia_predita)%>%
      pull(VPN)

    valor_credito_negado <- sen_espCN %>%
      filter(ID == inadimplencia_predita)%>%
      pull(VPN)

    valor_final <- ifelse( inadimplencia_predita > ponto_corte, valor_credito_negado, valor_credito_aprovado)
    valor_final <- (valor_final*100) %>% round(2)
    valor_final <- prettyNum(valor_final, big.mark = ".", decimal.mark = ",")

    shinydashboard::infoBox(
      title = msg,
      value = valor_final,
      color = cor,
      icon = icon("gavel")
    )
  })
  
  
   output$table_valor_simulador <- DT::renderDataTable({

     dados_simulacao <- dados_simulacao()

    valor_max_model <- dados_simulacao[["valor_max_model"]]
    valor_min <- dados_simulacao[["valor_min"]]
    valor_max <- dados_simulacao[["valor_max"]]

    probabilidade_inadimplencia_predita_min <- dados_simulacao[["probabilidade_inadimplencia_predita_min"]]
    probabilidade_inadimplencia_predita_max <- dados_simulacao[["probabilidade_inadimplencia_predita_max"]]
    probabilidade_inadimplencia_max_modelo <- dados_simulacao[["probabilidade_inadimplencia_max_modelo"]]

    tabela <- data.frame(
      descricao = c("Limite da Compra", "Valor do P90 comprado", "Valor Mediano comprado"),
      valor = c(valor_max_model, valor_max, valor_min),
      prob = c(probabilidade_inadimplencia_max_modelo, probabilidade_inadimplencia_predita_max,probabilidade_inadimplencia_predita_min)/100)

    colnames(tabela) <- c("Descrição", "Valor da Compra", "Probabilidade de Inadimplencia")
    
    background <- "value == 'Limite da Compra' ? '#e5cece': '#FFF'"
    class(background) <- "JS_EVAL"

    DT::datatable(
      data = tabela,
      rownames = FALSE,
      escape = FALSE,
      extensions = "Scroller",
      options = list(
        dom = "t",
        columnDefs = list(list(className = "dt-center", targets = "_all")),
        initComplete = JS(
          "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#600000', 'color': '#FFF'});",
            "}"
          )
        )
      )%>%
      formatStyle("Descrição", target = "row", backgroundColor = background, fontWeight = "bold")%>%
      formatCurrency(2, currency = "R$", digits = 0, mark = ".")%>%
      formatPercentage(3, digits = 2)
      
    
  })
   
   
   
   # Análise de Indicadores ------------------
   
    output$plot_valor_inadimplencia <- renderHighchart({
      
     lista_valores_menor_30 <- df_valor_inadimplencia[1,][-1]
     lista_valores_30_60 <- df_valor_inadimplencia [2,][-1]
     lista_valores_60_90 <- df_valor_inadimplencia [3,][-1]
     lista_valores_90_180 <- df_valor_inadimplencia [4,][-1]
     lista_valores_180_230 <- df_valor_inadimplencia [5,][-1]
     lista_valores_maior_230 <- df_valor_inadimplencia [6,][-1]
      
      lista_meses <- names(lista_valores_menor_30)
      
      criar_grafico_inadimplencia(
        lista_valores_menor_30,
        lista_valores_30_60 ,
        lista_valores_60_90 ,
        lista_valores_90_180 ,
        lista_valores_180_230 ,
        lista_valores_maior_230,
        lista_meses,
        tipo = "column",
        titulo = "Valor em Inadimplência",
        titulo_yAxis = "Valor em Inadimplência"
    )
      
  })
  
   output$plot_num_clientes_inadimplentes <- renderHighchart({
     
     # Remover a última coluna de texto antes de extrair os valores numéricos
     df_clientes_numerico <- df_clientes_inadimplentes[, -ncol(df_clientes_inadimplentes)]
     
     # Convertendo cada linha para vetor numérico corretamente
     lista_valores_menor_30 <- as.numeric(unlist(df_clientes_numerico[1, ]))
     lista_valores_30_60 <- as.numeric(unlist(df_clientes_numerico[2, ]))
     lista_valores_60_90 <- as.numeric(unlist(df_clientes_numerico[3, ]))
     lista_valores_90_180 <- as.numeric(unlist(df_clientes_numerico[4, ]))
     lista_valores_180_230 <- as.numeric(unlist(df_clientes_numerico[5, ]))
     lista_valores_maior_230 <- as.numeric(unlist(df_clientes_numerico[6, ]))
     
     # Pegando os nomes das colunas corretamente
     lista_meses <- colnames(df_clientes_numerico)
     
     criar_grafico_inadimplencia(
       lista_valores_menor_30,
       lista_valores_30_60,
       lista_valores_60_90,
       lista_valores_90_180,
       lista_valores_180_230,
       lista_valores_maior_230,
       lista_meses,
       tipo = "column",
       titulo = "Número de Clientes Inadimplentes",
       titulo_yAxis = "Grupo de Clientes Inadimplentes"
     )
   })
   
   output$plot_pct_clientes_ativos_inadimplentes <- renderHighchart({
     
     # Remover a última coluna de texto antes de extrair os valores numéricos
     df_pct_clientes_numerico <- df_pct_clientes_ativos_inadimplentes[, -ncol(df_pct_clientes_ativos_inadimplentes)]
     
     # Convertendo cada linha para vetor numérico corretamente
     lista_valores_menor_30 <- as.numeric(unlist(df_pct_clientes_numerico[1, ]))
     lista_valores_30_60 <- as.numeric(unlist(df_pct_clientes_numerico[2, ]))
     lista_valores_60_90 <- as.numeric(unlist(df_pct_clientes_numerico[3, ]))
     lista_valores_90_180 <- as.numeric(unlist(df_pct_clientes_numerico[4, ]))
     lista_valores_180_230 <- as.numeric(unlist(df_pct_clientes_numerico[5, ]))
     lista_valores_maior_230 <- as.numeric(unlist(df_pct_clientes_numerico[6, ]))
     
     # Pegando os nomes das colunas corretamente
     lista_meses <- colnames(df_pct_clientes_numerico)
     
     criar_grafico_inadimplencia(
       lista_valores_menor_30,
       lista_valores_30_60,
       lista_valores_60_90,
       lista_valores_90_180,
       lista_valores_180_230,
       lista_valores_maior_230,
       lista_meses,
       tipo = "line",
       titulo = "Percentual de Clientes Ativos Inadimplentes",
       titulo_yAxis = "Percentual de Clientes Ativos Inadimplentes"
     )
   })
   
   
   output$box_valor_60_dias <- renderValueBox({
     
     input_indicador <- input$input_indicador
     
     base_indicador_selecionado <- base_indicador_selecionado()
     
     total_60dias <- base_indicador_selecionado%>%
       pivot_longer(
         cols = -tempo_atraso,
         names_to = "meses",
         values_to = "valores"
       )%>%
       filter(tempo_atraso %in% c("Menor que 30", "30 - 60"))%>%
       pull(valores)%>%
       sum(., na.rm = TRUE)%>%
       round(2)
     
     total_60dias <- prettyNum(total_60dias, big.mark = ".", decimal.mark = ",")
     texto_box <- "INADIMPLENTES 60 DIAS"
     icon_box <- "user"
     
     if (input_indicador == "Valor") {
       total_60dias <- paste("R$", total_60dias)
       texto_box <- "INADIMPLÊNCIA ATÉ 60 DIAS"
       icon_box <- "money"
     }

     shinydashboard::valueBox(
       value = total_60dias,
       subtitle = texto_box,
       icon = icon(icon_box),
       color = "green",
       width = NULL
     )
     
   })
   
   output$box_valor_total <- renderValueBox({
     
     input_indicador <- input$input_indicador
     
     base_indicador_selecionado <- base_indicador_selecionado()
     
     total <- base_indicador_selecionado%>%
       pivot_longer(
         cols = -tempo_atraso,
         names_to = "meses",
         values_to = "valores"
       )%>%
       pull(valores)%>%
       sum(., na.rm = TRUE)%>%
       round(2)
     
     total <- prettyNum(total, big.mark = ".", decimal.mark = ",")
     texto_box <- "TOTAL DE INADIMPLENTES"
     icon_box <- "user"
     
     if (input_indicador == "Valor") {
       total_60dias <- paste("R$", total)
       texto_box <- "VALOR TOTAL DE INADIMPLENTES"
       icon_box <- "money"
     }
     
     shinydashboard::valueBox(
       value = total,
       subtitle = texto_box,
       icon = icon(icon_box),
       color = "red",
       width = NULL
     )
     
   })
   
   
    output$tabela_indicador_selecionado <- renderUI({
      
      base_indicador_selecionado <- base_indicador_selecionado()
      
      tempo_atraso <- base_indicador_selecionado %>% pull(tempo_atraso)
      
      base_indicador_selecionado <- base_indicador_selecionado %>%
        dplyr::select(-tempo_atraso)
        
      base_indicador_selecionado <- 
        apply(base_indicador_selecionado, 2, FUN = function(x) {prettyNum(round(x, 2), big.mark = ".", decimal.mark = ",")})
      
      rownames(base_indicador_selecionado) <- tempo_atraso
      
      tabela_html <-
        kable(base_indicador_selecionado, align = "c")%>%
        kable_styling(font_size = 11.5)%>%
        scroll_box(box_css = "border: none; padding: 15px; width: 100%; overflow-x: scroll;")
      
      HTML(tabela_html)
     
     
     
     
     
    })
   
   
   
   
   
   
}

