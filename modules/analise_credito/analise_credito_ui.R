
analise_credito_ui <- function(id, lista_uf){
  
  ns <- NS(id)
  
  tabItem(
    tabName = "tab_analise_credito",
    tags$div(
      style="background-color: #ffffff; padding: 10px; border-radius: 5px;",
      fluidRow(
        column(
          width = 7,
          h3(
            class = "titulo_simulador", "Simulação de compra para cliente novo")
        )
      ),
      fluidRow(
        column(
          width = 3,
          selectInput(
            inputId = ns("input_uf"),
            label = "SELECIONE UF:",
            choices = lista_uf,
            width = "100%"
          ),
          numericInput(
            inputId = ns("input_num_parcelas"),
            label = "SELECIONE O N° DE PARCELAS",
            value = 3,
            min = 0,
            max = 60,
            step = 1,
            width = "100%"
          ),
          numericInput(
            inputId = ns("input_prazo_total"),
            label = "SELECIONE O PRAZO TOTAL:",
            value = 30,
            min = 0,
            max = 120,
            step = 30,
            width = "100%"
          ),
          numericInput(
            inputId = ns("input_valor_compra"),
            label = "SELECIONE O VALOR DA COMPRA:",
            value = 2500,
            min = 0,
            max = 50000,
            step = 500,
            width = "100%"
        )
      ),
      column(
        width = 4,
        loader(highchartOutput(ns("plot_gauge")))
        ),
      column(
          width = 5,
          tags$div(
            style="float: right; width = 70;",
            infoBoxOutput(ns("info_credito")),
          ),
          fluidRow(
            DT::dataTableOutput(ns("table_valor_simulador"))
          )
        )
      )
    ),
    
    # Análise de Indicadores ------------------
    
    fluidRow(
      box(
        width = 4,
        loader(highchartOutput(ns("plot_valor_inadimplencia")))
      ),
      box(
        width = 4,
        loader(highchartOutput(ns("plot_num_clientes_inadimplentes")))
      ),
      box(
        width = 4,
        loader(highchartOutput(ns("plot_pct_clientes_ativos_inadimplentes")))
      )
    ),
    fluidRow(
      column(
        width = 3,
        selectInput(
          inputId = ns("input_indicador"),
          label = "Selecione o indicador:",
          choices= c(
            "Inadimplencia",
            "Inadimplentes Ativos",
            "Valor"
          ),
          selected = "Valor"
        )
      ),
      fluidRow(
        column(
          width = 3, 
          style = "margin-bottom: 0px;",
          valueBoxOutput(ns("box_valor_60_dias"), width = 12)  # Correção aqui
        ),
        column(
          width = 3, 
          style = "margin-bottom: 0px;",
          valueBoxOutput(ns("box_valor_total"), width = 12)  # Correção aqui
        )
      ),
      fluidRow(
        column(
          width = 12,
          uiOutput(ns("tabela_indicador_selecionado"))
        )
      )
    )
  )
}
