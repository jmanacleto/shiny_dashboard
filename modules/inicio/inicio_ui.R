inicio_ui <- function(id){
  
  ns <- NS(id)
  
  # Atualizar - Páginas adicionadas no menu lateral
  tabItem(
    tabName = "tab_inicio",   
    class="active",
    fluidRow(
      column(
        width = 5,
        loader(highchartOutput(ns("plot_mapa"), height = "600px"))
        ),
        column(
          width = 7,
          fluidRow(
            column(
              width = 6,
              valueBoxOutput(ns("box_valor_grupo1"))
            ),
            column(
              width = 6,
              valueBoxOutput(ns("box_valor_grupo2"))
            )
          ),
          fluidRow(
            loader(highchartOutput(ns("plot_acumulado")))
          )
        )
      )
    )
}
