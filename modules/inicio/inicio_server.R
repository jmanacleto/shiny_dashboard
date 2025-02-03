
inicio_server <- function(input, output, session, dados) {
  
  
  output$plot_mapa <- renderHighchart({
    
    total_uf <- dados %>%
      group_by(uf) %>%
      summarise(total = sum(total, na.rm = TRUE))
    
    hcmap(
      map = "countries/br/br-all",
      data = total_uf,
      value = "total",
      name = "Total de vendas",
      dataLabels = list(enabled = TRUE, format = '{point.value:.,0f}'),
      tooltip = list(valueDecimals = 2, valuePrefix = "R$")
    ) %>% hc_title(text = "Total de vendas por estado")%>%
      hc_credits(enable = FALSE)%>%
      hc_colorAxis(minColor = "#FFFFFF", maxColor = "#0F2B43")
    
  })
  
  
  output$box_valor_grupo1 <- renderValueBox({
    
    total_grupo1 <- dados %>%
      filter(grupos == "Grupo 1") %>%
      group_by(grupos)%>%
      summarise(total = sum(total, na.rm =  TRUE))%>%
      pull(total)
    
    total_grupo1 <- prettyNum(total_grupo1, big.mark = ".", decimal.mark = ",")
    total_grupo1 <- paste("R$", total_grupo1)
    
    shinydashboard::valueBox(
      value = total_grupo1,
      subtitle = "Grupo 1",
      color = "purple",
      width = 12,
      icon = icon("dollar-sign")
    )
  })
  
  output$box_valor_grupo2 <- renderValueBox({
    
    total_grupo2 <- dados %>%
      filter(grupos == "Grupo 2") %>%
      group_by(grupos)%>%
      summarise(total = sum(total, na.rm =  TRUE))%>%
      pull(total)
    
    total_grupo2 <- prettyNum(total_grupo2, big.mark = ".", decimal.mark = ",")
    total_grupo2 <- paste("R$", total_grupo2)
    
    shinydashboard::valueBox(
      value = total_grupo2,
      subtitle = "Grupo 2",
      color = "purple",
      width = 12,
      icon = icon("dollar-sign")
    )
  })
  
  output$plot_acumulado <- renderHighchart({
    
    total_regiao <- dados %>%
      group_by(regional)%>%
      summarise(total = sum(total, na.rm = TRUE))%>%
      mutate(
        cumulativo = cumsum(total),
        low = 0 + cumulativo,
        high = low,
        y = total,
        isIntermediateSum = FALSE,
        color = "#11695f"
      )
    total_brasil <- data.frame(
      regional = "Brasil",
      total = sum(total_regiao$total),
      cumulativo = 0,
      low = 0,
      high = 0,
      y = 0,
      isIntermediateSum = TRUE,
      color = "#283b4f"
    )
    
    dados_plot <- total_regiao %>%
      bind_rows(total_brasil)
    
    
    highchart()%>%
      hc_chart(type = "waterfall")%>%
      hc_xAxis(
        categories = dados_plot$regional,
        labels = list(rotation = 30, align = "left")
      )%>%
      hc_yAxis(
        title = list(text = "Total de vendas")
      )%>%
      hc_add_series(
        dados_plot,
        showInLegend = FALSE,
        dataLabels = list(
          enabled = TRUE,
          verticalAlign = "Top",
          format = '{point.total:.,2f}',
          style = list(color = "#000")
        ),
        tooltip = list(
          pointFormat = "Total vendido: <b>R$ {point.total:.,2f}<b>"
        )
      )%>% 
      hc_title(
        text = "Vendas acumuladas por região"
      )
    
  })

    
}

