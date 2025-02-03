

criar_grafico_inadimplencia <- function(
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
) {
  plot <-
  highchart()%>%
    hc_chart(type = tipo)%>%
    hc_title(text = titulo)%>%
    hc_yAxis(title = list(text = titulo_yAxis))%>%
    hc_xAxis(categories = lista_meses, labels = list(rotation = 30, align = "left"))%>%
    hc_plotOptions(
      column = list(
        dataLabels = list(enabled = FALSE),
        stacking = "normal"
      )
    )%>%
    hc_legend(
      title = list(text = "Tempo de Atraso"),
      enabled = TRUE,
      verticalAlign = "bottom",
      layout = "horizontal"
    )%>%
    hc_series(
      list(name = "Menor que 30", color = "#264653", data = as.numeric(lista_valores_menor_30)),
      list(name = "30-60", color = "#2a9d8f", data = as.numeric(lista_valores_30_60)),
      list(name = "60-90", color = "#e9c46a", data = as.numeric(lista_valores_60_90)),
      list(name = "90-180", color = "#f4a261", data = as.numeric(lista_valores_90_180)),
      list(name = "180-230", color = "#e76f51", data = as.numeric(lista_valores_180_230)),
      list(name = "Maior que 230", color = "#9d0208", data = as.numeric(lista_valores_maior_230))
    )
  
  if (titulo_yAxis == "Percentual de Clientes Ativos Inadimplentes"){
    plot <- plot %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, valueSuffix = "%")
  }
  plot
}
