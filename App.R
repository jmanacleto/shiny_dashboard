############################
# shinydev@operdata.com.br #
# agoravai3372             #
############################

# Sys.setlocale("LC_ALL", "English")

# Leitura dos Pacotes ####
rm(list=ls())
#options(OutDec = ",")

library(tidyverse)
library(openxlsx)
library(readxl)
library(zoo)
library(shinydashboardPlus)
library(dashboardthemes)
library(fireData)
library(highcharter)
library(stringr)
library(DT)
library(shinydashboard)
library(shiny)
library(shinyjs)
library(lubridate)
library(htmlwidgets)
library(shinycssloaders)
library(shinyWidgets)
library(shinyalert)
library(shinyBS)
library(kableExtra)


#### Carregando Funções --------------------------------------------------------
source("funcoes/funcoes_dash.R", encoding = "UTF-8")
source("funcoes/funcoes_treinamento.R", encoding = "UTF-8")

#### Carregando Modulos --------------------------------------------------------
source("modules/inicio/inicio_ui.R", encoding = "UTF-8")
source("modules/inicio/inicio_server.R", encoding = "UTF-8")
#--------------
source("modules/analise_credito/analise_credito_ui.R", encoding = "UTF-8")
source("modules/analise_credito/analise_credito_server.R", encoding = "UTF-8")

#### Carregando Dados ----------------------------------------------------------
dados_inicio <- readRDS ("C:/Users/jmana/OneDrive/Documentos/Oper/Shiny/start_shiny_project/start_shiny_project/dados/analise_produtos/dados_produtos.rds")
dados_modelagem <- readRDS("C:/Users/jmana/OneDrive/Documentos/Oper/Shiny/start_shiny_project/start_shiny_project/dados/analise_credito/dados_modelagem.rds")
dados_modelagem_analise_credito <- readRDS("C:/Users/jmana/OneDrive/Documentos/Oper/Shiny/start_shiny_project/start_shiny_project/dados/analise_credito/dados_modelagem.rds")

lista_uf_analise_credito <-
  dados_modelagem_analise_credito %>%
  pull(UF)%>%
  unique()%>%
  sort()






# Design ######################################################################

brbg <- hsv(0.5, .35, seq(.25, .95, length.out = 12))

logo_blue <- shinyDashboardLogoDIY(
  boldText = ""
  , mainText = ""
  , textSize = 16
  , badgeText = ""
  , badgeTextColor = "white"
  , badgeTextSize = 0
  , badgeBackColor = "#"
  , badgeBorderRadius = 3
)


header <- uiOutput("ui_menu_top")

sidebar <- uiOutput("mainsidebar")

body <- dashboardBody(
  tags$head(
    # Atualizar - Favicon (imagem miniatura) do cliente
    tags$link(rel = "shortcut icon", href = "img/favicon_cliente.png")
  ),
  tags$link(rel = "stylesheet", type = "text/css", href = "css/bootstrap.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "css/style_dash.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "css/style_button.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "css/style_treinamento.css"),
  useShinyjs(),
  uiOutput("mainbody")
)


# Atualizar - A função 'dashboardPagePlus':
# Caso queira que o menu lateral apareça ao clicar no menu sanduíche

# Atualizar - A função 'dashboardPage':
# Caso queira que o menu lateral 'NÃO' apareça ao clicar no menu sanduíche


ui <- dashboardPage(
  title = "Dashboard Analytics",
  header,
  sidebar,
  body
)

# ui <- fluidPage(
#   analise_credito_ui("tab_analise_credito", lista_uf)
# )


# Servidor ####################################################################

server <- function(input, output, session) {
  

  
  addClass(selector = ".sidebar-toggle", class = "sidebar-toggle-none")
  addClass(selector = ".wrapper", class = "fundo-login")
  addClass(selector = ".content-wrapper", class = "background-none")
  
  
  Logged <- TRUE
  USER <- reactiveValues(Logged = Logged)
  
  # Atualizar - Token de acesso - Firebase
  tokenLogin <- reactive({
    if (input$userName == "shinydev@operdata.com.br") {
      tokenLogin <- "AIzaSyCoozpdatfa_ZD4JZEBdcEYt1f6c6kB5rE"
    } else {
      tokenLogin <- "AIzaSyCoozpdatfa_ZD4JZEBdcEYt1f6c6kB5rE"
    }
    
    tokenLogin
  })
  
  # login
  observeEvent(input$Login, {
    token <- auth(tokenLogin(),
                  email = input$userName,
                  password = input$passwd
    )
    
    
    if (length(token$registered) > 0) {
      USER$Logged <- TRUE
    } else {
      output$login_fail <- renderText("Email ou senha errado, tente novamente")
    }
  })
  
  # logout
  observeEvent(input$Logout, {
    USER$Logged <- FALSE
  })
  
  ### Interface #################################################################
  
  output$mainsidebar <- renderUI({
    if (USER$Logged == TRUE) {
      uiOutput("sidebarpanel")
    } else if (USER$Logged == FALSE) {
      NULL
    }
  })
  
  # sidebar login
  output$sidebarpanel <- renderUI({
    
    removeClass(selector = ".sidebar-toggle", class = "sidebar-toggle-none")
    removeClass(selector = ".wrapper", class = "fundo-login")
    
    dashboardSidebar(
      sidebarUserPanel("User Logged",
                       # Atualizar - Endereço do projeto no servidor
                       subtitle = a(href = "https://www.grupoabg.com.br/shiny/start_shiny_project/", 
                                    icon("circle", class = "text-success"), "Logout"),
                       # Logo da Oper
                       image = "img/LogoOperD.png"
      ),
      
      sidebarMenu(
        id = "tabs",
        # Menu Lateral ------------
        menuItem(
          "Início", 
          tabName = "tab_inicio", 
          icon = icon("home")
        ),
        menuItem(
          "Análise de crédito", 
          tabName = "tab_analise_credito", 
          icon = icon("money-check-alt")
        )
      )
    )
  })
  
  # sidebar_logout
  output$sidebarpanel_logout <- renderUI({
    NULL
  })
  
  
  # Header UI
  output$ui_menu_top <- renderUI({
    if (USER$Logged == TRUE) {
      
      # Opção de header - Ajustar ao cliente - adicionando a logo
      dashboardHeader(
        title = tagList(
          # Atualizar -  Logo cliente
          img(
            class= "logo-lg", 
            src="img/logo_cliente.png", 
            width = "140px", 
            style="padding: 10px; margin: 0px auto;"
          ),
          # Atualizar - Logo miniatura do cliente
          img(
            class="logo-mini", 
            style="width: 42px; margin-left: -10px; margin-top: 5px;", 
            src = "img/favicon_cliente.png"
          )
        ),
        # Atualizar - Texto ou imagem que ficarão a direita - Caso seja necessário
        tags$li(
          class = "dropdown",
          h1(class="title-header", "Dashboard interativo")
        )
      )
      
      # Caso queira que não apareça nada - Adicionar função vazia
      # dashboardHeader()
      
    } else if (USER$Logged == FALSE) {
      NULL
    }
    
  })
    
  
  
  
  ### Body ######################################################################
  
  output$mainbody <- renderUI({
    if (USER$Logged == TRUE) {
      uiOutput("body")
    } else {
      if (USER$Logged == FALSE) {
        uiOutput("body_logout")
      }
    }
  })
  
  output$body <- renderUI({
    tabItems(
      
      # Tab - inicio --------
      
      inicio_ui(
        id = "tab_inicio"
      ),
      
      # Tab - Análise de crédito ----
      analise_credito_ui(
        id = "tab_analise_credito",
        lista_uf = lista_uf_analise_credito
      )
      
    )
  })
  
  
  
  # LOGIN - Página ----
  output$body_logout <- renderUI({
    
    if (USER$Logged == FALSE && is.null(input$Login) || input$Login == 0) {
      
      
      fluidPage(
        tags$div(
          class = "loginColumns animated fadeInDown",
          style = "padding: 120px 20px 20px 20px; max-width: 1000px;"
        ),
        fluidRow(
          column(
            width = 5,
            class = "text-center",
            tags$img(
              src = "img/LogoOperL.png",
              width="170px",
              style="margin:20px; margin-top: 40px;"
            ),
            h1(
              style="color: #fff;",
              "Acelerando a evolução"
            ),
            br(),
            h1(
              style="color: #fff; margin-top: -10px;",
              "através dos dados."
            )
          ),
          column(
            width = 7,
            class = "box-login",
            box(
              width = 11,
              title = "Entrar",
              style="padding: 20px 20px;",
              textInput(
                inputId = "userName",
                label = "Email:"
              ),
              passwordInput(
                inputId = "passwd",
                label = "Senha:"
              ),
              br(),
              textOutput("login_fail"),
              actionButton("Login", "Log in")
            )
          )
        )
      )
      
    } else if (USER$Logged == FALSE && input$Login > 0) {
      
      fluidPage(
        tags$div(
          class = "loginColumns animated fadeInDown",
          style = "padding: 100px 20px 20px 20px; max-width: 1000px;"
        ),
        fluidRow(
          column(
            width = 7,
            class="box-login",
            box(
              width = 11,
              title = "Entrar", textInput("userName", "Username"),
              passwordInput("passwd", "Password"),
              br(),
              textOutput("login_fail"),
              tags$head(tags$style("#login_fail{color: red;}")),
              actionButton("Login", "Log in")
            )
            
          ),
          column(
            width = 5,
            class = "text-center",
            tags$img(
              src = "./img/login.png",
              width="90%",
              style="margin:20px; margin-top: 40px;"
            )
          )
        )
      )
    }
  })
  
  
  
  
  ## Observe's -----------------------------------------------------------------
  
  
  
  
  
  
  
  ## Reactive's ----------------------------------------------------------------
  
  
  
  
  
  
  
  ## Output's  -----------------------------------------------------------------
  
  callModule(
    module = inicio_server,
    id = "tab_inicio",
    dados = dados_inicio
  )
  
  callModule(
    module = analise_credito_server,
    id = "tab_analise_credito",
    dados_modelagem = dados_modelagem_analise_credito
  )
  
  
  
}


shinyApp(ui = ui, server = server)
