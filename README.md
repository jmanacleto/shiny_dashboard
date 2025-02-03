Aqui está uma descrição para o seu projeto no GitHub:

---

# 📊 Dashboard de Análise de Crédito

Este repositório contém um **Dashboard Interativo** para análise de crédito, inadimplência e simulação de compras para novos clientes. O projeto foi desenvolvido utilizando **R** e **Shiny**, permitindo a visualização e exploração de dados financeiros de forma intuitiva e dinâmica.

## 🔥 Principais Funcionalidades

- **Simulação de Compra para Novos Clientes**: Permite selecionar UF, número de parcelas, prazo total e valor da compra para simular diferentes cenários.
- **Análise de Indicadores Financeiros**:
  - Valor total da inadimplência;
  - Número de clientes inadimplentes;
  - Percentual de clientes ativos inadimplentes;
- **Visualização Gráfica Interativa**:
  - Mapas interativos para análise geográfica;
  - Gráficos dinâmicos de inadimplência por tempo de atraso;
  - Indicadores e dashboards personalizáveis.
- **Ferramentas de Dashboard**:
  - Elementos interativos, como tooltips informativos;
  - Estrutura modular para fácil expansão.

## 🛠 Tecnologias Utilizadas

- **R** e **Shiny** para desenvolvimento da interface;
- **Highcharter** para visualização interativa de dados;
- **DT** para tabelas dinâmicas;
- **ShinyWidgets** para maior customização da interface.

## 📂 Estrutura do Repositório

```
📂 Projeto
 ├── 📄 funcoes_dash.R            # Funções para construção do dashboard
 ├── 📄 funcoes_treinamento.R      # Funções para análise de inadimplência
 ├── 📄 analise_credito_ui.R       # Interface para análise de crédito
 ├── 📄 inicio_ui.R                # Interface inicial do dashboard
```

## 🚀 Como Executar

1. Instale os pacotes necessários:
   ```r
   install.packages(c("shiny", "highcharter", "DT", "shinyWidgets"))
   ```
2. Execute a aplicação:
   ```r
   shiny::runApp("caminho_para_o_projeto")
   ```

---

Caso queira alguma modificação, me avise! 🚀
