
# Importação dos pacotes --------------------------------------------------

library(shiny) # o principal
library(bs4Dash) # criacao do layout
library(tidyverse)
library(corrplot)
library(arrow)
library(leaflet)
library(sf)
library(geobr)
library(plotly)


# Leitura da base ---------------------------------------------------------


link <- "https://www.dropbox.com/scl/fi/1o4qce3vfc5k2glujpjp6/df_enem_2023_filtrado.parquet?rlkey=uszz2lnjeo6hfrf1u7u6bsb7d&st=0ueauk05&dl=1"
# Ler o arquivo CSV
df <- read_parquet(link)



# Tratativas na base ----------------------------------------------------


# Alterando o tipo de algumas colunas para construção de gráficos
df_tratado <- df %>% 
  mutate(cor_raca = factor(TP_COR_RACA,
                           levels = c(0, 1, 2, 3, 4, 5, 6),
                           labels = c("Não declarado", "Branca", "Preta", "Parda", 
                                      "Amarela", "Indígena", "Não dispõe da informação")),
         escola = factor(TP_ESCOLA,
                         levels = c(1,2,3),
                         labels = c("Não respondeu", "Pública", "Privada")),
         localizacao_escola  = factor(TP_LOCALIZACAO_ESC,
                                      levels = c(1,2),
                                      labels = c("Urbana", "Rural"))
  )

# Criando faixas de acordo com o salario minimo de 2023 (1320 reais) as categorias
# podem ser acessadas no dicionário dos dados
salario_minimo <- 1320

df_tratado <- df_tratado %>%
  mutate(renda_salarios_min = case_when(
    Q006 %in% c("A") ~ "0 salários mínimos",
    Q006 %in% c("B") ~ "Até 1 SM",
    Q006 %in% c("C", "D") ~ "1 a 2 SM",
    Q006 %in% c("E", "F") ~ "2 a 3 SM",
    Q006 %in% c("G", "H") ~ "3 a 5 SM",
    Q006 %in% c("I", "J") ~ "5 a 7 SM",
    Q006 %in% c("K", "L") ~ "7 a 10 SM",
    Q006 %in% c("M", "N") ~ "10 a 15 SM",
    Q006 %in% c("O") ~ "15 a 20 SM",
    Q006 %in% c("P", "Q") ~ "Mais de 20 SM"
    ),
    faixa_etaria_reduzida = case_when(
      TP_FAIXA_ETARIA %in% c(1, 2) ~ "Menor de 18 anos",  # 1 = Menor de 17 anos, 2 = 17 anos
      TP_FAIXA_ETARIA %in% c(3, 4, 5) ~ "18 a 20 anos",   # 3 = 18 anos, 4 = 19 anos, 5 = 20 anos
      TP_FAIXA_ETARIA %in% c(6, 7, 8) ~ "21 a 23 anos",   # 6 = 21 anos, 7 = 22 anos, 8 = 23 anos
      TP_FAIXA_ETARIA %in% c(9, 10) ~ "24 a 25 anos",     # 9 = 24 anos, 10 = 25 anos
      TP_FAIXA_ETARIA == 11 ~ "26 a 30 anos",             # 11 = Entre 26 e 30 anos
      TP_FAIXA_ETARIA == 12 ~ "31 a 35 anos",             # 12 = Entre 31 e 35 anos
      TP_FAIXA_ETARIA == 13 ~ "36 a 40 anos",             # 13 = Entre 36 e 40 anos
      TP_FAIXA_ETARIA %in% c(14, 15) ~ "41 a 50 anos",    # 14 = Entre 41 e 45 anos, 15 = Entre 46 e 50 anos
      TP_FAIXA_ETARIA %in% c(16, 17) ~ "51 a 60 anos",    # 16 = Entre 51 e 55 anos, 17 = Entre 56 e 60 anos
      TP_FAIXA_ETARIA %in% c(18, 19, 20) ~ "Mais de 60 anos",  # 18 = Entre 61 e 65 anos, 19 = Entre 66 e 70 anos, 20 = Maior de 70 anos
      TRUE ~ as.character(TP_FAIXA_ETARIA)
      )
    )

df_tratado <- df_tratado %>%
  mutate(renda_salarios_min = factor(renda_salarios_min,
                                     levels = c("0 salários mínimos", "Até 1 SM", "1 a 2 SM",
                                                "2 a 3 SM", "3 a 5 SM", "5 a 7 SM",
                                                "7 a 10 SM", "10 a 15 SM", "15 a 20 SM", "Mais de 20 SM")),
         faixa_etaria_reduzida = factor(faixa_etaria_reduzida,
                                        levels = c("Menor de 18 anos","18 a 20 anos",
                                                   "21 a 23 anos","24 a 25 anos","26 a 30 anos",
                                                   "31 a 35 anos","36 a 40 anos","41 a 50 anos",
                                                   "51 a 60 anos","Mais de 60 anos"
                                                   )
                                        )
         )



df_notas <- df_tratado %>% 
  select(NU_NOTA_MT, NU_NOTA_CN, NU_NOTA_LC, NU_NOTA_CH, NU_NOTA_REDACAO,
         cor_raca, escola, localizacao_escola, TP_SEXO, SG_UF_PROVA, renda_salarios_min, faixa_etaria_reduzida)
df_notas$faixa_etaria_reduzida %>% table()

# Lendo informaçoes geoespaciais sobre os estados brasileiros
# Serão utilizados na construção do mapa interativo

estados_br <- geobr::read_state()

# Calcular a média das notas para cada estado
media_por_estado <- df_tratado %>%
  group_by(SG_UF_PROVA) %>%
  summarise(
    media_MT = mean(NU_NOTA_MT, na.rm = TRUE),
    media_LC = mean(NU_NOTA_LC, na.rm = TRUE),
    media_CH = mean(NU_NOTA_CH, na.rm = TRUE),
    media_CN = mean(NU_NOTA_CN, na.rm = TRUE),
    media_REDACAO = mean(NU_NOTA_REDACAO, na.rm = TRUE)
  )
# Mapeando o nome das provas para inserir no título dos gráficos
nome_prova <- c(
  "NU_NOTA_MT" = "Matemática",
  "NU_NOTA_LC" = "Linguagens",
  "NU_NOTA_CH" = "Ciências Humanas",
  "NU_NOTA_CN" = "Ciências da Natureza",
  "NU_NOTA_REDACAO" = "Redação"
)
# Mapeando o nome dos grupos
nome_segmento <- c(
  "cor_raca" = "Raça",
  "TP_SEXO" = "Sexo",
  "escola" = "Tipo da Escola",
  "localizacao_escola" = "Localização da Escola"
)

# Junta as médias calculadas com os dados acerca dos estados
estados_br <- estados_br %>%
  left_join(media_por_estado, by = c("abbrev_state" = "SG_UF_PROVA"))  


# Interface do usuário --------


ui <- dashboardPage(
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Dashboard Enem",
      color = "gray-dark",
      href = "https://adminlte.io/themes/v3",
      image = "https://www.ifpb.edu.br/noticias/2018/04/comecam-as-inscricoes-para-o-enem-2018/enem-logo.png"
    )
  ),
  sidebar = dashboardSidebar(
    sidebarUserPanel(
      image = "https://upload.wikimedia.org/wikipedia/commons/f/f9/Logomarca_UFSCAR.png",
      name = "Bem vindo ao dashboard!"
    ),
    
    sidebarMenu(
      id = "sidebarmenu",
      sidebarHeader("Menu de navegação"),
      menuItem(
        "Introdução",
        tabName = "intro",
        icon = icon("bars")
      ),
      menuItem(
        "Distribuição de Notas",
        tabName = "distribuicao",
        icon = icon("chart-bar")
      ),
      menuItem(
        "Distribuição por faixas",
        tabName = "faixas",
        icon = icon("user")
      ),
      menuItem(
        "Comparação Regional",
        tabName = "comparacao_regional",
        icon = icon("map-marker-alt")
      ),
      menuItem(
        "Correlação de Notas",
        tabName = "correlacao",
        icon = icon("chart-line")
      ),
      menuItem(
        "Sobre",
        tabName = "sobre",
        icon = icon("info")
      )
    )
  ),
  
  ## Conteúdo das abas -------
  body = dashboardBody(
    tabItems(
      ## Introducao ----
      tabItem(
        tabName = "intro",
        h2("Bem-vindo ao Dashboard do Enem 2023"),
        tabBox(
          id = "intro_tabs",
          height = "400px",
          width = 12,
          tabPanel(
            title = "Metodologia",
            h4("Metodologia da Análise"),
            p("Os dados foram obtidos do Enem 2023, disponíveis publicamente. Para acessar o dicionário com a descrição das 76 variáveis, clique", 
              a("aqui", href = "https://github.com/riquelmepereira/projeto-perspectivas-enem/blob/main/Dicion%C3%A1rio_Microdados_Enem_2023.xlsx", target = "_blank"), ".",
              style = "text-align: justify;"),
            p("As análises incluem a distribuição das notas, comparações regionais e correlações entre as áreas do conhecimento. Diferentes visualizações foram criadas para podermos responder as perguntas principais da pesquisa, disponíveis na seção de perguntas. Assim, o usuário do dashboard pode seguir tais perguntas para se orientar nas visualizações de acordo com o seu interesse.",
              style = "text-align: justify;"),
            p(
              "Utilizamos o software R, juntamente com os pacotes ", 
              em("shiny"), " e ", em("bs4Dash"), " para criar o layout do aplicativo. Além desses, outros pacotes foram fundamentais durante a análise, como o ", 
              em("tidyverse"), ", que reúne várias ferramentas que facilitam o tratamento de dados, visualizações e muitas outras tarefas. Para a criação do mapa interativo, utilizamos os pacotes ", 
              em("leaflet"), " e ", em("geobr"), ". A visualização da matriz de correlações lineares foi gerada com o pacote ", 
              em("corrplot"), ". Também utilizamos o ", 
              em("plotly"), " para produzir gráficos interativos e o ", 
              em("arrow"), " para agilizar a leitura da base de dados, que foi transformada em um arquivo no formato .parquet.",
              style = "text-align: justify;")
          ),
          tabPanel(
            title = "Banco de dados",
            h4("Sobre os dados: "),
            div(style = "text-align: justify;",
                p("Os microdados do ENEM são um conjunto detalhado de informações coletadas durante a aplicação do Exame Nacional do Ensino Médio (ENEM). Esses dados incluem uma ampla gama de variáveis, como notas dos candidatos em diferentes áreas do conhecimento, informações demográficas, socioeconômicas e escolares. Os microdados são disponibilizados pelo Instituto Nacional de Estudos e Pesquisas Educacionais Anísio Teixeira (INEP) e são utilizados para análises estatísticas e pesquisas acadêmicas, permitindo uma compreensão aprofundada do desempenho dos estudantes e das desigualdades educacionais no Brasil. Eles são essenciais para a formulação de políticas públicas e para estudos que visam melhorar a qualidade da educação no país.")
            )
          ),
          tabPanel(
            title = "Perguntas",
            h4("Perguntas a serem respondidas"),
            p("1. Como as notas se comportam entre os candidatos das escolas públicas x privadas?"),
            p("2. Existem diferenças nas notas entre os gêneros (masculino ou feminino)?"),
            p("3. Participantes de escolas de áreas rurais tendem a ter notas menores que alunos de áreas urbanas?"),
            p("4. Vemos diferenças entre as notas dos grupos raciais?"),
            p("5. Alunos com rendas familiares mais modestas têm desempenho menor que o de alunos com renda familiares altas?"),
            p("6. Há diferença entre as notas de acordo com a faixa etária do participante?"),
            p("7. Como as notas se comportam nos diferentes estados do país?"),
            p("8. Quais são as correlações entre as notas das diferentes áreas do conhecimento?")
          )
        )
      ),
      
      ## Boxplot segmentos ---- 
      tabItem(
        tabName = "distribuicao",
        h2("Distribuição de Notas por Segmentação"),
        div(style = "text-align: justify;",
            p("A análise mostra, por meio de boxplots, como as notas se distribuem em cada área do conhecimento. As distribuições revelam a variabilidade das notas entre os diferentes grupos demográficos, destacando possíveis desigualdades no desempenho dos estudantes.")
        ),
        selectInput("area", "Selecione a Área de Conhecimento", 
                    choices = c("Matemática" = "NU_NOTA_MT", 
                                "Linguagens" = "NU_NOTA_LC",
                                "Ciências Humanas" = "NU_NOTA_CH",
                                "Ciências da Natureza" = "NU_NOTA_CN",
                                "Redação" = "NU_NOTA_REDACAO")),
        selectInput("segment", "Selecione a Segmentação", 
                    choices = c("Raça" = "cor_raca", 
                                "Sexo" = "TP_SEXO",
                                "Tipo da escola" = "escola",
                                "Localização da Escola" = 'localizacao_escola')),
        plotOutput("score_boxplot"),
        hr()
      ),
      
      ## Por faixas ----
      tabItem(
        tabName = "faixas",
        h2("Distribuição pelas faixas de renda"),
        p("Aqui trazemos mais um agrupamento, mas dessa vez utilizando a nota média de cada área do conhecimento dividas pela renda familiar do estudante."),
        selectInput("renda_materia_escolhida", "Selecione a Área de Conhecimento", 
                    choices = c("Matemática" = "NU_NOTA_MT", 
                                "Linguagens" = "NU_NOTA_LC",
                                "Ciências Humanas" = "NU_NOTA_CH",
                                "Ciências da Natureza" = "NU_NOTA_CN",
                                "Redação" = "NU_NOTA_REDACAO")),
        plotOutput("barplot_renda"),
        hr(),
        h2("Distribuição pelas faixas etárias"),
        p("Pelos boxplots, podemos ver as diferenças das notas entre as faixas etárias."),
        selectInput("faixa_idade_materia_escolhida", "Selecione a Área de Conhecimento", 
                    choices = c("Matemática" = "NU_NOTA_MT", 
                                "Linguagens" = "NU_NOTA_LC",
                                "Ciências Humanas" = "NU_NOTA_CH",
                                "Ciências da Natureza" = "NU_NOTA_CN",
                                "Redação" = "NU_NOTA_REDACAO")),
        plotOutput("boxplot_idade")
      ),
      
      ## Comparação regional ----
      tabItem(
        tabName = "comparacao_regional",
        h2("Comparação Regional das Notas"),
        div(style = "text-align: justify;",  
            p("Os gráficos de comparação regional e o mapa interativo destacam as diferenças de desempenho entre os estados brasileiros. Alguns estados mostram um desempenho superior em áreas específicas, enquanto outros apresentam desafios maiores.")
        ),
        hr(),
        h2("Mapa"),
        leafletOutput("mapa", height = 600),
        hr(),
        h2("Visualizando as notas por estado"),
        selectInput("area_conhecimento", "Selecione a Área de Conhecimento", 
                    choices = c("Matemática" = "NU_NOTA_MT", 
                                "Linguagens" = "NU_NOTA_LC",
                                "Ciências Humanas" = "NU_NOTA_CH",
                                "Ciências da Natureza" = "NU_NOTA_CN",
                                "Redação" = "NU_NOTA_REDACAO")),
        plotlyOutput("comparacao_regional")
      ),
      
      ## Correlação das notas ----
      tabItem(
        tabName = "correlacao",
        h2("Correlação entre as Notas das Provas"),
        p("Aqui trazemos a correlação entre as notas das provas. Podemos selecionar quais variáveis iremos utilizar no cálculo das correlações e visualizar como se correlacionam linearmente."),
        selectInput("selected_vars", "Selecione as Variáveis para Correlação", 
                    choices = c("Matemática" = "NU_NOTA_MT", 
                                "Linguagens" = "NU_NOTA_LC",
                                "Ciências Humanas" = "NU_NOTA_CH",
                                "Ciências da Natureza" = "NU_NOTA_CN",
                                "Redação" = "NU_NOTA_REDACAO"),
                    selected = c("NU_NOTA_MT", "NU_NOTA_LC", "NU_NOTA_CH", "NU_NOTA_CN", "NU_NOTA_REDACAO"),
                    multiple = TRUE),
        plotOutput('correlacao_plot')
      ),
      
      ## Sobre ----
      tabItem(
        tabName = "sobre",
        h2("Autores:"),
        fluidRow(
          column(12,
                 box(
                   title = NULL, 
                   status = "primary", 
                   solidHeader = TRUE, 
                   width = 12,
                   p(strong("Felipe Luís Giacomini"), br(), "Estudante do curso de Estatística da UFSCar"),
                   style = "border: 2px solid #007bff; text-align: center;"
                 ),
                 box(
                   title = NULL, 
                   status = "primary", 
                   solidHeader = TRUE, 
                   width = 12,
                   p(strong("Riquelme Pereira Pimentel"), br(), "Estudante do curso de Estatística da UFSCar"),
                   style = "border: 2px solid #007bff; text-align: center;"
                 ),
                 box(
                   title = NULL, 
                   status = "primary", 
                   solidHeader = TRUE, 
                   width = 12,
                   p(strong("Victor Antonio Pereira Martins"), br(), "Estudante do curso de Estatística da UFSCar"),
                   style = "border: 2px solid #007bff; text-align: center;"
                 )
          )
        )
      )
    )
  ),
  controlbar = dashboardControlbar(),
  title = "Dashboard Enem 2023"
)




# Server ------------------------------------------------------------------


server <- function(input, output) {
  
  ## Boxplots por Segmentação -----
  output$score_boxplot <- renderPlot({
    # obtendo os nomes mapeados para o título do gráfico
    nome_legivel_area <- nome_prova[[input$area]]
    nome_legivel_segmento <- nome_segmento[[input$segment]]
    
    # criação do boxplot 
    ggplot(df_notas, aes(x = !!sym(input$segment), y = !!sym(input$area), fill = !!sym(input$segment))) +
      geom_boxplot(outliers=FALSE) +
      labs(title = paste("Distribuição de Notas em", nome_legivel_area, "por", nome_legivel_segmento),
           x = nome_legivel_segmento,
           y = "Nota") +
      theme_minimal() +
      theme(legend.position = "none")  # Remover a legenda
    
  })
  
  ## Por faixas -----
  output$barplot_renda <- renderPlot({
    req(input$renda_materia_escolhida)  # Assegura que o input não é NULL
    
    ggplot(df_tratado, aes(x = renda_salarios_min, y = !!sym(input$renda_materia_escolhida), fill = renda_salarios_min)) +
      stat_summary(fun = mean, geom = "bar") +
      labs(title = paste("Média da Nota em", nome_prova[[input$renda_materia_escolhida]], "por Faixa de Renda Familiar (em Salários Mínimos)"),
           x = "Faixa de Renda Familiar (Salários Mínimos)",
           y = "Média da Nota") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")  # Remover a legenda
  })
  output$boxplot_idade <- renderPlot({
    ggplot(df_notas, aes(x = faixa_etaria_reduzida, y = !!sym(input$faixa_idade_materia_escolhida), fill = faixa_etaria_reduzida)) +
      geom_boxplot(outliers=F) +
      labs(title = paste("Distribuição de Notas em", nome_prova[[input$faixa_idade_materia_escolhida]], "por Faixa Etária"),
           x = "Faixa Etária",
           y = "Nota") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none") 
    }
  )
  
  ## Comparação Regional ----
  output$mapa <- renderLeaflet({
    leaflet(estados_br) %>%
      addPolygons(
        fillColor = ~colorNumeric("viridis", NULL)(as.numeric(1:nrow(estados_br))), # Cor para cada estado
        weight = 2,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        label = ~lapply(paste0(
          "<strong>", name_state, "</strong><br>",
          "Nota Matemática: ", ifelse(is.na(media_MT), "Dados indisponíveis", round(media_MT, 2)), "<br>",
          "Nota Linguagens: ", ifelse(is.na(media_LC), "Dados indisponíveis", round(media_LC, 2)), "<br>",
          "Nota Ciências Humanas: ", ifelse(is.na(media_CH), "Dados indisponíveis", round(media_CH, 2)), "<br>",
          "Nota Ciências Natureza: ", ifelse(is.na(media_CN), "Dados indisponíveis", round(media_CN, 2)), "<br>",
          "Nota Redação: ", ifelse(is.na(media_REDACAO), "Dados indisponíveis", round(media_REDACAO, 2))
        ), htmltools::HTML), 
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      setView(lng = -55, lat = -15, zoom = 4) %>% # Centralizar o mapa no Brasil
      removeControl("layersControl") # Remove os controles de camadas
  })
  
  output$comparacao_regional <- renderPlotly({
    nome_legivel <- nome_prova[[input$area_conhecimento]]
    
    p <- df_notas %>%
      group_by(SG_UF_PROVA) %>%
      summarise(media_nota = mean(.data[[input$area_conhecimento]], na.rm = TRUE)) %>%
      arrange(desc(media_nota)) %>%  # ordenar do maior para o menor
      ggplot(aes(x = reorder(SG_UF_PROVA, -media_nota), y = media_nota, fill = media_nota)) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_c() +  
      labs(title = paste("Média da nota de", str_to_lower(nome_legivel), "por estado"),
           x = "Região",
           y = "Média de Nota") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)  
  })
  
  
  ## Correlação de Notas ----
  output$correlacao_plot <- renderPlot({
    notas <- df_notas %>% 
      select(all_of(input$selected_vars)) %>%
      na.omit()
    cor_matrix <- cor(notas)
    
    corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black", number.cex = 0.7,
             addCoefasPercent = TRUE)  # Adiciona os valores de correlação nas células
  })
  
  
}


shinyApp(ui, server)

