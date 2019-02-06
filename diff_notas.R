library(plotly)
library(readxl)
library(dplyr)

Sys.setenv("plotly_username"="kassio.silva")
Sys.setenv("plotly_api_key"="0BULwdSIo8khglNH31kJ")

ingressantes2017nota_M <- read.csv("C:/Users/erica.santos/workspace/trabalho/THE_rankings/Impact Ranking/consulta_geral_discente (2).csv",
                                   header = TRUE, sep=";", colClasses = c("character"))

ingressantes2017nota_F <- read.csv("C:/Users/erica.santos/workspace/trabalho/THE_rankings/Impact Ranking/consulta_geral_discente (3).csv",
                                   header = TRUE, sep=";", colClasses = c("character"))


ingressantes2017nota_M <- filter(ingressantes2017nota_M, startsWith(ingressantes2017nota_M$matricula, '2017'))
ingressantes2017nota_F <- filter(ingressantes2017nota_F, startsWith(ingressantes2017nota_F$matricula, '2017'))

iris %>% group_by(Species) %>% summarise(avg = mean(Sepal.Length))

# Notas médias dos cursos masculino

nota_M = ingressantes2017nota_M %>% group_by(curso)
nota_M$ira <- as.numeric(nota_M$ira)
nota_M$iea <- as.numeric(nota_M$iea)

nota_M = nota_M %>% summarise(avg_M = mean(ira))
nota_M = nota_M[complete.cases(nota_M), ]

# Notas médias dos cursos feminino

nota_F = ingressantes2017nota_F %>% group_by(curso)
nota_F$ira <- as.numeric(nota_F$ira)
nota_F$iea <- as.numeric(nota_F$iea)

nota_F = nota_F %>% summarise(avg_F = mean(ira))
nota_F = nota_F[complete.cases(nota_F), ]

# Numero de homens por curso
n_M <- ingressantes2017nota_M %>% group_by(curso) %>% count()
M_nota_numero = inner_join(nota_M, n_M, by="curso")

# Numero de mulheres por curso
n_F <- ingressantes2017nota_F %>% group_by(curso) %>% count()
F_nota_numero = inner_join(nota_F, n_F, by="curso")  

data = inner_join(F_nota_numero, M_nota_numero, by="curso")

data = data %>% mutate(diff = abs(data$avg_M-data$avg_F), indicador = ifelse(avg_F > avg_M, 1, 0))

# Plotando o gráfico

p <- plot_ly(data, x = ~n.y , y = ~n.x, 
             text = ~paste("Curso: ", curso, "Diferença: ", round(diff, 3)), type = 'scatter', mode = 'markers',
             marker = list(size = ~2*diff, opacity = 0.5, color = ~indicador)) %>%
  layout(title = 'Diferença de notas entre homens e mulheres por curso',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE))

chart_link = api_create(p, filename="diff_notas")
chart_link

# Testes de hipóteses para diferença de médias:


