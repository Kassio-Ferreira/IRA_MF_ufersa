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

f <- list(
  family = "Times New Roman",
  size = 24,
  color = "#7f7f7f"
)

x <- list(
  title = "Masculino",
  titlefont = f,
  showgrid = FALSE
)
y <- list(
  title = "Feminino",
  titlefont = f,
  showgrid = FALSE
)

p <- plot_ly(data, x = ~n.y , y = ~n.x, 
             text = ~paste("Curso: ", curso, "Diferença: ", round(diff, 3)), type = 'scatter', mode = 'markers',
             marker = list(size = ~2*diff, opacity = 0.5, color = ~indicador)) %>%
  layout(title = 'Diferença de notas entre homens e mulheres por curso',
         xaxis = x,
         yaxis = y)

p

#chart_link = api_create(p, filename="diff_notas")
#chart_link

# Testes de hipóteses para diferença de médias:

# Teste t
cursos_para_teste_t = data[data$n.x >= 25 & data$n.y >= 25, ]$curso

p_valores_t = c(0,0,0,0,0)

for(i in cursos_para_teste_t){
  curso_M = filter(ingressantes2017nota_M, curso == i)
  curso_F = filter(ingressantes2017nota_F, curso == i)
  
  rr = t.test(as.numeric(curso_M$ira),as.numeric(curso_F$ira))
  p_valores_t = rbind(p_valores_t, c(as.numeric(curso_M$ira) %>% mean,
                                     nrow(curso_M),
                                     as.numeric(curso_F$ira) %>% mean, 
                                     nrow(curso_F),
                                     rr$p.value))
}

p_valores_t = p_valores_t[-1, ]
rownames(p_valores_t) = cursos_para_teste_t
p_valores_t

# Teste de Mann-Whitney

cursos_para_teste_mw = data[(data$n.x <= 25 & data$n.x >= 8) & (data$n.y <= 25 & data$n.y >= 8), ]$curso

p_valores_mw = c(0,0,0)

for(i in cursos_para_teste_mw){
  curso_M = filter(ingressantes2017nota_M, curso == i)
  curso_F = filter(ingressantes2017nota_F, curso == i)
  
  rr = wilcox.test(as.numeric(curso_M$ira),as.numeric(curso_F$ira))
  p_valores_mw = rbind(p_valores_mw, c(as.numeric(curso_M$ira) %>% mean,
                                      nrow(curso_M),
                                      as.numeric(curso_F$ira) %>% mean, 
                                      nrow(curso_F),
                                      rr$p.value))
}

p_valores_mw = p_valores_mw[-1, ]
rownames(p_valores_mw) = cursos_para_teste_mw

p_valores_mw