library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
#Importando biblioteca de Harbinger do git
source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger/master/harbinger.R")

#Bibliotecas para manipular dados  


#Importando base de dados online
dados <- read.csv('https://raw.githubusercontent.com/cefet-rj-pratica-pesquisa/tema5/main/sih_IMUNOPREVENIVEIS.csv?token=GHSAT0AAAAAABQ3PBFAIJQ3HX4WEQACQ5LCYPQOYHQ')

#Importando base de dados local
dados <- read_excel('C:/Users/Matheus/Documents/R/imunopreviniveis.xlsx')
dados <- read_excel('D:/Repositórios/r/ByPython.xlsx')
#rm(obitosPorUf) #para apagar o objeto

#Coletando apenas dados a serem analisados
dadosDesejados <- subset(dados, select = c(int_SIGLA_UF, semana_int))

dadosDesejados2 <- subset(dados, select = c(int_SIGLA_UF, semana_int, qtd_int))

#Coletando UFs contidas na base de dados
ufs <- unique(dados$int_SIGLA_UF)
#ufs2 <- unique(subset(dados, select = c(int_SIGLA_UF)))
rm(ufs2)

#Quantidades de mortes por UF
obitosPorUf <- dadosDesejados2 %>%
  group_by(int_SIGLA_UF, semana_int) %>%
  summarise(totalDeMortes = sum(qtd_int))

#Convertendo semana_int para datetime
obitosPorUf$semana_int <- ym(obitosPorUf$semana_int,quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"))

#Quantidade de mortes no RJ
RJ <- obitosPorUf %>%
  filter(int_SIGLA_UF == 'RJ')

#Plotando gráfico do RJ
ggplot(data = RJ,aes(x = semana_int, y = totalDeMortes)) + 
  geom_line()+
  theme_bw()


#Quantidade de mortes no Brasil
mortesBrasil <- subset(dadosDesejados2, select = c(semana_int, qtd_int))

#Filtrando as mortes do Brasil para formar um conjunto menor
mortesBrasil <- mortesBrasil %>%
  group_by(semana_int) %>%
  summarise(totalMorteMensal = sum(qtd_int))

#Convertendo semana_int em data
mortesBrasil$semana_int <- ym(mortesBrasil$semana_int,quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"))

#Plotando as mortes do Brasil
ggplot(data = mortesBrasil,aes(x = semana_int, y = totalMorteMensal, group = 1)) + 
  geom_line()+
  theme_bw()

#Gerando doenças e suas respectivas mortes
doencasMaisMortes <- subset(dados, select = c(classificacao_cid,qtd_int))

#Filtrando grupo gerado acima, por doença
doencasMaisMortes <- doencasMaisMortes %>%
  group_by(classificacao_cid) %>%
  summarise(mortes = sum(qtd_int))

#Plotando mortes por doenças
ggplot(doencasMaisMortes) +
 aes(x = classificacao_cid, fill = mortes, weight = mortes) +
 geom_bar() +
 scale_fill_gradient(low = "#79BCFF", high = "#F75656") +
 labs(x = "Doenças", y = "Mortes", title = "Mortes por doenças") +
 theme_bw()

################################## Mortes no RJ #######################################
#Criando dataframe de eventos
tabela_eventos <- subset(RJ, select = c(semana_int, totalDeMortes))

#Criando dataframe de referencia
referencia <- subset(RJ, select = c(semana_int))
referencia$event <- FALSE
#detecção
events_scp <- evtdet.seminalChangePoint(tabela_eventos, w=50, na.action = na.omit)

#metrica
evaluate(events_scp, referencia, metric = "confusion_matrix")
evaluate(events_scp, referencia, metric = "precision")

#plot
print(evtplot(tabela_eventos, events_scp, referencia))

#========================== Mortes no Brasil =====================================

#Criando dataframe de eventos
tabela_eventos_brasil <- mortesBrasil

#Criando dataframe de referencia
referencia <- subset(mortesBrasil, select = c(semana_int))
referencia$event <- FALSE

#============================== Métrica ==========================================

#metrica
evaluate(events_scp, referencia, metric = "confusion_matrix")
evaluate(events_scp, referencia, metric = "precision")

ARIMA <- function(data) forecast::auto.arima(data)

#============================== Detecção =========================================

#deteccao
events_scp <- evtdet.seminalChangePoint(tabela_eventos_brasil, w=50, na.action = na.omit)

#Deteccao com o Metodo AN:
events_an <- evtdet.an_outliers(tabela_eventos_brasil, w=20, alpha=1)

#Deteccao com ARIMA
events_cf <- evtdet.changeFinder(tabela_eventos_brasil, mdl=ARIMA, m=5)

#============================= Plotagem ==========================================

#plotagem
print(evtplot(tabela_eventos_brasil, events_scp, referencia))

# Plotagem AN
print(evtplot(tabela_eventos_brasil,events_an,referencia))

#Plotagem ARIMA
print(evtplot(tabela_eventos_brasil,events_cf, referencia, mark.cp=TRUE))