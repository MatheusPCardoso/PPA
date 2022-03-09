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


#-----------------------------------------------------------------------------------------------------------
#Quantidades de internacoes por UF
obitosPorUf <- dadosDesejados2 %>%
  group_by(int_SIGLA_UF, semana_int) %>%
  summarise(totalDeInternacoes = sum(qtd_int))

#Convertendo semana_int para datetime
obitosPorUf$semana_int <- ym(obitosPorUf$semana_int,quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"))

#Quantidade de internacoes no RJ
RJ <- obitosPorUf %>%
  filter(int_SIGLA_UF == 'RJ')

#Plotando gráfico do RJ
ggplot(data = RJ,aes(x = semana_int, y = totalDeInternacoes)) + 
  geom_line()+
  theme_bw()

#-----------------------------------------------------------------------------------------------------------

#Quantidade de internacoes no Brasil
internacoesBrasil <- subset(dadosDesejados2, select = c(semana_int, qtd_int))

#Filtrando as internacoes do Brasil para formar um conjunto menor
internacoesBrasil <- internacoesBrasil %>%
  group_by(semana_int) %>%
  summarise(totalInternacaoMensal = sum(qtd_int))

#Convertendo semana_int em data
internacoesBrasil$semana_int <- ym(internacoesBrasil$semana_int,quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"))

#Plotando as internacoes do Brasil
ggplot(data = internacoesBrasil,aes(x = semana_int, y = totalInternacaoMensal, group = 1)) + 
  geom_line()+
  theme_bw()


#-----------------------------------------------------------------------------------------------------------
#Gerando doenças e suas respectivas internacoes
doencasMaisInternacoes <- subset(dados, select = c(classificacao_cid,qtd_int))

#Filtrando grupo gerado acima, por doença
doencasMaisInternacoes <- doencasMaisInternacoes %>%
  group_by(classificacao_cid) %>%
  summarise(internacoes = sum(qtd_int))

#Plotando internacoes por doenças
ggplot(doencasMaisInternacoes) +
 aes(x = classificacao_cid, fill = internacoes, weight = internacoes) +
 geom_bar() +
 scale_fill_gradient(low = "#79BCFF", high = "#F75656") +
 labs(x = "Doenças", y = "Internacoes", title = "Internacoes por doenças") +
 theme_bw()

#-----------------------------------------------------------------------------------------------------------

#Doencas BR
doencasSelecionadasInternacao <- subset(dados, select = c(classificacao_cid, semana_int, qtd_int))

#Convertendo semana_int em date
doencasSelecionadasInternacao$semana_int <- ym(doencasSelecionadasInternacao$semana_int,quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"))

doencasSelecionadasInternacao <- doencasSelecionadasInternacao %>%
                                group_by(classificacao_cid, semana_int) %>%
                                summarise(qtd_int = sum(qtd_int))

#-----------------------------------------------------------------------------------------------------------

#Filtrando por coqueluche
coquelucheBR <- doencasSelecionadasInternacao %>%
  filter(classificacao_cid == 'coqueluche')

#Plotando por coqueluche
ggplot(coquelucheBR) +
 aes(x = semana_int, y = qtd_int) +
 geom_line(size = 0.5, colour = "#000000") +
 labs(x = "Mês", y = "Quantidade de internações", title = "Coqueluche no Brasil") +
 theme_minimal() +
 theme(plot.title = element_text(size = 17L, face = "bold", hjust = 0.5), axis.title.y = element_text(face = "bold"), 
 axis.title.x = element_text(face = "bold"))

#Criacao de tabela para deteccao
tabela_eventos_coquelucheBR <- subset(coquelucheBR, select = c(semana_int, qtd_int))

#Criando tabela de referencia
tabela_referencia_coquelucheBR <- subset(coquelucheBR, select = c(semana_int))
tabela_referencia_coquelucheBR$event <- FALSE

#Deteccao de eventos
events_scp_coquelucheBR <- evtdet.seminalChangePoint(tabela_eventos_coquelucheBR, w=5, na.action = na.omit)

#metrica
evaluate(events_scp_coquelucheBR, tabela_referencia_coquelucheBR, metric = "confusion_matrix")
evaluate(events_scp_coquelucheBR, tabela_referencia_coquelucheBR, metric = "precision")

#Plotando eventos coquelucheBR
print(evtplot(tabela_eventos_coquelucheBR, events_scp_coquelucheBR, tabela_referencia_coquelucheBR))

#-----------------------------------------------------------------------------------------------------------
#Filtrando por hepatite b
hepatiteBBR <- doencasSelecionadasInternacao %>%
  filter(classificacao_cid == 'hepatite b')

#Plotando por hepatite b
ggplot(hepatiteBBR) +
 aes(x = semana_int, y = qtd_int) +
 geom_line(size = 0.5, colour = "#000000") +
 labs(x = "Mês", y = "Quantidade de internações", title = "Hepatite B no Brasil") +
 theme_minimal() +
 theme(plot.title = element_text(size = 17L, face = "bold", hjust = 0.5), axis.title.y = element_text(face = "bold"), 
 axis.title.x = element_text(face = "bold"))

#Criacao de tabela para deteccao
tabela_eventos_hepatiteBBR <- subset(hepatiteBBR, select = c(semana_int, qtd_int))

#Criando tabela de referencia
tabela_referencia_hepatiteBBR <- subset(hepatiteBBR, select = c(semana_int))
tabela_referencia_hepatiteBBR$event <- FALSE

#Deteccao de eventos
events_scp_hepatiteBBR <- evtdet.seminalChangePoint(tabela_eventos_hepatiteBBR, w=11, na.action = na.omit)

#metrica
evaluate(events_scp_hepatiteBBR, tabela_referencia_hepatiteBBR, metric = "confusion_matrix")
evaluate(events_scp_hepatiteBBR, tabela_referencia_hepatiteBBR, metric = "precision")

#Plotando eventos coquelucheBR
print(evtplot(tabela_eventos_hepatiteBBR, events_scp_hepatiteBBR, tabela_referencia_hepatiteBBR))

#-----------------------------------------------------------------------------------------------------------

#Filtrando por meningite por Haemophilus
meningiteHaemophilusBR <- doencasSelecionadasInternacao %>%
  filter(classificacao_cid == 'meningite por Haemophilus')

#Plotando por meningite por Haemophilus
ggplot(meningiteHaemophilusBR) +
  aes(x = semana_int, y = qtd_int) +
  geom_line(size = 0.5, colour = "#000000") +
  labs(x = "Mês", y = "Quantidade de internações", title = "Meningite por Haemophilus no Brasil") +
  theme_minimal() +
  theme(plot.title = element_text(size = 17L, face = "bold", hjust = 0.5), axis.title.y = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"))

#Criacao de tabela para deteccao
tabela_eventos_meningiteHaemophilusBR <- subset(meningiteHaemophilusBR, select = c(semana_int, qtd_int))

#Criando tabela de referencia
tabela_referencia_meningiteHaemophilusBR <- subset(meningiteHaemophilusBR, select = c(semana_int))
tabela_referencia_meningiteHaemophilusBR$event <- FALSE

#Deteccao de eventos
events_scp_meningiteHaemophilusBR <- evtdet.seminalChangePoint(tabela_eventos_meningiteHaemophilusBR, w=11, na.action = na.omit)

#metrica
evaluate(events_scp_meningiteHaemophilusBR, tabela_referencia_meningiteHaemophilusBR, metric = "confusion_matrix")
evaluate(events_scp_meningiteHaemophilusBR, tabela_referencia_meningiteHaemophilusBR, metric = "precision")


#Plotando eventos coquelucheBR
print(evtplot(tabela_eventos_meningiteHaemophilusBR, events_scp_meningiteHaemophilusBR, tabela_referencia_meningiteHaemophilusBR))

#-----------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------

#Doencas RJ
doencasSelecionadasInternacaoRJ <- subset(dados, select = c(int_SIGLA_UF, classificacao_cid, semana_int, qtd_int))

#Filtrando por RJ
doencasSelecionadasInternacaoRJ <- doencasSelecionadasInternacaoRJ %>%
                                filter(int_SIGLA_UF == 'RJ')

#Convertendo semana_int em date
doencasSelecionadasInternacaoRJ$semana_int <- ym(doencasSelecionadasInternacaoRJ$semana_int,quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"))

doencasSelecionadasInternacaoRJ <- doencasSelecionadasInternacaoRJ %>%
  group_by(classificacao_cid, semana_int) %>%
  summarise(qtd_int = sum(qtd_int))

#-----------------------------------------------------------------------------------------------------------

#Filtrando por coqueluche
coquelucheRJ <- doencasSelecionadasInternacaoRJ %>%
  filter(classificacao_cid == 'coqueluche')

#Plotando por coqueluche
ggplot(coquelucheRJ) +
  aes(x = semana_int, y = qtd_int) +
  geom_line(size = 0.5, colour = "#000000") +
  labs(x = "Mês", y = "Quantidade de internações", title = "Coqueluche no RJ") +
  theme_minimal() +
  theme(plot.title = element_text(size = 17L, face = "bold", hjust = 0.5), axis.title.y = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"))

#Criacao de tabela para deteccao
tabela_eventos_coquelucheRJ <- subset(coquelucheRJ, select = c(semana_int, qtd_int))

#Criando tabela de referencia
tabela_referencia_coquelucheRJ <- subset(coquelucheRJ, select = c(semana_int))
tabela_referencia_coquelucheRJ$event <- FALSE

#Deteccao de eventos
events_scp_coquelucheRJ <- evtdet.seminalChangePoint(tabela_eventos_coquelucheRJ, w=5, na.action = na.omit)

#metrica
evaluate(events_scp_coquelucheRJ, tabela_referencia_coquelucheRJ, metric = "confusion_matrix")
evaluate(events_scp_coquelucheRJ, tabela_referencia_coquelucheRJ, metric = "precision")

#Plotando eventos coquelucheRJ
print(evtplot(tabela_eventos_coquelucheRJ, events_scp_coquelucheRJ, tabela_referencia_coquelucheRJ))

#-----------------------------------------------------------------------------------------------------------
#Filtrando por hepatite b
hepatiteBRJ <- doencasSelecionadasInternacaoRJ %>%
  filter(classificacao_cid == 'hepatite b')

#Plotando por hepatite b
ggplot(hepatiteBRJ) +
  aes(x = semana_int, y = qtd_int) +
  geom_line(size = 0.5, colour = "#000000") +
  labs(x = "Mês", y = "Quantidade de internações", title = "Hepatite B no RJ") +
  theme_minimal() +
  theme(plot.title = element_text(size = 17L, face = "bold", hjust = 0.5), axis.title.y = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"))

#Criacao de tabela para deteccao
tabela_eventos_hepatiteBRJ <- subset(hepatiteBRJ, select = c(semana_int, qtd_int))

#Criando tabela de referencia
tabela_referencia_hepatiteBRJ <- subset(hepatiteBRJ, select = c(semana_int))
tabela_referencia_hepatiteBRJ$event <- FALSE

#Deteccao de eventos
events_scp_hepatiteBRJ <- evtdet.seminalChangePoint(tabela_eventos_hepatiteBRJ, w=5, na.action = na.omit)

#metrica
evaluate(events_scp_hepatiteBRJ, tabela_referencia_hepatiteBRJ, metric = "confusion_matrix")
evaluate(events_scp_hepatiteBRJ, tabela_referencia_hepatiteBRJ, metric = "precision")

#Plotando eventos coquelucheRJ
print(evtplot(tabela_eventos_hepatiteBRJ, events_scp_hepatiteBRJ, tabela_referencia_hepatiteBRJ))

#-----------------------------------------------------------------------------------------------------------

#Filtrando por meningite por Haemophilus
meningiteHaemophilusRJ <- doencasSelecionadasInternacaoRJ %>%
  filter(classificacao_cid == 'meningite por Haemophilus')

#Plotando por meningite por Haemophilus
ggplot(meningiteHaemophilusRJ) +
  aes(x = semana_int, y = qtd_int) +
  geom_line(size = 0.5, colour = "#000000") +
  labs(x = "Mês", y = "Quantidade de internações", title = "Meningite por Haemophilus no RJ") +
  theme_minimal() +
  theme(plot.title = element_text(size = 17L, face = "bold", hjust = 0.5), axis.title.y = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"))

#Criacao de tabela para deteccao
tabela_eventos_meningiteHaemophilusRJ <- subset(meningiteHaemophilusRJ, select = c(semana_int, qtd_int))

#Criando tabela de referencia
tabela_referencia_meningiteHaemophilusRJ <- subset(meningiteHaemophilusRJ, select = c(semana_int))
tabela_referencia_meningiteHaemophilusRJ$event <- FALSE

#Deteccao de eventos
events_scp_meningiteHaemophilusRJ <- evtdet.seminalChangePoint(tabela_eventos_meningiteHaemophilusRJ, w=11, na.action = na.omit)

#metrica
evaluate(events_scp_meningiteHaemophilusRJ, tabela_referencia_meningiteHaemophilusRJ, metric = "confusion_matrix")
evaluate(events_scp_meningiteHaemophilusRJ, tabela_referencia_meningiteHaemophilusRJ, metric = "precision")

#Plotando eventos coquelucheRJ
print(evtplot(tabela_eventos_meningiteHaemophilusRJ, events_scp_meningiteHaemophilusRJ, tabela_referencia_meningiteHaemophilusRJ))

#-----------------------------------------------------------------------------------------------------------


########################### Internacoes no Brasil #####################################

#Criando dataframe de eventos
tabela_eventos_brasil <- internacoesBrasil

#Criando dataframe de referencia
referencia <- subset(internacoesBrasil, select = c(semana_int))
referencia$event <- FALSE

#============================== Métrica ==========================================

#metrica
evaluate(events_scp, referencia, metric = "confusion_matrix")
evaluate(events_scp, referencia, metric = "F1")

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