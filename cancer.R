#######################################################################
###                                                                 ###
###                  TRABALHO DE CONCLUSÃO DE CURSO                 ###
###                    DATA SCIENCE & ANALYTICS                     ###
###                            USP/ESALQ                            ###
###                                                                 ###
#######################################################################

# O presente trabalho será desenvolvido utilizando a base de dados do 
# Registro Hospitalar de Câncer (RHC) disponível no endereço online
# da Fundação Oncocentro de São Paulo (FOSP) 
#http://www.fosp.saude.sp.gov.br/fosp/diretoria-adjunta-de-informacao-e-epidemiologia/rhc-registro-hospitalar-de-cancer/banco-de-dados-do-rhc/

install.packages("dplyr")
install.packages("raster")
install.packages("tmap")
install.packages("plotly")
library(dplyr)
library(foreign)
library(lubridate)
library(raster)
library(tmap)
library(plotly)


dados = read.dbf(file = "pacigeral.dbf") #pacote raster para leitura de .dbf

########################################################################
###                                                                  ###
###                            ANÁLISE EXPLORATÓRIA                  ###
###                                                                  ###
########################################################################

dim(dados) #Nosso banco de dados é composto por 1.103.941 observações e 100 variáveis.

str(dados) #Para ver os tipos de dados

head(dados) #Para ler os primeiros dados

tail(dados) #Para ler os últimos dados

colnames(dados) #Para saber o nome das colunas

#Encontraremos as idades mínimas e máximas dos pacientes.
max(dados$IDADE) #Para encontrarmos a idade máxima dos pacientes.
min(dados$IDADE) #Para encontrarmos a idade miníma dos pacientes.

#Encontraremos a média, mediana e quartis das idades dos pacientes.
summary(dados$IDADE)

#Encontraremos a moda da idade dos pacientes

frequencia <- table(dados$IDADE)

frequencia[frequencia == max(frequencia)]

moda <- function(x) {
  tab = table(x)
  tab[tab == max(tab)]
}

moda(dados$IDADE)

dados$PCID <- 1 # Criar nova coluna com quantidade 1 de paciente por linha

----------------------------------------------------------------------------
  
############################ MAPA 1 ######################################

# mapa estado de São Paulo
mapa <- shapefile("SP_Municipios_2021.shp")

# criando coluna IBGE com os dados de CD_MUN
mapa$IBGE <- mapa$CD_MUN

# mapa remodelado (mais bonito)
tmap_mode("plot")
tm_shape(mapa)+
  tm_fill()+
  tm_borders()

############## FALTA CUSTOMIZAR O MAPA E COLOCAR OS DADOS DENTRO DAS CIDADES

-----------------------------------------------------------------------------
 
# Criando um dataset para não trabalhar em cima do "original" 
  
anos <- dados

anos <-group_by(anos,ANODIAG, SEXO, ESCOLARI, CATEATEND, TOPO, TOPOGRUP, NAOTRAT, ULTINFO, TRATCONS, TRATAMENTO) %>%
  summarise(sum(PCID))
anos$TOTAL <- anos$`sum(PCID)`

  anos$SEXO[anos$SEXO==1] <- "Homem" # Renomeando os valores das observações de 1 para Homem e 2 para Mulher.
  anos$SEXO[anos$SEXO==2] <- "Mulher" 
  anos$ESCOLARI[anos$ESCOLARI==1] <- "Analfabeto"
  anos$ESCOLARI[anos$ESCOLARI==2] <- "Ens. Fund. Incompleto"
  anos$ESCOLARI[anos$ESCOLARI==3] <- "Ens. Fund. Completo"
  anos$ESCOLARI[anos$ESCOLARI==4] <- "Ens. Médio"
  anos$ESCOLARI[anos$ESCOLARI==5] <- "Superior"
  anos$ESCOLARI[anos$ESCOLARI==9] <- "Ignorada"
  anos$CATEATEND[anos$CATEATEND==1] <- "Convenio"
  anos$CATEATEND[anos$CATEATEND==2] <- "SUS"
  anos$CATEATEND[anos$CATEATEND==3] <- "Particular"
  anos$CATEATEND[anos$CATEATEND==9] <- "Sem info."
  anos$NAOTRAT[anos$NAOTRAT==1] <- "Recusa do tratamento"
  anos$NAOTRAT[anos$NAOTRAT==2] <- "Doença Avancada, falta de condicoes clinicas"
  anos$NAOTRAT[anos$NAOTRAT==3] <- "Outras doencas associadas"
  anos$NAOTRAT[anos$NAOTRAT==4] <- "Abandono de tratamento"
  anos$NAOTRAT[anos$NAOTRAT==5] <- "Obito por cancer"
  anos$NAOTRAT[anos$NAOTRAT==6] <- "Obito por outras causas, SOE"
  anos$NAOTRAT[anos$NAOTRAT==7] <- "Outras"
  anos$NAOTRAT[anos$NAOTRAT==8] <- "Nao se aplica"
  anos$NAOTRAT[anos$NAOTRAT==9] <- "Sem informacao"
  anos$ULTINFO[anos$ULTINFO==1] <- "Vivo, com cancer"
  anos$ULTINFO[anos$ULTINFO==2] <- "Vivo, SOE"
  anos$ULTINFO[anos$ULTINFO==3] <- "Obito por cancer"
  anos$ULTINFO[anos$ULTINFO==4] <- "Obito por outras causas, SOE"

############# PRIMEIRO GRÁFICO - TOTAL DE PACIENTES COM CÂNCER POR SEXO ###############
  
cancersexo <- group_by(anos,SEXO) %>%
  summarise(sum(PCID))

cancersexo$TOTAL <- cancersexo$`sum(PCID)`

g1 <- ggplot(cancersexo, aes(x = SEXO, y = TOTAL, fill = SEXO)) +
  geom_col(position = "dodge")+
  labs(title = "Total de pacientes com câncer por sexo",
       x = "Sexo", 
       y = "Total de pacientes")

ggplotly(g1 + scale_fill_manual(values=c('#00BFFF',
                                         '#DA70D6')))

---------------------------------------------------------------------------------------


############# SEGUNDO GRÁFICO - TOTAL DE PACIENTES COM CÂNCER POR ANO ###############

g2 <- ggplot(anos, aes(x = ANODIAG, y = TOTAL, fill = SEXO)) +
  geom_col(position = "dodge", stat='identity')+
  labs(title = "Total de pacientes com câncer por ano de 2000 a 2022",
       x = "Anos", 
       y = "Total de pacientes")

ggplotly(g2 + scale_fill_manual(values=c('#00BFFF',
                                         '#DA70D6')))

-----------------------------------------------------------------------------

############# TERCEIRO GRÁFICO - TOTAL DE PACIENTES COM CÂNCER POR ESCOLARIDADE ###############

escolaridade <-group_by(anos, ESCOLARI) %>%
  summarise(sum(PCID))
escolaridade$TOTAL <- escolaridade$`sum(PCID)`

g3 <- ggplot(anos, aes(x = ESCOLARI, y = TOTAL, fill = factor(ESCOLARI))) +
  geom_bar(position = "dodge", stat='identity')+ 
  labs(title = "Total de pacientes com câncer por Escolaridade",
       x = "Escolaridade", 
       y = "Total de pacientes")

ggplotly(g3+ scale_fill_brewer(palette = "BrBG"))

############# QUARTO GRÁFICO - TOTAL DE PACIENTES COM CÂNCER POR ESPECIALIDADE ###############

clinica <-group_by(anos,ANODIAG, CLINICA) %>%
  summarise(sum(PCID))
clinica$TOTAL <- clinica$`sum(PCID)`

g4 <- ggplot(clinica, aes(x = CLINICA, y = TOTAL, fill = factor(CLINICA))) +
  geom_bar(position = "dodge", stat='identity')+ 
  labs(title = "Total de pacientes com câncer por Especialidade",
       x = "Especialidade", 
       y = "Total de pacientes")

ggplotly(g4+ scale_fill_brewer(palette = "BrBG"))
