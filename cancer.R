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

-------------------------------------------------------------------------------
  
dim(dados) #Nosso banco de dados é composto por 1.103.941 observações e 100 variáveis.

str(dados) #Para ver os tipos de dados

head(dados) #Para ler os primeiros dados

tail(dados) #Para ler os últimos dados

colnames(dados) #Para saber o nome das colunas


#Encontraremos as idades mínimas e máximas dos pacientes.
max(dados$IDADE) #Para encontrarmos a idade máxima dos pacientes.
min(dados$IDADE) #Para encontrarmos a idade miníma dos pacientes.


dados$PCID <- 1 # Criar nova coluna com quantidade 1 de paciente por linha

----------------------------------------------------------------------------
  
  ############################ MAPA 1 ######################################

# mapa estado de São Paulo
mapa <- shapefile("SP_Municipios_2021.shp")
plot(mapa)

# criando coluna IBGE com os dados de CD_MUN
mapa$IBGE <- mapa$CD_MUN

# mapa remodelado (mais bonito)
tmap_mode("plot")
tm_shape(mapa)+
  tm_fill()+
  tm_borders()

############## FALTA CUSTOMIZAR O MAPA E COLOCAR OS DADOS DENTRO DAS CIDADES

-----------------------------------------------------------------------------
 
# Quantidade de casos de câncer por ano e por sexo
  
anos <- dados
  anos$SEXO[anos$SEXO==1] <- "Homem" # Renomeando os valores das observações de 1 para Homem e 2 para Mulher.
  anos$SEXO[anos$SEXO==2] <- "Mulher" 

  
############# PRIMEIRO GRÁFICO - TOTAL DE PACIENTES COM CÂNCER POR SEXO ###############
  
cancersexo <- group_by(anos,SEXO) %>%
  summarise(sum(PCID))

cancersexo$TOTAL <- cancersexo$`sum(PCID)`

g1 <- ggplot(cancersexo, aes(x = SEXO, y = TOTAL, fill = SEXO)) +
  geom_col(position = "dodge")+
  labs(title = "Total de pacientes com câncer por ano de 2000 a 2022",
       x = "Sexo", 
       y = "Total de pacientes")

ggplotly(g1 + scale_fill_manual(values=c('#00BFFF',
                                         '#DA70D6')))

---------------------------------------------------------------------------------------


############# SEGUNDO GRÁFICO - TOTAL DE PACIENTES COM CÂNCER POR ANO ###############

anos <-group_by(anos,ANODIAG, SEXO, ESCOLARI, CLINICA) %>%
  summarise(sum(PCID))
anos$TOTAL <- anos$`sum(PCID)`

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
