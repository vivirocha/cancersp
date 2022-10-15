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

-----------------------------------------------------------------------------
 
# Quantidade de casos por ano
  
anos <- dados
  anos$SEXO[anos$SEXO==1] <- "Homem" # Renomeando os valores das observações de 1 para Homem e 2 para Mulher.
  anos$SEXO[anos$SEXO==2] <- "Mulher" 

anos <- group_by(anos, ANODIAG, SEXO) %>%
  summarise(sum(PCID))


######plotar gráfico de série temporal ou barras usando o pacote PLOTLY

graf1 <- ggplot(anos, aes(x = ANODIAG, y = sum(PCID))) +
  geom_bar() 

ggplotly(graf1)


?geom_bar

-----------------------------------------------------------------------------


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

