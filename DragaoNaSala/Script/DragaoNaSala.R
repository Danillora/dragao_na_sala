####################################################################
####################################################################
####################################################################
#################       O DRAG�O NA SALA            ################
################       Material de replica��o       ################
################           do TCC de                ################
################                                    ################
################       DANILLO RAFAEL BATISTA       ################
################          DO NASCIMENTO             ################
################                                    ################
#################  ORIENTA��O: IAN REBOU�AS BATISTA ################
####################################################################
####################################################################
####################################################################


#chamando os pacotes necess�rios

library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(tidyselect)
library(ggplot2)
options(scipen = 999) ## desativando nota��o cient�fica
setwd() ##Defina aqui o diret�rio de trabalho onde estar� a pasta matriz do material de replica��o

### Dados do com�rcio dos pa�ses da Am�rica do Sul-China
## os dados de com�rcio mundial baixados do UN Comtrade Database s�o baixados em partes, as pastas comtrade_n

#abrindo bancos

comtrade_1 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtrade_1.csv")
comtrade_2 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtrade_2.csv")

#selecionando as variaveis no banco 
comtrade_1 <- select(comtrade_1, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                     -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                     -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                     -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                     -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                     -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                     -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                     -("Flag"), -("Classification"))

comtrade_2 <- select(comtrade_2, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                     -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                     -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                     -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                     -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                     -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                     -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                     -("Flag"), -("Classification"))

#juntando os bancos

BANCO <- rbind(comtrade_1, comtrade_2)

#removendo os dois bancos originais (pesados e poluem o environment---> )

rm(comtrade_1, comtrade_2)

#c�digo para filtrar o banco:

BANCO <- filter(BANCO, `Trade Flow` ==  "Export" |               #Selecionando somente Exporta��o e Importa��o
                     `Trade Flow` == "Import")            
BANCO <- filter(BANCO, `Reporter` != "Venezuela")                #Selecionando tudo que n�o for Venezuela 
BANCO <- filter(BANCO, `Partner` == "China")                     #Selecionando somente China

BANCO$Year <- as.numeric(BANCO$Year)
BANCO <- filter(BANCO, `Year` > 1997)                           #Selecionando o per�od 1998-2018
BANCO <- filter(BANCO, `Year` < 2019)


# Plotando o com�rcio da Regiao com a China ao longo dos anos (GR�FICO 1)

ggplot(data = BANCO) + 
  geom_smooth(mapping = aes(x = Year, y = `Trade Value (US$)`), se = FALSE) + xlab("Ano") + ylab("US$")

##### O BANCO ser� o banco com dados do com�rcio dos pa�ses da regi�o com a China


### Dados do com�rcio dos pa�ses da Am�rica do Sul com pa�ses da Am�rica do Sul

#abrindo bancos

comtrade_3 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtrade_3.csv")
comtrade_4 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtrade_4.csv")
comtrade_5 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtrade_5.csv")
comtrade_6 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtrade_6.csv")
comtrade_7 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtrade_7.csv")
comtrade_8 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtrade_8.csv")

#selecionando as variaveis no banco 

comtrade_3 <- select(comtrade_3, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                     -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                     -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                     -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                     -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                     -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                     -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                     -("Flag"), -("Classification"))

comtrade_4 <- select(comtrade_4, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                     -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                     -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                     -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                     -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                     -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                     -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                     -("Flag"), -("Classification"))

comtrade_5 <- select(comtrade_5, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                     -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                     -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                     -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                     -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                     -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                     -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                     -("Flag"), -("Classification"))

comtrade_6 <- select(comtrade_6, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                     -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                     -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                     -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                     -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                     -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                     -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                     -("Flag"), -("Classification"))

comtrade_7 <- select(comtrade_7, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                     -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                     -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                     -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                     -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                     -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                     -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                     -("Flag"), -("Classification"))

comtrade_8 <- select(comtrade_8, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                     -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                     -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                     -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                     -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                     -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                     -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                     -("Flag"), -("Classification"))


BANCO2 <- rbind(comtrade_3, comtrade_4, comtrade_5, comtrade_6) ## Banco com com�rcio intrarregional

BANCO3 <- rbind(comtrade_7, comtrade_8) ## Banco com total de com�rcio dos pa�ses

rm(comtrade_3)    #removendo os bancos maiores e originais 
rm(comtrade_4)
rm(comtrade_5)
rm(comtrade_6)
rm(comtrade_7)
rm(comtrade_8)

BANCO2$Year <- as.numeric(BANCO2$Year) #filtrando o per�odo
BANCO2 <- filter(BANCO2, `Year` > 1997)
BANCO2 <- filter(BANCO2, `Year` < 2019)

BANCO3$Year <- as.numeric(BANCO3$Year) #filtrando o per�odo
BANCO3 <- filter(BANCO3, `Year` > 1997)
BANCO3 <- filter(BANCO3, `Year` < 2019)

BANCO3 <- BANCO3 %>% 
  rename(`Total` = "Trade Value (US$)") ## Total � o nome da vari�vel "Total de Com�rcio do Pa�s"

#### O BANCO2 � o banco com os dados do com�rcio dos pa�ses da regi�o com os pa�ses da regi�o
#### O BANCO3 � o banco com dados do com�rcio total que os pa�ses da Am do Sul realizaram naquele ano

BANCO4 <- merge(BANCO2, BANCO3, by=c("Trade Flow", "Year", "Reporter")) ##Juntanto os dois bancos

BANCO4$`Trade Value (US$)` <- as.numeric(BANCO4$`Trade Value (US$)`) #tornando as variaveis num�ricas
BANCO4$Total <- as.numeric(BANCO4$Total)

BANCO4 <- filter(BANCO4, `Reporter` != `Partner.x`)  ## Excluindo casos onde o pa�s importa/exporta para ele mesmo

BANCO4$`Trade Value (US$)` <- NULL    #Excluidno essa variavel pra nao confundir com a do BANCO, quando juntar no BANCOFINAL

BANCO5 <- merge(BANCO, BANCO3, by=c("Trade Flow", "Year", "Reporter")) ##Juntando o banco com dados com a China (BANCO) com o banco que tem os dados do Total do com�rcio dos pa�sses da regi�o (BANCO3)

BANCO5$`Trade Value (US$)` <- as.numeric(BANCO5$`Trade Value (US$)`) ## transformando as vari�veis em num�ricas
BANCO5$Total <- as.numeric(BANCO5$Total)

BANCO5$prop_chi <- BANCO5$`Trade Value (US$)`/ BANCO5$Total ##Criando a vari�vel prop_chi, dividindo comercio com a china / Total

ggplot(data = BANCO5) + 
  geom_smooth(mapping = aes(x = Year, y = prop_chi), se= FALSE)     #evolu��o ao longo do tempo do peso do regional no com�rcio total da regi�o

###### TOTAL DE COM�RCIO DO MUNDO 
##caregando os bancos
comtradeW_1 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtradeW_1.csv")
comtradeW_2 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtradeW_2.csv")
comtradeW_3 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtradeW_3.csv")
comtradeW_4 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtradeW_4.csv")
comtradeW_5 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtradeW_5.csv")

#selecionando as variaveis
comtradeW_1 <- select(comtradeW_1, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                      -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                      -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                      -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                      -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                      -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                      -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                      -("Flag"), -("Classification"))

comtradeW_2 <- select(comtradeW_2, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                      -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                      -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                      -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                      -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                      -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                      -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                      -("Flag"), -("Classification"))

comtradeW_3 <- select(comtradeW_3, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                      -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                      -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                      -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                      -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                      -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                      -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                      -("Flag"), -("Classification"))

comtradeW_4 <- select(comtradeW_4, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                      -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                      -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                      -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                      -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                      -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                      -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                      -("Flag"), -("Classification"))

comtradeW_5 <- select(comtradeW_5, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                      -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                      -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                      -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                      -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                      -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                      -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                      -("Flag"), -("Classification"))

#juntando os bancos
BANCOW <- rbind(comtradeW_1, comtradeW_2, comtradeW_3, comtradeW_4, comtradeW_5)

#removendo os bancos originais
rm(comtradeW_1, comtradeW_2, comtradeW_3, comtradeW_4, comtradeW_5)

#filtrando somente exporta��o e importa��o
BANCOW <- filter(BANCOW, `Trade Flow` ==  "Export" | 
                   `Trade Flow` == "Import")  

#filtrando o per�odo
BANCOW$Year <- as.numeric(BANCOW$Year)
BANCOW <- filter(BANCOW, `Year` > 1997)                         
BANCOW <- filter(BANCOW, `Year` < 2019)

#agregando por ano (total de expo e impo realizadas no ano)
BANCOW <- aggregate(BANCOW$`Trade Value (US$)`, by = list(BANCOW$Year), FUN = sum)

#renomeando as variaveis ap�s o agregate
BANCOW <- BANCOW %>% 
  rename(`world_trade` = "x",
         `year` = "Group.1") 

ggplot(data = BANCOW) + 
  geom_smooth(mapping = aes(x = year, y = world_trade), se=FALSE )    

######################
###### Agregando a regi�o toda por ano

reg_chi_by_year <- aggregate(BANCO$`Trade Value (US$)`, by = list(BANCO$Year), FUN = sum) #comercio regi�o-china por ano

reg_reg_by_year <- aggregate(BANCO2$`Trade Value (US$)`, by = list(BANCO2$Year), FUN = sum) #comercio regi�o-regiao por ano

reg_chi_by_year <- reg_chi_by_year %>%  #renomeando ap�s o aggregate 
  rename(`Reg-Chi` = "x") 

reg_reg_by_year <- reg_reg_by_year %>%  #renomeando ap�s o aggregate
  rename(`Reg-Reg` = "x") 

data <- merge(reg_chi_by_year, reg_reg_by_year, by=c("Group.1")) #juntanto os dois ultimos bancos num banco para as an�lises

cor.test(data$`Reg-Chi`, data$`Reg-Reg`)   #essa rela��o � positiva pq faz sentido: ao longo dos anos vc tem mais com�rcio ent�o o com�rcio regional sobe e o com a china tambem
plot(data$`Reg-Chi`, data$`Reg-Reg`)

### Agora, adicionaremos a variavel de total de com�rcio no ano (BANCO5)

datatotal <- aggregate(BANCO5$Total, by = list(BANCO5$Year), FUN = sum) #agregando o total por ano (datatotal)
data_prop <- merge(data, datatotal, by=c("Group.1"))  #criando um banco com o data + o datatotal
data_prop$propchi <- data_prop$`Reg-Chi`/data_prop$x  #criando a variavel com a propor��o de comercio pra china
data_prop$propreg <- data_prop$`Reg-Reg`/data_prop$x  #criando a variavel com a propor��o de com�rcio para regi�o

cor.test(data_prop$propchi, data_prop$propreg)   #teste de correla��o, -0.89, significativo

#Grafico 3, de correla��o entre as propor��es
plot(data_prop$propchi, data_prop$propreg, 
     xlab = "Propor��o de com�rcio com a China", ylab="Propor��o de com�rcio intrarregional",
     pch=20)

# Gr�fico 2,queda da propor��o do com�rcio da regi�o realizado com a pr�pria regi�o
ggplot(data = data_prop) + 
  geom_smooth(mapping = aes(x = Group.1, y = propreg), se= FALSE) + xlab ("Ano") +
  ylab("Propor��o de com�rcio intrarregional")     #evolu��o ao longo do tempo do peso do regional no com�rcio total da regi�o

write.csv(data_prop, file = "AggregateTradeRegionChinaandWorld", fileEncoding = "WINDOWS-1252")

#################
##### Banco para regress�o - ser� com os n�meros totais, e n�o a propor��o

datachi <- aggregate(BANCO$`Trade Value (US$)`, by = list(BANCO$Reporter, BANCO$Year), FUN = sum) #agregando por pais e por ano o com�rcio com a china

datareg <- aggregate(BANCO2$`Trade Value (US$)`, by = list(BANCO2$Reporter, BANCO2$Year), FUN = sum) #agregando por pais e por ano o comercio com a regi�o

datachi <- datachi %>%                     #renomeando as variaveis 
  rename(`china_trade` = "x",
         `country` = "Group.1",
         `year` = "Group.2") 

datareg <- datareg %>%                    #renomeando as variaveis 
  rename(`region_trade` = "x",
         `country` = "Group.1",
         `year` = "Group.2") 

dataset <- merge(datachi, datareg, by=c("country", "year"))   #juntando em um s� banco o com�rcio com a china e com a regi�o

dataset <- merge(dataset, BANCOW, by=c("year"))       #juntando agora o total do com�rcio do mundo por ano (variavel de controle)

plot(dataset$china_trade, dataset$region_trade) ## a rela��o entre as vari�veis � linear, ent�o podemos fazer uma regress�o
cor.test(dataset$china_trade, dataset$region_trade)

### Criando as vari�veis em log que ser�o utilizadas na regress�o

dataset$china_trade_log <- log(dataset$china_trade) 
dataset$region_trade_log <- log(dataset$region_trade)
dataset$world_trade_log <- log(dataset$world_trade)

write.csv(dataset, file = "TradePerCountryToCountryandToChina", fileEncoding = "WINDOWS-1252")

## Criando os modelos da regress�o 
model1 <- lm(dataset$region_trade_log ~ dataset$china_trade_log)    # somente VD ~ VI
summary(model1)

model2 <- lm(dataset$region_trade_log ~ dataset$china_trade_log + factor(dataset$year) - 1)  #efeitos fixos por ano
summary(model2)

model3 <- lm(dataset$region_trade_log ~ dataset$china_trade_log + factor(dataset$country) - 1)  #efeitos fixos por pa�s
summary(model3)

model4 <- lm(dataset$region_trade_log ~ dataset$china_trade_log + dataset$world_trade_log)  #com controle 
summary(model4)

model5 <- lm(dataset$region_trade_log ~ dataset$china_trade_log + dataset$world_trade_log + factor(dataset$year) - 1)  #com controle e efeitos fixos por ano
summary(model5)

model6 <- lm(dataset$region_trade_log ~ dataset$china_trade_log + dataset$world_trade_log + factor(dataset$country) - 1) #com controle e efeitos fixos por pa�s
summary(model6)

####Salvando a tabela da regress�o e exportando
library(stargazer)
setwd("./DragaoNaSala/Output")   
stargazer(model1, model3, model4, model6, type = "html",
          add.lines = list(c("Fixed effects?", "No", "Yes", "No", "Yes")),
          out = "reg.doc", title = "Efeito do Com�rcio com a China no Com�rcio Regional") 

#Tabelas do Ap�ndice

#A1
stargazer(model2, type = "html",
          out = "reg.doc", title = "Efeito do Com�rcio com a China no Com�rcio Regional") 
#A2
stargazer(model5, type = "html",
          out = "reg.doc", title = "Efeito do Com�rcio com a China no Com�rcio Regional") 

## Os histogramas das vari�veis utilizadas na regress�o, distribui��es normais (AP�NDICE)

histograma <- ggplot(data = dataset) +
  geom_histogram(mapping = aes(x = china_trade_log), binwidth = 0.5)+
  labs(x = "Distribui��o da vari�vel china_trade", y = " ")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12))
print(histograma)

histograma <- ggplot(data = dataset) +
  geom_histogram(mapping = aes(x = region_trade_log), binwidth = 0.5)+
  labs(x = "Distribui��o da vari�vel region_trade", y = " ")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12))
print(histograma)


 