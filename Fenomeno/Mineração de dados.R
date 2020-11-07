rm(list=ls())
if(!require(pacman)) install.packages("pacman")
if(!require(rstudioapi)) install.packages("rstudioapi"); require(rstudioapi)
if(!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
if(!require(readxl)) install.packages("readxl"); require(readxl)
library(hrbrthemes)
library(GGally)
library(viridis)
#install.packages("hrbrthemes")
hrbrthemes::import_roboto_condensed()

pacman::p_load(tidyverse, cowplot, wesanderson)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

dados1 <- read_excel("Music.xlsx", sheet = 1)
dados2 <- read_excel("Music.xlsx", sheet = 2)#com NA

#APAGAR# relatorio#
rela<-dados1 %>% 
  select(Energia:Modo,Playlist_name)
summary(rela)
#APAGar# relatorio#
dados_teste <- dados1 %>% 
  select(Energia:Positividade, Playlist_name) %>% 
  group_by(Playlist_name) %>% 
  summarise(across(everything(), 
                   list(media = mean, sd = sd, max = max,
                        min = min, mediana = median)))


View(dados_teste)
dadosmm<- dados_teste %>% 
  select(Playlist_name,contains(c("media","mediana")))
View(dadosmm)

dadosmedia<- dados_teste %>% 
  select(Playlist_name,contains(c("media")))



# Data set is provided by R nativel

dados1$Playlist_name<-as.factor(dados1$Playlist_name)


ggparcoord(dados1,
           columns =1:11, groupColumn = 16,
           showPoints = TRUE, 
           title = "Parallel Coordinate Plot for the Iris Data",
           alphaLines = 0.3) + 
  scale_color_viridis(discrete=TRUE)






# Plot
ggparcoord(data,
           columns = 1:4, groupColumn = 5 ,order = "anyClass",
           showPoints = TRUE, 
           title = "Parallel Coordinate Plot for the Iris Data",
           alphaLines = 0.3) + 
  scale_color_viridis(discrete=TRUE)

glimpse(data)

dadoszum<-dados1 %>% 
  select(c(Playlist_name,Energia,Ao_vivo,Falado,Org?nico,Instrumental,Dan?abilidade,For?a,Positividade))

dadoszumm <- dadoszum %>% 
  select(2:9, Playlist_name) %>% 
  group_by(Playlist_name) %>% 
  summarise(across(everything(), 
                   list(media = mean)))
View(dadoszumm)

dadoszumm$Playlist_name<-as.factor(dadoszumm$Playlist_name)

ggparcoord(dadoszum,
           columns = 2:9, groupColumn = 1 ,order = "anyClass",scale="mean",
           showPoints = TRUE, 
           title = "Parallel Coordinate Plot for the Iris Data",
           alphaLines = 1) + 
  scale_color_viridis(discrete=TRUE)+
  theme_bw()

###BIBLIOTECAS PARA GRAFICOS#####
devtools::install_github("davidsjoberg/ggbump")
library(ggplot2)
library(ggbump)
###################################

##BANCO DE DADOS#################
dados_tesmed <- dados1 %>% 
  select(Energia:Instrumental, Playlist_name) %>% 
  group_by(Playlist_name) %>% 
  summarise(across(everything(), 
                   list(media = mean))) %>% 
  pivot_longer(!Playlist_name, names_to = "função", values_to = "valor")
##############################3

dados_tesmed<-dados_tesmed %>% 
  group_by(função) %>% 
  mutate(rank= rank(valor,ties.method = "random")) %>% 
  ungroup()



dados_tesmed <- dados_tesmed %>% 
  mutate(função = factor(função, levels = unique(função)))


levels(dados_tesmed$função)

ggplot(dados_tesmed, aes(x=função, y=rank, color = Playlist_name)) +
  geom_point()+
  geom_line(aes(group=Playlist_name),size=1.5)+
  scale_y_discrete(breaks=seq(1,12,1))


df <- tibble(country = c("India", "Finland","India","Finland"),
             year = c(2010,2010,2011,2011),
             value = c(100,200,200,100))


df <- df %>% 
  group_by(year) %>% 
  mutate(rank = rank(value, ties.method = "random")) %>% 
  ungroup()

df <- df %>% 
  mutate(year = factor(year, levels = unique(year)))


ggplot(df, aes(year, rank, color = country)) +
  geom_bump(smooth = 8,direction ="x" )

df <- tibble(season = c("Spring", "Pre-season", "Summer", "Season finale", "Autumn", "Winter", 
                        "Spring", "Pre-season", "Summer", "Season finale", "Autumn", "Winter", 
                        "Spring", "Pre-season", "Summer", "Season finale", "Autumn", "Winter",
                        "Spring", "Pre-season", "Summer", "Season finale", "Autumn", "Winter"),
             rank = c(1, 3, 4, 2, 1, 4,
                      2, 4, 1, 3, 2, 3,
                      4, 1, 2, 4, 4, 1,
                      3, 2, 3, 1, 3, 2),
             player = c(rep("David", 6),
                        rep("Anna", 6),
                        rep("Franz", 6),
                        rep("Ika", 6)))

View(df)
df <- df %>% 
  mutate(season = factor(season, levels = unique(season)))

levels()
ggplot(df, aes(season, rank, color = player)) +
  geom_bump(size = 2, smooth = 20, show.legend = F) +
  geom_point(size = 5, aes(shape = player))

#############
df <- tibble(season = c("Spring", "Pre-season", "Summer", "Season finale", "Autumn", "Winter", 
                        "Spring", "Pre-season", "Summer", "Season finale", "Autumn", "Winter", 
                        "Spring", "Pre-season", "Summer", "Season finale", "Autumn", "Winter",
                        "Spring", "Pre-season", "Summer", "Season finale", "Autumn", "Winter"),
             rank = c(1, 3, 4, 2, 1, 4,
                      2, 4, 1, 3, 2, 3,
                      4, 1, 2, 4, 4, 1,
                      3, 2, 3, 1, 3, 2),
             player = c(rep("David", 6),
                        rep("Anna", 6),
                        rep("Franz", 6),
                        rep("Ika", 6)))

# Create factors and order factor
df <- df %>% 
  mutate(season = factor(season, levels = unique(season)))

# Add manual axis labels to plot
ggplot(df, aes(season, rank, color = player)) +
  geom_bump(size = 2, smooth = 20, show.legend = F) +
  geom_point(size = 5, aes(shape = player)) +
  theme_minimal_grid(font_size = 10, line_size = 0) +
  theme(panel.grid.major = element_blank(),
        axis.ticks = element_blank()) +
  scale_color_manual(values = wes_palette(n = 4, name = "IsleofDogs1"))
################################
####Padronização dos dados######
library(scales)
library(xlsx)
dfva<-dados1 %>% 
  select(Energia:Modo)
dfoutra<-dados1 %>% 
  select(Modo:Music_name)

dfnorm<-data.frame(sentimento=dados1$Playlist_name,apply(dfva,2,rescale))
#dfnormdata<-data.frame(apply(dfva,2,rescale),dfoutra)
#write_excel_csv(dfnormdata,'dfnormdata.xlsx')

###############################
##Calculando a Média###########
#dfnm  <- dfnorm %>% 
#  select(Energia:Positividade, sentimento) %>% 
#  group_by(sentimento) %>% 
#  summarise(across(everything(), 
#                   list(media = mean))) %>% 
#  pivot_longer(!sentimento, names_to = "função", values_to = "valor")
#
#ggplot(dfnm,aes(x=função,y=valor,group=sentimento))+
#  geom_line(aes(color=sentimento))
##############################################
##Grafico com todas variaveis MEDIA######
dfnm_novo  <- dfnorm %>% 
  select(Energia:Modo, sentimento) %>% 
  group_by(sentimento) %>% 
  summarise(across(everything(), 
                   list(media = mean)))
names(dfnm_novo)<-c("sentimento","Energia","Ao_vivo","Tempo"
                    ,"Falado","Orgânico","Instrumental",
                    "Compasso" ,"Dançabilidade","Tom",
                    "Duração","Força","Positividade","Modo")
dfnm_novo<-dfnm_novo %>%
  pivot_longer(!sentimento, names_to = "funcao", values_to = "valor")
  
ggplot(dfnm_novo,aes(x=funcao,y=valor,group=sentimento))+
  geom_line(aes(color=sentimento),size=1.3)+
  geom_point(aes(col=sentimento))+
  ggtitle("Dados Normalizados e calculado a média")+
  xlab("Variavel")+
  ylab("Média do Valor Normalizado")+
  scale_y_continuous(breaks = seq(0,1,0.1))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, vjust =1 , hjust=1))
########################################################
#######Grafico com todas as variaveis MEDIANA##########
dfnmediana_novo  <- dfnorm %>% 
  select(Energia:Modo, sentimento) %>% 
  group_by(sentimento) %>% 
  summarise(across(everything(), 
                   list(media = median)))
names(dfnmediana_novo)<-c("sentimento","Energia","Ao_vivo","Tempo"
                    ,"Falado","Orgânico","Instrumental",
                    "Compasso" ,"Dançabilidade","Tom",
                    "Duração","Força","Positividade","Modo")
dfnmediana_novo<-dfnmediana_novo %>%
  pivot_longer(!sentimento, names_to = "função", values_to = "valor")

ggplot(dfnmediana_novo,aes(x=função,y=valor,group=sentimento))+
  geom_line(aes(color=sentimento),size=1.3)+
  geom_point(aes(col=sentimento))+
  ggtitle("Dados Normalizados e calculado a mediana")+
  xlab("Variavel")+
  ylab("Mediana Normalizada")+
  scale_y_continuous(breaks = seq(0,1,0.1))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, vjust =1 , hjust=1))
##########################################################
##Usando Rank  com todas as variaveis#######

dfnm_ranks<-dfnm_novo %>% 
  group_by(funcao) %>% 
  mutate(rank= rank(valor,ties.method = "random")) %>% 
  ungroup()

ggplot(dfnm_ranks,aes(x=funcao,y=rank,group=sentimento))+
  geom_line(aes(color=sentimento),size=1.3)+
  geom_point(aes(col=sentimento))+
  ggtitle("Dados Normalizados e Rankeados")+
  xlab("Variavel")+
  ylab("Ranks")+
  scale_y_continuous(breaks = seq(1,12,1))+
  theme(axis.text.x = element_text(angle = 60, vjust =1 , hjust=1))

#########################################################
##Retirando Variavêis Que nao tem tanta diferença#######
###Retirei:
#Duração
#Compasso
#Falado
#Tom
#Tempo
#Ao_vivo
##########################################################
names(dfva)
remover<- names(dfva) %in% c("Duração","Compasso","Falado","Tom","Ao_vivo")
dfva_sem<-dfva[!remover]
dfnorm_sem<-data.frame(sentimento=dados1$Playlist_name, apply(dfva_sem,2,rescale))
#########################################3
##FAZENDO GRAFICOS NOVA VARIAVEL##########3
dfnm_sem  <- dfnorm_sem %>% 
  select(Energia:Positividade, sentimento) %>% 
  group_by(sentimento) %>% 
  summarise(across(everything(), 
                   list(media = mean)))
names(dfnm_sem)<-c("sentimento","Energia","Tempo"
                   ,"Orgânico","Instrumental" ,"Dançabilidade",
                   "Força","Positividade")
dfnm_sem<-dfnm_sem %>%
  pivot_longer(!sentimento, names_to = "função", values_to = "valor")

ggplot(dfnm_sem,aes(x=função,y=valor,group=sentimento))+
  geom_line(aes(color=sentimento),size=1.3)+
  geom_point(aes(col=sentimento))+
  ggtitle("Dados Normalizados e calculado a média Sem algumas variaveis")+
  xlab("Variavel")+
  ylab("Média Normalizada")+
  scale_y_continuous(breaks = seq(0,1,0.1))+
  theme(axis.text.x = element_text(angle = 60, vjust =1 , hjust=1))
####################################################################
##Fazendo os graficos sem algumas variaveis com a mediana##########3
dfnmediana_sem  <- dfnorm_sem %>% 
  select(Energia:Positividade, sentimento) %>% 
  group_by(sentimento) %>% 
  summarise(across(everything(), 
                   list(media = median)))
names(dfnmediana_sem)<-c("sentimento","Energia","Tempo"
                   ,"Orgânico","Instrumental" ,"Dançabilidade",
                   "Força","Positividade")
dfnmediana_sem<-dfnmediana_sem %>%
  pivot_longer(!sentimento, names_to = "função", values_to = "valor")

ggplot(dfnmediana_sem,aes(x=função,y=valor,group=sentimento))+
  geom_line(aes(color=sentimento),size=1.3)+
  geom_point(aes(col=sentimento))+
  ggtitle("Dados Normalizados e calculado a mediana Sem algumas variaveis")+
  xlab("Variavel")+
  ylab("Mediana Normalizada")+
  scale_y_continuous(breaks = seq(0,1,0.1))+
  theme(axis.text.x = element_text(angle = 60, vjust =1 , hjust=1))
############################################
##USando os ranks sem algumas variaveis#####
dfnm_ranks_sem<-dfnm_sem %>% 
  group_by(função) %>% 
  mutate(rank= rank(valor,ties.method = "random")) %>% 
  ungroup()

ggplot(dfnm_ranks_sem,aes(x=função,y=rank,group=sentimento))+
  geom_line(aes(color=sentimento),size=1.3)+
  geom_point(aes(col=sentimento))+
  ggtitle("Dados Normalizados Rankeados sem algumas variáveis")+
  xlab("Variavel")+
  ylab("Ranks")+
  scale_y_continuous(breaks = seq(1,12,1))+
  theme(axis.text.x = element_text(angle = 60, vjust =1 , hjust=1))
#########################################################################
######################FUNÇÂO BRABA DOS GRAFICO###########################
require(ggplot2) 
require(dplyr)
require(tidyr)

gatherpairs <- function(data, ..., 
                        xkey = '.xkey', xvalue = '.xvalue',
                        ykey = '.ykey', yvalue = '.yvalue',
                        na.rm = FALSE, convert = FALSE, factor_key = FALSE) {
  vars <- quos(...)
  xkey <- enquo(xkey)
  xvalue <- enquo(xvalue)
  ykey <- enquo(ykey)
  yvalue <- enquo(yvalue)
  
  data %>% {
    cbind(gather(., key = !!xkey, value = !!xvalue, !!!vars,
                 na.rm = na.rm, convert = convert, factor_key = factor_key),
          select(., !!!vars)) 
  } %>% gather(., key = !!ykey, value = !!yvalue, !!!vars,
               na.rm = na.rm, convert = convert, factor_key = factor_key)
}



dfnm_sem_ %>% 
  gatherpairs(all_of(vetor)) %>% {
    ggplot(., aes(x = .xvalue, y = .yvalue, color = Playlist_name)) +
      geom_point(alpha=0.3) + 
      facet_wrap(.xkey ~ .ykey, ncol = length(unique(.$.ykey)), scales = 'free', labeller = label_both)
  }

#########################################################################################
## 3D significa 3 vezes Demais
install.packages("plotly")
library(plotly)
devtools::install_github("AckerDWM/gg3D")
library(gg3D)

plot_ly(dados1,x=dados1$Energia, y=dados1$Dançabilidade, z=dados1$Positividade, type="scatter3d", mode="markers", color=dados1$Playlist_name,size=0.3) %>% 
  add_markers() %>%
  layout(
    scene = list(xaxis = list(title = 'Energia'),
                 yaxis = list(title = 'Dançabilidade'),
                 zaxis = list(title = 'Positividade'))
  )

ggplot(dados1, aes(x=Energia, y=Dançabilidade, z=Positividade, color=Playlist_name)) + 
  theme_void() +
  axes_3D() +
  stat_3D()
#######################################################
##PARTE 3#############################################
remotes::install_github("tylermorganwall/rayshader")
library(rayshader)
library(ggplot2)
library(tidyverse)

gg = ggplot(dados1, aes(Energia, Dançabilidade)) +
  stat_density_2d(aes(fill = stat(nlevel)), 
                  geom = "polygon",
                  n = 100,bins = 10,contour = TRUE) +
  facet_wrap(Playlist_name~.) +
  scale_fill_viridis_c(option = "A")
plot_gg(gg,multicore=TRUE,width=5,height=5,scale=250)
#######################################################
###########GRAFICOS 3 D FODA############################
library(corrplot)
corrplot(cor(dfva), method = "circle",order="hclust")

plot_ly(x=dados1$Energia, y=dados1$Força, z=dados1$Orgânico, type="scatter3d", mode="markers", color=dados1$Playlist_name,size=0.3) %>% 
  add_markers() %>%
  layout(
    scene = list(xaxis = list(title = 'Energia'),
                 yaxis = list(title = 'Force'),
                 zaxis = list(title = 'Organico'))
  )
##############################################################
###############Teste de médias################################
##############################################################
#Retiramos algumas variaveis que nao ajudam no teste de medias
#Ao_vivo
#Compasso
#Duração
#Falado
#Tom
##############################################################
library(Hmisc)

unique(dados11$funcao)

# dplyr::filter(funcao != "Ao_vivo" & "Compasso" & "Duração"&"Falado"&"Tom") %>% 
#dplyr::filter(dados11,funcao %in% c("Ao_vivo","Tempo"))
 # dplyr::filter(funcao %in% c("Energia","Compasso","Orgânico","Instrumental",
 #                             "Dançabilidade","Tom","Força","Positividade")) %>%  
  
dados11 <- dfnm_novo %>% 
  dplyr :: filter(funcao %in% c("Energia","Orgânico","Instrumental",
                              "Dançabilidade","Tempo","Força","Positividade")) %>% 
  mutate(auxiliar = case_when(sentimento=="Sleepy" ~ "Sleepy", sentimento=="Peaceful" ~ "Peaceful",sentimento=="Relaxed"~"Relaxed",sentimento=="Calm"~"Calm",TRUE ~ "Outros"))

dados11mediana <- dfnmediana_novo %>% 
  dplyr :: filter(função %in% c("Energia","Orgânico","Instrumental",
                                "Dançabilidade","Tempo","Força","Positividade")) %>% 
  mutate(auxiliar = case_when(sentimento=="Sleepy" ~ "Sleepy", sentimento=="Peaceful" ~ "Peaceful",sentimento=="Relaxed"~"Relaxed",sentimento=="Calm"~"Calm",sentimento=="Sad"~"Sad",TRUE ~ "Outros"))

################SEPARAÇÂO########################################################
######Banco de dados com 4 grupos banco completo
df4grupo <- dados1 %>% 
  dplyr :: select(c("Energia","Orgânico","Instrumental",
                                "Dançabilidade","Tempo","Força","Positividade","Playlist_name")) %>% 
  mutate( auxiliar = case_when(Playlist_name == "Sleepy" ~ "Calmo", 
                              Playlist_name == "Peaceful" ~ "Calmo",
                              Playlist_name == "Relaxed"~"Calmo",
                              Playlist_name == "Calm" ~ "Calmo",
                              Playlist_name == "Sad" ~ "Sad",
                              Playlist_name == "Angry" ~ "Angry",
                              TRUE ~ "Medios"))


#################################################################################
#Banco de dados com 4 grupos apenas com as medias
df4grupo_media <- dfnm_novo %>% 
  dplyr :: filter(funcao %in% c("Energia","Orgânico","Instrumental",
                                "Dançabilidade","Tempo","Força","Positividade")) %>% 
  mutate( auxiliar = case_when(sentimento == "Sleepy" ~ "Calmo", 
                               sentimento == "Peaceful" ~ "Calmo",
                               sentimento == "Relaxed"~"Calmo",
                               sentimento == "Calm" ~ "Calmo",
                               sentimento == "Sad" ~ "Sad",
                               sentimento == "Angry" ~ "Angry",
                               TRUE ~ "Medios"))

#Banco de dados com 5 grupos apenas com as medias
df5grupo_media <- dfnm_novo %>% 
  dplyr :: filter(funcao %in% c("Energia","Orgânico","Instrumental",
                                "Dançabilidade","Tempo","Força","Positividade")) %>% 
  mutate( auxiliar = case_when(sentimento == "Sleepy" ~ "Sleepy", 
                               sentimento == "Peaceful" ~ "Sleepy",
                               sentimento == "Relaxed"~"Calmo",
                               sentimento == "Calm" ~ "Calmo",
                               sentimento == "Sad" ~ "Sad",
                               sentimento == "Angry" ~ "Angry",
                               TRUE ~ "Medios"))

#Banco de dados com 6 grupos apenas com as medias
df6grupo_media <- dfnm_novo %>% 
  dplyr :: filter(funcao %in% c("Energia","Orgânico","Instrumental",
                                "Dançabilidade","Tempo","Força","Positividade")) %>% 
  mutate( auxiliar = case_when(sentimento == "Sleepy" ~ "Sleepy", 
                               sentimento == "Peaceful" ~ "Sleepy",
                               sentimento == "Nervous" ~"Nervous",
                               sentimento == "Boring" ~"Nervous" ,
                               sentimento == "Relaxed"~"Calmo",
                               sentimento == "Calm" ~ "Calmo",
                               sentimento == "Sad" ~ "Sad",
                               sentimento == "Angry" ~ "Angry",
                               TRUE ~ "Medios"))


#Banco de dados com 7 grupos apenas com as medias
df7grupo_media <- dfnm_novo %>% 
  dplyr :: filter(funcao %in% c("Energia","Orgânico","Instrumental",
                                "Dançabilidade","Tempo","Força","Positividade")) %>% 
  mutate( auxiliar = case_when(sentimento == "Sleepy" ~ "Sleepy", 
                               sentimento == "Peaceful" ~ "Sleepy",
                               sentimento == "Nervous" ~"Nervous",
                               sentimento == "Boring" ~"Nervous" ,
                               sentimento == "Relaxed"~"Calmo",
                               sentimento == "Annoying" ~"Chato",
                               sentimento == "Calm" ~ "Calmo",
                               sentimento == "Sad" ~ "Sad",
                               sentimento == "Angry" ~ "Angry",
                               TRUE ~ "Medios"))





#table(dfnm_novo$funcao)
#table(dados11$auxiliar)



ggplot(df4grupo_media,aes(x=funcao,y=valor,group=sentimento))+
  geom_line(aes(color=auxiliar),size=1.3)+
  scale_color_manual(values=c("orange","blue",'green',"red","magenta","yellow","purple"))+
  geom_point(aes(col=auxiliar))+
  ggtitle("Dados Normalizados e calculado a média")+
  xlab("Variavel")+
  ylab("Média do Valor Normalizado")+
  scale_y_continuous(breaks = seq(0,1,0.1))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, vjust =1 , hjust=1))

ggplot(df5grupo_media,aes(x=funcao,y=valor,group=sentimento))+
  geom_line(aes(color=auxiliar),size=1.3)+
  scale_color_manual(values=c("orange","blue",'green',"red","magenta","yellow","purple"))+
  geom_point(aes(col=auxiliar))+
  ggtitle("Dados Normalizados e calculado a média")+
  xlab("Variavel")+
  ylab("Média do Valor Normalizado")+
  scale_y_continuous(breaks = seq(0,1,0.1))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, vjust =1 , hjust=1))

ggplot(df6grupo_media,aes(x=funcao,y=valor,group=sentimento))+
  geom_line(aes(color=auxiliar),size=1.3)+
  scale_color_manual(values=c("orange","blue",'green',"red","magenta","yellow","purple"))+
  geom_point(aes(col=auxiliar))+
  ggtitle("Dados Normalizados e calculado a média")+
  xlab("Variavel")+
  ylab("Média do Valor Normalizado")+
  scale_y_continuous(breaks = seq(0,1,0.1))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, vjust =1 , hjust=1))

ggplot(df7grupo_media,aes(x=funcao,y=valor,group=sentimento))+
  geom_line(aes(color=auxiliar),size=1.3)+
  scale_color_manual(values=c("orange","blue",'green',"red","magenta","yellow","purple"))+
  geom_point(aes(col=auxiliar))+
  ggtitle("Dados Normalizados e calculado a média")+
  xlab("Variavel")+
  ylab("Média do Valor Normalizado")+
  scale_y_continuous(breaks = seq(0,1,0.1))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, vjust =1 , hjust=1))

dados11mediana %>% filter(função=="Energia")

table(dados11mediana$auxiliar)



##
install.packages("randomForest")
library(randomForest)
dados11pG$auxiliar<-factor(dados11pG$auxiliar)
#bloco de cóigo do modelo ############################
music_model <- randomForest::randomForest(auxiliar ~ . , data = dados11pG)
print(music_model)
print(importance(music_model,type = 2)) 

err <- music_model$err.rate
head(err)
oob_err <- err[nrow(music_model$err),"OOB"]
print(oob_err)

plot(music_model)
legend(x = "right", legend = colnames(err),
       fill = 1:ncol(err))
#######################################################
library(ROSE)
table(dados11pG$auxiliar)
class(dados11pG$auxiliar)

dadosprop<-  dados11pG %>% 
    dplyr::select(auxiliar,Playlist_name) %>%
    group_by(auxiliar,Playlist_name) %>%
    summarise(soma=n()) %>% 
    ungroup(Playlist_name) %>% 
    summarise(soma2=soma,prop=soma/sum(soma2))
#########################################################
#INTRODUÇÃO
#transformação dos dados-------------------------linhas 624 até 631
#amostragem simples com 4 grupos ----------------linha 636 até 683
#amostragem simples com 5 grupos ----------------linhas 684 até 734
#amostragem simples com 6 grupos ----------------linhas 735 até 791
#amostragem simples com 7 grupos ----------------linhas 792 até 854
#amostragem estratificada -----------------------linhas 855
#amostragem estratificada com 4 grupos ----------linhas 859 até 987
#amostragem estratificada com 5 grupos ----------linhas 988 até 1135
#amostragem estratificada com 6 grupos ----------linhas 1136 até 1297
#amostragem estratifciada com 7 grupos ----------linhas 1298 até 1465
#1 Pegar o grupo que tem menor quantidade e fazer uma amostragem simples para cada um com
#O valor minimo, com todas as separalções depois rodar o random forest

#2 Pegar os grupos e fazer uma amostragem de porcentagem dentre os grupos e 
#depois aplicar o random forest

#Primeiro vamos normalizar os dados
library(scales)
dfva<-dados1 %>% 
  select(Energia:Positividade)
dfvacomplementar<- dados1 %>% 
  select(!Energia:Positividade)

df_completo_normalizado<-data.frame(apply(dfva,2,rescale),dfvacomplementar)
write.csv2(df_completo_normalizado,"dados1_normalizados.csv")
#Agora ja temos os dados normalizados agora iremos separar os grupos
###########################################################################
#Separando esse banco de dados em alguns com uma menos "Sentimentos"

##Data frame com 4 sentimentos ########################33333
df4grupo <- df_completo_normalizado %>% 
  mutate( auxiliar = case_when(Playlist_name == "Sleepy" ~ "Calmo", 
                               Playlist_name == "Peaceful" ~ "Calmo",
                               Playlist_name == "Relaxed"~"Calmo",
                               Playlist_name == "Calm" ~ "Calmo",
                               Playlist_name == "Sad" ~ "Sad",
                               Playlist_name == "Angry" ~ "Angry",
                               TRUE ~ "Medios"))
#Ver qual classe tem menor quantidade
table(df4grupo$auxiliar)
#Sad com 514, então faremos 3 amostragem simples com o valor de 514.

angry4.simples<-df4grupo %>% 
  filter(auxiliar=="Angry") %>% 
  sample_n(514,replace = F)
  
calmo4.simples<-df4grupo %>% 
  filter(auxiliar=="Calmo") %>% 
  sample_n(514,replace = F)

medios4.simples<-df4grupo %>% 
  filter(auxiliar=="Medios") %>% 
  sample_n(514,replace = F)

sad4.simples<-df4grupo %>% 
  filter(auxiliar=="Sad")
#Sad é o minimo então é apenas o filter
###############################################################
#Agora vamos juntar tudo em um banco de dados e aplicar a random forest
df.4grupos.amostrado<-rbind(angry4.simples,calmo4.simples,medios4.simples,sad4.simples)

library(randomForest)
df.4grupos.amostrado$auxiliar<-factor(df.4grupos.amostrado$auxiliar)

music_model <- randomForest::randomForest(auxiliar ~ Energia:Positividade , data = df.4grupos.amostrado)
print(music_model)
print(importance(music_model,type = 2)) 

err <- music_model$err.rate
head(err)
oob_err <- err[nrow(music_model$err),"OOB"]
print(oob_err)

plot(music_model)
legend(x = "right", legend = colnames(err),
       fill = 1:ncol(err))
###########################################################################
###Data frame com 5 sentimentos ########################33333
df5grupo <- df_completo_normalizado %>% 
  mutate( auxiliar = case_when(Playlist_name == "Sleepy" ~ "Sleepy", 
                             Playlist_name == "Peaceful" ~ "Sleepy",
                             Playlist_name == "Relaxed"~"Calmo",
                             Playlist_name == "Calm" ~ "Calmo",
                             Playlist_name == "Sad" ~ "Sad",
                             Playlist_name == "Angry" ~ "Angry",
                             TRUE ~ "Medios"))
#Ver qual classe tem menor quantidade
table(df5grupo$auxiliar)
#Sad com 514, então faremos 4 amostragem simples com o valor de 514.
angry5.simples<-df5grupo %>% 
  filter(auxiliar=="Angry") %>% 
  sample_n(514,replace = F)

calmo5.simples<-df5grupo %>% 
  filter(auxiliar=="Calmo") %>% 
  sample_n(514,replace = F)

medios5.simples<-df5grupo %>% 
  filter(auxiliar=="Medios") %>% 
  sample_n(514,replace = F)

sleepy5.simples<-df5grupo %>% 
  filter(auxiliar=="Sleepy") %>% 
  sample_n(514,replace = F)

sad5.simples<-df5grupo %>% 
  filter(auxiliar=="Sad")
#Sad é o minimo então é apenas o filter
###############################################################
#Agora vamos juntar tudo em um banco de dados e aplicar a random forest
df.5grupos.amostrado<-rbind(angry5.simples,calmo5.simples,medios5.simples,sleepy5.simples,sad5.simples)

library(randomForest)
df.5grupos.amostrado$auxiliar<-factor(df.5grupos.amostrado$auxiliar)

music_model <- randomForest::randomForest(auxiliar ~ Energia:Positividade , data = df.5grupos.amostrado)
print(music_model)
print(importance(music_model,type = 2)) 

err <- music_model$err.rate
head(err)
oob_err <- err[nrow(music_model$err),"OOB"]
print(oob_err)

plot(music_model)
legend(x = "right", legend = colnames(err),
       fill = 1:ncol(err))
###########################################################################
###Data frame com 6 sentimentos ########################33333
df6grupo <- df_completo_normalizado %>% 
mutate( auxiliar = case_when(Playlist_name == "Sleepy" ~ "Sleepy", 
                             Playlist_name == "Peaceful" ~ "Sleepy",
                             Playlist_name == "Nervous" ~"Nervous",
                             Playlist_name == "Boring" ~"Nervous" ,
                             Playlist_name == "Relaxed"~"Calmo",
                             Playlist_name == "Calm" ~ "Calmo",
                             Playlist_name == "Sad" ~ "Sad",
                             Playlist_name == "Angry" ~ "Angry",
                             TRUE ~ "Medios"))
#Ver qual classe tem menor quantidade
table(df6grupo$auxiliar)
#Sad com 514, então faremos 4 amostragem simples com o valor de 514.
angry6.simples<-df6grupo %>% 
  filter(auxiliar=="Angry") %>% 
  sample_n(514,replace = F)

calmo6.simples<-df6grupo %>% 
  filter(auxiliar=="Calmo") %>% 
  sample_n(514,replace = F)

medios6.simples<-df6grupo %>% 
  filter(auxiliar=="Medios") %>% 
  sample_n(514,replace = F)

nervous6.simples<-df6grupo %>% 
  filter(auxiliar=="Nervous") %>% 
  sample_n(514,replace = F)

sleepy6.simples<-df6grupo %>% 
  filter(auxiliar=="Sleepy") %>% 
  sample_n(514,replace = F)

sad6.simples<-df6grupo %>% 
  filter(auxiliar=="Sad")
#Sad é o minimo então é apenas o filter
###############################################################
#Agora vamos juntar tudo em um banco de dados e aplicar a random forest
df.6grupos.amostrado<-rbind(angry6.simples,calmo6.simples,medios6.simples,nervous6.simples,sleepy6.simples,sad6.simples)

library(randomForest)
df.6grupos.amostrado$auxiliar<-factor(df.6grupos.amostrado$auxiliar)

music_model <- randomForest::randomForest(auxiliar ~ Energia:Positividade , data = df.6grupos.amostrado)
print(music_model)
print(importance(music_model,type = 2)) 

err <- music_model$err.rate
head(err)
oob_err <- err[nrow(music_model$err),"OOB"]
print(oob_err)

plot(music_model)
legend(x = "right", legend = colnames(err),
       fill = 1:ncol(err))
###########################################################################
###Data frame com 7 sentimentos #########################################
df7grupo <- df_completo_normalizado %>% 
mutate( auxiliar = case_when(Playlist_name == "Sleepy" ~ "Sleepy", 
                             Playlist_name == "Peaceful" ~ "Sleepy",
                             Playlist_name == "Nervous" ~"Nervous",
                             Playlist_name == "Boring" ~"Nervous" ,
                             Playlist_name == "Relaxed"~"Calmo",
                             Playlist_name == "Annoying" ~"Chato",
                             Playlist_name == "Calm" ~ "Calmo",
                             Playlist_name == "Sad" ~ "Sad",
                             Playlist_name == "Angry" ~ "Angry",
                             TRUE ~ "Medios"))
#Ver qual classe tem menor quantidade
table(df7grupo$auxiliar)
#Chato com 470, então faremos 4 amostragem simples com o valor de 470.
angry7.simples<-df7grupo %>% 
  filter(auxiliar=="Angry") %>% 
  sample_n(470,replace = F)

calmo7.simples<-df7grupo %>% 
  filter(auxiliar=="Calmo") %>% 
  sample_n(470,replace = F)

medios7.simples<-df7grupo %>% 
  filter(auxiliar=="Medios") %>% 
  sample_n(470,replace = F)

nervous7.simples<-df7grupo %>% 
  filter(auxiliar=="Nervous") %>% 
  sample_n(470,replace = F)

sleepy7.simples<-df7grupo %>% 
  filter(auxiliar=="Sleepy") %>% 
  sample_n(470,replace = F)

sad7.simples<-df7grupo %>% 
  filter(auxiliar=="Sad") %>% 
  sample_n(470,replace = F)

chato7.simples<-df7grupo %>% 
  filter(auxiliar=="Chato") 
#Chato é o minimo então é apenas o filter
###############################################################
#Agora vamos juntar tudo em um banco de dados e aplicar a random forest
df.7grupos.amostrado<-rbind(angry7.simples,chato7.simples,calmo7.simples,medios7.simples,nervous7.simples,sleepy7.simples,sad7.simples)

library(randomForest)
df.7grupos.amostrado$auxiliar<-factor(df.7grupos.amostrado$auxiliar)

music_model <- randomForest::randomForest(auxiliar ~ Energia:Positividade , data = df.7grupos.amostrado)
print(music_model)
print(importance(music_model,type = 2)) 

err <- music_model$err.rate
head(err)
oob_err <- err[nrow(music_model$err),"OOB"]
print(oob_err)

plot(music_model)
legend(x = "right", legend = colnames(err),
       fill = 1:ncol(err))

###################################################################################
#Terminei para todos os grupos Com amostragem simples agora iremos fazer porcentual
###################################################################################
#################################################3
#Iremos Fazer amostragem com probabilidade######
#Comecaremos por 4 Grupos######################

table(df4grupo$auxiliar)
n=514

df.angry<-df4grupo %>% 
  filter(auxiliar=="Angry")
table(df.angry$Playlist_name)
#portanto a amostragem é simples
angry4.estratificada<-df4grupo %>% 
  filter(auxiliar=="Angry") %>% 
  sample_n(514)

###################################
#Dados calm estratificado 
df.calmo<-df4grupo %>% 
  filter(auxiliar=="Calmo")
table(df.calmo$Playlist_name)
#portanto a amostragem é porcentagem então farei as porcentagem
n.calmo<-3189
#Calm
n.calmo.calm<-round(514*635/3189,0)
n.calmo.peaceful<-round(514*833/n.calmo,0)
n.calmo.relaxed<-round(n*840/n.calmo,0)
n.calmo.sleepy<-round(n*881/n.calmo,0)
sum(n.calmo.calm,n.calmo.peaceful,n.calmo.relaxed,n.calmo.sleepy)

#Calm do dados calmo estatificada para 4 grupos
calmo4.estratificada.calm<-df4grupo %>% 
  filter(auxiliar=="Calmo") %>% 
  filter(Playlist_name=="Calm") %>% 
  sample_n(n.calmo.calm)
#Peaceful dos dados calmo estraficado por 4 grupos
calmo4.estratificada.peaceful<-df4grupo %>% 
  filter(auxiliar=="Calmo") %>% 
  filter(Playlist_name=="Peaceful") %>% 
  sample_n(n.calmo.peaceful)
#Relaxed dos dados calmo estraficado por 4 grupos
calmo4.estratificada.relaxed<-df4grupo %>% 
  filter(auxiliar=="Calmo") %>% 
  filter(Playlist_name=="Relaxed") %>% 
  sample_n(n.calmo.relaxed)
#spleepy dos dados calmo estraficado por 4 grupos
calmo4.estratificada.sleepy<-df4grupo %>% 
  filter(auxiliar=="Calmo") %>% 
  filter(Playlist_name=="Sleepy") %>% 
  sample_n(n.calmo.sleepy)
#Juntando no banco de dados de Calmo

calmo4.estratificada<-rbind(calmo4.estratificada.calm,calmo4.estratificada.peaceful,calmo4.estratificada.relaxed,calmo4.estratificada.sleepy)
length(calmo4.estratificada$auxiliar)
##################
#Sad
df.sad<-df4grupo %>% 
  filter(auxiliar=="Sad")
table(df.sad$Playlist_name)
#portanto a amostragem é simples
sad4.estratificada<-df4grupo %>% 
  filter(auxiliar =="Sad")
#################################
#medios

df.medios<-df4grupo %>% 
  filter(auxiliar=="Medios")
table(df.medios$Playlist_name)
#portanto a amostragem é porcentagem então farei as porcentagem
n.medios<-length(df.medios$auxiliar)
#Medios
n.medios.annoying<-round(n*470/n.medios,0)
n.medios.boring<-round(n*555/n.medios,0)
n.medios.excited<-round(n*518/n.medios,0)
n.medios.happy<-round(n*609/n.medios,0)
n.medios.nervous<-round(n*634/n.medios,0)
n.medios.pleased<-round(n*678/n.medios,0)

sum(n.medios.annoying,n.medios.boring,n.medios.excited,n.medios.happy,n.medios.nervous,n.medios.pleased)
#Annoying do dados Medios estatificada para 4 grupos
medios4.estratificada.annoying<-df4grupo %>% 
  filter(auxiliar=="Medios") %>% 
  filter(Playlist_name=="Annoying") %>% 
  sample_n(n.medios.annoying)
#Boring dos dados calmo estraficado por 4 grupos
medios4.estratificada.boring<-df4grupo %>% 
  filter(auxiliar=="Medios") %>% 
  filter(Playlist_name=="Boring") %>% 
  sample_n(n.medios.boring)
#Excited dos dados medios estraficado por 4 grupos
medios4.estratificada.excited<-df4grupo %>% 
  filter(auxiliar=="Medios") %>% 
  filter(Playlist_name=="Excited") %>% 
  sample_n(n.medios.excited)
#Happy dos dados medios estraficado por 4 grupos
medios4.estratificada.happy<-df4grupo %>% 
  filter(auxiliar=="Medios") %>% 
  filter(Playlist_name=="Happy") %>% 
  sample_n(n.medios.happy)
#Nervous dos dados medios estraficado por 4 grupos
medios4.estratificada.nervous<-df4grupo %>% 
  filter(auxiliar=="Medios") %>% 
  filter(Playlist_name=="Nervous") %>% 
  sample_n(n.medios.nervous)
#Pleased dos dados medios estraficado por 4 grupos
medios4.estratificada.pleased<-df4grupo %>% 
  filter(auxiliar=="Medios") %>% 
  filter(Playlist_name=="Pleased") %>% 
  sample_n(n.medios.pleased)
#Juntando agora os banco de dados Medios
medios4.estratificada<-rbind(medios4.estratificada.annoying,medios4.estratificada.boring,medios4.estratificada.excited,
                             medios4.estratificada.happy,medios4.estratificada.nervous,medios4.estratificada.pleased)
################################################################
#Agora vamos juntar tudo em um banco de dados e aplicar a random forest
df.4grupos.amostrado.estratificado<-rbind(angry4.estratificada,calmo4.estratificada,sad4.estratificada,medios4.estratificada)

library(randomForest)
df.4grupos.amostrado.estratificado$auxiliar<-factor(df.4grupos.amostrado.estratificado$auxiliar)

music_model <- randomForest::randomForest(auxiliar ~ Energia:Positividade , data = df.4grupos.amostrado.estratificado)
print(music_model)
print(importance(music_model,type = 2)) 

err <- music_model$err.rate
head(err)
oob_err <- err[nrow(music_model$err),"OOB"]
print(oob_err)

plot(music_model)
legend(x = "right", legend = colnames(err),
       fill = 1:ncol(err))
#################################################################################################
#Agora farei para os grupos de 5

table(df5grupo$auxiliar)
n=514
####
#Angry

df.angry<-df5grupo %>% 
  filter(auxiliar=="Angry")
table(df.angry$Playlist_name)

#portanto a amostragem é simples
angry5.estratificada<-df5grupo %>% 
  filter(auxiliar=="Angry") %>% 
  sample_n(514)
#####
#Sad
df.sad<-df5grupo %>% 
  filter(auxiliar=="Sad")
table(df.sad$Playlist_name)
#portanto a amostragem é simples
sad5.estratificada<-df5grupo %>% 
  filter(auxiliar =="Sad")

#######
#Sleepy
df.sleepy<-df5grupo %>% 
  filter(auxiliar=="Sleepy")
table(df.sleepy$Playlist_name)
#portanto a amostragem é estratificada

n.sleepy<-length(df.sleepy$auxiliar)
n.sleepy.peacful<-round(n/n.sleepy*833,0)
n.sleepy.sleepy<-round(n/n.sleepy*881,0)
sum(n.sleepy.peacful,n.sleepy.sleepy)

#Peaceful dos dados sleepy com 5 grupos estratificado
sleepy5.estratificada.peaceful<-df5grupo %>% 
  filter(auxiliar=="Sleepy") %>% 
  filter(Playlist_name=="Peaceful") %>% 
  sample_n(n.sleepy.peacful)


#sleepy dos dados sleepy com 5 grupos estratificado
sleepy5.estratificada.sleepy<-df5grupo %>% 
  filter(auxiliar=="Sleepy") %>% 
  filter(Playlist_name=="Sleepy") %>% 
  sample_n(n.sleepy.sleepy)
#Juntando agora os banco de dados sleepy
sleepy5.estratificada<-rbind(sleepy5.estratificada.peaceful,sleepy5.estratificada.sleepy)

#######
#Calmo
df.calmo<-df5grupo %>% 
  filter(auxiliar=="Calmo")
table(df.calmo$Playlist_name)
#portanto a amostragem é estratificada
#portanto a amostragem é porcentagem então farei as porcentagem
n.calmo<-1475
#Calm
n.calmo.calm<-round(n*635/n.calmo,0)
n.calmo.relaxed<-round(n*840/n.calmo,0)
sum(n.calmo.calm,n.calmo.relaxed)

#Calm do dados calmo estatificada para 4 grupos
calmo5.estratificada.calm<-df5grupo %>% 
  filter(auxiliar=="Calmo") %>% 
  filter(Playlist_name=="Calm") %>% 
  sample_n(n.calmo.calm)

#Relaxed dos dados calmo estraficado por 4 grupos
calmo5.estratificada.relaxed<-df5grupo %>% 
  filter(auxiliar=="Calmo") %>% 
  filter(Playlist_name=="Relaxed") %>% 
  sample_n(n.calmo.relaxed)

#Juntando no banco de dados de Calmo
calmo5.estratificada<-rbind(calmo5.estratificada.calm,calmo5.estratificada.relaxed)
#####################
#medios

df.medios<-df5grupo %>% 
  filter(auxiliar=="Medios")

table(df.medios$Playlist_name)
#portanto a amostragem é porcentagem então farei as porcentagem
n.medios<-length(df.medios$auxiliar)
#Medios
n.medios.annoying<-round(n*470/n.medios,0)
n.medios.boring<-round(n*555/n.medios,0)
n.medios.excited<-round(n*518/n.medios,0)
n.medios.happy<-round(n*609/n.medios,0)
n.medios.nervous<-round(n*634/n.medios,0)
n.medios.pleased<-round(n*678/n.medios,0)

sum(n.medios.annoying,n.medios.boring,n.medios.excited,n.medios.happy,n.medios.nervous,n.medios.pleased)
#Annoying do dados Medios estatificada para 5 grupos
medios5.estratificada.annoying<-df5grupo %>% 
  filter(auxiliar=="Medios") %>% 
  filter(Playlist_name=="Annoying") %>% 
  sample_n(n.medios.annoying)
#Boring dos dados calmo estraficado por 5 grupos
medios5.estratificada.boring<-df5grupo %>% 
  filter(auxiliar=="Medios") %>% 
  filter(Playlist_name=="Boring") %>% 
  sample_n(n.medios.boring)
#Excited dos dados medios estraficado por 5 grupos
medios5.estratificada.excited<-df5grupo %>% 
  filter(auxiliar=="Medios") %>% 
  filter(Playlist_name=="Excited") %>% 
  sample_n(n.medios.excited)
#Happy dos dados medios estraficado por 5 grupos
medios5.estratificada.happy<-df5grupo %>% 
  filter(auxiliar=="Medios") %>% 
  filter(Playlist_name=="Happy") %>% 
  sample_n(n.medios.happy)
#Nervous dos dados medios estraficado por 5 grupos
medios5.estratificada.nervous<-df5grupo %>% 
  filter(auxiliar=="Medios") %>% 
  filter(Playlist_name=="Nervous") %>% 
  sample_n(n.medios.nervous)
#Pleased dos dados medios estraficado por 5 grupos
medios5.estratificada.pleased<-df5grupo %>% 
  filter(auxiliar=="Medios") %>% 
  filter(Playlist_name=="Pleased") %>% 
  sample_n(n.medios.pleased)
#Juntando agora os banco de dados Medios
medios5.estratificada<-rbind(medios5.estratificada.annoying,medios5.estratificada.boring,medios5.estratificada.excited,
                             medios5.estratificada.happy,medios5.estratificada.nervous,medios5.estratificada.pleased)
#Agora vamos juntar tudo em um banco de dados e aplicar a random forest
df.5grupos.amostrado.estratificado<-rbind(angry5.estratificada,calmo5.estratificada,sad5.estratificada,medios5.estratificada,sleepy5.estratificada)

library(randomForest)
df.5grupos.amostrado.estratificado$auxiliar<-factor(df.5grupos.amostrado.estratificado$auxiliar)

music_model <- randomForest::randomForest(auxiliar ~ Energia:Positividade , data = df.5grupos.amostrado.estratificado)
print(music_model)
print(importance(music_model,type = 2)) 

err <- music_model$err.rate
head(err)
oob_err <- err[nrow(music_model$err),"OOB"]
print(oob_err)

plot(music_model)
legend(x = "right", legend = colnames(err),
       fill = 1:ncol(err))
#########################################################################
#Agora para 6 grupos

table(df6grupo$auxiliar)
n=514
####
#Angry

df.angry<-df6grupo %>% 
  filter(auxiliar=="Angry")
table(df.angry$Playlist_name)

#portanto a amostragem é simples
angry6.estratificada<-df6grupo %>% 
  filter(auxiliar=="Angry") %>% 
  sample_n(514)
#####
#Sad
df.sad<-df6grupo %>% 
  filter(auxiliar=="Sad")
table(df.sad$Playlist_name)
#portanto a amostragem é simples
sad6.estratificada<-df5grupo %>% 
  filter(auxiliar =="Sad")

#######
#Sleepy
df.sleepy<-df6grupo %>% 
  filter(auxiliar=="Sleepy")
table(df.sleepy$Playlist_name)
#portanto a amostragem é estratificada

n.sleepy<-length(df.sleepy$auxiliar)
n.sleepy.peacful<-round(n/n.sleepy*833,0)
n.sleepy.sleepy<-round(n/n.sleepy*881,0)
sum(n.sleepy.peacful,n.sleepy.sleepy)

#Peaceful dos dados sleepy com 6 grupos estratificado
sleepy6.estratificada.peaceful<-df6grupo %>% 
  filter(auxiliar=="Sleepy") %>% 
  filter(Playlist_name=="Peaceful") %>% 
  sample_n(n.sleepy.peacful)


#sleepy dos dados sleepy com 6 grupos estratificado
sleepy6.estratificada.sleepy<-df6grupo %>% 
  filter(auxiliar=="Sleepy") %>% 
  filter(Playlist_name=="Sleepy") %>% 
  sample_n(n.sleepy.sleepy)
#Juntando agora os banco de dados sleepy
sleepy6.estratificada<-rbind(sleepy6.estratificada.peaceful,sleepy6.estratificada.sleepy)

#######
#Calmo
df.calmo<-df6grupo %>% 
  filter(auxiliar=="Calmo")
table(df.calmo$Playlist_name)
#portanto a amostragem é estratificada
#portanto a amostragem é porcentagem então farei as porcentagem
n.calmo<-1475
#Calm
n.calmo.calm<-round(n*635/n.calmo,0)
n.calmo.relaxed<-round(n*840/n.calmo,0)
sum(n.calmo.calm,n.calmo.relaxed)

#Calm do dados calmo estatificada para 4 grupos
calmo6.estratificada.calm<-df6grupo %>% 
  filter(auxiliar=="Calmo") %>% 
  filter(Playlist_name=="Calm") %>% 
  sample_n(n.calmo.calm)

#Relaxed dos dados calmo estraficado por 4 grupos
calmo6.estratificada.relaxed<-df6grupo %>% 
  filter(auxiliar=="Calmo") %>% 
  filter(Playlist_name=="Relaxed") %>% 
  sample_n(n.calmo.relaxed)

#Juntando no banco de dados de Calmo
calmo6.estratificada<-rbind(calmo6.estratificada.calm,calmo6.estratificada.relaxed)
##############
#Medios
df.medios<-df6grupo %>% 
  filter(auxiliar=="Medios")

table(df.medios$Playlist_name)

n.medios<-length(df.medios$auxiliar)
#Medios
n.medios.annoying<-round(n*470/n.medios,0)
n.medios.excited<-round(n*518/n.medios,0)
n.medios.happy<-round(n*609/n.medios,0)
n.medios.pleased<-round(n*678/n.medios,0)

sum(n.medios.annoying,n.medios.excited,n.medios.happy,n.medios.pleased)
#Annoying do dados Medios estatificada para 6 grupos
medios6.estratificada.annoying<-df6grupo %>% 
  filter(auxiliar=="Medios") %>% 
  filter(Playlist_name=="Annoying") %>% 
  sample_n(n.medios.annoying)
#Excited dos dados medios estraficado por 6 grupos
medios6.estratificada.excited<-df6grupo %>% 
  filter(auxiliar=="Medios") %>% 
  filter(Playlist_name=="Excited") %>% 
  sample_n(n.medios.excited)
#Happy dos dados medios estraficado por 6 grupos
medios6.estratificada.happy<-df6grupo %>% 
  filter(auxiliar=="Medios") %>% 
  filter(Playlist_name=="Happy") %>% 
  sample_n(n.medios.happy)
#Pleased dos dados medios estraficado por 6 grupos
medios6.estratificada.pleased<-df6grupo %>% 
  filter(auxiliar=="Medios") %>% 
  filter(Playlist_name=="Pleased") %>% 
  sample_n(n.medios.pleased)
#Juntando agora os banco de dados Medios
medios6.estratificada<-rbind(medios6.estratificada.annoying,medios6.estratificada.excited,
                             medios6.estratificada.happy,medios6.estratificada.pleased)
#Nervous
df.nervous<-df6grupo %>% 
  filter(auxiliar=="Nervous")
table(df.nervous$Playlist_name)

n.nervous<-length(df.nervous$auxiliar)

n.nervous.boring<-round(n*555/n.nervous,0)
n.nervous.nervous<-round(n*634/n.nervous,0)

sum(n.nervous.boring,n.nervous.nervous)

#Boring dos dados calmo estraficado por 6 grupos
nervous6.estratificada.boring<-df6grupo %>% 
  filter(auxiliar=="Nervous") %>% 
  filter(Playlist_name=="Boring") %>% 
  sample_n(n.nervous.boring)

#Nervous dos dados calmo estraficado por 6 grupos
nervous6.estratificada.nervous<-df6grupo %>% 
  filter(auxiliar=="Nervous") %>% 
  filter(Playlist_name=="Nervous") %>% 
  sample_n(n.nervous.nervous)

#Agora juntando os bancos de dados
nervous6.estratificada<-rbind(nervous6.estratificada.boring,nervous6.estratificada.nervous)

#Agora vamos juntar tudo em um banco de dados e aplicar a random forest
df.6grupos.amostrado.estratificado<-rbind(angry6.estratificada,calmo6.estratificada,sad6.estratificada,medios6.estratificada,sleepy6.estratificada,nervous6.estratificada)

library(randomForest)
df.6grupos.amostrado.estratificado$auxiliar<-factor(df.6grupos.amostrado.estratificado$auxiliar)

music_model <- randomForest::randomForest(auxiliar ~ Energia:Positividade , data = df.6grupos.amostrado.estratificado)
print(music_model)
print(importance(music_model,type = 2)) 

err <- music_model$err.rate
head(err)
oob_err <- err[nrow(music_model$err),"OOB"]
print(oob_err)

plot(music_model)
legend(x = "right", legend = colnames(err),
       fill = 1:ncol(err))
##############################################
#Agora farei para 7 Grupos

table(df7grupo$auxiliar)
n=470
####
#Angry

df.angry<-df7grupo %>% 
  filter(auxiliar=="Angry")
table(df.angry$Playlist_name)

#portanto a amostragem é simples
angry7.estratificada<-df7grupo %>% 
  filter(auxiliar=="Angry") %>% 
  sample_n(n,replace = F)
#####
#Sad
df.sad<-df7grupo %>% 
  filter(auxiliar=="Sad")
table(df.sad$Playlist_name)
#portanto a amostragem é simples
sad7.estratificada<-df7grupo %>% 
  filter(auxiliar =="Sad") %>% 
  sample_n(n,replace = F)

#######
#Sleepy
df.sleepy<-df7grupo %>% 
  filter(auxiliar=="Sleepy")
table(df.sleepy$Playlist_name)
#portanto a amostragem é estratificada

n.sleepy<-length(df.sleepy$auxiliar)
n.sleepy.peacful<-round(n/n.sleepy*833,0)
n.sleepy.sleepy<-round(n/n.sleepy*881,0)
sum(n.sleepy.peacful,n.sleepy.sleepy)

#Peaceful dos dados sleepy com 7 grupos estratificado
sleepy7.estratificada.peaceful<-df7grupo %>% 
  filter(auxiliar=="Sleepy") %>% 
  filter(Playlist_name=="Peaceful") %>% 
  sample_n(n.sleepy.peacful)


#sleepy dos dados sleepy com 7 grupos estratificado
sleepy7.estratificada.sleepy<-df7grupo %>% 
  filter(auxiliar=="Sleepy") %>% 
  filter(Playlist_name=="Sleepy") %>% 
  sample_n(n.sleepy.sleepy)
#Juntando agora os banco de dados sleepy
sleepy7.estratificada<-rbind(sleepy7.estratificada.peaceful,sleepy7.estratificada.sleepy)

#######
#Calmo
df.calmo<-df7grupo %>% 
  filter(auxiliar=="Calmo")
table(df.calmo$Playlist_name)
#portanto a amostragem é estratificada
#portanto a amostragem é porcentagem então farei as porcentagem
n.calmo<-1475

n.calmo.calm<-round(n*635/n.calmo,0)
n.calmo.relaxed<-round(n*840/n.calmo,0)
sum(n.calmo.calm,n.calmo.relaxed)

#Calm do dados calmo estatificada para 4 grupos
calmo7.estratificada.calm<-df7grupo %>% 
  filter(auxiliar=="Calmo") %>% 
  filter(Playlist_name=="Calm") %>% 
  sample_n(n.calmo.calm)

#Relaxed dos dados calmo estraficado por 4 grupos
calmo7.estratificada.relaxed<-df7grupo %>% 
  filter(auxiliar=="Calmo") %>% 
  filter(Playlist_name=="Relaxed") %>% 
  sample_n(n.calmo.relaxed)

#Juntando no banco de dados de Calmo
calmo7.estratificada<-rbind(calmo7.estratificada.calm,calmo7.estratificada.relaxed)
##############################

#Medios
df.medios<-df7grupo %>% 
  filter(auxiliar=="Medios")

table(df.medios$Playlist_name)

n.medios<-length(df.medios$auxiliar)

n.medios.excited<-round(n*518/n.medios,0)
n.medios.happy<-round(n*609/n.medios,0)
n.medios.pleased<-round(n*678/n.medios,0)

sum(n.medios.excited,n.medios.happy,n.medios.pleased)
#Excited dos dados medios estraficado por 7 grupos
medios7.estratificada.excited<-df7grupo %>% 
  filter(auxiliar=="Medios") %>% 
  filter(Playlist_name=="Excited") %>% 
  sample_n(n.medios.excited)
#Happy dos dados medios estraficado por 7 grupos
medios7.estratificada.happy<-df7grupo %>% 
  filter(auxiliar=="Medios") %>% 
  filter(Playlist_name=="Happy") %>% 
  sample_n(n.medios.happy)
#Pleased dos dados medios estraficado por 7 grupos
medios7.estratificada.pleased<-df7grupo %>% 
  filter(auxiliar=="Medios") %>% 
  filter(Playlist_name=="Pleased") %>% 
  sample_n(n.medios.pleased)
#Juntando agora os banco de dados Medios
medios7.estratificada<-rbind(medios7.estratificada.excited,
                             medios7.estratificada.happy,medios7.estratificada.pleased)
#Nervous
df.nervous<-df7grupo %>% 
  filter(auxiliar=="Nervous")
table(df.nervous$Playlist_name)

n.nervous<-length(df.nervous$auxiliar)

n.nervous.boring<-round(n*555/n.nervous,0)
n.nervous.nervous<-round(n*634/n.nervous,0)

sum(n.nervous.boring,n.nervous.nervous)

#Boring dos dados calmo estraficado por 7 grupos
nervous7.estratificada.boring<-df7grupo %>% 
  filter(auxiliar=="Nervous") %>% 
  filter(Playlist_name=="Boring") %>% 
  sample_n(n.nervous.boring)

#Nervous dos dados calmo estraficado por 7 grupos
nervous7.estratificada.nervous<-df7grupo %>% 
  filter(auxiliar=="Nervous") %>% 
  filter(Playlist_name=="Nervous") %>% 
  sample_n(n.nervous.nervous)

#Agora juntando os bancos de dados
nervous7.estratificada<-rbind(nervous7.estratificada.boring,nervous7.estratificada.nervous)

#Chato
df.chato<-df7grupo %>% 
  filter(auxiliar=="Chato")
table(df.chato$Playlist_name)
#portanto é apenas uma amostragem simples
chato7.estratificada<-df7grupo %>% 
  filter(auxiliar=="Chato") %>% 
  sample_n(470,replace = F)

#Agora vamos juntar tudo em um banco de dados e aplicar a random forest
df.7grupos.amostrado.estratificado<-rbind(angry7.estratificada,calmo7.estratificada,sad7.estratificada,medios7.estratificada,sleepy7.estratificada,nervous7.estratificada,chato7.estratificada)

library(randomForest)
df.7grupos.amostrado.estratificado$auxiliar<-factor(df.7grupos.amostrado.estratificado$auxiliar)

music_model <- randomForest::randomForest(auxiliar ~ Energia:Positividade , data = df.7grupos.amostrado.estratificado)
print(music_model)
print(importance(music_model,type = 2)) 

err <- music_model$err.rate
head(err)
oob_err <- err[nrow(music_model$err),"OOB"]
print(oob_err)

plot(music_model)
legend(x = "right", legend = colnames(err),
       fill = 1:ncol(err))

###################FINALL###################################
library(dplyr)
library(patchwork)
library(scales)
library(rlang)
library(ggplot2)
library(tidyverse )

df <- dados1 %>% 
  dplyr::select(Energia:Modo, Playlist_name) %>% 
  pivot_longer(!Playlist_name,names_to = "variavel", values_to = "valor") %>%
  mutate(variavel = factor(variavel))

p1 <- df %>%  
  filter(variavel == "Energia") %>% 
  ggplot(aes(x = variavel, y = valor))+
  geom_boxplot(fill = "#EF9A9A")+
  theme_classic()+
  theme(axis.title.x=element_blank())

p2 <- df %>% 
  filter(variavel == "Ao_vivo") %>% 
  ggplot(aes(x = variavel, y = valor))+
  geom_boxplot(fill = "#D32F2F")+
  theme_classic()+
  theme(axis.title.x=element_blank())

p3 <- df %>% 
  filter(variavel == "Tempo") %>% 
  ggplot(aes(x = variavel, y = valor))+
  geom_boxplot(fill = "#7B1FA2")+
  theme_classic()+
  theme(axis.title.x=element_blank())

p4 <- df %>% 
  filter(variavel == "Falado") %>% 
  ggplot(aes(x = variavel, y = valor))+
  geom_boxplot(fill = "#D81B60")+
  theme_classic()+
  theme(axis.title.x=element_blank())

p5 <- df %>% 
  filter(variavel == "Orgânico") %>% 
  ggplot(aes(x = variavel, y = valor))+
  geom_boxplot(fill = "#2962FF")+
  theme_classic()+
  theme(axis.title.x=element_blank())

p6 <- df %>% 
  filter(variavel == "Instrumental") %>% 
  ggplot(aes(x = variavel, y = valor))+
  geom_boxplot(fill = "#81C784")+
  theme_classic()+
  theme(axis.title.x=element_blank())

p7 <- df %>% 
  filter(variavel == "Compasso") %>% 
  mutate(valor = factor(valor)) %>% 
  count(valor) %>% 
  ggplot(aes(x = valor, y = n, fill = valor))+
  geom_col()+
  geom_text(aes(label = n), vjust = -0.4)+
  theme_classic()+
  labs(title = "Compasso",
       y = "frequência")+
  theme(axis.title.x=element_blank())+
  ylim(0,7000)

p8 <- df %>% 
  filter(variavel == "Dançabilidade") %>% 
  ggplot(aes(x = variavel, y = valor))+
  geom_boxplot(fill = "#EEFF41")+
  theme_classic()+
  theme(axis.title.x=element_blank())

p9 <- df %>% 
  filter(variavel == "Tom") %>% 
  mutate(valor = factor(valor)) %>% 
  count(valor) %>% 
  ggplot(aes(x = valor, y = n, fill = valor))+
  geom_col()+
  geom_text(aes(label = n), vjust = -0.4)+
  theme_classic()+
  labs(title = "Tom",
       y = "frequência")+
  theme(axis.title.x=element_blank(),
        legend.position = "none")+
  ylim(0,1500)


p10 <- dados1 %>% 
  select(Duração) %>% 
  mutate(variavel = "Duração") %>% 
  ggplot(aes(x = variavel, y = Duração))+
  geom_boxplot(fill = "#FF6D00")+
  scale_y_continuous(label = scales::comma)+
  theme_classic()+
  theme(axis.title.x=element_blank())

p11 <- df %>% 
  filter(variavel == "Força") %>% 
  ggplot(aes(x = variavel, y = valor))+
  geom_boxplot(fill = "#B0BEC5")+
  theme_classic()+
  theme(axis.title.x=element_blank())

p12 <- df %>% 
  filter(variavel == "Positividade") %>% 
  ggplot(aes(x = variavel, y = valor))+
  geom_boxplot(fill = "#1DE9B6")+
  theme_classic()+
  theme(axis.title.x=element_blank())

p13 <- df %>% 
  filter(variavel == "Modo") %>% 
  mutate(valor = factor(valor)) %>% 
  count(valor) %>% 
  ggplot(aes(x = valor, y = n, fill = valor))+
  geom_col()+
  geom_text(aes(label = n), vjust = -0.4)+
  theme_classic()+
  labs(y = "frequência",
       x = "modo")+
  ylim(0,5500)

pescala <- (p1+p2+p4)/(p5+p6+p8)/(p11 + p12)
pescala <- pescala + plot_annotation(
  title = 'Boxplot das variáveis contínuas no intervalo [0,1]')


ptotal <- (p3+p7)/(p10+p13)/p9
ptotal <- ptotal + plot_annotation(
  title = "Gráficos de variáveis fora do intervalo e fatoriais")

ptotal
pescala

#ggridges
library(ggridges)
library(glue)

graf <- function(df,xvar){
  x.var <- rlang::sym(quo_name(enquo(xvar)))#usa esse codigo na variavel
  # e coloca !! na frente dos eixos com a variável
  p <- ggplot(df, aes(x =!! x.var, y = Playlist_name, fill = stat(x)))+
    geom_density_ridges_gradient(scale = 3, size = 0.4, rel_min_height = 0.01)+
    scale_fill_viridis_c(name = x.var, option = "C")+
    theme_minimal()+
    labs(title = glue("Distribuições de {xvar} por Sentimento"),
         y = "Sentimento da playlist") 
  return(p)
}

g_energia <- graf(dados1,"Energia"))
(g_aovivo <- graf(dados1,"Ao_vivo"))
(g_tempo <- graf(dados1,"Tempo"))
(g_falado <- graf(dados1,"Falado"))
(g_organico <- graf(dados1,"Orgânico"))

(g_instrumental <- graf(dados1,"Instrumental"))
(g_compasso <- graf(dados1,"Compasso"))
(g_dançabilidade <- graf(dados1,"Dançabilidade"))
(g_tom <- graf(dados1,"Tom"))
(g_duração <- graf(dados1,"Duração"))#Com problema na escala
(g_força <- graf(dados1,"Força"))
(g_positividade <- graf(dados1,"Positividade"))

