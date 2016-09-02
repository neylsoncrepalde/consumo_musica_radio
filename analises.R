# Artigo O Consumo de Música e o Rádio
# Aírton Gonçalves e Neylson Crepalde
# Script: Neylson Crepalde

library(foreign)
library(descr)
library(ggplot2)
library(ggthemes)
library(Hmisc)
library(ca)
library(xtable)
library(stargazer)

#funcao V de Cramer
cv.test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
  print.noquote("Cramér V / Phi:")
  return(as.numeric(CV))
}
###################################
dados = read.table("dados_airton.csv",sep=";", header=T, na="NA", 
                    stringsAsFactors = F, fileEncoding = "Latin1")
#head(dados)
#names(dados)
#View(dados)

dados$Sexo <- factor(dados$Sexo,
                     levels = c(0,1),
                     labels = c("Feminino", "Masculino"))
freq(dados$Sexo)

idade = summary(dados[2])

dados$Escolaridade <- factor(dados$Escolaridade,
                             levels = c(1,2,3,4,5),
                             labels = c("Ensino Médio incompleto", "Ensino Médio completo", 
                                        "Superior incompleto", "Superior completo", "Pós-graduação"))
freq(dados[[4]])
ggplot(data=dados, aes(x=factor(dados[[4]])))+geom_bar()+
  labs(title="Escolaridade", y="", x="")+
  scale_x_discrete(labels=c("EM inc.", "EM completo", "Superior inc.",
                            "Superior comp.", "Pós-graduação"))

freq(dados[[5]]) #bairros


freq(dados[[6]]) #profissão
#===================================
#arrumando as categorias
dados[[6]] = as.character(dados[[6]])
dados[[6]][dados[[6]]=="adminiatrador"] = "Administrador"
dados[[6]][dados[[6]]=="ADMINISTRADOR"] = "Administrador"
dados[[6]][dados[[6]]=="ASSIST. ADMINISTRATIVO"] = "Assistente Administrativo"
dados[[6]][dados[[6]]=="Assistente adm"] = "Assistente Administrativo"
dados[[6]][dados[[6]]=="Auxiliar administrativo"] = "Auxiliar Administrativo"
dados[[6]][dados[[6]]=="Auxiliar administrativo "] = "Auxiliar Administrativo"
dados[[6]][dados[[6]]=="Aux.Administrativo"] = "Auxiliar Administrativo"
dados[[6]][dados[[6]]=="costura"] = "Costureira"
dados[[6]][dados[[6]]=="contadora"] = "Contador"
dados[[6]][dados[[6]]=="desempregada"] = "Desempregado"
dados[[6]][dados[[6]]=="empresario"] = "Empresário"
dados[[6]][dados[[6]]=="empresário"] = "Empresário"
dados[[6]][dados[[6]]=="estagiário"] = "Estagiário"
dados[[6]][dados[[6]]=="Estagiario"] = "Estagiário"
dados[[6]][dados[[6]]=="estudante"] = "Estudante"
dados[[6]][dados[[6]]=="Estudante "] = "Estudante"
dados[[6]][dados[[6]]=="Mucissista "] = "Músico"
dados[[6]][dados[[6]]=="Musicista"] = "Músico"
dados[[6]][dados[[6]]=="musicista (violonista)"] = "Músico"
dados[[6]][dados[[6]]=="músico"] = "Músico"
dados[[6]][dados[[6]]=="professor"] = "Professor"
dados[[6]][dados[[6]]=="Professora"] = "Professor"
dados[[6]][dados[[6]]=="Professor "] = "Professor"
dados[[6]][dados[[6]]=="Professor de música Educação Infantil"] = "Professor de Música"
dados[[6]][dados[[6]]=="professora de música"] = "Professor de Música"
dados[[6]][dados[[6]]=="Professora de música"] = "Professor de Música"
dados[[6]][dados[[6]]=="psicologa"] = "Psicólogo"
dados[[6]][dados[[6]]=="Psicóloga"] = "Psicólogo"
dados[[6]][dados[[6]]=="PUBLICITÁRIO"] = "Publicitário"
dados[[6]][dados[[6]]=="Socióloga"] = "Sociólogo"
dados[[6]][dados[[6]]=="Teóloga"] = "Teólogo"
dados[[6]][dados[[6]]=="Produtora"] = "Produtor"
dados[[6]][dados[[6]]=="motorista"] = "Motorista"
dados[[6]][dados[[6]]=="motorista de caminhão"] = "Motorista"
dados[[6]][dados[[6]]=="Farmaceutica"] = "Farmacêutico"
dados[[6]][dados[[6]]=="ENGENHEIRO CIVIL"] = "Engenheiro"
dados[[6]][dados[[6]]=="Técnico de Desenvolvimento Econômico e Social"] = "Téc. em Desenv. Econ. e Social"

dados[[6]] = factor(dados[[6]])
freq(dados[[6]])
#======================================================

ggplot(data=dados, aes(x=reorder(dados[[6]], dados[[6]], length)))+geom_bar()+
  labs(title="Profissões", x="", y="")+coord_flip()


freq(dados[[7]]) #artista favorito
#=================================================
#arrumando as categorias
dados[[7]] = as.character(dados[[7]])
dados[[7]][dados[[7]]=="Bono Vox / U2"] = "U2"
dados[[7]][dados[[7]]=="chico buarque"] = "Chico Buarque"
dados[[7]][dados[[7]]=="Não tenho apenas um... Brasileiro eu diria que é Chico Buarque."] = "Chico Buarque"
dados[[7]][dados[[7]]=="Djavan "] = "Djavan"
dados[[7]][dados[[7]]=="Ed Sheersn"] = "Ed Sheeran"
dados[[7]][dados[[7]]=="Gosto de músicas, independente do artista. Só não gosto de Heavy Metal."] = "Nenhum"
dados[[7]][dados[[7]]=="MICHAEL JACKSON"] = "Michael Jackson"
dados[[7]][dados[[7]]=="Ministério Pedars Vivas"] = "Ministério Pedras Vivas"
dados[[7]][dados[[7]]=="nao ha"] = "Nenhum"
dados[[7]][dados[[7]]=="Não sei, gosto de vários"] = "Nenhum"
dados[[7]][dados[[7]]=="Não tenho"] = "Nenhum"
dados[[7]][dados[[7]]=="Paulo César Baruk "] = "Paulo César Baruk"
dados[[7]][dados[[7]]=="thales roberto"] = "Thalles Roberto"
dados[[7]][dados[[7]]=="Thalles Robert"] = "Thalles Roberto"
dados[[7]][dados[[7]]=="Varios"] = "Vários"
dados[[7]][dados[[7]]=="Ze Ramalho"] = "Zé Ramalho"
dados[[7]][dados[[7]]=="ZÉ RAMALHO"] = "Zé Ramalho"
dados[[7]][dados[[7]]=="zê ramalho"] = "Zé Ramalho"
dados[[7]][dados[[7]]=="varios"] = "Vários"
dados[[7]][dados[[7]]=="Vários e várias"] = "Vários"
dados[[7]] = factor(dados[[7]])
freq(dados[[7]])
#=================================================
ggplot(data=dados, aes(x=reorder(dados[[7]], dados[[7]], length)))+geom_bar()+
  labs(title="Quem é seu artista favorito?", x="", y="")+coord_flip()



freq(dados[[8]]) #como conheceu seu artista favorito
#=====================================================
#arrumando as categorias
dados[[8]] = as.character(dados[[8]])
dados[[8]][dados[[8]]=="amigos"] = "Amigos"
dados[[8]][dados[[8]]=="amigos, familiares, rádio, etc"] = "Amigos"
dados[[8]][dados[[8]]=="festival de musica"] = "Festival de Música"
dados[[8]][dados[[8]]=="Fita"] = "Fita K7"
dados[[8]][dados[[8]]=="influiencia de minha familia"] = "Influência Familiar"
dados[[8]][dados[[8]]=="Meu pai"] = "Influência Familiar"
dados[[8]][dados[[8]]=="núcleo familiar"] = "Influência Familiar"
dados[[8]][dados[[8]]=="nao se aplica"] = NA
dados[[8]][dados[[8]]=="Não tenho um artista favorito"] = NA
dados[[8]][dados[[8]]=="Sinceramente não lembro, desde criança eu escuto a música deles. "] = NA

dados[[8]] = factor(dados[[8]])
freq(dados[[8]])
#=====================================================
names(dados)[8] <- "meio.artista"
ggplot(data=dados[!is.na(dados$meio.artista), ], 
       aes(x=reorder(meio.artista, meio.artista, length)))+
  geom_bar()+
  labs(title="Através de que meio você conheceu seu artista favorito?", y="", x="")+
  coord_flip()



freq(dados[[9]])#que meio mais usa pra ouvir musica
#=====================================================
#arrumando as categorias
dados[[9]] = as.character(dados[[9]])
dados[[9]][dados[[9]]==" internet"] = "Mídias Sociais (internet)"
dados[[9]][dados[[9]]=="CD e Internet"] = "CD"
dados[[9]][dados[[9]]=="cd, rádio, internet"] = "CD"
dados[[9]][dados[[9]]=="Cd e Internet"] = "CD"
dados[[9]][dados[[9]]=="celular"] = "Celular"
dados[[9]][dados[[9]]=="Celular MP3"] = "Celular"
dados[[9]][dados[[9]]=="mp3 - através do celular"] = "Celular"
dados[[9]][dados[[9]]=="pen drive"] = "Pen Drive"
dados[[9]][dados[[9]]=="Pen drive carro"] = "Pen Drive"
dados[[9]][dados[[9]]=="playlist de pendrive"] = "Pen Drive"
dados[[9]][dados[[9]]=="SPOTIFY"] = "Spotify"
dados[[9]] = factor(dados[[9]])
freq(dados[[9]])
#=====================================================
ggplot(data=dados, aes(x=reorder(dados[[9]], dados[[9]], length)))+geom_bar()+
  labs(title="Que meio você mais usa para ouvir música?", y="", x="")+
  coord_flip()

tab.prof.radio = table(dados[[6]], dados[[9]])
chisq.test(tab.prof.radio)
cv.test(dados[[6]], dados[[9]])
#cramer.test(dados[[6]], dados[[9]])

ca = ca(tab.prof.radio)
plot(ca, dim=c(1,2), labels = 1, main = "Correspondence analysis (profissões x meio de escuta)")


freq(dados[[10]]) #que meio voce mais usa pra conhecer novas musicas
#===========================================================
# arrumando as categorias
dados[[10]] = as.character(dados[[10]])
dados[[10]][dados[[10]]=="internet"] = "Mídias Sociais (internet)"
dados[[10]][dados[[10]]=="amigos"] = "Amigos"
dados[[10]][dados[[10]]=="spotify"] = "Spotify"
dados[[10]][dados[[10]]=="SPOTIFY"] = "Spotify"
dados[[10]] = factor(dados[[10]])
freq(dados[[10]])
#===========================================================
ggplot(data=dados, aes(x=factor(dados[[10]])))+geom_bar()+
  labs(title="Que meio você mais usa para conhecer novas músicas?'", y="", x="")+
  theme(axis.text.x = element_text(angle=35, hjust=1))


tab.prof.radio = table(dados[[6]], dados[[9]])
ca = ca(tab.prof.radio)
plot(ca, dim=c(1,2), labels = 1, main = "Correspondence analysis (profissões x meio de escuta)")

freq(dados[[11]]) #voce tem o habito de ouvir o radio?

#Separando a amostra daquelas pessoas que ouvem radio
ouvem.radio = dados[dados[[11]]=="Sim",]

freq(ouvem.radio[[12]]) #qual estacao vc mais escuta
#arrumando as variaveis
#====================================================
ouvem.radio[[12]] = as.character(ouvem.radio[[12]])
ouvem.radio[[12]][ouvem.radio[[12]]=="100,9 Inconfidência"] = "100,9"
ouvem.radio[[12]][ouvem.radio[[12]]=="102"] = "102,1"
ouvem.radio[[12]][ouvem.radio[[12]]=="90.1"] = "90,1"
ouvem.radio[[12]][ouvem.radio[[12]]=="90.1 FM"] = "90,1"
ouvem.radio[[12]][ouvem.radio[[12]]=="94.9"] = "94,9"
ouvem.radio[[12]][ouvem.radio[[12]]=="98"] = "98 FM"
ouvem.radio[[12]][ouvem.radio[[12]]=="98 fm"] = "98 FM"
ouvem.radio[[12]][ouvem.radio[[12]]=="98,3"] = "98 FM"
ouvem.radio[[12]][ouvem.radio[[12]]=="98fm"] = "98 FM"
ouvem.radio[[12]][ouvem.radio[[12]]=="98FM"] = "98 FM"
ouvem.radio[[12]][ouvem.radio[[12]]=="Alvorada e UFMG"] = "Alvorada"
ouvem.radio[[12]][ouvem.radio[[12]]=="alvorada fm"] = "Alvorada"
ouvem.radio[[12]][ouvem.radio[[12]]=="ALVORADA FM"] = "Alvorada"
ouvem.radio[[12]][ouvem.radio[[12]]=="Alvorada FM 94,9"] = "Alvorada"
ouvem.radio[[12]][ouvem.radio[[12]]=="cbn"] = "CBN"
ouvem.radio[[12]][ouvem.radio[[12]]=="Cbn"] = "CBN"
ouvem.radio[[12]][ouvem.radio[[12]]=="CBN e Band News FM"] = "CBN"
ouvem.radio[[12]][ouvem.radio[[12]]=="inconfidencia"] = "Inconfidência"
ouvem.radio[[12]][ouvem.radio[[12]]=="Inconfidencia"] = "Inconfidência"
ouvem.radio[[12]][ouvem.radio[[12]]=="jovem pan"] = "Jovem Pan"
ouvem.radio[[12]][ouvem.radio[[12]]=="JOVEM PAN"] = "Jovem Pan"
ouvem.radio[[12]][ouvem.radio[[12]]=="quarani"] = "Guarani"
ouvem.radio[[12]][ouvem.radio[[12]]=="transamerica"] = "Transamérica"
ouvem.radio[[12]] = factor(ouvem.radio[[12]])
freq(ouvem.radio[[12]])
#=====================================================
ggplot(data=ouvem.radio, aes(ouvem.radio[[12]]))+geom_histogram()+
  labs(title="Qual estação você mais escuta?", x="", y="")+
  theme(axis.text.x = element_text(angle=45, hjust=1))


freq(ouvem.radio[[13]]) # em quais horarios

freq(ouvem.radio[[14]]) #com qual finalidade
motivos <- c("ouvir música","ouvir notícias","info. trânsito","pregações religiosas","músicas novas","diversão","jogos","futebol","humor","esporte")
valores <- c(58,39,16,8,16,1,2,2,2,1)
finalidade = rbind(motivos, valores)
finalidade = cbind(motivos, valores)
finalidade = as.data.frame(finalidade)
ggplot(data=finalidade, aes(x=motivos, y=valores))+
  geom_bar(stat="identity")+coord_flip()


freq(ouvem.radio[[15]]) #voce acompanha algum programa
#arrumando as categorias
#============================================================
ouvem.radio[[15]] = as.character(ouvem.radio[[15]])
ouvem.radio[[15]][ouvem.radio[[15]]=="graffite"] = "Graffite"
ouvem.radio[[15]][ouvem.radio[[15]]=="grafitte"] = "Graffite"
ouvem.radio[[15]][ouvem.radio[[15]]=="nao"] = "Não"
ouvem.radio[[15]][ouvem.radio[[15]]=="Nao"] = "Não"
ouvem.radio[[15]][ouvem.radio[[15]]=="NAO"] = "Não"
ouvem.radio[[15]][ouvem.radio[[15]]=="não"] = "Não"
ouvem.radio[[15]][ouvem.radio[[15]]=="NÃO"] = "Não"
ouvem.radio[[15]][ouvem.radio[[15]]=="não "] = "Não"
ouvem.radio[[15]][ouvem.radio[[15]]=="Não "] = "Não"
ouvem.radio[[15]][ouvem.radio[[15]]=="Não."] = "Não"
ouvem.radio[[15]][ouvem.radio[[15]]=="sim"] = "Sim"

ouvem.radio[[15]] = factor(ouvem.radio[[15]])
freq(ouvem.radio[[15]])
#============================================================

freq(ouvem.radio[[16]]) #voce acompanha algum radialista
#arrumando as categorias
#======================================================
ouvem.radio[[16]] = as.character(ouvem.radio[[16]])
ouvem.radio[[16]][ouvem.radio[[16]]=="Alguns que sei e outros que não sei o nome"] = "Não sei"
ouvem.radio[[16]][ouvem.radio[[16]]=="Dudu do Grafitte"] = "Dudu do Graffite"
ouvem.radio[[16]][ouvem.radio[[16]]=="DUDU GRAFITTE"] = "Dudu do Graffite"
ouvem.radio[[16]][ouvem.radio[[16]]=="gosto de alguns, mas nenhum específico"] = "Vários"
ouvem.radio[[16]][ouvem.radio[[16]]=="Gosto do Elias Santos, mas não o acompanho."] = "Elias Santos"
ouvem.radio[[16]][ouvem.radio[[16]]=="nã0"] = "Não"
ouvem.radio[[16]][ouvem.radio[[16]]=="nao"] = "Não"
ouvem.radio[[16]][ouvem.radio[[16]]=="NAO"] = "Não"
ouvem.radio[[16]][ouvem.radio[[16]]=="não"] = "Não"
ouvem.radio[[16]][ouvem.radio[[16]]=="não "] = "Não"
ouvem.radio[[16]][ouvem.radio[[16]]=="Não "] = "Não"
ouvem.radio[[16]][ouvem.radio[[16]]=="Não."] = "Não"
ouvem.radio[[16]][ouvem.radio[[16]]=="não  "] = "Não"
ouvem.radio[[16]][ouvem.radio[[16]]=="Não sei"] = "Não"
ouvem.radio[[16]][ouvem.radio[[16]]=="Não tenho preferência"] = "Não"
ouvem.radio[[16]][ouvem.radio[[16]]=="SIM"] = "Sim"
ouvem.radio[[16]][ouvem.radio[[16]]=="Sim. Sardemberg e Roberto Nonato"] = "Carlos Alberto Sardemberg"
ouvem.radio[[16]][ouvem.radio[[16]]=="Sou fã de vários mas o principais são o Eduardo Schechetel (Dudu), Alberto Rodrigues e o Gilbert Campos. "] = "Eduardo Schechetel"
ouvem.radio[[16]][ouvem.radio[[16]]=="tutti maravilha"] = "Tutti Maravilha"
ouvem.radio[[16]][ouvem.radio[[16]]=="Tuty maravilha"] = "Tutti Maravilha"
ouvem.radio[[16]][ouvem.radio[[16]]=="vários"] = "Vários"

ouvem.radio[[16]] = factor(ouvem.radio[[16]])
freq(ouvem.radio[[16]])
#========================================================

#tabulacoes
meio <- dados[dados$meio.musica == "Mídias Sociais (internet)" | dados$meio.musica == "Rádio",]
ggplot(data=meio, aes(x=meio.musica, y=Qual.é.a.sua.idade.))+
  geom_boxplot()+labs(title="Médias de idade", x="Meio", y="Idade")+
  annotate("text", x=1, y=27.6, label="27.1", size=4.5)+
  annotate("text", x=2, y=35.5, label="33.9", size=4.5)
