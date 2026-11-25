# Projeto: Modelagem Bioclimática do Alto Curso do Rio Paraíba
# Organização para o repositório (2026): Samuel Almeida Fragoso Vasconcelos

"
Copyright (c) 2017 Mônica Macêdo, Marcilio Lemos

@title  Funções auxiliares para o calculo do balanço hídrico 

@author Mônica Macêdo and Samuel Almeida Fragoso Vasconcelos
"

"
@description  fator que corrigi o valor de ETP para cada mês de acordo com a latitude do posto  
@param lat  valor da latitude.
"
fatorCorrecaoETP<-function(lat){
  
  # tabela de correcao de ETP segundo Dunne y Leopold, 1978. 
  corrTabelaS <- data.frame(Lat=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35),
                            Jan=c(12.2, 12.3, 12.3, 12.4, 12.4, 12.5, 12.5, 12.6, 12.6, 12.7, 12.7, 12.8, 12.8, 12.9, 12.9, 13.0, 13.0, 13.1, 13.2, 13.2, 13.3, 13.3, 13.4, 13.5, 13.5, 13.6, 13.6, 13.7, 13.8, 13.9, 13.9, 14.0, 14.1, 14.1, 14.2),
                            Fev=c(12.2, 12.2, 12.2, 12.3, 12.3, 12.3, 12.4, 12.4, 12.4, 12.5, 12.5, 12.5, 12.5, 12.6, 12.6, 12.6, 12.7, 12.7, 12.7, 12.8, 12.8, 12.8, 12.9, 12.9, 12.9, 13.0, 13.0, 13.1, 13.1, 13.1, 13.2, 13.2, 13.3, 13.3, 13.4),
                            Mar=c(12.2, 12.2, 12.2, 12.2, 12.2, 12.2, 12.2, 12.2, 12.2, 12.2, 12.2, 12.2, 12.2, 12.2, 12.2, 12.2, 12.2, 12.2, 12.2, 12.2, 12.3, 12.3, 12.3, 12.3, 12.3, 12.3, 12.3, 12.3, 12.3, 12.3, 12.3, 12.3, 12.3, 12.3, 12.3),
                            Abr=c(12.1, 12.1, 12.1, 12.1, 12.0, 12.0, 12.0, 12.0, 11.9, 11.9, 11.9, 11.9, 11.9, 11.8, 11.8, 11.8, 11.8, 11.7, 11.7, 11.7, 11.7, 11.6, 11.6, 11.6, 11.6, 11.5, 11.5, 11.5, 11.4, 11.4, 11.4, 11.3, 11.3, 11.3, 11.3),
                            Mai=c(12.1, 12.1, 12.0, 12.0, 11.9, 11.9, 11.8, 11.8, 11.7, 11.7, 11.6, 11.6, 11.6, 11.5, 11.5, 11.4, 11.4, 11.3, 11.3, 11.2, 11.2, 11.1, 11.0, 11.0, 10.9, 10.9, 10.8, 10.8, 10.7, 10.6, 10.6, 10.5, 10.5, 10.4, 10.3),
                            Jun=c(12.1, 12.0, 12.0, 11.9, 11.9, 11.8, 11.7, 11.7, 11.6, 11.6, 11.5, 11.5, 11.4, 11.3, 11.3, 11.2, 11.2, 11.1, 11.0, 11.0, 10.9, 10.8, 10.8, 10.7, 10.6, 10.5, 10.5, 10.4, 10.3, 10.2, 10.2, 10.1, 10.0, 9.9, 9.8),
                            Jul=c(12.1, 12.0, 12.0, 11.9, 11.9, 11.8, 11.8, 11.7, 11.7, 11.6, 11.6, 11.5, 11.5, 11.4, 11.3, 11.3, 11.2, 11.2, 11.1, 11.1, 11.0, 10.9, 10.9, 10.8, 10.7, 10.7, 10.6, 10.5, 10.5, 10.4, 10.3, 10.3, 10.2, 10.1, 10.0),
                            Ago=c(12.1, 12.1, 12.0, 12.0, 12.0, 11.9, 11.9, 11.9, 11.8, 11.8, 11.8, 11.7, 11.7, 11.7, 11.6, 11.6, 11.6, 11.5, 11.5, 11.5, 11.4, 11.4, 11.3, 11.3, 11.3, 11.2, 11.2, 11.1, 11.1, 11.0, 11.0, 11.0, 10.9, 10.9, 10.8),
                            Set=c(12.1, 12.1, 12.1, 12.1, 12.1, 12.1, 12.1, 12.1, 12.1, 12.1, 12.1, 12.1, 12.0, 12.0, 12.0, 12.0, 12.0, 12.0, 12.0, 12.0, 12.0, 12.0, 12.0, 12.0, 11.9, 11.9, 11.9, 11.9, 11.9, 11.9, 11.9, 11.9, 11.9, 11.8, 11.8),
                            Out=c(12.2, 12.2, 12.2, 12.2, 12.2, 12.3, 12.3, 12.3, 12.3, 12.3, 12.4, 12.4, 12.4, 12.4, 12.4, 12.5, 12.5, 12.5, 12.5, 12.6, 12.6, 12.6, 12.6, 12.6, 12.7, 12.7, 12.7, 12.7, 12.8, 12.8, 12.8, 12.8, 12.9, 12.9, 12.9),
                            Nov=c(12.2, 12.2, 12.3, 12.3, 12.4, 12.4, 12.5, 12.5, 12.5, 12.6, 12.6, 12.7, 12.7, 12.8, 12.8, 12.9, 12.9, 13.0, 13.0, 13.1, 13.1, 13.2, 13.2, 13.3, 13.3, 13.4, 13.4, 13.5, 13.5, 13.6, 13.7, 13.7, 13.8, 13.8, 13.9),
                            Dez=c(12.2, 12.3, 12.3, 12.4, 12.4, 12.5, 12.5, 12.6, 12.7, 12.7, 12.8, 12.8, 12.9, 13.0, 13.0, 13.1, 13.1, 13.2, 13.3, 13.3, 13.4, 13.5, 13.5, 13.6, 13.7, 13.7, 13.8, 13.9, 14.0, 14.0, 14.1, 14.2, 14.3, 14.4, 14.4))
  
  corrTabelaN <- data.frame(Lat=c(5, 4, 3, 2, 1, 0),
                            Jan=c(11.8, 11.8, 11.7, 11.7, 11.6, 12.1),
                            Fev=c(12.0, 11.9, 11.9, 11.9, 11.8, 12.1),
                            Mar=c(12.1, 12.1, 12.1, 12.1, 12.1, 12.1),
                            Abr=c(12.3, 12.3, 12.3, 12.4, 12.4, 12.1),
                            Mai=c(12.4, 12.5, 12.5, 12.6, 12.6, 12.1),
                            Jun=c(12.5, 12.5, 12.6, 12.7, 12.7, 12.1),
                            Jul=c(12.5, 12.5, 12.6, 12.6, 12.7, 12.1),
                            Ago=c(12.4, 12.4, 12.4, 12.5, 12.5, 12.1),
                            Set=c(12.2, 12.2, 12.2, 12.2, 12.2, 12.1),
                            Out=c(12.0, 12.0, 12.0, 12.0, 12.0, 12.1),
                            Nov=c(11.9, 11.8, 11.8, 11.8, 11.7, 12.1),
                            Dez=c(11.8, 11.8, 11.7, 11.6, 11.6, 12.1))
  
  
  
  Li<-NULL
  latAux<-as.numeric(as.character(lat))
  if(latAux<0)
    Li=corrTabelaS[corrTabelaS$Lat==round(abs(latAux)),]
  else if(latAux>=0)
    Li=corrTabelaN[corrTabelaN$Lat==round(latAux),]
  
  Li$Lat<-NULL
  
  diasMes<-c(31,28,31,30,31,30,31,31,30,31,30,31)
  return((diasMes*as.numeric(Li))/12)
}

"
@description  retorna a capacidade de armazenamento de água de acordo com o solo e cultivo
@param tipoUso tipo de cultivo do solo
@param tipoSolo tipo do solo
"
capacidadeCampo<-function(tipoSolo){
  
  CCTabela <- data.frame(Tipo.do.Solo=c('arenoso fino','franco-arenoso fino','franco-limosa','franco-argiloso','argiloso'),
                         CC= c(100,150,200,250,300))
  
  cc=CCTabela[CCTabela$Tipo.do.Solo==tipoSolo,2]
  return(as.numeric(cc))
}


"
@description  retorna o valor de ETP para temperaturas acima dos 26,5
@param T temperatura
"
tabelaETP<-function(T){
  
  ETPTabela<-matrix(c(0,0,0,0,0,4.5,4.5,4.6,4.6,4.6,4.6,4.7,4.7,4.7,4.8,4.8,
                      4.8,4.8,4.9,4.9,4.9,5.0,5.0,5.0,5.0,5.1,5.1,5.1,5.1,5.2,
                      5.2,5.2,5.2,5.2,5.3,5.3,5.3,5.3,5.4,5.4,5.4,5.4,5.4,5.5,
                      5.5,5.5,5.5,5.5,5.6,5.6,5.6,5.6,5.6,5.7,5.7,5.7,5.7,5.7,
                      5.7,5.8,5.8,5.8,5.8,5.8,5.8,5.8,5.9,5.9,5.9,5.9,5.9,5.9,
                      5.9,5.9,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.1,6.1,6.1,
                      6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,6.1,
                      6.1,6.1,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.2,6.2,
                      6.2,6.2,6.2,6.2,6.2,6.2),nrow = 12, ncol = 10,byrow = TRUE)
  fracao<-T%%1
  inteiro<-T-fracao
  return(ETPTabela[cbind(inteiro%%26+1, as.integer(fracao*10+1.5))])
}

"
@description  retorna o indice do ultimo mes do periodo chuvoso
@param PmenosETPCorr diferença entra precipitação e ETP corrigida
"
ultimoMesPeriodoChuvoso<-function(PmenosETPCorr){
  
  periodoChuvoso<-which(PmenosETPCorr>0)
  inicioChuvoso<-which(c(2,diff(periodoChuvoso))>1)
  maisChuvoso<-periodoChuvoso[inicioChuvoso]
  numPeriodos<-length(inicioChuvoso)
  if(numPeriodos>1){
    cum<-sapply(1:(numPeriodos-1),function(x) sum(PmenosETPCorr[periodoChuvoso[inicioChuvoso[x]]:periodoChuvoso[inicioChuvoso[x+1]-1]]))
    cum[numPeriodos]<-sum(PmenosETPCorr[periodoChuvoso[inicioChuvoso[length(inicioChuvoso)]]:periodoChuvoso[length(periodoChuvoso)]])
    maisChuvoso<-periodoChuvoso[inicioChuvoso[which.max(cum)]]  
  }
  periodoSeca<-which(PmenosETPCorr<0)
  inicioSeca<-which(periodoSeca>maisChuvoso)[1]
  return(periodoSeca[inicioSeca]-1)
}

"
@description classificao climatica do indice de umidade
@param IM indice de umidade
"
classifacaoIndiceUmidade<-function(IM){
  
  class<-character()
  if(IM>=100)
    class<-"super úmido A"
  else if(80<=IM & IM<100)
    class<-"úmido B4"
  else if(60<=IM & IM<80)
    class<-"úmido B3"
  else if(40<=IM & IM<60)
    class<-"úmido B2"
  else if(20<=IM & IM<40)
    class<-"úmido B1"
  else if(0<=IM & IM<20)
    class<-"subúmido C2"
  else if(-33.3<=IM & IM<0)
    class<-"subúmido seco C1"
  else if(-66.7<=IM & IM<(-33.3))
    class<-"semiárido D"
  else if(-100<=IM & IM<(-66.7))
    class<-"árido E"
  
  return(class)
}