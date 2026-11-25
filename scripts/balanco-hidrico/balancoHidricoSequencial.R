# Projeto: Modelagem Bioclimática do Alto Curso do Rio Paraíba
# Versão: 2026

"
@title Balanço hídrico de Thornthwaite and Mather

@author Mônica Macêdo and Samuel Almeida Fragoso Vasconcelos

@description Calcula o balanço hídrico sequencial a partir de séries anuaus de precipitação e temperatura.

@param localizacaoInfoFile caminho do arquivo contendo informações de localização (e.g. /home/bob/locale.csv)
as informações de localização requeridas são as seguintes:
Local: nome do local
Longitude: valor de longitude
Latitude: valor de latitude
Altitude: valor de autitude 
Período dos dados: período no qual os dados foram coletados
Capacidade de armazenamento do solo: capacidade de armazenamento de água no solo
Tipo do solo: arenoso fino, franco-arenoso fino, franco-limosa, franco-argiloso, argiloso

@param dataFile caminho do arquivo contendo os dados de precipitação e temperatura. (e.g. /home/bob/data.csv)
os dados requeridos são os seguintes:
Ano: ano no qual o dado foi coletado
Local: nome do local
Mês: mês correspondente a medição de temperatura e precipitação 
Temperatura: temperatura (em ºC)
Precipitação: precipitação (em mm)

@param bhClimatNormal objeto contendo os resultados dos calculos do balanço hidrico normal 
"

sourceDir <- getSrcDirectory(function(dummy) {dummy})

setwd(sourceDir)

source('utils.R')
source('balancoHidricoAux.R')
source('balancoHidricoObj.R')


library(stringr)

balancoHidricoSequencial<-function(localizacaoInfoFile,dataFile,bhClimatNormal){
  
  localizacaoColunas=c("Local","Long","Lat","Alt","Periodo","CA","solo")
  dadosColunas=c("Ano","Local","Mes","T","P")
  
  localizacaoInfo<-loadDados(localizacaoInfoFile)
  colnames(localizacaoInfo) <- localizacaoColunas #padroniza o nome das colunas
  verificaDados(localizacaoInfo,localizacaoColunas)
  
  dadosSeries<-loadDados(dataFile)
  colnames(dadosSeries) <- dadosColunas #padroniza o nome das colunas
  verificaDados(dadosSeries,dadosColunas)
  
  
  result<-vector(mode = "list")
  
  for(loc in 1:length(localizacaoInfo[,1])){ #para cada local calcular o balanço hidrico de thornthwaite
    
    #verifica se foram fornecidos P e T do local
    localizacao<-localizacaoInfo[loc,]
    dadosPosto<-dadosSeries[grepl(paste("^",localizacao$Local,"$",sep=""),dadosSeries$Local),]
    if(nrow(dadosPosto)==0)
      stop(paste('não foram fornecidados os dados de temperatura e precipitação para o local',localizacao$Local))
    
    
    T<-as.numeric(str_trim(gsub(",", ".", dadosPosto$T)))
    P<-as.numeric(str_trim(gsub(",", ".", dadosPosto$P)))
    
    qtMeses<-length(T) 
    
    #pega as informações do balanço hidrico normal para o calculo do I
    bhNormalIndex<-which(sapply(bhClimatNormal, function(x) grepl(paste("^",as.character(localizacao$Local),"$",sep=""),x$local)));
    
    #evapotranpiração potencial
    ETP<-numeric(qtMeses)
    ETPCorr<-numeric(qtMeses)
    condicionante=T<26.5 
    I<-sum((bhClimatNormal[[bhNormalIndex]]$tabela$T[1:12]/5)^1.514) #índice anual de calor
    a<-(((0.675*I-77.1)*I + 17920)*I +492390) *0.000001
    ETP[condicionante]<-0.533*((10*T[condicionante]/I)^a)
    if(any(!condicionante)) #caso a temperatura não seja menor que 26.5 puxar da tabela
      ETP[!condicionante]<-tabelaETP(T[!condicionante])
    fatorCorrecao<-rep(fatorCorrecaoETP(localizacao$Lat),qtMeses/12)
    ETPCorr[condicionante]<-ETP[condicionante]*fatorCorrecao[condicionante]
    ETPCorr[!condicionante]<-ETP[!condicionante]
    
    #P-ETPCorr
    PMenosETPCorr<-P-ETPCorr
    
    negAcc<-numeric(qtMeses)
    ARM<-numeric(qtMeses)
    ALT<-numeric(qtMeses)
    DEF<-numeric(qtMeses)
    ETR<-numeric(qtMeses)
    EXC<-numeric(qtMeses)
    
    CA=localizacao$CA

    #caso 2
    if(all(PMenosETPCorr>=0)){
      negAcc<-rep(0,qtMeses)
      ARM<-rep(CA,qtMeses)
      ALT<-rep(0,qtMeses)
      DEF<-rep(0,qtMeses)
      ETR<-ETPCorr
      EXC<-PMenosETPCorr
    }else if(all(PMenosETPCorr<0)){ #caso 3
      negAcc<-rep('-',qtMeses)
      ARM<-rep(0,qtMeses)
      ALT<-rep(0,qtMeses)
      DEF<-(-PMenosETPCorr)
      ETR<-P
      EXC<-rep(0,qtMeses)
    }else{ # caso 1
    
      inicioPeriodoChuvoso<-which(PMenosETPCorr>=0)[1]
      mesesConsiderados<-rep(TRUE,qtMeses)
      mesesConsiderados[1:(inicioPeriodoChuvoso-1)]=FALSE
      
      #quando PMenosETP é positiva
      condicionante=PMenosETPCorr>=0 & mesesConsiderados
      negAcc[condicionante]<-0
      ARM[condicionante]<-CA
      
      #quando PMenosETP é negativa
      mesesCond<-which(PMenosETPCorr<0 & mesesConsiderados)
      if(mesesCond[1]==1){
        negAcc[1]<-PMenosETPCorr[1]
        ARM[1]<-CA*exp(negAcc[1]/CA)
        mesesCond[!mesesCond == 1]
      }
      for(x in mesesCond){
        negAcc[x]<-negAcc[x-1]+PMenosETPCorr[x]
        ARM[x]<-CA*exp(negAcc[x]/CA)
      }
      mesesCond<-inicioPeriodoChuvoso:qtMeses
      ALT[mesesCond]<-c(ARM[inicioPeriodoChuvoso]-ARM[qtMeses],filter(ARM[mesesCond],c(1,-1)))[1:length(mesesCond)]
      condicionante<-PMenosETPCorr>=0 & mesesConsiderados
      ETR[condicionante]<-ETPCorr[condicionante]
      condicionante<-PMenosETPCorr<0 & mesesConsiderados
      ETR[condicionante]<-P[condicionante]+abs(ALT[condicionante])
      condicionante<-PMenosETPCorr<0 & mesesConsiderados
      DEF[condicionante]<-ETR[condicionante]-ETPCorr[condicionante]
      condicionante<-ARM>=CA & mesesConsiderados
      EXC[condicionante]<-P[condicionante]-ETPCorr[condicionante]-ALT[condicionante]
      EXC[EXC<0]<-0
    }#fim do caso 1
    
    #verifica se os calculso efetuados estão corretos
    if(round(sum(ALT[mesesConsiderados]))!=0)
      stop('o calculo do balanço hidrico falhou na seguinte condição: sum(ALT) = 0')
    if(round(abs(sum(DEF[mesesConsiderados]))+sum(ETR[mesesConsiderados]))!=round(sum(ETPCorr[mesesConsiderados])))
      stop('o calculo do balanço hidrico falhou na seguinte condição: sum(ETPcorr) = sum(ETR)+sum(DEF)')
    #if(round(sum(ETR[mesesConsiderados])+sum(EXC[mesesConsiderados]))!=round(sum(P)))
     # stop('o calculo do balanço hidrico falhou na seguinte condição: sum(P) = sum(ETR)+sum(EXC)')
    if(round(sum(ETPCorr)+sum(PMenosETPCorr))!=round(sum(P)))
      stop('o calculo do balanço hidrico falhou na seguinte condição: sum(P) = sum(ETPcorr)+sum(P-ETPcorr)')
    
    R<-numeric(qtMeses)
    DT<-numeric(qtMeses)
    RP<-numeric(qtMeses) #%R(P)
    
    
    mesesCond<-which(PMenosETPCorr>=0 & ARM>=CA & mesesConsiderados)
    if(mesesCond[1]==1){
      R[1]<-0.5*EXC[1]
      mesesCond[!mesesCond == 1]
    }
    for(x in mesesCond){
      R[x]<-0.5*EXC[x]+R[x-1]
    }
    
    meses<-c(qtMeses,inicioPeriodoChuvoso:qtMeses) 
    DT[mesesConsiderados]<-sapply(2:(qtMeses-inicioPeriodoChuvoso+2),function(x) ARM[meses[x]]+EXC[meses[x]]+0.5*R[meses[x-1]]-R[meses[x]])
    RP<-R/100
    
    #calculo dos indices
    anosConsiderados<-unique(dadosPosto[1]);
    qtdAnos<-nrow(anosConsiderados)-1;
    
    if(inicioPeriodoChuvoso!=1){
      condicionante<-1:(inicioPeriodoChuvoso-1)
      ARM[condicionante]=NA
      ALT[condicionante]=NA
      DEF[condicionante]=NA
      ETR[condicionante]=NA
      EXC[condicionante]=NA
      R[condicionante]=NA
      DT[condicionante]=NA
      RP[condicionante]=NA
    }
    
    
    tabela<-data.frame(Mes=rep(c("Janeiro","Fevereiro","Março","Abril","Maio",
                                 "Junho","Julho", "Agosto", "Setembro", "Outubro", 
                                 "Novembro", "Dezembro"),nrow(anosConsiderados)),T=T,
                       P=P,ETPcorr=round2(ETPCorr),PMenosETPcorr=round2(PMenosETPCorr),
                       ARM=round2(ARM),ALT=round2(ALT),DEF=round2(DEF),ETR=round2(ETR),
                       EXC=round2(EXC),R=round2(R),DT=round2(DT),RP=round2(RP))
    
    bh<-BalancoHidricoObj(local = as.character(localizacao$Local), lat = localizacao$Lat, long = localizacao$Long,
                          alt = localizacao$Alt, CA = localizacao$CA, periodo = paste(min(anosConsiderados),max(anosConsiderados), sep = "-"),
                          tabela = tabela)
    result[[loc]]<-bh
  }#end for
  return(result)
}#end function

