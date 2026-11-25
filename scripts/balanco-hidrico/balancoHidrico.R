# Projeto: Modelagem Bioclimática do Alto Curso do Rio Paraíba
# Organização para o repositório (2026): Samuel Almeida Fragoso Vasconcelos

"
Copyright (c) 2017 Mônica Macêdo, Marcilio Lemos

@title Balanço hídrico de Thornthwaite and Mather

@author Mônica Macêdo and Samuel Almeida Fragoso Vasconcelos

@description Calcula o balanço hídrico climatológico a partir de séries mensais de precipitação e temperatura.

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
Local: nome do local
Mês: mês correspondente a medição de temperatura e precipitação 
Temperatura: temperatura (em ºC)
Precipitação: precipitação (em mm)
"

sourceDir <- getSrcDirectory(function(dummy) {dummy})

setwd(sourceDir)

source('utils.R')
source('balancoHidricoAux.R')
source('balancoHidricoObj.R')


library(stringr)

balancoHidrico<-function(localizacaoInfoFile,dataFile){
  
  localizacaoColunas=c("Local","Long","Lat","Alt","Periodo","CA","solo")
  dadosColunas=c("Local","Mes","T","P")
  
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
    
    qtMeses<-length(T) #meses considerados, para o climatologico sempre serão 12
    
    #evapotranpiração potencial
    ETP<-numeric(qtMeses)
    ETPCorr<-numeric(qtMeses)
    condicionante=T<26.5 
    I<-sum((T/5)^1.514) #índice anual de calor
    a<-(((0.675*I-77.1)*I + 17920)*I +492390) *0.000001
    ETP[condicionante]<-0.533*((10*T[condicionante]/I)^a)
    if(any(!condicionante)) #caso a temperatura não seja menor que 26.5 puxar da tabela
      ETP[!condicionante]<-tabelaETP(T[!condicionante])
    ETPCorr[condicionante]<-ETP[condicionante]*fatorCorrecaoETP(localizacao$Lat)[condicionante]
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
      
      k<-ultimoMesPeriodoChuvoso(PMenosETPCorr)
      negAcc[k]<-0
      ARM[k]<-CA
      sequencia<-c(k:qtMeses,1:(k-1))
      
      negAccK<-0
      aRMK<-0
      aRMAssumido<-CA
      
      repeat{
        for(i in 2:length(sequencia)){
          if(PMenosETPCorr[sequencia[i]]<=0){
            negAcc[sequencia[i]]<-negAcc[sequencia[i-1]]+PMenosETPCorr[sequencia[i]]
            ARM[sequencia[i]]<-CA*exp(negAcc[sequencia[i]]/CA)
          }else{
            ARM[sequencia[i]]<-pmin(ARM[sequencia[i-1]]+PMenosETPCorr[sequencia[i]],CA)
            negAcc[sequencia[i]]<-CA*log(ARM[sequencia[i]]/CA)
          }
        }
        if(PMenosETPCorr[k]<=0){
          negAccK<-negAcc[sequencia[length(sequencia)]]+PMenosETPCorr[k]
          aRMK<-CA*exp(negAccK/CA)
        }else{
          aRMK<-pmin(ARM[sequencia[length(sequencia)]]+PMenosETPCorr[k],CA)
          negAccK<-CA*log(aRMK/CA)
        }
        
        if(aRMK==aRMAssumido){
          break
        }
        
        else{
          ARM[k]<-aRMK
          negAcc[k]<-negAccK
          aRMAssumido<-aRMK
        }
        
      }# fim do repeat
      
      ALT<-c(ARM[1]-ARM[qtMeses],filter(ARM,c(1,-1)))[1:qtMeses]
      condicionante<-PMenosETPCorr>=0
      ETR[condicionante]<-ETPCorr[condicionante]
      ETR[!condicionante]<-P[!condicionante]+abs(ALT[!condicionante])
      condicionante<-PMenosETPCorr<0
      DEF[condicionante]<-ETR[condicionante]-ETPCorr[condicionante]
      condicionante<-ARM>=CA 
      EXC[condicionante]<-P[condicionante]-ETPCorr[condicionante]-ALT[condicionante]
    }#fim do caso 1
    
    #verifica se os calculso efetuados estão corretos
    if(round(sum(ALT))!=0)
      stop('o calculo do balanço hidrico falhou na seguinte condição: sum(ALT) = 0')
    if(round(abs(sum(DEF))+sum(ETR))!=round(sum(ETPCorr)))
      stop('o calculo do balanço hidrico falhou na seguinte condição: sum(ETPcorr) = sum(ETR)+sum(DEF)')
    if(round(sum(ETR)+sum(EXC))!=round(sum(P)))
      stop('o calculo do balanço hidrico falhou na seguinte condição: sum(P) = sum(ETR)+sum(EXC)')
    if(round(sum(ETPCorr)+sum(PMenosETPCorr))!=round(sum(P)))
      stop('o calculo do balanço hidrico falhou na seguinte condição: sum(P) = sum(ETPcorr)+sum(P-ETPcorr)')
    
    R<-numeric(qtMeses)
    DT<-numeric(qtMeses)
    RP<-numeric(qtMeses) #%R(P)
    
    condicionante<-PMenosETPCorr>=0 & ARM>=CA
    meses<-c(qtMeses,1:qtMeses) 
    mesCond<-which(condicionante)
    if(length(mesCond)!=0)
      R[condicionante]<-sapply(mesCond,function(x) 0.5*(EXC[meses[x+1]]+R[meses[x]]))
    
    DT<-sapply(1:qtMeses,function(x) ARM[meses[x+1]]+EXC[meses[x+1]]+0.5*R[meses[x]]-R[meses[x+1]])
    RP<-R/100
    
    #calculo dos indices
    IM<-100*(sum(PMenosETPCorr)/sum(ETPCorr)) #indice de umidade
    classIM<-classifacaoIndiceUmidade(IM)
    
    ITC<-numeric(0) #indice de termicidade compensado
    mesFrio<-min(T)
    Am<-max(T)-mesFrio
    IT<-10*(mean(T)+2*mesFrio)
    if(Am<9)
      ITC<-IT-9
    else if(9<= Am & Am<=18)
      ITC<-IT
    else if(Am>18)
      ITC<-IT+5*(Am-18)+10*(Am-21)+5*(Am-27)+20*(Am-46)
    
    tabela<-data.frame(Mes=c("Janeiro","Fevereiro","Março","Abril","Maio",
                               "Junho","Julho", "Agosto", "Setembro", "Outubro", 
                               "Novembro", "Dezembro","TOTAL/ANO"),T=c(round2(T),round2(mean(T))),
                              P=c(P,sum(P)),ETPcorr=c(round2(ETPCorr),round2(sum(ETPCorr))),
                              PMenosETPcorr=c(round2(PMenosETPCorr),round2(sum(PMenosETPCorr))),
                              ARM=c(round2(ARM),round2(sum(ARM))),ALT=c(round2(ALT),round2(sum(ALT))),
                              DEF=c(round2(DEF),round2(sum(DEF))),ETR=c(round2(ETR),round2(sum(ETR))),
                              EXC=c(round2(EXC),NA),R=c(round2(R),NA),DT=c(round2(DT),NA),RP=c(round2(RP),NA))
    
    bh<-BalancoHidricoObj(local = as.character(localizacao$Local), lat = localizacao$Lat, long = localizacao$Long,
                                   alt = localizacao$Alt, CA = localizacao$CA, periodo = as.character(localizacao$Periodo),
                                   tabela = tabela, indUmid = IM, classIndUmid = classIM,indTC = ITC, Am = Am, TP = sum(T[T>0]))
    result[[loc]]<-bh
  }#end for
  return(result)
}#end function

