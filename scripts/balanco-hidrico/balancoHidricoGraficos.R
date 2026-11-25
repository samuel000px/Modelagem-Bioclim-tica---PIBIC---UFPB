# Projeto: Modelagem Bioclimática do Alto Curso do Rio Paraíba
# Versão: 2026

require(ggplot2)
require(RColorBrewer)
require(reshape2)

sourceDir <- getSrcDirectory(function(dummy) {dummy})

setwd(sourceDir)


balancoHidricoGraficos<-function(bhObj){
  
  #diretório onde serão salvos os gráficos
  dirName<-paste(getwd(),'/balanco_hidrico_graficos/',sep = "")
  if(!file.exists(dirName))
    dir.create(dirName)
  
  for(loc in 1:length(bhObj)){
    
    bh<-bhObj[[loc]]
    tabela<-bh$tabela[-nrow(bh$tabela),]
    retirada<-numeric(12)
    reposicao<-numeric(12)
    condicionante<-tabela$ALT>=0
    retirada[!condicionante]<-tabela$ALT[!condicionante]
    reposicao[condicionante]<-tabela$ALT[condicionante]
  
    dados<-data.frame(Mes=c("Jan","Fev","Mar","Abr","Mai",
                            "Jun","Jul", "Ago", "Set", "Out", 
                            "Nov", "Dez"), 
                            Deficiencia=tabela$DEF,
                            Excedente=tabela$EXC,
                            Retirada=retirada,
                            Reposição=reposicao)
  
    dados$Mes<-factor(dados$Mes, levels = dados$Mes)
    
    colnames(dados)<-c("Mês","Deficiência","Excedente",
                       "Retirada","Reposição")
    
    dat.m <- melt(dados,id.vars = "Mês")
    
    colnames(dat.m)<-c("Mês","Legenda","mm")
    
    ggplot(dat.m, aes(x = Mês, y = mm, fill = Legenda))+geom_bar(stat = "identity")+
    theme(axis.line = element_line())+
    scale_fill_manual( values = c('#CC3333','#66CCCC','#FF6666','#66CC66'))
    ggsave(paste(dirName,gsub("/","_",gsub(" ","_",bh$local)),".png",sep = ""))
    
  }
}