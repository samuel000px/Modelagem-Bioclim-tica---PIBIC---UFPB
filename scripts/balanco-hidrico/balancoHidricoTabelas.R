# Projeto: Modelagem Bioclimática do Alto Curso do Rio Paraíba
# Organização para o repositório (2026): Samuel Almeida Fragoso Vasconcelos

library(gridExtra)
library(grid)
library(gtable)
source('utils.R')

sourceDir <- getSrcDirectory(function(dummy) {dummy})

setwd(sourceDir)

balancoHidricoTabelas<-function(bhObj){
  
  
  #diretório onde serão salvos as tabelas
  dirName<-paste(getwd(),'/balanco_hidrico_tabelas/',sep = "")
  if(!file.exists(dirName))
    dir.create(dirName)
  
  for(loc in 1:length(bhObj)){
    
    bh<-bhObj[[loc]]
    
    tabela<-bh$tabela
    colnames(tabela) <- c("Mês","T","P","ETPcorr","P-ETPcorr","ARM","ALT","DEF","ETR",
                          "EXC", "R","DT","%R(P)")
    tt <- ttheme_default( core = list(fg_params=list(cex = 0.8), padding=unit(rep(4,4),"mm")),
                          colhead = list(fg_params=list(cex = 0.9)),
                          rowhead = list(fg_params=list(cex = 0.9)))
    g <- tableGrob(tabela, theme=tt, rows=NULL)
    ids <- find_cells(g, 1:13, rep(0,13), "core-fg")
    g <- modify_cells(g, ids, gpar(cex = 0.8,fontface="bold"))
    
    str<-paste("BALANÇO HÍDRICO SEGUNDO THORNTHWAITE e MATHER (1957)\n\n\n\nLocal: ",
               bh$local,"\n","Lat.: ",bh$lat,"         Long.: ",bh$long,"         Alt.: ",
               bh$alt,"\n","Capacidade de Armazenamento do Solo (CA) =  ",bh$CA," mm",
               "\n","Período dos dados: ",bh$periodo,"\n\n",
               sep = "")
    
    header <- textGrob(str,gp=gpar(fontsize=12, fontface="bold"))
    padding <- unit(0.5,"line")
    g <- gtable_add_rows(g, 
                         heights = grobHeight(header) + padding,
                         pos = 0)
    
    str<-paste("\nÍndice de Umidade: ",round2(bh$indUmid)," (",bh$classIndUmid,")",
               "          Índice de Termicidade Compensado: ",round2(bh$indTC),
               "\nAmplitude Termica Anual: ",bh$Am,
               "                            Temperatura Positiva: ",
               bh$TP,
               sep = "")
    
    footnote <- textGrob(str, x=0, hjust=0,
                         gp=gpar( fontface="italic"))
    g <- gtable_add_rows(g, heights = grobHeight(footnote)+ padding)
    g <- gtable_add_grob(g, list(header, footnote),
                         t=c(1, nrow(g)), l=c(1,2), 
                         r=ncol(g))
    
    fileName<-paste(dirName,gsub("/","_",gsub(" ","_",bh$local)),".pdf",sep = "")
    pdf(fileName, height=8, width=10.5)#height=8, width=10.5
    grid.draw(g)
    dev.off()
    
  }
}

find_cells <- function(table, row, col, name="core-fg"){
  l <- table$layout
  unlist(Map(function(r, c) which(((l$t-1) == r) & ((l$l-1) == c) & (l$name == name)), row, col))
}

modify_cells <- function(g, ids, gp=gpar()){
  for(id in ids) g$grobs[id][[1]][["gp"]] <- gp
  return(g)
}
