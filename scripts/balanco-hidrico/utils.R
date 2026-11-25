# Projeto: Modelagem Bioclimática do Alto Curso do Rio Paraíba
# Versão: 2026

"
@title  Funções auxiliares: 

@author Mônica Macêdo and Samuel Almeida Fragoso Vasconcelos
"

"
@description Lê o conteúdo do arquivo, inserindo-o em um data frame
@param path  caminho do arquivo (e.g. /home/bob/file.csv).
@param hasHeader  indica se o arquivo possui cabeçalho.
"
loadDados<-function(path = NULL, hasHeader=TRUE){
  
  if(is.null(path)){
    stop('parâmetro @path é obrigatorio')
  }
  if(!file.exists(path)){
    stop('parâmetro @path: arquivo não encontrado')
  }
  data <- read.csv(path,header = hasHeader, na.strings=c("","NA"))
  
  return (data)
}

"
@description Verifica se existem dados invalidos ou omissos  
@param dados  data frame contendo os dados.
@param info   vetor contendo as informações que são mandatórias.
"
verificaDados<-function(dados,info){
  
  #verifica se o número de colunas está correto
  if (ncol(dados)!=length(info)){
    stop(paste('a seguinte informação é obrigatória:',info,'\n', collapse = NULL))
  }
  
  #verifica se alguma informação está faltando
  aux=which(is.na(dados), arr.ind = T)
  if(length(aux)!=0){
    rnames = rownames(dados)[aux[,1]]
    cnames = colnames(dados)[aux[,2]]
    stop(paste('valor inválido -> linha:',rnames,'coluna:',cnames,'\n', collapse = NULL))
  }
}
  
"
@description faz arrendondamento considerando duas casas decimais
@param IM x valor 
"  
round2<-function(x) {
  posneg = sign(x)
  z = abs(x)*10^2
  z = z + 0.5
  z = trunc(z)
  z = z/10^2
  z*posneg
}
