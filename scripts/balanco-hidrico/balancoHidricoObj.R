# Projeto: Modelagem Bioclimática do Alto Curso do Rio Paraíba
# Organização para o repositório (2026): Samuel Almeida Fragoso Vasconcelos

"
Copyright (c) 2017 Mônica Macêdo, Marcilio Lemos

@title  Objeto que armazena o resultado do calculo do balanço hídrico 

@author Mônica Macêdo and Samuel Almeida Fragoso Vasconcelos

@param local nome do local
@param lat valor de latitude
@param long valor de longitude
@param alt valor de altitude
@param CA  valor de capacidade de armazenamento do solo
@param periodo valor correspondente ao período no qual os dados foram coletados 
@param tabela dataframe contendo os calculos realizados no balanço hidrico
@param indUmid valor do indice de umidade
@param classIndUmid classificação de acordo com o indice de umidade
@param indTC valor do indice de termicidade compensada 
@param Am valor da amplitude termica anual
@param TP soma dos valores de temperatura positiva

"


BalancoHidricoObj <- function(local,lat,long,alt,CA,periodo,tabela,indUmid = NA,classIndUmid = NA,indTC = NA,Am = NA,TP= NA)
{
  
  me <- list(
    local = local,
    lat = lat,
    long = long,
    alt = alt,
    CA = CA,
    periodo = periodo,
    tabela = tabela,
    indUmid = indUmid,
    classIndUmid = classIndUmid,
    indTC = indTC,
    Am = Am,
    TP = TP
  )
  
  ## Set the name for the class
  class(me) <- append(class(me),"BalancoHidricoObj")
  return(me)
}