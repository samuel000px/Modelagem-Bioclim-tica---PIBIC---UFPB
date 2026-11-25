# Modelagem Bioclimática do Alto Curso do Rio Paraíba

Este projeto de iniciação científica (PIBIC) tem como objetivo aplicar o pacote `bioclim` do R à modelagem do Alto Curso do Rio Paraíba. A proposta explora uma aplicação inovadora da ferramenta, integrando os balanços hídrico e bioclimático em uma análise espacial. Ao documentar o código e as etapas do cálculo, o projeto busca oferecer à comunidade acadêmica um exemplo que estudantes e pesquisadores possam reproduzir e adaptar a outras áreas de estudo.

## Modelagem espacial com WorldClim

A análise usa as normais climatológicas mensais de temperatura média e precipitação do WorldClim 2.1, referentes ao período de 1970 a 2000. Cada variável tem 12 camadas, de janeiro a dezembro. O pacote `geodata` obtém os dados e o `terra` recorta os rasters conforme a delimitação da área de estudo no Alto Curso do Rio Paraíba. O script prevê resolução de 10 minutos de arco para testes e de 30 segundos de arco para a análise em maior detalhe.

Para cada célula, `bioclim::watbalRaster()` estima o balanço hídrico mensal a partir da precipitação, da evapotranspiração e da capacidade de armazenamento de água no solo. O cálculo fornece estimativas de armazenamento, déficit e excedente hídrico. Em seguida, `bioclim::biobalRaster()` usa esse balanço para calcular as intensidades bioclimáticas potencial, real, condicionada e livre.

O fluxo prevê a exportação de mapas em GeoTIFF, da delimitação da área de estudo em GeoPackage, de tabelas mensais em CSV e de gráficos em PDF, incluindo o diagrama ombrotérmico de Walter–Lieth.

## Balanço hídrico por local

O projeto também estuda os scripts de balanço hídrico de Thornthwaite e Mather. A versão de 2026 credita Mônica Macêdo e Samuel Almeida Fragoso Vasconcelos. Esses scripts usam arquivos CSV com temperatura, precipitação e características de cada local. A versão climatológica trabalha com 12 valores mensais de referência. A versão sequencial acompanha os meses de uma série de vários anos e usa as temperaturas climatológicas para calcular o índice de calor.

A revisão acompanha o código desde a leitura dos dados até o cálculo e a geração de tabelas e gráficos. Um dos pontos em estudo é o tratamento de séries inteiramente secas ou úmidas, em que o script sequencial pode usar variáveis antes de defini-las.

## Materiais de estudo e estado do projeto

Os materiais incluem exercícios de R, explicações sobre séries temporais e rasters mensais, estudo da documentação do `bioclim` no CRAN e um roteiro para executar a análise no RStudio.

O projeto está em desenvolvimento. A execução completa depende do arquivo que delimita a área de estudo no Alto Curso do Rio Paraíba. A conferência dos resultados numéricos e a justificativa de parâmetros como a capacidade de armazenamento de água no solo fazem parte da etapa de validação.
