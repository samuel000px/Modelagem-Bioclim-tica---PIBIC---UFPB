# Desenvolvimento de um pacote de modelagem bioclimática em R

Este projeto de iniciação científica (PIBIC/UFPB) tem como objetivo desenvolver um pacote em R para modelagem bioclimática. A proposta é reunir funções para preparar dados climáticos, calcular os balanços hídrico e bioclimático e apresentar os resultados em mapas, tabelas e gráficos. O pacote pretende oferecer à comunidade acadêmica um fluxo documentado e reproduzível, que estudantes e pesquisadores possam aplicar a diferentes áreas de estudo.

## Modelagem espacial com WorldClim

Os testes exploratórios usam as normais climatológicas mensais de temperatura média e precipitação do WorldClim 2.1, referentes ao período de 1970 a 2000. Cada variável tem 12 camadas, de janeiro a dezembro. O pacote `geodata` permite baixar os dados, e o `terra` permite recortar os rasters conforme a delimitação da área analisada. O fluxo de teste usa resolução de 10 minutos de arco para os primeiros ensaios e de 30 segundos de arco para análises em maior detalhe.

As funções `watbalRaster()` e `biobalRaster()` do pacote existente `bioclim` são referências para estudar o cálculo do armazenamento de água no solo, do déficit e do excedente hídrico e das intensidades bioclimáticas potencial, real, condicionada e livre.

O fluxo prevê a exportação de mapas em GeoTIFF, da delimitação da área de estudo em GeoPackage, de tabelas mensais em CSV e de gráficos em PDF, incluindo o diagrama ombrotérmico de Walter–Lieth.

## Balanço hídrico por local

O projeto também estuda os scripts de balanço hídrico de Thornthwaite e Mather. A versão de 2026 credita Mônica Macêdo e Samuel Almeida Fragoso Vasconcelos. Esses scripts usam arquivos CSV com temperatura, precipitação e características de cada local. A versão climatológica trabalha com 12 valores mensais de referência. A versão sequencial acompanha os meses de uma série de vários anos e usa as temperaturas climatológicas para calcular o índice de calor.

A revisão acompanha o código desde a leitura dos dados até o cálculo e a geração de tabelas e gráficos. Um dos pontos em estudo é o tratamento de séries inteiramente secas ou úmidas, em que o script sequencial pode usar variáveis antes de defini-las.

## Estado do desenvolvimento

O repositório reúne os [scripts de balanço hídrico](scripts/balanco-hidrico/) que servem de ponto de partida. O trabalho em andamento inclui organizar as funções na estrutura do pacote, documentar as entradas e as saídas e validar os resultados com dados de referência. A capacidade de armazenamento de água no solo é um dos parâmetros a avaliar nessa etapa.
