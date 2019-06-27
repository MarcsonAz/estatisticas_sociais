setwd("/mnt/publica/ESTATISTICA ESPACIAL")
getwd()
library(rgdal)
rio<-readOGR("RJ_Mun97_region.shp")
plot(rio)
## Informações gerais sobre o shapefile
class(rio)
names(rio)
head(rio@data)
#------------------------------------
## Lendo o arquivo de dados
## Esses dados foram simulados representam a venda do produto H e a renda dos municípios do estado do RJ
dados_aux <- read.table("dados.txt", header=TRUE)

head(dados_aux)
#classe dos dados
class(dados_aux)
#representar as vendas no mapa do rio
#mais escuro mais vendas
#montamos os dados de área
# passo 1:
# vincular ao shapefile
## Associando os munícipios do Rio de Janeiro a 
#satisfação media e número de produtos H vendidos  ---- USANDO a FUNÇÃO MERGE ----
## As informações são relacionadas a partir do 
#código único para cada município (coluna CODMUN6 
#no objeto rio e coluna Cod no objeto dados_aux)
dados_rio <- merge(rio, dados_aux, by.x = "CODMUN6", by.y = "Cod")
# by.x = "CODMUN6", by.y = "Cod") nome da variável que tem mesmos dados
head(dados_rio@data)
dim(rio@data)
dim(dados_rio@data)

#Conventendo em projções espaciais
dados_rio <- spTransform(dados_rio, CRS("+proj=longlat +datum=WGS84 +no_defs"))

#Carregando os pacotes RColorBrewer e sp
library(RColorBrewer)
library(sp)

# Mapas coropléticos

#  Renda - mapa coroplético

#Criando os intervalos com base em quantis
intervalos=quantile(dados_rio$Venda_1SEM, probs = seq(0,1,0.125))

#Plotando o mapa com tons avermelhados
spplot(dados_rio,c("Venda_1SEM"),at=intervalos,col.regions =brewer.pal(8, "Dark2")) 
#Outras opções de cores: Greens, BrBG, Accent

#Criando os intervalos de mesmo tamanho
ampli_dados = max(dados_rio$Venda_1SEM) - min(dados_rio$Venda_1SEM)
k = 6 #número de classes + 1
ampli_classe = ampli_dados/k
intervalos2 = NULL
intervalos2[1] = min(dados_rio$Venda_1SEM)
for(i in 2:k){
  intervalos2[i] = intervalos2[i-1] + ampli_classe    
}

#Plotando o mapa com tons azulados
spplot(dados_rio,c("Venda_1SEM"),at=intervalos2,col.regions =brewer.pal(5, "Blues"))


# Comparando dois mapas-----------------------------------------------------

#Criando os intervalos com base em quantis
intervalos3=quantile(c(dados_rio$Venda_1SEM,dados_rio$Venda_2SEM), probs = seq(0,1,0.125))

#Plotando o mapa com tons alaranjados
spplot(dados_rio,c("Venda_1SEM","Venda_2SEM"),at=intervalos3,col.regions =brewer.pal(8,
                   "Oranges"),names.attr = c("1 Semestre","2 Semestre"))

# juntando com o mapa completo-----------------------------------------------------
#Carregabndo o pacote plotGoogleMaps
library(plotGoogleMaps)

# Pacote plotGoogleMaps
# http://www2.uaem.mx/r-mirror/web/packages/plotGoogleMaps/vignettes/plotGoogleMaps-intro.pdf

#Criando um dataframe com os dados Venda_1SEM, Satisfacao e nome do município sem acento
dados_google <- dados_rio[,c("Venda_1SEM", "Satisfacao", "SEM_ACENTO")]

#Modificando o nome das variáveis
names(dados_google) <- c("Venda (1 Semestre)", "Satisfacao com o Produto", "Municipio")

#Visualizando os dados
head(dados_google)

#Plotando o mapa do número de vendas no 1 semestre
map <- plotGoogleMaps(dados_google, zcol = "Venda (1 Semestre)", zoom = 8, fitBounds = F,
                      filename = "Map_GoogleMaps.html", layerName = "Vendas do Produto no 1 Semestre",
                      colPalette = brewer.pal(5, "Oranges"), mapTypeId = "TERRAIN") 
#Outras opções de mapTypeId: HYBRID, SATELLITE, TERRAIN


#Adicionando a Satisfacao
map2 <- plotGoogleMaps(dados_google, zcol = "Venda (1 Semestre)", zoom = 8,
                       fitBounds = F, filename = "Map_GoogleMaps2.html", 
                       layerName = "Vendas do Produto no 1 Semestre",
                       colPalette = brewer.pal(5, "Blues"), add=TRUE)

map3 <- plotGoogleMaps(dados_google, zcol = "Satisfacao com o Produto", 
                       zoom = 8, fitBounds = F, filename = "Map_GoogleMaps3.html", 
                       layerName = "Satisfacao com o Produto H", 
                       colPalette = brewer.pal(5, "YlOrRd"), 
                       previousMap = map2)
#isto irá gerar somente um gráfico


#estudando padrões de pontos
#estudando planta rara
library(rworldmap)
library(RgoogleMaps)
library(googleVis)

# Leitura dos dados
laurus = read.table("laurus.txt", header=TRUE)

#Visualizando os dados
head(laurus)

#Transformar o data frame com os dados em um objeto espacial
coordinates(laurus) <- c("lon", "lat")

# Definir projeção espacial
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  
# geographical, datum WGS84
proj4string(laurus) <- crs.geo


#Verificando a classe do objeto    
class(laurus)

# Mapa das ocorrências
mapa_mundo <- getMap()
plot(laurus, pch = 19, cex = 0.2, col = "blue")
plot(mapa_mundo, add = TRUE)

# Número de ocorrências por país
table(laurus$country)

# Laurus nobilis no Reino Unido
laurus_uk = subset(laurus, country == "United Kingdom")


# Mapa da distribuição das ocorrências no Reino unido
plot(laurus_uk, pch = 19, cex = 0.2, col = "blue")
plot(mapa_mundo, add=TRUE)


# Mapa usando RgoogleMaps

laurus_uk_coords <- as.data.frame(coordinates(laurus_uk))

mapa_rgm1 <- GetMap(center = c(54, -3), zoom = 6, maptype = "terrain")
# "roadmap", "satellite", "terrain", "hybrid", "mapmaker-roadmap", 
#"mapmaker-hybrid" 

PlotOnStaticMap(mapa_rgm1, lat = laurus_uk_coords$lat, 
                lon = laurus_uk_coords$lon, 
                cex = 0.5, pch = 19, col = "blue", FUN = points)

#estabelecendo vizinhanças--------------------------------------------------------

library(spdep)

## Descobrindo os vizinhos de cada região
vizinhos <- poly2nb(rio, queen = TRUE)
vizinhos
## Neighbour list object:
## Number of regions: 91 
## Number of nonzero links: 444 
## Percentage nonzero weights: 5.361671 
## Average number of links: 4.879121


#Matriz de vizinhanças
plot(rio)
plot(vizinhos, coordinates(rio), add=T, pch = 19)

## Matriz de vizinhanças (W)
W_rio <- nb2mat(vizinhos, style = "B")   # outras opções de estilo: W
colnames(W_rio) <- rownames(W_rio)

head(W_rio)

#buscando a quantidade de vizinhos dentro de cada vizinhança
apply(W_rio,2,sum)
#vizinhança com mais vizinhos
max(apply(W_rio,2,sum))

#Carregando pacote
library(leaflet)

## Carregando conjunto de dados
## Localizações e concentrações de metais pesados da camada superficial, 
#juntamente com uma série de variávies de solo e 
## paisagem nos locais de observação, numa planície de inundação do rio Meuse.
data("meuse")
class(meuse)
## Carregando limites do rio Meuse
data(meuse.riv)

## Informações gerais sobre o conjunto de dados
names(meuse)
head(meuse)
#estrutura dos objetos
str(meuse)
# resumo
summary(meuse)


## Transformando os dados em um objeto espacial
coordinates(meuse)<-c("x","y")# latitudes e longitudes
class(meuse)

# Agora podemos começar a plotar
## Mapas exploratórios
plot(meuse, asp = 1, pch = 1)
lines(meuse.riv)

# Mapa 1 (dados originais para facilitar a visualização das diferenças 
#entre localizações)
plot(meuse, asp = 1, cex = (4 * meuse$zinc/max(meuse$zinc)), pch = 1)
#no cex, temos para mostrar as quantidades de zinco
lines(meuse.riv)


# Mapa 2
meuse_proj = meuse
proj4string(meuse_proj) <- CRS("+init=epsg:28992")
z_mapa2 <- plotGoogleMaps(meuse_proj, filename='zinco_mapa2.htm', 
                        zcol = "zinc", colPalette = brewer.pal(5, "Dark2"))
# Mapa 3
z_bb_mapa3 <- bubbleGoogleMaps(meuse_proj, zcol='zinc', max.radius = 80, 
                               filename='zinco_mapa3.htm')


# Georeferencia
#carregando pacote
library(ggmap)

#Georreferenciando um endereço-----------------------------------

geocode("Universidade Federal Fluminense")

geocode("Universidade Federal Fluminense", output = "latlona")

geocode("Universidade Federal Fluminense", output = "more")

mapImageData1 <- get_map(location = c(lon = -43.11672, lat = -22.90303),
                         color = "color",
                         source = "google",
                         maptype = "satellite",
                         zoom = 17)

ggmap(mapImageData1,
      extent = "device",
      ylab = "Latitude",
      xlab = "Longitude")



#testandominha casa
geocode("Rua Palmiro Alves,395")
geocode("Rua Palmiro Alves,395",output = "latlona")
laemcasa<-get_map(location=c(lon = -43.47192, lat = -22.88875),
                  color = "color",
                  source = "google",
                  maptype = "satellite",
                  zoom = 17)

#testando casa da mamá
geocode("Rua Paulo Vidal,330")
geocode("Rua Paulo Vidal,330",output = "latlona")
laemcasamo<-get_map(location=c(lon = -43.43335 , lat = -22.89426),
                  color = "color",
                  source = "google",
                  maptype = "satellite",
                  zoom = 17)
ggmap(laemcasamo,
      extent = "device",
      ylab = "Latitude",
      xlab = "Longitude")
