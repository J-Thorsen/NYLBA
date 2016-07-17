library( maptools )
library( sp )
library(leaflet)
library(htmltools)
library(dplyr)

##load us county shape files
setwd("C:/Users/jthor_000/Desktop/working directory")
UsCounty<-readShapePoly(fn="tl_2015_us_county.shp", proj4string=CRS("+proj=longlat +datum=WGS84") )
#subset NYS county shapefiles
NyCounty<-UsCounty[UsCounty$STATEFP==36,]
#LB Counties/City Fills



#BEINC
Erie<-NyCounty[NyCounty$NAME=="Erie",]
Niagara<-NyCounty[NyCounty$NAME=="Niagara",]
Townawanda<-readShapePoly(fn="City_of_Tonawanda.shp", proj4string=CRS("+proj=longlat +datum=WGS84") )
Lackawanna<-readShapePoly(fn="City_of_Lackawanna.shp", proj4string=CRS("+proj=longlat +datum=WGS84") )
Buffalo<-readShapePoly(fn="City_of_Buffalo.shp", proj4string=CRS("+proj=longlat +datum=WGS84") )
#Chautauqua
Chautauqua<-NyCounty[NyCounty$NAME=="Chautauqua",]

#Cattaraugus
Cattaraugus<-NyCounty[NyCounty$NAME=="Cattaraugus",]
#Allegany
Allegany<-NyCounty[NyCounty$NAME=="Allegany",]
#Seneca
Seneca<-NyCounty[NyCounty$NAME=="Seneca",]
#Broome
Broome<-NyCounty[NyCounty$NAME=="Broome",]
#Oswego
Oswego<-NyCounty[NyCounty$NAME=="Oswego",]
#Onondaga
Onondaga<-NyCounty[NyCounty$NAME=="Onondaga",]
Syracuse<-readShapePoly(fn="City_of_Syracuse.shp", proj4string=CRS("+proj=longlat +datum=WGS84") )
#Albany
Albany<-NyCounty[NyCounty$NAME=="Albany",]
#Troy
Troy<-readShapePoly(fn="City_of_Troy.shp", proj4string=CRS("+proj=longlat +datum=WGS84") )
#Schenectady
Schenectady<-NyCounty[NyCounty$NAME=="Schenectady",]
City.Sch<-readShapePoly(fn="City_of_Schenectady.shp", proj4string=CRS("+proj=longlat +datum=WGS84") )
Amsterdam<-readShapePoly(fn="City_of_Amsterdam.shp", proj4string=CRS("+proj=longlat +datum=WGS84") )

#Newburgh
Newburgh<-readShapePoly(fn="City_of_Newburgh.shp", proj4string=CRS("+proj=longlat +datum=WGS84") )

#Nassau
Nassau<-NyCounty[NyCounty$NAME=="Nassau",]

#Suffolk
Suffolk<-NyCounty[NyCounty$NAME=="Suffolk",]
#Rochester
Rochester<-readShapePoly(fn="City_of_Rochester.shp", proj4string=CRS("+proj=longlat +datum=WGS84") )

CountyNames<-data.frame(NyCounty)
CountyNames<-tbl_df(CountyNames)


#NY Land Bank Location Data
NYLB<-read.csv("NYS Land Bank locations.csv")

#Markers with Popup Links
popBuff<-paste(sep="<br/>","<b><a href='http://www.benlic.org'>Buffalo Erie Niagara Land Improvement Corporation</a></b>")
popAlbany<-paste(sep="<br/>","<b><a href='http://www.albanycountylandbank.org'>Albany County Land Bank Corporation</a></b>")
popBroome<-paste(sep="<br/>","<b><a href='http://www.Broomelandbank.org'>Broome County Land Bank Corporation</a></b>")
popCapital<-paste(sep="<br/>","<b><a href='http://capitalregionlandbank.com'>Capital Region Land Bank</a></b>")
popChaut<-paste(sep="<br/>","<b><a href='http://chautauqualandbank.org'>Chautauqua County Land Bank Corporation</a></b>")
popSyr<-paste(sep="<br/>","<b><a href='http://syracuselandbank.org'>Greater Syracuse Land Bank</a></b>")
popNew<-paste(sep="<br/>","<b><a href='http://newburghcommunitylandbank.org'>Newburgh Community Land Bank</a></b>")  
popOsw<-paste(sep="<br/>","<b>Owsego County Land Bank</a></b>")
popRoc<-paste(sep="<br/>","<b><a href='http://www.cityofrochester.gov/landbank/'>Rochester Land Bank Corporation</a></b>")  
popTro<-paste(sep="<br/>","<b><a href='http://www.troycommunitylandbank.org/'>Troy Community Land Bank</a></b>")  
popFin<-paste(sep="<br/>","<b>Finger Lakes Land Bank (seneca County)</a></b>") 
popSuf<-paste(sep="<br/>","<b><a href='http://www.suffolkcountylandbank.org/'>Suffolk Count Land Bank Corporation</a></b>")
popNas<-paste(sep="<br/>","<b>Nassau County Land Bank</a></b>")
popCat<-paste(sep="<br/>","<b>Cattaraugus County Land Bank</a></b>")
popAll<-paste(sep="<br/>","<b>Allegany Count Land Bank</a></b>")
#Plot Map
NyState<- 
  leaflet()%>% 
  addProviderTiles("CartoDB.Positron", tileOptions(minZoom=10, maxZoom=17))%>%
  setView(lng=-76, lat=43, zoom=7)%>%
  addPolygons(data=NyCounty, stroke = .2,color= "#bababa",  weight= 1, fillOpacity = 0)%>%
  addPolygons(data=Erie, stroke=.2, weight= .3,fillColor = "#0571b0", fillOpacity = .5)%>%
  addPolygons(data=Buffalo, stroke=.2, weight= .3, fillColor= "#ca0020", fillOpacity = .5)%>%
  addPolygons(data=Niagara, stroke=.2, weight= .3, fillColor = "#0571b0",  fillOpacity = .5)%>%
  addPolygons(data=Townawanda, stroke=.2, weight= .3, fillColor= "#ca0020", fillOpacity = .5)%>%
  addPolygons(data=Lackawanna, stroke=.2, weight= .3, fillColor= "#ca0020", fillOpacity = .5)%>%
  addPolygons(data=Chautauqua, stroke=.2, weight= .3, fillColor="#f4a582", fillOpacity = .5)%>%
  addPolygons(data=Cattaraugus, stroke=.2, weight= .3,  fillColor="#92c5de", fillOpacity = .5)%>%
  addPolygons(data=Allegany, stroke=.2, weight= .3, fillColor="#ca0020", fillOpacity = .5)%>%
  addPolygons(data=Seneca, stroke=.2, weight= .3, fillColor="#92c5de", fillOpacity = .5)%>%
  addPolygons(data=Broome, stroke=.2, weight= .3, fillColor= "#f4a582", fillOpacity = .5)%>%
  addPolygons(data=Oswego, stroke=.2, weight= .3, fillColor="#ca0020", fillOpacity = .5)%>%
  addPolygons(data=Onondaga, stroke=.2, weight= .3, fillColor="#0571b0", fillOpacity = .5)%>%
  addPolygons(data=Syracuse, stroke=.2, weight= .3, fillColor="#ca0020", fillOpacity = .5)%>%
  addPolygons(data=Albany, stroke=.2, weight= .3, fillColor="#f4a582", fillOpacity = .5)%>%
  addPolygons(data=Troy, stroke=.2, weight= .3, fillColor="#92c5de", fillOpacity = .5)%>%
  addPolygons(data=Schenectady, stroke=.2, weight= .3, fillColor="#0571b0",fillOpacity = .5)%>%
  addPolygons(data=City.Sch, stroke=.2, weight= .3,  fillColor="#ca0020",fillOpacity = .5)%>%
  addPolygons(data=Amsterdam, stroke=.2, weight= .3,  fillColor="#ca0020",fillOpacity = .5)%>%
  addPolygons(data=Newburgh, stroke=.2, weight= .3, fillColor="#0571b0",fillOpacity = .5)%>%
  addPolygons(data=Nassau, stroke=.2, weight= .3, fillColor="#f4a582", fillOpacity = .5)%>%
  addPolygons(data=Suffolk, stroke=.2, weight= .3, fillColor="#92c5de", fillOpacity = .5)%>%
  addPolygons(data=Rochester, stroke=.2, weight= .3, fillColor="#ca0020", fillOpacity = .5)%>%
  addCircles(data=NYLB, lng=-78.87650, lat=42.88434, color="black", popup=popBuff)%>%
  addCircles(data=NYLB, lng=-73.75846, lat=42.65852, color="black", popup=popAlbany)%>%
  addCircles(data=NYLB, lng=-75.91075, lat=42.09695, color="black", popup=popBroome)%>%
  addCircles(data=NYLB, lng=-73.93959, lat=42.81433, color="black", popup=popCapital)%>%
  addCircles(data=NYLB, lng=-79.23314, lat=42.09448, color="black", popup=popChaut)%>%
  addCircles(data=NYLB, lng=-76.14632, lat=43.04911, color="black", popup=popSyr)%>%
  addCircles(data=NYLB, lng=-74.01206, lat=41.50101, color="black", popup=popNew)%>%
  addCircles(data=NYLB, lng=-76.51125, lat=43.45559, color="black", popup=popOsw)%>%
  addCircles(data=NYLB, lng=-77.61492, lat=43.15722, color="black", popup=popRoc)%>%
  addCircles(data=NYLB, lng=-73.68693, lat=42.73798, color="black", popup=popTro)%>%
  addCircles(data=NYLB, lng=-76.84366, lat=42.90826, color="black", popup=popFin)%>%
  addCircles(data=NYLB, lng=-73.22420, lat=40.82136, color="black", popup=popSuf)%>%
  addCircles(data=NYLB, lng=-73.64578, lat=40.71521, color="black", popup=popNas)%>%
  addCircles(data=NYLB, lng=-78.86662, lat=42.32925, color="black", popup=popCat)%>%
  addCircles(data=NYLB, lng=-78.03445, lat=42.22313, color="black", popup=popAll)
NyState
