12/6/2025 JK
All the shapefiles have come from the malariaAtlas R package:

# Admin1 shapefiles 
zmb_adm1 <- getShp(country = "Zambia", admin_level =  "admin1")
tza_adm1 <- getShp(country = "Tanzania", admin_level =  "admin1")

#st_write(zmb_adm1, "zmb_adm1.shp")
#st_write(tza_adm1, "tza_adm1.shp")

# Admin2 shapefiles 
zmb_adm1 <- getShp(country = "Zambia", admin_level =  "admin2")
tza_adm1 <- getShp(country = "Tanzania", admin_level =  "admin2")

#st_write(zmb_adm1, "zmb_adm2.shp")
#st_write(tza_adm1, "tza_adm2.shp")
