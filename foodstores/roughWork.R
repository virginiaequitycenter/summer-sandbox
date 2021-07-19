library(tidyverse)
library(jsonlite)
library(sf)
library(leaflet)

write_csv(stores_4326, path = "rawCvilleStores.csv")   # so i can split screen / make counting easier

included <- stores_4326[c(7, 10, 12, 24:25, 27, 34:36, 39:43, 55:59, 62, 92, 94:96, 
                          99, 114, 123, 127:128, 131, 133:135, 137:138, 144:147,
                          150:154, 159:163, 165, 167, 179:180, 185:186, 190, 199:201,
                          210:211, 213, 219, 221, 224:225, 227, 231, 236:237, 247:248,
                          251, 254, 258:264, 281:282, 285:288, 290:291, 294, 
                          298:300, 304, 306, 309, 311, 320, 325:327, 330:331, 339,
                          356, 361:362, 365, 367, 369:370, 373, 388:391, 395:396,
                          398, 400:402, 404, 406, 410, 424:425, 428:429, 432, 
                          436, 442, 449), ] 

#58-59: international food grocery stores so not sure if they fit criteria of "wide variety" of healthy foods, but i think they do so going to include
#92: food depot -- google search is a bit ambigiuous but looks like a grocery store
#96: INTERNATIONAL FOODS -- again not a "wide variety" but i think it still fits the bill
#127: african market place -- same issue
#138: not sure about Rebecca's Natural Food-- definitely not a large grocery store but it does provide healthy food options?
#161: same issue ^
#163: kosher/halal foods only, but still a grocery store with variety
#179: furniture and outlet store, but also has wide variety of groceries available
#185-186: international food stores...
#231: Stuarts Draft Farm Market-- farmers market but has fairly accessible hours: 9am-6pm daily except sunday, 12pm-5pm
#294: international food market...
#300: outside a gas station but seems to still be a grocery store (though a small) (according to groupon)
#311: C'ville Oriental-- international food market...
#326: The Ole Country Store and Bakery -- more general store than grocery store but does have a "wide variety..."
#395: actually called Riverside Grocery
#442: wholesale restaurant provider, but not an exclusive club so including
#449: ambiguous but has grocery in the name

excluded <- stores_4326[c(1:6, 8:9, 11, 13:23, 26, 28:33, 37:38, 44:54, 60:61, 
                          63:91, 93, 97:98, 100:113, 115:122, 124:126, 129:130, 
                          132, 136, 139:143, 148:149, 155:158, 164, 166, 168:178, 
                          181:184, 187:189, 191:198, 202:209, 212, 214:218, 220,
                          222:223, 226, 228:230, 232:235, 238:246, 249:250, 252:253,
                          255:257, 265:280, 283:284, 289, 292:293, 295:297, 301:303,
                          305, 307:308, 310, 312:319, 321:324, 328:329, 332:338,
                          340:355, 357:360, 363:364, 366, 368, 371:372, 374:387,
                          392:394, 397, 399, 403, 405, 407:409, 411:423, 426:427,
                          430:431, 433:435, 437:441, 443:448, 450:455), ]
  
#1: not open/running according to https://rvagriculture.org/ that runs the market
#2: actually named "ZACHS COUNTRY STORE" and is a gas station
#13: goochland farmers market only open tuesdays 4-6.30pm -- not accessible
#26: culpeper renaissance is similar to our downtown mall -- definitely not a grocery store, more outside dining
#37: bakery -- not a "wide variety of healthy foods"
#47: scottsville farmers market only open 9am-1pm on saturdays -- not accessible
#72-74, 81: delis?
#78: bakery
#94: deli
#112: greene farmers market only open sat 8am-12pm -- not accessible
#130: Second Street Farmers Market only open thurs 3.30pm-5.30pm -- not accessible
#132: meating place is a butcher -- not a "wide variety"      # could be included though?
#175,194: truck stop
#195: permanently closed
#197: farmers market-- inaccessible hours
#217: i thought The Cheese Shop might be too specific to include, but they have a lot more than just cheese -- spices, candies, jams, etc, so not really sure if this meets the "wide variety of healthy foods" criteria?
#223: 3b grocery actually a convenience store inside a gas station
#308: Harrisonburg Farmers Market -- inaccessible hours
#310: afghan grand market-- consider it more a restaurant than a market/grocery store
#312: Mineral Farmer's Market -- inaccessible hours
#328: North Augusta Farmers Market ^
#337: Waynesboro Farmers Market ^^
#345: slaughterhouse -- !"wide variety"
#350: inacessible farmers market
#354: frozen food store -- !wide variety
#376: wouldn't consider big lots a grocery store-- don't think they have non-perishables
#383: hours only Thursday	4–8PM and Saturday	8AM–12PM -- wouldn't consider this easily accessible? 
#393: more restaurant than grocery
#418: ambiguous --listed as department store though so i think exclude
#441: Staunton Farmers' Market-- inaccessible
#453: inaccessible hours

# if not mentioned by row specifically, it fell into category of convenience, dollar store, gas station, etc.

# Included Key Words (shorter than excluded key words)

keyWords <- c("lion", "Super", "Friendly city", "Stokesville", "Buenavista", "Kroger", "Whole Foods",
              "Safeway", "Bryant’s", "Walmart", "Hilltop", "Walgreens", "Tio", "Tienda", "Bridgwater", 
              "Depot", "International", "Eastside", "Chaparro", "African", "Trader", "Beginning",
              "Natural", "Millers", "Schwans", "Sharp", "Teeter", "Martins", "Integral", "Sweets",
              "Valu", "Finders", "Asian", "Weis", "Target", "Aldi", "H Mart", "Medina", "Draft Farm",
              "Oriental", "Rose", "Market Street", "Blue", "Thomas inc", "Sinbad", "Latin", 
              "Keystone", "Caul's", "World", "Oriental", "Ole Country", "Yoder's", "Village market",
              "Indian", "Dona", "Giant", "Belmont", "Mira", "Lidl", "d’amores", "Wegmans", 
              "Sysco", "Express grocery")

keyWords <- c("lion|Super|Friendly city|Stokesville|Buenavista|Kroger|
              |Safeway|Bryant|Walmart|Hilltop|Walgreens|Tio|Tienda|Whole Foods|
              |Depot|International|Eastside|Chaparro|African|Trader|Beginning|
              |Natural|Millers|Schwans|Sharp|Teeter|Martins|Integral|Sweets|
              |Valu|Finders|Asian|Weis|Target|Aldi|H Mart|Medina|Draft Farm|
              |Oriental|Market Street|Blue|Thomas inc|Sinbad|Latin|
              |Keystone|Caul's|World|Oriental|Ole Country|Yoder's|Village market|
              |Indian|Dona|Giant|Belmont|Mira|Lidl|amores|Wegmans|Bridgewater|
              |Sysco|Express grocery")

storeNames <- dplyr::pull(stores, Store_Name) # create vector to use in str_detect

groceryOnly <- stores %>% 
  filter(str_detect(storeNames, regex(keyWords, ignore_case = T), negate = F))

write_csv(included, path = "included.csv") # split screen / compare

groceryOnly[!groceryOnly$Store_Name%in%included$Store_Name,] 
# do not understand why staunton junction keeps being called-- no part of name 
# nor address is a part of key words? only one with this issue
# -- just going to select it out for now to make maps while i think a lil more on it

groceryOnly <- stores_4326 %>% 
  filter(Store_Name != "Staunton Junction",
  str_detect(storeNames, regex(keyWords, ignore_case = T), negate = F))


# Maps 

cville_tracts <- readRDS("~/DemofData/summer-sandbox/cville_region_collection/data/cville_tracts.RDS")

groceryOnly <- groceryOnly %>% 
  dplyr::rename(ObjectId = )

cville_claims_sum <- cville_tracts %>% 
  left_join(cvilleClaimSum, by = c("TRACTCE" = "TRACT"))

cville_claims_sum <- st_transform(cville_claims_sum, crs = 4326) # to WGS84, given error


#spatial intersection of amount paid on building claim over polygon
pal <- colorNumeric("plasma", reverse = TRUE, domain = groceryOnly$n) # viridis

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = groceryOnly,
              fillColor = ~pal(n),
              weight = 1,
              opacity = 1,
              color = "white", 
              fillOpacity = 0.6,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T
              ),
              popup = paste0("Tract Number: ", groceryOnly$NAME, "<br>",
                             "Number: ", round(groceryOnly$n, 2))
  ) %>% 
  addLegend("bottomright", pal = pal, values = groceryOnly$n, 
            title = "# of Grocery Stores", opacity = 0.7)

