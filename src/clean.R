library(tidyverse)
library(forcats)

# read in data
ae <- read_csv("../data/raw/ae_com.csv")
ae$rating <- as.numeric(ae$rating)
ae$review_count <- as.integer(ae$review_count)

amz <- read_csv("../data/raw/amazon_com.csv")
amz$rating <- as.numeric(amz$rating)
amz$review_count <- as.integer(amz$review_count)

wacoal <- read_csv("../data/raw/btemptd_com.csv")
wacoal$rating <- as.numeric(wacoal$rating)
wacoal$review_count <- as.integer(wacoal$review_count)

ck <- read_csv("../data/raw/calvinklein_com.csv")
ck$rating <- as.numeric(ck$rating)
ck$review_count <- as.integer(ck$review_count)

hk <- read_csv("../data/raw/hankypanky_com.csv")
hk$rating <- as.numeric(hk$rating)
hk$review_count <- as.integer(hk$review_count)

macy <- read_csv("../data/raw/macys_com.csv")
macy$rating <- as.numeric(macy$rating)
macy$review_count <- as.integer(macy$review_count)

nordstrom <- read_csv("../data/raw/shop_nordstrom_com.csv")
nordstrom$rating <- as.numeric(nordstrom$rating)
nordstrom$review_count <- as.integer(nordstrom$review_count)

topshop <- read_csv("../data/raw/us_topshop_com.csv")
topshop$rating <- as.numeric(topshop$rating)
topshop$review_count <- as.integer(topshop$review_count)

vs <- read_csv("../data/raw/victoriassecret_com.csv")
vs$rating <- as.numeric(vs$rating)
vs$review_count <- as.integer(vs$review_count)

raw_df <- bind_rows(ae, amz, wacoal, ck, hk, macy, nordstrom, topshop, vs)

df <- raw_df

# clean data
## mrp, price, pct_sales
df$mrp_converted <- df$mrp %>% 
  str_extract(.,"[0-9]*\\.[0-9]*")%>% 
  trimws(., which = "right")
df$mrp_converted <- as.numeric(df$mrp_converted)

df$price_converted <- df$price %>% 
  str_extract(.,"[0-9]*\\.[0-9]*")%>% 
  trimws(., which = "right")
df$price_converted <- as.numeric(df$price_converted)

df <- df %>% 
  mutate(pct_sales = -1*(price_converted-mrp_converted)/mrp_converted)

## brand and retailers
df$retailer <- as.factor(df$retailer)
df$retailer <- df$retailer %>% recode("Ae US" = "American Eagle",
                       "Btemptd US" = "Wacoal",
                       "Hankypanky US" = "Hanky Panky",
                       "Nordstrom US" = "Nordstrom",
                       "Victoriassecret US" = "Victoria Secret",
                       "Amazon US" = "Amazon",
                       "Calvin Klein US" = "Calvin Klein",
                       "Macys US" = "Macys",
                       "Topshop US" = "Topshop")
as.character <- as.character(df$retailer)

df$brand_name <- df$brand_name %>% 
  str_to_lower()

df$brand_name <- df$brand_name %>% 
  str_replace_all(., c(".*aeo.*" = "American Eagle",
                       ".*aerie.*" = "AERIE",
                       ".*temp.*" = "Wacoal",
                       ".*calvin.*" = "Calvin Klein",
                       ".*hanky.*" = "Hanky Panky",
                       ".*compression.*" = "Others",
                       ".*creative.*" = "Others",
                       ".*nintendo.*" = "Others",
                       ".*lucky.*" = "Others",
                       "nordstrom lingerie" = "Nordstrom",
                       ".*wacoal.*" = "Wacoal",
                       "us topshop" = "Topshop",
                       ".*secret.*" = "Victoria Secret",
                       ".*vanity.*" = "Others",
                       "s" = "Others",
                       ".*hair.*" = "Others",
                       ".*fila.*" = "Others"))

df$brand_name <- as.factor(df$brand_name)
df$brand_name <- df$brand_name %>% recode("Other brandOthers" = "Others",
                                          "NordOtherstrom" = "Nordstrom",
                                          "OtherOthers" = "Others",
                                          "TopOthershop" = "Topshop")

df$brand_name <- paste(df$retailer, df$brand_name, sep = "-")

df <- df %>% 
  mutate(brand = as.factor(df$brand_name))
df$brand <- df$brand %>% recode("Nordstrom-Nordstrom" = "Nordstrom",
                    "Victoria Secret-Victoria Secret" = "Victoria Secret",
                    "Calvin Klein-Calvin Klein" = "Calvin Klein",
                    "Wacoal-Wacoal" = "Wacoal",
                    "Hanky Panky-Hanky Panky" = "Hanky Panky",
                    "Topshop-Topshop" = "Topshop")


## category
df$product_category <- as.factor(df$product_category)
category <- data.frame(levels(df$product_category))

df$product_category <- df$product_category %>% 
  str_to_lower()

df$category <- df$product_category %>% 
  str_replace_all(., c(".*bra.*" = "bra",
                       ".*sock.*" = "accessories",
                       ".*thong.*" = "panties",
                       ".*bikini.*" = "swimwear",
                       ".*sunnie wireless.*" = "bra",
                       ".*bralette.*" = "bralette",
                       ".*bandeau.*" = "bra",
                       ".*brief.*" = "panties",
                       ".*hat.*" = "accessories",
                       ".*cheeky.*" = "panties",
                       ".*hiphugger.*" = "panties",
                       ".*shortie.*" = "panties",
                       ".*boyshort.*" = "panties",
                       ".*bridal.*" = "bridal",
                       ".*sport bra.*" = "sport",
                       ".*bridget.*" = "bra",
                       ".*audrey.*" = "bra",
                       ".*accessories.*" = "accessories",
                       ".*brooke.*" = "brooke",
                       ".*bottle.*" = "accessories",
                       ".*cheekies.*" = "panties",
                       ".*cami.*" = "sleep wear",
                       "cage front high-neck crop" = "swimwear",
                       ".*panty.*" = "panties",
                       "coconut waters travel kit" = "accessories",
                       ".*lingerie.*" = "lingerie",
                       ".*push.*" = "bra",
                       ".*tote.*" = "accessories",
                       ".*hipster.*" = "panties",
                       ".*babydoll.*" = "sleep wear",
                       "wrap triangle top" = "swimwear",
                       ".*tomgirl.*" = "panties",
                       "ultimate high waist legging" = "sport",
                       "ultimate pocket legging" = "sport",
                       "ultimate yoga legging" = "sport",
                       ".*undies.*" = "panties",
                       ".*tank.*" = "sleep wear",
                       ".*demi.*" = "bra",
                       ".*bustier.*" = "bra",
                       ".*brooke.*" = "bra",
                       ".*lace.*" = "lingerie",
                       "cross-back scoop" = "swimwear",
                       ".*teddy.*" = "lingerie",
                       ".*sleep.*" = "sleep wear",
                       "embroidered mesh bodysuit" = "lingerie",
                       ".*cheekster.*" = "panties",
                       ".*kimono.*" = "sleep wear",
                       ".*gym.*" = "sport",
                       ".*belt.*" = "accessories",
                       ".*one-piece.*" = "swimwear",
                       ".*onepiece.*" = "swimwear",
                       ".*halter.*" = "swimwear",
                       "high-neck wrap" = "swimwear",
                       "knotted back body wrap top" = "swimwear",
                       ".*panties.*" = "panties",
                       ".*triangle.*" = "panties",
                       ".*slip.*" = "sleep wear",
                       "sunnie full coverage" = "bra",
                       ".*one piece.*" = "swimwear",
                       ".*sport.*" = "sport",
                       "logo elastic short" = "sleepwear",
                       ".*tee.*" = "sleepwear",
                       "lightly lined macramé high-neck" = "swimwear",
                       ".*racerback.*" = "bra",
                       ".*petal.*" = "accessories",
                       "deep v crop" = "bra",
                       ".*hannah.*" = "bra",
                       ".*full-zip.*" = "sport",
                       ".*hineck.*" = "bra",
                       ".*katie.*" = "bra",
                       ".*longline.*" = "bra",
                       ".*lorna jane.*" = "bra",
                       "mesh-cap-sleeve bodysuit" = "lingerie",
                       ".*move.*" = "sport",
                       "new! day to play" = "bra",
                       "padded" = "bra",
                       "play" = "bra",
                       "plunge" = "bra",
                       "split hem tunic" = "sport",
                       "summer" = "bra",
                       "sleep wear" = "sleepwear",
                       "classic" = "bra",
                       "bride" = "bridal"))

## treat "chill" and "collections" categories
df <- df %>% 
  mutate(last_word_chill = ifelse(category == "chill", word(df$product_name,-3, -1), "NA"),
         last_word_collections = ifelse(category == "collections", word(df$product_name,-3, -1), "NA"))

### "chill" category
df$last_word_chill <- as.factor(df$last_word_chill)
big_categories <- data.frame(levels(df$last_word_chill))

bra <- str_which(df$last_word_chill, pattern = ".*Bra.*|.*Bralette.*|.*Bandeau.*|.*Crop.*")
df$category[bra] <- "bra"

### "collections" category
df$last_word_collections <- as.factor(df$last_word_collections)
big_categories_2 <- data.frame(levels(df$last_word_collections))

panties2 <- str_which(df$last_word_collections, pattern = ".*G-.*|.*V-.*|.*Panty.*|.*Hipster.*|.*Thong.*|.*Boyshort.*|.*Teddy.*|.*Tanga.*|.*Brief.*|.*Panties.*|.*Tap Pant.*|.*thong.*")
bra2 <- str_which(df$last_word_collections, pattern = ".*Bra.*|.*Bralette.*|.*Bandeau.*|.*Crop.*")
bride2 <- str_which(df$last_word_collections, pattern = ".*Bride.*")
sleepwear2 <- str_which(df$last_word_collections, pattern = ".*Chemise.*|.*Cami.*|.*Top.*|.*T-Shirt.*|.*Slip.*|.*Sleep.*|.*Tank.*|.*Robe.*|.*Gown.*|.*Pajamas.*|.*Short.*|.*Pant.*")
accessories2 <- str_which(df$last_word_collections, pattern = ".*Sock.*|.*Garter.*|.*Cuff.*|.*Plaything.*|.*Leg.*|Set with Gift Box")
swimwear2 <- str_which(df$last_word_collections, pattern = ".*Bikini.*")
lingerie2 <- str_which(df$last_word_collections, pattern = ".*Bodysuit.*|.*Slit.*")

df$category[panties2] <- "panties"
df$category[bra2] <- "bra"
df$category[bride2] <- "bridal"
df$category[sleepwear2] <- "sleepwear"
df$category[accessories2] <- "accessories"
df$category[swimwear2] <- "swimwear"
df$category[lingerie2] <- "lingerie"

### check final category list after cleaning
df$category <- as.factor(df$category)
grouped_category <- data.frame(levels(df$category))

## drop unused factors from category
df$category <- fct_drop(df$category, only = "collections")
df$category <- fct_drop(df$category, only = "chill")

## color
df$color <- df$color %>% 
  str_to_lower()

df$color <- as.factor(df$color)
color_list <- data.frame(levels(df$color))  

df$color_group <- df$color %>% 
  str_replace_all(., c(".*white.*" = "white",
                       ".*turquoise.*" = "turquoise",
                       ".*pink.*" = "pink",
                       ".*teal.*" = "teal",
                       ".*navy.*" = "navy",
                       ".*berry.*" = "berry",
                       ".*green.*" = "green",
                       ".*red.*" = "red",
                       ".*blue.*" = "blue",
                       ".*cobalt.*" = "blue",
                       ".*buff.*" = "nude",
                       ".*charcoal.*" = "grey",
                       ".*grey.*" = "grey",
                       ".*gray.*" = "grey",
                       ".*coral.*" = "coral",
                       ".*crystal.*" = "no color",
                       ".*burgundy.*" = "purple",
                       ".*plum.*" = "purple",
                       ".*slate.*" = "multiple colors",
                       "be pretty" = "multiple colors",
                       ".*mint.*" = "green",
                       "fresh bright" = "multiple colors",
                       "heather frost" = "multiple colors",
                       ".*hush.*" = "nude",
                       ".*jade.*" = "green",
                       ".*kissed.*" = "multiple colors",
                       "light aglow" = "multiple colors",
                       "darkness falls" = "black",
                       ".*multi.*" = "multiple colors",
                       ".*ocean.*" = "blue",
                       ".*nude.*" = "nude",
                       ".*oatmeal.*" = "nude",
                       ".*olive.*" = "olive",
                       ".*purple.*" = "purple",
                       "rose" = "red",
                       "sienna" = "nude",
                       "rio" = "multiple colors",
                       ".*muslin.*" = "nude",
                       ".*peach.*" = "nude",
                       "stargaze" = "multiple colors",
                       "stone" = "multiple colors",
                       "tangerine crush" = "coral",
                       ".*black.*" = "black",
                       ".*berry.*" = "purple",
                       "dark heather" = "multiple colors",
                       "light cashew" = "nude",
                       "light heather" = "multiple colors",
                       "luminous fuchsia" = "purple",
                       "maroon lagoon" = "multiple colors",
                       "medium heather" = "multiple colors",
                       "radiance" = "multiple colors",
                       "silver shadow" = "multiple colors",
                       "ur cheeky" = "multiple colors",
                       "valentine" = "red"))

## transform data type
df$rating <- as.numeric(df$rating)
df$review_count <- as.integer(df$review_count)

## transform missing data in mrp, price, rating, view count
df$mrp[is.na(df$mrp)] <- round(mean(df$mrp, na.rm = TRUE), 2)
df$price[is.na(df$price)] <- round(mean(df$price, na.rm = TRUE), 2)
df$rating[is.na(df$rating)] <- min(df$rating, na.rm = TRUE)
df$review_count[is.na(df$review_count)] <- min(df$review_count, na.rm = TRUE)

# getting final dataframe
## drop NAs from "mrp" since one of the main goal is to look at price range
df <- df %>% 
  filter(!category%in% c("chill", "collections")) %>% 
  select(-last_word_chill, -last_word_collections, 
         -product_category, -total_sizes, -available_size,
         -style_attributes, -description)

## mutate new column - percentage of sales
df_final <- df %>% 
  select(product_name, mrp, price, pct_sales, category, color_group, brand_name, rating, review_count)


# output clean data
write_csv(df_final, "../data/cleaned/cleaned_shiny.csv")



