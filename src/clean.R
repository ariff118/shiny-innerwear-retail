library(tidyverse)

# read in data
files <-  list.files(path = "../data/raw/", pattern = "*.csv")

file_path <- paste("../data/raw/",files, sep = "")
raw_df <-  do.call(rbind, lapply(file_path, function(x) read.csv(x, row.names = NULL, stringsAsFactors = FALSE)))

df <- raw_df

# clean data
## mrp and price
df$mrp <- df$mrp %>% 
  gsub("usd", "", ., ignore.case = TRUE) %>% 
  trimws(., which = "right")
df$mrp <- as.numeric(df$mrp)


df$price <- df$price %>% 
  gsub("usd", "", ., ignore.case = TRUE) %>% 
  trimws(., which = "right")
df$price <- as.numeric(df$mrp)

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

df$brand <- paste(df$brand_name, df$retailer, sep = "-")

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
                       "classic" = "bra"))

## treat "chill" and "collections" categories

df <- df %>% 
  mutate(last_word = ifelse(df$category %in% c("collections", "chill"), word(df$product_name,-4, -1), "NA"))
df$last_word <-  as.factor(df$last_word)
# big_categories <- data.frame(levels(df$last_word))

panties <- str_which(df$last_word, pattern = ".*G-.*|.*V-.*|.*Panty.*|.*Hipster.*|.*Thong.*|.*Boyshort.*|.*Teddy.*|.*Tanga.*|.*Brief.*|.*Panties.*|.*Tap Pant.*|.*thong.*")
bra <- str_which(df$last_word, pattern = ".*Bra.*|.*Bralette.*|.*Bandeau.*|.*Crop.*")
bride <- str_which(df$last_word, pattern = ".*Bride.*")
sleepwear <- str_which(df$last_word, pattern = ".*Chemise.*|.*Cami.*|.*Top.*|.*T-Shirt.*|.*Slip.*|.*Sleep.*|.*Tank.*|.*Robe.*|.*Gown.*|.*Pajamas.*|.*Short.*|.*Pant.*")
accessories <- str_which(df$last_word, pattern = ".*Sock.*|.*Garter.*|.*Cuff.*|.*Plaything.*|.*Leg.*|Set with Gift Box")
swimwear <- str_which(df$last_word, pattern = ".*Bikini.*")
lingerie <- str_which(df$last_word, pattern = ".*Bodysuit.*|.*Slit.*")

df$category[panties] <- "panties"
df$category[bra] <- "bra"
df$category[bride] <- "bride"
df$category[sleepwear] <- "sleepwear"
df$category[accessories] <- "accessories"
df$category[swimwear] <- "swimwear"
df$category[lingerie] <- "lingerie"

### check final category list after cleaning
df$category <- as.factor(df$category)
grouped_category <- data.frame(levels(df$category))

