
# Hanafuda - Flower Card

library(rvest)
library(tidyverse)
library(magick)
library(ggthemes)

## I want to get table of card data from wiki site below
hanafuda <-read_html("https://en.wikipedia.org/wiki/Hanafuda") %>%
  html_nodes("table") %>%
  html_table() 


df <- hanafuda[[4]] %>% 
  separate(Flower,into=c("Name","Extra"), sep="\\(") %>%
  separate(Extra, into=c("flower_ja","flower_en"), sep=",") %>%
  mutate_all(str_remove,pattern="\\)") %>%
  mutate_all(str_trim)

hanafuda_score <- read_html("https://ja.wikipedia.org/wiki/%E8%8A%B1%E6%9C%AD") %>%
  html_nodes("table") %>%
  html_table(fill=T)

df2 <- hanafuda_score[[2]] %>%
  mutate_all(str_remove_all, pattern="\\[.+\\]") %>%
  mutate_all(str_remove_all, pattern="\\（.+\\）") %>%
  mutate_all(str_trim) %>%
  mutate_all(str_remove_all, pattern="－")
df2$Month <- month.name

df <- df %>% inner_join(df2)
names(df)
names(df) <- c("mo","name","flower_ja","flower_en","cards","imgages","mo_ja","flower","hikari_20","tane_10","tan_5","kasu_1")

df_long <- df %>% 
  select(mo, hikari_20:kasu_1) %>%
  mutate_all(na_if,"") %>%
  pivot_longer(-mo, 
               names_sep="_",
               names_to=c("type","point"),
               values_drop_na=T)

df_long <- df_long %>% 
  group_by(mo) %>%
  mutate(idx=row_number())

df_long2 <-df_long %>% filter(idx==max(idx)) %>% mutate(idx=idx+1) %>% filter(idx<5)
df_long3 <-df_long2 %>% filter(idx<4) %>% mutate(idx=idx+1)

df_long_comb <- bind_rows(df_long, df_long2, df_long3) %>%
  ungroup() %>%
  mutate(mo=factor(mo, levels=month.name)) %>%
  group_by(mo) %>%
  arrange(mo, idx) %>%
  mutate(point=as.numeric(point))

## 
df_long_comb$idx <- NULL
df_long_comb <-df_long_comb %>% group_by(mo) %>%
  arrange(mo, point) %>%
  mutate(idx=row_number())

## tidying up
rm(df_long,df_long2,df_long3)
rm(df,df2,hanafuda,hanafuda_score)

df_long_comb$mo_num <- rep(c(1:12), each=4)

df_long_comb <- df_long_comb %>% 
  ungroup() %>%
  mutate(tmp_url = str_glue("https://commons.wikimedia.org/wiki/File:Hanafuda_{mo_num}-{idx}.svg"))

######  Getting Images from  Wiki Common
get_url <- function(x){
  tmp <-read_html(x) %>%
    html_nodes("#file a")  %>%
    html_attr("href")
  return(tmp[[1]])
}

df_long_comb <- df_long_comb %>%
  mutate(img_svg = map_chr(tmp_url,get_url))


## separate value
df_long_comb <- df_long_comb %>%
  separate(col=value, into=c("flower","item"), remove=F, sep="(の|に)") %>%
  mutate(card_type = case_when(item %in% c("短冊","青短","赤短") ~ "Tan",
                               item %in% c("幕", "月", "小野道風", "鳳凰", "鶴") ~ "Light",
                               item %in%  c("不如帰", "八橋", "小野道風", "燕", "猪", "盃", "蝶", "雁", "鴬", "鹿") ~ "Tane",
                               TRUE ~ "Kasu")) %>%
  ungroup()


df_long_comb$flower_en <- rep(c("Pine","Plum","Cherry","Wisteria","Iris","Peony","Bush Clover","Pampas Grass","Chrysanthemum","Maple","Willow","Paulownia"),each=4)


df_long_comb <-df_long_comb %>%
  arrange(mo, -idx)

df_long_comb$item

df_long_comb$item_en <- c("Crane","Red Poem Ribbon","Plain","Plain",
                          "Nightingale","Red Poem Ribbon","Plain","Plain",
                          "Curtain","Red Poem Ribbon","Plain","Plain",
                          "Cuckoo","Red Ribbon","Plain","Plain",
                          "Bridge","Red Ribbon","Plain","Plain",
                          "Butterflies","Blue Ribbon","Plain","Plain",
                          "Boar","Red Ribbon","Plain","Plain",
                          "Moon","Geese","Plain","Plain",
                          "Sake Cup","Blue Ribbon","Plain","Plain",
                          "Deer","Blue Ribbon","Plain","Plain",
                          "Rainman","Swallow","Red Ribbon","Plain",
                          "Pheonix","Plain","Plain","Plain")

df_long_comb <-df_long_comb %>%
  arrange(mo, idx) %>%
  mutate(card_id = str_c(mo_num,"-",idx),
         card_idx = row_number())

write_rds(df_long_comb, "data/hanafuda.rds")

### Start Here
df_long_comb <- read_rds("data/hanafuda.rds")


card_by_mo <- df_long_comb %>%
  group_by(mo) %>%
  arrange(-idx) %>%
  summarise(img_list=list(img_svg)) %>%
  mutate(imgs = map(img_list, image_read_svg, width=200)) %>%
  mutate(imgs_joined = map(imgs,image_append, stack=T))

card_by_mo$imgs_joined %>%
  image_join() %>%
  image_append() %>%
  image_background(color="white") %>%
  image_channel(channel="Gray")


card_by_type <- df_long_comb %>%
  group_by(card_type) %>%
  summarise(img_list=list(img_svg),
            n=n(),
            width=round(2400/n)) %>%
  mutate(imgs=map2(img_list,width,image_read_svg)) %>%
  mutate(imgs_joined = map(imgs, image_append)) %>%
  arrange(n)

card_by_type$imgs_joined %>%
  image_join() %>%
  image_append(stack=T) %>%
  image_background(color="white")


ref_img <- df_long_comb$img_svg[17] %>% image_read_svg(width=100)
df_long_comb %>% 
  filter(point>5) %>%
  pull(img_svg) %>%
  image_read_svg(width=100) %>%
  image_background(color="white")%>%
  image_channel("Gray") %>%
  #image_compare(reference_image=ref_img) %>%
  image_append()



shuffled_cards <-df_long_comb$img_svg[sample(c(1L:48L),size=48)] %>%
  image_read_svg(width=200) %>%
  image_background(color="white")

a<-shuffled_cards[1:8] %>% image_append()
ba<-shuffled_cards[9:16] %>% image_append()
b<-shuffled_cards[17:24] %>% image_append()

mtn1 <- shuffled_cards[25:32] %>% image_append()
mtn2 <- shuffled_cards[33:40] %>% image_append()

lo <- shuffled_cards[41:48] %>% image_append()

image_join(a,ba,b,mtn1,mtn2,lo) %>%
  image_join() %>%
  image_append(stack=T)
  



#####
library(ggraph)
library(tidygraph)
library(UpSetR)

## Yaku 

unique(df_long_comb$item_en)
yaku_list <- list(
  "Kasu"= df_long_comb %>% filter(type=="kasu") %>% pull(card_idx) %>% sample(size=10),
  "Tan" = df_long_comb %>% filter(card_type=="Tan") %>% pull(card_idx) %>% sample(size=5),
  "Akatan" =df_long_comb %>% filter(item_en=="Red Poem Ribbon") %>% pull(card_idx),
  "Aotan" =df_long_comb %>% filter(item_en=="Blue Ribbon") %>% pull(card_idx),
  "Akatan/Aotan Choufuku" =df_long_comb %>% filter(item_en=="Blue Ribbon"|item_en=="Red Ribbon") %>% pull(card_idx),
  "Tane" = df_long_comb %>% filter(card_type=="Tane") %>% pull(card_idx) %>% sample(size=5),
  "Inoshikacho" = df_long_comb %>% filter(item_en %in% c("Boar","Deer","Butterflies")) %>% pull(card_idx),
  "FullMoon Sake" = df_long_comb %>% filter(item_en %in% c("Moon","Sake Cup")) %>% pull(card_idx),
  "Hanami Sake" = df_long_comb %>% filter(item_en %in% c("Curtain","Sake Cup")) %>% pull(card_idx),
  "Goko" = df_long_comb %>% filter(card_type=="Light") %>% pull(card_idx),
  "Shiko" = df_long_comb %>% filter(card_type=="Light"&!mo_num==10) %>% pull(card_idx),
  "Sanko" = df_long_comb %>% filter(card_type=="Light") %>% pull(card_idx) %>% sample(size=3),
  "AmeShiko"=df_long_comb %>% filter(card_type=="Light") %>% pull(card_idx) %>% sample(size=4)
)


df_long_comb$img_svg[yaku_list$Inoshikacho[c(2,3,1)]] %>%
  image_read_svg(width=250) %>%
  image_background(color="white") %>%
  image_append()


df_long_comb$img_svg[yaku_list$Kasu] %>%
  image_read_svg(width=100) %>%
  image_background(color="white") %>%
  image_append()






yaku_list$Goko %>% combn(m=5, simplify=F)
yaku_list$Shiko %>% combn(m=4, simplify=F)
yaku_list$Kasu %>% combn(m=10, simplify=F)

yaku_list$Tan %>% combn(m=5, simplify=F)
yaku_list$Aotan %>% combn(m=3)

mo_num <- rep(c(1:12), each=4)
mo_idx <- rep(c(1:4), times=12) ## 4 is the better card for images

img_svg_url <- str_glue("https://commons.wikimedia.org/wiki/File:Hanafuda_{mo_num}-{mo_idx}.svg")

get_url <- function(x){
  tmp <-read_html(x) %>%
    html_nodes("#file a")  %>%
    html_attr("href")
  return(tmp[[1]])
}

# quick test of function
get_url(img_svg_url[[1]])  

## save list of image urls to img_svg -- this takes bit of time
img_svg <- map_chr(img_svg_url, get_url)

## now load all 48 images
imgs <- image_read_svg(img_svg, width=400)
imgs

a <-imgs[1:12*4] %>% image_append()
b <-imgs[(1:12*4)-1] %>% image_append() 
c <-imgs[(1:12*4)-2] %>% image_append() 
d <-imgs[(1:12*4)-3] %>% image_append() 

hanafuda_img <- image_join(a,b,c,d) %>% image_append(stack=T)

hanafuda_img2 <-hanafuda_img %>% image_background(color="gray24")
hanafuda_img1 <-hanafuda_img %>% image_background(color="white")

hanafuda_img2
hanafuda_img1

## I should save image url in dataframe!
df_long_comb$mo_num <- mo_num
df_long_comb$svg_img <- img_svg






df_long_comb$item
df_long_comb$value %>% cat()
df_long_comb$item_en <- c("Crane","Red Poem Ribbon","Plain","Plain",
                          "Nightingale","Red Poem Ribbon","Plain","Plain",
                          "Curtain","Red Poem Ribbon","Plain","Plain",
                          "Cuckoo","Red Ribbon","Plain","Plain",
                          "Bridge","Red Ribbon","Plain","Plain",
                          "Butterflies","Blue Ribbon","Plain","Plain",
                          "Boar","Red Ribbon","Plain","Plain",
                          "Moon","Geese","Plain","Plain",
                          "Sake Cup","Blue Ribbon","Plain","Plain",
                          "Deer","Blue Ribbon","Plain","Plain",
                          "Rainman","Swallow","Red Ribbon","Plain",
                          "Pheonix","Plain","Plain","Plain")

  
df_long_comb %>% count(item) %>% filter(n<2) %>% pull(item) %>% cat(sep=",")



df_test <-df_long_comb %>%
  ungroup() %>%
  group_by(mo) %>%
  summarise(img_list=list(svg_img)) %>%
  mutate(test = map(img_list, image_read_svg, width=200))

df_test <-df_test %>% 
  mutate(test2 = map(test, image_append))

df_test$test2[[2]]


library(ggraph)
library(tidygraph)

g <- as_tbl_graph(test_hc)


 
 
library(wordcloud2)
df_long_comb %>% count(item, sort=T) #%>% 
  mutate(n=log(n+1)) %>%  ## just to adjust the size
  wordcloud2::wordcloud2(rotateRatio=0, fontFamily="Hiragino Mincho ProN W6")
  
# total of 48 cards, of 24 are Kasu.  4 plain ribbons, 3 red poetry ribbons, 3 blue poetry ribbons


g <- grid::rasterGrob(hanafuda_img2, interpolate=TRUE)


df_long_comb %>%
  ggplot(aes(x=(idx)*631, y=as.numeric(mo)*400)) +
  annotation_custom(grob=g) +
  geom_text(aes(label=str_c(mo,value,sep="\n")), 
            color="#ffffffde", family="Osaka", hjust=0, vjust=1) +
  theme_void() +
  scale_fill_pander() +
  scale_x_reverse() +
  scale_y_reverse() +
  coord_flip() +
  scale_size_identity() 






mo_num <- rep(c(1:12), each=4)
mo_idx <- rep(c(1:4), times=12)

img_svg_url <- str_glue("https://commons.wikimedia.org/wiki/File:Hanafuda_{mo_num}-{mo_idx}.svg")

get_url <- function(x){
  tmp <-read_html(x) %>%
    html_nodes("#file a")  %>%
    html_attr("href")
  return(tmp[[1]])
  
}
get_url(img_svg_url[[1]])  


img_svg <- map_chr(img_svg_url, get_url)

test <- image_read_svg(img_svg, width=400)
test

a <-test[12:1*4] %>% image_append()
b <-test[(12:1*4)-1] %>% image_append() 
c <-test[(12:1*4)-2] %>% image_append() 
d <-test[(12:1*4)-3] %>% image_append() 

hanafuda_img <- image_join(a,b,c,d) %>% image_append(stack=T)

hanafuda_img2 <-hanafuda_img %>% image_background(color="gray24")

hanafuda_img2

hanafuda_img2 %>% 
  image_quantize(colorspace="rgb", max=16) %>%
  imager::magick2cimg() %>%
  as.data.frame(wide="c") %>%
  mutate(rgb = rgb(c.1,c.2,c.3)) %>%
  count(rgb, sort=T) %>%
  ggplot(aes(x=rgb,y=log(n)))+
  geom_col(aes(fill=rgb)) +
  scale_fill_identity() +
  theme_fivethirtyeight() +
  coord_polar()




