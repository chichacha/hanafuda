library(tidyverse)
library(ggraph)
library(tidygraph)
library(magick)


# install ggimage
install.packages("ggimage")
library(ggimage)

### Read Datasets
df_long_comb <- read_rds("data/hanafuda.rds")


img_read_write <- function(x,y){
  x %>% 
    image_read_svg(width=500)%>%
    image_background(color="white") %>%
    image_write(path=str_c("pngs/",y,".png"))
}

walk2(df_long_comb$img_svg, df_long_comb$card_idx, img_read_write)

df_long_comb <-df_long_comb %>%
  mutate(img_png_local = str_c("pngs/",card_idx,".png"))

df_long_comb$img_png_local %>%
  sample(size=48) %>%
  image_read() %>%
  image_resize(100)%>%
  image_append()


### Create separate Yaku df

yaku_class <- df_long_comb %>% 
  group_by(card_type) %>%
  summarise(yaku_descr = paste(unique(item),collapse=" "),
            grp_cnt=n(),
            img_list = list(img_svg),
            value_list=list(unique(value)),
            card_idx=list(card_idx)) %>%
  rename(yaku_name = card_type)

## Tan
yaku_class2 <- df_long_comb %>% 
  filter(card_type=="Tan") %>%
  group_by(item_en) %>%
  summarise(yaku_descr = paste(unique(item),collapse=" "),
            grp_cnt=n(),
            img_list = list(img_svg),
            value_list=list(unique(value)),
            card_idx=list(card_idx))%>%
  rename(yaku_name = item_en) %>%
  filter(yaku_descr!="短冊")

yaku_class2b<- df_long_comb %>% 
  filter(card_type=="Tan") %>%
  filter(item!="短冊") %>%
  summarise(yaku_descr = paste(unique(item),collapse=" "),
            grp_cnt=n(),
            img_list = list(img_svg),
            value_list=list(unique(value)),
            card_idx=list(card_idx))%>%
  mutate(yaku_name = "Aotan Akatan no Chofuku") %>%
  select(yaku_name, everything())
  


unique(df_long_comb$item)
## Special
yaku_class3 <- df_long_comb %>% 
  filter(item  %in% c("蝶", "猪", "鹿")) %>%
  summarise(yaku_descr = paste(unique(item),collapse=" "),
            grp_cnt=n(),
            img_list = list(img_svg),
            value_list=list(unique(value)),
            card_idx=list(card_idx))%>%
  mutate(yaku_name = "Inoshikacho") %>%
  select(yaku_name, everything())

yaku_class4 <- df_long_comb %>% 
  filter(item  %in% c("幕", "盃")) %>%
  summarise(yaku_descr = paste(unique(item),collapse=" "),
            grp_cnt=n(),
            img_list = list(img_svg),
            value_list=list(unique(value)),
            card_idx=list(card_idx))%>%
  mutate(yaku_name = "Hanami Sake") %>%
  select(yaku_name, everything())


yaku_class5 <- df_long_comb %>% 
  filter(item  %in% c("月", "盃")) %>%
  summarise(yaku_descr = paste(unique(item),collapse=" "),
            grp_cnt=n(),
            img_list = list(img_svg),
            value_list=list(unique(value)),
            card_idx=list(card_idx))%>%
  mutate(yaku_name = "Fullmoon Sake") %>%
  select(yaku_name, everything())


yaku_master <- bind_rows(yaku_class, yaku_class2,yaku_class2b, yaku_class3, yaku_class4, yaku_class5)

edges <- yaku_master %>%
  select(yaku_name,card_idx) %>%
  unnest(card_idx) %>%
  rename(from=card_idx,to=yaku_name)

nodes <- tibble(
  name = unique(c(edges$from, edges$to))
) %>% left_join(df_long_comb %>% select(name=card_idx, item_en, item, card_type, img_svg, mo, mo_num, value) %>% mutate(name=as.character(name)))

g <- tbl_graph(nodes=nodes,edges=edges, directed=T)

g
g %>%  ggraph("graphopt") +
  geom_node_point(aes(color=card_type)) +
  geom_edge_fan() +
  geom_node_label(aes(filter=!is.na(item), label=value), family="Osaka")


yaku_master <- yaku_master %>% arrange(grp_cnt)
map2(yaku_master$img_list,yaku_master$grp_cnt, ~image_read_svg(.x, height=1800/.y) %>% image_append(stack=T)) %>%
  image_join() %>%
  image_append() %>%
  image_background(color="gray24")

##
shuffle_art <- df_long_comb %>% sample_n(size=48)
shuffle_art$grp_id <- c(rep("1_PlayerA",times=8),
                        rep("2_Ba",times=4),
                        rep("2_Ba",times=4),
                        rep("4_PlayerB",8),
                        rep(c("1_PlayerA","4_PlayerB"),times=8),
                        rep("7_LeftOver",8))

shuffle_art <-shuffle_art %>%
  group_by(grp_id) %>%
  summarise(n=n(),
            img_lst=list(img_svg))

map2(shuffle_art$img_lst, shuffle_art$n,  ~image_read_svg(.x, width=2400/.y) %>%
       image_append(stack=F)) %>%
  image_join() %>%
  image_append(stack=T) %>%
  image_background(color="gray24") %>%
  image_write("output/ShuffledHanafuda.png")



df_long_comb %>%
  group_by(card_type) %>%
  summarise(n=n(),
            img_list=list(img_svg)) %>%
  mutate(img_row = map2(img_list, n, ~image_read_svg(.x, width=2400/.y) %>%
                          image_append(stack=F))) -> tmp

tmp$img_row %>% image_join() %>%
  image_append(stack=T) %>%
  image_background(color="gray24")%>%
  image_write("output/ArrangedByPoints.png")
  


