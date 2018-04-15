

#===================================
#    Mapping for Nghệ An Province
#===================================

rm(list = ls())
library(tidyverse)
library(raster)

# Lấy dữ liệu địa lí của VN đến cấp xã: 
vietnam_h <- getData("GADM", country = "Vietnam", level = 3)

# Tách riêng tỉnh Nghệ An: 
na <- vietnam_h[vietnam_h$NAME_1 == "Nghệ An", ]
detach(package:raster)

# Chuyển hóa về DF quen thuộc: 
na_df1 <- na %>% fortify(region = "ID_3")

# Vẽ bản đồ đến cấp xã cho Nghệ An: 
na_df1 %>% 
  ggplot(aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = id), 
               color = "grey90",
               show.legend = FALSE, 
               size = 0.01) +  
  labs(x = NULL, y = NULL, 
       title = "Map of Nghe An Province by Commune Level") + 
  theme_dark()


# Vẽ đến cấp huyện: 
na_df2 <- na %>% fortify(region = "ID_2")

na_df2 %>% 
  ggplot(aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = id), 
               color = "grey90",
               show.legend = FALSE, 
               alpha = 0.7) +  
  labs(x = NULL, y = NULL, 
       title = "Map of Nghe An Province by District Level") + 
  theme_minimal()


# Vẽ đến cấp huyện đồng thời biểu diễn cả centroid - trung tâm của huyện: 

na_cen_long <- na_df2 %>% 
  group_by(id) %>% 
  summarise_each(funs(mean), long)

na_cen_lat <- na_df2 %>% 
  group_by(id) %>% 
  summarise_each(funs(mean), lat)

cen <- data.frame(long = na_cen_long$long, lat = na_cen_lat$lat)


# Vẽ: 
ggplot() + 
  ggplot2::geom_polygon(data = na_df2, aes(x = long, y = lat, group = group, fill = id), 
               color = "grey90", show.legend = FALSE, alpha = 0.7) +  
  geom_point(data = cen, aes(x = long, y = lat), size = 3, color = "red") + 
  labs(x = NULL, y = NULL, 
       title = "Map of Nghe An Province by District Level with Corresponding Centroids") + 
  theme_minimal()
  




