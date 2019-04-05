library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(sp)
library(sf)
library(mapview)

bikenet <- read_csv("data/bikenet-change.csv")
head(bikenet)
summary(factor(bikenet$facility2013))

# gather the facility columns into single year variable
colnames(bikenet)
bikenet_long <- bikenet %>% 
  gather(key = "year", value = "facility", facility2008:facility2013, na.rm = T) %>% 
  mutate(year = stringr::str_sub(year, start = -4))
head(bikenet_long)


#DANGER BELOW! fname can have multiple words

# collapse/unite street and suffix to one value
bikenet_long <- bikenet_long %>% 
  unite(col = "street", c("fname", "ftype"), sep = " ")
head(bikenet_long)

# separate street and suffix back to two values
bikenet_long <-  bikenet_long %>% 
  separate(street, c("name", "suffix"))

#ok danger over

bikenet_long %>% filter(bikeid == 139730)

fac_lengths <- bikenet_long %>% 
  filter(facility %in% c("BKE-LANE", "BKE-BLVD", "BKE-BUFF", "BKE-TRAK", "PTH-REMU")) %>% 
  group_by(year, facility) %>% 
  summarise(metres = sum(length_m)) %>% 
  mutate(miles = metres / 1609)

p <-  ggplot(fac_lengths, aes(x=year, y=miles, group = facility, color = facility))
p + geom_line()
p + geom_point()
p + geom_line() + scale_y_log10()
p + geom_line() + labs(title = "Change in facilities Portland", 
                       subtitle = "2008-2013", caption = "source: Portland metro") + 
  xlab("year") + ylab("Total miles")

p2 <-  ggplot(fac_lengths, aes(x=year, y=miles, group = facility))  
p2 + geom_line(size=1, color = "blue") + facet_wrap( ~ facility) + scale_y_log10()



