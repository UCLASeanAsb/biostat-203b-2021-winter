library("tidyverse")
iris
as_tibble(iris)
tibble(
  x = 1:5,
  y = 1,
  z = x ^ 2 + y
)
tribble(
  ~x, ~y, ~z,
  #--/--/----
  "a", 2, 3.6,
  "b", 1, 8.5
)

nycflights13::flights

nycflights13::flights %>%
  print(n = 10, width = Inf)

df <- tibble(
  x = runif(5),
  y = rnorm(5)
)
df

df$x
df[["x"]]
df[[1]]

df %>% .$x
df %>% .[["x"]]

head heights.csv
library(readr)
library(readxl)
read_excel("datasets.xlsx", sheet = "mtcars")
mtcars
setosa
table1
head heights.csv


library(tidyverse)
library(ggplot2)
mpg %>% print(width = Inf)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ class)

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, color = drv))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() + geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

diamonds
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))
ggplot(data = diamonds) +
  stat_count(mapping = aes(x = cut))
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = ..prop..))
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, colour = cut))
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = cut))
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity))
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))
ggplot(data = diamonds) +
  geom_col(mapping = aes(x = cut, y = carat))
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")
ggplot(data = mpg) +
  geom_jitter(mapping = aes(x = displ, y = hwy))
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "stack")
mpg
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot() +
  coord_cartesian(xlim = c(0, 5))
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot() +
  coord_fixed(ratio = 1/2)

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot() +
  coord_flip()
 ggplot(data = mpg, mapping = aes(x = factor(1), fill = class)) +
   geom_bar(width = 1) +
   coord_polar("y")
library("maps")
nz <-map_data("nz")
head(nz, 20)

ggplot(nz, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) + 
  labs(title = "Fuel efficiency generally decreases with engine size")

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Fuel efficiency generally decreases with engine size",
    subtitle = "Two seaters (sports cars) are an exception because of their light weight" ,
    caption = "data from fueleconomy.gov"
  )
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)"
  )
df <- tibble(x = runif(10), y = runif(10))  
ggplot(df, aes(x , y)) +geom_point() +
  labs(
    x = quote(sum(x[i] ^ 2, i ==1, n)),
    y = quote(alpha + beta + frac(delta, theta))
  )
best_in_class <- mpg %>%
  group_by(class) %>%
  filter(row_number(desc(hwy)) == 1)
best_in_class

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(colour = class)) +
  geom_text(aes(label = model), data = best_in_class)
library("ggrepel")
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_point(size = 3, shape = 1, data =  best_in_class) +
  ggrepel::geom_label_repel(aes(label = model), data = best_in_class)
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class))
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_color_discrete()
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_y_continuous(breaks = seq(15, 40, by = 5))
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL)
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  scale_y_log10()
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  scale_x_reverse()
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  theme(legend.position = "left")
ggplot(mpg, mapping = aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  coord_cartesian(xlim = c(5, 7), ylim = c(10, 30))
ggplot(mpg, mapping = aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  xlim(5, 7) + ylim(10, 30)
ggplot(mpg, mapping = aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  scale_x_continuous(limits = c(5, 7)) +
  scale_y_continuous(limits = c(10, 30))
 mpg %>%
   filter(displ >= 5, displ <= 7, hwy >= 10, hwy <= 30) %>%
   ggplot(aes(displ, hwy))+
   geom_point(aes(color = class)) +
   geom_smooth()
 ggplot(mpg, aes(displ, hwy)) +
   geom_point(aes(color = class)) +
   geom_smooth(se = FALSE) +
   theme_bw()
 ggplot(mpg, aes(displ, hwy)) + geom_point()
ggsave() 