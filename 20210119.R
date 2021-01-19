install.packages("tidyverse")
library(tidyverse)
library(dplyr)
install.packages(c("nycflights13", "Lahman", "gapminder"))
data(mpg)
mpg

ggplot(mpg, aes(displ, hwy))+geom_point()

ggplot(mpg)+
  geom_point(aes(x = displ, 
                 y = hwy,
                 color = class, #색 
                 size = 0.8, #크기
                 alpha= 0.5)) #명암, Shape =모양

ggplot(mpg)+
  geom_point(aes(x = displ, 
                 y = hwy,
                 color = class))+
  facet_wrap(~class) #화면 분할
                 
ggplot(mpg)+
  geom_point(aes(x = displ, 
                 y = hwy,
                 color = class))+
  facet_wrap(~class,
             ncol = 2)
                 

ggplot(mpg)+
  geom_point(aes(x = displ, 
                 y = hwy,
                 color = class))+
  facet_wrap(~class,
             nrow = 2)

ggplot(mpg)+
  geom_point(aes(x = displ, 
                 y = hwy,
                 color = class))+
  facet_grid(drv ~ cyl)


ggplot(mpg, aes(x = displ, 
                y = hwy))+
  geom_point()+
  geom_smooth(method = "lm") #method ="lm" 선형회귀선 추가


ggplot(mpg, aes(x = displ, 
                y = hwy))+
  geom_point(aes(color = class))+ #변수마다 색 추가
  geom_smooth(method = "lm")

#diamonds
data("diamonds")
diamonds

ggplot(diamonds,aes(x= cut))+
  geom_bar()

ggplot(diamonds)+
  geom_bar(aes(x= cut, y = stat(prop),
                    group = 1))


ggplot(diamonds)+
  geom_bar(aes(x= cut, fill = cut))

#가로
ggplot(diamonds)+
  geom_bar(aes(x= cut, fill = clarity))

#세로
ggplot(diamonds)+
  geom_bar(aes(x= cut, fill = clarity),
           position = "dodge")

install.packages('maps')
nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group))+  #geom_polygon 지도 그리는 함수
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group))+  #geom_polygon 지도 그리는 함수
  geom_polygon(fill = "white", colour = "black")+
  coord_quickmap()
       



#3.24
#1
data(mpg)
ggplot(data = mpg)

#2
length(mpg)
dim(mpg)

#3
?mpg

#4
ggplot(mpg, aes(x = hwy, y= cyl))+
  geom_point()  

plot(mpg$hwy.Length)
plot(mpg$cyl.Length)

#5
ggplot(mpg, aes(x = class, y= drv))+
  geom_point()  
#뭉쳐있지 않아서? 범주형이라서 

#3.3.1
#1 hwy뒤에 괄호닫고 반점 안써서 그럼 
ggplot(data=mpg)+
  geom_point(mapping = aes(x = displ,
                           y = hwy), 
             color = "blue")

#2
?mpg
class(mpg$manufacturer)
class(mpg$model)
class(mpg$displ)
class(mpg$year)
class(mpg$cyl)
class(mpg$trans)
class(mpg$drv)
class(mpg$cty)
class(mpg$hwy)
class(mpg$fl)
class(mpg$class)

#3
ggplot(mpg, aes(x= displ,
                y = cty,
                color = cty,
                size = 0.5))+geom_point()


ggplot(mpg, aes(x= displ,
                y = class,
                color = class,
                size = 0.5))+geom_point()

#4 실행 잘 된다

#5
?geom_point
#stroke는 선그래프 굵기 설정에 쓰인다

#6
aes(color = displ < 5)
#오류가 나겠죠?

#1.5.1
#1 연속형변수로 면분할 하면 알아보기 힘들게 된다. 범주형으로 면분할 하는 게 맞는 것 같다. 
ggplot(mpg)+
  geom_point(aes(x = displ, 
                 y = hwy))+
  facet_wrap(~cty) 

#2 해당되는게 없다.
ggplot(data = mpg)+
  geom_point(mapping = aes(x = drv, y = cyl))+
  facet_grid(~) 


#3
ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y = hwy))+
  facet_grid(drv~.) #.은 모두포함의 의미

ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y =hwy))+
  facet_grid(. ~cyl)

#4
ggplot(data =mpg)+
  geom_point(mapping = aes(x= displ, y = hwy))+
  facet_wrap(~ class, nrow=2)
 #색깔을 나누지 않고 면 분할 하면 분포만 볼수 있다. 단점은 뭐별로 나눈지 모름 

#5
?facet_wrap #Number of rows and columns

#6
#고유수준이 더 많은 걸로 해야 조금 더 세분화해서 분류가 가능하다.

mpg %>% 
  group_by(manufacturer) %>% 
  summarise(n = n())

mpg %>% 
  group_by(model) %>% 
  summarise(n = n())

#1.6.1
#1 
#geom_line, geom_boxplot, geom_histogram, geom_area

#2
ggplot(data = mpg, mapping = aes( x= displ, y = hwy, color =drv))+
  geom_point()+
  geom_smooth(se = FALSE)

#3
ggplot(data = mpg, mapping = aes( x= displ, y = hwy, color =drv))+
  geom_point()+
  geom_smooth(show.legend = FALSE)

#4
ggplot(data = mpg, mapping = aes( x= displ, y = hwy, color =drv))+
  geom_point()+
  geom_smooth()
#se는 표본오차
#5
#다음 두 그래프는 같게 나타난다.


#1.7.1
#1 stat_summary()와 연관된 기본지옴은 boxplot이다.

#2
#geom_col()의 역할은 변수가 하나인 막대그래프 geom_bar() 변수가2개이상인 막대그래프 이다.

#3

#4
#stat_smooth()는 굴곡선을 그리는 옵션, 연속

#5 prop는 백분율 인데, group을 주면 함수 개개인의 빈도백분율이아닌 전체 변수의 백분율을 나타낸다. 
ggplot(data = diamonds)+
  geom_bar(mapping = aes(x = cut, y = ..prop..))

#그룹으로 나누면 x축 기준으로 
ggplot(data = diamonds)+
  geom_bar(mapping = aes(x = cut, y = ..prop..,
           group = 1))

ggplot(data = diamonds)+
  geom_bar(mapping =  aes(x = cut, fill = color, y = ..prop..))

ggplot(data = diamonds)+
  geom_bar(mapping =  aes(x = cut, fill = color, y = ..prop..,group = 1))

