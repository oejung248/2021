install.packages("tidyverse")
library(tidyverse)
library(nycflights13)
data(flights)

day25 <- flights %>% 
  filter(month == 1,
   day == 1)

jan1 <- flights %>% 
  filter(month == 12,
         day == 25)
#1월1일
ggjan1 <- ggplot(jan1, aes(x=dep_delay))+
  geom_histogram()

#12월 25일
ggdec25 <- ggplot(day25, aes(x=dep_delay))+
  geom_histogram()


april1 <- flights %>% 
  filter(month == 4,
         day == 1)

#4월1일
ggapril1 <- ggplot(april1, aes(x =dep_delay))+
  geom_histogram()

install.packages("egg")
library(egg)
ggarrange(ggjan1, ggdec25, ggapril1)

ggjan_arr <- ggplot(jan1, aes(x =arr_delay))+
  geom_histogram()
  
ggday25_arr <- ggplot(day25, aes(x =arr_delay))+
  geom_histogram()

ggapril_arr <- ggplot(april1, aes(x =arr_delay))+
  geom_histogram()

ggarrange(ggjan_arr, ggday25_arr, ggapril_arr)

ggjan_arrbox <- ggplot(jan1, aes(x= arr_delay))+
  geom_boxplot()

ggday25_arrbox <- ggplot(day25, aes(x= arr_delay))+
  geom_boxplot()

ggapril1_arrbox <- ggplot(april1, aes(x= arr_delay))+
  geom_boxplot()

ggarrange(ggjan_arrbox, ggday25_arrbox, ggapril1_arrbox)

#?
month1 <- flights %>% 
  filter(month %in% C(1, 4, 12))

flights %>% filter(arr_delay >= 120,
                   dep_delay <= 0)

flights %>% 
  arrange(desc(arr_delay, dep_delay))

#arrange를 범주형에 쓸수 있을까? factor, 문자 다 된다. 알파벳 순으로 정렬된다. 하지만 잘 안씀 ㅋㅋ

flights %>% 
  arrange(carrier)


group_1 <- flights %>% 
  select(year, month, day) 

flights %>% 
  select(year:day)

#distance부터 timehour까지 빼고싶을때, 꼭 둘다 - 써야함
flights %>% 
  select(-distance:-time_hour)

#select로 뽑으면서 rename가능
flights %>% 
  select(chulbalsigan = dep_time,
         tesangsigan = sched_dep_time)

#내가보고싶은 변수를 앞으로 당겨오고 싶을때 #everything
flights %>% 
  select(dest, everything())

#공통인 변수이름 뒤에 붙는걸로 뽑아줘   #ends_with ~지연인거 다 뽑아줌
flights %>% 
  select(ends_with("delay"))

#공통인 변수이름 앞에 붙는걸로 추려줘
flights %>% 
  select(starts_with("arr"))

flights %>% 
  select(starts_with("sched"))


#mutate는 변수를 만드는 것 
flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time)
flights_sml %>% 
  mutate(gain = dep_delay - arr_delay,
         speed = distance / air_time)

# true는 출발보다 도착이 늦은것?
#filter는 True값만 남긴다. !쓰면 반대로 False만 남긴다.
flights_sml %>% 
  mutate(ind = arr_delay > dep_delay) %>% 
  filter(ind)

flights_sml %>% 
  mutate(ind = arr_delay > dep_delay) %>% 
  filter(!ind)

flights %>% 
  filter(arr_delay > dep_delay)

flights_sml %>% 
  filter(!(arr_delay > dep_delay))

#엑셀의 피벗이 여기서 group_by, summarise 

#5.2.4연습문제
data(flights)
#1-1
delay <- flights %>% 
  select(arr_delay) %>% 
  filter(arr_delay > 120)
#1-2
flights %>% 
  select(dest) %>% 
  filter(dest == "IAH")
#1-2
flights %>% 
  select(dest) %>% 
  filter(dest == "HOU")

#1-3
flights %>% 
  select(carrier) %>% 
  filter(carrier %in% c("AA", "UA", "DL"))
#1-4
flights %>% 
  filter(month %in% c("7","8","9"))

#1-5
flights %>%
  filter(arr_delay >= 120, dep_delay <= 0)

#1-6
flights %>% 
  filter(dep_delay >= 60, arr_delay >= -30)

#1-7
ddd <- flights %>% 
  filter(dep_time <= 600|dep_time == 2400)


#2
#between()은 무엇을 하는 함수인가 

#3
table(!is.na(flights$dep_time))

#4


#5.3.1
#1 arrange를 써라
flights %>% 
  arrange(is.na(dep_time))

#2
ddd <- flights %>% 
  select(dep_delay) %>% 
  arrange(-dep_delay)

#3

#4
flights %>% 
  select(distance) %>% 
  arrange(-distance) %>% 
  head()

flights %>% 
  select(distance) %>% 
  arrange(distance) %>% 
  head()

#5.4.1
#1
#2
#3 묶어서 출력하는 함수이다.
vars <- c("year", "month", "day", "dep_delay", "arr_delay")

#4
select(flights, contains("TIME"))

#5.5.2
#1
#2

flights %>% 
  mutate(air_time, arr_time - dep_time)

#3 출발과 관련된 함수이다. 
  
#4
flights %>% 
  select(dep_delay, everything()) %>% 
  arrange(-dep_delay)

#5 ???
1:3 + 1:10
  
#6
#R이 제공하는 삼각 함수는??

#5.6.7

