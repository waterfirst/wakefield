# wakefield
https://github.com/trinker/wakefield


if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("trinker/wakefield")
pacman::p_load(dplyr, tidyr, ggplot2)

library(wakefield)

race(n=10)

attributes(race(n=10))



#임의의 데이터 프레임 만들기 (49개 항목) r_data_frame()
# 나이	주사위	머리카락	군대	섹스\_포함
# 동물	DNA	키	월	담배를 피우다
# 대답	도브	소득	이름	속도
# 지역	가짜의	인터넷 브라우저	정상	상태
# 자동차	교육	아이큐	정치적인	끈
# 어린이들	고용	언어	경주	높은
# 동전	눈	수준	종교	유효한
# 색깔	등급	리커트	수능	년도
# 날짜\_스탬프	등급\_레벨	lorem\_ipsum	문장	우편 번호
# 죽음	그룹	결혼	섹스	
 
# age	dice	hair	military	sex\_inclusive
# animal	dna	height	month	smokes
# answer	dob	income	name	speed
# area	dummy	internet\_browser	normal	state
# car	education	iq	political	string
# children	employment	language	race	upper
# coin	eye	level	religion	valid
# color	grade	likert	sat	year
# date\_stamp	grade\_level	lorem\_ipsum	sentence	zip\_code
# death	group	marital	sex	


r_data_frame(
  n = 500,
  race
)

r_data_frame(
  n = 500,
  id,
  race,
  age,
  sex,
  hour,
  iq,
  height,
  died
)


r_data_frame(
  n = 500,
  id,
  age, age, age,
  grade, grade, grade
)



r_data_frame(
  n = 500,
  id,
  Scoring = rnorm,
  Smoker = valid,
  race,
  age,
  sex,
  hour,
  iq,
  height,
  died
)


r_data_frame(
  n = 500,
  id,
  Scoring = rnorm,
  Smoker = valid,
  `Reading(mins)` = rpois(lambda=20),  #포아송 분포, 평균 20
  race,
  age(x = 8:14),
  sex,
  hour,
  iq,
  height(mean=50, sd = 10),
  died
)

# 무작위로 결측치를 넣을때
r_data_frame(
  n = 30,
  id,
  race,
  age,
  sex,
  hour,
  iq,
  height,
  died,
  Scoring = rnorm,
  Smoker = valid
) %>%
  r_na(prob=.4)


# 시계열 데이터 만들때 (j : 열수)
set.seed(10)
r_series(likert, j = 3, n=10)
help("likert") # 설문조사시 5종, 강한긍정 ~ 강한부정

set.seed(10)
as_integer(r_series(likert, j = 5, n=10, name = "Item")) # 숫자로 


set.seed(10)
r_data_frame(n=100,
             id,
             age,
             sex,
             r_series(likert, 3, name = "Question") #3종 설문조사 질문을 넣어 만듦
)




set.seed(10)
r_data_frame(n=100,
             id,
             age,
             sex,
             r_series(likert, 5, name = "Item", integer = TRUE) #숫자로 5종 설문조사 대답
)

#r_data_frame 이 아니라 r_seriese 로도 만들수 있음

r_series(grade, j = 5, n = 100, relate = "+1_6") %>% 
  ggplot(aes(Grade_1))+geom_histogram()


#relate : fM_sd : f(+, -, *, /), M(mean), sd(stard deviation)
r_series(age, 5, 100, relate = "+5_0")
r_series(likert, 5,  100, name ="Item", relate = "-.5_.1")
r_series(grade, j = 5, n = 100, relate = "*1.05_.1")

library(corrplot)
corrplot(round(cor(r_series(grade, 8, 10, relate = "-1_2")), 2))
round(cor(r_series(grade, 8, 10, relate = "+1_2")), 2)
round(cor(r_series(grade, 8, 10, relate = "+1_0")), 2)

round(cor(r_series(grade, 8, 10, relate = "+1_20")), 2)
round(cor(r_series(grade, 8, 10, relate = "+15_20")), 2)



dat <- r_data_frame(12,
                    name,
                    r_series(grade, 100, relate = "-3_6")
) 

#시각화 (Pivot_longer 대신 gather를 사용)
dat %>%
  gather(Time, Grade, -c(Name)) %>%
  mutate(Time = as.numeric(gsub("\\D", "", Time))) %>%
  ggplot(aes(x = Time, y = Grade, color = Name, group = Name)) +
  geom_line(size=.8) + 
  theme_bw()



set.seed(10)
r_data_frame(n=100,
             id,
             age,
             r_dummy(sex, prefix = TRUE), #0, 1로 dummy 변수로 만듦
             r_dummy(political)
)


set.seed(10)

r_data_frame(n=100,
             id, #문자
             dob, #생일 날짜
             animal,#범주(동물)
             grade, grade, #숫자/ 학점
             death, #사망여부
             dummy, #0, 1
             grade_letter, #A+ ~ F
             gender,#Male, Female
             paragraph, #단락
             sentence #문장
) %>%
  r_na() %>% #중간중간 NA 넣기
  plot(palette = "Set1") #컬럼별 변주 종류 확인, NA 포함


