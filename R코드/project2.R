
### 데이터전처리
dim(Q_dataset)
dim(Y_dataset)
## 결측치 처리

sum(is.na(Q_dataset))
sum(is.na(Y_dataset))

## 이상치 처리 (제거 or 대체)

# 이상치확인 방법

# 1) 기술통계량분석 (평균,중앙값,표준편차 계산/이상치가 있으면 기술통계량 왜곡)
summary(Q_dataset)
sapply(Q_dataset[,-1],sd)
summary(Y_dataset)
sapply(Y_dataset[,-1],sd)

# 2) 시각화 (boxplot,histogram,scatter plot)

boxplot(Q_dataset[,-1])

sapply(Q_dataset[,-1],hist)

plot(Q_dataset)

# 3) 이상치탐지알고리즘(대표적으로는 Isolation Forest, Local Outlier Factor(LOF), One-Class SVM )

## 이상치 대체방법 (중앙값,평균,가장가까운이웃값)
### 데이터가 정규분포를 따르는지 확인 ###
# 샤피로-윌크 검정: 대체로 적은 양의 데이터(200개 이하)

sapply(Q_dataset[,-1],shapiro.test) # marriage_cnt 만 비정규 : lm
sapply(Y_dataset[,-1],shapiro.test) # primary 만 정규  : robust

### IQR을 이용해서 데이터의 이상값 확인 ###
# 중앙값: 이상치에 덜 민감해서 전반적인 분포파악가능
# 평균: 데이터가 정규분포를 따를 때
# 가까운이웃값: 이상치 수가 적을 때 효과적

## 변수변환 : 변수 변환은 변수의 분포를 변환하는 작업
# 로그 변환 : 데이터가 오른쪽으로 치우친 경우
# 제곱근 변환 : 데이터가 왼쪽으로 치우친 경우
# Box-Cox 변환 : 데이터가 왜곡되어 있을 때
# Z-score 변환 : 데이터의 평균과 표준편차를 이용해 데이터를 변환

## 스케일링 : 스케일링은 변수의 크기(scale)를 조정하는 작업

# min-max 
# (x - min) / (max - min)
foreign[,2]; foreign$foreign2 

# z-score : scale
# (x - mean(x)) / sd(x)

Q_dataset_sc<-data.frame(scale(Q_dataset[,-1]))
Y_dataset_sc<-data.frame(scale(Y_dataset[,-1]))
# log


# Robust (IQR) #  (변수의 값 - 중앙값) / IQR
# (data - median(data)) / IQR(data)



# 데이터 합치기

# 등분산 자기상관성 다중공선성

# 외국인투자, 소비자물가지수, 결혼수, 내국인투자, 부동산, 

