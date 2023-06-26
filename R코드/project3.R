# 상관분석
cor(Q_dataset_sc)

# 회귀분석
### 


# 국내투자(총저축,총투자,GNDI) 와 (정규직수,비정규직수,근로시간,민간임금,공공임금) 사이의 회귀 

fit_saving_00<-lm(Y2_dataset_sc$saving~.,data=Y2_dataset_sc[,4:8])
summary(fit_saving_00)
fit_investment_00<-lm(Y2_dataset_sc$investment~.,data=Y2_dataset_sc[,4:8])
summary(fit_investment_00)
fit_GNDI_00<-lm(Y2_dataset_sc$GNDI~.,data=Y2_dataset_sc[,4:8])
summary(fit_GNDI_00)

# 외국인투자 0년후 
fit_test<-lm(Q_dataset_sc$service~Q_dataset_sc$customer2)
summary(fit_test)

fit_tot<-lm(Q_dataset_sc$total~.,, data=Q_dataset_sc[,6:9])
summary(fit_tot)
fit_pri<-lm(Q_dataset_sc$primary~.,data=Q_dataset_sc[,6:9])
summary(fit_pri)
fit_manu<-lm(Q_dataset_sc$manufacture~.,data=Q_dataset_sc[,6:9])
summary(fit_manu)
fit_ser<-lm(Q_dataset_sc$service~.,data=Q_dataset_sc[,6:9])
summary(fit_ser)
fit_manage<-lm(Q_dataset_sc$management~.,data=Q_dataset_sc[,6:9])
summary(fit_manage)

###
fit_saving<-lm(Q_dataset_sc$saving~.,data=Q_dataset_sc[,6:9])
summary(fit_saving)
fit_invest<-lm(Q_dataset_sc$investment~.,data=Q_dataset_sc[,6:9])
summary(fit_invest)
fit_GNDI<-lm(Q_dataset_sc$GNDI~.,data=Q_dataset_sc[,6:9])
summary(fit_GNDI)

#################

fit_Y_total<-lm(Y_dataset_sc$total~.,data=Y_dataset_sc[,6:10])
summary(fit_Y_total)
fit_Y_primary <-lm(Y_dataset_sc$primary ~.,data=Y_dataset_sc[,6:10])
summary(fit_Y_primary)
fit_Y_manufacture<-lm(Y_dataset_sc$manufacture~.,data=Y_dataset_sc[,6:10])
summary(fit_Y_manufacture)
fit_Y_service<-lm(Y_dataset_sc$service~.,data=Y_dataset_sc[,6:10])
summary(fit_Y_service)
fit_Y_management<-lm(Y_dataset_sc$management~.,data=Y_dataset_sc[,6:10])
summary(fit_Y_management)

#################
# 외국인 투자 1년후
fit_tot_1<-lm(Q_dataset_sc_1$total~.,, data=Q_dataset_sc_1[,6:9])
summary(fit_tot_1)
fit_pri_1<-lm(Q_dataset_sc_1$primary~.,data=Q_dataset_sc_1[,6:9])
summary(fit_pri_1)
fit_manu_1<-lm(Q_dataset_sc_1$manufacture~.,data=Q_dataset_sc_1[,6:9])
summary(fit_manu_1)
fit_ser_1<-lm(Q_dataset_sc_1$service~.,data=Q_dataset_sc_1[,6:9])
summary(fit_ser_1)
fit_manage_1<-lm(Q_dataset_sc_1$management~.,data=Q_dataset_sc_1[,6:9])
summary(fit_manage_1)


###
fit_saving_1<-lm(Q_dataset_sc_1$saving~.,data=Q_dataset_sc_1[,6:9])
summary(fit_saving_1)
fit_invest_1<-lm(Q_dataset_sc_1$investment~.,data=Q_dataset_sc_1[,6:9])
summary(fit_invest_1)
fit_GNDI_1<-lm(Q_dataset_sc_1$GNDI~.,data=Q_dataset_sc_1[,6:9])
summary(fit_GNDI_1)

#################

fit_Y_total_1<-lm(Y_dataset_sc_1$total~.,data=Y_dataset_sc_1[,6:10])
summary(fit_Y_total_1)
fit_Y_primary_1 <-lm(Y_dataset_sc_1$primary ~.,data=Y_dataset_sc_1[,6:10])
summary(fit_Y_primary_1)
fit_Y_manufacture_1<-lm(Y_dataset_sc_1$manufacture~.,data=Y_dataset_sc_1[,6:10])
summary(fit_Y_manufacture_1)
fit_Y_service_1<-lm(Y_dataset_sc_1$service~.,data=Y_dataset_sc_1[,6:10])
summary(fit_Y_service_1)
fit_Y_management_1<-lm(Y_dataset_sc_1$management~.,data=Y_dataset_sc_1[,6:10])
summary(fit_Y_management_1)

# 외국인 투자 2년후
fit_tot_2<-lm(Q_dataset_sc_2$total~.,, data=Q_dataset_sc_2[,6:9])
summary(fit_tot_2)
fit_pri_2<-lm(Q_dataset_sc_2$primary~.,data=Q_dataset_sc_2[,6:9])
summary(fit_pri_2)
fit_manu_2<-lm(Q_dataset_sc_2$manufacture~.,data=Q_dataset_sc_2[,6:9])
summary(fit_manu_2)
fit_ser_2<-lm(Q_dataset_sc_2$service~.,data=Q_dataset_sc_2[,6:9])
summary(fit_ser_2)
fit_manage_2<-lm(Q_dataset_sc_2$management~.,data=Q_dataset_sc_2[,6:9])
summary(fit_manage_2)


###
fit_saving_2<-lm(Q_dataset_sc_2$saving~.,data=Q_dataset_sc_2[,6:9])
summary(fit_saving_2)
fit_invest_2<-lm(Q_dataset_sc_2$investment~.,data=Q_dataset_sc_2[,6:9])
summary(fit_invest_2)
fit_GNDI_2<-lm(Q_dataset_sc_2$GNDI~.,data=Q_dataset_sc_2[,6:9])
summary(fit_GNDI_2)

#################

head(Y_dataset_sc)
fit_Y_total_2<-lm(Y_dataset_sc_2$total~.,data=Y_dataset_sc_2[,6:10])
summary(fit_Y_total_2)
fit_Y_primary_2 <-lm(Y_dataset_sc_2$primary ~.,data=Y_dataset_sc_2[,6:10])
summary(fit_Y_primary_2)
fit_Y_manufacture_2<-lm(Y_dataset_sc_2$manufacture~.,data=Y_dataset_sc_2[,6:10])
summary(fit_Y_manufacture_2)
fit_Y_service_2<-lm(Y_dataset_sc_2$service~.,data=Y_dataset_sc_2[,6:10])
summary(fit_Y_service_2)
fit_Y_management_2<-lm(Y_dataset_sc_2$management~.,data=Y_dataset_sc_2[,6:10])
summary(fit_Y_management_2)

# 외국인 투자 3년후
fit_tot_3<-lm(Q_dataset_sc_3$total~.,, data=Q_dataset_sc_3[,6:9])
summary(fit_tot_3)
fit_pri_3<-lm(Q_dataset_sc_3$primary~.,data=Q_dataset_sc_3[,6:9])
summary(fit_pri_3)
fit_manu_3<-lm(Q_dataset_sc_3$manufacture~.,data=Q_dataset_sc_3[,6:9])
summary(fit_manu_3)
fit_ser_3<-lm(Q_dataset_sc_3$service~.,data=Q_dataset_sc_3[,6:9])
summary(fit_ser_3)
fit_manage_3<-lm(Q_dataset_sc_3$management~.,data=Q_dataset_sc_3[,6:9])
summary(fit_manage_3)


###
fit_saving_3<-lm(Q_dataset_sc_3$saving~.,data=Q_dataset_sc_3[,6:9])
summary(fit_saving_3)
fit_invest_3<-lm(Q_dataset_sc_3$investment~.,data=Q_dataset_sc_3[,6:9])
summary(fit_invest_3)
fit_GNDI_3<-lm(Q_dataset_sc_3$GNDI~.,data=Q_dataset_sc_3[,6:9])
summary(fit_GNDI_3)

#################

fit_Y_total_3<-lm(Y_dataset_sc_3$total~.,data=Y_dataset_sc_3[,6:10])
summary(fit_Y_total_3)
fit_Y_primary_3 <-lm(Y_dataset_sc_3$primary ~.,data=Y_dataset_sc_3[,6:10])
summary(fit_Y_primary_3)
fit_Y_manufacture_3<-lm(Y_dataset_sc_3$manufacture~.,data=Y_dataset_sc_3[,6:10])
summary(fit_Y_manufacture_3)
fit_Y_service_3<-lm(Y_dataset_sc_3$service~.,data=Y_dataset_sc_3[,6:10])
summary(fit_Y_service_3)
fit_Y_management_3<-lm(Y_dataset_sc_3$management~.,data=Y_dataset_sc_3[,6:10])
summary(fit_Y_management_3)


# 모델이 통계적으로 유의미한가? (F통계량)
# F-statistic:

# 계수가 유의미한가? (t value)


# 모델의 설명력 (결정계수)
# 결정계수 R^2 , 

# 데이터에 적용된 선형회귀는 적절한가
cor(data)



#2009~2019
work_time
per_temp_work
wage

# 자기상관성 다중공선성 
# dwtest viftest