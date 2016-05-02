#===============================================================================================================
#| Purpose		: XGBoost on Otto Group Product Classification Challenge
#| Created by 	: Shashank Sharma | Chidambara Natarajan
#| Subject		: XGBoost
#===============================================================================================================

##| Setting up the working directory

getwd()
setwd("C:\\DATA\\Fall 15\\CIS 508\\02 Assignments\\01 Team Assignment - Kaggle\\Product Classification\\02 Data\\")
getwd()

####| Importing Packages

require(xgboost)
## Loading required package: xgboost
require(methods)
## Loading required package: methods
require(data.table)
## Loading required package: data.table
require(magrittr)
## Loading required package: magrittr

## Reading datasets
train <- read.csv("train-3.csv")
#str(train)
test <- read.csv("test.csv")
 
# Delete ID column
train[, id := NULL]
test[, id := NULL]
train[1:6, ncol(train), with  = F]

# Save the name of the last column
nameLastCol <- names(train)[ncol(train)]

# Convert from classes to numbers
train$count=rowSums(train!=0)
test$count=rowSums(test!=0)
write.csv(train,file="Otto_train_1.csv",row.names=TRUE)
write.csv(test,file="Otto_test_2.csv",row.names=TRUE)
y <- train[, nameLastCol, with = F][[1]] %>% gsub('Class_','',.) %>% {as.integer(.) -1}
# Display the first 5 levels
y[1:5]
## Set the target field to NULL to drop of the column
train[, nameLastCol:=NULL, with = F]

library(plyr)
### Train Feature extraction starts 
#feat1_tab=count(train,"feat_1")
feat1_tab=count(train,"feat_1")
feat2_tab=count(train,"feat_2")
feat3_tab=count(train,"feat_3")
feat4_tab=count(train,"feat_4")
feat5_tab=count(train,"feat_5")
feat6_tab=count(train,"feat_6")
feat7_tab=count(train,"feat_7")
feat8_tab=count(train,"feat_8")
feat9_tab=count(train,"feat_9")
feat10_tab=count(train,"feat_10")
feat11_tab=count(train,"feat_11")
feat12_tab=count(train,"feat_12")
feat13_tab=count(train,"feat_13")
feat14_tab=count(train,"feat_14")
feat15_tab=count(train,"feat_15")
feat16_tab=count(train,"feat_16")
feat17_tab=count(train,"feat_17")
feat18_tab=count(train,"feat_18")
feat19_tab=count(train,"feat_19")
feat20_tab=count(train,"feat_20")
feat19_tab=count(train,"feat_19")
feat20_tab=count(train,"feat_20")
feat21_tab=count(train,"feat_21")
feat22_tab=count(train,"feat_22")
feat23_tab=count(train,"feat_23")
feat24_tab=count(train,"feat_24")
feat25_tab=count(train,"feat_25")
feat26_tab=count(train,"feat_26")
feat27_tab=count(train,"feat_27")
feat28_tab=count(train,"feat_28")
feat29_tab=count(train,"feat_29")
feat30_tab=count(train,"feat_30")
feat31_tab=count(train,"feat_31")
feat32_tab=count(train,"feat_32")
feat33_tab=count(train,"feat_33")
feat34_tab=count(train,"feat_34")
feat35_tab=count(train,"feat_35")
feat36_tab=count(train,"feat_36")
feat37_tab=count(train,"feat_37")
feat38_tab=count(train,"feat_38")
feat39_tab=count(train,"feat_39")
feat40_tab=count(train,"feat_40")
feat41_tab=count(train,"feat_41")
feat42_tab=count(train,"feat_42")
feat43_tab=count(train,"feat_43")
feat44_tab=count(train,"feat_44")
feat45_tab=count(train,"feat_45")
feat46_tab=count(train,"feat_46")
feat47_tab=count(train,"feat_47")
feat48_tab=count(train,"feat_48")
feat49_tab=count(train,"feat_49")
feat50_tab=count(train,"feat_50")
feat51_tab=count(train,"feat_51")
feat52_tab=count(train,"feat_52")
feat53_tab=count(train,"feat_53")
feat54_tab=count(train,"feat_54")
feat55_tab=count(train,"feat_55")
feat56_tab=count(train,"feat_56")
feat57_tab=count(train,"feat_57")
feat58_tab=count(train,"feat_58")
feat59_tab=count(train,"feat_59")
feat60_tab=count(train,"feat_60")
feat61_tab=count(train,"feat_61")
feat62_tab=count(train,"feat_62")
feat63_tab=count(train,"feat_63")
feat64_tab=count(train,"feat_64")
feat65_tab=count(train,"feat_65")
feat66_tab=count(train,"feat_66")
feat67_tab=count(train,"feat_67")
feat68_tab=count(train,"feat_68")
feat69_tab=count(train,"feat_69")
feat70_tab=count(train,"feat_70")
feat71_tab=count(train,"feat_71")
feat72_tab=count(train,"feat_72")
feat73_tab=count(train,"feat_73")
feat74_tab=count(train,"feat_74")
feat75_tab=count(train,"feat_75")
feat76_tab=count(train,"feat_76")
feat77_tab=count(train,"feat_77")
feat78_tab=count(train,"feat_78")
feat79_tab=count(train,"feat_79")
feat80_tab=count(train,"feat_80")
feat81_tab=count(train,"feat_81")
feat82_tab=count(train,"feat_82")
feat83_tab=count(train,"feat_83")
feat84_tab=count(train,"feat_84")
feat85_tab=count(train,"feat_85")
feat86_tab=count(train,"feat_86")
feat87_tab=count(train,"feat_87")
feat88_tab=count(train,"feat_88")
feat89_tab=count(train,"feat_89")
feat90_tab=count(train,"feat_90")
feat91_tab=count(train,"feat_91")
feat92_tab=count(train,"feat_92")
feat93_tab=count(train,"feat_93")
feat1_tab

#plyr1 <- join(train, feat1_tab, by = "feat_1")
plyr1 <- join(train,feat1_tab, by ="feat_1")
train$feat_1_cnt<-plyr1$freq
plyr1 <- join(train,feat2_tab, by ="feat_2")
train$feat_2_cnt<-plyr1$freq
plyr1 <- join(train,feat3_tab, by ="feat_3")
train$feat_3_cnt<-plyr1$freq
plyr1 <- join(train,feat4_tab, by ="feat_4")
train$feat_4_cnt<-plyr1$freq
plyr1 <- join(train,feat5_tab, by ="feat_5")
train$feat_5_cnt<-plyr1$freq
plyr1 <- join(train,feat6_tab, by ="feat_6")
train$feat_6_cnt<-plyr1$freq
plyr1 <- join(train,feat7_tab, by ="feat_7")
train$feat_7_cnt<-plyr1$freq
plyr1 <- join(train,feat8_tab, by ="feat_8")
train$feat_8_cnt<-plyr1$freq
plyr1 <- join(train,feat9_tab, by ="feat_9")
train$feat_9_cnt<-plyr1$freq
plyr1 <- join(train,feat10_tab, by ="feat_10")
train$feat_10_cnt<-plyr1$freq
plyr1 <- join(train,feat11_tab, by ="feat_11")
train$feat_11_cnt<-plyr1$freq
plyr1 <- join(train,feat12_tab, by ="feat_12")
train$feat_12_cnt<-plyr1$freq
plyr1 <- join(train,feat13_tab, by ="feat_13")
train$feat_13_cnt<-plyr1$freq
plyr1 <- join(train,feat14_tab, by ="feat_14")
train$feat_14_cnt<-plyr1$freq
plyr1 <- join(train,feat15_tab, by ="feat_15")
train$feat_15_cnt<-plyr1$freq
plyr1 <- join(train,feat16_tab, by ="feat_16")
train$feat_16_cnt<-plyr1$freq
plyr1 <- join(train,feat17_tab, by ="feat_17")
train$feat_17_cnt<-plyr1$freq
plyr1 <- join(train,feat18_tab, by ="feat_18")
train$feat_18_cnt<-plyr1$freq
plyr1 <- join(train,feat19_tab, by ="feat_19")
train$feat_19_cnt<-plyr1$freq
plyr1 <- join(train,feat20_tab, by ="feat_20")
train$feat_20_cnt<-plyr1$freq
plyr1 <- join(train,feat21_tab, by ="feat_21")
train$feat_21_cnt<-plyr1$freq
plyr1 <- join(train,feat22_tab, by ="feat_22")
train$feat_22_cnt<-plyr1$freq
plyr1 <- join(train,feat23_tab, by ="feat_23")
train$feat_23_cnt<-plyr1$freq
plyr1 <- join(train,feat24_tab, by ="feat_24")
train$feat_24_cnt<-plyr1$freq
plyr1 <- join(train,feat25_tab, by ="feat_25")
train$feat_25_cnt<-plyr1$freq
plyr1 <- join(train,feat26_tab, by ="feat_26")
train$feat_26_cnt<-plyr1$freq
plyr1 <- join(train,feat27_tab, by ="feat_27")
train$feat_27_cnt<-plyr1$freq
plyr1 <- join(train,feat28_tab, by ="feat_28")
train$feat_28_cnt<-plyr1$freq
plyr1 <- join(train,feat29_tab, by ="feat_29")
train$feat_29_cnt<-plyr1$freq
plyr1 <- join(train,feat30_tab, by ="feat_30")
train$feat_30_cnt<-plyr1$freq
plyr1 <- join(train,feat31_tab, by ="feat_31")
train$feat_31_cnt<-plyr1$freq
plyr1 <- join(train,feat32_tab, by ="feat_32")
train$feat_32_cnt<-plyr1$freq
plyr1 <- join(train,feat33_tab, by ="feat_33")
train$feat_33_cnt<-plyr1$freq
plyr1 <- join(train,feat34_tab, by ="feat_34")
train$feat_34_cnt<-plyr1$freq
plyr1 <- join(train,feat35_tab, by ="feat_35")
train$feat_35_cnt<-plyr1$freq
plyr1 <- join(train,feat36_tab, by ="feat_36")
train$feat_36_cnt<-plyr1$freq
plyr1 <- join(train,feat37_tab, by ="feat_37")
train$feat_37_cnt<-plyr1$freq
plyr1 <- join(train,feat38_tab, by ="feat_38")
train$feat_38_cnt<-plyr1$freq
plyr1 <- join(train,feat39_tab, by ="feat_39")
train$feat_39_cnt<-plyr1$freq
plyr1 <- join(train,feat40_tab, by ="feat_40")
train$feat_40_cnt<-plyr1$freq
plyr1 <- join(train,feat41_tab, by ="feat_41")
train$feat_41_cnt<-plyr1$freq
plyr1 <- join(train,feat42_tab, by ="feat_42")
train$feat_42_cnt<-plyr1$freq
plyr1 <- join(train,feat43_tab, by ="feat_43")
train$feat_43_cnt<-plyr1$freq
plyr1 <- join(train,feat44_tab, by ="feat_44")
train$feat_44_cnt<-plyr1$freq
plyr1 <- join(train,feat45_tab, by ="feat_45")
train$feat_45_cnt<-plyr1$freq
plyr1 <- join(train,feat46_tab, by ="feat_46")
train$feat_46_cnt<-plyr1$freq
plyr1 <- join(train,feat47_tab, by ="feat_47")
train$feat_47_cnt<-plyr1$freq
plyr1 <- join(train,feat48_tab, by ="feat_48")
train$feat_48_cnt<-plyr1$freq
plyr1 <- join(train,feat49_tab, by ="feat_49")
train$feat_49_cnt<-plyr1$freq
plyr1 <- join(train,feat50_tab, by ="feat_50")
train$feat_50_cnt<-plyr1$freq
plyr1 <- join(train,feat51_tab, by ="feat_51")
train$feat_51_cnt<-plyr1$freq
plyr1 <- join(train,feat52_tab, by ="feat_52")
train$feat_52_cnt<-plyr1$freq
plyr1 <- join(train,feat53_tab, by ="feat_53")
train$feat_53_cnt<-plyr1$freq
plyr1 <- join(train,feat54_tab, by ="feat_54")
train$feat_54_cnt<-plyr1$freq
plyr1 <- join(train,feat55_tab, by ="feat_55")
train$feat_55_cnt<-plyr1$freq
plyr1 <- join(train,feat56_tab, by ="feat_56")
train$feat_56_cnt<-plyr1$freq
plyr1 <- join(train,feat57_tab, by ="feat_57")
train$feat_57_cnt<-plyr1$freq
plyr1 <- join(train,feat58_tab, by ="feat_58")
train$feat_58_cnt<-plyr1$freq
plyr1 <- join(train,feat59_tab, by ="feat_59")
train$feat_59_cnt<-plyr1$freq
plyr1 <- join(train,feat60_tab, by ="feat_60")
train$feat_60_cnt<-plyr1$freq
plyr1 <- join(train,feat61_tab, by ="feat_61")
train$feat_61_cnt<-plyr1$freq
plyr1 <- join(train,feat62_tab, by ="feat_62")
train$feat_62_cnt<-plyr1$freq
plyr1 <- join(train,feat63_tab, by ="feat_63")
train$feat_63_cnt<-plyr1$freq
plyr1 <- join(train,feat64_tab, by ="feat_64")
train$feat_64_cnt<-plyr1$freq
plyr1 <- join(train,feat65_tab, by ="feat_65")
train$feat_65_cnt<-plyr1$freq
plyr1 <- join(train,feat66_tab, by ="feat_66")
train$feat_66_cnt<-plyr1$freq
plyr1 <- join(train,feat67_tab, by ="feat_67")
train$feat_67_cnt<-plyr1$freq
plyr1 <- join(train,feat68_tab, by ="feat_68")
train$feat_68_cnt<-plyr1$freq
plyr1 <- join(train,feat69_tab, by ="feat_69")
train$feat_69_cnt<-plyr1$freq
plyr1 <- join(train,feat70_tab, by ="feat_70")
train$feat_70_cnt<-plyr1$freq
plyr1 <- join(train,feat71_tab, by ="feat_71")
train$feat_71_cnt<-plyr1$freq
plyr1 <- join(train,feat72_tab, by ="feat_72")
train$feat_72_cnt<-plyr1$freq
plyr1 <- join(train,feat73_tab, by ="feat_73")
train$feat_73_cnt<-plyr1$freq
plyr1 <- join(train,feat74_tab, by ="feat_74")
train$feat_74_cnt<-plyr1$freq
plyr1 <- join(train,feat75_tab, by ="feat_75")
train$feat_75_cnt<-plyr1$freq
plyr1 <- join(train,feat76_tab, by ="feat_76")
train$feat_76_cnt<-plyr1$freq
plyr1 <- join(train,feat77_tab, by ="feat_77")
train$feat_77_cnt<-plyr1$freq
plyr1 <- join(train,feat78_tab, by ="feat_78")
train$feat_78_cnt<-plyr1$freq
plyr1 <- join(train,feat79_tab, by ="feat_79")
train$feat_79_cnt<-plyr1$freq
plyr1 <- join(train,feat80_tab, by ="feat_80")
train$feat_80_cnt<-plyr1$freq
plyr1 <- join(train,feat81_tab, by ="feat_81")
train$feat_81_cnt<-plyr1$freq
plyr1 <- join(train,feat82_tab, by ="feat_82")
train$feat_82_cnt<-plyr1$freq
plyr1 <- join(train,feat83_tab, by ="feat_83")
train$feat_83_cnt<-plyr1$freq
plyr1 <- join(train,feat84_tab, by ="feat_84")
train$feat_84_cnt<-plyr1$freq
plyr1 <- join(train,feat85_tab, by ="feat_85")
train$feat_85_cnt<-plyr1$freq
plyr1 <- join(train,feat86_tab, by ="feat_86")
train$feat_86_cnt<-plyr1$freq
plyr1 <- join(train,feat87_tab, by ="feat_87")
train$feat_87_cnt<-plyr1$freq
plyr1 <- join(train,feat88_tab, by ="feat_88")
train$feat_88_cnt<-plyr1$freq
plyr1 <- join(train,feat89_tab, by ="feat_89")
train$feat_89_cnt<-plyr1$freq
plyr1 <- join(train,feat90_tab, by ="feat_90")
train$feat_90_cnt<-plyr1$freq
plyr1 <- join(train,feat91_tab, by ="feat_91")
train$feat_91_cnt<-plyr1$freq
plyr1 <- join(train,feat92_tab, by ="feat_92")
train$feat_92_cnt<-plyr1$freq
plyr1 <- join(train,feat93_tab, by ="feat_93")
train$feat_93_cnt<-plyr1$freq

### Train Feature extraction ends
## Test feature extraction
feat1_tab_test=count(test,"feat_1")
feat2_tab_test=count(test,"feat_2")
feat3_tab_test=count(test,"feat_3")
feat4_tab_test=count(test,"feat_4")
feat5_tab_test=count(test,"feat_5")
feat6_tab_test=count(test,"feat_6")
feat7_tab_test=count(test,"feat_7")
feat8_tab_test=count(test,"feat_8")
feat9_tab_test=count(test,"feat_9")
feat10_tab_test=count(test,"feat_10")
feat11_tab_test=count(test,"feat_11")
feat12_tab_test=count(test,"feat_12")
feat13_tab_test=count(test,"feat_13")
feat14_tab_test=count(test,"feat_14")
feat15_tab_test=count(test,"feat_15")
feat16_tab_test=count(test,"feat_16")
feat17_tab_test=count(test,"feat_17")
feat18_tab_test=count(test,"feat_18")
feat19_tab_test=count(test,"feat_19")
feat20_tab_test=count(test,"feat_20")
feat19_tab_test=count(test,"feat_19")
feat20_tab_test=count(test,"feat_20")
feat21_tab_test=count(test,"feat_21")
feat22_tab_test=count(test,"feat_22")
feat23_tab_test=count(test,"feat_23")
feat24_tab_test=count(test,"feat_24")
feat25_tab_test=count(test,"feat_25")
feat26_tab_test=count(test,"feat_26")
feat27_tab_test=count(test,"feat_27")
feat28_tab_test=count(test,"feat_28")
feat29_tab_test=count(test,"feat_29")
feat30_tab_test=count(test,"feat_30")
feat31_tab_test=count(test,"feat_31")
feat32_tab_test=count(test,"feat_32")
feat33_tab_test=count(test,"feat_33")
feat34_tab_test=count(test,"feat_34")
feat35_tab_test=count(test,"feat_35")
feat36_tab_test=count(test,"feat_36")
feat37_tab_test=count(test,"feat_37")
feat38_tab_test=count(test,"feat_38")
feat39_tab_test=count(test,"feat_39")
feat40_tab_test=count(test,"feat_40")
feat41_tab_test=count(test,"feat_41")
feat42_tab_test=count(test,"feat_42")
feat43_tab_test=count(test,"feat_43")
feat44_tab_test=count(test,"feat_44")
feat45_tab_test=count(test,"feat_45")
feat46_tab_test=count(test,"feat_46")
feat47_tab_test=count(test,"feat_47")
feat48_tab_test=count(test,"feat_48")
feat49_tab_test=count(test,"feat_49")
feat50_tab_test=count(test,"feat_50")
feat51_tab_test=count(test,"feat_51")
feat52_tab_test=count(test,"feat_52")
feat53_tab_test=count(test,"feat_53")
feat54_tab_test=count(test,"feat_54")
feat55_tab_test=count(test,"feat_55")
feat56_tab_test=count(test,"feat_56")
feat57_tab_test=count(test,"feat_57")
feat58_tab_test=count(test,"feat_58")
feat59_tab_test=count(test,"feat_59")
feat60_tab_test=count(test,"feat_60")
feat61_tab_test=count(test,"feat_61")
feat62_tab_test=count(test,"feat_62")
feat63_tab_test=count(test,"feat_63")
feat64_tab_test=count(test,"feat_64")
feat65_tab_test=count(test,"feat_65")
feat66_tab_test=count(test,"feat_66")
feat67_tab_test=count(test,"feat_67")
feat68_tab_test=count(test,"feat_68")
feat69_tab_test=count(test,"feat_69")
feat70_tab_test=count(test,"feat_70")
feat71_tab_test=count(test,"feat_71")
feat72_tab_test=count(test,"feat_72")
feat73_tab_test=count(test,"feat_73")
feat74_tab_test=count(test,"feat_74")
feat75_tab_test=count(test,"feat_75")
feat76_tab_test=count(test,"feat_76")
feat77_tab_test=count(test,"feat_77")
feat78_tab_test=count(test,"feat_78")
feat79_tab_test=count(test,"feat_79")
feat80_tab_test=count(test,"feat_80")
feat81_tab_test=count(test,"feat_81")
feat82_tab_test=count(test,"feat_82")
feat83_tab_test=count(test,"feat_83")
feat84_tab_test=count(test,"feat_84")
feat85_tab_test=count(test,"feat_85")
feat86_tab_test=count(test,"feat_86")
feat87_tab_test=count(test,"feat_87")
feat88_tab_test=count(test,"feat_88")
feat89_tab_test=count(test,"feat_89")
feat90_tab_test=count(test,"feat_90")
feat91_tab_test=count(test,"feat_91")
feat92_tab_test=count(test,"feat_92")
feat93_tab_test=count(test,"feat_93")
feat1_tab_test

#plyr1 <- join(test, feat1_tab_test, by = "feat_1")
plyr1 <- join(test,feat1_tab_test, by ="feat_1")
test$feat_1_cnt<-plyr1$freq
plyr1 <- join(test,feat2_tab_test, by ="feat_2")
test$feat_2_cnt<-plyr1$freq
plyr1 <- join(test,feat3_tab_test, by ="feat_3")
test$feat_3_cnt<-plyr1$freq
plyr1 <- join(test,feat4_tab_test, by ="feat_4")
test$feat_4_cnt<-plyr1$freq
plyr1 <- join(test,feat5_tab_test, by ="feat_5")
test$feat_5_cnt<-plyr1$freq
plyr1 <- join(test,feat6_tab_test, by ="feat_6")
test$feat_6_cnt<-plyr1$freq
plyr1 <- join(test,feat7_tab_test, by ="feat_7")
test$feat_7_cnt<-plyr1$freq
plyr1 <- join(test,feat8_tab_test, by ="feat_8")
test$feat_8_cnt<-plyr1$freq
plyr1 <- join(test,feat9_tab_test, by ="feat_9")
test$feat_9_cnt<-plyr1$freq
plyr1 <- join(test,feat10_tab_test, by ="feat_10")
test$feat_10_cnt<-plyr1$freq
plyr1 <- join(test,feat11_tab_test, by ="feat_11")
test$feat_11_cnt<-plyr1$freq
plyr1 <- join(test,feat12_tab_test, by ="feat_12")
test$feat_12_cnt<-plyr1$freq
plyr1 <- join(test,feat13_tab_test, by ="feat_13")
test$feat_13_cnt<-plyr1$freq
plyr1 <- join(test,feat14_tab_test, by ="feat_14")
test$feat_14_cnt<-plyr1$freq
plyr1 <- join(test,feat15_tab_test, by ="feat_15")
test$feat_15_cnt<-plyr1$freq
plyr1 <- join(test,feat16_tab_test, by ="feat_16")
test$feat_16_cnt<-plyr1$freq
plyr1 <- join(test,feat17_tab_test, by ="feat_17")
test$feat_17_cnt<-plyr1$freq
plyr1 <- join(test,feat18_tab_test, by ="feat_18")
test$feat_18_cnt<-plyr1$freq
plyr1 <- join(test,feat19_tab_test, by ="feat_19")
test$feat_19_cnt<-plyr1$freq
plyr1 <- join(test,feat20_tab_test, by ="feat_20")
test$feat_20_cnt<-plyr1$freq
plyr1 <- join(test,feat21_tab_test, by ="feat_21")
test$feat_21_cnt<-plyr1$freq
plyr1 <- join(test,feat22_tab_test, by ="feat_22")
test$feat_22_cnt<-plyr1$freq
plyr1 <- join(test,feat23_tab_test, by ="feat_23")
test$feat_23_cnt<-plyr1$freq
plyr1 <- join(test,feat24_tab_test, by ="feat_24")
test$feat_24_cnt<-plyr1$freq
plyr1 <- join(test,feat25_tab_test, by ="feat_25")
test$feat_25_cnt<-plyr1$freq
plyr1 <- join(test,feat26_tab_test, by ="feat_26")
test$feat_26_cnt<-plyr1$freq
plyr1 <- join(test,feat27_tab_test, by ="feat_27")
test$feat_27_cnt<-plyr1$freq
plyr1 <- join(test,feat28_tab_test, by ="feat_28")
test$feat_28_cnt<-plyr1$freq
plyr1 <- join(test,feat29_tab_test, by ="feat_29")
test$feat_29_cnt<-plyr1$freq
plyr1 <- join(test,feat30_tab_test, by ="feat_30")
test$feat_30_cnt<-plyr1$freq
plyr1 <- join(test,feat31_tab_test, by ="feat_31")
test$feat_31_cnt<-plyr1$freq
plyr1 <- join(test,feat32_tab_test, by ="feat_32")
test$feat_32_cnt<-plyr1$freq
plyr1 <- join(test,feat33_tab_test, by ="feat_33")
test$feat_33_cnt<-plyr1$freq
plyr1 <- join(test,feat34_tab_test, by ="feat_34")
test$feat_34_cnt<-plyr1$freq
plyr1 <- join(test,feat35_tab_test, by ="feat_35")
test$feat_35_cnt<-plyr1$freq
plyr1 <- join(test,feat36_tab_test, by ="feat_36")
test$feat_36_cnt<-plyr1$freq
plyr1 <- join(test,feat37_tab_test, by ="feat_37")
test$feat_37_cnt<-plyr1$freq
plyr1 <- join(test,feat38_tab_test, by ="feat_38")
test$feat_38_cnt<-plyr1$freq
plyr1 <- join(test,feat39_tab_test, by ="feat_39")
test$feat_39_cnt<-plyr1$freq
plyr1 <- join(test,feat40_tab_test, by ="feat_40")
test$feat_40_cnt<-plyr1$freq
plyr1 <- join(test,feat41_tab_test, by ="feat_41")
test$feat_41_cnt<-plyr1$freq
plyr1 <- join(test,feat42_tab_test, by ="feat_42")
test$feat_42_cnt<-plyr1$freq
plyr1 <- join(test,feat43_tab_test, by ="feat_43")
test$feat_43_cnt<-plyr1$freq
plyr1 <- join(test,feat44_tab_test, by ="feat_44")
test$feat_44_cnt<-plyr1$freq
plyr1 <- join(test,feat45_tab_test, by ="feat_45")
test$feat_45_cnt<-plyr1$freq
plyr1 <- join(test,feat46_tab_test, by ="feat_46")
test$feat_46_cnt<-plyr1$freq
plyr1 <- join(test,feat47_tab_test, by ="feat_47")
test$feat_47_cnt<-plyr1$freq
plyr1 <- join(test,feat48_tab_test, by ="feat_48")
test$feat_48_cnt<-plyr1$freq
plyr1 <- join(test,feat49_tab_test, by ="feat_49")
test$feat_49_cnt<-plyr1$freq
plyr1 <- join(test,feat50_tab_test, by ="feat_50")
test$feat_50_cnt<-plyr1$freq
plyr1 <- join(test,feat51_tab_test, by ="feat_51")
test$feat_51_cnt<-plyr1$freq
plyr1 <- join(test,feat52_tab_test, by ="feat_52")
test$feat_52_cnt<-plyr1$freq
plyr1 <- join(test,feat53_tab_test, by ="feat_53")
test$feat_53_cnt<-plyr1$freq
plyr1 <- join(test,feat54_tab_test, by ="feat_54")
test$feat_54_cnt<-plyr1$freq
plyr1 <- join(test,feat55_tab_test, by ="feat_55")
test$feat_55_cnt<-plyr1$freq
plyr1 <- join(test,feat56_tab_test, by ="feat_56")
test$feat_56_cnt<-plyr1$freq
plyr1 <- join(test,feat57_tab_test, by ="feat_57")
test$feat_57_cnt<-plyr1$freq
plyr1 <- join(test,feat58_tab_test, by ="feat_58")
test$feat_58_cnt<-plyr1$freq
plyr1 <- join(test,feat59_tab_test, by ="feat_59")
test$feat_59_cnt<-plyr1$freq
plyr1 <- join(test,feat60_tab_test, by ="feat_60")
test$feat_60_cnt<-plyr1$freq
plyr1 <- join(test,feat61_tab_test, by ="feat_61")
test$feat_61_cnt<-plyr1$freq
plyr1 <- join(test,feat62_tab_test, by ="feat_62")
test$feat_62_cnt<-plyr1$freq
plyr1 <- join(test,feat63_tab_test, by ="feat_63")
test$feat_63_cnt<-plyr1$freq
plyr1 <- join(test,feat64_tab_test, by ="feat_64")
test$feat_64_cnt<-plyr1$freq
plyr1 <- join(test,feat65_tab_test, by ="feat_65")
test$feat_65_cnt<-plyr1$freq
plyr1 <- join(test,feat66_tab_test, by ="feat_66")
test$feat_66_cnt<-plyr1$freq
plyr1 <- join(test,feat67_tab_test, by ="feat_67")
test$feat_67_cnt<-plyr1$freq
plyr1 <- join(test,feat68_tab_test, by ="feat_68")
test$feat_68_cnt<-plyr1$freq
plyr1 <- join(test,feat69_tab_test, by ="feat_69")
test$feat_69_cnt<-plyr1$freq
plyr1 <- join(test,feat70_tab_test, by ="feat_70")
test$feat_70_cnt<-plyr1$freq
plyr1 <- join(test,feat71_tab_test, by ="feat_71")
test$feat_71_cnt<-plyr1$freq
plyr1 <- join(test,feat72_tab_test, by ="feat_72")
test$feat_72_cnt<-plyr1$freq
plyr1 <- join(test,feat73_tab_test, by ="feat_73")
test$feat_73_cnt<-plyr1$freq
plyr1 <- join(test,feat74_tab_test, by ="feat_74")
test$feat_74_cnt<-plyr1$freq
plyr1 <- join(test,feat75_tab_test, by ="feat_75")
test$feat_75_cnt<-plyr1$freq
plyr1 <- join(test,feat76_tab_test, by ="feat_76")
test$feat_76_cnt<-plyr1$freq
plyr1 <- join(test,feat77_tab_test, by ="feat_77")
test$feat_77_cnt<-plyr1$freq
plyr1 <- join(test,feat78_tab_test, by ="feat_78")
test$feat_78_cnt<-plyr1$freq
plyr1 <- join(test,feat79_tab_test, by ="feat_79")
test$feat_79_cnt<-plyr1$freq
plyr1 <- join(test,feat80_tab_test, by ="feat_80")
test$feat_80_cnt<-plyr1$freq
plyr1 <- join(test,feat81_tab_test, by ="feat_81")
test$feat_81_cnt<-plyr1$freq
plyr1 <- join(test,feat82_tab_test, by ="feat_82")
test$feat_82_cnt<-plyr1$freq
plyr1 <- join(test,feat83_tab_test, by ="feat_83")
test$feat_83_cnt<-plyr1$freq
plyr1 <- join(test,feat84_tab_test, by ="feat_84")
test$feat_84_cnt<-plyr1$freq
plyr1 <- join(test,feat85_tab_test, by ="feat_85")
test$feat_85_cnt<-plyr1$freq
plyr1 <- join(test,feat86_tab_test, by ="feat_86")
test$feat_86_cnt<-plyr1$freq
plyr1 <- join(test,feat87_tab_test, by ="feat_87")
test$feat_87_cnt<-plyr1$freq
plyr1 <- join(test,feat88_tab_test, by ="feat_88")
test$feat_88_cnt<-plyr1$freq
plyr1 <- join(test,feat89_tab_test, by ="feat_89")
test$feat_89_cnt<-plyr1$freq
plyr1 <- join(test,feat90_tab_test, by ="feat_90")
test$feat_90_cnt<-plyr1$freq
plyr1 <- join(test,feat91_tab_test, by ="feat_91")
test$feat_91_cnt<-plyr1$freq
plyr1 <- join(test,feat92_tab_test, by ="feat_92")
test$feat_92_cnt<-plyr1$freq
plyr1 <- join(test,feat93_tab_test, by ="feat_93")
test$feat_93_cnt<-plyr1$freq


### Test Feature extraction ends
trainMatrix <- train[,lapply(.SD,as.numeric)] %>% as.matrix
testMatrix <- test[,lapply(.SD,as.numeric)] %>% as.matrix
numberOfClasses <- max(y) + 1

##| Creating the list of parameters for XGBoost
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
                "num_class" = numberOfClasses)

cv.nround <- 5
cv.nfold <- 10


##| Running Cross Validation

bst.cv = xgb.cv(param=param, data = trainMatrix, label = y, 
                nfold = cv.nfold, nrounds = cv.nround)

eta = 0.02
depth = 16
child_weight = 6
subsample = 0.9
colsample_bytree = 0.8
gamma = 1
nround = 1500

#bst = xgboost(param=param, data = trainMatrix, label = y, nrounds=nround, max.depth =16)
bst = xgboost(param=param, data = trainMatrix, label = y, nrounds=nround,
              eta=0.02,min_child_weight=6,subsample=0.5,colsample_bytree = 0.8,
              gamma = 1,max.depth =16)

#watchlist <- list(train=trainMatrix, test=testMatrix)
#bst <- xgb.train(param=param,data=trainMatrix, max.depth=9, eta=1, nthread = 2, nround=100, watchlist=watchlist)

model <- xgb.dump(bst, with.stats = T)
model[1:10]


# Get the feature real names
names <- dimnames(trainMatrix)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bst)


# Nice graph
xgb.plot.importance(importance_matrix[1:10,])
xgb.plot.tree(feature_names = names, model = bst, n_first_tree = 2)


pred <- predict(bst, testMatrix)
print(length(pred))
print(head(pred))
summary(pred)
str(testMatrix)


######

prediction <- pred
print(head(prediction))
str(prediction)
pred_matrix=matrix(prediction,ncol=144368)
pred_matrix_out=t(pred_matrix)
ID_col=1:144368
pred_matrix_out=cbind(ID_col,pred_matrix_out)
submit=as.data.frame(pred_matrix_out)
names(submit)<-c('id','Class_1','Class_2','Class_3','Class_4','Class_5','Class_6','Class_7','Class_8','Class_9')

write.csv(submit,file="Otto_sub_6.csv",row.names=TRUE)

#Rank 636
