############### Air Quality Ground True Features Analysis ###############
# UCI - Air Quality Data Set
# https://archive.ics.uci.edu/ml/datasets/Air+Quality

# Abstract: Contains the responses of a gas multisensor device deployed on the field in an Italian city. Hourly responses averages are recorded along with gas concentrations references from a certified analyzer.
# 摘要：包含部署在意大利城市現場的氣體多傳感器設備的響應。 記錄每小時的響應平均值以及來自經認證的分析儀的氣體濃度參考。

# Data Set Information:
# The dataset contains 9358 instances of hourly averaged responses from an array of 5 metal oxide chemical sensors embedded in an Air Quality Chemical Multisensor Device. 
# The device was located on the field in a significantly polluted area, at road level,within an Italian city. 
# Data were recorded from March 2004 to February 2005 (one year)representing the longest freely available recordings of on field deployed air quality chemical sensor devices responses. 
# Ground Truth hourly averaged concentrations for CO, Non Metanic Hydrocarbons, Benzene, Total Nitrogen Oxides (NOx) and Nitrogen Dioxide (NO2) and were provided by a co-located reference certified analyzer. Evidences of cross-sensitivities as well as both concept and sensor drifts are present as described in De Vito et al., Sens. And Act. B, Vol. 129,2,2008 (citation required) eventually affecting sensors concentration estimation capabilities. 
# Missing values are tagged with -200 value.

# 數據集信息：
# 該數據集包含來自空氣質量化學多傳感器設備中的5個金屬氧化物化學傳感器陣列每小時平均響應的9358個實例。
# 該設備位於意大利城市內道路污染嚴重的地區的田野上。
# 從2004年3月到2005年2月（一年）記錄了數據，這些數據是現場部署的空氣質量化學傳感器設備響應的最長免費記錄。
# 一氧化碳，非代謝碳氫化合物，苯，總氮氧化物（NOx）和二氧化氮（NO2）的地面真實小時平均濃度由共同定位的參考認證分析儀提供。如De Vito等人在《 Sens.And Act》中所述，存在交叉敏感性以及概念漂移和傳感器漂移的證據。 B卷129,2,2008（需要引用）最終影響傳感器濃度估算能力。
# 缺少的值用-200值標記。

#Attribute Information:
# 0 Date (DD/MM/YYYY)
# 1 Time (HH.MM.SS)
# 2 True hourly averaged concentration CO in mg/m^3 (reference analyzer)
# 3 PT08.S1 (tin oxide) hourly averaged sensor response (nominally CO targeted)
# 4 True hourly averaged overall Non Metanic HydroCarbons concentration in microg/m^3 (reference analyzer)
# 5 True hourly averaged Benzene concentration in microg/m^3 (reference analyzer)
# 6 PT08.S2 (titania) hourly averaged sensor response (nominally NMHC targeted)
# 7 True hourly averaged NOx concentration in ppb (reference analyzer)
# 8 PT08.S3 (tungsten oxide) hourly averaged sensor response (nominally NOx targeted)
# 9 True hourly averaged NO2 concentration in microg/m^3 (reference analyzer)
# 10 PT08.S4 (tungsten oxide) hourly averaged sensor response (nominally NO2 targeted)
# 11 PT08.S5 (indium oxide) hourly averaged sensor response (nominally O3 targeted)
# 12 Temperature in Â°C
# 13 Relative Humidity (%)
# 14 AH Absolute Humidity

# 屬性信息：
# 0 日期（DD / MM / YYYY）
# 1 時間（HH.MM.SS）
# 2 真實的每小時平均平均CO濃度，單位為mg / m ^ 3（參考分析儀）
# 3 PT08.S1（氧化錫）每小時平均傳感器響應（標稱為CO目標）
# 4 真實的每小時平均平均非代謝碳氫化合物的總體濃度，單位為微克/立方米3（參考分析儀）
# 5 每小時平均真實苯濃度，以微克/米^ 3為單位（參考分析儀）
# 6 PT08.S2（二氧化鈦）每小時平均傳感器響應（名義上為NMHC）
# 7 每小時的真實平均NOx濃度ppb（參考分析儀）
# 8 PT08.S3（氧化鎢）每小時平均傳感器響應（標稱為NOx）
# 9 每小時的真實平均NO2濃度，以microg / m ^ 3為單位（參考分析儀）
# 10 每小時 PT08.S4（氧化鎢）傳感器的平均響應（標稱目標為NO2）
# 11 PT08.S5（氧化銦）每小時平均傳感器響應（標稱目標為O3）
# 12 溫度（°C）
# 13 相對濕度（％）
# 14 AH絕對濕度

# Activating the necessary packages
install.packages("openair")
library(openair)

kc1 <- importAURN(site = "kc1", year = 2017:2020)
head(kc1)
tail(kc1)

my1 <- importAURN(site = "my1", year = 2017:2020)
head(my1)
tail(my1)


kc100 <- importAURN(site = "kc1", year = 1900:2020)
head(kc100)
tail(kc100)

# Utility functions
# Using the selectByDate function it is easy to select quite complex time-based periods. For example, to select weekday (Monday to Friday) data from June to September for 2012 and for the hours 7am to 7pm inclusive:
  
sub <- selectByDate(kc1, day = "weekday", year = 2017, month = 6:9, hour = 7:19)
head(sub)

# Similarly it is easy to time-average data in many flexible ways. For example, 2-week means can be calculated as
sub2 <- timeAverage(kc1, avg.time = "2 week")
sub21 <- timeAverage(kc1, avg.time = "week")
sub22 <- timeAverage(kc1, avg.time = "month")
sub23 <- timeAverage(kc1, avg.time = "year")

# Wind roses and pollution roses
# openair can plot basic wind roses very easily provided the variables ws (wind speed) and wd (wind direction) are available.
windRose(kc1)
windRose(kc100)
windRose(my1)

# However, the real flexibility comes from being able to use the type option.
windRose(kc1, type = "year", layout = c(4, 2))
windRose(kc100, type = "year")

dataset = read.csv2('AirQualityUCI.csv', stringsAsFactors = FALSE)
View(dataset)

dim(dataset)
# [1] 9471   17

names(dataset)
# [1] "Date"          "Time"          "CO.GT."        "PT08.S1.CO."   "NMHC.GT."      "C6H6.GT."     
# [7] "PT08.S2.NMHC." "NOx.GT."       "PT08.S3.NOx."  "NO2.GT."       "PT08.S4.NO2."  "PT08.S5.O3."  
# [13] "T"             "RH"            "AH"            "X"             "X.1" 

# Removing the last 114 rows and last 2 columns, as all of them are null values along with the date
head(dataset)
#         Date     Time CO.GT. PT08.S1.CO. NMHC.GT. C6H6.GT. PT08.S2.NMHC. NOx.GT. PT08.S3.NOx. NO2.GT.
# 1 10/03/2004 18.00.00    2.6        1360      150     11.9          1046     166         1056     113
# 2 10/03/2004 19.00.00    2.0        1292      112      9.4           955     103         1174      92
# 3 10/03/2004 20.00.00    2.2        1402       88      9.0           939     131         1140     114
# 4 10/03/2004 21.00.00    2.2        1376       80      9.2           948     172         1092     122
# 5 10/03/2004 22.00.00    1.6        1272       51      6.5           836     131         1205     116
# 6 10/03/2004 23.00.00    1.2        1197       38      4.7           750      89         1337      96
#   PT08.S4.NO2. PT08.S5.O3.    T   RH     AH  X X.1
# 1         1692        1268 13.6 48.9 0.7578 NA  NA
# 2         1559         972 13.3 47.7 0.7255 NA  NA
# 3         1555        1074 11.9 54.0 0.7502 NA  NA
# 4         1584        1203 11.0 60.0 0.7867 NA  NA
# 5         1490        1110 11.2 59.6 0.7888 NA  NA
# 6         1393         949 11.2 59.2 0.7848 NA  NA

tail(dataset, n = 115)
datasetOriginal = dataset

dataset = dataset[1:9357, 1:15]

dim(dataset)
# [1] 9357   15
tail(dataset)

head(dataset, 12)
#          Date     Time CO.GT. PT08.S1.CO. NMHC.GT. C6H6.GT. PT08.S2.NMHC. NOx.GT. PT08.S3.NOx. NO2.GT.
# 1  10/03/2004 18.00.00    2.6        1360      150     11.9          1046     166         1056     113
# 2  10/03/2004 19.00.00    2.0        1292      112      9.4           955     103         1174      92
# 3  10/03/2004 20.00.00    2.2        1402       88      9.0           939     131         1140     114
# 4  10/03/2004 21.00.00    2.2        1376       80      9.2           948     172         1092     122
# 5  10/03/2004 22.00.00    1.6        1272       51      6.5           836     131         1205     116
# 6  10/03/2004 23.00.00    1.2        1197       38      4.7           750      89         1337      96
# 7  11/03/2004 00.00.00    1.2        1185       31      3.6           690      62         1462      77
# 8  11/03/2004 01.00.00    1.0        1136       31      3.3           672      62         1453      76
# 9  11/03/2004 02.00.00    0.9        1094       24      2.3           609      45         1579      60
# 10 11/03/2004 03.00.00    0.6        1010       19      1.7           561    -200         1705    -200
# 11 11/03/2004 04.00.00 -200.0        1011       14      1.3           527      21         1818      34
# 12 11/03/2004 05.00.00    0.7        1066        8      1.1           512      16         1918      28

# Replacing -200 value in the dataset with NA
dataset[dataset == -200] = NA
head(dataset, 12)
#          Date     Time CO.GT. PT08.S1.CO. NMHC.GT. C6H6.GT. PT08.S2.NMHC. NOx.GT. PT08.S3.NOx. NO2.GT.
# 1  10/03/2004 18.00.00    2.6        1360      150     11.9          1046     166         1056     113
# 2  10/03/2004 19.00.00    2.0        1292      112      9.4           955     103         1174      92
# 3  10/03/2004 20.00.00    2.2        1402       88      9.0           939     131         1140     114
# 4  10/03/2004 21.00.00    2.2        1376       80      9.2           948     172         1092     122
# 5  10/03/2004 22.00.00    1.6        1272       51      6.5           836     131         1205     116
# 6  10/03/2004 23.00.00    1.2        1197       38      4.7           750      89         1337      96
# 7  11/03/2004 00.00.00    1.2        1185       31      3.6           690      62         1462      77
# 8  11/03/2004 01.00.00    1.0        1136       31      3.3           672      62         1453      76
# 9  11/03/2004 02.00.00    0.9        1094       24      2.3           609      45         1579      60
# 10 11/03/2004 03.00.00    0.6        1010       19      1.7           561      NA         1705      NA
# 11 11/03/2004 04.00.00     NA        1011       14      1.3           527      21         1818      34
# 12 11/03/2004 05.00.00    0.7        1066        8      1.1           512      16         1918      28

# Changing the date and time format and concatenating them into a single column
dataset[1:3, 1:2]
#         Date     Time
# 1 10/03/2004 18.00.00
# 2 10/03/2004 19.00.00
# 3 10/03/2004 20.00.00

names(dataset)
# [1] "Date"          "Time"          "CO.GT."        "PT08.S1.CO."   "NMHC.GT."      "C6H6.GT."     
# [7] "PT08.S2.NMHC." "NOx.GT."       "PT08.S3.NOx."  "NO2.GT."       "PT08.S4.NO2."  "PT08.S5.O3."  
# [13] "T"             "RH"            "AH"  

dataset = within(dataset, {DateTime = format(as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %H.%M.%S"))})
View(dataset)

names(dataset)
# [1] "Date"          "Time"          "CO.GT."        "PT08.S1.CO."   "NMHC.GT."      "C6H6.GT."      "PT08.S2.NMHC."
# [8] "NOx.GT."       "PT08.S3.NOx."  "NO2.GT."       "PT08.S4.NO2."  "PT08.S5.O3."   "T"             "RH"           
# [15] "AH"            "DateTime"    

datasetOriginal02 <- dataset

dataset = dataset[, c(16, 3:15)]
names(dataset)
# [1] "DateTime"      "CO.GT."        "PT08.S1.CO."   "NMHC.GT."      "C6H6.GT."      "PT08.S2.NMHC." "NOx.GT."      
# [8] "PT08.S3.NOx."  "NO2.GT."       "PT08.S4.NO2."  "PT08.S5.O3."   "T"             "RH"            "AH" 

str(dataset)
# 'data.frame':	9357 obs. of  14 variables:
# $ DateTime     : chr  "2004-03-10 18:00:00" "2004-03-10 19:00:00" "2004-03-10 20:00:00" "2004-03-10 21:00:00" ...
# $ CO.GT.       : num  2.6 2 2.2 2.2 1.6 1.2 1.2 1 0.9 0.6 ...
# $ PT08.S1.CO.  : int  1360 1292 1402 1376 1272 1197 1185 1136 1094 1010 ...
# $ NMHC.GT.     : int  150 112 88 80 51 38 31 31 24 19 ...
# $ C6H6.GT.     : num  11.9 9.4 9 9.2 6.5 4.7 3.6 3.3 2.3 1.7 ...
# $ PT08.S2.NMHC.: int  1046 955 939 948 836 750 690 672 609 561 ...
# $ NOx.GT.      : int  166 103 131 172 131 89 62 62 45 NA ...
# $ PT08.S3.NOx. : int  1056 1174 1140 1092 1205 1337 1462 1453 1579 1705 ...
# $ NO2.GT.      : int  113 92 114 122 116 96 77 76 60 NA ...
# $ PT08.S4.NO2. : int  1692 1559 1555 1584 1490 1393 1333 1333 1276 1235 ...
# $ PT08.S5.O3.  : int  1268 972 1074 1203 1110 949 733 730 620 501 ...
# $ T            : num  13.6 13.3 11.9 11 11.2 11.2 11.3 10.7 10.7 10.3 ...
# $ RH           : num  48.9 47.7 54 60 59.6 59.2 56.8 60 59.7 60.2 ...
# $ AH           : num  0.758 0.726 0.75 0.787 0.789 ...

dataset$DateTime = as.POSIXct(dataset$DateTime)
str(dataset)
# 'data.frame':	9357 obs. of  14 variables:
# $ DateTime     : POSIXct, format: "2004-03-10 18:00:00" "2004-03-10 19:00:00" "2004-03-10 20:00:00" "2004-03-10 21:00:00" ...
# $ CO.GT.       : num  2.6 2 2.2 2.2 1.6 1.2 1.2 1 0.9 0.6 ...

# Rearranging the columns according to the pollutants true and targeted sensor response
# 根據污染物的真實和目標傳感器響應重新排列色譜柱
names(dataset)
dataset = dataset[, c(1:3, 4, 6, 5, 7:14)]
names(dataset)
# [1] "DateTime"      "CO.GT."        "PT08.S1.CO."   "NMHC.GT."      "PT08.S2.NMHC." "C6H6.GT."   "NOx.GT."  "PT08.S3.NOx." 
# [9] "NO2.GT."       "PT08.S4.NO2."  "PT08.S5.O3."   "T"             "RH"            "AH"   

install.packages('corrplot')
library(corrplot)

cor(dataset[2:14], use = "complete.obs")
#                   CO.GT. PT08.S1.CO.   NMHC.GT. PT08.S2.NMHC.   C6H6.GT.     NOx.GT. PT08.S3.NOx.    NO2.GT.
# CO.GT.         1.0000000  0.93626072  0.8871675     0.9584256  0.9726596  0.95134166  -0.82372808  0.8614319
# PT08.S1.CO.    0.9362607  1.00000000  0.7817468     0.9363456  0.9313679  0.92288458  -0.82957680  0.8665794
# NMHC.GT.       0.8871675  0.78174678  1.0000000     0.8750609  0.8979281  0.81118197  -0.77423675  0.7280519
# PT08.S2.NMHC.  0.9584256  0.93634564  0.8750609     1.0000000  0.9848344  0.92663333  -0.91065086  0.8850232
# C6H6.GT.       0.9726596  0.93136785  0.8979281     0.9848344  1.0000000  0.92730382  -0.84885013  0.8467426
# NOx.GT.        0.9513417  0.92288458  0.8111820     0.9266333  0.9273038  1.00000000  -0.81429650  0.8574250
# PT08.S3.NOx.  -0.8237281 -0.82957680 -0.7742368    -0.9106509 -0.8488501 -0.81429650   1.00000000 -0.8152242
# NO2.GT.        0.8614319  0.86657945  0.7280519     0.8850232  0.8467426  0.85742501  -0.81522418  1.0000000
# Correlation, Variance and Covariance (Matrices)
# var, cov and cor compute the variance of x and the covariance or correlation of x and y if these are vectors. If x and y are matrices then the covariances (or correlations) between the columns of x and the columns of y are computed.
# 相關，方差和協方差（矩陣）
# var，cov和cor計算x的方差以及x和y的協方差或相關性（如果它們是向量）。 如果x和y是矩陣，則計算x列與y列之間的協方差（或相關性）。

corrplot(corr=cor(dataset[2:14], use = "complete.obs"))

# Changing the column names
dataset_columns = c('date', 'co', 'co_s', 'NMHC', 'NMHC_s', 'c6h6',
                    'nox', 'nox_s', 'no2', 'no2_s', 'o3_s','temp', 'RH%', 'AH')
colnames(dataset) = dataset_columns
names(dataset)
#  [1] "date"   "co"     "co_s"   "NMHC"   "NMHC_s" "c6h6"   "nox"    "nox_s"  "no2"    "no2_s"  "o3_s"   "temp"   "RH%"    "AH"  

summary(dataset)
#      date                           co              co_s           NMHC            NMHC_s            c6h6            nox        
# Min.   :2004-03-10 18:00:00   Min.   : 0.100   Min.   : 647   Min.   :   7.0   Min.   : 383.0   Min.   : 0.10   Min.   :   2.0  
# 1st Qu.:2004-06-16 05:00:00   1st Qu.: 1.100   1st Qu.: 937   1st Qu.:  67.0   1st Qu.: 734.5   1st Qu.: 4.40   1st Qu.:  98.0  
# Median :2004-09-21 16:00:00   Median : 1.800   Median :1063   Median : 150.0   Median : 909.0   Median : 8.20   Median : 180.0  
# Mean   :2004-09-21 16:00:00   Mean   : 2.153   Mean   :1100   Mean   : 218.8   Mean   : 939.2   Mean   :10.08   Mean   : 246.9  
# 3rd Qu.:2004-12-28 03:00:00   3rd Qu.: 2.900   3rd Qu.:1231   3rd Qu.: 297.0   3rd Qu.:1116.0   3rd Qu.:14.00   3rd Qu.: 326.0  
# Max.   :2005-04-04 14:00:00   Max.   :11.900   Max.   :2040   Max.   :1189.0   Max.   :2214.0   Max.   :63.70   Max.   :1479.0  
#                               NA's   :1683     NA's   :366    NA's   :8443     NA's   :366      NA's   :366     NA's   :1639    
#     nox_s             no2            no2_s           o3_s             temp            RH%              AH        
# Min.   : 322.0   Min.   :  2.0   Min.   : 551   Min.   : 221.0   Min.   :-1.90   Min.   : 9.20   Min.   :0.1847  
# 1st Qu.: 658.0   1st Qu.: 78.0   1st Qu.:1227   1st Qu.: 731.5   1st Qu.:11.80   1st Qu.:35.80   1st Qu.:0.7368  
# Median : 806.0   Median :109.0   Median :1463   Median : 963.0   Median :17.80   Median :49.60   Median :0.9954  
# Mean   : 835.5   Mean   :113.1   Mean   :1456   Mean   :1022.9   Mean   :18.32   Mean   :49.23   Mean   :1.0255  
# 3rd Qu.: 969.5   3rd Qu.:142.0   3rd Qu.:1674   3rd Qu.:1273.5   3rd Qu.:24.40   3rd Qu.:62.50   3rd Qu.:1.3137  
# Max.   :2683.0   Max.   :340.0   Max.   :2775   Max.   :2523.0   Max.   :44.60   Max.   :88.70   Max.   :2.2310  
# NA's   :366      NA's   :1642    NA's   :366    NA's   :366      NA's   :366     NA's   :366     NA's   :366     

# Summary Plot of the dataset
# Function to rapidly provide an overview of air quality data
summaryPlot(dataset, period = 'months')
summaryPlot(dataset, period = 'years')

sub2004 <- selectByDate(dataset, day = "weekday", year = 2004, month = 6:9, hour = 7:19)
head(sub2004)

# Trendlevel plot
trendLevel(dataset, pollutant = 'AH', auto.text = TRUE, main = 'AH')
trendLevel(sub2004, pollutant = 'AH', auto.text = TRUE, main = 'AH')
trendLevel(dataset, pollutant = 'no2_s', auto.text = TRUE, main = 'no2_s')
trendLevel(dataset, pollutant = 'temp', auto.text = TRUE, main = 'temp')

# Calender plot
# Plot time series values in convential calendar format
calendarPlot(dataset, pollutant = 'co', year = 2004)
calendarPlot(dataset, pollutant = 'co', year = 2005)
calendarPlot(dataset, pollutant = 'NMHC', year = 2004)
calendarPlot(dataset, pollutant = 'NMHC', year = 2005)
calendarPlot(dataset, pollutant = 'c6h6', year = 2004)
calendarPlot(dataset, pollutant = 'c6h6', year = 2005)
calendarPlot(dataset, pollutant = 'nox', year = 2004)
calendarPlot(dataset, pollutant = 'nox', year = 2005)
calendarPlot(dataset, pollutant = 'no2', year = 2004)
calendarPlot(dataset, pollutant = 'no2', year = 2005)

# Timevariation plot
# Diurnal, day of the week and monthly variation
timeVariation(dataset, pollutant = 'co', auto.text = TRUE, main = 'CO TimeVariation plot')
timeVariation(sub2004, pollutant = 'co', auto.text = TRUE, main = 'CO TimeVariation plot')
timeVariation(dataset, pollutant = 'c6h6', auto.text = TRUE, main = 'C6H6 TimeVariation plot')
timeVariation(dataset, pollutant = 'nox', auto.text = TRUE, main = 'NOx TimeVariation plot')
timeVariation(dataset, pollutant = 'no2', auto.text = TRUE, main = 'NO2 TimeVariation plot')

# Trendlevel
trendLevel(mydata = dataset, pollutant = 'co', auto.text = TRUE, main = 'CO TrendLevel plot')
trendLevel(mydata = dataset, pollutant = 'c6h6', auto.text = TRUE, main = 'C6H6 TrendLevel plot')
trendLevel(mydata = dataset, pollutant = 'nox', auto.text = TRUE, main = 'NOx TrendLevel plot')
trendLevel(mydata = dataset, pollutant = 'no2', auto.text = TRUE, main = 'NO2 TrendLevel plot')





