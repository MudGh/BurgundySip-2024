# INTRODUCTION =======

# BurgundySip, a renowned wine cellar/store, seeks to enhance its pricing strategy.
# Our team is conducting a data-driven pilot study to uncover hidden patterns and 
# trends within the wine dataset.By leveraging data analytics, we aim to optimize 
# wine pricing, improve profitability, and enhance customer satisfaction.

# ANALYTICALLY OBJECTIVES ======

#    Descriptive (statistical) questions ====
# 1. How does the residual sugar level influence the average rating of wine across 
# different production areas?
# 2. Which wine variety has the highest average price, and how does this relate 
# to the average rating within each variety?
# 3. Is there a significant difference in average price based on the age of the 
# wine across different wine varieties?
# 4. How do the average price and average age of wines differ among North-Spain, 
# South-Spain, and East-Spain production areas?
# 5. What is the relationship between the average total ratings and the average 
# price of wines for each wine variety?

#    Predictive Objectives ====
# 1. How accurately can the price of a wine be predicted using its rating, age, 
# residual sugar level, and alcohol content across different wine varieties?
# 2. To what extent do residual sugar level, age and alcoholic strength predict 
# the overall rating of wines from different production areas?

#    Cluster-based learning objectives ====
# 1. How accurately can the price of a wine be predicted using its rating, age, 
# residual sugar level, and alcohol content across different wine varieties?
# 2. To what extent do residual sugar level, age and alcoholic strength predict 
# the overall rating of wines from different production areas?

# DATA ANALYSIS PROCESS ======
# STEP-0: IMPORT DATA ====
burgundysip <- read.csv("~/BurgundySip.csv", na.string=c('NA', '', ' '));

#Show the first few rows to understand the data
head(burgundysip)

#Check the structure and types of each variable
str(burgundysip)

# We can see that RT, PR, BD, and ACD have NA's
summary(burgundysip)

# STEP-1: VARIABLE ANALYSIS ====
# BASIC ASSUMPTIONS  ##

# Check for missing values in the dataset,
# Count missing values per variable
sapply(burgundysip, function(x) sum(is.na(x)))

# 'data.frame': 7500 obs. of  14 variables:
# $ SN  : chr [NOM][further analysis is required for 3 NA's]
# $ NAME: chr [NOM][further analysis is required for 112 NA's]
# $ WINE: chr [NOM][further analysis is required for 81 NA's]
# $ YR  : chr [NOM][further analysis is required for 5 NA's]
# $ REG : chr [NOM][further analysis is required for 43 NA's]
# $ TP  : chr [NOM - Strong Categorical][further analysis is required for 581 NA's]
# $ RT  : num [ORD - Created another column - Levels 1 to 5][further analysis is required for 9 NA's]
# $ NUMR: int
# $ PR  : num [further analysis is required for 58 NA's]
# $ BD  : int [ORD][further analysis is required for 1169 NA's]
# $ ACD : int [ORD][further analysis is required for 1169 NA's]
# $ RSG : chr [Created another column - Levels 1 to 16][further analysis is required for 18 NA's]
# $ AL  : chr [Changed as double - further analysis is required for 36 NA's]
# $ DN  : chr [Changed as double - further analysis is required for 29 NA's]

## CHANGED VARIABLES TO FACTORS AND DOUBLE ##

burgundysip[1:3] <- lapply(burgundysip[1:3], factor)
burgundysip[5:6] <- lapply(burgundysip[5:6], factor)

# We changed YR, RSG, AL, and DN from char to double because we can see if they have NA's

burgundysip$YR <- as.double(burgundysip$YR)
length(burgundysip$YR[is.na(burgundysip$YR) == TRUE])

burgundysip$RSG <- as.double(burgundysip$RSG)
length(burgundysip$RSG[is.na(burgundysip$RSG) == TRUE])

burgundysip$AL <- as.double(burgundysip$AL)
length(burgundysip$AL[is.na(burgundysip$AL) == TRUE])

burgundysip$DN <- as.double(burgundysip$DN)
length(burgundysip$DN[is.na(burgundysip$DN) == TRUE])

# We can run the summary and str again to see the changes
str(burgundysip)
summary(burgundysip)

# Check for missing values in the dataset again
sapply(burgundysip, function(x) sum(is.na(x)))

# STEP-2: CHECK ANY DUPLICATED ====

# We can see that if we evaluate rows full data set there are not any duplicated row
duplicated_rows <- duplicated(burgundysip)
burgundysip[duplicated_rows, ]

anyDuplicated(burgundysip)
ifelse (!anyDuplicated(burgundysip),
        "No duplicated observations found.",
        "Duplicated observations found.");

# Since burgundysip has an ID of the sample that is column SN, it would be considered the candidate key
# for duplicate detection
anyDuplicated(burgundysip$SN)
ifelse (!anyDuplicated(burgundysip$SN),
        "No duplicated observations found.",
        "Duplicated observations found.");

# Top - Bottom Duplicate Detection
# The top most record of a duplicated set is considered the cleanest
burgundysip[duplicated(burgundysip$SN), ]

# Bottom - Top Duplicate Detection
# The bottom most record of a duplicated set is considered the cleanest
burgundysip[duplicated(burgundysip$SN, fromLast = T), ]

# Review the duplicated entries we can see that the last three variables "RSG",
# "AL", and "DN" has different values. To know how this variables impact our target "Price"
# We evaluate the correlation between them.
# Generate a correlation table with duplicate entries
library(dplyr)
qn_burgundysip <- burgundysip %>% select(where(is.numeric)) %>% na.omit()
qn_burgundysip
cord <- cor(qn_burgundysip); cord
# Show a graph of the performance analysis
library(PerformanceAnalytics)
chart.Correlation(qn_burgundysip, method = "spearman")
# The relationship between PR and RSG is Perfect Negative Correlation
# The relationship between PR and AL is Weak Positive Correlation
# The relationship between PR and DN is Weak Negative Correlation

# Remove the duplicated rows by SN
df_unique <- burgundysip[!duplicated(burgundysip$SN), ]
# After analyzing some duplicate sets, it is conclusive that the top most observation
# is more complete in each set. Therefore, Top-Bottom duplicate treatment strategy would be used.

ifelse (!anyDuplicated(df_unique$SN),
        "No duplicated observations found.",
        "Duplicated observations found.");

str(df_unique)
summary(df_unique)

# Generate a correlation table without duplicate entries
qn_c<- df_unique %>% select(where(is.numeric)) %>% na.omit()
cord <- cor(qn_c); cord
# Show a graph of the performance analysis
chart.Correlation(qn_c, method = "spearman")
# A slight increase in the correlation of the variables is observed.
# Based on the fact that these three variables have an impact on price,
# we will eliminate duplicates to improve the analysis.

# STEP-3: ADD NEW VARIABLE IN BASED OF REGION ====

unique(burgundysip$REG)

# South-Spain
south_spain <- c("Jerez Amontillado", "Jerez-Xeres-Sherry", "Jerez Pedro Ximenes (PX)",
                 "Montilla-Moriles", "Cadiz", "Manzanilla", "Jerez Palo Cortado",
                 "Jerez Cream", "Jerez Oloroso", "Andalucia", "Malaga",
                 "Sierras de Malaga", "Abona", "La Palma", "Extremadura", "Condado de Huelva")

# North-Spain
north_spain <- c("Valdeorras", "Ribera del Duero", "Ribera Sacra", "Bierzo",
                 "Rias Baixas", "Galicia", "Monterrei", "Navarra", "Rioja",
                 "Rioja Alta", "Rioja Alavesa", "Dominio de Valdepusa",
                 "Pago Calzadilla", "Arinzano", "Somontano", "Ribeira Sacra", "Ribeiro")

# West-Spain
west_spain <- c("Castilla y Leon", "Toro", "Rueda", "Castilla", "Tierra del Vino de Zamora",
                "Arribes", "Mentrida", "Cigales")

# East-Spain
east_spain <- c("Pla de Bages", "Sardon de Duero", "Priorato", "Cava", "Penedes",
                "Alella", "Campo de Borja", "Carinena", "Mallorca", "Binissalem-Mallorca",
                "Alicante", "Utiel-Requena", "Valencia", "Aragon", "Calatayud",
                "Jumilla", "Valdejalon", "Cataluna", "Costers del Segre", "Emporda",
                "Montsant", "Tarragona", "Terra Alta", "Ribera del Gallego-Cinco Villas",
                "Pla i Llevant", "Conca de Barbera", "La Mancha", "Murcia", "El Terrerazo",
                "Madrid", "Otazu", "Gran Canaria", "Dehesa del Carrizal", "Yecla",
                "Almansa", "Vino de Espana")

df_unique$PA <- ifelse(df_unique$REG %in% south_spain, "South-Spain",
                       ifelse(df_unique$REG %in% north_spain, "North-Spain",
                              ifelse(df_unique$REG %in% west_spain, "West-Spain",
                                     ifelse(df_unique$REG %in% east_spain, "East-Spain", NA))))
df_unique$PA <- factor(df_unique$PA)

str(df_unique)
summary(df_unique)

# STEP-4: HANDLING MISSING VALUES ====

# Verify the changes by checking for NA values again
sapply(df_unique, function(x) sum(is.na(x)))

# 'data.frame': 3667 obs. of  14 variables:
# $ SN  : chr [1 NA's] [SI] [Since SN is like wine's ID we decided that the type of missingness is Missing Not at Random (MNAR)]
# $ NAME: chr [1 NA's] [SI]
# $ WINE: chr [0 NA's] [SI]
# $ YR  : chr [2 NA's] [SI]
# $ REG : chr [0 NA's] [SI]

# $ TP  : chr [154 NA's] [I/D][We take the assumptions that TP are correlated with PA]
# $ RT  : num [0 NA's] [I/D - SD][We take the assumptions that ACD, RSG, AL, and DN influence in RT]
# $ NUMR: int [SI]
# $ PR  : num [58 NA's] [SD]
# $ BD  : int [443 NA's] [I/D][We take the assumptions that TP, YR, and REG influence in BD]
# $ ACD : int [443 NA's] [I/D][We take the assumptions that TP, YR, and REG influence in ACD]
# $ RSG : chr [18 NA's] [I/D][We take the assumptions that TP, YR, and REG influence in ACD]
# $ AL  : chr [36 NA's] [I/D][We take the assumptions that TP, YR, and REG influence in AL]
# $ DN  : chr [20 NA's] [I/D][We take the assumptions that TP, YR, and REG influence in DN]
# $ PA  : chr [0 NA's] [I/D]

#    HANDLING MISSING VALUES IN SERIAL NUMBER ====
# Since SN is as ID of the sample, we decided that the type of missingness is Missing Not at Random (MNAR).
df_unique[is.na(df_unique$SN) == TRUE, ]
# We can see that there is only one entry with missing SN which is “Nounat”.
df_unique[df_unique$WINE == "Nounat", ]
# We can see that this observation has considerable NA to decide that
# we can eliminate because it has no impact on our model.
df_clean <- df_unique[!is.na(df_unique$SN), ]
str(df_clean)
summary(df_clean)

#    HANDLING MISSING VALUES IN YEARS ====
# Since YR is a strong independent variable we can't get information from another variable. However,
# we take an assumption based on the missing years of the region or wine

# Also, we see that missing data could be a N.V. value, because or it's a vintage wine or N.V wine.
# Based on that we evaluate the missingness.
df_clean[is.na(df_clean$YR) == TRUE, ]

# We can evaluate the missingness of Missing at Random (MCAR)

# Treating N.V.
#we noticed that there are values with N.V. and we want know more about it.
# By Asking the "Google" we know there ar two types
#https://oconnellfamilywines.com/non-vintage-wines/

#1- Single Vintage Wines
#Single Vintage wines are bottled using grapes from the same growing year.
#At harvest the grapes are made into juice and fermented in toasted barrels,
#or stainless steel.  Each barrel is dated, and the vintner then bottles the wine
#from that year’s harvest into the designated vintage year.

#2- Non-Vintage Wines (N.V.)
#A non-vintage wine is created by blending wines from different harvests to create
#a blend that has nice depth and complexity. Vintners us the “NV” as the designation
#for wines that are non-vintage in place of the year of a varietal.

#Thus, it will be assumed that the N.V. wine was released in the value of the year closest
# to the current year shown in the YR variable, because this means that it was the last year
# in which the wines were released.

# We will look for the years without repetition and order them, we can see that the
max(df_clean$YR) #to see the closest year to the current year
# closest year to the current year is 2021.
sort(unique(df_clean$YR))

# On this basis we will do a pattern-based imputation of the values of NA and N.V. for 2021.
df_clean$YR[is.na(df_clean$YR)] <- 2021 #Impute 'N.V.' years as 2021.

str(df_clean)
summary(df_clean)

#    HANDLING MISSING VALUES IN WINE VARIETY ====
df_clean[is.na(df_clean$TP) == TRUE, ]

variety_and_production <- df_clean %>% na.omit() %>% select(c("TP", "PA"))

summary(variety_and_production)
str(variety_and_production)

# Chi-Square test for Variety vs Production Area
# H0: There is no association or relationship between the categorical variables under investigation.
# Ha: The two variables are NOT independent
contingency_table <- table(variety_and_production$TP,variety_and_production$PA)
summary(contingency_table)
# Variety vs Production Area : p-value = 0
# p < a (0.05). Therefore, H0 is approved. The two variables are independent.
# How those variables are independent, this is a case of Missing Not at Random (MCAR).

# Next, I will assess whether this variable is likely to be price-dependent;
# if so, a single-value imputation will be applied.

variety_and_prince <- df_clean %>% na.omit() %>% select(c("TP", "PR"))

# One-Way ANNOVA Test
price_variety_agg <- aggregate(PR ~ TP, data = variety_and_prince, FUN = mean)
barplot(PR ~ TP, data = price_variety_agg, main="Price vs Wine variety")

### Assumption 1: Test for Normality
shapiro.test(variety_and_prince$PR)
# p-value < 2.2e-16. So, p < 0.05.
# H0 (the data is normally distributed) is rejected.
# Assumption 1 fails.

### Assumption 2: Test for homogeneity of variances
library(car)

# Null Hypothesis (H0): The variances of the groups being compared are equal (homogeneous).
# Alternative Hypothesis (H1): The variances of the groups being compared are not equal (heterogeneous).

lt_result <- leveneTest(PR ~ TP, data = variety_and_prince)
lt_result
# Pr(>F) < 2.2e-16 ***. So, p < 0.05.
# H0 (the groups being compared are homogeneous) is rejected.
# The variances of the groups being compared are not equal (heterogeneous)
# Assumption 2 fails.

### Assumption 3: Test for independence of observations
# H0: There is NO significant difference between the mean values of the
# categories for furnishing wine variety Thus, furnishing wine variety and price maintain no significant relationship.
# Ha: There is a significant difference between the mean values of the
# categories for furnishing wine variety Thus, furnishing wine variety and price are less likely to be independent.
anova_result <- aov(PR ~ TP, data = variety_and_prince)
summary(anova_result)
# Pr(>F) <2e-16 ***. So, p < 0.05.
# There fore H0 is rejected. Thus, furnishing wine variety and price are less likely to be independent.
# Assumption 3 rejected.

# Applied single value imputation (mode of the variable) by Production Area

str(df_clean)
summary(df_clean)

df <- df_clean
summary(df)

# Group Wine Variety values by Production Area
unique_ns <- unique(df[df$PA == "North-Spain", ]$TP[!is.na(df[df$PA == "North-Spain", ]$TP)])
unique_es <- unique(df[df$PA == "East-Spain", ]$TP[!is.na(df[df$PA == "East-Spain", ]$TP)])
unique_ws <- unique(df[df$PA == "West-Spain", ]$TP[!is.na(df[df$PA == "West-Spain", ]$TP)])
unique_ss <- unique(df[df$PA == "South-Spain", ]$TP[!is.na(df[df$PA == "South-Spain", ]$TP)])

# Obtain the mode according to the frequency with which these values appear in
# the group wine variety by production area
mode_ns <- unique_ns[which.max(table(unique_ns))]
mode_es <- unique_es[which.max(table(unique_es))]
mode_ws <- unique_ws[which.max(table(unique_ws))]
mode_ss <- unique_ss[which.min(table(unique_ss))]

# Create a data frame with the calculated modes and the unique values of production area
mode_df <- data.frame(PA = unique(df$PA), mode_TP = c(mode_ns, mode_es, mode_ws, mode_ss))

# Merge the original data frame with the mode data frame by PA
df <- merge(df, mode_df, by = "PA"); df

# Replace the NA's in TP with the corresponding mode
df_clean$TP[is.na(df_clean$TP)] <- df$mode_TP[is.na(df$TP)]

# Checks whether the imputation was performed correctly
summary(df_clean)

#    HANDLING MISSING VALUES IN BODY SCORE ====

## Assumption 1: Test for Normality
shapiro.test(df_clean$BD)
# p-value < 2.2e-16. So, p < 0.05.
# H0 (the data is normally distributed) is rejected.
# Assumption 1 fails.

## Assumption 2: Test for Homogeneity of variances
# Null Hypothesis (H0): The variances of the groups being compared are equal (homogeneous).
# Alternative Hypothesis (H1): The variances of the groups being compared are not equal (heterogeneous).

lt_result <- leveneTest(BD ~ TP * PA, data = df_clean)
lt_result
# Pr(>F) = 5.45e-12 ***. So, p > 0.05.
# H0 (the groups being compared are homogeneous) is rejected.
# Assumption 2 rejected.

# Use only TP because TP * PA was rejected
lt_result <- leveneTest(BD ~ TP, data = df_clean)
lt_result
# Pr(>F) = 0.9493. So, p < 0.05.
# H0 (the groups being compared are homogeneous) is approved.

## Assumption 3: Test for independence of observations
# H0: There is NO significant difference between the mean values of the
# categories for furnishing BD Thus, furnishing BD and TP maintain no significant relationship.
# Ha: There is a significant difference between the mean values of the
# categories for furnishing BD Thus, furnishing BD and TP are less likely to be independent.
bd_anova <- aov(BD ~ TP, data = df_clean); summary(bd_anova)
summary(bd_anova)
# Pr(>F) <2e-16 ***. So, p < 0.05.
# There fore H0 is rejected. Thus, furnishing BD and TP are less likely to be independent.
# Assumption 3 rejected

# How ANOVA indicates significant differences, I run hoc tests.
tukey_result <- TukeyHSD(bd_anova)
# Display the Tukey's HSD results
tukey_result

# Display distribution of BD mean by TP
bd.tp.aggregate <- aggregate(BD ~ TP, data = df_clean, mean)
barplot(BD ~ TP, bd.tp.aggregate, main="Body score vs Wine variety")

# Method: BD : Categorical Imputation
bd.tp.agg <- aggregate(BD ~ TP, data = df_clean, mean); bd.tp.agg
names(bd.tp.agg)[2] <- 'BDMeans'
bd.tp.agg

# Join df_clean with bd.tp.agg by TP
df_imputation <- merge(df_clean, bd.tp.agg, by = 'TP'); df_imputation

# Replace the NA's values of BD for BDMeans values
df_clean$BD[is.na(df_clean$BD)] <- df_imputation$BDMeans[is.na(df_imputation$BD)]

summary(df_clean)

#    HANDLING MISSING VALUES IN ACIDITY SCORE ====

## Assumption 1: Test for Normality
shapiro.test(df_clean$ACD)
# p-value < 2.2e-16. So, p < 0.05.
# H0 (the data is normally distributed) is rejected.
# Assumption 1 fails.

## Assumption 2: Test for Homogeneity of variances
# Null Hypothesis (H0): The variances of the groups being compared are equal (homogeneous).
# Alternative Hypothesis (H1): The variances of the groups being compared are not equal (heterogeneous).

lt_result <- leveneTest(ACD ~ PA, data = df_clean); lt_result
# Pr(>F) < 2.2e-16 ***. So, p < 0.05.
# H0 (the groups being compared are homogeneous) is rejected.
# Assumption 2 rejected.

# We will use Kruskal test because the variance it's not homogeneous.
## Assumption 3: Test for independence of observations
# H0: There is NO significant difference between the mean values of the
# categories for furnishing ACD Thus, furnishing ACD and PA maintain no significant relationship.
# Ha: There is a significant difference between the mean values of the
# categories for furnishing ACD Thus, furnishing ACD and PA are less likely to be independent.
kruskal_result <- kruskal.test(ACD ~ PA, data = df_clean); kruskal_result
# Pr(>F) <2e-16 ***. So, p < 0.05.
# There fore H0 is rejected. Thus, furnishing ACD and PA are less likely to be independent.
# Assumption 3 rejected

# How Kruskal indicates significant differences, I run dunn tests.
install.packages("dunn.test")
library(dunn.test)

# Display the DUNN results
dunn_result <- dunn.test(df_clean$ACD, df_clean$PA); dunn_result

# Display distribution of ACD mean by PA
acd.pa.aggregate <- aggregate(ACD ~ PA, data = df_clean, mean)
barplot(ACD ~ PA, acd.pa.aggregate, main="Acidity score vs Production Area")

# Method: ACD : Categorical Imputation
acd.pa.agg <- aggregate(ACD ~ PA, data = df_clean, mean); acd.pa.agg
names(acd.pa.agg)[2] <- 'ACDMeans'
acd.pa.agg

# Join df_clean with acd.pa.agg by PA
df_imputation <- merge(df_clean, acd.pa.agg, by = 'PA'); df_imputation

# Replace the NA's values of ACD for ACDMeans values
df_clean$ACD[is.na(df_clean$ACD)] <- df_imputation$ACDMeans[is.na(df_imputation$ACD)]

summary(df_clean)

#    HANDLING MISSING VALUES IN RESIDUAL SUGAR LEVEL ====

## Assumption 1: Test for Normality
shapiro.test(df_clean$RSG)
# p-value = 3.362e-10. So, p < 0.05.
# H0 (the data is normally distributed) is rejected.
# Assumption 1 fails.

## Assumption 2: Test for Homogeneity of variances
# Null Hypothesis (H0): The variances of the groups being compared are equal (homogeneous).
# Alternative Hypothesis (H1): The variances of the groups being compared are not equal (heterogeneous).

lt_result <- leveneTest(RSG ~ TP, data = df_clean); lt_result
# Pr(>F) < 2.2e-16 ***. So, p < 0.05.
# H0 (the groups being compared are homogeneous) is rejected.
# Assumption 2 rejected.

# We will use Kruskal test because the variance it's not homogeneous.
## Assumption 3: Test for independence of observations
# H0: There is NO significant difference between the mean values of the
# categories for furnishing RSG Thus, furnishing RSG and TP maintain no significant relationship.
# Ha: There is a significant difference between the mean values of the
# categories for furnishing RSG Thus, furnishing RSG and TP are less likely to be independent.
kruskal_result <- kruskal.test(RSG ~ TP, data = df_clean); kruskal_result
# p-value < 2.2e-16. So, p < 0.05.
# There fore H0 is rejected. Thus, furnishing RSG and TP are less likely to be independent.
# Assumption 3 rejected

# How Kruskal indicates significant differences, I run dunn tests.
# Display the DUNN results
dunn_result <- dunn.test(df_clean$RSG, df_clean$TP); dunn_result

# Display distribution of RSG mean by TP
rsg.tp.aggregate <- aggregate(RSG ~ TP, data = df_clean, mean)
barplot(RSG ~ TP, rsg.tp.aggregate, main="Residual sugar level vs Wine variety")

# Method: RSG : Categorical Imputation
rsg.tp.agg <- aggregate(RSG ~ TP, data = df_clean, mean); rsg.tp.agg
names(rsg.tp.agg)[2] <- 'RSGMeans'
rsg.tp.agg

# Join df_clean with rsg.tp.agg by TP
df_imputation <- merge(df_clean, rsg.tp.agg, by = 'TP'); df_imputation

# Replace the NA's values of RSG for RSGMeans values
df_clean$RSG[is.na(df_clean$RSG)] <- df_imputation$RSGMeans[is.na(df_imputation$RSG)]

summary(df_clean)

#    HANDLING MISSING VALUES IN ALCOHOL PERCENTAGE ====

# 1) Calculate the categorical means of the AL column in based of PA and TP.
# 2) Import those categorical means (of the AL column) into the main dataset by using merge.
# 3) Use those categorical means to replace the NAs in the AL column.

## Assumption 1: Test for Normality
shapiro.test(df_clean$AL)
# p-value = 6.21e-12. So, p < 0.05.
# H0 (the data is normally distributed) is rejected.
# Assumption 1 fails.

## Assumption 2: Test for Homogeneity of variances
# Null Hypothesis (H0): The variances of the groups being compared are equal (homogeneous).
# Alternative Hypothesis (H1): The variances of the groups being compared are not equal (heterogeneous).

lt_result <- leveneTest(AL ~ PA * TP, data = df_clean); lt_result
# Pr(>F) = 0.003164 **. So, p < 0.05.
# H0 (the groups being compared are homogeneous) is rejected.
# Assumption 2 rejected.

# As PA * TP are rejected, we will evaluate the homogeneous variances by PA.
lt_result <- leveneTest(AL ~ PA, data = df_clean); lt_result
# Pr(>F) = 0.1418. So, p > 0.05.
# H0 (the groups being compared are homogeneous) is approved
# Assumption 2 approved

## Assumption 3: Test for independence of observations
# H0: There is NO significant difference between the mean values of the
# categories for furnishing AL Thus, furnishing AL and PA maintain no significant relationship.
# Ha: There is a significant difference between the mean values of the
# categories for furnishing AL Thus, furnishing AL and PA are less likely to be independent.
al_anova <- aov(AL ~ PA, data = df_clean); summary(al_anova)
# Pr(>F) = 0.000857 ***. So, p < 0.05.
# There fore H0 is rejected. Thus, furnishing AL and PA are less likely to be independent.
# Assumption 3 rejected

# How ANOVA indicates significant differences, I run hoc tests.
tukey_result <- TukeyHSD(al_anova); tukey_result

# Display distribution of AL mean by PA
al.pa.aggregate <- aggregate(AL ~ PA, data = df_clean, mean)
barplot(AL ~ PA, al.pa.aggregate, main="Alcohol percentage vs Production Area")

# Method: AL : Categorical Imputation
al.pa.agg <- aggregate(AL ~ PA, data = df_clean, mean); al.pa.agg
names(al.pa.agg)[2] <- 'ALMeans'
al.pa.agg

# Join df_clean with al.pa.agg by PA
df_imputation <- merge(df_clean, al.pa.agg, by = 'PA'); df_imputation

# Replace the NA's values of AL for ALMeans values
df_clean$AL[is.na(df_clean$AL)] <- df_imputation$ALMeans[is.na(df_imputation$AL)]

summary(df_clean)

#    HANDLING MISSING VALUES IN TYPICAL DENSITY ====

# 1) Calculate the categorical means of the DN column in based of PA and TP.
# 2) Import those categorical means (of the DN column) into the main dataset by using merge.
# 3) Use those categorical means to replace the NAs in the DN column.

## Assumption 1: Test for Normality
shapiro.test(df_clean$DN)
# p-value = 1.629e-08. So, p < 0.05.
# H0 (the data is normally distributed) is rejected.
# Assumption 1 fails.

## Assumption 2: Test for Homogeneity of variances
# Null Hypothesis (H0): The variances of the groups being compared are equal (homogeneous).
# Alternative Hypothesis (H1): The variances of the groups being compared are not equal (heterogeneous).

lt_result <- leveneTest(DN ~ PA * TP, data = df_clean); lt_result
# Pr(>F) = 0.05376. So, p > 0.05.
# H0 (the groups being compared are homogeneous) is approved.
# Assumption 2 approved.

## Assumption 3: Test for independence of observations
# H0: There is NO significant difference between the mean values of the
# categories for furnishing AL Thus, furnishing AL and PA maintain no significant relationship.
# Ha: There is a significant difference between the mean values of the
# categories for furnishing AL Thus, furnishing AL and PA are less likely to be independent.
dn_anova <- aov(DN ~ PA * TP, data = df_clean); summary(dn_anova)
# PA     Pr(>F) = 0.00148 **
# TP     Pr(>F) < 2e-16 ***
# PA:TP  Pr(>F) > 0.17104
# So, p < 0.05. There fore H0 is rejected. Thus, furnishing AL and PA are less likely to be independent.
# Assumption 3 rejected

# How ANOVA indicates significant differences, I run hoc tests.
tukey_result <- TukeyHSD(dn_anova); tukey_result

# Display distribution of DN mean by PA
al.pa.aggregate <- aggregate(DN ~ PA, data = df_clean, mean)
barplot(DN ~ PA, al.pa.aggregate, main="Typical density vs Production Area")
# Display distribution of DN mean by TP
al.pa.aggregate <- aggregate(DN ~ TP, data = df_clean, mean)
barplot(DN ~ TP, al.pa.aggregate, main="Typical density vs Wine variety")
# Display distribution of DN mean by TP and PA
al.pa.aggregate <- aggregate(DN ~ TP * PA, data = df_clean, mean)
barplot(DN ~ TP + PA, al.pa.aggregate, main="Typical density vs Wine variety and Production Area")

# Method: DN : Categorical Imputation
dn.pa.tp.agg <- aggregate(DN ~ TP * PA, data = df_clean, mean); dn.pa.tp.agg
names(dn.pa.tp.agg)[3] <- 'DNMeans'
dn.pa.tp.agg

# Join df_clean with dn.pa.tp.agg by PA
df_imputation <- merge(df_clean, dn.pa.tp.agg, by = 'PA'); df_imputation

# Replace the NA's values of DN for DNMeans values
df_clean$DN[is.na(df_clean$DN)] <- df_imputation$DNMeans[is.na(df_imputation$DN)]

summary(df_clean)

# STEP-5: ADD NEW VARIABLES IN BASED YEAR & RATING ====
# ADD AGE FROM YEAR

# Calculate AGE from YR
current_year <- 2024
df_clean$AGE <- current_year - df_clean$YR

# Check the structure to confirm changes
str(df_clean)
summary(df_clean)

# View the first few rows to see the AGE column
head(df_clean)

# ADD TOTAL RATING

# Total Rating per Region / Total Testers per Region = Regional Rating Average
df_clean[c('RT', 'NUMR', 'REG', 'PA')]

# Create a total rating feature
df_clean$TOTAL_RATING <- df_clean$RT * df_clean$NUMR

summary(df_clean)
str(df_clean)

# Aggregate and calculate average ratings per region
total.rating.reg.agg <- aggregate(cbind(TOTAL_RATING , NUMR) ~ REG ,data = df_clean, FUN = sum)
total.rating.reg.agg$AVR_RATING <- total.rating.reg.agg$TOTAL_RATING/total.rating.reg.agg$NUMR
summary(total.rating.reg.agg)
# Bar plot of average ratings by region
barplot(AVR_RATING ~ REG ,data=total.rating.reg.agg, main="Average Rating vs Region")

# Aggregate and calculate average ratings by Production Area
total.rating.pa.agg <- aggregate(cbind(TOTAL_RATING , NUMR) ~ PA ,data = df_clean, FUN = sum)
total.rating.pa.agg$AVR_RATING <- total.rating.pa.agg$TOTAL_RATING/total.rating.pa.agg$NUMR
summary(total.rating.pa.agg)
# Bar plot of average ratings by Production Area
barplot(TOTAL_RATING ~ PA ,data=total.rating.pa.agg, main="Total Rating vs Production Area")

qn_df_clean <- df_clean %>% select(where(is.numeric)) %>% na.omit()
chart.Correlation(qn_df_clean)
chart.Correlation(qn_df_clean, method = 'spearman')
boxplot(qn_df_clean$PR, main="Price without NA's")

# Final summary and structure check
str(df_clean)
summary(df_clean)

# STEP-6: DETECTING OUTLIERS =============
# Technique IQR: Boxplot for all numeric variables
numeric_vars <- sapply(df_clean, is.numeric); numeric_vars
df_numeric <- df_clean[numeric_vars]
# Boxplot to detect outliers in numeric variables
for (i in 1:ncol(df_numeric)) {
  boxplot(df_numeric[[i]],
          main = paste("Box Plot of", colnames(df_numeric)[i]),
          col = rgb(i*0.05, i*0.05, i*0.05, 0.8),
          xlab = colnames(df_numeric)[i]
  )
}

#    BODY SCORE ====

# TREATING OUTLIERS
# Since BD has not NA, we will apply the ceiling and floor method.

bd_quant <- quantile(df_clean$BD, c(0.25, 0.75)); bd_quant
bd_iqr <- IQR(df_clean$BD); bd_iqr
bd_out  <- df_clean$BD <= bd_quant[2] + 1.5*bd_iqr |
  df_clean$BD <= bd_quant[1] - 1.5*bd_iqr

df_clean$BD[df_clean$BD >= bd_quant[2] + 1.5*bd_iqr] <- bd_quant[2] + 1.5*bd_iqr
df_clean$BD[df_clean$BD <= bd_quant[2] - 1.5*bd_iqr] <- bd_quant[1] - 1.5*bd_iqr
boxplot(df_clean$BD, col = rgb(0.8, 0.7, 0.6, 0.8), main="Box Plot of Body Score")
str(df_clean)
summary(df_clean)

#    ACIDITY SCORE ====

# TREATING OUTLIERS
# Since ACD has not NA, we will apply the ceiling and floor method.

acd_quant <- quantile(df_clean$ACD, c(0.25, 0.75)); acd_quant
acd_iqr <- IQR(df_clean$ACD); acd_iqr
acd_out  <- df_clean$ACD <= acd_quant[2] + 1.5*acd_iqr |
  df_clean$BD <= acd_quant[1] - 1.5*acd_iqr

df_clean$ACD[df_clean$ACD >= acd_quant[2] + 1.5*acd_iqr] <- acd_quant[2] + 1.5*acd_iqr
df_clean$ACD[df_clean$ACD <= acd_quant[2] - 1.5*acd_iqr] <- acd_quant[1] - 1.5*acd_iqr
boxplot(df_clean$ACD, col = rgb(0.8, 0.7, 0.6, 0.8), main="Box Plot of Body Score")
str(df_clean)
summary(df_clean)


#    NUMBERS OF TESTERS ====

# TREATING OUTLIERS
# Since NUMR has not NA, we will apply the ceiling and floor method.

numr_quant <- quantile(df_clean$NUMR, c(0.25, 0.75)); numr_quant
numr_iqr <- IQR(df_clean$NUMR); numr_iqr
numr_out  <- df_clean$NUMR <= numr_quant[2] + 1.5*numr_iqr |
  df_clean$NUMR <= numr_quant[1] - 1.5*numr_iqr

df_clean$NUMR[df_clean$NUMR >= numr_quant[2] + 1.5*numr_iqr] <- numr_quant[2] + 1.5*numr_iqr
df_clean$NUMR[df_clean$NUMR <= numr_quant[2] - 1.5*numr_iqr] <- numr_quant[1] - 1.5*numr_iqr
boxplot(df_clean$NUMR, col = rgb(0.8, 0.7, 0.6, 0.8), main="Box Plot of Number of Testers")
str(df_clean)
summary(df_clean)

#    TOTAL RATING ====

# TREATING OUTLIERS
# Since TOTAL_RATING has not NA, we will apply the ceiling and floor method.

t_r_quant <- quantile(df_clean$TOTAL_RATING, c(0.25, 0.75)); t_r_quant
t_r_iqr <- IQR(df_clean$TOTAL_RATING); t_r_iqr
t_r_out  <- df_clean$TOTAL_RATING <= t_r_quant[2] + 1.5*t_r_iqr |
  df_clean$TOTAL_RATING <= t_r_quant[1] - 1.5*t_r_iqr

df_clean$TOTAL_RATING[df_clean$TOTAL_RATING >= t_r_quant[2] + 1.5*t_r_iqr] <- t_r_quant[2] + 1.5*t_r_iqr
df_clean$TOTAL_RATING[df_clean$TOTAL_RATING <= t_r_quant[2] - 1.5*t_r_iqr] <- t_r_quant[1] - 1.5*t_r_iqr
boxplot(df_clean$TOTAL_RATING, col = rgb(0.8, 0.7, 0.6, 0.8), main="Box Plot of Total Rating")
str(df_clean)
summary(df_clean)

#    PRICE ====

# We want to know how many rows are excluded in IQR.
pr_quant <- quantile(df_clean$PR, c(0.25, 0.75), na.rm = T); pr_quant
pr_iqr <- IQR(df_clean$PR, na.rm = T); pr_iqr

pr_out <- df_clean$PR<= pr_quant[2] + 1.5*pr_iqr |
  df_clean$PR <= pr_quant[1] - 1.5*pr_iqr
pr_out

# We do not treat outlier in Price by replaced with NA's even if this is a target variable
# because outliers is more than 89.6% of the total data.
length(df_clean$PR[pr_out]) / nrow(df_clean) * 100
# We decided treat the outlier by removing the 1% of the top Price.

# TREATING OUTLIERS
# The range of 1% of the top Price are un stable by removing it we can predicted more accurate.
summary(df_clean$PR)
boxplot(df_clean$PR, main="Wine price")

PR_mod <- df_clean$PR[df_clean$PR < quantile(df_clean$PR, 0.99, na.rm=T)]
boxplot(PR_mod, main="Price below 1% of the top")
max(PR_mod, na.rm=T)

# Split the data frame into two, one with the NAs in the prices and one without
# this to eliminate only 1% of the outliers.
na_rows_pr <- df_clean[is.na(df_clean$PR), ]
non_na_rows_pr <- df_clean[!is.na(df_clean$PR), ]

pr_below_one_percentage  <- non_na_rows_pr[non_na_rows_pr$PR < quantile(non_na_rows_pr$PR, 0.99, na.rm=F), ]

# Join the observations less than 1% of the top with the NA's columns.
df_clean <- rbind(pr_below_one_percentage, na_rows_pr)

# Since Price is the TARGET (ultimate dependent variable), a model based imputation is strongly recommended.

# Final summary and structure check
str(df_clean)
summary(df_clean)

# Final Boxplot in numeric variables
boxplot(df_clean[numeric_vars], main="All numeric variables")

# DESCRIPTIVE ANALYSIS ====
#    Residual Sugar Level vs Average Rating -------
# Scatter plot with a loess smoothing line
plot(df_clean$RSG, df_clean$RT,
     main = "Residual Sugar Level vs Average Rating",
     xlab = "Residual Sugar Level",
     ylab = "Average Rating",
     col = "green",
     pch = 16)  # Solid circle for points

# Fit a loess model for smoothing
loess_model <- loess(RT ~ RSG, data = df_clean)

# Predict values from the loess model
smooth_line <- predict(loess_model, newdata = data.frame(RSG = df_clean$RSG))

# Add the smooth wave line
lines(df_clean$RSG[order(df_clean$RSG)],
      smooth_line[order(df_clean$RSG)],
      col = "blue",
      lwd = 2)

# Add grid lines
grid()

# Optional: Add a legend
legend("topright",
       legend = c("Data Points", "Smooth Line"),
       col = c("green", "blue"),
       pch = c(16, NA),
       lwd = c(NA, 2))
# Finding
# The lower the sugar level of the wine, the higher the score of the sample.

#    Average Price by Wine Variety ------
# Aggregate data to get the mean price per variety
price_variety_agg <- aggregate(PR ~ TP, data = df_clean, FUN = mean, na.rm = TRUE)

# Set up a custom background color
#par(bg = "lightgray")  # Change background color

# Create a bar plot for Price vs Wine Variety
barplot(height = price_variety_agg$PR,
        names.arg = price_variety_agg$TP,
        main = "Average Price by Wine Variety",
        xlab = "Wine Variety",
        ylab = "Average Price (€)",
        col = "lightblue",
        las = 2,       # Rotate the x-axis labels
        cex.names = 0.8, # Reduce the font size of x-axis labels
        border = "black") # Add borders to bars for better visibility

# Add grid lines
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")  # Add grid lines

# Reset background color to default (if needed for further plots)
par(bg = "white")

# Finding
# Market preferences for "Pedro Ximenez", "Priorat Tinto", and "Ribera del Duero Tinto".

#    Average Price vs Age of Wine---------
# Aggregate data to calculate average price by age of wine
agg_data <- aggregate(PR ~ AGE, data = df_clean, FUN = mean, na.rm = TRUE)

# Fit a linear model
lm_model <- lm(PR ~ AGE, data = agg_data)

# Create the line chart
plot(agg_data$AGE, agg_data$PR,
     type = "o",                # "o" for both lines and points
     col = "blue",              # Line color
     xlab = "Age of Wine (Years)",
     ylab = "Average Price (€)",
     main = "Average Price vs Age of Wine",
     pch = 16,                  # Point character
     lwd = 2)                   # Line width

# Add the linear regression line
abline(lm_model, col = "red", lwd = 2)  # Red line for regression

# Add grid lines
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")  # Add grid lines

# Optional: Add a legend
legend("topleft",
       legend = c("Data Points", "Linear Regression"),
       col = c("blue", "red"),
       pch = c(16, NA),
       lwd = c(2, 2))

# Finding
# Increase in average price when the wine is more aged.

#    Average Price and Average Age by Production Area---------
# Aggregate data to calculate average price and average age by production area
agg_data <- aggregate(cbind(Price = PR, Age = AGE) ~ PA, data = df_clean, FUN = mean, na.rm = TRUE)

# Create the bar plot for Average Price
bar_heights <- agg_data$Price
barplot_heights <- barplot(bar_heights,
                           names.arg = agg_data$PA,
                           ylim = c(0, max(bar_heights, na.rm = TRUE) * 1.2), # Adjust y-axis limit
                           col = "pink",
                           ylab = "Average Price (€)",
                           xlab = "Production Area",
                           main = "Average Price and Average Age of Wine by Production Area")

# Add the line plot for Average Age
line_data <- agg_data$Age
lines(barplot_heights, line_data, type = "o", col = "red", lwd = 2, pch = 16)

# Add grid lines
#grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

# Add a legend
legend("topright",
       legend = c("Average Price", "Average Age"),
       fill = c("pink", "red"),
       pch = c(NA, 16),
       lwd = c(NA, 2),
       title = "Legend")

# Finding
# Marked regional differentiation in average prices when the wine is more aged.

#    Average Total Ratings and Average Price of Wine by Wine Variety======
# Aggregate data to get average total ratings and average price by wine variety
agg_data <- aggregate(cbind(AvgTotalRating = TOTAL_RATING, AvgPrice = PR) ~ TP, data = df_clean, FUN = function(x) mean(x, na.rm = TRUE))

# Extract data for plotting
avg_total_ratings <- agg_data$AvgTotalRating
avg_price <- agg_data$AvgPrice

# Names for x-axis
wine_variety_names <- agg_data$TP

# Create the bar chart for Average Total Ratings
bar_heights <- avg_total_ratings
barplot_heights <- barplot(bar_heights,
                           names.arg = wine_variety_names,
                           ylim = c(0, max(c(bar_heights, avg_price), na.rm = TRUE) * 1.5), # Increase y-axis limit
                           col = "lightgreen",
                           ylab = "Average Total Ratings",
                           xlab = "Wine Variety",
                           main = "Average Total Ratings and Average Price of Wine by Wine Variety",
                           las = 2, # Rotate the x-axis labels for better readability
                           border = "black")  # Add borders to bars for better visibility

# Create a secondary y-axis for the average price
par(new = TRUE)  # Create a new plot on top of the existing one
plot(barplot_heights, avg_price,
     type = "o",
     col = "darkorange",
     lwd = 4,           # Increased line width for better visibility
     pch = 16,          # Solid circle for points
     axes = FALSE,      # Do not draw default axes
     xlab = "",
     ylab = "",
     ylim = c(0, max(c(bar_heights, avg_price), na.rm = TRUE) * 1.5)) # Same y-axis limits

# Add secondary axis
axis(side = 4, at = pretty(range(avg_price)), col.axis = "darkorange", col = "darkorange", las = 1)
mtext("Average Price (€)", side = 4, line = 3, col = "darkorange")

# Add grid lines
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

# Add a legend
legend("topright",
       legend = c("Average Total Ratings", "Average Price"),
       fill = c("lightgreen", "darkorange"),
       pch = c(NA, 16),
       lwd = c(NA, 4),
       title = "Legend")

# Finding
# Disconnection between price and total average score for majority of wines.

# PREDICTIVE ANALYSIS =============
library(ggplot2)

#    PREDICT PRICE (y) BASED ON RATING (x) ====

# Split the data set into rows with NA and without NA of the price column.
# This because we want to predict the NA's values of price. So, this NA's
# will be include in our tests.
na_rows <- df_clean[is.na(df_clean$PR), ]
non_na_rows <- df_clean[!is.na(df_clean$PR), ]

# Train-Test split
# We divide the data set into 80% for training and 20% for testing.

# Applied Simple Random Sampling
train_indices <- sample(seq_len(nrow(non_na_rows)), size = 0.8 * nrow(non_na_rows)); train_indices

# Create data set train and data set test (which will be combined with NA rows for 20% of train_indices)
df_clean_train <- non_na_rows[train_indices, ]
df_clean_test <- rbind(non_na_rows[-train_indices, ], na_rows)

summary(df_clean_train)
summary(df_clean_test)

# We see a slightly lineal regression between Rating and Price
# Also, we know that exist a strong positive relationship among those variables.
plot(df_clean_train$RT,df_clean_train$PR,
     main='Rating Vs Price',
     xlab='Rating', ylab='Price');

# Set up a custom background color
#par(bg = "lightgray")  # Change background color

# Simple Linear Regression *******
# We will try to predict the missing values based on Rating.
# Price ~ Rating
pr_lm1 <- lm(PR ~ RT, data = df_clean_train)
ggplot(df_clean_train, aes(x = RT, y = PR)) +
  geom_point() + geom_smooth(method = 'lm');

pr_lm1

# PR ~ RT
# PR = -2225.3*RT + 547.3
summary(pr_lm1)

# R-squared (R2): metrics used to check how well the model fits to our data.
# R-squared:  0.65. The metric is not closest to 1.
# Therefore, our model fits the data regularly.

# Predict NA's values of Price
pr_prection <- predict(pr_lm1, newdata = df_clean_test)
pr_prection

df_compare <- df_clean_test["PR"]
df_compare$predicted_price <- pr_prection; df_compare

predict(pr_lm1, newdata = df_clean_test, interval = 'prediction')

df_clean_test$prediction_variance <- df_clean_test$PR - df_compare$predicted_price
df_clean_test

df_clean_test$prediction_variance_percent <- df_clean_test$prediction_variance/df_clean_test$PR * 100
df_clean_test

#    PREDICT PRICE BASED ON RATING, AGE, RESIDUAL SUGAR, AND ALCOHOL ====

# Split the data set into rows with NA and without NA of the price column.
# This because we want to predict the NA's values of price. So, this NA's
# will be include in our tests.
na_rows <- df_clean[is.na(df_clean$PR), ]
non_na_rows <- df_clean[!is.na(df_clean$PR), ]

# Train-Test split
# We divide the data set into 80% for training and 20% for testing.

# Applied Simple Random Sampling
train_indices <- sample(seq_len(nrow(non_na_rows)), size = 0.8 * nrow(non_na_rows)); train_indices

# Create data set train and data set test (which will be combined with NA rows for 20% of train_indices)
df_clean_train <- non_na_rows[train_indices, ]
df_clean_test <- rbind(non_na_rows[-train_indices, ], na_rows)

# Multiple Linear Regression  *******

# We will try to predict the missing values based on RT, AGE, RSG, and AL.
# Price ~ Rating * AGE * RSG *AL
pr_lm2 <- lm(PR ~ RT + AGE + RSG + AL,
             data = df_clean_train)
pr_lm2

# PR ~ RT + AGE + RSG + AL
# PR = -5944.9171*RT + 1227.0949 + 0.8869 + 90.8693 + 2.6526
summary(pr_lm2)

# R-squared (R2): metrics used to check how well the model fits to our data.
# R-squared:  0.8339 The metric is closest to 1.
# Therefore, our model fits our data well.

# Predict NA's values of Price
pr_prection <- predict(pr_lm2, newdata = df_clean_test)
pr_prection

df_compare <- df_clean_test["PR"]
df_compare$predicted_price <- pr_prection; df_compare

predict(pr_lm2, newdata = df_clean_test, interval = 'prediction')

## Findings

# 1. Positive Relationship between Rating and Price:
# The coefficient for RT is 1227.0949, meaning that for every one-unit increase
# in the rating, wine price increases by approximately 1227.09 euros.
# This suggests a strong positive relationship between higher ratings and higher prices,
# indicating that better-rated wines are perceived as more valuable and expensive.

# 2. Moderate Effect of Age on Price:
# The AGE coefficient is 0.8869, meaning that for each additional year of aging,
# the price of the wine increases by approximately 0.89 euros. While this indicates
# a positive relationship, the impact of age on price is relatively modest compared
# to other factors like rating.

# 3. Importance of Residual Sugar Level:
# The coefficient for RSG is 90.8693, indicating that higher residual sugar levels are
# associated with significant increases in wine prices (approximately 90.87 euros per
# unit increase in RSG). This suggests that wines with higher sugar content tend to be
# valued and priced higher.

# 4. Positive Relationship between Alcohol Content and Price:
# The AL coefficient is 2.6526, meaning that each one-percentage point increase in
# alcohol content results in an approximately 2.65 euro increase in price. Although
# this effect is smaller, it shows that wines with higher alcohol content may command
# slightly higher prices.


#    PREDICT RATING BASED ON RESIDUAL SUGAR, AGE, AND ALCOHOL ====

# Train-Test split
# We divide the data set into 80% for training and 20% for testing.

# Applied Simple Random Sampling
train_indices <- sample(nrow(df_clean), nrow(df_clean)*.8); train_indices

df_clean1_train <- df_clean[train_indices,]
df_clean1_test <- df_clean[-train_indices,]

# Multiple Linear Regression  *******

# We will try to predict the rating values based on RT, AGE, RSG, and AL.
# Rating ~ AGE * RSG *AL
rt_lm1 <- lm(RT ~ AGE + RSG + AL,
             data = df_clean1_train)
rt_lm1

# RT ~ AGE + RSG + AL
# RT = 4.709898*AGE + 0.0019870 + -0.1006520 + 0.0334542
summary(rt_lm1)

# R-squared (R2): metrics used to check how well the model fits to our data.
# R-squared:  0.8795. The metric is closest to 1.
# Therefore, our model fits our data well.

## Findings

# 1. Positive Effect between Rating and Age:
# Each one unit increase in AGE (presumably wine age) is associated with a 0.00199
# increase in RT, which is statistically significant. This suggests that wine age
# has a positive effect on the dependent variable RT.

# 2. Negative Relationship between Rating and Residual Sugar Level:
# Each one unit increase in RSG (residual sugar level) is associated with a 0.10065
# decrease in RT, also significant. This indicates that a higher residual sugar level
# tends to reduce RT.

# 3. Positive Relationship between Rating and Alcohol Score:
# Each one unit increase in AL (alcohol level) is associated with a 0.03345 increase
# in RT, significant. This shows that a higher alcohol level has a positive effect on RT.


# CLUSTER-BASED ANALYSIS =============
#    PREDICT PRODUCTION AREA BASED ON RATING ====
str(df_clean)

hist(df_clean$RT, main = "Histogram of Rating", xlab = "RT")
table(df_clean$PA)

# Verifying production area classification using clustering
# Univariate Analysis of Rating
plot(RT~ as.integer(PA), data = df_clean,
     main = 'Verifying Production Area Classification\nusing Clustering',
     sub = 'Univariate Production Area of Rating',
     xlab = 'Production Area',
     ylab = 'Rating',
     pch = 20,
     col = PA)

# K-Means Cluster Analysis
# We calculate the k-means of the 4 clusters and add them in the vector K-Means.
fit_rt <- kmeans(df_clean$RT, 4); fit_rt; fit_rt$cluster;

# K-means clustering with 4 clusters of sizes 917, 1097, 1453, 162
# Cluster means:
#   [,1]
# 1 3.995267
# 2 4.123810
# 3 4.324694
# 4 4.724074

# Get the clusters
cluster_pa1 <- df_clean[fit_rt$cluster==1,]; cluster_pa1;
cluster_pa2 <- df_clean[fit_rt$cluster==2,]; cluster_pa2;
cluster_pa3 <- df_clean[fit_rt$cluster==3,]; cluster_pa3;
cluster_pa4 <- df_clean[fit_rt$cluster==4,]; cluster_pa4;

# Plot the clusters
df_clean2 <- df_clean
df_clean2$cluster_rt <- fit_rt$cluster;
df_clean2

# We can notice that there is a higher density of data in cluster 4
# and this cluster obtains an mean rating of 4.4724074.
plot(df_clean2$RT, pch=21, bg=df_clean2$cluster_rt * 2 + 3,
     main = 'Verifying Production Area Classification\nusing Clustering',
     sub = 'Univariate Production Area of Rating',
     xlab = 'Production Area',
     ylab = 'Rating');

## Findings

# - The lower internal variability in Cluster 1 and higher internal variability
# in Cluster 3 might suggest that the observations in Cluster 1 are very similar
# to each other, whereas in Cluster 3 there is greater diversity.

# - The clusters with higher mean RT scores (Cluster 4 and Cluster 3) may represent
# areas with higher overall quality or more desirable characteristics compared to
# the clusters with lower scores (Cluster 2 and Cluster 1).


#    PREDICT WINE VARIETY BASED ON ALCOHOL PERCENTAGE ====
str(df_clean)

hist(df_clean$AL, main = "Histogram of Alcohol Percentage", xlab = "AL")
table(df_clean$TP)

# Verifying wine variety classification using clustering
# Univariate Analysis of Alcohol Percentage
plot(AL~ as.integer(TP), data = df_clean,
     main = 'Verifying Wine Variety Classification\nusing Clustering',
     sub = 'Univariate Wine Variety of Alcohol Percentage',
     xlab = 'Wine Variety',
     ylab = 'Alcohol Percentage',
     pch = 20,
     col = TP)

# K-Means Cluster Analysis
# We calculate the k-means of the 4 clusters and add them in the vector K-Means.
fit_al <- kmeans(df_clean$AL, 4); fit_al; fit_al$cluster;

# K-means clustering with 4 clusters of sizes 1143, 1347, 699, 440
# Cluster means:
#   [,1]
# 1 11.73572
# 2 11.19911
# 3 10.60062
# 4 12.43956

# Get the clusters
cluster_al1 <- df_clean[fit_al$cluster==1,]; cluster_al1;
cluster_al2 <- df_clean[fit_al$cluster==2,]; cluster_al2;
cluster_al3 <- df_clean[fit_al$cluster==3,]; cluster_al3;
cluster_al4 <- df_clean[fit_al$cluster==4,]; cluster_al4;

# Plot the clusters
df_clean3 <- df_clean
df_clean3$cluster_al <- fit_al$cluster;
df_clean3

# We can notice that there is a higher density of data in cluster 4
# and this cluster obtains an mean alcohol percentage of 12.43956
plot(df_clean3$AL, pch=21, bg=df_clean3$cluster_al * 2 + 3,
     main = 'Verifying Wine Variety Classification\nusing Clustering',
     sub = 'Univariate Wine Variety of Alcohol Percentage',
     xlab = 'Wine Variety',
     ylab = 'Alcohol Percentage');

## Findings

# - The lower internal variability in Cluster 1 and higher internal 
# variability in Cluster 4 could suggest that the observations in Cluster 
# 1 are more homogeneous in terms of alcohol percentage, whereas the observations 
# in Cluster 4 have greater diversity.

# - The clusters with higher (Cluster 4) and lower (Cluster 3) alcohol 
# percentages could represent different types of wines in terms of alcohol content.
# Cluster 4 has a significantly higher alcohol percentage, which could indicate 
# stronger wines, while Cluster 3 contains wines with lower alcohol content.

# RECOMENDATIONS =============

# Based on Findings we will recommend:

# 1. Focus on Producing Low-Sugar Wines:
# Since lower sugar levels in wine are associated with higher ratings, consider
# adjusting the production process to emphasize wines with lower residual sugar content.
# This can be highlighted in marketing efforts, especially in markets that prioritize
# quality and taste, positioning low-sugar wines as premium options.

# 2. Prioritize Popular Wines in Production:
# Given the market preference for Pedro Ximenez, Priorat Red, and Ribera Del Duero Red,
# it's advisable to allocate more resources to producing and distributing these varietals.
# Ensure a steady supply of these popular wines, especially during peak sales seasons,
# to meet consumer demand.

# 3. Capitalize on the Value of Aged Wines:
# Since older wines tend to have higher average prices, consider emphasizing aged wines
# in your product portfolio. This can be especially profitable in premium and luxury markets.
# Establish a tiered pricing strategy that clearly differentiates wines based on age,
# with aged wines positioned as high-end products. Invest in premium packaging for aged wines
# to further reinforce their luxury status and justify the higher price points.

# 4. Tailor Regional Marketing:
# The significant regional differentiation in the pricing of aged wines suggests the need
# for tailored marketing strategies in different regions. Consider regional preferences,
# economic conditions, and cultural factors when setting prices. Conduct further analysis to
# identify which regions are most responsive to aged wines and adjust pricing strategies accordingly.

# 5. Leveraging Undervalued High-Rated Wines for Market Opportunity
# Reevaluate the pricing strategy for these undervalued varieties to better reflect their quality and
# high ratings. A price adjustment could not only increase profit margins but also reposition the wine
# as a premium option in the market.