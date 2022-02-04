library("readxl")
library(ggplot2)
library(tidyverse)
setwd(getwd())
rm(list = ls())
#
#User can change these values
StartYear <- 2019
StartProjectionYear <- 2022
EndProjectionYear <- 2052
FileName <- 'SCRS_Model_Inputs 2021.xlsx'
#Updates to Input file:

#2021:
#---> MOYNCExistNewDR & OrigDR
#---> ER_NC_CurrentHires
#---> ER_Amo_CurrentHires
#---> ER_Amo_ORP & New / Existing hires
#---> DC_NewHires = 0.15
#---> Multiplier = 1.82%
#---> ORP Offset = 5%
#--->Benefit Payment Percent = 2.7%
#--->Returns: 
#--->  Assumed=0.077139
#--->  Conservative=0.067904
#--->  Volatility=0.1240112

#
#Reading Input File
user_inputs_numeric <- read_excel(FileName, sheet = 'Numeric Inputs')
user_inputs_character <- read_excel(FileName, sheet = 'Character Inputs')
Historical_Data <- read_excel(FileName, sheet = 'Historical Data')
Scenario_Data <- read_excel(FileName, sheet = 'Inv_Returns')
BenefitPayments <- read_excel(FileName, sheet = 'Benefit Payments')
PayrollData <- read_excel(FileName, sheet = 'Payroll')

#
##################################################################################################################################################################
#

#DC_NewHires <- 0.75

#Functions for later use
#Function for Present Value for Amortization
PresentValue = function(rate, nper, pmt) {
  PV = pmt * (1 - (1 + rate) ^ (-nper)) / rate * (1 + rate)
  return(PV)
}

#Function for calculating amo payments
#pmt0 = basic amo payment calculation, assuming payment beginning of period 
PMT0 <- function(r, nper, pv) {
  if (r == 0) {
    a <- pv/nper 
  } else {
    a <- pv*r*(1+r)^(nper-1)/((1+r)^nper-1)  
  }
  
  if(nper == 0){
    a <- 0
  }
  
  return(a)
}

#pmt = amo payment function with growth rate and timing added; t = 1 for end of period payment, 0.5 for half period. 
PMT <- function(r, g = 0, nper, pv, t) {
  a <- PMT0((1+r)/(1+g) - 1, nper, pv*(1+r)^t)
  return(a)
}
#
##################################################################################################################################################################
#
#Replace NAs
Historical_Data[is.na(Historical_Data)] <- 0
#
#Reading Values from Input input file and assigning values
#Assigning numeric inputs
for(i in 1:nrow(user_inputs_numeric)){
  if(!is.na(user_inputs_numeric[i,2])){
    assign(as.character(user_inputs_numeric[i,2]),as.double(user_inputs_numeric[i,3]))
  }
}
#
#Assigning character inputs
for(i in 1:nrow(user_inputs_character)){
  if(!is.na(user_inputs_character[i,2])){
    assign(as.character(user_inputs_character[i,2]),as.character(user_inputs_character[i,3]))
  }
}

#Create an empty Matrix for the Projection Years
EmptyMatrix <- matrix(0,(EndProjectionYear - StartProjectionYear + 1), 1)
for(j in 1:length(colnames(Historical_Data))){
  TempMatrix <- rbind(as.matrix(Historical_Data[,j]), EmptyMatrix)
  assign(as.character(colnames(Historical_Data)[j]), TempMatrix)
}
#Assign values for Projection Years
FYE <- StartYear:EndProjectionYear
#Get Start Index, since historical data has 3 rows, we want to start at 4
StartIndex <- StartProjectionYear - StartYear + 1
HistoricalIndex <- StartProjectionYear - StartYear

#Assign values for simulation
if(SimType == 'Assumed'){
  SimReturn <- SimReturnAssumed
} else if(AnalysisType == 'Conservative'){
  SimReturn <- SimReturnConservative
}

#Initialize Amortization and Outstnading Base
RowColCount <- (EndProjectionYear - StartProjectionYear + 1)
OutstandingBase_CurrentHires <- matrix(0,RowColCount, RowColCount + 1)
Amortization_CurrentHires <- matrix(0,RowColCount, RowColCount + 1)
AmoYearsInput_CurrentHires <- read_excel(FileName, sheet = 'Amortization_CurrentHires')

OutstandingBase_NewHires <- matrix(0,RowColCount, RowColCount + 1)
Amortization_NewHires <- matrix(0,RowColCount, RowColCount + 1)
AmoYearsInput_NewHires <- read_excel(FileName, sheet = 'Amortization_NewHires')

#DC_NewHires
#Vol <- user_inputs_numeric[41,3]
#View(Historical_Data)
#View(Scenario_Data[4,6:7])
#
##################################################################################################################################################################
#
for(i in StartIndex:length(FYE)){
  #Payroll
  TotalPayroll[i] <- TotalPayroll[i-1]*(1 + Payroll_growth)
  TERIPayroll[i] <- TERIPayroll[i-1]*(1 + Payroll_growth)
  AllNewHires[i] <- TotalPayroll[i]*PayrollData$PayrollNewPct[i]
  AllCurrentHires[i] <- TotalPayroll[i]*PayrollData$PayrollLegacyPct[i]
  ORPCurrentPayroll[i] <- AllCurrentHires[i]*DC_OldHires
  ORPNewPayroll[i] <- AllNewHires[i]*DC_NewHires
  
  DBLegacyPayroll[i] <- AllCurrentHires[i] - ORPCurrentPayroll[i]
  DBNewHirePayroll[i] <- AllNewHires[i] - ORPNewPayroll[i]
  Tier2Payroll[i] <- PayrollData$Tier2Payroll[i]
  Tier3Payroll[i] <- DBLegacyPayroll[i] - Tier2Payroll[i]
  RehPayroll[i] <- (RehPayroll[i-1] + TERIPayroll[i-1])*(1 + Payroll_growth) - TERIPayroll[i]
  
  ProjectedPayroll[i] <- (AllCurrentHires[i] - ORPCurrentPayroll[i])*(1 + Payroll_ratio)
  ProjectedTier23Payroll[i] <- (Tier2Payroll[i] + Tier3Payroll[i])*(1 + Payroll_ratio)
  ProjectedTier4Payroll[i] <-  AllNewHires[i]*(1 + Payroll_ratio)
  
  #Discount Rate, Original, New and Blended
  OriginalDR_CurrentHires[i] <- dis_r_currentHires
  NewDR_CurrentHires[i] <- dis_r_proj_currentHires
  OriginalDR_NewHires[i] <- dis_r_newHires
  NewDR_NewHires[i] <- dis_r_proj_newHires
  BlendedDR[i] <- ((1 + NewDR_CurrentHires[i]) / (1 + AmoBaseInc)) - 1
  
  #Benefit Payments, Admin Exp, Refund
  BenPayments_CurrentHires[i] <- BenefitPayments$BP_CurrentHires[i]
  BenPayments_NewHires[i] <- BenefitPayments$BP_NewHires[i]
  Refunds_CurrentHires[i] <- BenefitPayments$Refunds_CurrentHires[i]
  Refunds_NewHires[i] <- BenefitPayments$Refunds_NewHires[i]
  AdminExp_CurrentHires[i] <- -Admin_Exp_Pct*DBLegacyPayroll[i]
  AdminExp_NewHires[i] <- -Admin_Exp_Pct*DBNewHirePayroll[i]
  
  #NC, Liability - Original DR
  MOYNCExistOrigDR[i] <- Tier2Payroll[i]*NC_Tier2 + Tier3Payroll[i]*NC_Tier3
  MOYNCNewHiresOrigDR[i] <- DBNewHirePayroll[i]*NC_Tier4
  AccrLiabOrigDR_CurrentHires[i] <- AccrLiabOrigDR_CurrentHires[i-1]*(1+OriginalDR_CurrentHires[i]) + (MOYNCExistOrigDR[i] + BenPayments_CurrentHires[i] + Refunds_CurrentHires[i])*(1+OriginalDR_CurrentHires[i])^0.5
  AccrLiabOrigDR_NewHires[i] <- AccrLiabOrigDR_NewHires[i-1]*(1+OriginalDR_NewHires[i]) + (MOYNCNewHiresOrigDR[i] + BenPayments_NewHires[i] + Refunds_NewHires[i])*(1+OriginalDR_NewHires[i])^0.5
  AccrLiabOrigDR_Total[i] <- AccrLiabOrigDR_CurrentHires[i] + AccrLiabOrigDR_NewHires[i]
  
  #NC, Liability - New DR
  DRDifference_CurrentHires <- 100*(OriginalDR_CurrentHires[i] - NewDR_CurrentHires[i])
  DRDifference_NewHires <- 100*(OriginalDR_NewHires[i] - NewDR_NewHires[i])
  MOYNCExistNewDR[i] <- MOYNCExistOrigDR[i]*((1+(NCSensDR/100))^(DRDifference_CurrentHires))
  MOYNCNewHiresNewDR[i] <- MOYNCNewHiresOrigDR[i]*((1+(NCSensDR/100))^(DRDifference_NewHires))
  AccrLiabNewDR_CurrentHires[i] <- AccrLiabOrigDR_CurrentHires[i]*((1+(LiabSensDR/100))^(DRDifference_NewHires))*((1+(Convexity/100))^((DRDifference_NewHires)^2/2))
  AccrLiabNewDR_NewHires[i] <- AccrLiabOrigDR_NewHires[i]*((1+(LiabSensDR/100))^(DRDifference_NewHires))*((1+(Convexity/100))^((DRDifference_NewHires)^2/2))
  AccrLiabNewDR_Total[i] <- AccrLiabOrigDR_Total[i]*((1+(LiabSensDR/100))^(DRDifference_NewHires))*((1+(Convexity/100))^((DRDifference_NewHires)^2/2))
}

RunModel <- function(Analysis_Type = AnalysisType, 
                     Sim_Return = SimReturn, 
                     Sim_Volatility = SimVolatility, 
                     ERPolicyCurrentHires = ER_Policy_CurrentHires, 
                     ERPolicyNewHires = ER_Policy_NewHires, 
                     Scen_Type = ScenType,
                     ADEC_Trigger = ADECTrigger,
                     CostSharingNC = CostSharing_NC,
                     CostSharingAmo = CostSharing_Amo,
                     CostSharing_Pct = CostSharingPct,
                     ORPOffsetDBNC = ORPOffset_DBNC){
  
  #Default is statutory, change to ADC amo policy if need be
   if(ERPolicyCurrentHires == 'ADEC'){
     AmoYearsInput_CurrentHires[,2] <- 20
   }
  
   if(ERPolicyNewHires == 'ADEC'){
    AmoYearsInput_CurrentHires[,2] <- 20
   }
  
  #Offset Matrix. Used later for Amortization calculation
  OffsetYears_CurrentHires <- matrix(0,RowColCount, RowColCount)
  for(i in 1:nrow(OffsetYears_CurrentHires)){
    for(j in 1:i){
      OffsetYears_CurrentHires[i,j] <- max(AmoYearsInput_CurrentHires[i,2] - j + 2, 1) 
    }
  }
  
  OffsetYears_NewHires <- matrix(0,RowColCount, RowColCount)
  for(i in 1:nrow(OffsetYears_NewHires)){
    for(j in 1:i){
      OffsetYears_NewHires[i,j] <- max(AmoYearsInput_NewHires[i,2] - j + 2, 1) 
    }
  }
  
  # #Initialize first row Outstanding Base
  #Amortization is not done for the first period so its not done here
  OutstandingBase_CurrentHires[1,1] <- UAL_AVA_CurrentHires[HistoricalIndex]
  OutstandingBase_NewHires[1,1] <- UAL_AVA_NewHires[HistoricalIndex]
  #
  #Initialize Cumulative ER
  Total_ER[StartIndex-1] <- 0
  #Scenario Index for referencing later based on investment return data
  ScenarioIndex <- which(colnames(Scenario_Data) == as.character(Scen_Type))
  
  #The NC, Liabilities are calculated separately because the later rates such as NC, Amo, etc. have a lag of i-2 and
  #so to avoid contradictions, i put them separately
  for(i in StartIndex:length(FYE)){
    #ProjectionCount is used because amortization and return scenarios do not start at the same time as start index
    #Because start index includes historical data and thus might be 3 by the time ProjectionCount is 1
    ProjectionCount <- i - StartIndex + 1
    
    #Normal Cost and Amo Rate. set this condition so that i + 2 is valid
    if(i <= (length(FYE) - 2)){
      NC_CurrentHires[i] <- Admin_Exp_Pct + MOYNCExistNewDR[i+2]/(Tier2Payroll[i+2] + Tier3Payroll[i+2])
      NC_NewHires[i] <- Admin_Exp_Pct + MOYNCNewHiresNewDR[i+2]/DBNewHirePayroll[i+2] 
    }
    
    #The rates here are for Amortization years - 2
    AmoRate_CurrentHires[i-2] <- sum(Amortization_CurrentHires[ProjectionCount,]) / (TotalPayroll[i] + RehPayroll[i])
    AmoRate_NewHires[i-2] <- sum(Amortization_NewHires[ProjectionCount,]) / AllNewHires[i]
    if(AllNewHires[i] == 0) {AmoRate_NewHires[i-2] <- 0}
    
    if(i >= (StartIndex+1)){
      if(CostSharingAmo == 'Yes'){
        EE_AmoRate_NewHires[i] <- AmoRate_NewHires[i]/2
      } else {
        EE_AmoRate_NewHires[i] <- 0
      }
    }
    
    #These values have a lag of 2 years which is why the initial years are based on historical #s
    EE_NC_CurrentHires[i] <- EmployeeNC_CurrentHires[i-2]*DBLegacyPayroll[i]
    EE_OtherHires[i] <- EmployeeNC_CurrentHires[i-2]*(TERIPayroll[i] + RehPayroll[i])
    EE_NC_NewHires[i] <- EmployeeNC_NewHires[i-2]*DBNewHirePayroll[i]
    EE_Amo_NewHires[i] <- EE_AmoRate_NewHires[i-2]*DBNewHirePayroll[i]
    ER_NC_CurrentHires[i] <- NC_CurrentHires[i-2]*DBLegacyPayroll[i] - EE_NC_CurrentHires[i]
    ER_NC_NewHires[i] <- NC_NewHires[i-2]*DBNewHirePayroll[i] - EE_NC_NewHires[i]
    Total_Contrib_DC[i] <- DC_Contrib*(ORPCurrentPayroll[i] + ORPNewPayroll[i])
    
    #Values based on ER Policy
    if(ERPolicyCurrentHires == 'Statutory Rate'){
      if((ADEC_Trigger == 'No') | ((ADEC_Trigger == 'Yes') && (FR_AVA[i-1] <  1))){
        ER_Amo_CurrentHires[i] <- ER_AmoRate_CurrentHires[i-2]*DBLegacyPayroll[i]
        ER_Amo_Stat_NewHires[i] <- ER_AmoRate_NewHires[i-2]*DBNewHirePayroll[i]
        ER_Amo_ORP_CurrentHires[i] <- max(ER_AmoRate_ORP[i-2],0)*ORPCurrentPayroll[i]
        ER_Amo_ORP_NewHires[i] <- max(ER_AmoRate_ORP[i-2],0)*ORPNewPayroll[i]
        ER_Amo_RehRet[i] <- ER_AmoRate_Rehire[i-2]*RehPayroll[i]
      } else {
        ER_Amo_CurrentHires[i] <- AmoRate_CurrentHires[i-2]*(TotalPayroll[i] + RehPayroll[i])
        ER_Amo_Stat_NewHires[i] <- 0
        ER_Amo_ORP_CurrentHires[i] <- 0
        ER_Amo_ORP_NewHires[i] <- 0
        ER_Amo_RehRet[i] <- 0
      }
    } else {
      ER_Amo_CurrentHires[i] <- AmoRate_CurrentHires[i-2]*(TotalPayroll[i] + RehPayroll[i])
      ER_Amo_Stat_NewHires[i] <- 0
      ER_Amo_ORP_CurrentHires[i] <- 0
      ER_Amo_ORP_NewHires[i] <- 0
      ER_Amo_RehRet[i] <- 0
    }
    
    if(ERPolicyNewHires == 'Statutory Rate'){
      if((ADEC_Trigger == 'No') || ((ADEC_Trigger == 'Yes') && (FR_AVA[i-1] <  1))){
        ER_Amo_ADC_NewHires[i] <- 0
      } else {
        ER_Amo_ADC_NewHires[i] <- max(-ER_NC_CurrentHires[i]-ER_NC_NewHires[i]-ER_Amo_CurrentHires[i],AmoRate_NewHires[i-2]*AllNewHires[i]-EE_Amo_NewHires[i])
      }
    } else {
      ER_Amo_ADC_NewHires[i] <- max(-ER_NC_CurrentHires[i]-ER_NC_NewHires[i]-ER_Amo_CurrentHires[i],AmoRate_NewHires[i-2]*AllNewHires[i]-EE_Amo_NewHires[i])
    }
    
    #Set default to 0 for easier coding
    #Only code if for yes condition
    StatContFloor[i] <- 0
    if(StatFloor_FullFunding == 'Yes'){
      if(max(FR_AVA[StartIndex:i-1]) < 1){
        TotalContValue <- TotalContPost[i-2]*(TotalPayroll[i] + RehPayroll[i])
        Cashflows <- ER_NC_CurrentHires[i] + ER_NC_NewHires[i] + ER_Amo_CurrentHires[i] + 
          ER_Amo_Stat_NewHires[i] + ER_Amo_ADC_NewHires[i] + ER_Amo_ORP_CurrentHires[i] + 
          ER_Amo_ORP_NewHires[i] + ER_Amo_RehRet[i] + Total_Contrib_DC[i]
        StatContFloor[i] <- max(TotalContValue - Cashflows,0)
      }
    }
    #Cash Flows and Solvency Contribution
    Cashflows <- BenPayments_CurrentHires[i] + BenPayments_NewHires[i] + Refunds_CurrentHires[i] +  Refunds_NewHires[i] + 
      AdminExp_CurrentHires[i] + AdminExp_NewHires[i] + EE_NC_CurrentHires[i] + EE_NC_NewHires[i] +
      EE_Amo_NewHires[i] + EE_OtherHires[i] + ER_NC_CurrentHires[i] + ER_NC_NewHires[i] +
      ER_Amo_CurrentHires[i] + ER_Amo_Stat_NewHires[i] + ER_Amo_ADC_NewHires[i] + ER_Amo_ORP_CurrentHires[i] + 
      ER_Amo_ORP_NewHires[i] + ER_Amo_RehRet[i] + StatContFloor[i]
    Solv_Contrib_Total[i] <- as.double(max(-(MVA[i-1]*(1+ROA_MVA[i]) + Cashflows*(1+ROA_MVA[i])^0.5) / (1+ROA_MVA[i])^0.5,0))
    Solv_Contrib_CurrentHires[i] <- Solv_Contrib_Total[i]*(AccrLiabNewDR_CurrentHires[i]/AccrLiabOrigDR_Total[i])
    Solv_Contrib_NewHires[i] <- Solv_Contrib_Total[i]*(AccrLiabNewDR_NewHires[i]/AccrLiabOrigDR_Total[i])
    
    #Return based on Deterministic or Stochastic
    if(Analysis_Type == 'Stochastic'){
      ROA_MVA[i] <- rnorm(1,Sim_Return,Sim_Volatility)
    } else if(Analysis_Type == 'Deterministic'){
      ROA_MVA[i] <- as.double(Scenario_Data[i,ScenarioIndex]) 
    }
    
    #Net CF, Expected MVA, Solvency Contribution
    NetCF_CurrentHires[i] <- BenPayments_CurrentHires[i] + Refunds_CurrentHires[i] +  AdminExp_CurrentHires[i] +
      EE_NC_CurrentHires[i] + EE_OtherHires[i] + ER_NC_CurrentHires[i] + ER_Amo_CurrentHires[i] + 
      ER_Amo_ORP_CurrentHires[i] + ER_Amo_RehRet[i] + Solv_Contrib_CurrentHires[i] + StatContFloor[i]
    ExpInvInc_CurrentHires[i] <- (MVA_CurrentHires[i-1]*NewDR_CurrentHires[i-1]) + (NetCF_CurrentHires[i]*NewDR_CurrentHires[i-1]*0.5)
    MVA_CurrentHires[i] <- MVA_CurrentHires[i-1]*(1+ROA_MVA[i]) + NetCF_CurrentHires[i]*(1+ROA_MVA[i])^0.5
    ActualReturnMVA_CurrentHires[i] <- MVA_CurrentHires[i] - MVA_CurrentHires[i-1] - NetCF_CurrentHires[i]
    
    NetCF_NewHires[i] <- BenPayments_NewHires[i] + Refunds_NewHires[i] + AdminExp_NewHires[i] + EE_NC_NewHires[i] +
      EE_Amo_NewHires[i] + ER_NC_NewHires[i] + ER_Amo_Stat_NewHires[i] + ER_Amo_ADC_NewHires[i] + ER_Amo_ORP_NewHires[i]
    ExpInvInc_NewHires[i] <- (MVA_NewHires[i-1]*NewDR_NewHires[i-1]) + (NetCF_NewHires[i]*NewDR_NewHires[i-1]*0.5)
    MVA_NewHires[i] <- MVA_NewHires[i-1]*(1+ROA_MVA[i]) + NetCF_NewHires[i]*(1+ROA_MVA[i])^0.5
    ActualReturnMVA_NewHires[i] <- MVA_NewHires[i] - MVA_NewHires[i-1] - NetCF_NewHires[i]
    
    #Gain Loss, Defered Losses
    GainLoss_CurrentHires[i] <- ActualReturnMVA_CurrentHires[i] - ExpInvInc_CurrentHires[i] 
    DeferedCurYear_CurrentHires[i] <- GainLoss_CurrentHires[i]*(0.8/1)
    Year1GL_CurrentHires[i] <- DeferedCurYear_CurrentHires[i-1]*(0.6/0.8)
    Year2GL_CurrentHires[i] <- Year1GL_CurrentHires[i-1]*(0.4/0.6)
    Year3GL_CurrentHires[i] <- Year2GL_CurrentHires[i-1]*(0.2/0.4)
    TotalDefered_CurrentHires[i] <- Year1GL_CurrentHires[i] + Year2GL_CurrentHires[i] + Year3GL_CurrentHires[i] + DeferedCurYear_CurrentHires[i]
    
    GainLoss_NewHires[i] <- ActualReturnMVA_NewHires[i] - ExpInvInc_NewHires[i] 
    DeferedCurYear_NewHires[i] <- GainLoss_NewHires[i]*(0.8/1)
    Year1GL_NewHires[i] <- DeferedCurYear_NewHires[i-1]*(0.6/0.8)
    Year2GL_NewHires[i] <- Year1GL_NewHires[i-1]*(0.4/0.6)
    Year3GL_NewHires[i] <- Year2GL_NewHires[i-1]*(0.2/0.4)
    TotalDefered_NewHires[i] <- Year1GL_NewHires[i] + Year2GL_NewHires[i] + Year3GL_NewHires[i] + DeferedCurYear_NewHires[i]
    
    #AVA, MVA, UAL, FR
    AVA_CurrentHires[i] <- MVA_CurrentHires[i] - TotalDefered_CurrentHires[i]
    UAL_AVA_CurrentHires[i] <- AccrLiabNewDR_CurrentHires[i] - AVA_CurrentHires[i]
    UAL_MVA_CurrentHires[i] <- AccrLiabNewDR_CurrentHires[i] - MVA_CurrentHires[i]
    
    AVA_NewHires[i] <- MVA_NewHires[i] - TotalDefered_NewHires[i]
    UAL_AVA_NewHires[i] <- AccrLiabNewDR_NewHires[i] - AVA_NewHires[i]
    UAL_MVA_NewHires[i] <- AccrLiabNewDR_NewHires[i] - MVA_NewHires[i]
    
    UAL_AVA[i] <- UAL_AVA_CurrentHires[i] + UAL_AVA_NewHires[i]
    UAL_MVA[i] <- UAL_MVA_CurrentHires[i] + UAL_MVA_NewHires[i]
    AVA[i] <- AVA_CurrentHires[i] + AVA_NewHires[i]
    MVA[i] <- MVA_CurrentHires[i] + MVA_NewHires[i]
    FR_AVA[i] <- AVA[i] / AccrLiabNewDR_Total[i]
    FR_MVA[i] <- MVA[i] / AccrLiabNewDR_Total[i]
    UAL_AVA_InflAdj[i] <- UAL_AVA[i] / ((1 + asum_infl)^(FYE[i] - NC_StaryYear))
    UAL_MVA_InflAdj[i] <- UAL_MVA[i] / ((1 + asum_infl)^(FYE[i] - NC_StaryYear))
    
    #Total Contrib, ER Contrib Rate and $ Amount
    Total_Contrib_DB[i] <-  ER_NC_CurrentHires[i] + ER_NC_NewHires[i] + ER_Amo_CurrentHires[i] + ER_Amo_Stat_NewHires[i] + ER_Amo_ADC_NewHires[i] + 
      ER_Amo_ORP_CurrentHires[i] + ER_Amo_ORP_NewHires[i] + ER_Amo_RehRet[i] + Solv_Contrib_Total[i]
    
    Total_Contrib[i] <- Total_Contrib_DC[i] + Total_Contrib_DB[i]
    ER_InflAdj[i] <- Total_Contrib[i] / ((1 + asum_infl)^(FYE[i] - NC_StaryYear))
    ER_Percentage[i] <- Total_Contrib[i] / (TotalPayroll[i] + RehPayroll[i])
    Total_ER[i] <- Total_ER[i-1] + ER_InflAdj[i]      #Make sure that the Total_ER number before StartIndex equals 0  
    AllInCost[i] <- Total_ER[i] + UAL_MVA_InflAdj[i]
    
    #Amortization
    #Current Hires
    if(ProjectionCount < nrow(Amortization_CurrentHires)){
      #Oustanding Balance
      OutstandingBase_CurrentHires[ProjectionCount+1,2:(ProjectionCount + 1)] <- OutstandingBase_CurrentHires[ProjectionCount,1:ProjectionCount]*(1 + NewDR_CurrentHires[i-1]) - (Amortization_CurrentHires[ProjectionCount,1:ProjectionCount]*(1 + NewDR_CurrentHires[i-1])^0.5)
      OutstandingBase_CurrentHires[ProjectionCount+1,1] <- UAL_AVA_CurrentHires[i] - sum(OutstandingBase_CurrentHires[ProjectionCount+1,2:ncol(OutstandingBase_CurrentHires)])
      
      #Amo Layers
      #Start at "2:(ProjectionCount + 1)" because amortization period 1 is not done
      Amortization_CurrentHires[ProjectionCount+1,2:(ProjectionCount + 1)] <- PMT(pv = OutstandingBase_CurrentHires[ProjectionCount+1,2:(ProjectionCount + 1)], 
                                                                                  r = NewDR_CurrentHires[i], g = AmoBaseInc, t = 0.5,
                                                                                  nper = pmax(OffsetYears_CurrentHires[ProjectionCount+1,2:(ProjectionCount + 1)],1))
    }
    
    #New Hires
    if(ProjectionCount < nrow(Amortization_NewHires)){
      #Oustanding Balance
      OutstandingBase_NewHires[ProjectionCount+1,2:(ProjectionCount + 1)] <- OutstandingBase_NewHires[ProjectionCount,1:ProjectionCount]*(1 + NewDR_NewHires[i-1]) - (Amortization_NewHires[ProjectionCount,1:ProjectionCount]*(1 + NewDR_NewHires[i-1])^0.5)
      OutstandingBase_NewHires[ProjectionCount+1,1] <- UAL_AVA_NewHires[i] - sum(OutstandingBase_NewHires[ProjectionCount+1,2:ncol(OutstandingBase_NewHires)])
      
      #Amo Layers
      #Start at "2:(ProjectionCount + 1)" because amortization period 1 is not done
      Amortization_NewHires[ProjectionCount+1,2:(ProjectionCount + 1)] <- PMT(pv = OutstandingBase_NewHires[ProjectionCount+1,2:(ProjectionCount + 1)],
                                                                              r = NewDR_NewHires[i], g = AmoBaseInc, t = 0.5,
                                                                              nper = pmax(OffsetYears_NewHires[ProjectionCount+1,2:(ProjectionCount + 1)],1))
    }
    
    #Annuity Factor
    if(UAL_AVA[i] < 0){
      AnnuityFactor[i] <- 0
    } else {
      AnnuitySum <- EE_Amo_NewHires[i] + EE_OtherHires[i] + ER_Amo_CurrentHires[i] + ER_Amo_Stat_NewHires[i] +
        ER_Amo_ADC_NewHires[i] + ER_Amo_ORP_CurrentHires[i] + ER_Amo_ORP_NewHires[i] + ER_Amo_RehRet[i] + 
        Solv_Contrib_Total[i] + StatContFloor[i]
      
      AnnuityFactor[i] <- UAL_AVA[i]*(1+NewDR_CurrentHires[i])^0.5 / AnnuitySum
    }
    
    #Implied and Max Funding Period
    TempValue <- (1+BlendedDR[i])/(1+BlendedDR[i]*(1-AnnuityFactor[i]))
    if(TempValue < 0){
      ImplFundPeriodPre[i] <- 999
    } else {
      ImplFundPeriodPre[i] <- log(TempValue) / log(1+BlendedDR[i]) 
    }
    MaxFundPeriod[i] <- max(MaxFundPeriod[i-1]-1,20)
    
    #Total Contribution Pre and Post Adjustment
    if(FYE[i] <= 2022){
      TotalContPre[i] <- TotalContPre[i-1] + MaxAdjContrib
    } else if(FR_AVA[i-1] < ContFloor){
      TotalContPre[i] <-TotalContPre[i-1]
    } else {
      TotalContPre[i] <- TotalContPre[i-1] - MaxAdjContrib 
    }
    
    if(ImplFundPeriodPre[i] <= MaxFundPeriod[i]){
      TotalContPost[i] <- TotalContPre[i] 
    } else {
      TotalContPost[i] <- TotalContPost[i-1] + MaxAdjContrib
    }
    
    #Employee NC, Amo Rates, etc.
    #TempValue <- min(EmployeeNC_CurrentHires[i-1]+(TotalContPost[i]-TotalContPost[i-1])/2,MaxEEContrib)
    #EmployeeNC_CurrentHires[i] <- max(TempValue,0)
    EmployeeNC_CurrentHires[i] <- MaxEEContrib
    
    if(CostSharingNC == 'Yes'){
      EmployeeNC_NewHires[i] <- NC_NewHires[i]/2
    } else {
      EmployeeNC_NewHires[i] <- EmployeeNC_CurrentHires[i]
    }
    
    EmployerNC_CurrentHires[i] <- NC_CurrentHires[i] - EmployeeNC_CurrentHires[i]
    EmployerNC_NewHires[i] <- NC_NewHires[i] - EmployeeNC_NewHires[i]
    
    ER_AmoRate_CurrentHires[i] <- TotalContPost[i] - EmployerNC_CurrentHires[i]
    ER_AmoRate_NewHires[i] <- TotalContPost[i] - EmployerNC_NewHires[i]
    ER_AmoRate_Rehire[i] <- TotalContPost[i]
    
    if(ORPOffsetDBNC == 'Yes' | FR_AVA[i-1] >= 1){
      ER_AmoRate_ORP[i] <- TotalContPost[i] - EmployerNC_CurrentHires[i]
      
    } else {
      ER_AmoRate_ORP[i] <- TotalContPost[i] - ORPOffset
    }
  }
  
  #Join all the outputs together
  #Initialize Output as the first column FYE
  Output <- FYE
  for(i in 2:length(Historical_Data)){
    Output <- cbind(Output,get(colnames(Historical_Data)[i]))
  }
  return(as.data.frame(Output))
}
#
# ##################################################################################################################################################################
#
#Scenarios
Scenario_Returns <- as.data.frame(FYE)
Scenario_UAL <- as.data.frame(FYE)
Scenario_FR <- as.data.frame(FYE)
Scenario_ER_Percentage <- as.data.frame(FYE)
Scenario_ER_InflAdj <- as.data.frame(FYE)
Scenario_Total_ER <- as.data.frame(FYE)
Scenario_AllIn_ER <- as.data.frame(FYE)

Scenarios <- c('Assumption','6% Constant','Recession','Recurring Recession')

############ RunModel ##############
x <- RunModel(Scen_Type = Scenarios[1],
              ERPolicyCurrentHires = 'Statutory Rate', 
              ERPolicyNewHires = 'Statutory Rate',
              ADEC_Trigger = 'Yes',
              CostSharingNC = 'No',
              CostSharingAmo = 'No',
              ORPOffsetDBNC = 'No')

#View(x %>% select(Output, 
#                  TotalContPost, 
#                  FR_AVA, ER_Percentage, 
                  #EmployeeNC_NewHires, 
                  #ER_AmoRate_NewHires, 
                  #ER_Amo_ADC_NewHires, 
                  #ER_Amo_CurrentHires, 
#                  AVA, 
#                  UAL_AVA))
#########

for (i in 1:length(Scenarios)){
  NewData <- RunModel( Scen_Type = Scenarios[i],
                       ERPolicyCurrentHires = 'Statutory Rate', 
                       ERPolicyNewHires = 'Statutory Rate',
                       ADEC_Trigger = 'Yes',
                       CostSharingNC = 'No',
                       CostSharingAmo = 'No',
                       ORPOffsetDBNC = 'No')
  
  Scenario_Returns <- cbind(Scenario_Returns,NewData$ROA_MVA)
  Scenario_UAL <- cbind(Scenario_UAL,NewData$UAL_MVA_InflAdj)
  Scenario_FR <- cbind(Scenario_FR,NewData$FR_MVA)
  Scenario_ER_Percentage <- cbind(Scenario_ER_Percentage,NewData$ER_Percentage)
  Scenario_ER_InflAdj <- cbind(Scenario_ER_InflAdj,NewData$ER_InflAdj)
  Scenario_Total_ER <- cbind(Scenario_Total_ER,NewData$Total_ER)
  Scenario_AllIn_ER <- cbind(Scenario_AllIn_ER,NewData$AllInCost)
  
  #Start from StartIndex because thats when the projection is
  #Total ER is already inflation adjusted
  TotalERScenario <- sum(Scenario_Total_ER[nrow(Scenario_Total_ER),i+1])/1000
  #inflation adjusted UAL
  EndingUAL <- Scenario_UAL[nrow(Scenario_UAL),i+1]/1000
  AllInER <- Scenario_AllIn_ER[nrow(Scenario_AllIn_ER),i+1]/1000
  
  if(i == 1){
    ERCostTable <- c(TotalERScenario,EndingUAL, AllInER)
  } else {
    ERCostTable <- rbind(ERCostTable, c(TotalERScenario,EndingUAL, AllInER))
  }
}
#Scenario names should be the same as Scenarios but for some cases like supplemental, lv$, etc. it will be different
ScenarioNames <- Scenarios
colnames(ERCostTable) <- c('Total ER Contributions','Ending UAL','All in ER Cost')
rownames(ERCostTable) <- ScenarioNames

colnames(Scenario_Returns) <- c('FYE',ScenarioNames)
colnames(Scenario_UAL) <- c('FYE',ScenarioNames)
colnames(Scenario_FR) <- c('FYE',ScenarioNames)
colnames(Scenario_ER_Percentage) <- c('FYE',ScenarioNames)
colnames(Scenario_ER_InflAdj) <- c('FYE',ScenarioNames)

ScenarioPlot <- function(Data, YAxisLabel){
  ggplot(Data, aes(x = FYE)) +
    geom_line(aes(y = Data[,2]), color = "#FF6633", size = 2) +
    geom_line(aes(y = Data[,3]), color = "#FFCC33", size = 2) +
    geom_line(aes(y = Data[,4]), color = "#0066CC", size = 2) +
    geom_line(aes(y = Data[,5]), color = "#CC0000", size = 2) +
    labs(y = YAxisLabel, x = 'Year') + ggtitle(YAxisLabel)
  #scale_linetype_manual(labels = '')
}

#View(Scenario_FR)
ScenarioPlot(Scenario_FR, 'Funded Ratio (MVA)')
#View(Scenario_UAL)
#write_csv(Scenario_ER_Percentage, "SCRS.FModel_ERC_ADEC.csv")
#
##################################################################################################################################################################
#
#Simulations
start_time <- Sys.time()
#Set seed insures a consistency when simulations are run multiple times
set.seed((1234))
NumberofSimulations <- 10
#initialize the return simulations based on years and # of simulations
Returns_Sims <- matrix(1:length(FYE),nrow = length(FYE), ncol = NumberofSimulations + 1)
UAL_Sims <- matrix(1:length(FYE),nrow = length(FYE), ncol = NumberofSimulations + 1)
FR_Sims <- matrix(1:length(FYE),nrow = length(FYE), ncol = NumberofSimulations + 1)
ER_Sims <- matrix(1:length(FYE),nrow = length(FYE), ncol = NumberofSimulations + 1)

#Run the simulations
for (i in 1:NumberofSimulations){
  NewData <- RunModel(ERPolicyCurrentHires = 'Statutory Rate', 
                      ERPolicyNewHires = 'Statutory Rate', 
                      Analysis_Type = 'Stochastic')
  
  Returns_Sims[,i+1] <- NewData$ROA_MVA
  UAL_Sims[,i+1] <- NewData$UAL_MVA_InflAdj
  FR_Sims[,i+1] <- NewData$FR_MVA
  ER_Sims[,i+1] <- NewData$ER_Percentage
}

Simulations_Returns <- cbind(FYE,FYE,FYE)
Simulations_UAL <- cbind(FYE,FYE,FYE)
Simulations_FR <- cbind(FYE,FYE,FYE)
Simulations_ER <- cbind(FYE,FYE,FYE)

#Get the 25th, 50th, 75th percentile
for(i in 1:length(FYE)){
  Simulations_Returns[i,] <- t(quantile(Returns_Sims[i,2:ncol(Returns_Sims)],c(0.25,0.5,0.75)))
  Simulations_UAL[i,] <- t(quantile(UAL_Sims[i,2:ncol(UAL_Sims)],c(0.25,0.5,0.75)))
  Simulations_FR[i,] <- t(as.data.frame(quantile(FR_Sims[i,2:ncol(FR_Sims)],c(0.25,0.5,0.75))))
  Simulations_ER[i,] <- t(quantile(ER_Sims[i,2:ncol(ER_Sims)],c(0.25,0.5,0.75)))
}

#plot the graphs
SimulationPlot <- function(Data, FYE){
  Data <- (as.data.frame(Data))
  Data <- cbind(FYE, Data)
  colnames(Data) <- c('FYE','25th Percentile', '50th Percentile', '75th Percentile')
  ggplot(Data, aes(x = Data[,1])) +
    geom_line(aes(y = Data[,2]), color = "#FF6633", size = 2) +
    geom_line(aes(y = Data[,3]), color = "#FFCC33", size = 2) +
    geom_line(aes(y = Data[,4]), color = "#0066CC", size = 2)
}
#SimulationPlot(Simulations_FR, FYE)



end_time <- Sys.time()
print(end_time - start_time)
