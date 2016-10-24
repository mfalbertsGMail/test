###################################################################################################
# START OF SOLVAS|CAPITAL HEADER - DO NOT MODIFY 
###################################################################################################

# declare diagnostic variables 
sc_undefined = "undefined"
# set array with all the sc_variables to track for existences and return status information
# add new sc_ variables  - passed in or used for diagnostic result set
sc_var_name = c("sc_is_running_from_server","sc_connection_string", "sc_event_id", "sc_fs_id", "sc_diag_rodbc_lib", "sc_diag_solvas_utility_lib", "sc_diag_connection")

# check for required libary RODBC
sc_diag_rodbc_lib =
  tryCatch(
    {
      library(RODBC)
      "success"
    },  
    warning = function(cond) paste("ERROR:  ", cond),
    error = function(cond)  paste("ERROR:  ", cond) 
  )

# check for required library Solvas.Capital.Utility
sc_diag_solvas_utility_lib =
  tryCatch(
    {
      library(Solvas.Capital.SQLUtility)
      "success"
    },  
    warning = function(cond) paste("ERROR:  ", cond),
    error = function(cond) paste("ERROR:  ", cond)	  
  )


# check for odbc connectivity
sc_diag_connection = 
  tryCatch(
    {
      cn <- odbcDriverConnect(connection=sc_connection_string)
      print(sqlQuery(cn, "SELECT CURRENT_USER", errors=TRUE))
      odbcClose(cn)
      "success"
    } ,  
    warning = function(cond) paste("ERROR:  ", cond),
    error = function(cond)  paste("ERROR:  ", cond)	  
)
# set vector with variable values, use sc_undefined if variable does not exist - get0 checks if the var name is a variable ifnotfound is a parameter name to get0
sc_var_value = unname((sapply(sc_var_name,get0,ifnotfound=sc_undefined))) 
sc_result_set = data.frame(sc_var_name, sc_var_value, stringsAsFactors = FALSE)

print(sc_result_set)
# push the results to the SQL server parameter from sp_execute_external_script
sc_output_table <- as.data.frame(sc_result_set)
###################################################################################################
# END OF SOLVAS|CAPITAL HEADER - DO NOT MODIFY 
###################################################################################################


# Read In -----------------------------------------------------------------

#Inputs
InputALLL <- read.csv("~/R/R Files/InputALLL.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
InputBancware <- read.csv("~/R/R Files/InputBancware.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
InputBasel1 <- read.csv("~/R/R Files/InputBasel1.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
InputBudgetBS <- read.csv("~/R/R Files/InputBudgetBS.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
InputBudgetIS <- read.csv("~/R/R Files/InputBudgetIS.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
InputCapitalPlan <- read.csv("~/R/R Files/InputCapitalPlan.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
InputEconometricModel <- read.csv("~/R/R Files/InputEconometricModel.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
InputModelAssumptions <- read.csv("~/R/R Files/InputModelAssumptionsTab.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
InputMortgageBanking <- read.csv("~/R/R Files/InputMortgageBanking.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
InputParentCFForecast <- read.csv("~/R/R Files/InputParentCFforecast.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
InputRegCapital <- read.csv("~/R/R Files/InputRegCapital.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
InputOffBSItems <- read.csv("~/R/R Files/InputOffBSItems.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
InputSprBS <- read.csv("~/R/R Files/InputSubProRataBS.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
InputSprIS <- read.csv("~/R/R Files/InputSubProRataIS.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
InputTMBANKBS <- read.csv("~/R/R Files/InputTMRKBankBS.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
InputTMBANKIS <- read.csv("~/R/R Files/InputTMRKBankIS.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
InputTMBANKCalcBS <- read.csv("~/R/R Files/InputTMBankCalcBS.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
InputTMBANKCalcIS <- read.csv("~/R/R Files/InputTMBankCalcIS.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
InputValueDriverAssumpt <- read.csv("~/R/R Files/InputValueDriverAssumpt.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)

#Outputs
BalanceSheetOutput <- read.csv("~/R/R Files/BalanceSheetOutput.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
IncomeStatementOutput <- read.csv("~/R/R Files/IncomeStatementOutput.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
OutputBasel3Cap <- read.csv("~/R/R Files/OutputBasel3Cap.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
OutputDTA <- read.csv("~/R/R Files/OutputDefTaxAssets.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
OutputCapBasel1 <- read.csv("~/R/R Files/OutputBasel1Cap.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
OutputParentCF <- read.csv("~/R/R Files/OutputParentCF.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
OutputSuppItems <- read.csv("~/R/R Files/OutputSuppItems.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)
OutputRWABasel1 <- read.csv("~/R/R Files/OutputBasel1RWA.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)


# Begin - Balance Sheet ---------------------------------



for(two in 2)
{
  for(one in 1)
  {
    for(oneten in 1)
    {  
      for(twelve in 12)
      {
        for(eight in 8)
        {
          for(thirt in 13)
          {
            for(three in 3)
            {
              for(four in 4)
              {
                for(eleven in 11)
                {
              # BS - Assets ------------------------------------------------------------------
              k <- as.numeric(InputBancware['MTM Adjust on Existing AFS',oneten])
              m <- as.numeric(InputEconometricModel['Available for Sale',oneten])
              BalanceSheetOutput['AFS Investments',oneten] <- k+m
              remove(k,m)
              
              m <- as.numeric(InputEconometricModel['Held to Maturity',oneten])
              k <- as.numeric(InputBudgetBS['Cum MTM adjust on AFS Sec trans to HTM', ,eleven])
              BalanceSheetOutput['HTM Investments',oneten] <-k+m
              remove(k,m)
              
              k <-as.numeric(BalanceSheetOutput['AFS Investments',1])
              l <-as.numeric(BalanceSheetOutput['HTM Investments',1])
              m <-as.numeric(InputBudgetBS['HTM Securities',11])
              n <-as.numeric(InputBudgetBS['AFS Securities',11])
              BalanceSheetOutput['Investment Adjustments',1] <- (m+n)-(k+l)
              remove(k,l,m,n)
           
              BalanceSheetOutput['Investment Adjustments',two]<-BalanceSheetOutput['Investment Adjustments',one]

              k<-as.numeric(BalanceSheetOutput['AFS Investments',oneten])
              l<-as.numeric(BalanceSheetOutput['HTM Investments',oneten])
              m<-as.numeric(BalanceSheetOutput['Investment Adjustments',oneten])
              BalanceSheetOutput['Total Investments',oneten]=(k+l+m)
              remove(k,l,m)
              
              l<-as.numeric(InputEconometricModel['C&I',oneten])
              BalanceSheetOutput['C&I',oneten]<-l
              m<-as.numeric(InputEconometricModel['CRE',oneten])
              BalanceSheetOutput['CRE',oneten]<-m
              n<-as.numeric(InputEconometricModel['Construction',oneten])
              BalanceSheetOutput['Construction',oneten]<-n
              o<-as.numeric(InputEconometricModel['Residential',oneten])
              BalanceSheetOutput['Residential',oneten]<-o
              p<-as.numeric(InputEconometricModel['Consumer',oneten])
              BalanceSheetOutput['Consumer',oneten]<-p
              q<-as.numeric(InputEconometricModel['Other',oneten])
              BalanceSheetOutput['Other',oneten]<-q
              r<-as.numeric(InputEconometricModel['S&P Subdivisions',oneten])
              BalanceSheetOutput['S&P Subdivisions',oneten]<-r
              
              remove(l,m,n,o,p,q,r)
              
              l<-InputEconometricModel['C&I',1]
              l<- as.numeric(l)
              m<-InputEconometricModel['CRE',1]
              m<- as.numeric(m)
              n<-InputEconometricModel['Construction',1]
              n<- as.numeric(n)
              o<-InputEconometricModel['Residential',1]
              o<- as.numeric(o)
              p<-InputEconometricModel['Consumer',1]
              p<- as.numeric(p)
              q<-InputEconometricModel['Other',1]
              q<- as.numeric(q)
              r<-InputEconometricModel['S&P Subdivisions',1]
              r<- as.numeric(r)
              
              s <- InputBudgetBS['Total Loans and Leases', 11]
              s <- as.numeric(s)
              
              BalanceSheetOutput['Loan Adjustments',1] <- s-(l+m+n+o+p+q+r)
              
              
              BalanceSheetOutput['Loan Adjustments',two]<-BalanceSheetOutput['Loan Adjustments',one]
              
              remove(l,m,n,o,p,q,r,s)
              
              l<-as.numeric(BalanceSheetOutput['C&I',oneten])
              m<-as.numeric(BalanceSheetOutput['CRE',oneten])
              n<-as.numeric(BalanceSheetOutput['Construction',oneten])
              o<-as.numeric(BalanceSheetOutput['Residential',oneten])
              p<-as.numeric(BalanceSheetOutput['Consumer',oneten])
              q<-as.numeric(BalanceSheetOutput['Other',oneten])
              r<-as.numeric(BalanceSheetOutput['S&P Subdivisions',oneten])
              s<-as.numeric(BalanceSheetOutput['Loan Adjustments',oneten])
              
              BalanceSheetOutput['Total Loans',oneten]<-l+m+n+o+p+q+r+s
              
              remove(l,m,n,o,p,q,r,s)
              
              
              BalanceSheetOutput['FDIC LSA Receivable',oneten]<-InputBudgetBS['FDIC Indemnification Asset',eleven]
              BalanceSheetOutput['FDIC LSA Receivable',oneten][is.na(BalanceSheetOutput['FDIC LSA Receivable',oneten])] <- 0
              
              
              BalanceSheetOutput['Mortgage Servicing Rights',oneten]<-InputBudgetBS['Mortgage Servicing Rights',eleven]
              
              k<- as.numeric(InputBudgetBS['Other Assets',eleven])
              l<- as.numeric(InputBudgetBS['Premises and Equipment, net',eleven])
              
              BalanceSheetOutput['Other Assets',oneten]<- k+l
              remove(k,l)
              
              
              # BS - Liabilities -------------------------------------------------------------
              
              BalanceSheetOutput['DDA',oneten]<-InputEconometricModel['DDA',oneten]
              BalanceSheetOutput['NOW',oneten]<-InputEconometricModel['NOW',oneten]
              BalanceSheetOutput['MMDA',oneten]<-InputEconometricModel['MMDA',oneten]
              BalanceSheetOutput['Savings',oneten]<-InputEconometricModel['SAV',oneten]
              BalanceSheetOutput['CDs',oneten]<-InputEconometricModel['CDs',oneten]
              
              k<- as.numeric(BalanceSheetOutput['DDA',1])
              l<- as.numeric(BalanceSheetOutput['NOW',1])
              m<- as.numeric(BalanceSheetOutput['MMDA',1])
              n<- as.numeric(BalanceSheetOutput['Savings',1])
              o<- as.numeric(BalanceSheetOutput['CDs',1])
              p<- as.numeric(InputBudgetBS['Total Deposits',11])
              
              BalanceSheetOutput['Deposit Adjustments',1]<- p-(k+l+m+n+o)
              
              
              BalanceSheetOutput['Deposit Adjustments',two]<-BalanceSheetOutput['Deposit Adjustments',one]
              
              
              
              remove(k,l,m,n,o,p)
              
              k<- as.numeric(BalanceSheetOutput['DDA',oneten])
              l<- as.numeric(BalanceSheetOutput['NOW',oneten])
              m<- as.numeric(BalanceSheetOutput['MMDA',oneten])
              n<- as.numeric(BalanceSheetOutput['Savings',oneten])
              o<- as.numeric(BalanceSheetOutput['CDs',oneten])
              p<- as.numeric(BalanceSheetOutput['Deposit Adjustments',oneten])
              
              BalanceSheetOutput['Total Deposits',oneten]<- (k+l+m+n+o+p)
              
              remove(k,l,m,n,o,p)
              
              BalanceSheetOutput['Short Term Borrowings',1]<-InputBudgetBS['Short-term Borrow (includes Fed Fund Purch)',11]
              BalanceSheetOutput['Short Term Borrowings',2:10]<-InputBancware['Short Term Debt Bal',2:10]
              
              
              k<- as.numeric(InputBudgetBS['Long-term FHLB Advances',11])
              k[is.na(k)] <- 0
              l<- as.numeric(InputBudgetBS['Qualifying Subordinated Debt',11])
              l[is.na(l)] <- 0
              m<- as.numeric(InputBudgetBS['Other Subordinated Debt',11])
              m[is.na(m)] <- 0
              n<- as.numeric(InputBudgetBS['Qualifying Trust Pref Sec',11])
              n[is.na(n)] <- 0
              o<- as.numeric(InputBudgetBS['Other Trust Pref Sec',11])
              o[is.na(o)] <- 0
              
              BalanceSheetOutput['Long Term Borrowings',1]<- (k+l+m+n+o)
              
              remove(k,l,m,n,o)
              
              k2<-as.numeric(InputBancware['Long Term Debt Bal',2])
              k3<-as.numeric(InputBancware['Long Term Debt Bal',3])
              k4<-as.numeric(InputBancware['Long Term Debt Bal',4])
              k5<-as.numeric(InputBancware['Long Term Debt Bal',5])
              k6<-as.numeric(InputBancware['Long Term Debt Bal',6])
              k7<-as.numeric(InputBancware['Long Term Debt Bal',7])
              k8<-as.numeric(InputBancware['Long Term Debt Bal',8])
              k9<-as.numeric(InputBancware['Long Term Debt Bal',9])
              k10<-as.numeric(InputBancware['Long Term Debt Bal',10])
              
              n2<- sum(as.numeric(InputCapitalPlan['Senior Debt Issued ($ Actual)',2]))
              n3<- sum(as.numeric(InputCapitalPlan['Senior Debt Issued ($ Actual)',2:3]))
              n4<- sum(as.numeric(InputCapitalPlan['Senior Debt Issued ($ Actual)',2:4]))
              n5<- sum(as.numeric(InputCapitalPlan['Senior Debt Issued ($ Actual)',2:5]))
              n6<- sum(as.numeric(InputCapitalPlan['Senior Debt Issued ($ Actual)',2:6]))
              n7<- sum(as.numeric(InputCapitalPlan['Senior Debt Issued ($ Actual)',2:7]))
              n8<- sum(as.numeric(InputCapitalPlan['Senior Debt Issued ($ Actual)',2:8]))
              n9<- sum(as.numeric(InputCapitalPlan['Senior Debt Issued ($ Actual)',2:9]))
              n10<- sum(as.numeric(InputCapitalPlan['Senior Debt Issued ($ Actual)',2:10]))
              
              m2<- sum(as.numeric(InputCapitalPlan['Qualified Subordinated Debt Issued ($ Actual)',2]))
              m3<- sum(as.numeric(InputCapitalPlan['Qualified Subordinated Debt Issued ($ Actual)',2:3]))
              m4<- sum(as.numeric(InputCapitalPlan['Qualified Subordinated Debt Issued ($ Actual)',2:4]))
              m5<- sum(as.numeric(InputCapitalPlan['Qualified Subordinated Debt Issued ($ Actual)',2:5]))
              m6<- sum(as.numeric(InputCapitalPlan['Qualified Subordinated Debt Issued ($ Actual)',2:6]))
              m7<- sum(as.numeric(InputCapitalPlan['Qualified Subordinated Debt Issued ($ Actual)',2:7]))
              m8<- sum(as.numeric(InputCapitalPlan['Qualified Subordinated Debt Issued ($ Actual)',2:8]))
              m9<- sum(as.numeric(InputCapitalPlan['Qualified Subordinated Debt Issued ($ Actual)',2:9]))
              m10<- sum(as.numeric(InputCapitalPlan['Qualified Subordinated Debt Issued ($ Actual)',2:10]))
              
              l2<- sum(as.numeric(InputCapitalPlan['Senior Debt Maturing or Repurchased ($ Actual)',2]))
              l3<- sum(as.numeric(InputCapitalPlan['Senior Debt Maturing or Repurchased ($ Actual)',2:3]))
              l4<- sum(as.numeric(InputCapitalPlan['Senior Debt Maturing or Repurchased ($ Actual)',2:4]))
              l5<- sum(as.numeric(InputCapitalPlan['Senior Debt Maturing or Repurchased ($ Actual)',2:5]))
              l6<- sum(as.numeric(InputCapitalPlan['Senior Debt Maturing or Repurchased ($ Actual)',2:6]))
              l7<- sum(as.numeric(InputCapitalPlan['Senior Debt Maturing or Repurchased ($ Actual)',2:7]))
              l8<- sum(as.numeric(InputCapitalPlan['Senior Debt Maturing or Repurchased ($ Actual)',2:8]))
              l9<- sum(as.numeric(InputCapitalPlan['Senior Debt Maturing or Repurchased ($ Actual)',2:9]))
              l10<- sum(as.numeric(InputCapitalPlan['Senior Debt Maturing or Repurchased ($ Actual)',2:10]))
              
              o2<- sum(as.numeric(InputCapitalPlan['Qualified Subordinated Debt Maturing ($ Actual)',2]))
              o3<- sum(as.numeric(InputCapitalPlan['Qualified Subordinated Debt Maturing ($ Actual)',2:3]))
              o4<- sum(as.numeric(InputCapitalPlan['Qualified Subordinated Debt Maturing ($ Actual)',2:4]))
              o5<- sum(as.numeric(InputCapitalPlan['Qualified Subordinated Debt Maturing ($ Actual)',2:5]))
              o6<- sum(as.numeric(InputCapitalPlan['Qualified Subordinated Debt Maturing ($ Actual)',2:6]))
              o7<- sum(as.numeric(InputCapitalPlan['Qualified Subordinated Debt Maturing ($ Actual)',2:7]))
              o8<- sum(as.numeric(InputCapitalPlan['Qualified Subordinated Debt Maturing ($ Actual)',2:8]))
              o9<- sum(as.numeric(InputCapitalPlan['Qualified Subordinated Debt Maturing ($ Actual)',2:9]))
              o10<- sum(as.numeric(InputCapitalPlan['Qualified Subordinated Debt Maturing ($ Actual)',2:10]))
              
              p2<- sum(as.numeric(InputCapitalPlan['Trust Preferred Redemption ($ Actual)',2]))
              p3<- sum(as.numeric(InputCapitalPlan['Trust Preferred Redemption ($ Actual)',2:3]))
              p4<- sum(as.numeric(InputCapitalPlan['Trust Preferred Redemption ($ Actual)',2:3]))
              p5<- sum(as.numeric(InputCapitalPlan['Trust Preferred Redemption ($ Actual)',2:3]))
              p6<- sum(as.numeric(InputCapitalPlan['Trust Preferred Redemption ($ Actual)',2:3]))
              p7<- sum(as.numeric(InputCapitalPlan['Trust Preferred Redemption ($ Actual)',2:3]))
              p8<- sum(as.numeric(InputCapitalPlan['Trust Preferred Redemption ($ Actual)',2:3]))
              p9<- sum(as.numeric(InputCapitalPlan['Trust Preferred Redemption ($ Actual)',2:3]))
              p10<- sum(as.numeric(InputCapitalPlan['Trust Preferred Redemption ($ Actual)',2:3]))
              
              BalanceSheetOutput['Long Term Borrowings',2]<-k2+(n2+m2)- (l2+o2+p2)
              BalanceSheetOutput['Long Term Borrowings',3]<-k3+(n3+m3)- (l3+o3+p3)
              BalanceSheetOutput['Long Term Borrowings',4]<-k4+(n4+m4)- (l4+o4+p4)
              BalanceSheetOutput['Long Term Borrowings',5]<-k5+(n5+m5)- (l5+o5+p5)
              BalanceSheetOutput['Long Term Borrowings',6]<-k6+(n6+m6)- (l6+o6+p6)
              BalanceSheetOutput['Long Term Borrowings',7]<-k7+(n7+m7)- (l7+o7+p7)
              BalanceSheetOutput['Long Term Borrowings',8]<-k8+(n8+m8)- (l8+o8+p8)
              BalanceSheetOutput['Long Term Borrowings',9]<-k9+(n9+m9)- (l9+o9+p9)
              BalanceSheetOutput['Long Term Borrowings',10]<-k10+(n10+m10)- (l10+o10+p10)
              
              remove(k2,k3,k4,k5,k6,k7,k8,k9,k10)
              remove(m2,m3,m4,m5,m6,m7,m8,m9,m10)
              remove(l2,l3,l4,l5,l6,l7,l8,l9,l10)
              remove(n2,n3,n4,n5,n6,n7,n8,n9,n10)
              remove(o2,o3,o4,o5,o6,o7,o8,o9,o10)
              remove(p2,p3,p4,p5,p6,p7,p8,p9,p10)
              
              
              BalanceSheetOutput['Other Liabilities',oneten]<-InputBudgetBS['Other Liabilities',eleven]
              
              # BS - Stockholders Equity -----------------------------------------------------
              
              k<- as.numeric(InputBudgetBS['Common Stock',11])
              l<- as.numeric(InputBudgetBS['Capital Surplus',11])
              
              BalanceSheetOutput['Common Stock',1]<- (k+l)
              
              remove(k,l)
              
              BalanceSheetOutput['Common Stock',two]<- as.numeric(BalanceSheetOutput['Common Stock',1])+as.numeric(InputCapitalPlan['Common Stock Issuance ($ Actual)',two])
              
              
              o<- as.numeric(InputBudgetBS['Treasury Stock',11])
              o[is.na(o)] <- 0
              BalanceSheetOutput['Treasury Stock',1]<-o
              
              BalanceSheetOutput['Treasury Stock',two]<- as.numeric(BalanceSheetOutput['Treasury Stock',1])+as.numeric(InputCapitalPlan['Common Stock Buyback ($ Actual)',two])
              
              o<- as.numeric(InputBudgetBS['Preferred Stock',11])
              o[is.na(o)] <- 0
              BalanceSheetOutput['Preferred Stock',1]<-o
              
              BalanceSheetOutput['Preferred Stock',two]<- as.numeric(BalanceSheetOutput['Preferred Stock',1])+as.numeric(InputCapitalPlan['Tier 1 Preferred Stock Issuance/Buyback ($ Actual)',two])
              
              o<- as.numeric(InputValueDriverAssumpt['Ending AOCI rel to AFS/HTM Sec',oneten])
              o[is.na(o)] <- 0
              BalanceSheetOutput['Unrealized Gain / Loss on AFS Securities, net',oneten]<-o
              
              
              k<- as.numeric(InputValueDriverAssumpt['AOCI - Other',oneten])
              k[is.na(k)] <- 0
              BalanceSheetOutput['Other AOCI Adjustments, net',oneten]<-k
              
              remove(o,k)
              
              k<- as.numeric(BalanceSheetOutput['Unrealized Gain / Loss on AFS Securities, net',oneten])
              m<- as.numeric(BalanceSheetOutput['Other AOCI Adjustments, net',oneten])
              
              BalanceSheetOutput['AOCI',oneten]<-k+m
              
              remove(k,m)
              # End - Balance Sheet---------------------------------
              
              # Begin - Income Statement------------------------------------------------
              # IS - Interest Income ---------------------------------------------------------
              
              k <- as.numeric(InputBudgetIS['Interest Income - w/o Acquired Loans',11])
              m <- as.numeric(InputBudgetIS['Interest Income - Acquired Loans',11])
              IncomeStatementOutput['Loan Interest Income',1] = k+m
              remove(k,m)
              
              h <- as.numeric(InputBancware['C&I',two])
              i <- as.numeric(InputBancware['CRE',two])
              j <- as.numeric(InputBancware['Construction',two])
              k <- as.numeric(InputBancware['Residential',two])
              l <- as.numeric(InputBancware['Consumer',two])
              m <- as.numeric(InputBancware['Other',two])
              n <- as.numeric(InputBancware['S&P Subdivisions',two])
              
              o <- as.numeric(BalanceSheetOutput['Loan Adjustments',two])
              
              p <- as.numeric(InputEconometricModel['C&I',one])
              q <- as.numeric(InputEconometricModel['CRE',one])
              r <- as.numeric(InputEconometricModel['Construction',one])
              s <- as.numeric(InputEconometricModel['Residential',one])
              t <- as.numeric(InputEconometricModel['Consumer',one])
              u <- as.numeric(InputEconometricModel['Other',one])
              v <- as.numeric(InputEconometricModel['S&P Subdivisions',one])
              
              w <- as.numeric(InputEconometricModel['C&I',two])
              x <- as.numeric(InputEconometricModel['CRE',two])
              y <- as.numeric(InputEconometricModel['Construction',two])
              z <- as.numeric(InputEconometricModel['Residential',two])
              a <- as.numeric(InputEconometricModel['Consumer',two])
              b <- as.numeric(InputEconometricModel['Other',two])
              c <- as.numeric(InputEconometricModel['S&P Subdivisions',two])
              
              IncomeStatementOutput['Loan Interest Income',two]<- (h+i+j+k+l+m+n)+(o*((h+i+j+k+l+m+n)/mean((p+q+r+s+t+u+v+w+x+y+z+a+b+c))))
              
              remove(h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a,b,c)
              
              
              k <- as.numeric(InputBudgetIS['Interest Income - Securities',11])
              IncomeStatementOutput['Investments - Securities',1] <- k
              remove(k)
              
              h <- as.numeric(InputBancware['Securities (AFS and HTM)',two])
              i <- as.numeric(BalanceSheetOutput['Investment Adjustments',two])
              
              j <- as.numeric(InputEconometricModel['Available for Sale',one])
              k <- as.numeric(InputEconometricModel['Held to Maturity',one])
              
              l <- as.numeric(InputEconometricModel['Available for Sale',two])
              m <- as.numeric(InputEconometricModel['Held to Maturity',two])
              
              IncomeStatementOutput['Investments - Securities',two]<- h +(i*(h/mean(j+k+l+m)))
              
              k <- as.numeric(InputBudgetIS['FDIC LSA Receivable',eleven])
              k[is.na(k)] <- 0
              IncomeStatementOutput['FDIC LSA Receivable',oneten]<-k
              # Excel Model had this Zeroed out, placeholder in case future sheets have item
              
              k <- as.numeric(InputBudgetIS['Excess Cash Interest Income',11])
              k[is.na(k)] <- 0
              IncomeStatementOutput['Excess Cash Interest Income',1]<-k
              
              remove(k)
              
              
              # ISSUE WITH EXCESS CASH & EQUIV -----
              
              h <- as.numeric(InputBancware['Excess Cash Reinv Rate (5-year Treas)',two])
              h[is.na(h)] <- 0
              i <- as.numeric(BalanceSheetOutput['Excess Cash & Equivalents',one])
              i[is.na(i)] <- 0
              IncomeStatementOutput['Excess Cash Interest Income',two]<-(h/4)*i
              # May need to revisit this code if values are pulled in, in future use
              
              remove(h,i)
              
              k <- as.numeric(InputBudgetIS['Interest Income - Other',11])
              k[is.na(k)] <- 0
              IncomeStatementOutput['Other Interest Income',1]<-k
              
              remove(k)
              
              k <- as.numeric(InputBudgetIS['Interest Income - Other',twelve])
              k[is.na(k)] <- 0
              l <- as.numeric(InputValueDriverAssumpt['Other Int inc Stress Factor (Reduct in Fees)',two])
              l[is.na(l)] <- 0
              m <- as.numeric(InputBancware['Loan Fees',two])
              
              IncomeStatementOutput['Other Interest Income',two]<- (k*(1-l))+m
              
              remove(k,l,m)
              
              
              k <- as.numeric(IncomeStatementOutput['Loan Interest Income',oneten])
              l <- as.numeric(IncomeStatementOutput['Investments - Securities',oneten])
              m <- as.numeric(IncomeStatementOutput['FDIC LSA Receivable',oneten])
              n <- as.numeric(IncomeStatementOutput['Excess Cash Interest Income',oneten])
              o <- as.numeric(IncomeStatementOutput['Other Interest Income',oneten])
              
              IncomeStatementOutput['Total Interest Income',oneten]<- k+l+m+n+o
              
              remove(k,l,m,n,o)
              
              IncomeStatementOutput['DDA',oneten] <- as.numeric(InputBancware['DDA',oneten])
              IncomeStatementOutput['NOW',oneten] <- as.numeric(InputBancware['NOW',oneten])
              IncomeStatementOutput['MMDA',oneten] <- as.numeric(InputBancware['MMDA',oneten])
              IncomeStatementOutput['Savings',oneten] <- as.numeric(InputBancware['SAV',oneten])
              IncomeStatementOutput['CDs',oneten] <- as.numeric(InputBancware['CDs',oneten])
              
              IncomeStatementOutput['Deposits',1] <- as.numeric(InputBudgetIS['Interest Expense - Deposits',11])
              
              b <- as.numeric(IncomeStatementOutput['DDA',two])
              c <- as.numeric(IncomeStatementOutput['NOW',two])
              d <- as.numeric(IncomeStatementOutput['MMDA',two])
              e <- as.numeric(IncomeStatementOutput['Savings',two])
              f <- as.numeric(IncomeStatementOutput['CDs',two])
              g <- as.numeric(BalanceSheetOutput['Deposit Adjustments',two])
              
              m <- as.numeric(InputBancware['DDA',two])
              n <- as.numeric(InputBancware['NOW',two])
              o <- as.numeric(InputBancware['MMDA',two])
              p <- as.numeric(InputBancware['SAV',two])
              q <- as.numeric(InputBancware['CDs',two])
              
              h <- as.numeric(InputEconometricModel['DDA',one])
              i <- as.numeric(InputEconometricModel['NOW',one])
              j <- as.numeric(InputEconometricModel['MMDA',one])
              k <- as.numeric(InputEconometricModel['SAV',one])
              l <- as.numeric(InputEconometricModel['CDs',one])
              
              r <- as.numeric(InputEconometricModel['DDA',two])
              s <- as.numeric(InputEconometricModel['NOW',two])
              t <- as.numeric(InputEconometricModel['MMDA',two])
              u <- as.numeric(InputEconometricModel['SAV',two])
              v <- as.numeric(InputEconometricModel['CDs',two])
              
              IncomeStatementOutput['Deposits',two] <- (b+c+d+e+f)+g*((m+n+o+p+q)/mean(h+i+j+k+l+r+s+t+u+v))
              
              remove(b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v)
              
              
              IncomeStatementOutput['Short Term Borrowings',1] <- as.numeric(InputBudgetIS['Interest Expense - Borrowings',11])
              
              k <- as.numeric(BalanceSheetOutput['Short Term Borrowings',1:9])
              l <- as.numeric(BalanceSheetOutput['Short Term Borrowings',2:10])
              m <- as.numeric(InputBancware['Projected Short Term Debt Yield %',2:10])
              
              IncomeStatementOutput['Short Term Borrowings',2:10] <- ((k+l)/2)*m
              
              remove(k,l,m)
              
              IncomeStatementOutput['ST Borrow to Meet Min Cash Levels',1]<-0
              k <- as.numeric(BalanceSheetOutput['ST Borrowings to Meet Minimum Cash Levels',2:10])
              l <- as.numeric(InputBancware['Projected Short Term Debt Yield %',2:10])
              
              IncomeStatementOutput['ST Borrow to Meet Min Cash Levels',2:10]<-k*l
              
              remove(k,l)
              
              k <- as.numeric(InputBudgetIS['Long Term Borrowings',1])
              k[is.na(k)] <- 0
              IncomeStatementOutput['Long Term Borrow',1]<- k
              
              k <- as.numeric(BalanceSheetOutput['Long Term Borrowings',1:9])
              l <- as.numeric(BalanceSheetOutput['Long Term Borrowings',2:10])
              m <- as.numeric(InputBancware['Projected Long Term Debt Yield %',2:10])
              
              IncomeStatementOutput['Long Term Borrow',2:10] <- ((k+l)/2)*m
              
              remove(k,l,m)
              
              k <- as.numeric(IncomeStatementOutput['Deposits',oneten])
              l <- as.numeric(IncomeStatementOutput['Long Term Borrow',oneten])
              m <- as.numeric(IncomeStatementOutput['Short Term Borrowings',oneten])
              n <- as.numeric(IncomeStatementOutput['ST Borrow to Meet Min Cash Levels',oneten])
              
              IncomeStatementOutput['Total Interest Expense',oneten] <- k+l+m+n
              remove(k,l,m,n)
              
              k <- as.numeric(IncomeStatementOutput['Total Interest Income',oneten])
              l <- as.numeric(IncomeStatementOutput['Total Interest Expense',oneten])
              
              IncomeStatementOutput['Net Interest Income',oneten]<- k-l
              
              remove(k,l)
              
              # IS - Non-Interest Income -----------------------------------------------------
              
              IncomeStatementOutput['MBO Income',1] <- as.numeric(InputBudgetIS['Mortgage Banking, Net',11])
              IncomeStatementOutput['MBO Income',2:10] <- as.numeric(InputMortgageBanking['Total',2:10])
              
              k <- as.numeric(InputBudgetIS['Service Charges',11])
              l <- as.numeric(InputBudgetIS['NSF / OD Charges',11])
              
              IncomeStatementOutput['Deposit Service Charges',1]<-k+l
              remove(k,l)
              
              k <- as.numeric(InputValueDriverAssumpt['Deposit Serv Charges (% DDA & NOW Dep)',2:10])
              l <- as.numeric(InputEconometricModel['DDA',2:10])
              m <- as.numeric(InputEconometricModel['NOW',2:10])
              
              IncomeStatementOutput['Deposit Service Charges',2:10]<-k*(l+m)
              
              remove(k,l,m)
              
              IncomeStatementOutput['Bank Card & Other Fees',1] <- as.numeric(InputBudgetIS['Bank Card and Other Fees',11])
              IncomeStatementOutput['Bank Card & Other Fees',2:10]<- as.numeric(InputEconometricModel['Bank card and Other Fees',2:10])
              
              IncomeStatementOutput['AUM Fees',1] <- as.numeric(InputBudgetIS['Wealth Management',11])
              IncomeStatementOutput['AUM Fees',2:10]<- as.numeric(InputEconometricModel['Wealth Management',2:10])
              
              k<- as.numeric(InputBudgetIS['Gain or Loss on Sale of Investments',eleven])
              k[is.na(k)] <- 0
              IncomeStatementOutput['Gain or Loss on Sale of Invest',oneten] <-k
              
              remove(k)
              
              IncomeStatementOutput['Insurance Commissions',1]<- as.numeric(InputBudgetIS['Insurance Commissions',11]) 
              IncomeStatementOutput['Insurance Commissions',2:10]<- as.numeric(InputEconometricModel['Insurance Commissions',2:10])
              
              k <- as.numeric(InputBudgetIS['Other Income, Net',11])
              l <- as.numeric(InputBudgetIS['Security Gains, Net',11])
              IncomeStatementOutput['Other Non-interest Income',1]<- k+l
              remove(k,l)
              
              k <- as.numeric(InputBudgetIS['Other Income, Net',12:20])
              l <- as.numeric(InputBudgetIS['Security Gains, Net',12:20])
              m <- as.numeric(InputValueDriverAssumpt['Other Non-Int inc Stress Factor (Reduct in Fees)',2:10])
              
              IncomeStatementOutput['Other Non-interest Income',2:10] <- (k+l)*(1-m)
              remove(k,l,m)
              
              k <- as.numeric(IncomeStatementOutput['MBO Income',oneten])
              l <- as.numeric(IncomeStatementOutput['Deposit Service Charges',oneten])
              m <- as.numeric(IncomeStatementOutput['Bank Card & Other Fees',oneten])
              n <- as.numeric(IncomeStatementOutput['AUM Fees',oneten])
              o <- as.numeric(IncomeStatementOutput['Gain or Loss on Sale of Invest',oneten])
              p <- as.numeric(IncomeStatementOutput['Insurance Commissions',oneten])
              q <- as.numeric(IncomeStatementOutput['Other Non-interest Income',oneten])
              
              IncomeStatementOutput['Total Non-Interest Income',oneten] <- k+l+m+n+o+p+q
              
              remove(k,l,m,n,o,p,q)
              
              
              # IS - Non-Interest Expense ----------------------------------------------------
              
              IncomeStatementOutput['Salaries & Benefits',oneten]<- as.numeric(InputBudgetIS['Salaries And Benefits',eleven]) 
              IncomeStatementOutput['Commissions',1]<- as.numeric(InputBudgetIS['Commissions',11]) 
              
              k<- as.numeric(InputBudgetIS['Commissions',12:20])
              l<- as.numeric(InputValueDriverAssumpt['Total Orig Vol Strs Factor (Reduct in Exps)',2:10])
              
              IncomeStatementOutput['Commissions',2:10]<-k*(1-l) 
              remove(k,l)
              
              ## IS - OREO from BS &  OREO Writedown Expense from ---------------------
              
              IncomeStatementOutput['OREO Writedown Expense',1]<- as.numeric(InputBudgetIS['ORE Writedown Expense',11]) 
              
              k<- as.numeric(InputBudgetBS['Other Real Estate Owned - Excl Cov',11])
              l<- as.numeric(InputBudgetBS['Other Real Estate Owned - Covered',11])
              
              BalanceSheetOutput['Other Real Estate Owned',1]<-k+l
              
              remove(k,l)
              
              k<- as.numeric(InputValueDriverAssumpt['OREO Writedn Exp (% of Total OREO)',two])
              l<- as.numeric(BalanceSheetOutput['Other Real Estate Owned',one])
              IncomeStatementOutput['OREO Writedown Expense',two]<-k*l
              remove(k,l)
              
              m<- as.numeric(InputALLL['Total End OREO Bal',two])
              n<- as.numeric(IncomeStatementOutput['OREO Writedown Expense',two])
              
              BalanceSheetOutput['Other Real Estate Owned',two]<- m-n
              
              remove(m)
              
              
              ## IS - OREO Foreclosure Expense ----------
              
              IncomeStatementOutput['OREO Foreclosure Expense',1]<- as.numeric(InputBudgetIS['ORE Other Foreclosure Expense',11]) 
              k<- as.numeric(InputValueDriverAssumpt['OREO Foreclos Exp (% of Total OREO)',2:10])
              l<- as.numeric(BalanceSheetOutput['Other Real Estate Owned',2:10])
              m<- as.numeric(IncomeStatementOutput['OREO Writedown Expense',2:10])
              IncomeStatementOutput['OREO Foreclosure Expense',2:10]<-k*(l+m)
              remove(k,l,m)
              IncomeStatementOutput['Intangible Asset Amortization',oneten]<- as.numeric(InputBudgetIS['Amortization of Intangibles',eleven]) 
              
              ##  BS - Goodwill & Intangibles ------------------
              
              k<- as.numeric(InputBudgetBS['Goodwill',11])
              l<- as.numeric(InputBudgetBS['Identifiable Intangible Assets',11])
              
              BalanceSheetOutput['Goodwill and Intangibles',1]<-k+l
              
              remove(k,l)
              
              k<- as.numeric(BalanceSheetOutput['Goodwill and Intangibles',one])
              l<- as.numeric(IncomeStatementOutput['Intangible Asset Amortization',two])
              
              BalanceSheetOutput['Goodwill and Intangibles',two]<- k-l
              
              
              remove(k,l)
              
              ## IS - FDIC Expense ----------
              
              IncomeStatementOutput['FDIC Expense',1]<- as.numeric(InputBudgetIS['FDIC Expense',11]) 
              
              k<- as.numeric(InputValueDriverAssumpt['Dep Insur Exp (% of Avg Assets - Avg Tang Equit)',two])
              l<- as.numeric(BalanceSheetOutput['Goodwill and Intangibles',one])
              m<- as.numeric(BalanceSheetOutput['Goodwill and Intangibles',two])
              n<- as.numeric(BalanceSheetOutput['Total Deposits',one])
              o<- as.numeric(BalanceSheetOutput['Total Deposits',two])
              p<- as.numeric(BalanceSheetOutput['Short Term Borrowings',one])
              q<- as.numeric(BalanceSheetOutput['Short Term Borrowings',two])
              r<- as.numeric(BalanceSheetOutput['ST Borrowings to Meet Minimum Cash Levels',one])
              s<- as.numeric(BalanceSheetOutput['Other Liabilities',one])
              t<- as.numeric(BalanceSheetOutput['Other Liabilities',two])
              
              IncomeStatementOutput['FDIC Expense',two]<- k*mean((l+n+p+r+t),(m+o+q+s+t))
              
              # This number is off because Goodwill needs to be calc'd
              remove(k,l,m,n,o,p,q,r,s,t)
              
              IncomeStatementOutput['Loan Expense',1]<- as.numeric(InputBudgetIS['Loan Expense',11]) 
              
              k<- as.numeric(InputBudgetIS['Loan Expense',12:20])
              l<- as.numeric(InputValueDriverAssumpt['Total Orig Vol Strs Factor (Reduct in Exps)',2:10])
              
              
              IncomeStatementOutput['Loan Expense',2:10]<- k*(1-l)
              
              
              k<- as.numeric(InputBudgetIS['Office & Equipment Expense',11])
              l<- as.numeric(InputBudgetIS['Data Processing Expense',11])
              m<- as.numeric(InputBudgetIS['Other Services & Fees',11])
              n<- as.numeric(InputBudgetIS['Communication Expense',11])
              o<- as.numeric(InputBudgetIS['Other Expense (excluding operational losses)',11])
              
              IncomeStatementOutput['Other Expense',1]<-(k+l+m+n+o)
              remove(k,l,m,n,o)
              
              k<- as.numeric(InputBudgetIS['Office & Equipment Expense',12:20])
              l<- as.numeric(InputBudgetIS['Data Processing Expense',12:20])
              m<- as.numeric(InputBudgetIS['Other Services & Fees',12:20])
              n<- as.numeric(InputBudgetIS['Communication Expense',12:20])
              o<- as.numeric(InputBudgetIS['Other Expense (excluding operational losses)',12:20])
              p<- as.numeric(InputBudgetIS['Issuance Costs ($ Actual)',2:10])
              
              IncomeStatementOutput['Other Expense',2:10]<-(k+l+m+n+o)
              remove(k,l,m,n,o,p)
              
              k<- as.numeric(IncomeStatementOutput['Salaries & Benefits',oneten])
              l<- as.numeric(IncomeStatementOutput['Commissions',oneten])
              m<- as.numeric(IncomeStatementOutput['OREO Writedown Expense',oneten])
              n<- as.numeric(IncomeStatementOutput['OREO Foreclosure Expense',oneten])
              o<- as.numeric(IncomeStatementOutput['Intangible Asset Amortization',oneten])
              p<- as.numeric(IncomeStatementOutput['FDIC Expense',oneten])
              q<- as.numeric(IncomeStatementOutput['Loan Expense',oneten])
              r<- as.numeric(IncomeStatementOutput['Other Expense',oneten])
              
              IncomeStatementOutput['Total Non-Interest Expense',oneten]<- (k+l+m+n+o+p+q+r)
              remove(k,l,m,n,o,p,q,r)
              
              ## IS - Pre-Provision Net Revenue ------------ 
              
              k<- as.numeric(IncomeStatementOutput['Net Interest Income',oneten])
              l<- as.numeric(IncomeStatementOutput['Total Non-Interest Income',oneten])
              m<- as.numeric(IncomeStatementOutput['Total Non-Interest Expense',oneten])
              
              
              IncomeStatementOutput['Pre-Provision Net Revenue',oneten]<- k+l-m
              
              remove(k,l,m)
              
              # IS - Net Income --------------------------------------------------------------
              
              IncomeStatementOutput['Ops Risk',1]<- as.numeric(InputBudgetIS['Operational Losses',11])
              
              k<- as.numeric(InputBudgetIS['Operational Losses',12:20])
              l<- as.numeric(InputValueDriverAssumpt['Addtl Ops Exp',2:10])
              
              IncomeStatementOutput['Ops Risk',2:10]<- k+l
              remove(k,l)
              
              
              k<- as.numeric(InputBudgetIS['Other Than Temporary Impairment (OTTI)',11])
              k[is.na(k)] <- as.numeric(0)
              
              IncomeStatementOutput['Other Than Temporary Impairment (OTTI)',1]<- k
              remove(k)
              
              k<- as.numeric(InputValueDriverAssumpt['Addtl OTTI ($ Actual)',2:10])
              IncomeStatementOutput['Other Than Temporary Impairment (OTTI)',2:10]<- k
              remove(k)
              
              
              IncomeStatementOutput['Preferred Dividend',1]<- as.numeric(InputBudgetIS['Preferred Dividend',11])
              
              k<- as.numeric(InputValueDriverAssumpt['Pref Stock Div ($ Actual)',2:10])
              
              IncomeStatementOutput['Preferred Dividend',2:10]<- k
              
              remove(k)
              
              k<- as.numeric(InputBudgetIS['Common Dividends Per Share (Not in 000s)',11])
              
              IncomeStatementOutput['Common Stock Dividend',1]<- k
              
              remove(k)
              
              k<- as.numeric(InputValueDriverAssumpt['Comm Stock Div ($ per share)',2:10])
              
              IncomeStatementOutput['Common Stock Dividend',2:10]<- k
              
              remove(k)
              
              k<- as.numeric(InputBudgetIS['Common Shares Outstanding (Not in 000s)',11])
              
              IncomeStatementOutput['Common Shares Outstanding',1]<- k/1000
              
              remove(k)
              
              k<- as.numeric(InputValueDriverAssumpt['Comm Stock Outstand (Actual)',2:10])
              
              IncomeStatementOutput['Common Shares Outstanding',2:10]<- k
              
              remove(k)
              
              
              m<- as.numeric(IncomeStatementOutput['Common Stock Dividend',oneten]) 
              n<- as.numeric(IncomeStatementOutput['Common Shares Outstanding',oneten])
              
              IncomeStatementOutput['Total Com Stock DivParent Upstream Div',oneten]<- m*n
              
              
              remove(m,n)
              
              ## BS - Retained Earnings --------------------
              
              o<- as.numeric(InputBudgetBS['Retained Earnings',11])
              o[is.na(o)] <- 0
              BalanceSheetOutput['Retained Earnings',1]<-o
              
              k<- as.numeric(IncomeStatementOutput['Net Income',two])
              l<- as.numeric(IncomeStatementOutput['Preferred Dividend',two])
              m<- as.numeric(IncomeStatementOutput['Total Com Stock DivParent Upstream Div',two])
              
              BalanceSheetOutput['Retained Earnings',two]<-as.numeric(BalanceSheetOutput['Retained Earnings',one])+k-l-m
              
              remove(k,l,m)
              
              
              ## BS - Other Equity Adjustments -----------
              
              j<- as.numeric(BalanceSheetOutput['Total Assets',1])
              k<- as.numeric(BalanceSheetOutput['Total Liabilities',1])
              l<- as.numeric(BalanceSheetOutput['Common Stock',1])
              m<- as.numeric(BalanceSheetOutput['Treasury Stock',1])
              n<- as.numeric(BalanceSheetOutput['Preferred Stock',1])
              o<- as.numeric(BalanceSheetOutput['Retained Earnings',1])
              p<- as.numeric(BalanceSheetOutput['AOCI',1])
              BalanceSheetOutput['Other Equity Adjustments',1]<- (j-k-l-m-n-o-p)
              BalanceSheetOutput['Other Equity Adjustments',2:10]<-0
              
              ## BS - Total Shareholder's Equity ---------
              
              l<- as.numeric(BalanceSheetOutput['Common Stock',oneten])
              m<- as.numeric(BalanceSheetOutput['Treasury Stock',oneten])
              n<- as.numeric(BalanceSheetOutput['Preferred Stock',oneten])
              o<- as.numeric(BalanceSheetOutput['Retained Earnings',oneten])
              p<- as.numeric(BalanceSheetOutput['AOCI',oneten])
              q<- as.numeric(BalanceSheetOutput['Other Equity Adjustments',oneten])
              
              BalanceSheetOutput['Total Shareholders Equity',oneten]<- (l+m+n+o+p+q)
              
              remove(l,m,n,o,p,q)
              
              m<- as.numeric(BalanceSheetOutput['Total Shareholders Equity',oneten])
              n<- as.numeric(BalanceSheetOutput['Total Liabilities',oneten])
              
              BalanceSheetOutput['Total Liabilities and Shareholders Equity',oneten] <-(m+n)
              
              remove(m,n)
              
              
              # End - Income Statement ----------------------------------------------------
              
              
              # Begin - Supplemental Items ------------------------------------------------
              
              OutputSuppItems['Net Charge-offs',1]<- as.numeric(InputBudgetIS['Net (Charge-offs) Recov Exclud Acq Loans',11]) 
              
              OutputSuppItems['Net Charge-offs',2:10]<- as.numeric(InputALLL['Net Charge-Offs',2:10]) 
              
              ## IS - ALLL & Loan Loss Provision(Inc Qualitative & Quantitative)(Interweaved) ------------------
              
              k<- as.numeric(InputALLL['Loan Loss Provision',2:10])
              
              IncomeStatementOutput['Quantitative Loan Loss Provision',2:10]<- -k
              remove(k)
              
              IncomeStatementOutput['Loan Loss Prov (To Arrive at Min ALLL Adj)',1]<- 0
              
              k<- as.numeric(InputBudgetIS['Provision for Loan Losses, LHFI',11])
              l<- as.numeric(InputBudgetIS['Provision for Loan Losses, acquired loans',11])
              
              IncomeStatementOutput['Quantitative Loan Loss Provision',1]<- k+l
              
              remove(k,l)
              
              IncomeStatementOutput['Qualitative Loan Loss Provision',1]<- 0
              
              k<- as.numeric(InputBudgetIS['Provision for Loan Losses, LHFI',11])
              l<- as.numeric(InputBudgetIS['Provision for Loan Losses, acquired loans',11])
              
              IncomeStatementOutput['Loan Loss Provision',1]<- k+l
              
              remove(k,l)
              
              l<-as.numeric(InputBudgetBS['Allowance for Loan & Lease Losses - LHFI',11])
              m<-as.numeric(InputBudgetBS['Allowance for Loan & Lease Losses - Acq',11])
              
              BalanceSheetOutput['Allowance for Loan and Lease Losses',1] <-l+m
              
              remove(l,m)
              
              k<- as.numeric(InputValueDriverAssumpt['Min ALLL % of Loans',two])
              l<- as.numeric(BalanceSheetOutput['Total Loans', two])
              m<- as.numeric(BalanceSheetOutput['Allowance for Loan and Lease Losses',one])
              n<- as.numeric(OutputSuppItems['Net Charge-offs',two])
              
              IncomeStatementOutput['Loan Loss Prov (To Arrive at Min ALLL Adj)',two]<- -(-k*l)+m
              
              remove(k,l,m)
              
              k<- as.numeric(IncomeStatementOutput['Quantitative Loan Loss Provision',two]) 
              l<- as.numeric(IncomeStatementOutput['Loan Loss Prov (To Arrive at Min ALLL Adj)',two])
              m<- (l-k)
              
              IncomeStatementOutput['Qualitative Loan Loss Provision',two]<- max(0,m)
              remove(k,l,m)
              
              k<- as.numeric(IncomeStatementOutput['Quantitative Loan Loss Provision',two])
              l<- as.numeric(IncomeStatementOutput['Qualitative Loan Loss Provision',two])
              
              IncomeStatementOutput['Loan Loss Provision',two]<- k+l
              remove(k,l)
              
              k<- as.numeric(BalanceSheetOutput['Allowance for Loan and Lease Losses',one])
              l<- as.numeric(IncomeStatementOutput['Loan Loss Provision',two])
              m<- as.numeric(OutputSuppItems['Net Charge-offs',two])
              
              BalanceSheetOutput['Allowance for Loan and Lease Losses',two]<- k-l+m
              
              ## BS - Net Loans----------------------------------
              
              k<- as.numeric(BalanceSheetOutput['Allowance for Loan and Lease Losses',oneten])
              l<- as.numeric(BalanceSheetOutput['Total Loans',oneten])
              BalanceSheetOutput['Net Loans',oneten]<-k+l
              
              remove(k,l)
              
              k<- as.numeric(BalanceSheetOutput['Total Investments',oneten]) 
              l<- as.numeric(BalanceSheetOutput['Net Loans',oneten])
              m<- as.numeric(BalanceSheetOutput['FDIC LSA Receivable',oneten]) 
              m[is.na(m)] <- as.numeric(0)
              n<- as.numeric(BalanceSheetOutput['Mortgage Servicing Rights',oneten])
              
              OutputSuppItems['Earning Assets',oneten]<- (k+l+m+n)
              remove(k,l,m,n)
              
              
              ## IS - Pre-Tax Income ----------------
              
              k<- as.numeric(IncomeStatementOutput['Pre-Provision Net Revenue',oneten])
              l<- as.numeric(IncomeStatementOutput['Loan Loss Provision',oneten])
              m<- as.numeric(IncomeStatementOutput['Ops Risk',oneten])
              n<- as.numeric(IncomeStatementOutput['Other Than Temporary Impairment (OTTI)',oneten])
              
              IncomeStatementOutput['Pre-Tax Income',oneten]<- k-l-m-n
              
              remove(k,l,m,n)
              
              k<- as.numeric(InputBudgetBS['Equity Securities',eleven]) 
              k[is.na(k)] <- as.numeric(0)
              
              OutputSuppItems['Equity Securities',oneten]<- k
              remove(k)
              
              k<- as.numeric(InputBudgetBS['Equity Investin Unconsolidated Financial Inst',eleven]) 
              k[is.na(k)] <- as.numeric(0)
              
              OutputSuppItems['Equity Invest in Unconsol Fin Inst',oneten]<- k
              remove(k)
              
              
              k<- InputRegCapital['QualifySubordinated Debt',6]
              
              OutputSuppItems['Qualified Subordinated Debt',1]<-k
              
              remove(k)
              
              k<- as.numeric(OutputSuppItems['Qualified Subordinated Debt',one])
              l<- as.numeric(InputCapitalPlan['Qualified Subordinated Debt Issued ($ Actual)',two])
              m<- as.numeric(InputCapitalPlan['Qualified Subordinated Debt Maturing ($ Actual)',two])
              
              OutputSuppItems['Qualified Subordinated Debt',two]<- k+(l-m)
              
              remove(k,l,m)
              
              k<- as.numeric(InputValueDriverAssumpt['Home Equity Loans Not Sub 50% RW',oneten])
              l<- as.numeric(BalanceSheetOutput['Residential',oneten])
              
              OutputSuppItems['Residential Loans with 50% RW',oneten]<- k*l
              remove(k,l)
              
              k<- as.numeric(InputValueDriverAssumpt['High Volatility CRE ($)',oneten])
              
              OutputSuppItems['High Vola Commercial RE Loans (Uncov)',oneten]<- k
              
              remove(k)
              
              l<- as.numeric(InputBudgetBS['Loans Held for Sale',eleven])
              
              OutputSuppItems['Loans Held For Sale',oneten]<- l
              
              remove(l)
              
              k<- as.numeric(InputValueDriverAssumpt['Home Equity Loans Not Sub 50% RW',oneten])
              l<- as.numeric(BalanceSheetOutput['Residential',oneten])
              
              OutputSuppItems['CRE Loans (Multifamily)',oneten]<- k*l
              
              remove(k,l)
              
              k<- c(0,0,0,0,0,0,0,0,0,0)
              OutputSuppItems['Covered Loans C&I',oneten]<- k
              OutputSuppItems['Covered Loans CRE',oneten]<- k
              OutputSuppItems['Covered Loans Construc',oneten]<- k
              OutputSuppItems['Covered Loans Resi',oneten]<- k
              OutputSuppItems['Covered Loans Consumer',oneten]<- k
              OutputSuppItems['Covered Loans Other',oneten]<- k
              OutputSuppItems['Covered Loans S&P Subdivisions',oneten]<- k
              
              remove(k)
              
              k<- as.numeric(InputValueDriverAssumpt['High Volatility CRE ($)',oneten])
              
              OutputSuppItems['0% Risk Weighted Loans',oneten]<- k
              
              remove(k)
              
              k<- as.numeric(InputValueDriverAssumpt['20% RW Loans',oneten])
              l<- as.numeric(BalanceSheetOutput['Total Loans',oneten])
              m<- as.numeric(OutputSuppItems['Covered Loans C&I',oneten])
              n<- as.numeric(OutputSuppItems['Covered Loans CRE',oneten])
              o<- as.numeric(OutputSuppItems['Covered Loans Construc',oneten])
              p<- as.numeric(OutputSuppItems['Covered Loans Resi',oneten])
              q<- as.numeric(OutputSuppItems['Covered Loans Consumer',oneten])
              r<- as.numeric(OutputSuppItems['Covered Loans Other',oneten])
              s<- as.numeric(OutputSuppItems['Covered Loans S&P Subdivisions',oneten])
              
              OutputSuppItems['20% Risk Weighted Loans',oneten]<-round(k*(l-(m+n+o+p+q+r+s)),2)
              
              remove(k,l,m,n,o,p,q,r,s)
              
              k<- as.numeric(InputALLL['Resi Loans (90+ DPD %)',oneten])
              l<- as.numeric(BalanceSheetOutput['Residential',oneten])
              m<- as.numeric(OutputSuppItems['Residential Loans with 50% RW',oneten])
              n<- as.numeric(OutputSuppItems['Covered Loans Resi',oneten])
              
              OutputSuppItems['1st Lien Resi 90+ DPD % of Total Resi (Uncov)',oneten]<- k*(l-m-n)
              
              remove(k,l,m,n)
              
              k<- as.numeric(OutputSuppItems['Residential Loans with 50% RW',oneten])
              l<- as.numeric(InputALLL['Home Equity(90+ DPD %)',oneten])
              
              OutputSuppItems['2nd Lien Resi 90+ DPD % of Total Resi (Uncov)',oneten]<- k*l
              
              remove(k,l)
              
              k<- as.numeric(InputALLL['C&I Loans (90+ DPD %)',oneten])
              l<- as.numeric(BalanceSheetOutput['C&I',oneten])
              m<- as.numeric(OutputSuppItems['Covered Loans C&I',oneten])
              
              OutputSuppItems['C&I Loans - 90+ DPD % of Total C&I (Uncov)',oneten]<- round(k*(l-m),2)
              
              remove(k,l,m)
              
              k<- as.numeric(InputALLL['CRE Loans (90+ DPD %)',oneten])
              l<- as.numeric(BalanceSheetOutput['CRE',oneten])
              m<- as.numeric(OutputSuppItems['Covered Loans CRE',oneten])
              
              OutputSuppItems['CRE Loans - 90+ DPD % of Total CRE (Uncov)',oneten]<- round(k*(l-m),2)
              
              remove(k,l,m)
              
              k<- as.numeric(InputALLL['Constr Loans (90+ DPD %)',oneten])
              l<- as.numeric(BalanceSheetOutput['Construction',oneten])
              m<- as.numeric(OutputSuppItems['Covered Loans Construc',oneten])
              
              OutputSuppItems['Construc - 90+ DPD % of Total Construc (Uncov)',oneten]<- round(k*(l-m),2)
              
              remove(k,l,m)
              
              k<- as.numeric(InputALLL['Cons Loans (90+ DPD %)',oneten])
              l<- as.numeric(BalanceSheetOutput['Consumer',oneten])
              m<- as.numeric(OutputSuppItems['Covered Loans Consumer',oneten])
              
              OutputSuppItems['Consumer - 90+ DPD % of Total Consumer (Uncov)',oneten]<- round(k*(l-m),2)
              
              remove(k,l,m)
              
              k<- as.numeric(InputALLL['Other Loans (90+ DPD %)',oneten])
              l<- as.numeric(BalanceSheetOutput['S&P Subdivisions',oneten])
              m<- as.numeric(OutputSuppItems['Covered Loans Other',oneten])
              
              OutputSuppItems['Other- 90+ DPD % of Total Other (Uncov)',oneten]<- round(k*(l-m),2)
              
              remove(k,l,m)
              
              # End - Supplemental Items --------------------------------------------------
              
              # Begin - Basel1 Capital ----------------------
              
              k <- as.numeric(BalanceSheetOutput['Common Stock',oneten]) 
              l <- as.numeric(BalanceSheetOutput['Treasury Stock',oneten]) 
              m <- as.numeric(BalanceSheetOutput['Retained Earnings',oneten]) 
              n <- as.numeric(BalanceSheetOutput['Other Equity Adjustments',oneten]) 
              
              OutputCapBasel1['Common Stock, Retained Earnings, and Other Equity',oneten] <- k+l+m+n
              
              remove(k,l,m,n)
              
              OutputCapBasel1['Pl AOCI (if opt in is elected)',oneten] <- c(0,0,0,0,0,0,0,0,0,0)
              # Per "Opt out of OCI" on model assumptions page
              
              
              k <- as.numeric(InputRegCapital['LESSUnrlzd Loss AFS Eq Secs',6]) 
              OutputCapBasel1['Ls Unreal gains on AFS equity securities',1] <- k
              OutputCapBasel1['Ls Unreal gains on AFS equity securities',2:10] <- c(0,0,0,0,0,0,0,0,0)
              
              OutputCapBasel1['Ls Non-Qualif perpetual preferred stock',oneten] <- c(0,0,0,0,0,0,0,0,0,0)
              
              OutputCapBasel1['Ls Gains (losses) on cash flow hedges',oneten] <- c(0,0,0,0,0,0,0,0,0,0)
              
              k <- as.numeric(BalanceSheetOutput['Goodwill and Intangibles',oneten]) 
              l <- as.numeric(InputValueDriverAssumpt['Diff in Goodwill for Reg Cap (Actual)',oneten])
              
              OutputCapBasel1['Ls Goodwill & Other Intangibles',oneten] <- k-l
              
              k <- as.numeric(BalanceSheetOutput['Mortgage Servicing Rights',oneten]) 
              l <- as.numeric(InputALLL['Disallow serv assets & PCCR (% of Other Assets)',oneten])
              
              OutputCapBasel1['Ls Disallow servicing assets and PCCR',oneten] <- k*l
              
              remove(k,l)
              
              k <- as.numeric(BalanceSheetOutput['Preferred Stock',oneten]) 
              
              OutputCapBasel1['Pl Addt Tier 1 capital instruments',oneten] <- k
              
              OutputCapBasel1['Pl Tier 1 Minority Interest not included in CET1',oneten] <- c(0,0,0,0,0,0,0,0,0,0)
              
              OutputCapBasel1['Pl Subordinated Capital Addition / (Deduction)',oneten] <- c(0,0,0,0,0,0,0,0,0,0)
              
              OutputCapBasel1['Ls Other deductions (additions) to Tier 1',oneten] <- c(0,0,0,0,0,0,0,0,0,0)
              
              
              
              k <- as.numeric(OutputSuppItems['Qualified Subordinated Debt',oneten])
              
              OutputCapBasel1['Pl Qual subord debt & Redeem Pref Stock',oneten] <- k
              
              OutputCapBasel1['Pl Total Minority Interest not included in Tier 1',oneten] <- c(0,0,0,0,0,0,0,0,0,0)
              
              
              k <- as.numeric(BalanceSheetOutput['Allowance for Loan and Lease Losses',two])
              l <- as.numeric(BalanceSheetOutput['Allowance for Loan and Lease Losses',1])
              m <- as.numeric(InputRegCapital['Allw for Ln Losses Incl Tier 2',6])
              n <- as.numeric(OutputRWABasel1['Total Risk Weighted Assets',two])
              o <- -k+(l+m)
              p <- 0.0125*n
              q <- min(o,p)
              
              OutputCapBasel1['Pl Allowance for Loan & Lease Losses',two] <- q
              
              
              
              OutputCapBasel1['Pl Unreal gains on AFS equity securities',oneten] <- c(0,0,0,0,0,0,0,0,0,0)
              
              
              
              k <- as.numeric(InputValueDriverAssumpt['Deducts from Total Cap (Actual)',2:10])
              
              OutputCapBasel1['Ls Other deductions (additions) to Tier 2',1] <- 0
              OutputCapBasel1['Ls Other deductions (additions) to Tier 2',2:10] <- k
              
              
              
              
              k <- as.numeric(InputBudgetBS['Total Assets',10]) 
              l <- as.numeric(BalanceSheetOutput['Total Assets',1])
              m <- (k:l)
              n <- mean(m)
              
              OutputCapBasel1['Average Total Assets',1] <- n
              
              
              k <- as.numeric(BalanceSheetOutput['Total Assets',one])
              l <- as.numeric(BalanceSheetOutput['Total Assets',two])
              m <- (k:l)
              n <- mean(m)
              
              OutputCapBasel1['Average Total Assets',two] <- n
              
              
              k <- as.numeric(OutputCapBasel1['Ls Goodwill & Other Intangibles',oneten])
              OutputCapBasel1['Ls Goodwill & Other Intangibles2',oneten] <- k
              
              k <- as.numeric(OutputCapBasel1['Ls Disallow servicing assets and PCCR',oneten])
              OutputCapBasel1['Ls Disallow servicing assets and PCCR2',oneten] <- k
              
              
              
              k <- as.numeric(InputValueDriverAssumpt['Deducts from Leverage Cap (Actual)',oneten])
              
              OutputCapBasel1['Ls Other deductions from leverage capital',1] <- 0
              OutputCapBasel1['Ls Other deductions from leverage capital',oneten] <- k
              
              # End - Base 1 Capital ---------------------------------
              
              # Begin - Deffered Tax Asset Calculations ----------
              
              ##  BS - Deferred Tax Asset (CHECK THIS)---------------
              BalanceSheetOutput['Deferred Tax Asset',1]<-InputBudgetBS['Deferred Tax Asset',11]
              
              if (OutputDTA['End Def Tax Asset (Liab)',one]>0) {
                l<-as.numeric(OutputDTA['End Def Tax Asset (Liab)',one])
              } else {
                l<-0
                BalanceSheetOutput['Deferred Tax Asset',two]<-l
              }
              
              BalanceSheetOutput['ST Borrowings to Meet Minimum Cash Levels',1]<- 0
              BalanceSheetOutput['ST Borrowings to Meet Minimum Cash Levels',2]<-BalanceSheetOutput['ST Borrowings to Meet Minimum Cash Levels',1]
              BalanceSheetOutput['ST Borrowings to Meet Minimum Cash Levels',3]<-BalanceSheetOutput['ST Borrowings to Meet Minimum Cash Levels',2]
              BalanceSheetOutput['ST Borrowings to Meet Minimum Cash Levels',4]<-BalanceSheetOutput['ST Borrowings to Meet Minimum Cash Levels',3]
              BalanceSheetOutput['ST Borrowings to Meet Minimum Cash Levels',5]<-BalanceSheetOutput['ST Borrowings to Meet Minimum Cash Levels',4]
              BalanceSheetOutput['ST Borrowings to Meet Minimum Cash Levels',6]<-BalanceSheetOutput['ST Borrowings to Meet Minimum Cash Levels',5]
              BalanceSheetOutput['ST Borrowings to Meet Minimum Cash Levels',7]<-BalanceSheetOutput['ST Borrowings to Meet Minimum Cash Levels',6]
              BalanceSheetOutput['ST Borrowings to Meet Minimum Cash Levels',8]<-BalanceSheetOutput['ST Borrowings to Meet Minimum Cash Levels',7]
              BalanceSheetOutput['ST Borrowings to Meet Minimum Cash Levels',9]<-BalanceSheetOutput['ST Borrowings to Meet Minimum Cash Levels',8]
              BalanceSheetOutput['ST Borrowings to Meet Minimum Cash Levels',10]<-BalanceSheetOutput['ST Borrowings to Meet Minimum Cash Levels',9]
              
              ## BS - Deferred Tax Liability ----------------
              
              o<-InputBudgetBS['Deferred Tax Liability',11]
              o[is.na(o)] <- 0
              BalanceSheetOutput['Deferred Tax Liability',1]<-o
              remove(o)
              
              if (OutputDTA['End Def Tax Asset (Liab)',one]<0) {
                l<-(-OutputDTA['End Def Tax Asset (Liab)',one])
              } else {
                l<-0
                
                BalanceSheetOutput['Deferred Tax Liability',two]<-l
              }
              
              ## BS - Total Liabilities ------------
              
              k<- as.numeric(BalanceSheetOutput['Total Deposits',oneten])
              l<- as.numeric(BalanceSheetOutput['Short Term Borrowings',oneten])
              m<- as.numeric(BalanceSheetOutput['ST Borrowings to Meet Minimum Cash Levels',oneten])
              n<- as.numeric(BalanceSheetOutput['Long Term Borrowings',oneten])
              o<- as.numeric(BalanceSheetOutput['Deferred Tax Liability',oneten])
              o[is.na(o)] <- 0
              p<- as.numeric(BalanceSheetOutput['Other Liabilities',oneten])
              
              BalanceSheetOutput['Total Liabilities',oneten]<- (k+l+m+n+o+p)
              
              remove(k,l,m,n,o,p)
              
              ## BS - Total Liabilities & Shareholders Equity ------------
              
              m<- as.numeric(BalanceSheetOutput['Total Shareholders Equity',oneten])
              n<- as.numeric(BalanceSheetOutput['Total Liabilities',oneten])
              
              BalanceSheetOutput['Total Liabilities and Shareholders Equity',oneten]<- (m+n)
              
              remove(m,n)
              
              
              # DTA - Begin Taxable Income Calculation -------------------------------
              
              OutputDTA['GAAP Pre-tax Inc',1]<- 0
              OutputDTA['GAAP Pre-tax Inc',2:10]<- as.numeric(IncomeStatementOutput['Pre-Tax Income',2:10])
              
              OutputDTA['Plus: Prov For Loan Losses',1]<- 0
              OutputDTA['Plus: Prov For Loan Losses',2:10]<- as.numeric(IncomeStatementOutput['Loan Loss Provision',2:10])
              
              OutputDTA['Less: Net Charge-offs',1]<- 0
              OutputDTA['Less: Net Charge-offs',2:10]<- -(as.numeric(OutputSuppItems['Net Charge-offs',2:10]))
              
              OutputDTA['Plus: OTTI',1]<- 0
              a<- (as.numeric(InputValueDriverAssumpt['Addtl OTTI ($ Actual)',2:10]))
              
              OutputDTA['Plus: OTTI',2:10]<- a
              
              remove(a)
              
              OutputDTA['Plus/Less: chg in AOCI (pre-tax)',oneten]<- c(0,0,0,0,0,0,0,0,0,0)
              # Per Model Assumption of Not including OCI in Calc (Cell D5 on Model Assumpt Tab)
              
              OutputDTA['Less: Perm Diffs',1]<- 0
              
              a<- (as.numeric(InputBudgetBS['Total Perm Differences for Tax Inc',12:20]))
              
              OutputDTA['Less: Perm Diffs',2:10]<- a
              
              remove(a)
              
              OutputDTA['Plus: OREO Writedwn',1]<- 0
              
              a<- (as.numeric(IncomeStatementOutput['OREO Writedown Expense',2:10]))
              
              OutputDTA['Plus: OREO Writedwn',2:10]<- a
              
              remove(a)
              
              OutputDTA['Plus: Goodwill Amort Tax Adjust',1]<- 0
              
              a<- (as.numeric(InputValueDriverAssumpt['Quart tax inc impact from amort of inTangs',2:10]))
              
              OutputDTA['Plus: Goodwill Amort Tax Adjust',2:10]<- a
              
              remove(a)
              
              OutputDTA['Plus: Section 382 Adjust',1]<- 0
              
              a<- (as.numeric(InputValueDriverAssumpt['Quart tax inc impact from Section 382 amounts',2:10]))
              
              OutputDTA['Plus: Section 382 Adjust',2:10]<- a
              
              remove(a)
              
              OutputDTA['Plus: BancTrust loan Adjust',1]<- 0
              
              a<- (as.numeric(InputValueDriverAssumpt['Quart tax inc impact from acq loan mark',2:10]))
              
              OutputDTA['Plus: BancTrust loan Adjust',2:10]<- a
              
              remove(a)
              
              a <- as.numeric(OutputDTA['GAAP Pre-tax Inc',oneten])
              b <- as.numeric(OutputDTA['Plus: Prov For Loan Losses',oneten])
              c <- as.numeric(OutputDTA['Less: Net Charge-offs',oneten])
              d <- as.numeric(OutputDTA['Plus: OTTI',oneten])
              e <- as.numeric(OutputDTA['Plus/Less: chg in AOCI (pre-tax)',oneten])
              f <- as.numeric(OutputDTA['Less: Perm Diffs',oneten])
              g <- as.numeric(OutputDTA['Plus: OREO Writedwn',oneten])
              h <- as.numeric(OutputDTA['Plus: Goodwill Amort Tax Adjust',oneten])
              i <- as.numeric(OutputDTA['Plus: Section 382 Adjust',oneten])
              j <- as.numeric(OutputDTA['Plus: BancTrust loan Adjust',oneten])
              
              OutputDTA['Taxable Inc (Loss)',oneten]<- a+b+c+d+e+f+g+h+i+j
              
              # DTA - Tax Calculation (IRC) ---------------------
              
              OutputDTA['Preliminary Tax Amt',1]<- 0
              
              a<- as.numeric(InputValueDriverAssumpt['Effective Tax Rate (Consolidated entity)',2:10])
              b<- as.numeric(OutputDTA['Taxable Inc (Loss)',2:10])
              
              OutputDTA['Preliminary Tax Amt',2:10]<- a*b
              
              remove(a,b)
              
              OutputDTA['Rolling Bal of Eligible Carrybk Already Utilized',1]<- 0 
              
              OutputDTA['Increm NOL Tax Credit',1]<- 0 
              
              OutputDTA['Increm NOL Balance (for DTA)',1]<- 0 
              
              a<- sum(as.numeric(InputBudgetIS['Income Taxes',one:eight]))
              b<- 0
              
              OutputDTA['Eligible Tax Carrybk Amt',two]<- max(a,b)
              
              remove(a,b)
              
              OutputDTA['Eligible Tax Carrybk Amt',one]<- OutputDTA['Eligible Tax Carrybk Amt',two]
              
              OutputDTA['Tax Carrybk Adjust',one]<- 0 
              
              ifelse((OutputDTA['Preliminary Tax Amt',two]>0), l<- 0, ifelse((-as.numeric(OutputDTA['Preliminary Tax Amt',two])>as.numeric(OutputDTA['Eligible Tax Carrybk Amt',two])), l<- -as.numeric(OutputDTA['Eligible Tax Carrybk Amt',two]), l<- as.numeric(OutputDTA['Preliminary Tax Amt',two])))
              
              OutputDTA['Tax Carrybk Adjust',two]<- l
              
              remove(j,k,l)
              
              a<- as.numeric(OutputDTA['Tax Carrybk Amt in Final Quarter of Eligibility',two])
              b<- as.numeric(OutputDTA['Tax Carrybk Adjust',two])
              c<- as.numeric(OutputDTA['Rolling Bal of Eligible Carrybk Already Utilized',one])
              d<- max(a,0)
              e<- b+c+d
              
              OutputDTA['Rolling Bal of Eligible Carrybk Already Utilized',two]<- min(e,0)
              
              remove(a,b,c,d,e)
              
              if (OutputDTA['Preliminary Tax Amt',two]<0) {
                l<- -as.numeric(OutputDTA['Preliminary Tax Amt',two])
                m<- as.numeric(OutputDTA['Tax Carrybk Adjust',two])
                n<- l+m
              } else {
                n<- 0
              }
              
              OutputDTA['Increm NOL Tax Credit',two]<- n
              
              remove(n)
              
              l<- as.numeric(OutputDTA['Increm NOL Tax Credit',two])
              m<- as.numeric(InputValueDriverAssumpt['Effective Tax Rate (Consolidated entity)',two])
              
              OutputDTA['Increm NOL Balance (for DTA)',two]<- l/m
              
              remove(l,m)
              
              OutputDTA['NOL Balance Available',one]<- as.numeric(InputModelAssumptions['Beginning NOLs',one])
              
              ## DTA - NOL Balance Avaialble ---- 
              
              OutputDTA['NOL Balance Available',1]<- as.numeric(InputModelAssumptions['Beginning NOLs',1])
              
              
              if (OutputDTA['Taxable Inc (Loss)',two]>0) {
                l<- as.numeric(OutputDTA['NOL Balance Available',one])
                m<- as.numeric(OutputDTA['Taxable Inc (Loss)',two])
                n<- l-m
                o<- max(n,0)
              } else {
                o<- as.numeric(OutputDTA['NOL Balance Available',one])
              }
              
              p<- as.numeric(OutputDTA['Increm NOL Balance (for DTA)',two])
              
              OutputDTA['NOL Balance Available',two]<- o+p
              
              remove(o,p)
              
              ##  IS - Income Tax (Check Validity) --------------------
              
              IncomeStatementOutput['Income Taxes',1]<- as.numeric(InputBudgetIS['Income Taxes',11])
              
                if (IncomeStatementOutput['Pre-Tax Income',two]>=0) {
                k<- as.numeric(IncomeStatementOutput['Pre-Tax Income',two])
                l<- as.numeric(OutputDTA['NOL Balance Available',one])
                m<- as.numeric(InputValueDriverAssumpt['Effective Tax Rate (Consolidated entity)',two])
                n<- k-l*m
                o<- max(n,0)
              } else if(OutputDTA['Taxable Income (Loss)',two]<=0) {
                o<- as.numeric(OutputDTA['Tax Carrybk Adjust',two])
              } else {
                o<- 0
              }
              IncomeStatementOutput['Income Taxes',1]<- o
              
              OutputDTA['Tax Carrybk Amt in Final Quarter of Eligibility',1]<- 0
              
              a<- as.numeric(InputBudgetIS['Income Taxes',1:9])
              
              OutputDTA['Tax Carrybk Amt in Final Quarter of Eligibility',2:10]<- a
              
              
              ## IS - Net Income -------------------------
              
              k<- as.numeric(IncomeStatementOutput['Pre-Tax Income',oneten])
              l<- as.numeric(IncomeStatementOutput['Income Taxes',oneten])
              
              IncomeStatementOutput['Net Income',oneten]<- k-l
              
              remove(k,l)
              
              if (BalanceSheetOutput['Deferred Tax Asset',1]=0) {
                l<- -as.numeric(BalanceSheetOutput['Deferred Tax Liability',1])
              } else {
                l<- as.numeric(BalanceSheetOutput['Deferred Tax Asset',1])
              }
              
              OutputDTA['End Def Tax Asset (Liab)',1]<- l
              
              OutputDTA['Beginning Def Tax Asset',1]<- 0
              
              OutputDTA['Beginning Def Tax Asset',2]<- OutputDTA['End Def Tax Asset (Liab)',1]
              
              OutputDTA['Increm DTA chg in ALLL',1]<- 0
              
              a<- as.numeric(InputValueDriverAssumpt['Effective Tax Rate (For DTA Calc Only)',two])
              b<- as.numeric(BalanceSheetOutput['Allowance for Loan and Lease Losses',one])
              c<- as.numeric(BalanceSheetOutput['Allowance for Loan and Lease Losses',two])
              
              OutputDTA['Increm DTA chg in ALLL',two]<- -a*(c-b)
              
              
              
              k<- as.numeric(InputValueDriverAssumpt['DTA from change in acq loan mark',oneten])
              
              OutputDTA['Increm DTA chg in acquired loan mark',oneten]<- k
              
              OutputDTA['Increm DTA OREO Writedwn',1]<- 0
              
              
              k<- as.numeric(InputModelAssumptions['Marginal Tax Rate for DTA Calculations',one])
              l<- as.numeric(IncomeStatementOutput['OREO Writedown Expense',two])
              
              OutputDTA['Increm DTA OREO Writedwn',two]<- k*l
              
              
              OutputDTA['Increm DTA chg in goodwill and intangibles',1]<- 0
              
              k<- as.numeric(InputValueDriverAssumpt['DTA from change in goodwill and inTangs',2:10])
              
              OutputDTA['Increm DTA chg in goodwill and intangibles',2:10]<- k
              
              OutputDTA['Increm DTA chg in Section 382 Amts',1]<- 0
              
              k<- as.numeric(InputValueDriverAssumpt['DTA from change in Section 382 amounts',2:10])
              
              OutputDTA['Increm DTA chg in Section 382 Amts',2:10]<- k
              remove(k)
              
              OutputDTA['Increm DTA chg in AOCI',1]<- 0
              
              k<- as.numeric(InputValueDriverAssumpt['Effective Tax Rate (For DTA Calc Only)',2:10])
              l<- as.numeric(BalanceSheetOutput['AOCI',2:10])
              m<- as.numeric(BalanceSheetOutput['AOCI',1:9])
              n<- as.numeric(InputValueDriverAssumpt['Effective Tax Rate (For DTA Calc Only)',1:9])
              
              OutputDTA['Increm DTA chg in AOCI',2:10]<- k*-((l/(1-k)-m/(1-n)))
              
              remove(k,l,m,n)
              
              OutputDTA['Net chg in DTA before NOLs',1]<- 0
              
              a<- as.numeric(OutputDTA['Increm DTA chg in ALLL',2:10])
              b<- as.numeric(OutputDTA['Increm DTA chg in acquired loan mark',2:10])
              c<- as.numeric(OutputDTA['Increm DTA OREO Writedwn',2:10])
              d<- as.numeric(OutputDTA['Increm DTA chg in goodwill and intangibles',2:10])
              e<- as.numeric(OutputDTA['Increm DTA chg in Section 382 Amts',2:10])
              f<- as.numeric(OutputDTA['Increm DTA chg in AOCI',2:10])
              
              OutputDTA['Net chg in DTA before NOLs',2:10]<- a+b+c+d+e+f
              
              remove(a,b,c,d,e,f)
              
              
              
              OutputDTA['Increm DTA NOLs',1]<-0
              k<- as.numeric(InputValueDriverAssumpt['Effective Tax Rate (For DTA Calc Only)',2:10])
              l<- as.numeric(OutputDTA['NOL Balance Available',2:10])
              m<- as.numeric(OutputDTA['NOL Balance Available',1:9])
              
              OutputDTA['Increm DTA NOLs',2:10]<- k*(l-m)
              remove(k,l,m)
              
              
              a<- as.numeric(OutputDTA['Beginning Def Tax Asset',two])
              b<- as.numeric(OutputDTA['Net chg in DTA before NOLs',two])
              c<- as.numeric(OutputDTA['Increm DTA NOLs',two])
              
              OutputDTA['End Def Tax Asset (Liab)',two]<- a+b+c
              
              remove(a,b,c)
              
              
              a<- as.numeric(InputRegCapital['LESSNet Unrlzd Gain AFS Sec',6])
              a[is.na(a)] <- 0
              b<- as.numeric(OutputDTA['End Def Tax Asset (Liab)',1])
              
              OutputDTA['Disallow DTA End Def Tax Asset (Liab)',1]<- a+b
              
              remove(a,b)
              
              z<- as.numeric(OutputDTA['Disallow DTA End Def Tax Asset (Liab)',one])
              a<- as.numeric(OutputDTA['Increm DTA chg in ALLL',two])
              b<- as.numeric(OutputDTA['Increm DTA chg in acquired loan mark',two])
              c<- as.numeric(OutputDTA['Increm DTA OREO Writedwn',two])
              d<- as.numeric(OutputDTA['Increm DTA chg in goodwill and intangibles',two])
              e<- as.numeric(OutputDTA['Increm DTA chg in Section 382 Amts',two])
              f<- as.numeric(OutputDTA['Increm DTA NOLs',two])
              
              OutputDTA['Disallow DTA End Def Tax Asset (Liab)',two]<- z+a+b+c+d+e+f
              
              remove(z,a,b,c,d,e,f)
              
              # PLACEHOLDER (Working on figuring out where to put Cash & Equiv)----
              ## BS - Total Assets ----------------
              
              k<- as.numeric(BalanceSheetOutput['Cash & Equivalents',oneten])
              l<- as.numeric(BalanceSheetOutput['Total Investments',oneten])
              m<- as.numeric(BalanceSheetOutput['Net Loans',oneten])
              n<- as.numeric(BalanceSheetOutput['FDIC LSA Receivable',oneten])
              n[is.na(n)] <- 0
              o<- as.numeric(BalanceSheetOutput['Goodwill and Intangibles',oneten])
              p<- as.numeric(BalanceSheetOutput['Mortgage Servicing Rights',oneten])
              q<- as.numeric(BalanceSheetOutput['Deferred Tax Asset',oneten])
              r<- as.numeric(BalanceSheetOutput['Other Real Estate Owned',oneten])
              s<- as.numeric(BalanceSheetOutput['Other Assets',oneten])
              
              BalanceSheetOutput['Total Assets',oneten]<- k+l+m+n+o+p+q+r+s
              
              remove(k,l,m,n,o,p,q,r,s)
              
              ##  BS - Cash & Equivalents ---------------------------------------
              
              k <- as.numeric(InputBudgetBS['Cash & Equivalents (includes Fed Funds Sold)',11])
              BalanceSheetOutput['Cash & Equivalents',1] <- k
              remove(k)
              
              BalanceSheetOutput['Cash & Equivalents - Calculated',1]<- BalanceSheetOutput['Cash & Equivalents',1]
              
              k<- as.numeric(BalanceSheetOutput['Total Investments',2:10])
              l<- as.numeric(BalanceSheetOutput['Net Loans',2:10])
              m<- as.numeric(BalanceSheetOutput['FDIC LSA Receivable',2:10])
              n<- as.numeric(BalanceSheetOutput['Goodwill and Intangibles',2:10])
              o<- as.numeric(BalanceSheetOutput['Mortgage Servicing Rights',2:10])
              p<- as.numeric(BalanceSheetOutput['Deferred Tax Asset',1:9])
              q<- as.numeric(BalanceSheetOutput['Other Real Estate Owned',2:10])
              r<- as.numeric(BalanceSheetOutput['Other Assets',2:10])
              s<- as.numeric(BalanceSheetOutput['Total Deposits',2:10])
              t<- as.numeric(BalanceSheetOutput['Short Term Borrowings',2:10])
              u<- as.numeric(BalanceSheetOutput['Long Term Borrowings',2:10])
              v<- as.numeric(BalanceSheetOutput['Other Liabilities',2:10])
              w<- as.numeric(BalanceSheetOutput['Common Stock',2:10])
              x<- as.numeric(BalanceSheetOutput['Treasury Stock',2:10])
              y<- as.numeric(BalanceSheetOutput['Preferred Stock',2:10])
              z<- as.numeric(BalanceSheetOutput['Retained Earnings',2:10])
              a<- as.numeric(BalanceSheetOutput['AOCI',2:10])
              
              BalanceSheetOutput['Cash & Equivalents - Calculated',2:10]<- (s+t+u+v+w+x+y+z+a)-(k+l+m+n+o+p+q+r)
              
              remove(k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a)
              
              BalanceSheetOutput['Cash & Equivalents - Managed Amount',1]<- 0
              k<- as.numeric(InputValueDriverAssumpt['Excess Cash Threshold',2:10])
              l<- as.numeric(BalanceSheetOutput['Cash & Equivalents - Calculated',2:10])
              m<- as.numeric(InputValueDriverAssumpt['Minimum Cash Threshold',2:10])
              n<- l-k
              o<- l-m
              p<- (max(0,n)-min(0,o))
              p[is.na(p)] <- 0
              
              BalanceSheetOutput['Cash & Equivalents - Managed Amount',2:10]<- p
              
              remove(k,l,m,n,o,p)
              
              k<- as.numeric(BalanceSheetOutput['Cash & Equivalents - Managed Amount',2:10])
              l<- as.numeric(BalanceSheetOutput['Cash & Equivalents - Calculated',2:10])
              
              BalanceSheetOutput['Cash & Equivalents',2:10]<- k+l
              
              remove(j,k,l,m,n,o,p)
              
              # End - Deferred Tax Asset ------------------
              
              # Begin - Base1 RWA --------------------
              
              k <-as.numeric(InputRegCapital['Crdt EqFinancial SB LOC',6])
              l <-as.numeric(InputRegCapital['Crdt EqPerformance SB LOC',6])
              m <-as.numeric(InputRegCapital['Crdt EqCommercial LOC',6])
              
              
              OutputRWABasel1['Credit Equiv Standby Letters of Credit',1]<- k+l+m
              
              remove(k,l,m)
              
              OutputRWABasel1['Credit Equiv Standby Letters of Credit',two]<- as.numeric(OutputRWABasel1['Credit Equiv Standby Letters of Credit',one])
              
              
              k <-as.numeric(InputRegCapital['Crdt EqUnused Cmt 1yr Mat',6])
              
              OutputRWABasel1['Credit Equiv Unfunded Commit > 1 year',1]<- k
              
              remove(k)
              
              
              k <- as.numeric(OutputRWABasel1['Credit Equiv Unfunded Commit > 1 year',one])
              l <- as.numeric(BalanceSheetOutput['Total Loans',one])
              m <- as.numeric(BalanceSheetOutput['Total Loans',two])
              
              OutputRWABasel1['Credit Equiv Unfunded Commit > 1 year',two]<- (k/l)*m
              
              remove(k,l,m)
              
              
              k <- as.numeric(InputOffBSItems['Unused Commitments: Other Commitments',4])
              l <- as.numeric(InputRegCapital['NotAmtUnused Cmt 1yr Orig Mat',6])
              m <- as.numeric(InputOffBSItems['Unused Cmt:1-4 Fam Rev Lines',4])
              n <- as.numeric(InputOffBSItems['Unused Commitments: Credit Card Lines',4])
              o <- k-(l-(m+n))
              
              OutputRWABasel1['Notional Equiv Unfunded Commit < 1 year',1]<- o
              
              remove(k,l,m,n,o)
              
              
              k <- as.numeric(OutputRWABasel1['Notional Equiv Unfunded Commit < 1 year',one])
              l <- as.numeric(BalanceSheetOutput['Total Loans',one])
              m <- as.numeric(BalanceSheetOutput['Total Loans',two])
              
              OutputRWABasel1['Notional Equiv Unfunded Commit < 1 year',two]<- (k/l)*m
              
              remove(k,l,m)
              
              
              
              k <- as.numeric(InputRegCapital['Crdt EqDerivative contracts',6])
              
              OutputRWABasel1['Credit Equiv Deriv Contracts',1]<- k
              
              OutputRWABasel1['Credit Equiv Deriv Contracts',two]<- as.numeric(OutputRWABasel1['Credit Equiv Deriv Contracts',one])
              
              
              
              k <- as.numeric(OutputRWABasel1['Cash & Equivs',11])
              l <- as.numeric(BalanceSheetOutput['Cash & Equivalents',1])
              m <- as.numeric(BalanceSheetOutput['Excess Cash & Equivalents',1])
              
              OutputRWABasel1['Cash & Equivs',1]<- (l-m)*k
              
              k <- as.numeric(OutputRWABasel1['Cash & Equivs',11])
              l <- as.numeric(BalanceSheetOutput['Cash & Equivalents',two])
              m <- as.numeric(BalanceSheetOutput['Excess Cash & Equivalents',two])
              
              OutputRWABasel1['Cash & Equivs',two]<- (l-m)*k
              
              
              
              
              k <- as.numeric(OutputRWABasel1['Excess Cash & Equivs',11])
              l <- as.numeric(BalanceSheetOutput['Excess Cash & Equivalents',oneten])
              m <- k*l
              
              OutputRWABasel1['Excess Cash & Equivs',oneten] <- m
              
              remove(k,l,m)
              
              k <- as.numeric(OutputRWABasel1['AFS Investments',11])
              l <- as.numeric(BalanceSheetOutput['AFS Investments',oneten])
              m <- k*l
              
              OutputRWABasel1['AFS Investments',oneten] <- m
              
              remove(k,l,m)
              
              k <- as.numeric(OutputRWABasel1['HTM & Other Investments',11])
              l <- as.numeric(BalanceSheetOutput['HTM Investments',oneten])
              n <- as.numeric(BalanceSheetOutput['Investment Adjustments',oneten])
              m <- k*(l+n)
              
              OutputRWABasel1['HTM & Other Investments',oneten] <- m
              
              remove(k,l,m,n)
              
              k <- as.numeric(InputModelAssumptions['FDIC Reimbursement %',1])
              l <- as.numeric(OutputSuppItems['Covered Loans C&I',oneten])
              m <- as.numeric(OutputSuppItems['Covered Loans CRE',oneten])
              n <- as.numeric(OutputSuppItems['Covered Loans Construc',oneten])
              o <- as.numeric(OutputSuppItems['Covered Loans Resi',oneten])
              p <- as.numeric(OutputSuppItems['Covered Loans Consumer',oneten])
              q <- as.numeric(OutputSuppItems['Covered Loans Other',oneten])
              r <- as.numeric(OutputSuppItems['Covered Loans S&P Subdivisions',oneten])
              s <- as.numeric(OutputRWABasel1['Covered Loans (Guaranteed Balance Only)',11])
              
              OutputRWABasel1['Covered Loans (Guaranteed Balance Only)',oneten] <- k*(l+m+n+o+p+q+r)*s
              
              remove(k,l,m,n,o,p,q,r,s)
              
              k <- as.numeric(BalanceSheetOutput['C&I',oneten])
              l <- as.numeric(OutputSuppItems['Covered Loans C&I',oneten])
              m <- as.numeric(InputModelAssumptions['FDIC Reimbursement %',1])
              n <- as.numeric(OutputRWABasel1['C&I Loans',11])
              
              OutputRWABasel1['C&I Loans',oneten] <- (k-(l*(m)))*n
              
              k <- as.numeric(BalanceSheetOutput['CRE',oneten])
              l <- as.numeric(OutputSuppItems['CRE Loans (Multifamily)',oneten])
              m <- as.numeric(OutputSuppItems['Covered Loans CRE',oneten])
              n <- as.numeric(InputModelAssumptions['FDIC Reimbursement %',1])
              o <- as.numeric(OutputRWABasel1['CRE Loans (Non-Multifam)',11])
              
              OutputRWABasel1['CRE Loans (Non-Multifam)',oneten] <- (k-(m*(n))-l)*o
              
              k <- as.numeric(OutputRWABasel1['CRE Loans (Multifam)',11])
              l <- as.numeric(OutputSuppItems['CRE Loans (Multifamily)',oneten])
              m <- k*l
              
              OutputRWABasel1['CRE Loans (Multifam)',oneten] <- m
              
              remove(k,l,m)
              
              k <- as.numeric(BalanceSheetOutput['Construction',oneten])
              l <- as.numeric(OutputSuppItems['Covered Loans Construc',oneten])
              m <- as.numeric(InputModelAssumptions['FDIC Reimbursement %',1])
              n <- as.numeric(OutputRWABasel1['Construction Loans',11])
              
              OutputRWABasel1['Construction Loans',oneten] <- (k-(l*(m)))*n
              
              k <- as.numeric(BalanceSheetOutput['Residential',oneten])
              l <- as.numeric(OutputSuppItems['Residential Loans with 50% RW',oneten])
              m <- as.numeric(OutputSuppItems['Loans Held For Sale',oneten])
              n <- as.numeric(OutputSuppItems['1st Lien Resi 90+ DPD % of Total Resi (Uncov)',oneten])
              o <- as.numeric(OutputSuppItems['2nd Lien Resi 90+ DPD % of Total Resi (Uncov)',oneten])
              p <- as.numeric(OutputRWABasel1['Residential Loans - 2nd lien',11])
              
              OutputRWABasel1['Residential Loans - 2nd lien',oneten]<- (k-l-m-n-o)*p
              
              remove(k,l,m,n,o,p)
              
              k <- as.numeric(OutputSuppItems['1st Lien Resi 90+ DPD % of Total Resi (Uncov)',oneten])
              l <- as.numeric(OutputSuppItems['2nd Lien Resi 90+ DPD % of Total Resi (Uncov)',oneten])
              m <- as.numeric(OutputRWABasel1['Residential Loans - 90+ DPD',11])
              
              OutputRWABasel1['Residential Loans - 90+ DPD',oneten]<- (k+l)*m
              
              remove(k,l,m)
              
              k <- as.numeric(OutputSuppItems['Residential Loans with 50% RW',oneten])
              l <- as.numeric(OutputRWABasel1['Residential Loans',11])
              
              OutputRWABasel1['Residential Loans',oneten]<- k*l
              
              k <- as.numeric(BalanceSheetOutput['Consumer',oneten])
              l <- as.numeric(OutputSuppItems['Covered Loans Consumer',oneten])
              m <- as.numeric(InputModelAssumptions['FDIC Reimbursement %',1])
              n <- as.numeric(OutputRWABasel1['Consumer Loans',11])
              
              OutputRWABasel1['Consumer Loans',oneten]<- (k-(l)*m)*n
              
              remove(k,l,m,n)
              
              k <- as.numeric(BalanceSheetOutput['Other',oneten])
              l <- as.numeric(BalanceSheetOutput['S&P Subdivisions',oneten])
              m <- as.numeric(BalanceSheetOutput['Loan Adjustments',oneten])
              n <- as.numeric(OutputSuppItems['0% Risk Weighted Loans',oneten]) 
              o <- as.numeric(InputModelAssumptions['FDIC Reimbursement %',1])
              p <- as.numeric(OutputSuppItems['20% Risk Weighted Loans',oneten])
              q <- as.numeric(OutputSuppItems['Covered Loans Other',oneten])
              r <- as.numeric(OutputSuppItems['Covered Loans S&P Subdivisions',oneten])
              s <- as.numeric(OutputRWABasel1['Other Loans',11])
              
              OutputRWABasel1['Other Loans',oneten]<- ((k+l-((n+p)*o)-q-r+m)*s)
              
              remove(k,l,m,n,o,p,q,r,s)
              
              k <- as.numeric(OutputSuppItems['Loans Held For Sale',oneten]) 
              l <- as.numeric(OutputRWABasel1['Loans Held for Sale',11])
              
              OutputRWABasel1['Loans Held for Sale',oneten]<- k*l
              
              k <- as.numeric(OutputSuppItems['0% Risk Weighted Loans',oneten]) 
              l <- as.numeric(OutputRWABasel1['0% RW Loans',11])
              
              OutputRWABasel1['0% RW Loans',oneten]<- k*l
              
              k <- as.numeric(OutputSuppItems['20% Risk Weighted Loans',oneten]) 
              l <- as.numeric(OutputRWABasel1['20% RW Loans',11])
              
              OutputRWABasel1['20% RW Loans',oneten]<- k*l
              
              k <- as.numeric(BalanceSheetOutput['Allowance for Loan and Lease Losses',oneten]) 
              l <- as.numeric(OutputRWABasel1['Allow and Loan Adjust',11])
              
              OutputRWABasel1['Allow and Loan Adjust',oneten]<- k*l
              
              k <- as.numeric(BalanceSheetOutput['FDIC LSA Receivable',oneten]) 
              k[is.na(k)] <- 0
              l <- as.numeric(OutputRWABasel1['FDIC LSA Receivable',11])
              
              
              OutputRWABasel1['FDIC LSA Receivable',oneten]<- k*l
              
              k <- as.numeric(BalanceSheetOutput['Goodwill and Intangibles',oneten]) 
              l <- as.numeric(OutputRWABasel1['Goodwill and Intangibles',11])
              
              OutputRWABasel1['Goodwill and Intangibles',oneten]<- k*l
              
              k <- as.numeric(BalanceSheetOutput['Mortgage Servicing Rights',oneten]) 
              l <- as.numeric(OutputRWABasel1['Mortgage Servicing Rights',11])
              
              OutputRWABasel1['Mortgage Servicing Rights',oneten]<- k*l
              
              
              k <- as.numeric(BalanceSheetOutput['Other Real Estate Owned',oneten]) 
              l <- as.numeric(OutputRWABasel1['Other Real Estate Owned',11])
              
              OutputRWABasel1['Other Real Estate Owned',oneten]<- k*l
              
              k <- as.numeric(OutputSuppItems['Equity Securities',oneten]) 
              l <- as.numeric(OutputRWABasel1['Equity Securities',11])
              
              OutputRWABasel1['Equity Securities',oneten]<- k*l
              
              k <- as.numeric(BalanceSheetOutput['Other Assets',oneten]) 
              l <- as.numeric(OutputSuppItems['Equity Securities',oneten]) 
              m <- as.numeric(OutputRWABasel1['Other Assets',11])
              
              OutputRWABasel1['Other Assets',oneten]<- (k-l)*m
              
              k <- as.numeric(OutputRWABasel1['Credit Equiv Standby Letters of Credit',oneten]) 
              l <- as.numeric(OutputRWABasel1['Standby Letters of Credit',11])
              
              OutputRWABasel1['Standby Letters of Credit',oneten]<- k*l
              
              k <- as.numeric(OutputRWABasel1['Credit Equiv Unfunded Commit > 1 year',oneten]) 
              l <- as.numeric(OutputRWABasel1['Unfunded Commit',11])
              
              OutputRWABasel1['Unfunded Commit',oneten]<- k*l
              
              k <- as.numeric(OutputRWABasel1['Credit Equiv Deriv Contracts',oneten]) 
              l <- as.numeric(OutputRWABasel1['Deriv Contracts',11])
              
              OutputRWABasel1['Deriv Contracts',oneten]<- k*l
              
              k <- as.numeric(OutputCapBasel1['Total Tier 1 Equity',oneten])
              l <- as.numeric(OutputCapBasel1['Total Assets for Leverage Ratio',oneten])
              
              OutputRWABasel1['Tier 1 Leverage Ratio',oneten] <- k/l
              
              k <- as.numeric(BalanceSheetOutput['Total Shareholders Equity',oneten]) 
              l <- as.numeric(BalanceSheetOutput['Preferred Stock',oneten]) 
              m <- as.numeric(BalanceSheetOutput['Goodwill and Intangibles',oneten]) 
              n <- as.numeric(BalanceSheetOutput['Total Assets',oneten]) 
              
              OutputRWABasel1['Tangible Common Equity Ratio',oneten] <- (k-l-m)/(n-m)
              
              k <- as.numeric(BalanceSheetOutput['Net Loans',oneten]) 
              l <- as.numeric(BalanceSheetOutput['Total Deposits',oneten]) 
              
              OutputRWABasel1['Loan to Deposit Ratio',oneten] <- k/l
              
              k <- as.numeric(BalanceSheetOutput['Short Term Borrowings',oneten]) 
              l <- as.numeric(BalanceSheetOutput['ST Borrowings to Meet Minimum Cash Levels',oneten]) 
              m <- as.numeric(BalanceSheetOutput['Long Term Borrowings',oneten]) 
              n <- as.numeric(BalanceSheetOutput['Total Assets',oneten]) 
              o <- .4313
              
              # o hardcoded from percentage in Scenario Model
              
              OutputRWABasel1['FHLB Borrowings to Total Assets',oneten] <- o*(k+l+m)/n
              
              ## (check) Basel1RWA - Deferred Tax Asset ---------
              
              k <- as.numeric(OutputDTA['Disallow DTA End Def Tax Asset (Liab)',oneten]) 
              m <- as.numeric(OutputRWABasel1['Deferred Tax Asset',11])
              
              OutputRWABasel1['Deferred Tax Asset',oneten]<- k*m
              
              # This has a max formula in calculation
              
              ## Total RWA Basel 1 ------
              a <- as.numeric(OutputRWABasel1['Cash & Equivs',oneten]) 
              b <- as.numeric(OutputRWABasel1['Excess Cash & Equivs',oneten]) 
              c <- as.numeric(OutputRWABasel1['AFS Investments',oneten]) 
              d <- as.numeric(OutputRWABasel1['HTM & Other Investments',oneten]) 
              e <- as.numeric(OutputRWABasel1['Covered Loans (Guaranteed Balance Only)',oneten]) 
              f <- as.numeric(OutputRWABasel1['C&I Loans',oneten]) 
              g <- as.numeric(OutputRWABasel1['CRE Loans (Non-Multifam)',oneten]) 
              h <- as.numeric(OutputRWABasel1['CRE Loans (Multifam)',oneten]) 
              i <- as.numeric(OutputRWABasel1['Construction Loans',oneten]) 
              j <- as.numeric(OutputRWABasel1['Residential Loans - 2nd lien',oneten]) 
              k <- as.numeric(OutputRWABasel1['Residential Loans - 90+ DPD',oneten]) 
              l <- as.numeric(OutputRWABasel1['Residential Loans',oneten]) 
              m <- as.numeric(OutputRWABasel1['Consumer Loans',oneten]) 
              n <- as.numeric(OutputRWABasel1['Other Loans',oneten]) 
              o <- as.numeric(OutputRWABasel1['Loans Held for Sale',oneten]) 
              p <- as.numeric(OutputRWABasel1['0% RW Loans',oneten]) 
              q <- as.numeric(OutputRWABasel1['20% RW Loans',oneten]) 
              r <- as.numeric(OutputRWABasel1['Allow and Loan Adjust',oneten]) 
              s <- as.numeric(OutputRWABasel1['FDIC LSA Receivable',oneten]) 
              t <- as.numeric(OutputRWABasel1['Goodwill and Intangibles',oneten])
              u <- as.numeric(OutputRWABasel1['Mortgage Servicing Rights',oneten]) 
              v <- as.numeric(OutputRWABasel1['Deferred Tax Asset',oneten]) 
              w <- as.numeric(OutputRWABasel1['Other Real Estate Owned',oneten]) 
              x <- as.numeric(OutputRWABasel1['Equity Securities',oneten]) 
              y <- as.numeric(OutputRWABasel1['Other Assets',oneten]) 
              z <- as.numeric(OutputRWABasel1['Standby Letters of Credit',oneten]) 
              aa <- as.numeric(OutputRWABasel1['Unfunded Commit',oneten]) 
              ab <- as.numeric(OutputRWABasel1['Deriv Contracts',oneten]) 
              
              OutputRWABasel1['Total Risk Weighted Assets',oneten]<- (a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+aa+ab)
              
              remove(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab)
              
              ## Basel1RWA - Less: Excess Allowance for Loan & Lease Loss --------
              
              k <- as.numeric(BalanceSheetOutput['Allowance for Loan and Lease Losses',oneten]) 
              l <- as.numeric(InputRegCapital['Allw for Ln Losses Incl Tier 2',6]) 
              m <- as.numeric(OutputRWABasel1['Total Risk Weighted Assets',oneten])
              n <- k-(k+l)+0.0125*m
              o <- min(0,o)
              
              OutputRWABasel1['Less: Excess Allow for Loan & Lease Losses',oneten] <- o
              
              
              
              ## Basel1RWA - Adjusted Total RW Assets ------
              
              m <- as.numeric(OutputRWABasel1['Total Risk Weighted Assets',oneten])
              n <- as.numeric(OutputRWABasel1['Less: Excess Allow for Loan & Lease Losses',oneten])
              
              OutputRWABasel1['Adjusted Total Risk Weighted Assets',oneten] <- m+n
              
              k <- as.numeric(BalanceSheetOutput['Allowance for Loan and Lease Losses',onethen])
              l <- as.numeric(BalanceSheetOutput['Allowance for Loan and Lease Losses',oneten])
              m <- as.numeric(InputRegCapital['Allw for Ln Losses Incl Tier 2',6])
              n <- as.numeric(OutputRWABasel1['Total Risk Weighted Assets',oneten])
              o <- -k+(l+m)
              p <- 0.0125*n
              q <- min(o,p)
              
              OutputCapBasel1['Pl Allowance for Loan & Lease Losses',oneten] <- q
              
              ## Basel1Cap - Total Tier 2 Equity & Total Risk-Based Capital ----------
              
              k <- as.numeric(OutputCapBasel1['Pl Qual subord debt & Redeem Pref Stock',oneten])
              l <- as.numeric(OutputCapBasel1['Pl Total Minority Interest not included in Tier 1',oneten])
              m <- as.numeric(OutputCapBasel1['Pl Allowance for Loan & Lease Losses',oneten])
              n <- as.numeric(OutputCapBasel1['Pl Unreal gains on AFS equity securities',oneten])
              o <- as.numeric(OutputCapBasel1['Ls Other deductions (additions) to Tier 2',oneten])
              
              OutputCapBasel1['Total Tier 2 Equity',oneten] <- (k+l+m+n)-o
              
              
              k <- as.numeric(OutputCapBasel1['Total Tier 1 Equity',oneten]) 
              l <- as.numeric(OutputCapBasel1['Total Tier 2 Equity',oneten])
              
              OutputCapBasel1['Total Risk-Based Capital',oneten] <- (k+l)
              
              ## Basel1RWA - Total Tier 1 Common Equity Cap Ratio -----
              
              k <- as.numeric(OutputCapBasel1['Total Tier 1 Common Equity ("CET1")',oneten])
              l <- as.numeric(OutputRWABasel1['Adjusted Total Risk Weighted Assets',oneten]) 
              
              OutputRWABasel1['Tier 1 Common Equity Capital Ratio',oneten] <- k/l
              
              k <- as.numeric(OutputCapBasel1['Total Tier 1 Equity',oneten])
              l <- as.numeric(OutputRWABasel1['Adjusted Total Risk Weighted Assets',oneten]) 
              
              OutputRWABasel1['Tier 1 Equity Capital Ratio',oneten] <- k/l
              
              k <- as.numeric(OutputCapBasel1['Total Risk-Based Capital',oneten])
              l <- as.numeric(OutputRWABasel1['Adjusted Total Risk Weighted Assets',oneten]) 
              
              OutputRWABasel1['Total Risk Based Capital Ratio',oneten] <- k/l
              
              # End - Basel 1 RWA ---------------
              
              # Begin - Disallowed DTA Calculation ----------------
              
              a<- as.numeric(InputBasel1['Comm Stock, Retained Earn, and Other Equity',oneten])
              b<- as.numeric(InputBasel1['Ps: AOCI (if opt in is elected)',oneten])
              c<- as.numeric(InputBasel1['Ls: Unreal gains on AFS equity sec',oneten])
              d<- as.numeric(InputBasel1['Ls: Non-Qual perpetual pref stock',oneten])
              e<- as.numeric(InputBasel1['Ls: Gains (losses) on cash flow hedges',oneten])
              f<- as.numeric(InputBasel1['Ls: Goodwill & Other Intang',oneten])
              g<- as.numeric(InputBasel1['Ls: Disallow servicing assets and PCCR',oneten])
              h<- as.numeric(InputBasel1['Ls: Other deduct (addit) to Tier 1 Comm',oneten])
              i<- as.numeric(InputValueDriverAssumpt['Trust Pref Outstand ($ Thousands)',oneten])
              
              OutputDTA['Total Tier 1 Capital (before DTA Adjust)',oneten]<- (a+b)-(c+d+e+f+g+h)+i
              
              remove(a,b,c,d,e,f,g,h,i)
              
              a<- as.numeric(OutputDTA['Total Tier 1 Capital (before DTA Adjust)',oneten])
              
              OutputDTA['10% Threshold',oneten]<- a*0.1
              
              remove(a)
              
              # did not loop this because of patterning 10/6/16
              a<- as.numeric(IncomeStatementOutput['Income Taxes',2:5])
              
              OutputDTA['Proj NTM Inc Tax',1]<- sum(a)
              
              remove(a)
              
              a<- as.numeric(IncomeStatementOutput['Income Taxes',3:6])
              
              OutputDTA['Proj NTM Inc Tax',2]<- sum(a)
              
              remove(a)
              
              a<- as.numeric(IncomeStatementOutput['Income Taxes',4:7])
              
              OutputDTA['Proj NTM Inc Tax',3]<- sum(a)
              
              remove(a)
              
              a<- as.numeric(IncomeStatementOutput['Income Taxes',5:8])
              
              OutputDTA['Proj NTM Inc Tax',4]<- sum(a)
              
              remove(a)
              
              a<- as.numeric(IncomeStatementOutput['Income Taxes',6:9])
              
              OutputDTA['Proj NTM Inc Tax',5]<- sum(a)
              
              remove(a)
              
              
              a<- as.numeric(IncomeStatementOutput['Income Taxes',7:10])
              
              OutputDTA['Proj NTM Inc Tax',6]<- sum(a)*(4/4)
              
              remove(a)
              
              a<- as.numeric(IncomeStatementOutput['Income Taxes',8:10])
              
              OutputDTA['Proj NTM Inc Tax',7]<- sum(a)*(4/3)
              
              remove(a)
              
              a<- as.numeric(IncomeStatementOutput['Income Taxes',9:10])
              
              OutputDTA['Proj NTM Inc Tax',8]<- sum(a)*(4/2)
              
              remove(a)
              
              a<- as.numeric(IncomeStatementOutput['Income Taxes',10])
              
              OutputDTA['Proj NTM Inc Tax',9]<- sum(a)*(4/2)
              
              remove(a)
              
              a<- as.numeric(IncomeStatementOutput['Income Taxes',10])
              
              OutputDTA['Proj NTM Inc Tax',10]<- sum(a)*(4/2)
              
              remove(a)
              
              # Check this calc once Net Income is completed!
              
              a<- as.numeric(OutputDTA['Disallow DTA End Def Tax Asset (Liab)',oneten])
              
              OutputDTA['Net Def Tax Asset',oneten]<- a
              
              remove(a)
              
              a<- as.numeric(OutputDTA['Eligible Tax Carrybk Amt',oneten])
              b<- as.numeric(OutputDTA['Tax Carrybk Adjust',oneten])
              OutputDTA['Less: Carrybk Adjust',oneten]<- -(a+b)
              
              a<- as.numeric(OutputDTA['Net Def Tax Asset',oneten])
              b<- as.numeric(OutputDTA['Less: Carrybk Adjust',oneten])
              
              OutputDTA['Amt Dependent on Future Inc',oneten]<- (a+b)
              
              remove(a,b)
              
              
              a<- as.numeric(OutputDTA['10% Threshold',oneten])
              b<- as.numeric(OutputDTA['Proj NTM Inc Tax',oneten])
              c<- as.numeric(OutputDTA['Amt Dependent on Future Inc',oneten])
              d<- min(a,b,c)
              
              OutputDTA['Lesser of 10% Threshold and Proj Inc Tax',oneten]<- d
            }
            
            
            if (OutputDTA['Disallow DTA End Def Tax Asset (Liab)',oneten]<0) {
              OutputDTA['Disallow Def Tax Asset',oneten]<- 0
            } else {
              l<- as.numeric(OutputDTA['Disallow DTA End Def Tax Asset (Liab)',oneten])
              m<- as.numeric(OutputDTA['10% Threshold',oneten])
              n<- as.numeric(OutputDTA['Amt Dependent on Future Inc',oneten])
              o<- as.numeric(OutputDTA['Lesser of 10% Threshold and Proj Inc Tax',oneten])
              p<- (l-m)
              q<- (n-o)
              r<- 0
              s<- max(r,p,q)
              OutputDTA['Disallow Def Tax Asset',oneten]<-min(l,s)
            }
            
            remove(a,b,c,l,m,n,o,p,q,r,s)
            
            k <- as.numeric(OutputDTA['Disallow Def Tax Asset',oneten]) 
            
            OutputCapBasel1['Ls Disallow DTA',oneten] <- k
            
            k <- as.numeric(InputValueDriverAssumpt['Deducts from CET1 (Actual)',2:10])
            
            OutputCapBasel1['Ls Other deductions (additions) to Tier 1 Common',1] <- 0
            OutputCapBasel1['Ls Other deductions (additions) to Tier 1 Common',2:10] <- k
            
            k <- as.numeric(OutputCapBasel1['Common Stock, Retained Earnings, and Other Equity',oneten]) 
            l <- as.numeric(OutputCapBasel1['Pl AOCI (if opt in is elected)',oneten]) 
            m <- as.numeric(OutputCapBasel1['Ls Unreal gains on AFS equity securities',oneten]) 
            n <- as.numeric(OutputCapBasel1['Ls Non-Qualif perpetual preferred stock',oneten]) 
            o <- as.numeric(OutputCapBasel1['Ls Gains (losses) on cash flow hedges',oneten]) 
            p <- as.numeric(OutputCapBasel1['Ls Goodwill & Other Intangibles',oneten]) 
            q <- as.numeric(OutputCapBasel1['Ls Disallow servicing assets and PCCR',oneten]) 
            r <- as.numeric(OutputCapBasel1['Ls Disallow DTA',oneten]) 
            s <- as.numeric(OutputCapBasel1['Ls Other deductions (additions) to Tier 1 Common',oneten]) 
            
            OutputCapBasel1['Total Tier 1 Common Equity ("CET1")',oneten] <- (k+l)-(m+n+o+p+q+r+s)
            
            remove(k,l,m,n,o,p,q,r,s)
            ## Pl Qualif TruPS & minority interest in subs (25% of...) at the end of section
            
            
            k <- as.numeric(InputValueDriverAssumpt['Trust Pref Outstand (Thousands)',oneten])
            l <- as.numeric(OutputCapBasel1['Total Tier 1 Common Equity ("CET1")',oneten])
            m <- as.numeric(OutputCapBasel1['Pl Addt Tier 1 capital instruments',oneten])
            n <- as.numeric(OutputCapBasel1['Pl Tier 1 Minority Interest not included in CET1',oneten]) 
            o <- as.numeric(OutputCapBasel1['Pl Subordinated Capital Addition / (Deduction)',oneten])
            p <- as.numeric(OutputCapBasel1['Ls Other deductions (additions) to Tier 1',oneten])
            q <- (l+m+n+o-p)*.3333
            r <- min(k,q)
            #.3333 hard coded from percentage on Scenario Model
            
            OutputCapBasel1['Pl Qualif TruPS & minor int in subs (25% of Tier 1 Cap)',oneten] <- r
            
            
            k <- as.numeric(OutputCapBasel1['Total Tier 1 Common Equity ("CET1")',oneten])
            l <- as.numeric(OutputCapBasel1['Pl Qualif TruPS & minor int in subs (25% of Tier 1 Cap)',oneten])
            m <- as.numeric(OutputCapBasel1['Pl Addt Tier 1 capital instruments',oneten])
            n <- as.numeric(OutputCapBasel1['Pl Tier 1 Minority Interest not included in CET1',oneten]) 
            o <- as.numeric(OutputCapBasel1['Pl Subordinated Capital Addition / (Deduction)',oneten])
            p <- as.numeric(OutputCapBasel1['Ls Other deductions (additions) to Tier 1',oneten])
            
            OutputCapBasel1['Total Tier 1 Equity',oneten] <- (k+l+m+n+o)-p
            
            remove(k,l,m,n,o,p)
            
            k <- as.numeric(OutputDTA['Disallow Def Tax Asset',oneten])
            OutputCapBasel1['Ls Disallow deferred tax assets',oneten] <- k
            
            k <- as.numeric(OutputCapBasel1['Average Total Assets',oneten])
            l <- as.numeric(OutputCapBasel1['Ls Goodwill & Other Intangibles2',oneten])
            m <- as.numeric(OutputCapBasel1['Ls Disallow servicing assets and PCCR2',oneten])
            n <- as.numeric(OutputCapBasel1['Ls Disallow deferred tax assets',oneten])
            o <- as.numeric(OutputCapBasel1['Ls Other deductions from leverage capital',oneten])
            
            
            OutputCapBasel1['Total Assets for Leverage Ratio',oneten] <- k-(l+m+n+o)
            
            # End - Deffered Tax Asset Calculations ---------------
            
            # Begin - Parent Company Cash Calculations --------------
            # (Purposely did not loop this first section due to formulas)
            
            OutputParentCF['Beginning Cash Bal',1] <- 0
            OutputParentCF['Pl Parent Cash Inc',1] <- 0
            OutputParentCF['Ls Parent Cash Int Exp',1] <- 0
            OutputParentCF['Ls Parent Cash Op Exp',1] <- 0
            OutputParentCF['Parent Pre-Tax Inc',1] <- 0
            OutputParentCF['Parent After-Tax Cash Inc',1] <- 0
            
            OutputParentCF['Beginning Cash Bal',1] <- 0
            k <- as.numeric(InputParentCFForecast['TMK Corp Cash - Beg of Pd',12])
            OutputParentCF['Beginning Cash Bal',2] <- k
            remove(k)
            
            m <- as.numeric(InputParentCFForecast['Other Sources',12:20])
            OutputParentCF['Pl Parent Cash Inc',2:10] <- m
            remove(m)
            
            
            m <- -as.numeric(InputParentCFForecast['Int Payments',12])
            OutputParentCF['Ls Parent Cash Int Exp',2] <- m
            remove(m)
            
            k <- as.numeric(InputParentCFForecast['Cash Collat Position on Int Rate Swaps',12])
            l <- as.numeric(InputParentCFForecast['Investment in /Sale of Subs',12])
            m <- as.numeric(InputParentCFForecast['Other Uses',12])
            n <- as.numeric(InputParentCFForecast['BTrust TRUPS/TARP/Silverton/One-Time Exp',12])
            
            OutputParentCF['Ls Parent Cash Op Exp',2]<- -(k+l+m+n)
            
            remove(k,l,m,n)
            
            k <- as.numeric(OutputParentCF['Pl Parent Cash Inc',2])
            l <- as.numeric(OutputParentCF['Ls Parent Cash Int Exp',2])
            m <- as.numeric(OutputParentCF['Ls Parent Cash Op Exp',2])
            
            OutputParentCF['Parent Pre-Tax Inc',2]<- k-l-m
            
            remove(k,l,m)
            
            OutputParentCF['Ls Parent Tax Exp',oneten]<-c(0,0,0,0,0,0,0,0,0,0)
            
            k <- as.numeric(OutputParentCF['Parent Pre-Tax Inc',2])
            l <- as.numeric(OutputParentCF['Ls Parent Tax Exp',2])
            
            OutputParentCF['Parent After-Tax Cash Inc',2]<- k-l
            
            remove(k,l)
            
            k <- as.numeric(InputTMBANKCalcIS['Net Income',oneten])
            OutputParentCF['Sub Bank Net Inc',oneten] <- k
            
            remove(k)
            
            k <- as.numeric(InputValueDriverAssumpt['Upstream Divs to TRMK ($ Actual)',oneten])
            OutputParentCF['Prelim Sub Bank Upstream Divs Paid',oneten] <- k
            
            remove(k)
            
            k <- as.numeric(sum(InputTMBANKIS['Net Income',1:11]))
            l <- as.numeric(sum(InputTMBANKIS['Upstream Dividends to Holding Company',oneten]))
            
            OutputParentCF['Upstream Div Limit',1] <- k-l
            
            # Need to calculate Non-Bank Dividend to Parent before "Ending Avail Dividend" & "Selected Dividend to Parent"
            
            if (OutputParentCF['Prelim Sub Bank Upstream Divs Paid',1]>OutputParentCF['Upstream Div Limit',1]) {
              l<- as.numeric(OutputParentCF['Upstream Div Limit',1])
              m<- max(l,0)
            } else {
              m<- as.numeric(OutputParentCF['Prelim Sub Bank Upstream Divs Paid',1])
            }
            
            OutputParentCF['Selected Div to Parent',1] <- m
            
            remove(m)
            
            k<- OutputParentCF['Selected Div to Parent',1]
            l<- OutputParentCF['Upstream Div Limit',1]
            
            OutputParentCF['Ending Available Div Capacity',1]<- l-k
            
            remove(k,l)
            
            OutputParentCF['Non-Bank Div to Parent',1]<- 0
            
            k <- as.numeric(InputParentCFForecast['Extra Div from Bank/Perm Reduc of Surplus',eleven])
            l <- as.numeric(InputParentCFForecast['Div from Somerville',eleven])
            
            OutputParentCF['Non-Bank Div to Parent',2:10]<- k+l
            
            remove(k,l)
            
            k <- as.numeric(OutputParentCF['Ending Available Div Capacity',1])
            l <- as.numeric(OutputParentCF['Sub Bank Net Inc',2])
            m <- as.numeric(sum(InputTMBANKIS['Upstream Dividends to Holding Company',1:4]))
            n <- as.numeric(sum(InputTMBANKIS['Net Income',1:4]))
            
            OutputParentCF['Upstream Div Limit',2]<- k+l-(n-m)
            
            remove(k,l,m,n)
            
            
            if (OutputParentCF['Prelim Sub Bank Upstream Divs Paid',2]>OutputParentCF['Upstream Div Limit',2]) {
              l<- as.numeric(OutputParentCF['Upstream Div Limit',2])
              m<- max(l,0)
            } else {
              m<- as.numeric(OutputParentCF['Prelim Sub Bank Upstream Divs Paid',2])
            }
            
            OutputParentCF['Selected Div to Parent',2] <- m
            
            remove(m)
            
            k <- as.numeric(OutputParentCF['Upstream Div Limit',2])
            l <- as.numeric(OutputParentCF['Selected Div to Parent',2])
            
            OutputParentCF['Ending Available Div Capacity',2]<- k-l
            
            remove(k,l)
            
            k <- as.numeric(OutputParentCF['Ending Available Div Capacity',2])
            l <- as.numeric(OutputParentCF['Sub Bank Net Inc',3])
            
            OutputParentCF['Upstream Div Limit',3]<- k+l
            
            remove(k,l)
            
            if (OutputParentCF['Prelim Sub Bank Upstream Divs Paid',3]>OutputParentCF['Upstream Div Limit',3]) {
              l<- as.numeric(OutputParentCF['Upstream Div Limit',3])
              m<- max(l,0)
            } else {
              m<- as.numeric(OutputParentCF['Prelim Sub Bank Upstream Divs Paid',3])
            }
            
            OutputParentCF['Selected Div to Parent',3] <- m
            
            remove(m)
            
            k <- as.numeric(OutputParentCF['Upstream Div Limit',3])
            l <- as.numeric(OutputParentCF['Selected Div to Parent',3])
            
            OutputParentCF['Ending Available Div Capacity',3]<- k-l
            
            remove(k,l)
            
            
            k <- as.numeric(OutputParentCF['Ending Available Div Capacity',3])
            l <- as.numeric(OutputParentCF['Sub Bank Net Inc',4])
            
            OutputParentCF['Upstream Div Limit',4]<- k+l
            
            remove(k,l)
            
            if (OutputParentCF['Prelim Sub Bank Upstream Divs Paid',4]>OutputParentCF['Upstream Div Limit',4]) {
              l<- as.numeric(OutputParentCF['Upstream Div Limit',4])
              m<- max(l,0)
            } else {
              m<- as.numeric(OutputParentCF['Prelim Sub Bank Upstream Divs Paid',4])
            }
            
            OutputParentCF['Selected Div to Parent',4] <- m
            
            remove(m)
            
            k <- as.numeric(OutputParentCF['Upstream Div Limit',4])
            l <- as.numeric(OutputParentCF['Selected Div to Parent',4])
            
            OutputParentCF['Ending Available Div Capacity',4]<- k-l
            
            remove(k,l)
            
            
            k <- as.numeric(OutputParentCF['Ending Available Div Capacity',4])
            l <- as.numeric(OutputParentCF['Sub Bank Net Inc',5])
            
            OutputParentCF['Upstream Div Limit',5]<- k+l
            
            remove(k,l)
            
            if (OutputParentCF['Prelim Sub Bank Upstream Divs Paid',5]>OutputParentCF['Upstream Div Limit',5]) {
              l<- as.numeric(OutputParentCF['Upstream Div Limit',5])
              m<- max(l,0)
            } else {
              m<- as.numeric(OutputParentCF['Prelim Sub Bank Upstream Divs Paid',5])
            }
            
            OutputParentCF['Selected Div to Parent',5] <- m
            
            remove(m)
            
            k <- as.numeric(OutputParentCF['Upstream Div Limit',5])
            l <- as.numeric(OutputParentCF['Selected Div to Parent',5])
            
            OutputParentCF['Ending Available Div Capacity',5]<- k-l
            
            remove(k,l)
            
            
            ## Change this one
            
            k <- as.numeric(OutputParentCF['Ending Available Div Capacity',5])
            l <- as.numeric(OutputParentCF['Sub Bank Net Inc',6])
            m <- as.numeric(sum(InputTMBANKIS['Upstream Dividends to Holding Company',5:8]))
            n <- as.numeric(sum(InputTMBANKIS['Net Income',5:8]))
            
            OutputParentCF['Upstream Div Limit',6]<- k+l-(n-m)
            
            remove(k,l,m,n)
            
            if (OutputParentCF['Prelim Sub Bank Upstream Divs Paid',6]>OutputParentCF['Upstream Div Limit',6]) {
              l<- as.numeric(OutputParentCF['Upstream Div Limit',6])
              m<- max(l,0)
            } else {
              m<- as.numeric(OutputParentCF['Prelim Sub Bank Upstream Divs Paid',6])
            }
            
            OutputParentCF['Selected Div to Parent',6] <- m
            
            remove(m)
            
            k <- as.numeric(OutputParentCF['Upstream Div Limit',6])
            l <- as.numeric(OutputParentCF['Selected Div to Parent',6])
            
            OutputParentCF['Ending Available Div Capacity',6]<- k-l
            
            remove(k,l)
            
            k <- as.numeric(OutputParentCF['Ending Available Div Capacity',6])
            l <- as.numeric(OutputParentCF['Sub Bank Net Inc',7])
            
            OutputParentCF['Upstream Div Limit',7]<- k+l
            
            remove(k,l)
            
            if (OutputParentCF['Prelim Sub Bank Upstream Divs Paid',7]>OutputParentCF['Upstream Div Limit',7]) {
              l<- as.numeric(OutputParentCF['Upstream Div Limit',7])
              m<- max(l,0)
            } else {
              m<- as.numeric(OutputParentCF['Prelim Sub Bank Upstream Divs Paid',7])
            }
            
            OutputParentCF['Selected Div to Parent',7] <- m
            
            remove(m)
            
            k <- as.numeric(OutputParentCF['Upstream Div Limit',7])
            l <- as.numeric(OutputParentCF['Selected Div to Parent',7])
            
            OutputParentCF['Ending Available Div Capacity',7]<- k-l
            
            remove(k,l)
            
            k <- as.numeric(OutputParentCF['Ending Available Div Capacity',7])
            l <- as.numeric(OutputParentCF['Sub Bank Net Inc',8])
            
            OutputParentCF['Upstream Div Limit',8]<- k+l
            
            remove(k,l)
            
            if (OutputParentCF['Prelim Sub Bank Upstream Divs Paid',8]>OutputParentCF['Upstream Div Limit',8]) {
              l<- as.numeric(OutputParentCF['Upstream Div Limit',8])
              m<- max(l,0)
            } else {
              m<- as.numeric(OutputParentCF['Prelim Sub Bank Upstream Divs Paid',8])
            }
            
            OutputParentCF['Selected Div to Parent',8] <- m
            
            remove(m)
            
            k <- as.numeric(OutputParentCF['Upstream Div Limit',8])
            l <- as.numeric(OutputParentCF['Selected Div to Parent',8])
            
            OutputParentCF['Ending Available Div Capacity',8]<- k-l
            
            remove(k,l)
            
            k <- as.numeric(OutputParentCF['Ending Available Div Capacity',8])
            l <- as.numeric(OutputParentCF['Sub Bank Net Inc',9])
            
            OutputParentCF['Upstream Div Limit',9]<- k+l
            
            remove(k,l)
            
            if (OutputParentCF['Prelim Sub Bank Upstream Divs Paid',9]>OutputParentCF['Upstream Div Limit',9]) {
              l<- as.numeric(OutputParentCF['Upstream Div Limit',9])
              m<- max(l,0)
            } else {
              m<- as.numeric(OutputParentCF['Prelim Sub Bank Upstream Divs Paid',9])
            }
            
            OutputParentCF['Selected Div to Parent',9] <- m
            
            remove(m)
            
            k <- as.numeric(OutputParentCF['Upstream Div Limit',9])
            l <- as.numeric(OutputParentCF['Selected Div to Parent',9])
            
            OutputParentCF['Ending Available Div Capacity',9]<- k-l
            
            remove(k,l)
            
            
            k <- as.numeric(OutputParentCF['Ending Available Div Capacity',9])
            l <- as.numeric(OutputParentCF['Sub Bank Net Inc',10])
            m <- as.numeric(sum(InputTMBANKIS['Upstream Dividends to Holding Company',9:10]))
            n <- as.numeric(sum(InputTMBANKIS['Net Income',9:10]))
            o <- as.numeric(sum(OutputParentCF['Selected Div to Parent',1:2]))
            p <- as.numeric(sum(OutputParentCF['Sub Bank Net Inc',1:2]))
            q <- m+o
            r <- n+p
            
            
            OutputParentCF['Upstream Div Limit',10]<- k+l-(r-q)
            
            remove(k,l,m,n,o,p,q,r)
            
            if (OutputParentCF['Prelim Sub Bank Upstream Divs Paid',10]>OutputParentCF['Upstream Div Limit',10]) {
              l<- as.numeric(OutputParentCF['Upstream Div Limit',10])
              m<- max(l,0)
            } else {
              m<- as.numeric(OutputParentCF['Prelim Sub Bank Upstream Divs Paid',10])
            }
            
            OutputParentCF['Selected Div to Parent',10] <- m
            
            remove(m)
            
            k <- as.numeric(OutputParentCF['Upstream Div Limit',10])
            l <- as.numeric(OutputParentCF['Selected Div to Parent',10])
            
            OutputParentCF['Ending Available Div Capacity',10]<- k-l
            
            remove(k,l)
            
            k <- as.numeric(InputCapitalPlan['Common Stock Issuance ($ Actual)',oneten])
            
            OutputParentCF['Comm Equity Issuance',oneten] <- k
            
            remove(k)
            
            k <- as.numeric(InputCapitalPlan['Common Stock Buyback ($ Actual)',oneten])
            
            OutputParentCF['Comm Equity Buybacks',oneten] <- k
            
            remove(k)
            
            k <- as.numeric(InputCapitalPlan['Tier 1 Preferred Stock Issuance/Buyback ($ Actual)',oneten])
            
            OutputParentCF['Pref Stock Issuance',oneten] <- k
            
            remove(k)
            
            k <- as.numeric(InputCapitalPlan['Issuance Costs ($ Actual)',oneten])
            
            OutputParentCF['Issuance Costs',oneten] <- k
            
            remove(k)
            
            OutputParentCF['Other Tier 1 Cap',oneten] <- c(0,0,0,0,0,0,0,0,0,0)
            
            k <- as.numeric(InputCapitalPlan['Trust Preferred Redemption ($ Actual)',oneten])
            
            OutputParentCF['Trust Pref Redemption',oneten] <- k
            
            remove(k)
            
            k <- as.numeric(InputCapitalPlan['Senior Debt Issued ($ Actual)',oneten])
            
            OutputParentCF['Sen Debt Issued',oneten] <- k
            
            remove(k)
            
            k <- as.numeric(InputCapitalPlan['Trust Preferred Redemption ($ Actual)',oneten])
            
            OutputParentCF['Sen Debt Maturing or Repurchased',oneten] <- k
            
            remove(k)
            
            
            k <- as.numeric(InputCapitalPlan['Qualified Subordinated Debt Issued ($ Actual)',oneten])
            
            OutputParentCF['Qualified Subordinated Debt Issued',oneten] <- k
            
            remove(k)
            
            k <- as.numeric(InputCapitalPlan['Qualified Subordinated Debt Maturing ($ Actual)',oneten])
            
            OutputParentCF['Qualified Subordinated Debt Maturing',oneten] <- k
            
            remove(k)
            
            a <- as.numeric(OutputParentCF['Sen Debt Issued',oneten])
            b <- as.numeric(OutputParentCF['Qualified Subordinated Debt Issued',oneten])
            c <- as.numeric(OutputParentCF['Trust Pref Redemption',oneten])
            d <- as.numeric(OutputParentCF['Sen Debt Maturing or Repurchased',oneten])
            e <- as.numeric(OutputParentCF['Qualified Subordinated Debt Maturing',oneten])
            
            OutputParentCF['Net increase in borrowed funds',oneten] <- (a+b)-(c+d+e)
            
            remove(a,b,c,d,e)
            
            a <- as.numeric(OutputParentCF['Comm Equity Issuance',oneten])
            b <- as.numeric(OutputParentCF['Comm Equity Buybacks',oneten])
            c <- as.numeric(OutputParentCF['Pref Stock Issuance',oneten])
            d <- as.numeric(OutputParentCF['Issuance Costs',oneten])
            e <- as.numeric(OutputParentCF['Other Tier 1 Cap',oneten])
            f <- as.numeric(OutputParentCF['Net increase in borrowed funds',oneten])
            
            OutputParentCF['Total External Sources of Cash',oneten]<- a-b+c-d+e+f
            
            remove(a,b,c,d,e,f)
            
            k <- -as.numeric(IncomeStatementOutput['Preferred Dividend',2:10])
            OutputParentCF['Pref Divs',1]<- 0
            OutputParentCF['Pref Divs',2:10]<- k
            
            remove(k)
            
            k <- -as.numeric(IncomeStatementOutput['Total Com Stock DivParent Upstream Div',2:10])
            OutputParentCF['Comm Divs',1]<- 0
            OutputParentCF['Comm Divs',2:10]<- k
            
            remove(k)
            
            k <- -as.numeric(InputCapitalPlan['Addt Paid in Cap Invest in Subsid ($ Actual)',2:10])
            OutputParentCF['Addt Paid in Cap Invest in Subsidiaries',1]<- 0
            OutputParentCF['Addt Paid in Cap Invest in Subsidiaries',2:10]<- k
            
            remove(k)
            
            a <- as.numeric(OutputParentCF['Pref Divs',2:10])
            b <- as.numeric(OutputParentCF['Comm Divs',2:10])
            c <- as.numeric(OutputParentCF['Addt Paid in Cap Invest in Subsidiaries',2:10])
            
            OutputParentCF['Total External Uses of Cash',1]<- 0
            OutputParentCF['Total External Uses of Cash',2:10]<- a+b+c
            
            remove(a,b,c)
            
            a <- as.numeric(OutputParentCF['Parent After-Tax Cash Inc',2])
            b <- as.numeric(OutputParentCF['Selected Div to Parent',2])
            c <- as.numeric(OutputParentCF['Non-Bank Div to Parent',2])
            d <- as.numeric(OutputParentCF['Total External Sources of Cash',2])
            e <- as.numeric(OutputParentCF['Total External Uses of Cash',2])
            
            OutputParentCF['Net Change in Cash Position',1]<- 0
            OutputParentCF['Net Change in Cash Position',2]<- a+b+c+d+e
            
            a <- as.numeric(OutputParentCF['Beginning Cash Bal',2])
            b <- as.numeric(OutputParentCF['Net Change in Cash Position',2])
            
            OutputParentCF['Ending Cash Bal',1]<- 0
            OutputParentCF['Ending Cash Bal',2]<- a+b
            
            a <- as.numeric(OutputParentCF['Ending Cash Bal',2])
            
            OutputParentCF['Beginning Cash Bal',3]<- a
            
            ## (CHECK THIS) Begin Calculation of fields at top of Parent CF that needed ending first PD balance ----
            
            m <- -as.numeric(InputParentCFForecast['Int Payments',thirt])
            OutputParentCF['Ls Parent Cash Int Exp',three] <- m
            remove(m)
            
            k <- as.numeric(InputParentCFForecast['Cash Collat Position on Int Rate Swaps',thirt])
            l <- as.numeric(InputParentCFForecast['Investment in /Sale of Subs',thirt])
            m <- as.numeric(InputParentCFForecast['Other Uses',thirt])
            n <- as.numeric(InputParentCFForecast['BTrust TRUPS/TARP/Silverton/One-Time Exp',thirt])
            
            OutputParentCF['Ls Parent Cash Op Exp',three]<- -(k+l+m+n)
            
            remove(k,l,m,n)
            
            k <- as.numeric(OutputParentCF['Pl Parent Cash Inc',three])
            l <- as.numeric(OutputParentCF['Ls Parent Cash Int Exp',three])
            m <- as.numeric(OutputParentCF['Ls Parent Cash Op Exp',three])
            
            OutputParentCF['Parent Pre-Tax Inc',three]<- k-l-m
            
            remove(k,l,m)
            
            OutputParentCF['Ls Parent Tax Exp',oneten]<-c(0,0,0,0,0,0,0,0,0,0)
            
            k <- as.numeric(OutputParentCF['Parent Pre-Tax Inc',three])
            l <- as.numeric(OutputParentCF['Ls Parent Tax Exp',three])
            
            OutputParentCF['Parent After-Tax Cash Inc',three]<- k-l
            
            remove(k,l)
            
            a <- as.numeric(OutputParentCF['Parent After-Tax Cash Inc',three])
            b <- as.numeric(OutputParentCF['Selected Div to Parent',three])
            c <- as.numeric(OutputParentCF['Non-Bank Div to Parent',three])
            d <- as.numeric(OutputParentCF['Total External Sources of Cash',three])
            e <- as.numeric(OutputParentCF['Total External Uses of Cash',three])
            
            
            OutputParentCF['Net Change in Cash Position',three]<- a+b+c+d+e
            
            a <- as.numeric(OutputParentCF['Beginning Cash Bal',three])
            b <- as.numeric(OutputParentCF['Net Change in Cash Position',three])
            
            
            OutputParentCF['Ending Cash Bal',three]<- a+b
            
            a <- as.numeric(OutputParentCF['Ending Cash Bal',three])
            
            OutputParentCF['Beginning Cash Bal',four]<- a  
            
            
            a<- as.numeric(OutputParentCF['Ls Parent Cash Int Exp',2:10])
            b<- as.numeric(OutputParentCF['Ls Parent Cash Op Exp',2:10])
            c<- as.numeric(OutputParentCF['Parent After-Tax Cash Inc',2:10])
            d<- as.numeric(OutputParentCF['Selected Div to Parent',2:10])
            e<- as.numeric(OutputParentCF['Non-Bank Div to Parent',2:10])
            f<- as.numeric(OutputParentCF['Sen Debt Maturing or Repurchased',2:10])
            g<- as.numeric(OutputParentCF['Qualified Subordinated Debt Maturing',2:10])
            h<- as.numeric(OutputParentCF['Pref Divs',2:10])
            
            OutputParentCF['Fixed Charge Coverage Ratio',1] <- 0
            OutputParentCF['Fixed Charge Coverage Ratio',2:10] <- (a+b+c+d+e)/(a+b+f+g+h)
            
            remove(a,b,c,d,e,f,g,h)
            
            a<- as.numeric(OutputParentCF['Parent After-Tax Cash Inc',2:10])
            b<- as.numeric(OutputParentCF['Selected Div to Parent',2:10])
            c<- as.numeric(OutputParentCF['Non-Bank Div to Parent',2:10])
            d<- as.numeric(OutputParentCF['Sen Debt Maturing or Repurchased',2:10])
            e<- as.numeric(OutputParentCF['Qualified Subordinated Debt Maturing',2:10])
            f<- as.numeric(OutputParentCF['Pref Divs',2:10])
            g<- as.numeric(OutputParentCF['Comm Divs',2:10])
            
            OutputParentCF['Comm Stock Cash Div Coverage Ratio',1] <- 0
            OutputParentCF['Comm Stock Cash Div Coverage Ratio',2:10] <- (a+b+c-d-e-f)/-g
            
            remove(a,b,c,d,e,f,g)
            
            a<- as.numeric(InputTMBANKCalcBS['Total Shareholders Equity',2:10])
            b<- as.numeric(BalanceSheetOutput['Total Shareholders Equity',2:10])
            
            OutputParentCF['Double Leverage Ratio',1] <- 0
            OutputParentCF['Double Leverage Ratio',2:10] <- a/b
            
            remove(a,b)
            
            OutputParentCF['Addt Inc needed for 1.00 CSCDC',1] <- 0
            
            if (OutputParentCF['Comm Stock Cash Div Coverage Ratio',two]>1) {
              m<- 0
            } else {
              a<- as.numeric(OutputParentCF['Parent After-Tax Cash Inc',two])
              b<- as.numeric(OutputParentCF['Selected Div to Parent',two])
              c<- as.numeric(OutputParentCF['Non-Bank Div to Parent',two])
              d<- as.numeric(OutputParentCF['Sen Debt Maturing or Repurchased',two])
              e<- as.numeric(OutputParentCF['Qualified Subordinated Debt Maturing',two])
              f<- as.numeric(OutputParentCF['Pref Divs',two])
              g<- as.numeric(OutputParentCF['Comm Divs',two])
              m<- -g-(a+b+c-d-e-f)
            }
            OutputParentCF['Addt Inc needed for 1.00 CSCDC',two] <- m
            
            
            a<- as.numeric(OutputParentCF['Ls Parent Cash Op Exp',2:10])
            b<- as.numeric(OutputParentCF['Parent Pre-Tax Inc',2:10])
            c<- as.numeric(OutputParentCF['Selected Div to Parent',2:10])
            d<- as.numeric(OutputParentCF['Non-Bank Div to Parent',2:10])
            
            OutputParentCF['Op Exp Coverage Ratio',1]<- 0
            OutputParentCF['Op Exp Coverage Ratio',2:10]<- (b+c+d)/a
            
            # End - Parent Company Cash Calculations -----------------
            
            # Begin - Basel III Capital-----------------
            k <- as.numeric(OutputCapBasel1['Total Tier 1 Common Equity ("CET1")',oneten])
            
            OutputBasel3Cap['Total Tier 1 Common Equity ("CET1")',oneten] <- k
            
            k <- as.numeric(OutputCapBasel1['Total Tier 1 Equity',oneten])
            
            OutputBasel3Cap['Total Tier 1 Equity',oneten] <- k
            
            k <- as.numeric(OutputCapBasel1['Total Risk-Based Capital',oneten])
            
            OutputBasel3Cap['Total Risk-Based Capital',oneten] <- k
            
            k <- as.numeric(OutputRWABasel1['Total Risk Weighted Assets',oneten])
            
            OutputBasel3Cap['Total Risk Weighted Assets',oneten] <- k
            
            k <- as.numeric(OutputRWABasel1['Adjusted Total Risk Weighted Assets',oneten])
            
            OutputBasel3Cap['Adjusted Total Risk Weighted Assets',oneten] <- k
            
            k <- as.numeric(OutputCapBasel1['Total Assets for Leverage Ratio',oneten])
            
            OutputBasel3Cap['Total Assets for Leverage Ratio',oneten] <- k
            
            # Calculate this once Tier 1 Common Equity is calced
            
            k <- as.numeric(InputValueDriverAssumpt['Trust Pref Outstand (Thousands)',1])
            l <- as.numeric(OutputCapBasel1['Total Tier 1 Common Equity ("CET1")',1])
            m <- as.numeric(OutputCapBasel1['Pl Addt Tier 1 capital instruments',1])
            n <- as.numeric(OutputCapBasel1['Pl Tier 1 Minority Interest not included in CET1',1]) 
            o <- as.numeric(OutputCapBasel1['Pl Subordinated Capital Addition / (Deduction)',1])
            p <- as.numeric(OutputCapBasel1['Ls Other deductions (additions) to Tier 1',1])
            q <- (l+m+n+o-p)*.3333
            r <- min(k,q)
            # .3333 hard coded from percentage on Scenario Model
            
            OutputBasel3Cap['TruPS includable in Tier 1 Equity',1] <- r
            
            k <- as.numeric(OutputDTA['Disallow DTA End Def Tax Asset (Liab)',oneten])
            
            OutputBasel3Cap['Net DTA for RWA Calculation',oneten] <- k
            
            k <- as.numeric(InputValueDriverAssumpt['Effective Tax Rate (For DTA Calc Only)',oneten])
            l <- as.numeric(OutputDTA['NOL Balance Available',oneten])
            
            OutputBasel3Cap['Net DTA Arising from NOLs',oneten] <- k*l
            
            k <- as.numeric(OutputBasel3Cap['Net DTA for RWA Calculation',oneten])
            l <- as.numeric(OutputBasel3Cap['Net DTA Arising from NOLs',oneten])
            
            OutputBasel3Cap['Net DTA Arising from Temporary Differences',oneten] <- k-l
            
            
            k <- as.numeric(OutputBasel3Cap['Net DTA Arising from Temporary Differences',oneten])
            l <- as.numeric(OutputDTA['Eligible Tax Carrybk Amt',oneten])
            m <- k-l
            n <- max(0,m)
            
            OutputBasel3Cap['Net Temporary Differences DTA Not Realizable Through NOL Tax Carrybacks',oneten] <- n
            
            
            k <- as.numeric(BalanceSheetOutput['Mortgage Servicing Rights',oneten])
            
            OutputBasel3Cap['Mortgage Servicing Rights',oneten] <- k
            
            
            
            k <- as.numeric(OutputSuppItems['Equity Invest in Unconsol Fin Inst',oneten])
            l <- as.numeric(InputTMBANKCalcBS['Common Stock',oneten])
            if (k>(l*.10)){
              n<- k
            } else {
              n<- 0
            }
            OutputBasel3Cap['Significant Equity Investment in Unconsolidated Financial Institutions',oneten] <- n
            
            
            
            
            { k<- as.numeric(OutputBasel3Cap['Total Tier 1 Common Equity ("CET1")',oneten])
              l<- as.numeric(OutputBasel3Cap['Net DTA Arising from NOLs',oneten])
              m<- as.numeric(OutputBasel3Cap['Significant Equity Investment in Unconsolidated Financial Institutions',oneten])
              n<- as.numeric(OutputCapBasel1['Ls Gains (losses) on cash flow hedges',oneten])
              o<- as.numeric(OutputSuppItems['Equity Invest in Unconsol Fin Inst',oneten])
              if ((o-m)>(k+n-l)*.10) {
                n<- (k+n-l)-(o-m)
              } else {
                n<- 0
              }
              OutputBasel3Cap['Not Significant Equity Investment in Unconsolidated Financial Institutions < 10% CET1',oneten] <- n
              
              
              k <- as.numeric(OutputBasel3Cap['Total Tier 1 Common Equity ("CET1")',oneten])
              l <- as.numeric(OutputBasel3Cap['Not Significant Equity Investment in Unconsolidated Financial Institutions < 10% CET1',oneten])
              m <- as.numeric(OutputBasel3Cap['Net DTA Arising from NOLs',oneten])
              n <- as.numeric(OutputCapBasel1['Disallow servicing assets and PCCR',oneten])
              o <- as.numeric(OutputCapBasel1['Ls Disallow DTA',oneten])
              
              OutputBasel3Cap['10% Individual Threshold',oneten] <- (k+(n+o)-l-m)*.1
              
              # This will need to be updated as year is changed!! (0% Phase-in in 2014)
              
              OutputBasel3Cap['Phase-in Amount (%)',oneten] <- c(0,0,0,0,0,0,0,0,0,0)
              
              n <- as.numeric(OutputCapBasel1['Disallow servicing assets and PCCR',oneten])
              o <- as.numeric(OutputCapBasel1['Ls Disallow DTA',oneten])
              
              OutputBasel3Cap['Reversal of Basel I Disallowed DTA & Servicing Assets and PCCR',oneten] <- -(n+o)
              
              
              
              l <- as.numeric(OutputBasel3Cap['Net Temporary Differences DTA Not Realizable Through NOL Tax Carrybacks',oneten])
              m <- as.numeric(OutputBasel3Cap['10% Individual Threshold',oneten])
              if (l>m) {
                n<- (l-m)
              } else {
                n<- 0
              }
              OutputBasel3Cap['DTAs in excess of 10% threshold',oneten] <- n
              
              
              l <- as.numeric(OutputBasel3Cap['Mortgage Servicing Rights',oneten])
              m <- as.numeric(OutputBasel3Cap['10% Individual Threshold',oneten])
              if (l>m) {
                n<- (l-m)
              } else {
                n<- 0
              }
              OutputBasel3Cap['MSRs in excess of 10% threshold',oneten] <- n
              
              
              l <- as.numeric(OutputBasel3Cap['Significant Equity Investment in Unconsolidated Financial Institutions',oneten])
              m <- as.numeric(OutputBasel3Cap['10% Individual Threshold',oneten])
              if (l>m) {
                n<- (l-m)
              } else {
                n<- 0
              }
              OutputBasel3Cap['Significant Equity Investments in excess of 10% threshold',oneten] <- n
              
              
              l <- as.numeric(OutputBasel3Cap['Net Temporary Differences DTA Not Realizable Through NOL Tax Carrybacks',oneten])
              m <- as.numeric(OutputBasel3Cap['Mortgage Servicing Rights',oneten])
              n <- as.numeric(OutputBasel3Cap['Significant Equity Investment in Unconsolidated Financial Institutions',oneten])
              o <- as.numeric(OutputBasel3Cap['15% Aggregate Threshold',oneten])
              p <- as.numeric(OutputBasel3Cap['DTAs in excess of 10% threshold',oneten])
              q <- as.numeric(OutputBasel3Cap['MSRs in excess of 10% threshold',oneten])
              r <- as.numeric(OutputBasel3Cap['Significant Equity Investments in excess of 10% threshold',oneten])
              if (((l+m+n)-(p+q+r))>o) {
                n<- (l+m+n)-(p+q+r)-o
              } else {
                n<- 0
              }
              OutputBasel3Cap['Disallowed assets in excess of 15% threshold',oneten] <- n
              
              # This will need to be updated as year is changed!! (0% Phase-in in 2014)
              
              OutputBasel3Cap['Phase-in Amount (%)2',oneten] <- c(0,0,0,0,0,0,0,0,0,0)
              
              n <- as.numeric(OutputSuppItems['High Vola Commercial RE Loans (Uncov)',oneten])
              o <- as.numeric(OutputRWABasel1['CRE Loans (Non-Multifam)',11])
              
              OutputBasel3Cap['High Volatility CRE',oneten] <- n*1.5-n*o
              # 150% is direct input in Scenario Model
              
              k <- as.numeric(OutputSuppItems['C&I Loans - 90+ DPD % of Total C&I (Uncov)',oneten])
              l <- as.numeric(OutputSuppItems['CRE Loans - 90+ DPD % of Total CRE (Uncov)',oneten])
              m <- as.numeric(OutputSuppItems['Construc - 90+ DPD % of Total Construc (Uncov)',oneten])
              n <- as.numeric(OutputSuppItems['Consumer - 90+ DPD % of Total Consumer (Uncov)',oneten])
              o <- as.numeric(OutputSuppItems['Other- 90+ DPD % of Total Other (Uncov)',oneten])
              p <- 1.5
              # 150% is direct input in Scenario Model
              q <- as.numeric(OutputRWABasel1['C&I Loans',11])
              r <- as.numeric(OutputRWABasel1['CRE Loans (Non-Multifam)',11])
              s <- as.numeric(OutputRWABasel1['Construction Loans',11])
              t <- as.numeric(OutputRWABasel1['Consumer Loans',11])
              u <- as.numeric(OutputRWABasel1['Other Loans',11])
              
              OutputBasel3Cap['90+ Day Past Due Loans',oneten] <- (k+l+m+n+o)*p-((k*q)+(l*r)+(m*s)+(n*t)+(p*u))
              
              l <- as.numeric(OutputBasel3Cap['Net Temporary Differences DTA Not Realizable Through NOL Tax Carrybacks',oneten])
              m <- as.numeric(OutputBasel3Cap['Mortgage Servicing Rights',oneten])
              n <- as.numeric(OutputBasel3Cap['Significant Equity Investment in Unconsolidated Financial Institutions',oneten])
              o <- as.numeric(OutputBasel3Cap['Phase-in Amount (%)',oneten])
              p <- as.numeric(OutputBasel3Cap['DTAs in excess of 10% threshold',oneten])
              q <- as.numeric(OutputBasel3Cap['MSRs in excess of 10% threshold',oneten])
              r <- as.numeric(OutputBasel3Cap['Significant Equity Investments in excess of 10% threshold',oneten])
              s <- as.numeric(OutputBasel3Cap['Disallowed assets in excess of 15% threshold',oneten])
              t <- 2.5
              # 250% is direct input in Scenario Model
              if (o=1) {
                u<- t
              } else {
                u<- 1
              }
              v <- ((l+m+n)-(p+q+r+s))*(u)-(((l+m+n)-(p+q+r+s))*1)
              w <- max(0,v)
              
              OutputBasel3Cap['DTAs, MSRs and Equity Investments below thresholds',oneten] <- w
              
              l <- as.numeric(OutputRWABasel1['Notional Equiv Unfunded Commit < 1 year',oneten])
              m <- .2
              # 20% is direct input in Scenario Model
              
              OutputBasel3Cap['Unfunded Commitments <1 Year',oneten] <- l*m
              
              l <- as.numeric(InputValueDriverAssumpt['Repurchase Agreements',oneten])
              m <- .2
              # 20% is direct input in Scenario Model
              
              OutputBasel3Cap['Repurchase Agreements',oneten] <- l*m
              
              # Less:Excess Allowance for Loan & Lease Losses calculated below this section
              
              k <- as.numeric(OutputBasel3Cap['Total Tier 1 Common Equity ("CET1")',oneten])
              l <- as.numeric(OutputBasel3Cap['Net DTA Arising from NOLs',oneten])
              m <- as.numeric(OutputBasel3Cap['DTAs in excess of 10% threshold',oneten])
              n <- as.numeric(OutputBasel3Cap['MSRs in excess of 10% threshold',oneten])
              o <- as.numeric(OutputBasel3Cap['Significant Equity Investments in excess of 10% threshold',oneten])
              p <- as.numeric(OutputBasel3Cap['Disallowed assets in excess of 15% threshold',oneten])
              q <- as.numeric(OutputBasel3Cap['Phase-in Amount (%)',oneten])
              
              OutputBasel3Cap['Basel III Tier 1 Common Equity ("CET1")',oneten] <- k-(l+m+n+o+p)*q
              
              k <- as.numeric(OutputBasel3Cap['Basel III Tier 1 Common Equity ("CET1")',oneten])
              l <- as.numeric(OutputBasel3Cap['TruPS includable in Tier 1 Equity',oneten])
              m <- as.numeric(OutputCapBasel1['Pl Addt Tier 1 capital instruments',oneten])
              n <- as.numeric(OutputCapBasel1['Pl Tier 1 Minority Interest not included in CET1',oneten])
              o <- as.numeric(OutputCapBasel1['Pl Subordinated Capital Addition / (Deduction)',oneten])
              p <- as.numeric(OutputCapBasel1['Ls Other deductions (additions) to Tier 1',oneten])
              
              OutputBasel3Cap['Basel III Tier 1 Equity',oneten] <- k+l+(m+n+o+-p)
              
              k <- as.numeric(OutputBasel3Cap['Basel III Tier 1 Equity',oneten])
              l <- as.numeric(OutputBasel3Cap['Remaining TruPS includable in Tier 2 Equity',oneten])
              m <- as.numeric(OutputBasel3Cap['Plus: Allowance for Loan & Lease Losses',oneten])
              n <- as.numeric(OutputCapBasel1['Pl Qual subord debt & Redeem Pref Stock',oneten])
              o <- as.numeric(OutputCapBasel1['Pl Total Minority Interest not included in Tier 1',oneten])
              p <- as.numeric(OutputCapBasel1['Pl Unreal gains on AFS equity securities',oneten])
              q <- as.numeric(OutputCapBasel1['Ls Other deductions (additions) to Tier 2',oneten])
              
              OutputBasel3Cap['Basel III Total Risk-Based Capital',oneten] <- k+l+m+(n+o+p+-q)
              
              k <- as.numeric(OutputBasel3Cap['Total Risk Weighted Assets',oneten])
              l <- as.numeric(OutputBasel3Cap['Phase-in Amount (%)',oneten])
              m <- as.numeric(OutputBasel3Cap['High Volatility CRE',oneten])
              n <- as.numeric(OutputBasel3Cap['90+ Day Past Due Loans',oneten])
              o <- as.numeric(OutputBasel3Cap['DTAs, MSRs and Equity Investments below thresholds',oneten])
              p <- as.numeric(OutputBasel3Cap['Unfunded Commitments <1 Year',oneten])
              q <- as.numeric(OutputBasel3Cap['Repurchase Agreements',oneten])
              
              OutputBasel3Cap['Basel III Total Gross Risk Weighted Assets',oneten] <- k+(m+n+o+p+q)*l
              
              k <- -as.numeric(BalanceSheetOutput['Allowance for Loan and Lease Losses',oneten])
              l <- as.numeric(OutputBasel3Cap['Basel III Total Gross Risk Weighted Assets',oneten])
              m <- l*0.0125
              n <- min(k,m)
              OutputBasel3Cap['Less: Excess Allowance for Loan & Lease Losses',oneten] <- n
              
              
              k <- as.numeric(OutputBasel3Cap['Basel III Total Adjusted Risk Weighted Assets',oneten])
              l <- as.numeric(OutputBasel3Cap['Less: Excess Allowance for Loan & Lease Losses',oneten])
              
              OutputBasel3Cap['Basel III Total Adjusted Risk Weighted Assets',oneten] <- k+l
              
              
              k <- as.numeric(OutputBasel3Cap['Basel III Tier 1 Common Equity ("CET1")',oneten])
              l <- as.numeric(OutputBasel3Cap['Basel III Tier 1 Equity',oneten])
              m <- as.numeric(OutputBasel3Cap['Basel III Total Risk-Based Capital',oneten])
              n <- as.numeric(OutputBasel3Cap['Basel III Total Gross Risk Weighted Assets',oneten]) 
              o <- as.numeric(OutputBasel3Cap['Basel III Total Adjusted Risk Weighted Assets',oneten])
              p <- as.numeric(OutputCapBasel1['Total Assets for Leverage Ratio',oneten])
              
              OutputBasel3Cap['Basel III Tier 1 Common Equity Capital Ratio',oneten] <- k/o
              OutputBasel3Cap['Basel III Tier 1 Equity Capital Ratio',oneten] <- l/o
              OutputBasel3Cap['Basel III Total Risk Based Capital Ratio',oneten] <- m/o
              OutputBasel3Cap['Basel III Tier 1 Leverage Ratio',oneten] <- l/p
                }            
              }
            }
          }
        }
      }
    }
  }
}            

remove(a,b,c,d,m,o,p,q,r,u)
# End - Basel III --------------------

# Begin - Sheet Formatting ------------------
# BalanceSheetOutput<- round(BalanceSheetOutput)
# IncomeStatementOutput<- round(IncomeStatementOutput)
# OutputSuppItems<- round(OutputSuppItems)
# OutputDTA<- round(OutputDTA)
# OutputParentCF<- round(OutputParentCF)
# OutputRWABasel1<- round(OutputRWABasel1)
# OutputCapBasel1<- round(OutputCapBasel1)
# OutputBasel3Cap<- round(OutputBasel3Cap)

# End - Sheet Formatting ------------------

