
#all libraries for shinydashboard
library(treemap)
library(maps)
library(data.table)
library(plotly)
library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(DT)
library(shinydashboard)
library(shinythemes)


gnrl_pmnt = fread("general_payments.csv", stringsAsFactors = FALSE)

Total_Providers=length(unique(gnrl_pmnt$Physician_Profile_ID))
Total_Payments_Made=sum(gnrl_pmnt$Total_Amount_of_Payment_USDollars)/1000000
Total_Companies=length(unique(gnrl_pmnt$Submitting_Applicable_Manufacturer_or_Applicable_GPO_Name))


#state level payments
state_pymnt= gnrl_pmnt %>% group_by(Recipient_State) %>% summarise(Payments=sum(Total_Amount_of_Payment_USDollars),
                                                                   Provider_Cnt=length(unique(Physician_Profile_ID)))

state_pymnt=filter(state_pymnt,state_pymnt$Recipient_State!="") #omitting records with no state information

state_pymnt$Pymnt_Mn=round(state_pymnt$Payments/1000000,2)  #converting to millions

state_pymnt$Specialty_Final="All" #creating specialty column for totals at state level

state_pymnt$hover <- with(state_pymnt, paste("mn USD Payments Made",'<br>',Recipient_State)) #creating hover details


#state split by specialty
state_spec_pymnt = gnrl_pmnt%>% group_by(Specialty_Final, Recipient_State)%>%
  summarise(Payments=sum(Total_Amount_of_Payment_USDollars),
            Provider_Cnt=length(unique(Physician_Profile_ID)))

state_spec_pymnt=state_spec_pymnt[state_spec_pymnt$Specialty_Final=="Orthopaedics"|
                                    state_spec_pymnt$Specialty_Final=="Surgery"|
                                    state_spec_pymnt$Specialty_Final=="Psychiatry & Neurology"|
                                    state_spec_pymnt$Specialty_Final=="Cardiology"|
                                    state_spec_pymnt$Specialty_Final=="Endocrinology, Diabetes & Metabolism"|
                                    state_spec_pymnt$Specialty_Final=="Oncology"|
                                    state_spec_pymnt$Specialty_Final=="Gastroenterology",]  #filtering for some of the common therapy areas

state_spec_pymnt= filter(state_spec_pymnt, Recipient_State!="") #omitting records with no state information

state_spec_pymnt$Pymnt_Mn=round(state_spec_pymnt$Payments/1000000,2) #into millions

state_spec_pymnt$hover <- with(state_spec_pymnt, paste("mn USD Payments Made",'<br>',Recipient_State)) #hover details for map

state_spec_pymnt=as.data.frame(state_spec_pymnt)

state_spec_pymnt=dplyr::select(state_spec_pymnt,"Recipient_State","Payments","Provider_Cnt", 
                               "Pymnt_Mn","Specialty_Final", "hover"
) #using select to order columns for matching sequence to another file for rbind 



#creating a complete dataset of state totals and specialty splits
state_pymnt_final=as.data.frame(rbind(state_pymnt,state_spec_pymnt)) 

state_pymnt_final$Avg_Pymnt_HCP=round(state_pymnt_final$Payments/state_pymnt_final$Provider_Cnt,2) #avg payments made to HCP

state_pymnt_final$hover_avg <- with(state_pymnt_final, paste("USD Payment Per HCP",'<br>',Recipient_State)) #creating hover details for avg per HCP map



#creating company level totals
Company_totals= gnrl_pmnt %>% group_by(Company=Submitting_Applicable_Manufacturer_or_Applicable_GPO_Name)%>%
  summarise(Payments=sum(Total_Amount_of_Payment_USDollars),
            Provider_Cnt=length(unique(Physician_Profile_ID)))

Company_totals$Specialty_Final = "All" #adding column for rbinding with specialty split by company

Company_totals = select(Company_totals, Company, Specialty_Final, Payments, Provider_Cnt) #reordering columns to rbind

Company_totals= as.data.frame(Company_totals)


#company totals by specialty
Company_Pymnts = gnrl_pmnt %>% group_by(Company=Submitting_Applicable_Manufacturer_or_Applicable_GPO_Name, Specialty_Final)%>%
                       summarise(Payments=sum(Total_Amount_of_Payment_USDollars),
                                 Provider_Cnt=length(unique(Physician_Profile_ID)))

Company_Pymnts=as.data.frame(Company_Pymnts)


#creating a consolidated dataset for company info

Company_final = as.data.frame(rbind(Company_totals, Company_Pymnts)) 

Company_final$PaymentPerProvider = Company_final$Payments/Company_final$Provider_Cnt

Company_final=filter(Company_final, Specialty_Final=="Orthopaedics"|
                       Specialty_Final=="Surgery"|
                       Specialty_Final=="Psychiatry & Neurology"|
                       Specialty_Final=="Cardiology"|
                       Specialty_Final=="Endocrinology, Diabetes & Metabolism"|
                       Specialty_Final=="Oncology"|
                       Specialty_Final=="Gastroenterology"|
                       Specialty_Final=="All")  #filtering for common specialties and total

Company_final = Company_final %>% group_by(Specialty_Final) %>% top_n(n=20, wt=Payments) #top 20 companies by each specialty


#creating drug level totals for drugs in each column
Drug1 <- subset(gnrl_pmnt, Name_of_Associated_Covered_Drug_or_Biological1!="",
                select=c(Name_of_Associated_Covered_Drug_or_Biological1,
                         Total_Amount_of_Payment_USDollars)) #filter out any records with no drug1 and select only dug name
                                                            # and payment to avoid large sized dataset. Repeat for all drug columns

Drug1 <- Drug1 %>% group_by(Product=Name_of_Associated_Covered_Drug_or_Biological1) %>%
  summarise(Payments=sum(Total_Amount_of_Payment_USDollars))  #total payment for each drug1 and repeat below for all drug columns

Drug2 <- subset(gnrl_pmnt, Name_of_Associated_Covered_Drug_or_Biological2!="",
                select=c(Name_of_Associated_Covered_Drug_or_Biological2,
                         Total_Amount_of_Payment_USDollars)) 

Drug2 <- Drug2 %>% group_by(Product=Name_of_Associated_Covered_Drug_or_Biological2) %>%
  summarise(Payments=sum(Total_Amount_of_Payment_USDollars))


Drug3 <- subset(gnrl_pmnt, Name_of_Associated_Covered_Drug_or_Biological3!="",
                select=c(Name_of_Associated_Covered_Drug_or_Biological3,
                         Total_Amount_of_Payment_USDollars))

Drug3 <- Drug3 %>% group_by(Product=Name_of_Associated_Covered_Drug_or_Biological3) %>%
  summarise(Payments=sum(Total_Amount_of_Payment_USDollars))

Drug4 <- subset(gnrl_pmnt, Name_of_Associated_Covered_Drug_or_Biological4!="",
                select=c(Name_of_Associated_Covered_Drug_or_Biological4,
                         Total_Amount_of_Payment_USDollars))

Drug4 <- Drug4 %>% group_by(Product=Name_of_Associated_Covered_Drug_or_Biological4) %>%
  summarise(Payments=sum(Total_Amount_of_Payment_USDollars))

Drug5 <- subset(gnrl_pmnt, Name_of_Associated_Covered_Drug_or_Biological5!="",
                select=c(Name_of_Associated_Covered_Drug_or_Biological5,
                         Total_Amount_of_Payment_USDollars))

Drug5 <- Drug5 %>% group_by(Product=Name_of_Associated_Covered_Drug_or_Biological5) %>%
  summarise(Payments=sum(Total_Amount_of_Payment_USDollars))


Med_Supply1 <- subset(gnrl_pmnt, Name_of_Associated_Covered_Device_or_Medical_Supply1!="",
                      select=c(Name_of_Associated_Covered_Device_or_Medical_Supply1,
                               Total_Amount_of_Payment_USDollars))  #filter out any records with no drug1 and select only dug name
                                                                    # and payment to avoid large sized dataset. Repeat for all drug columns

Med_Supply1 <- Med_Supply1 %>% group_by(Product=Name_of_Associated_Covered_Device_or_Medical_Supply1)%>%
  summarise(Payments=sum(Total_Amount_of_Payment_USDollars))    #total payment for each drug1 and repeat below for all drug columns


Med_Supply2 <- subset(gnrl_pmnt, Name_of_Associated_Covered_Device_or_Medical_Supply2!="",
                      select=c(Name_of_Associated_Covered_Device_or_Medical_Supply2,
                               Total_Amount_of_Payment_USDollars))

Med_Supply2 <- Med_Supply2%>%group_by(Product=Name_of_Associated_Covered_Device_or_Medical_Supply2)%>%
  summarise(Payments=sum(Total_Amount_of_Payment_USDollars))


Med_Supply3 <- subset(gnrl_pmnt, Name_of_Associated_Covered_Device_or_Medical_Supply3!="",
                      select=c(Name_of_Associated_Covered_Device_or_Medical_Supply3,
                               Total_Amount_of_Payment_USDollars))

Med_Supply3 <- Med_Supply3%>%group_by(Product=Name_of_Associated_Covered_Device_or_Medical_Supply3)%>%
  summarise(Payments=sum(Total_Amount_of_Payment_USDollars))


Med_Supply4 <- subset(gnrl_pmnt, Name_of_Associated_Covered_Device_or_Medical_Supply4!="",
                      select=c(Name_of_Associated_Covered_Device_or_Medical_Supply4,
                               Total_Amount_of_Payment_USDollars))

Med_Supply4 <- Med_Supply4%>%group_by(Product=Name_of_Associated_Covered_Device_or_Medical_Supply4)%>%
  summarise(Payments=sum(Total_Amount_of_Payment_USDollars))


Med_Supply5 <- subset(gnrl_pmnt, Name_of_Associated_Covered_Device_or_Medical_Supply5!="",
                      select=c(Name_of_Associated_Covered_Device_or_Medical_Supply5,
                               Total_Amount_of_Payment_USDollars))

Med_Supply5 <- Med_Supply5%>%group_by(Product=Name_of_Associated_Covered_Device_or_Medical_Supply5)%>%
  summarise(Payments=sum(Total_Amount_of_Payment_USDollars))


Drug_Pymnt=rbind(Drug1,Drug2,Drug3,Drug4,Drug5) #combining all drug datasets
Med_Supply_Pymnt=rbind(Med_Supply1,Med_Supply2,Med_Supply3,Med_Supply4,Med_Supply5) #consolidating medical supply dataset


Drug_Pymnt <- Drug_Pymnt %>% group_by(Product) %>%
  summarise(Payments=sum(Payments))     #position does not matter and hence adding payments for drugs from across all drug datasets created above

Drug_Pymnt=top_n(Drug_Pymnt,10) #filter top 10 drugs


Drug_Pymnt$Product_type="Drugs" #add an identification column before merging with medical supplies data


Med_Supply_Pymnt <- Med_Supply_Pymnt %>% group_by(Product) %>%
  summarise(Payments=sum(Payments))

Med_Supply_Pymnt=top_n(Med_Supply_Pymnt,10)

Med_Supply_Pymnt$Product_type="Medical Supplies"


#consolidated procuct dataset
Products_final=rbind(Drug_Pymnt,Med_Supply_Pymnt)

Products_final$Product=toupper(Products_final$Product)

