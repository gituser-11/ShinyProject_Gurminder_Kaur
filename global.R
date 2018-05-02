
library(treemap)


library(maps)

library(data.table)

library(plotly)

library(mapdata)
library(wordcloud)
library(tm)
library(zipcode)
library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
#library(googleVis)
#library(ggmap)

library(DT)

library(shinydashboard)
library(shinythemes)
library(d3treeR)
#library(googleVis)
#source('help.R')


data(zipcode)

gnrl_pmnt = fread("general_payments.csv", stringsAsFactors = FALSE)

Total_Providers=length(unique(gnrl_pmnt$Physician_Profile_ID))
Total_Payments_Made=sum(gnrl_pmnt$Total_Amount_of_Payment_USDollars)/1000000
Total_Companies=length(unique(gnrl_pmnt$Submitting_Applicable_Manufacturer_or_Applicable_GPO_Name))
# colnames(gnrl_pmnt)[names(gnrl_pmnt)=="Recipient_Zip_Code"] <-"zip"
# 
# gnrl_pmnt$zip=clean.zipcodes(gnrl_pmnt$zip)
# 
# #reading zip db from usps #cleaned it outside to match names with inbuilt USA polygon GADM data
# zip_db=read.csv("zip_code_database.csv",stringsAsFactors = FALSE)
# zip_db$zip=clean.zipcodes(zip_db$zip)
# colnames(zip_db)[names(zip_db)=="county"] <-"subregion"
# 
# #fetching county
# gnrl_pmnt=left_join(gnrl_pmnt,zip_db,by="zip")
# 
# 
# county_pymnt=gnrl_pmnt %>% group_by(subregion)%>% summarise(Payments = sum(Total_Amount_of_Payment_USDollars))
# county_pymnt=filter(county_pymnt, subregion!="")
# county_pymnt$subregion=tolower(county_pymnt$subregion)
# 
# county_pymnt$subregion %<>%
#   gsub(" county", "", .) %>%
#   gsub(" parish", "", .) %>%
#   gsub(" ", "", .) %>%
#   gsub("[.]", "", .)




#state_payments
state_pymnt= gnrl_pmnt %>% group_by(Recipient_State) %>% summarise(Payments=sum(Total_Amount_of_Payment_USDollars),
                                                                   Provider_Cnt=length(unique(Physician_Profile_ID)))
state_pymnt=state_pymnt[order(-state_pymnt$Payments),]
state_pymnt=filter(state_pymnt,state_pymnt$Recipient_State!="")
state_pymnt$Pymnt_Mn=round(state_pymnt$Payments/1000000,2)
state_pymnt$Specialty_Final="All"


state_pymnt$hover <- with(state_pymnt, paste("mn USD Payments Made",'<br>',Recipient_State))


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
                                    state_spec_pymnt$Specialty_Final=="Gastroenterology",]

state_spec_pymnt= filter(state_spec_pymnt, Recipient_State!="")
state_spec_pymnt$Pymnt_Mn=round(state_spec_pymnt$Payments/1000000,2)
state_spec_pymnt$hover <- with(state_spec_pymnt, paste("mn USD Payments Made",'<br>',Recipient_State))
state_spec_pymnt=as.data.frame(state_spec_pymnt)
state_spec_pymnt=dplyr::select(state_spec_pymnt,"Recipient_State","Payments","Provider_Cnt", 
                               "Pymnt_Mn","Specialty_Final", "hover"
)

state_pymnt_final=as.data.frame(rbind(state_pymnt,state_spec_pymnt))
state_pymnt_final$Avg_Pymnt_HCP=round(state_pymnt_final$Payments/state_pymnt_final$Provider_Cnt,2)
state_pymnt_final$hover_avg <- with(state_pymnt_final, paste("USD Payment Per HCP",'<br>',Recipient_State))



g <- list(
  scope = 'usa',
  projection = list(type = 'choropleth'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)





#creating company level totals
Company_totals= gnrl_pmnt %>% group_by(Company=Submitting_Applicable_Manufacturer_or_Applicable_GPO_Name)%>%
  summarise(Payments=sum(Total_Amount_of_Payment_USDollars),
            Provider_Cnt=length(unique(Physician_Profile_ID)))

Company_totals$Specialty_Final = "All"

Company_totals = select(Company_totals, Company, Specialty_Final, Payments, Provider_Cnt)
Company_totals= as.data.frame(Company_totals)

Company_Pymnts = gnrl_pmnt %>% group_by(Company=Submitting_Applicable_Manufacturer_or_Applicable_GPO_Name, Specialty_Final)%>%
                       summarise(Payments=sum(Total_Amount_of_Payment_USDollars),
                                 Provider_Cnt=length(unique(Physician_Profile_ID)))

Company_Pymnts=as.data.frame(Company_Pymnts)

Company_final = as.data.frame(rbind(Company_totals, Company_Pymnts))

Company_final$PaymentPerProvider = Company_final$Payments/Company_final$Provider_Cnt
Company_final=subset(Company_final, Company_final$Specialty_Final!="")

Company_final = Company_final %>% group_by(Specialty_Final) %>% top_n(n=20, wt=Payments)



Company_final=filter(Company_final, Specialty_Final=="Orthopaedics"|
                       Specialty_Final=="Surgery"|
                       Specialty_Final=="Psychiatry & Neurology"|
                        Specialty_Final=="Cardiology"|
                       Specialty_Final=="Endocrinology, Diabetes & Metabolism"|
                       Specialty_Final=="Oncology"|
                       Specialty_Final=="Gastroenterology"|
                     Specialty_Final=="All")



#creating nature of pymnt totals for table
Nature_Pymnts_Table=gnrl_pmnt %>% group_by(Nature_of_pymnt) %>% summarise(Payments=sum(Total_Amount_of_Payment_USDollars))

#creating word cloud

# Nature_Pymnts_Table$Nature_of_pymnt = ifelse(Nature_Pymnts_Table$Nature_of_pymnt ==
#   "Charitable Contribution","Charity",
#     ifelse(Nature_Pymnts_Table$Nature_of_pymnt ==
#            "Consulting Fee","Consulting",
#          ifelse(Nature_Pymnts_Table$Nature_of_pymnt ==
#                   "Faculty - Continuing education program","Faculty",
#                 ifelse(Nature_Pymnts_Table$Nature_of_pymnt ==
#                          "Food and Beverage","Food",
#                        ifelse(Nature_Pymnts_Table$Nature_of_pymnt=="Ownership/ investment interest","Investment",
#                               ifelse(Nature_Pymnts_Table$Nature_of_pymnt=="Royalty or License","Royalty",
#                                      ifelse(Nature_Pymnts_Table$Nature_of_pymnt=="Food and Beverage","Food",
#                                             ifelse(Nature_Pymnts_Table$Nature_of_pymnt=="Royalty or License","Royalty",
#                                                    ifelse(Nature_Pymnts_Table$Nature_of_pymnt=="Speaker events","Speaker",
#                                                           ifelse(Nature_Pymnts_Table$Nature_of_pymnt=="Travel and Lodging","Travel",
#                                                                  Nature_Pymnts_Table$Nature_of_pymnt))))))))))

# nature_pymnt_word=read.csv("nature_pymnt_word.csv",stringsAsFactors = FALSE)
# nature_pymnt_word.Corpus=Corpus(VectorSource(nature_pymnt_word$Nature_of_pymnt))
# nature_pymnt_word.Clean<-tm_map(nature_pymnt_word.Corpus, PlainTextDocument)
# nature_pymnt_word.Clean<-tm_map(nature_pymnt_word.Corpus,tolower)



#creating drug level totals
Drug1 <- subset(gnrl_pmnt, Name_of_Associated_Covered_Drug_or_Biological1!="",
                select=c(Name_of_Associated_Covered_Drug_or_Biological1,
                         Total_Amount_of_Payment_USDollars))

Drug1 <- Drug1 %>% group_by(Product=Name_of_Associated_Covered_Drug_or_Biological1) %>%
  summarise(Payments=sum(Total_Amount_of_Payment_USDollars))

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
                               Total_Amount_of_Payment_USDollars))

Med_Supply1 <- Med_Supply1 %>% group_by(Product=Name_of_Associated_Covered_Device_or_Medical_Supply1)%>%
  summarise(Payments=sum(Total_Amount_of_Payment_USDollars))


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


Drug_Pymnt=rbind(Drug1,Drug2,Drug3,Drug4,Drug5)
Med_Supply_Pymnt=rbind(Med_Supply1,Med_Supply2,Med_Supply3,Med_Supply4,Med_Supply5)

Drug_Pymnt$Drug= toupper(Drug_Pymnt$Product)
Med_Supply_Pymnt$Med_Supply= toupper(Med_Supply_Pymnt$Product)

Drug_Pymnt <- Drug_Pymnt %>% group_by(Product) %>%
  summarise(Payments=sum(Payments))

Drug_Pymnt=top_n(Drug_Pymnt,10)
arrange(Drug_Pymnt,desc(Payments))

Drug_Pymnt$Product_type="Drugs"


Med_Supply_Pymnt <- Med_Supply_Pymnt %>% group_by(Product) %>%
  summarise(Payments=sum(Payments))

Med_Supply_Pymnt=top_n(Med_Supply_Pymnt,10)

Med_Supply_Pymnt$Product_type="Medical Supplies"

Products_final=rbind(Drug_Pymnt,Med_Supply_Pymnt)

Products_final$Product=toupper(Products_final$Product)

#creating barplots for top drugs and medical supplies

plot_ly(x = Products_final$Payments, y = Products_final$Product, type = 'bar', 
        orientation = 'h',marker = list(color = 'green'))


