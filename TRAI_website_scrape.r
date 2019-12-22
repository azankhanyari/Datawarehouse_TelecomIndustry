library("rvest")
library("dplyr")

url <- "https://trai.gov.in/consumer-info/telecom/service-provider-list"
page <- read_html(url)
table <- html_table(page, fill = TRUE)

table

Telecoms <- table[[1]]  #extract relevant table

Telecom_df <- data.frame(Telecoms)    #parse into dataframe

#write.csv(Telecom_df, file = "Telecom_list.csv")

company_id<- as.character(c('Tata','Jio','Reliance','Telenor','MTNL','Vodafone','BSNL','Airtel','Aircel'))
Telecom_df <-  cbind(Telecom_df,company_id)

Telecom_df$Name.of.the.Service.provider <- NULL
Telecom_df$Link <- NULL
Telecom_df <- Telecom_df[,c(1,9,3,4,5,6,7,8)]
Telecom_df$Fax <-NULL


Videocon <- c(10,'Videocon','Mr. Sanjay Goel','SD Head - VP','Idea Cellular Limited
3rd Floor, Sunny Square, Gangyal, Jammu - 180010','9086046198','dftr@ideacellular.com')

MTS <- c(11,'MTS','Mr. Neera Sharma','Chief Operating Officer','Corp. Office : 334, Udyog Vihar, Phase-IV, Gurgaon - 122001','0124-8756745','rsharma@MTS.co.in')
str(Telecom_df$company_id)
Telecom_df$company_id <- as.character(Telecom_df$company_id)  
Telecom_df <- rbind(Telecom_df, Videocon)
Telecom_df <- rbind(Telecom_df, MTS)

#Telecom_df <- Telecom_df[-c(10),]

Telecom_df$company_id <- as.character(Telecom_df$company_id)

Telecom_df$country <- strrep('INDIA',times = 1)
Telecom_df$city <- c('Port Blair','Hyderabad','Itnagar','Dispur','Patna','Chandigarh','Daman','Srinagar','Ranchi','Kavaratti','Mumbai')
Telecom_df$state <- c('Andaman and Nicobar Islands','Andhra Pradesh','Arunachal Pradesh','Assam','Bihar','Chandigarh','Daman and Diu','Jammu and Kashmir','Jharkhand','Lakshadweep','Maharashtra')
Telecom_df$CIN <- c('L27100MH1907PLC000260','U72900MH2007PLC234712','L17110MH1973PLC019786','U74999DL2009PTC189153',' L32101DL1986GOI023501','U32200MH1992PLC119108','U74899DL2000GOI107739','L74899DL1995PLC070609','U74999MH1992PLC284457','L32100GJ1996PLC030976','U64201RJ1995PLC017779')

names(Telecom_df) <- c('company_id','company_name','name','designation','address','phone_number','email_id','country','city','state','CIN')
Telecom_df

phone <- Telecom_df$phone_number
phone_number <- c('023-12543556','011-43523795','011-30331781','011-30331781','012-5434567','011-23210134','23734097','0124-4243106','0124-4765100','9086046198','0124-8756745')
Telecom_df <- Telecom_df[,-6]
Telecom_df <- cbind(Telecom_df,phone_number)
emails <- Telecom_df$email_id
email_mk <- c("sunil[dot]tandon[at]tatatel[dot]co[dot]in","kapoor[dot]guliani[at]ril[dot]com","a[dot]mathur[at]relianceada[dot]com","a[dot]mathur[at]relianceada[dot]com","gmracomtnl[at]gmail[dot]com","Vibha[dot]munjal[at]vodafoneidea[dot]com","averma[at]bsnl[dot]co[dot]in","ravi[dot]gandhi[at]airtel[dot]com","iuht[dot]ghgti[at]ftl[dot]com","dftr@ideacellular.com","rsharma@MTS.co.in")
Telecom_df <- Telecom_df[,-6]
Telecom_df <- cbind(Telecom_df,email_mk)
Telecom_df <- Telecom_df[,c(1,2,3,4,5,10,11,6,7,8,9)]

connstring <- odbcDriverConnect("Driver=SQL Server;Server=DESKTOP-IRL6306\\MSBI_SERVER;Database=MasterData_Telecom;trusted_connection=true")
sqlSave(connstring,Telecom_df,tablename = "MasterData_Company_Info",rownames = F)
#write.csv(Regions_df, file = "States.csv")
#write.csv(Telecom_df, file = 'company_info.csv')
