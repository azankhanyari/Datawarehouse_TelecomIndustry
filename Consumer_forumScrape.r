library(rvest)
url <- "http://www.mouthshut.com/mobile-operators"
mouthshut_html <- read_html(url)

rating_list <- mouthshut_html %>%
  html_nodes("span.avg-star")%>%
  html_text() %>%                       
  gsub("^\\s+|\\s+$", "", .)   

operator_list <- mouthshut_html %>%
  html_nodes("div.rtitle")%>%
  html_text() %>%                       
  gsub("^\\s+|\\s+$", "", .) 

review_list <- mouthshut_html %>%
  html_nodes("a.small")%>%
  html_text() %>%                       
  gsub("^\\s+|\\s+$", "", .) 

clean_review <- gsub('Reviews', "",review_list)

telcom_df <- data.frame(operator_list,rating_list,clean_review)
write.csv(telcom_df, "C:/DWBI_telco/Twitter & Mouthshut/Mouthshut_Telco.csv",row.names = TRUE)