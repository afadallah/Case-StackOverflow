getExternalData <-function(){
  
  ################################################################################################
  ### Loading Packages
  ################################################################################################
  library(stringr)
  library(readr)
  library(tidyr)
  library(dplyr)
  library(magrittr)
  library(countrycode)
  ### Getting data in packages
  library(rvest)
  library(fuzzyjoin)
  
  ################################################################################################
  ### Extracting Data
  ################################################################################################
  print("Phase One of data extraction has begun")
  base = "https://nl.wikipedia.org"
  LijstStedenDorpen = read_html(str_c(base,"/wiki/Lijst_van_Nederlandse_plaatsen"))
  plaatsen = LijstStedenDorpen %>% html_nodes("p > a") %>% html_text
  print("----->Phase One is completed. We now have all the names of existing cities and villages in the netherlands")
  
  print("Phase Two of data extraction has begun")
  links = LijstStedenDorpen  %>% html_nodes("p > a") %>% html_attr("href")
  print("----->Phase Two is completed. We now have all the links of the wikipedia pages of all the Dutch cities and villages")
  
  n=length(links)
  print("Phase Three of data extraction has begun")
  PagePiece = matrix(nrow = n, ncol = 7)
  cnt = 0
  for (link in links[1:n]){
    cnt = cnt +1
    pagina = str_c(base,link) %>% read_html()
    piece = pagina %>% html_nodes("tr:nth-child(5) td , tr:nth-child(6) td , tr:nth-child(7) td , tr:nth-child(8) td") %>% html_text(trim = TRUE)
    PagePiece[cnt,1:7] = piece[1:7]
  }
  PagePiece[is.na(PagePiece)]<-"" 
  print("----->Phase Three is completed. We now have the interesting content piece of the wikipedia pages")
  
  ################################################################################################
  ### Cleaning RAW Data
  ################################################################################################
  print("The First Phase of data cleaning has begun")
  nrrows = nrow(PagePiece)
  
  Gemeente = rep("NA",nrrows)
  Provincie = rep("NA",nrrows)
  Coordinaten = rep("NA",nrrows)
  
  next_value_is_gemeente = FALSE
  next_value_is_provincie = FALSE
  next_value_is_coordinaten = FALSE
  
  for (row in seq(nrrows)){
    rowsubset = PagePiece[row,]
#     print("====================================================================================================")
#     print("====================================================================================================")
    print(row)
#     print("----------------------------------------------------------------------------------------------------")
    ncols=length(rowsubset)
    for (col in seq(ncols)){
      #   print(class(element))
      element = rowsubset[[col]]
      skip=FALSE
      if (element =="Provincie" || element =="provincie"){
        next_value_is_provincie = TRUE
        skip=TRUE
      } else if (element =="Gemeente" || element =="gemeente"){
        next_value_is_gemeente = TRUE
        skip=TRUE
      } else if (element =="Coördinaten" || element =="Coordinaten" || element =="coordinaten"){
        next_value_is_coordinaten = TRUE
        skip=TRUE
      }
      
      if (skip == FALSE){
        if (next_value_is_provincie == TRUE){
          Provincie[row] = element
          next_value_is_provincie = FALSE
          print("provincie gevonden")
        } else if (next_value_is_gemeente == TRUE){
          Gemeente[row] = element
          next_value_is_gemeente = FALSE
          print("gemeente gevonden")
        } else if (next_value_is_coordinaten == TRUE){
          Coordinaten[row] = element
          next_value_is_coordinaten = FALSE
          print("coordinaten gevonden")
        }
      }
    }
  }
  # print(Provincie)
  # print(Gemeente)
  # print(Coordinaten)
  # 
  
  Provincie = Provincie %>% str_replace_all("Fryslân","Friesland") %>% str_replace_all("Drenthe, Groningen","Drenthe") %>%
    str_replace_all("Drenthe Groningen","Drenthe") %>% str_replace_all("Drenthe, Overijssel","Drenthe") %>% 
    str_replace_all("Friesland (Fryslân)","Friesland") %>% str_replace_all("provincie","") %>% str_replace_all("\\(","") %>% str_replace_all("\\)","") %>%
    str_replace_all("Limburg/ Gelderland","Limburg") %>% str_replace_all("Noord-Brabant en Antwerpen","Noord-Brabant") %>%
    str_replace_all("Antwerpen","Noord-Brabant") %>% str_replace_all("Utrecht /  Zuid-Holland","Utrecht") %>%
    str_replace_all("Zuid-Holland,  Gelderland en  Utrecht","Gelderland")  %>% str_trim()

  
  df = data.frame(Provincien=as.factor(Provincie),Gemeenten=as.factor(Gemeente),Plaatsen=as.character(plaatsen),Coordinaten=as.character(Coordinaten))
  
  
  ################################################################################################
  ### Detecting, Localizing and Correcting Errors in Data
  ################################################################################################
  
  # check this later on!
  
  return(df)
}

getMultinationals <-function(){
  ################################################################################################
  ### Loading Packages
  ################################################################################################
  library(stringr)
  library(readr)
  library(tidyr)
  library(dplyr)
  library(magrittr)
  library(countrycode)
  ### Getting data in packages
  library(rvest)

  ################################################################################################
  ### Extracting Data
  ################################################################################################
  print("Phase One of data extraction has begun")
  base = "https://nl.wikipedia.org"
  pagina = read_html(str_c(base,"/wiki/Categorie:Nederlandse_multinational"))
  Multinationals = pagina %>% html_nodes("#mw-pages a") %>% html_text
  print("----->Phase One is completed. We now have all the names of all the multinationals in the Netherlands")
  
  
  pagina = read_html("https://www2.computable.nl/apps/100.php")
  top100bedrijven = pagina %>% html_nodes("nobr .txt-link") %>% html_text
  print("----->Phase Two is completed. We now have all the names of Top 100 IT companies in the Netherlands according to Computable")
  
  bedrijven = c(top100bedrijven,Multinationals) %>% sort()  %>% as.data.frame()   %>% distinct() 
  bedrijven = bedrijven$. %>% str_replace_all("\\(","") %>% str_replace_all("\\)","") %>% str_replace_all("bedrijf","") %>% str_trim()
#   %>% select(bedrijf=.)
  BedrvLoc = data.frame(TopITBedrijven=bedrijven,Nederland=" Nederland")
  TopITBedrijven = str_c(BedrvLoc$TopITBedrijven, BedrvLoc$Nederland)
  
  
  return(TopITBedrijven)
  
}






getProgLangApplications <-function(){
  ################################################################################################
  ### Loading Packages
  ################################################################################################
  library(stringr)
  library(readr)
  library(tidyr)
  library(dplyr)
  library(magrittr)
  library(countrycode)
  ### Getting data in packages
  library(rvest)
  library(XML)

  ################################################################################################
  ### Extracting Data
  ################################################################################################
  print("Phase One of data extraction has begun")
  base = "https://en.wikipedia.org"
  setwd("D:/data/AFA21296/Desktop")
  #download.file(str_c(base,"/wiki/Comparison_of_programming_languages"),destfile = "Comparison of programming languages.html")
  pagina = read_html("Comparison of programming languages - Wikipedia, the free encyclopedia.html")
  programming_lang = pagina %>% html_nodes(".jquery-tablesorter:nth-child(12) tbody th a") %>% html_text()
  programming_application = pagina %>% html_nodes(".jquery-tablesorter:nth-child(12) th+ td") %>% html_text()
  print("----->Phase One is completed. We now have all the names of existing cities and villages in the netherlands")
  
  
  prog = data.frame(Prog_Language=programming_lang,Prog_Application = programming_application)  %>% arrange(programming_lang) 
  
  Application = programming_application %>% str_replace_all("includes vba","") %>% str_c(collapse = ", ")  %>%  tolower() %>% str_replace_all(", , ",", ") %>% str_replace_all("\\[11\\] "," ") %>% 
    str_replace_all("web application","web applications") %>% str_replace_all("distribution","distribut") %>% str_replace_all("distributed","distribut")%>% str_replace_all("embedded scripting","embedded") %>%
    str_replace_all("games","game") %>% str_replace_all("general purpose","general")  %>% 
    str_replace_all("mobile development","mobile") %>% str_replace_all("mobile app","mobile") %>% 
    str_replace_all("symbolic computation","symbolic comput") %>% str_replace_all("symbolic computing","symbolic comput")   %>% 
    str_replace_all("\\(","") %>% str_replace_all("\\)","") %>% str_replace("web ewf","web") %>%  str_replace_all("web applications","web") %>% str_replace_all("webs","web") %>%str_split(", ") 
  
  Application = Application[[1]] %>%unique() %>% sort() 
  
  
  return(list(Application, prog))
  }

