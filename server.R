#### INI libraries ####
library(openxlsx)
library(flextable)
library(readxl)
library(pryr)
require(roperators)
library(readxl)
#### END libraries ####

# By default the file size limit is 5MB. Here limit is 70MB.
options(shiny.maxRequestSize = 70*1024^2)
memory.size(max = FALSE)
Sys.setlocale("LC_ALL", "Russian")

shinyServer(function(input, output, session){

  #reactive variable for temperature/salinity boolean option 
  TSflag <- reactiveVal("T") #T is temperature
  observe({
    if (input$norm_charactRB == "температура") isolate(TSflag("T"))
    else isolate(TSflag("S"))
  })
  
  #expedition list to store additional data loaded by user
  #text file stores previously loaded filenames with data
  #to be used to recalculate norms
  norm_explist <- reactiveVal(
    readLines("./www/explist.txt", encoding="UTF-8")
  )
  
  dimM <- 12 # number of months
  st.levels <- seq(69.5,78.0,by = 0.5) 
  dimS <- length(st.levels) #18 - number of stations along section
  dep.levels <- seq.int(0,345,by = 5)
  dimD <- length(dep.levels) #70 - number of depth levels (horizons)
  dimT <- dimD*dimS*dimM #total dim of 1D array

#### 2 INI NORM Export to Excel ####
 
  normdownloader <- function(file,wodonly)
  {
    startyear <- as.integer(input$norm_year_fromSI)
    stopyear <- as.integer(input$norm_year_toSI)
       
    shinyjs::toggle(id = "norm_exportP_Panel", anim = T,animType = "slide", time = 0.2)
    updateProgressBar(session = session, id = "norm_exportP", value = 0)
    
    
    vallist <- NULL 
    yearlist <- NULL
    
    #Open files
    #8% of progress (8% total) - started
    #8 is value without concrete sense
    if(TSflag() == "T"){
      if(wodonly){
        vallist <- readRDS("./www/t_WODT0.2_vallist.rds")
        yearlist <- readRDS("./www/t_WODT0.2_yearlist.rds")
      }
      else{
        vallist <- readRDS("./www/t_total_vallist.rds")
        yearlist <- readRDS("./www/t_total_yearlist.rds")
      }
    }
    else{
      if(wodonly){
        vallist <- readRDS("./www/s_WODT0.2_vallist.rds")
        yearlist <- readRDS("./www/s_WODT0.2_yearlist.rds")
      }
      else{
        vallist <- readRDS("./www/s_total_vallist.rds")
        yearlist <- readRDS("./www/s_total_yearlist.rds")
      }
    }
    updateProgressBar(session = session, id = "norm_exportP", value = 8)
    #8% of progress (8% total) - finished
    
    normvec = vector("integer",length = dimT)
    normcountvec = vector("integer",length = dimT)
    
    #88% of progress (88%) - started
    frstep = as.integer(dimT/16)
    for (i in 1:dimT){
      liye <- yearlist[[i]]
      liva <- vallist[[i]]
      if(length(liye) == 0) {
        normvec[i] <- NaN
        normcountvec[i] <- NaN
        next
      }
      liva <- liva[liye >= startyear & liye <= stopyear]
      liye <- liye[liye >= startyear & liye <= stopyear]
      yearlist[[i]] <- liye
      vallist[[i]] <- liva
      
      if(length(vallist[[i]]) == 0) normvec[i] <- NaN
      else normvec[i] <- mean(vallist[[i]])
      
      if(length(vallist[[i]]) == 0) normcountvec[i] <- NaN
      else normcountvec[i] <- length(vallist[[i]])
      
      if((i %% frstep) == 0) updateProgressBar(session = session, id = "norm_exportP", value = 8 + 5*(i/frstep))
    }
    #88% of progress (88%) - finished 
    
    val1 <- as.integer(100*object_size(vallist) / val1)
    shinyalert("Получен результат", 
               paste0(" С учётом выбранных лет использовано ",val1,"% данных."), type = "info")
    wb <- createWorkbook()
    
    #12% of progress (100%) - started
    for(m in 1:dimM){
      addWorksheet(wb, sheetName = paste0("",m))
      matr <- matrix(normvec[(dimD*dimS*(m-1)+1):(dimD*dimS*m)],ncol=dimS,byrow=TRUE)
      mode(matr) <- "numeric"
      matr %/=% 100000.0
      matr <- round(matr, digits = 5)
      matr[is.nan(matr)] = NA  #in.na - for data.frame
      writeData(wb, sheet = m, x = matr,
                startCol = 2, startRow = 3, borders = "columns")
      writeData(wb, sheet = m, x = dep.levels,
                startCol = 1, startRow = 4, borders = "rows")
      s <- createStyle(numFmt = "0.00000",border = "LeftRight")
      addStyle(wb, m, style = s, cols =2:(ncol(matr)+5), rows=1:(nrow(matr)+5), gridExpand = TRUE)
      
    }
    
    for(m in 13:(dimM+12)){
      addWorksheet(wb, sheetName = paste0("Num ",as.character(m-12)))
      matr <- matrix(normcountvec[(dimD*dimS*(m-13)+1):(dimD*dimS*(m-12))],ncol=dimS,byrow=TRUE)
      # mode(matr) <- "numeric"
      # matr <- round(matr, digits = 0)
      matr[is.nan(matr)] = NA  #in.na - for data.frame
      writeData(wb, sheet = paste0("Num ",as.character(m-12)), x = matr,
                startCol = 2, startRow = 3, borders = "columns")
      writeData(wb, sheet = paste0("Num ",as.character(m-12)), x = dep.levels,
                startCol = 1, startRow = 4, borders = "rows")
      s <- createStyle(numFmt = "#",border = "LeftRight")
      addStyle(wb, paste0("Num ",as.character(m-12)), style = s, cols =2:(ncol(matr)+5), rows=1:(nrow(matr)+5), gridExpand = TRUE)
      updateProgressBar(session = session, id = "norm_exportP", value = 88 + m-12)
    }
    saveWorkbook(wb, file)  
    #12% of progress (100%) - finished
    
    shinyjs::hide(id = "norm_exportP_Panel", anim = T,animType = "slide", time = 0.2)
  }
  
  
   output$norm_exportB <- downloadHandler(
    filename = paste0(TSflag(),"_WOD Norms ",input$norm_year_fromSI,"-",input$norm_year_toSI,".xlsx"),
    content = function(file) {
      normdownloader(file,TRUE)
    }
  )
  output$norm_exportallB <- downloadHandler(
    filename = paste0(TSflag(),"_Custom Norms ",input$norm_year_fromSI,"-",input$norm_year_toSI,".xlsx"),
    content = function(file) {
      normdownloader(file,FALSE)
    }
  )
   
#### 2 END NORM Export to Excel ####
  
#### 3 INI NORM Import to DB ####
  observe({
    if(is.null(input$norm_supplyFI)) return()
    
    #shinyalert("Дождитесь завершения!",paste0("vvfvfvv"), type = "info",html=TRUE)
    
    shinyjs::toggle(id = "norm_supplyP_Panel", anim = T,animType = "slide", time = 0.2)
    updateProgressBar(session = session, id = "norm_supplyP", value = 0)
    
    fd <- file("./www/explist.txt", open = "a")
    temp.st <- 0
    temp.lat <- 0.0
    temp.lon <- 0.0
    temp.date <- Sys.Date()
    temp.y <- as.numeric(format(temp.date,'%Y'))
    temp.m <- as.numeric(format(temp.date,'%m'))
    temp.d <- as.numeric(format(temp.date,'%d'))
    
    temp.str_s <- character(0)
    temp.str_t <- character(0)
    temp.t <- -1e10
    temp.s <- -1e10
    
    df_t <- data.frame(id=integer(0),lat=numeric(0),lon=numeric(0),year=integer(0),month=numeric(0),day=numeric(0),data=character(0))
    df_t$data <- as.character(df_t$data)
    df_s <- data.frame(id=integer(0),lat=numeric(0),lon=numeric(0),year=integer(0),month=numeric(0),day=numeric(0),data=character(0))
    df_s$data <- as.character(df_s$data)
   
    #prepare for warning list
    warnlistlen <- length(input$norm_supplyFI$name)
    warnlist <- vector("list",length = warnlistlen)
    for (i in 1:warnlistlen){
      warnlist[[i]] <- reactiveVal(vector(mode="character"))
      warnlist[[i]](c(isolate(warnlist[[i]]()), paste0("Файл \"",input$norm_supplyFI$name[[i]],"\":")))
    }
    
    #for each loaded .XLSX
    for(i in 1:length(input$norm_supplyFI$name)){
      tryCatch(withCallingHandlers({
      if(input$norm_supplyFI$name[[i]] %in% isolate(norm_explist())){
        shinyalert("Дублирование данных", 
                   paste0("Файл с именем \"", input$norm_supplyFI$name[[i]],
                          "\" игнорируется, так как уже присутствует в БД. Если требуется перезапись существующего файла, обратитесь к администратору!"), type = "warning")
        next
      } 
      #print(paste0("Started: ",input$norm_supplyFI$name[[i]]))
      file.copy(input$norm_supplyFI$datapath[[i]], paste0("./www/", input$norm_supplyFI$name[[i]]))
      writeLines(input$norm_supplyFI$name[[i]], fd, useBytes=T)
      norm_explist(isolate(c(norm_explist(),input$norm_supplyFI$name[[i]])))
      file.remove(input$norm_supplyFI$datapath[[i]])
      
      #open XLSX
      norm_loadedxlsx <- read_excel(paste0("./www/", input$norm_supplyFI$name[[i]]),col_names = TRUE,col_types=c("numeric","date",rep("numeric",each=6)))
      #print("Opened")
      #str(norm_loadedxlsx)

      temp.row <- 1
      norm_loadedxlsx <- norm_loadedxlsx[!(is.na(norm_loadedxlsx$station) & is.na(norm_loadedxlsx$date) 
                                           & is.na(norm_loadedxlsx$lat) & is.na(norm_loadedxlsx$lon) 
                                           & is.na(norm_loadedxlsx$depth) & is.na(norm_loadedxlsx$z)),]
      while(temp.row <= nrow(norm_loadedxlsx)){
        
        temp.st <- norm_loadedxlsx[temp.row,1]
        temp.lat <- norm_loadedxlsx[temp.row,3]
        temp.lon <- norm_loadedxlsx[temp.row,4]
        temp.date <- norm_loadedxlsx[temp.row,]$date

        temp.y <- as.numeric(format(temp.date,'%Y'))
        temp.m <- as.numeric(format(temp.date,'%m'))
       
        temp.str_s <-"("
        temp.str_t <-"("
        temp.str_s <- paste0(temp.str_s,norm_loadedxlsx[temp.row,6],"|",norm_loadedxlsx[temp.row,8],"|")
        temp.str_t <- paste0(temp.str_t,norm_loadedxlsx[temp.row,6],"|",norm_loadedxlsx[temp.row,7],"|")
       
        while(TRUE){
          temp.row %+=% 1
          if(temp.row > nrow(norm_loadedxlsx)) {break}
          if(!(is.na(norm_loadedxlsx[temp.row,1]) & is.na(norm_loadedxlsx[temp.row,2]) 
             & is.na(norm_loadedxlsx[temp.row,3]) & is.na(norm_loadedxlsx[temp.row,4]) 
             & is.na(norm_loadedxlsx[temp.row,5] ))){break}
          temp.str_t <- paste0(temp.str_t,norm_loadedxlsx[temp.row,6],"|",norm_loadedxlsx[temp.row,7],"|")
          temp.str_s <- paste0(temp.str_s,norm_loadedxlsx[temp.row,6],"|",norm_loadedxlsx[temp.row,8],"|")
        }
        temp.str_t <- paste0(temp.str_t,")")
        temp.str_s <- paste0(temp.str_s,")")
        df_t[nrow(df_t) + 1,] <- list(as.integer(paste0(temp.y,temp.m,temp.d,temp.st)),temp.lat,temp.lon,temp.y,temp.m,temp.d,temp.str_t)
        df_s[nrow(df_s) + 1,] <- list(as.integer(paste0(temp.y,temp.m,temp.d,temp.st)),temp.lat,temp.lon,temp.y,temp.m,temp.d,temp.str_s)
      }
    
    },warning = function(w) {
      warnlist[[i]](c(isolate(warnlist[[i]]()), paste0("Ошибка W: ",as.character(w))))
      },
    error = function(e)  {
      warnlist[[i]](c(isolate(warnlist[[i]]()), paste0("Ошибка W: ",as.character(e))))
      }))
    }
    
    for (i in 1:warnlistlen){
      if(length(isolate(warnlist[[i]]())) == 1) warnlist[[i]](c(isolate(warnlist[[i]]()), "Ошибок нет!"))
    }
    
    newlisttopr <- character(0)
    for (i in 1:warnlistlen){
      newlisttopr <- c(newlisttopr,unlist(isolate(warnlist[[i]]())))
    }
  
    shinyalert("Отчёт об импорте:<br>при наличии хотя бы одной ошибки весь импорт отменяется!",paste0(
      "<pre><div style=\"white-space: pre-wrap; white-space: -moz-pre-wrap; white-space: -pre-wrap; 
      white-space: -o-pre-wrap; width:400px;max-height:300px;overflow:auto;word-wrap: break-word;\">",
               paste(newlisttopr, collapse="\n"),"</div></pre>"), type = "info",html=TRUE)
 
    for (i in 1:warnlistlen){
      if(length(isolate(warnlist[[i]]())) > 2) {
        print("True error")
        return()
      }
    }
    
    df_s$lat <- as.numeric(df_s$lat)
    df_s$lon <- as.numeric(df_s$lon)
    
    df_t$lat <- as.numeric(df_t$lat)
    df_t$lon <- as.numeric(df_t$lon)
    #str(df)
    
    file.remove("./www/s_sessionLoadedXLSXs.rds")
    file.remove("./www/s_sessionLoadedXLSXs.csv")
    file.remove("./www/t_sessionLoadedXLSXs.rds")
    file.remove("./www/t_sessionLoadedXLSXs.csv")
    
    saveRDS(df_t, file = "./www/t_sessionLoadedXLSXs.rds")
    saveRDS(df_s, file = "./www/s_sessionLoadedXLSXs.rds")
    write.csv(df_s,"./www/s_sessionLoadedXLSXs.csv",row.names = FALSE,col.names = c('cast','lat','lon','year','month','day','data'))
    write.csv(df_t,"./www/t_sessionLoadedXLSXs.csv",row.names = FALSE,col.names = c('cast','lat','lon','year','month','day','data'))
    
    updateProgressBar(session = session, id = "norm_supplyP", value = 20)
    
    #### to main norm storage
   # vallist <- readRDS("./www/total_vallist.rds")
   # yearlist <- readRDS("./www/total_yearlist.rds")
    
    writeDFLists(df_t,TRUE)
    writeDFLists(df_s,FALSE)
    
    shinyjs::hide(id = "norm_supplyP_Panel", anim = T,animType = "slide", time = 0.2)
    close(fd)
  })
  
  writeDFLists <- function(df,ifT){
    
    vallist <- NULL
    yearlist <- NULL
    
    if(ifT){
      vallist <- readRDS("./www/t_WODT0.2_vallist.rds")
      yearlist <- readRDS("./www/t_WODT0.2_yearlist.rds")
    }
    else{
    vallist <- readRDS("./www/s_WODT0.2_vallist.rds")
    yearlist <- readRDS("./www/s_WODT0.2_yearlist.rds")
    }
    
    #print(sprintf("Before: %.3f Mb",object_size(vallist)/1024/1024))
    get.st.index <- function(lat)
    {
      for(j in 1:dimS)
      {
        if(lat >=st.levels[j]-0.25 & lat < st.levels[j]+0.25)
          return(j)
      }
      return(0)
    }
    
    sumvec <- vector(mode="numeric", length=dimD)
    couvec <- vector(mode="integer", length=dimD)
   
    for(ind in 1:nrow(df)){
      print(paste0(ind,' of ',nrow(df),', id=',df[ind,]$id))
      sumvec[] <- 0
      couvec[] <- 0
      
      stind <- get.st.index(df[ind,]$lat)
      moind <- df[ind,]$mon
      
      line <- df[ind,]$data
      line <- chartr('"()|','   ,', line); #Replace with whitespace
      line <- substr(line,1,nchar(line)-2)
      print("ERROR HERE?")
      m <- matrix(as.numeric(strsplit(line,",")[[1]]),ncol=2,byrow=TRUE)
      for (i in 1:nrow(m)){
        for(j in 1:dimD){
          if (m[i,1] >= dep.levels[j]-2.5 & m[i,1] < dep.levels[j]+2.5){
            couvec[j] %+=% 1
            sumvec[j] %+=% m[i,2]
            break
          }
        }
      }
      sumvec <- sumvec/couvec
      
      #if (ind == 171) browser()
      #dimD*dimS*dimM
      for (i in 1:dimD){
        indtotal <- dimD*dimS*(moind-1) + dimS*(i-1) + stind
        if(!is.nan(sumvec[i])){
          vallist[[indtotal]] <- c(vallist[[indtotal]],as.integer(sumvec[i]*100000))
          yearlist[[indtotal]] <- c(yearlist[[indtotal]],df[ind,]$year)
        }
      }
    }
    if(ifT){
      file.remove("./www/t_total_vallist.rds")
      file.remove("./www/t_total_yearlist.rds")
      saveRDS(vallist, file = "./www/t_total_vallist.rds")
      saveRDS(yearlist, file = "./www/t_total_yearlist.rds")
    }
    else{
      file.remove("./www/s_total_vallist.rds")
      file.remove("./www/s_total_yearlist.rds")
      saveRDS(vallist, file = "./www/s_total_vallist.rds")
      saveRDS(yearlist, file = "./www/s_total_yearlist.rds")
    }
  }
#### 3 END NORM Import to DB ####

#### 4 INI NORM Clear MMBI loaded XLSX data ####
  observeEvent(input$norm_clearB,{
    print("Clear norm DB...")
    listfiles <- paste0("./www/",norm_explist())
    print(listfiles)
    for (i in 1:length(listfiles)){
      file.remove(listfiles[i])
    }
    fd <- file("./www/explist.txt", open = "w")
    close(fd) 
    norm_explist(NULL)
  })
#### 4 END NORM Clear MMBI loaded XLSX data ####
  
#### 5 INI NORM Render table with MMBI XLSX ####
  output$norm_listexpDT <- DT::renderDataTable({ 
    print("Render norm DB list...")
    datatable(data.frame(col=norm_explist()),rownames = FALSE,
              options = list(headerCallback = JS(
      "function(thead, data, start, end, display){",
      "  $(thead).remove();",
      "}"),dom = 't',pageLength=length(norm_explist()),
    scrollY = 200, scroller = TRUE
  ),selection = 'none')})
#### 5 INI NORM Render table with MMBI XLSX ####
  
 #----------------------------------------------------------------------------------------------------------------------------------------
  
#### 6 INI ANO Load expedition file .XLSX as matrix ####
  expm <- reactiveVal(NULL)
  inputanoname <- reactiveVal(NULL)
  
  
  observe({
    if(is.null(input$ano_xlsFI)) return()
    print("START ANO - LOAD one exp XLSX")    
    
    temp.st <- 0
    temp.lat <- 0.0
    temp.lon <- 0.0
    temp.date <- Sys.Date()
    temp.y <- as.numeric(format(temp.date,'%Y'))
    temp.m <- as.numeric(format(temp.date,'%m'))
    temp.d <- as.numeric(format(temp.date,'%d'))
    temp.str <- character(0)
    temp.t <- -1e10
    temp.s <- -1e10
    
    df <- data.frame(id=integer(0),lat=numeric(0),lon=numeric(0),year=integer(0),month=numeric(0),day=numeric(0),data=character(0))
    df$data <- as.character(df$data)
    
    xlsxname <- input$ano_xlsFI$name
    inputanoname(input$ano_xlsFI$name)
    
    #prepare for warning list
    warnlist <- reactiveVal(vector(mode="character"))
    warnlist(c(isolate(warnlist()), paste0("Файл \"",xlsxname,"\":")))
    
    file.remove(paste0("./www/ANO_INPUT_", xlsxname))
    file.copy(input$ano_xlsFI$datapath, paste0("./www/ANO_INPUT_", xlsxname))
    file.remove(input$ano_xlsFI$datapath)
    
    tryCatch(withCallingHandlers({
      
      #open XLSX
      loadedxlsx <- read_excel(paste0("./www/ANO_INPUT_", xlsxname),col_names = TRUE,col_types=c("numeric","date",rep("numeric",each=6)))
      #print("Opened")
      #str(norm_loadedxlsx)
      
      temp.row <- 1
      loadedxlsx <- loadedxlsx[!(is.na(loadedxlsx$station) & is.na(loadedxlsx$date) 
                                           & is.na(loadedxlsx$lat) & is.na(loadedxlsx$lon) 
                                           & is.na(loadedxlsx$depth) & is.na(loadedxlsx$z)),]
      while(temp.row <= nrow(loadedxlsx)){
        
        temp.st <- loadedxlsx[temp.row,1]
        temp.lat <- loadedxlsx[temp.row,3]
        temp.lon <- loadedxlsx[temp.row,4]
        temp.date <- loadedxlsx[temp.row,]$date
        
        temp.y <- as.numeric(format(temp.date,'%Y'))
        temp.m <- as.numeric(format(temp.date,'%m'))
        
        temp.str <-"("
        if(TSflag() == "T"){
        temp.str <- paste0(temp.str,loadedxlsx[temp.row,6],"|",loadedxlsx[temp.row,7],"|")
        }
        else{
          temp.str <- paste0(temp.str,loadedxlsx[temp.row,6],"|",loadedxlsx[temp.row,8],"|")
        }
        
        while(TRUE){
          temp.row %+=% 1
          if(temp.row > nrow(loadedxlsx)) {break}
          if(!(is.na(loadedxlsx[temp.row,1]) & is.na(loadedxlsx[temp.row,2]) 
               & is.na(loadedxlsx[temp.row,3]) & is.na(loadedxlsx[temp.row,4]) 
               & is.na(loadedxlsx[temp.row,5] ))){break}
          if(TSflag() == "T"){
          temp.str <- paste0(temp.str,loadedxlsx[temp.row,6],"|",loadedxlsx[temp.row,7],"|")
          }
          else{
            temp.str <- paste0(temp.str,loadedxlsx[temp.row,6],"|",loadedxlsx[temp.row,8],"|")
          }
        }
        temp.str <- paste0(temp.str,")")
        df[nrow(df) + 1,] <- list(as.integer(paste0(temp.y,temp.m,temp.d,temp.st)),temp.lat,temp.lon,temp.y,temp.m,temp.d,temp.str)
      }
      
    },warning = function(w) {
      warnlist(c(isolate(warnlist()), paste0("Ошибка W: ",as.character(w))))
    },
    error = function(e)  {
      warnlist(c(isolate(warnlist()), paste0("Ошибка W: ",as.character(e))))
    }))

    if(length(isolate(warnlist())) == 1) warnlist(c(isolate(warnlist()), "Ошибок нет!"))
    else return
    
    shinyalert("Отчёт об импорте:<br>при наличии хотя бы одной ошибки весь импорт отменяется!",paste0(
      "<pre><div style=\"white-space: pre-wrap; white-space: -moz-pre-wrap; white-space: -pre-wrap; 
      white-space: -o-pre-wrap; width:400px;max-height:300px;overflow:auto;word-wrap: break-word;\">",
      paste(warnlist(), collapse="\n"),"</div></pre>"), type = "info",html=TRUE)

    if(length(isolate(warnlist())) != 2) return()
    
    file.remove(paste0("./www/ANO_INPUT_", xlsxname))
   
    #from df to vallist (each station can be repeater, so vallist neaded, but months set is not used)
    vallist <- vector("list",length = dimD*dimS)
    for (i in 1:dimD*dimS){
      vallist[[i]] <- vector(mode="numeric")
    }
    
    get.st.index <- function(lat)
    {
      for(j in 1:dimS)
      {
        if(lat >=st.levels[j]-0.25 & lat < st.levels[j]+0.25)
          return(j)
      }
      return(0)
    }
    
    sumvec <- vector(mode="numeric", length=dimD)
    couvec <- vector(mode="integer", length=dimD)
    
    for(ind in 1:nrow(df)){
      print(paste0(ind,' of ',nrow(df)))
      sumvec[] <- 0
      couvec[] <- 0
      line <- df[ind,]$data
      line <- chartr('"()|','   ,', line); #Replace with whitespace
      line <- substr(line,1,nchar(line)-2)
      m <- matrix(as.numeric(strsplit(line,",")[[1]]),ncol=2,byrow=TRUE);
      
      for (i in 1:nrow(m)){
        for(j in 1:dimD){
          if (m[i,1] >= dep.levels[j]-2.5 & m[i,1] < dep.levels[j]+2.5){
            couvec[j] %+=% 1
            sumvec[j] %+=% m[i,2]
            break
          }
        }
      }
      
      sumvec <- sumvec/couvec
      
      #dimD*dimS*dimM
      for (i in 1:dimD){
        indtotal <- dimS*(i-1) + get.st.index(df[ind,]$lat)
        if(!is.nan(sumvec[i])){
          vallist[[indtotal]] <- c(vallist[[indtotal]],sumvec[i])
        }
      }
    }
    
    #from vallist to expvec to expmatr
    expvec = vector("numeric",length = dimD*dimS)
    
    for (i in 1:(dimD*dimS)){
      if(length(vallist[[i]]) == 0) expvec[i] <- NA
      else expvec[i] <- mean(vallist[[i]])
    }
    #print(expvec)
    expm(matrix(expvec,ncol=dimS,byrow=TRUE))
 })
#### 6 END ANO Load expedition file .XLSX as matrix ####

#### 7 INI ANO Calculate and export anomalies to .XLSX #### 
  monthsList <- reactiveVal(c("январь","февраль","март","апрель","май","июнь","июль",
                  "август","сентябрь","октябрь","ноябрь","декабрь"))
  
  bottomindexes <- reactiveVal(c(30,31,50,43,56,53,56,43,56,55,rep(50,each=8),70))
  
  calcMeanVal <- function(vals,subindexes,j,threshold = 0.8){
    subarr <- vals[subindexes,j]
    subarr <- subarr[!is.na(subarr)]
    if(length(subarr) > threshold*length(subindexes))
          return(mean(subarr))
    else return(NA)
  }
  
  meanweighted <- function(vals,threshold = 0.8){
    nsmall <- matrix(nrow = 5,ncol=dimS+1)
    for (j in 1:(dimS+1)){
      #0-50
      nsmall[1,j] <- calcMeanVal(vals,c(1:11),j,threshold)
      #0-100
      nsmall[2,j] <- calcMeanVal(vals,c(1:21),j,threshold)
      #150-200
      nsmall[3,j] <- calcMeanVal(vals,c(31:41),j,threshold)
      #0-200
      nsmall[4,j] <- calcMeanVal(vals,c(1:41),j,threshold)
      #0-bottom   
      nsmall[5,j] <- calcMeanVal(vals,c(1:bottomindexes()[j]),j,threshold)
    }
    return(nsmall)
  }
  
  calcCountVal <- function(vals,subindexes,j,threshold = 0.8){
    subarr <- vals[subindexes,j]
    subarr <- subarr[!is.na(subarr)]
    if(length(subarr) > threshold*length(subindexes))
      return(length(subarr))
    else return(NA)
  }
  
  meanweightedCount <- function(vals,threshold = 0.8){
    nsmall <- matrix(nrow = 5,ncol=dimS+1)
    for (j in 1:(dimS+1)){
      #0-50
      nsmall[1,j] <- calcCountVal(vals,c(1:11),j,threshold)
      #0-100
      nsmall[2,j] <- calcCountVal(vals,c(1:21),j,threshold)
      #150-200
      nsmall[3,j] <- calcCountVal(vals,c(31:41),j,threshold)
      #0-200
      nsmall[4,j] <- calcCountVal(vals,c(1:41),j,threshold)
      #0-bottom   
      nsmall[5,j] <- calcCountVal(vals,c(1:bottomindexes()[j]),j,threshold)
    }
    return(nsmall)
  }
  
  output$ano_runandexportB <- downloadHandler(  
    filename = paste0(TSflag(),"_Anoms_",inputanoname(),"_AS_",input$ano_monSI,".xlsx"),
    content = function(file) {
      #get norm data for month
      startyear <- as.integer(input$norm_year_fromSI)
      stopyear <- as.integer(input$norm_year_toSI)
      #vallist <- readRDS("./www/WODT0.2_vallist.rds")
      #yearlist <- readRDS("./www/WODT0.2_yearlist.rds")
      
      vallist <- NULL
      yearlist <- NULL
      if(TSflag() == "T"){

          vallist <- readRDS("./www/t_total_vallist.rds")
          yearlist <- readRDS("./www/t_total_yearlist.rds")
        
      }
      else{
       
          vallist <- readRDS("./www/s_total_vallist.rds")
          yearlist <- readRDS("./www/s_total_yearlist.rds")
        
      }
      
      normvec = vector("numeric",length = dimT)
      for (i in 1:dimT){
        liye <- yearlist[[i]]
        liva <- vallist[[i]]
        if(length(liye) == 0) {
          normvec[i] <- NaN
          next
        }
        liva <- liva[liye >= startyear & liye <= stopyear]
        liye <- liye[liye >= startyear & liye <= stopyear]
        yearlist[[i]] <- liye
        vallist[[i]] <- liva
        
        if(length(vallist[[i]]) == 0) normvec[i] <- NaN
        else normvec[i] <- mean(vallist[[i]])
      }
      m <- which(monthsList() == input$ano_monSI)
      
      print(paste0("Month detected: ",m))
     
      normmatr <- matrix(normvec[(dimD*dimS*(m-1)+1):(dimD*dimS*m)],ncol=dimS,byrow=TRUE)
      normmatr <- cbind(normmatr,matrix(NA, nrow = dimD, ncol = 1))
      for(i in 1:dimD){
        normmatr[i,ncol(normmatr)] = mean(normmatr[i,!is.na(normmatr[i,])])
      }
      
      normmatr = normmatr / 100000.0
      
      write.csv(normmatr,"./www/normmama.csv",row.names = FALSE)
      
      
      normmatr <- meanweighted(normmatr,0.0000001)
    #  print(expm())
      expmatr <- expm()
      expmatr <- cbind(expmatr,matrix(NA, nrow = dimD, ncol = 1))
      for(i in 1:dimD){
        expmatr[i,ncol(expmatr)] = mean(expmatr[i,!is.na(expmatr[i,])])
      }
      expmatr <- meanweighted(expmatr,0.0000001)

      expmatr <- expmatr - normmatr
      
      wb <- createWorkbook()
      addWorksheet(wb,sheetName = "1")
      expmatr <- round(expmatr, digits = 5)
      writeData(wb, sheet = 1,x = paste0(TSflag(),"_Anoms_",inputanoname(),"_AS_",input$ano_monSI),
                startCol = 2, startRow = 2)
     
      writeData(wb, sheet = 1,x = expmatr,
                  startCol = 2, startRow = 3, borders = "columns")
      # writeData(wb, sheet = m, x = dep.levels,
      #             startCol = 1, startRow = 4, borders = "rows")
      s <- createStyle(numFmt = "0.00000",border = "LeftRight")
      addStyle(wb, 1, style = s, cols =2:(ncol(expmatr)+5), rows=3:(nrow(expmatr)+5), gridExpand = TRUE)
      saveWorkbook(wb, file) 
    })
   
#### 7 END ANO Calculate and export anomalies to .XLSX ####
  
#### 8 INI ANO Test calc for pair from file #### 
  # observeEvent(input$ano_pairTestB,{
  #   #normtestmatr <- matrix(0L, nrow = dimD, ncol = dimS)
  #   
  #   exptestxlsx <- read_excel("H:\\_РАБОТА\\2020весна\\Статья по нормам\\от Татьяны\\2019_апрель_z.xlsx",sheet = 8,range = cell_cols("Q:Z"),col_names = TRUE,col_types=rep("numeric",each=10))
  #   exptestmatr <- data.matrix(exptestxlsx[1:70,])
  #   exptestmatr <- cbind(exptestmatr,matrix(NA, nrow = 70, ncol = 9))
  #   for(i in 1:nrow(exptestmatr)){
  #     exptestmatr[i,ncol(exptestmatr)] = mean(exptestmatr[i,!is.na(exptestmatr[i,])])
  #   }
  #   
  #   normtestxlsx <- read_excel("H:\\_РАБОТА\\2020весна\\Статья по нормам\\от Татьяны\\2019_апрель_z.xlsx",sheet = 8,range = cell_cols("B:K"),col_names = TRUE,col_types=rep("numeric",each=10))
  #   normtestmatr <- data.matrix(normtestxlsx[1:70,])
  #   normtestmatr <- cbind(normtestmatr,matrix(NA, nrow = 70, ncol = 9))
  #   for(i in 1:nrow(normtestmatr)){
  #     normtestmatr[i,ncol(normtestmatr)] = mean(normtestmatr[i,!is.na(normtestmatr[i,])])
  #   }
  #   
  #   exptestmatr <- meanweighted(exptestmatr)
  #   normtestmatr <- meanweighted(normtestmatr)
  #   print(exptestmatr)
  #   print("")
  #   print(normtestmatr)
  #   print("")
  #   print(exptestmatr - normtestmatr)
  #   
  # })
  
#### 8 END ANO Test calc for pair from file ####
  
#### 9 INI ANO All WOD anomalies for month #### 
  output$ano_allWODanoTestB <- downloadHandler(  
    filename = paste0(TSflag(),"_ALL_Anoms_",inputanoname(),"_AS_",input$ano_monSI,".xlsx"),
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb,sheetName = "1")
      addWorksheet(wb,sheetName = "2")
      tempmonth <- which(monthsList() == input$ano_monSI)
      
      for (yeartemp in 1970:2019){
        
        print(paste0("*********************",yeartemp,"************************"))
        
        writeData(wb, sheet = 1,x = paste0("Anoms_",yeartemp,"_in_",input$ano_monSI),
                  startCol = 2, startRow = 2+(yeartemp-1970)*8)
        writeData(wb, sheet = 2,x = paste0("Anoms_",yeartemp,"_in_",input$ano_monSI),
                  startCol = 2, startRow = 2+(yeartemp-1970)*8)
        
        vallist <-NULL
        yearlist <-NULL
        if(TSflag() == "T"){
          vallist <- readRDS("./www/t_total_vallist.rds")
          yearlist <- readRDS("./www/t_total_yearlist.rds")
        }
        else
        {
          vallist <- readRDS("./www/s_total_vallist.rds")
          yearlist <- readRDS("./www/s_total_yearlist.rds")
        }
        startyear <- as.integer(input$norm_year_fromSI)
        stopyear <- as.integer(input$norm_year_toSI)
        normvec = vector("numeric",length = dimT)
        for (i in 1:dimT){
          liye <- yearlist[[i]]
          liva <- vallist[[i]]
          if(length(liye) == 0) {
            normvec[i] <- NaN
            next
          }
          liva <- liva[liye >= startyear & liye <= stopyear]
          liye <- liye[liye >= startyear & liye <= stopyear]
          yearlist[[i]] <- liye
          vallist[[i]] <- liva
          
          
          if(length(vallist[[i]]) == 0) normvec[i] <- NaN
          else normvec[i] <- mean(vallist[[i]])
        }
        m <- tempmonth
        
        print(paste0("Month detected: ",m))
        
        normmatr <- matrix(normvec[(dimD*dimS*(m-1)+1):(dimD*dimS*m)],ncol=dimS,byrow=TRUE)
        normmatr <- cbind(normmatr,matrix(NA, nrow = dimD, ncol = 1))
        for(i in 1:dimD){
          normmatr[i,ncol(normmatr)] = mean(normmatr[i,!is.na(normmatr[i,])])
        }
        
        normmatr = normmatr / 100000.0
        
        normmatr <- meanweighted(normmatr,0.0000001)
        
        ####################################
        df <- NULL
        
        if(TSflag() == "T") df <- readRDS("./www/t_total_AS_strdf.rds")
        else df <- readRDS("./www/s_total_AS_strdf.rds")
        
        
        df <- df %>% subset((df$month == tempmonth) & (df$year == yeartemp))
      
        
        if(nrow(df) == 0) next
        else print(paste0("HUBBbbbbbbbbbbbbbbbbbbbbbbbbbbbbBA",yeartemp))
        
        #from df to vallist (each station can be repeater, so vallist neaded, but months set is not used)
        vallist <- vector("list",length = dimD*dimS)
        for (i in 1:dimD*dimS){
          vallist[[i]] <- vector(mode="numeric")
        }
        
        get.st.index <- function(lat)
        {
          for(j in 1:dimS)
          {
            if(lat >=st.levels[j]-0.25 & lat < st.levels[j]+0.25)
              return(j)
          }
          return(0)
        }
        
        sumvec <- vector(mode="numeric", length=dimD)
        couvec <- vector(mode="integer", length=dimD)
        
        for(ind in 1:nrow(df)){
          print(paste0(ind,' of ',nrow(df)))
          sumvec[] <- 0
          couvec[] <- 0
          line <- df[ind,]$data
          line <- chartr('"()|','   ,', line); #Replace with whitespace
          line <- substr(line,1,nchar(line)-2)
          m <- matrix(as.numeric(strsplit(line,",")[[1]]),ncol=2,byrow=TRUE);
          
          for (i in 1:nrow(m)){
            for(j in 1:dimD){
              if (m[i,1] >= dep.levels[j]-2.5 & m[i,1] < dep.levels[j]+2.5){
                couvec[j] %+=% 1
                sumvec[j] %+=% m[i,2]
                break
              }
            }
          }
          
          sumvec <- sumvec/couvec
          
          #dimD*dimS*dimM
          for (i in 1:dimD){
            indtotal <- dimS*(i-1) + get.st.index(df[ind,]$lat)
            if(!is.nan(sumvec[i])){
              vallist[[indtotal]] <- c(vallist[[indtotal]],sumvec[i])
            }
          }
        }
        
        #from vallist to expvec to expmatr
        expvec = vector("numeric",length = dimD*dimS)
        
        for (i in 1:(dimD*dimS)){
          if(length(vallist[[i]]) == 0) expvec[i] <- NA
          else expvec[i] <- mean(vallist[[i]])
        }
       
        expmatr <- matrix(expvec,ncol=dimS,byrow=TRUE)
        expmatr <- cbind(expmatr,matrix(NA, nrow = dimD, ncol = 1))
        
        
        
        expmatrCount <- meanweightedCount(expmatr,0.0000001)
        writeData(wb, sheet = 2,x = expmatrCount,
                  startCol = 2, startRow = 3+(yeartemp-1970)*8, borders = "columns")
        
        
        expmatr <- meanweighted(expmatr,0.0000001)
        expmatr <- expmatr - normmatr
        expmatr <- round(expmatr, digits = 5)
  
        writeData(wb, sheet = 1,x = expmatr,
                  startCol = 2, startRow = 3+(yeartemp-1970)*8, borders = "columns")
  
      } 
      s <- createStyle(numFmt = "0.00000",border = "LeftRight")
      addStyle(wb, 1, style = s, cols =2:30, rows=3:400, gridExpand = TRUE)
      s <- createStyle(numFmt = "0.0",border = "LeftRight")
      addStyle(wb, 2, style = s, cols =2:30, rows=3:400, gridExpand = TRUE)
      saveWorkbook(wb, file)  
  })
  
#### 9 END ANO All WOD anomalies for month ####
  
})