
setwd("D:/Documents/SQL script/R/Logi")

d<-read.xlsx2(file="Exampl.xlsx",sheetName='All',startRow=1,stringsAsFactors = FALSE,colClasses="character")
col_old<-colnames(d)
colnames(d)<- c('operator','type','object','data','start_t','end_t','check','pay','date_vie','start_vie','end_vie'
                ,'delta_t_vie','s_com','com','resault','pre_type','id_user_fraud','fio_user_fraud','respon','zer')
d$start_t<-as.character(format(as.POSIXct((as.numeric(d$data) + as.numeric(d$start_t))* 
                                            (60*60*24), origin="1899-12-30", tz="GMT"),'%H:%M:%S'))
d$end_t<-as.character(format(as.POSIXct((as.numeric(d$data) + as.numeric(d$end_t))* (60*60*24), origin="1899-12-30", tz="GMT"),'%H:%M:%S'))
d$data<-as.Date(as.numeric(d$data), origin="1899-12-30")
otl$otl_time<-otl$otl_time
d$date_vie<-as.Date(as.numeric(d$date_vie), origin="1899-12-30")
d$start_vie<-as.POSIXct((as.numeric(d$date_vie) + as.numeric(d$start_vie))* (60*60*24), origin="1899-12-30", tz="GMT")
d$end_vie<-as.POSIXct((as.numeric(d$date_vie) + as.numeric(d$end_vie))* (60*60*24), origin="1899-12-30", tz="GMT")
d$pay<-as.numeric(sub('р','',d$pay))
is.na(d$pay)<-c(0)
d<-d%>%arrange(desc(object),desc(data),desc(start_t),desc(end_t))
d$case_id <- 1:dim(d)[1]

################
otl<-read.xlsx2(file="Exampl.xlsx",sheetName='otl',startRow=1,stringsAsFactors = FALSE,colClasses="character")
col_otl_old<-colnames(otl)
colnames(otl)<- c('object','data','otl_time','dif_time','check','check_line','ref_data','ref_check'
                  ,'item','item_name','brend'
                  ,'type','type2','amount','price','sale','pay','check_type','dogovor','imei','ck','fio_ck','mob'
                  ,'phon','user_id','user_name')
otl$otl_time<-as.POSIXct((as.integer(otl$data) + as.numeric(otl$otl_time))* (60*60*24), origin="1899-12-30", tz="GMT")
otl$data<-as.Date(as.POSIXct((as.numeric(otl$data))* (60*60*24), origin="1899-12-30", tz="GMT"))
otl<-otl%>%arrange(desc(object),desc(data),desc(check),desc(otl_time))


otl<- otl %>% left_join(d[,c('object','data','check','resault','case_id')]
                       ,by = c('object'='object','data'='data','check'='check'))

otl[is.na(otl$resault),'resault']<-'new'
otl[is.na(otl$case_id),'case_id']<-0
col_otl_old <- c(col_otl_old,'resault','case_id')
otl$otl_time<-as.character(format(otl$otl_time,'%H:%M:%S'))
otl$data<-as.character(format(otl$data,'%d.%m.%Y %H:%M:%S'))
otl$id<-rowid(otl$check)
col_otl_old<-c(col_otl_old,'id')
otl<-otl%>% 
  tibble::rownames_to_column()
#################
###
an<-read.xlsx2(file="Exampl.xlsx",sheetName='an',startRow=1,stringsAsFactors = FALSE,colClasses="character")
col_an_old<-colnames(an)
colnames(an)<- c('object','data','in_time','end_time','check_type','item','item_name','type'
                 ,'imei','amount','price','pay','dogovor','phon','user_id')
an$in_time<-as.POSIXct((as.integer(an$data) + as.numeric(an$in_time))* (60*60*24), origin="1899-12-30", tz="GMT")
an$in_time<-as.character(format(an$in_time,'%H:%M:%S'))
an$end_time<-as.POSIXct((as.integer(an$data) + as.numeric(an$end_time))* (60*60*24), origin="1899-12-30", tz="GMT")
an$end_time<-as.character(format(an$end_time,'%H:%M:%S'))
an$data<-as.Date(as.POSIXct((as.numeric(an$data))* (60*60*24), origin="1899-12-30", tz="GMT"))
#an$data1 <- as.Date(an$data)
an<-an%>%arrange(desc(object),desc(data),desc(in_time),desc(end_time))
an<- an %>% left_join(d[,c('object','data','resault','start_t','end_t','case_id')]
                        ,by = c('object'='object','data'='data','in_time'='start_t','end_time'='end_t'))
an[is.na(an$resault),'resault']<-'new'
an[is.na(an$case_id),'case_id']<-0
an$id<-rowid(an$in_time)
col_an_old <- c(col_an_old,'resault','case_id','id')

an$data<-as.character(format(an$data,'%d.%m.%Y'))

an<-an%>% 
  tibble::rownames_to_column()


library(shiny)
library(dplyr)
library(DT)
library(data.table)
library(shinyFiles)
library(xlsx)


ui<- navbarPage(id = "nav-page",
                title = "",
                position = "fixed-top"
                ,header = tags$style(type="text/css", "body {padding-top: 70px;}"),
                tabPanel("Отложенные чеки",
                         fluidRow(
                           column(2, 
                                  selectInput('o', 'Новый код',c("All", unique(otl$object)))),
                           column(2, #offset = 3,
                                  selectInput('u', 'Сотрудник',c("All", unique(otl$user_name)))),
                           column(2, #offset = 3,
                                  selectInput('d1', 'Дата',c("All",sort(unique(substr(otl$data,1,10)))))),
                           column(2, #offset = 3,
                                  selectInput('t', 'Статус',c("All", unique(otl$resault)))),
                           actionButton( "submit", label = "Создать case" 
                                         ,style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                         ),
                         hr(), 
                         h4('Детальная информация по отложенным чекам'),
                         DT::dataTableOutput("x4")        
                ),tabPanel("Аннулированные чеки",
                           fluidRow(
                             column(2, 
                                    selectInput('o2', 'Новый код',c("All", unique(an$object)))),
                             column(2, #offset = 3,
                                    selectInput('u2', 'Сотрудник',c("All", unique(an$user_id)))),
                             column(2, #offset = 3,
                                    selectInput('d2', 'Дата',c("All",sort(unique(substr(an$data,1,10)))))),
                             column(2, #offset = 3,
                                    selectInput('t2', 'Статус',c("All", unique(an$resault)))),
                             actionButton( "submit2", label = "Создать case" 
                                           ,style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                           ),
                           hr(), 
                           h4('Детальная информация по аннулированным чекам'),
                           DT::dataTableOutput("xa")        
                ),tabPanel("Case",
                          fluidRow(
                            column(2, 
                                   selectInput('op', 'Оператор',c("new", unique(d$operator)))),
                            column(2, #offset = 3,
                                   selectInput('d1', 'Дата разбора',c("new",'сейчас'))),
                            column(2, #offset = 3,
                                   selectInput('tp', 'Тип фрода',c("new",'Другое',"Отложенные",'Аннулированные'))),
                            column(2, #offset = 3,
                                   selectInput('s', 'Статус',c("В работе",'Фрод','Не фрод','Закрыто')))
                            
                          ),
                          hr(), 
                          h4('Кейс для разбора'),
                          column(6,offset = 6, 
                                 HTML('<div class="btn-group" role="group" aria-label="Basic example">'),
                                 actionButton(inputId = "Add_row_head",label = "Добавить"),
                                 actionButton(inputId = "Del_row_head",label = "Удалить"),
                                 actionButton(inputId = "Save",label = "Сохранить"
                                              ,style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                 HTML('</div>')),
                          DT::dataTableOutput("x5"),
                          h4('Список кейсов за сегодня'),
                          DT::dataTableOutput("x6"),
                          hr(), 
                          h4('Детали чека, отложенные'),
                          DT::dataTableOutput("x7"),
                          h4('Детали чека, аннулированные'),
                          DT::dataTableOutput("x8")
    )
    #,tabPanel("video",tags$video(src = "video.mp4", type = "video/mp4", autoplay = NA, controls = NA)  )  
    )

server <- shinyServer(function(input, output, session){
  
  observeEvent(input$submit, {
    updateNavbarPage(session, "nav-page", "Case")
  })
  observeEvent(input$submit2, {
    updateNavbarPage(session, "nav-page", "Case")
  })  
  
  ########## OTL
  ####
  x <- reactiveValues()
  x$dt<- data.frame(case_id=0,resault=NA,object=NA,data=NA,check=NA,in_time=NA,tp=NA,op=NA,d=NA,s=NA,com=NA,stringsAsFactors=FALSE)
  x$resault<-data.frame(case_id=0,resault=NA,object=NA,data=NA,check=NA,in_time=NA,tp=NA,op=NA,d=NA,s=NA,com=NA,d2=NA,stringsAsFactors=FALSE)
  x$otl<-otl
  x$an<-an
 #####
  updateSelectInput(session, "t",
                    #choices = s_options,
                    selected = ,c("All", unique(an$resault))
  )
  
  
  ###
  dataInput<- reactive({  
    data<-x$otl
    if(input$o != "All"){  data<-filter(data,object %in% input$o) }
    if(input$u != "All"){  data<-filter(data,user_name %in% input$u) } 
    if(input$d1 != "All"){  data<-filter(data,substr(data,1,10) %in% input$d1) }
    if(input$t != "All"){  data<-filter(data,resault %in% input$t) }
    data
    
  })  
  ##############  
  output$x4 <-  DT::renderDataTable(server = FALSE,{
    data <- dataInput()

    datatable(data ,rownames = FALSE
              , colnames = col_otl_old
              #,filter = 'top'
              ,extensions =c ('ColReorder', 'Buttons', 'FixedHeader') 
              , options=list(fixedHeader = TRUE, columnDefs = list(list(width = '60%', targets = c(10,11,12),
                                                                        render = JS("function(data, type, row, meta) {",
                                                                                    "return type === 'display' && data.length > 15 ?",
                                                                                    "'<span title=\"' + data + '\">' + data.substr(0, 15) + '...</span>' : data;",
                                                                                    "}"
                                                                        ))),
                             dom = 'Blfrtip',   
                             buttons = 
                               list( I('colvis'), list(
                                 extend = 'collection',
                                 buttons = c('csv', 'excel','pdf'),
                                 text = 'Download'
                               )),
                             colReorder = TRUE)
    )  %>% formatStyle(0,target = "row", fontWeight = styleEqual(nrow(data)+1, "bold")) %>%
      formatStyle("id",target = "row", backgroundColor = styleEqual(1, c('LightGrey')))%>% 
      formatStyle("resault","resault", backgroundColor = styleEqual(c('Нет фрода', 'Фрод'), c('LightGreen', 'Orange')))
    
  })
############AN

  output$xa <-  DT::renderDataTable(server = FALSE,{
    
    if(input$o2 != "All"){  x$an<-filter(x$an,object %in% input$o2) }
    if(input$u2 != "All"){  x$an<-filter(x$an,user_id %in% input$u2) }
    if(input$d2 != "All"){  x$an<-filter(x$an,substr(data,1,10) %in% input$d2) }
    if(input$t2 != "All"){  x$an<-filter(x$an,resault %in% input$t2) }
    data <-x$an
    
    datatable(data ,rownames = FALSE
              , colnames = col_an_old
              #,filter = 'top'
              ,extensions =c ('ColReorder', 'Buttons', 'FixedHeader') 
              , options=list(fixedHeader = TRUE, columnDefs = list(list(width = '60%', targets = c(10,11,12),
                                                                        render = JS("function(data, type, row, meta) {",
                                                                                    "return type === 'display' && data.length > 15 ?",
                                                                                    "'<span title=\"' + data + '\">' + data.substr(0, 15) + '...</span>' : data;",
                                                                                    "}"
                                                                        ))),
                             dom = 'Blfrtip',   
                             buttons = 
                               list( I('colvis'), list(
                                 extend = 'collection',
                                 buttons = c('csv', 'excel','pdf'),
                                 text = 'Download'
                               )),
                             colReorder = TRUE)
    )  %>% formatStyle(0,target = "row", fontWeight = styleEqual(nrow(data)+1, "bold")) %>%
      formatStyle("id",target = "row", backgroundColor = styleEqual(1, c('LightGrey')))%>% 
      formatStyle("resault","resault", backgroundColor = styleEqual(c('Нет фрода', 'Фрод'), c('LightGreen', 'Orange')))
    
  })
  
  ###Case

  ###
  observeEvent(input$submit,{
    data <- dataInput()

    s4 <- input$x4_rows_selected
     if (length(s4)>0) {
     dt<- unique(data[s4,c('case_id','resault','object','data','check')] )
      dt<-cbind(dt,in_time=c(NA),tp=c('Отложенные'),op=c(NA),d=c(NA),s=c(NA),com=c(NA))
      dt[is.na(dt$case_id|| dt$case_id==0),'case_id']<-c(ifelse(max(x$resault$case_id)<=0,1,max(x$resault$case_id) +1))
      if(nrow(x$dt[x$dt$resault!='ручной' & !is.na(x$dt$resault),])>0) { x$dt<-rbind(x$dt,dt)} else x$dt<-dt
      
    } #else data.frame(case_id =NA,resault='ручной',object=NA,data=NA,check=NA)
  })
  ###
  observeEvent(input$submit2,{
    data <- x$an
    
    s5 <- input$xa_rows_selected
    if (length(s5)>0) {
      dt<- unique(data[s5,c('case_id','resault','object','data','in_time')] )
      dt<-cbind(dt,check=c(NA),tp=c('Аннулированные'),op=c(NA),d=c(NA),s=c(NA),com=c(NA))
      dt<-dt[,c(1,2,3,4,6,5,7,8,9,10,11)]
      dt[is.na(dt$case_id|| dt$case_id==0),'case_id']<-c(ifelse(max(x$resault$case_id)<=0,1,max(x$resault$case_id) +1))
      if(nrow(x$dt[x$dt$resault!='ручной' & !is.na(x$dt$resault),])>0) { x$dt<-rbind(x$dt,dt)} else x$dt<-dt
    } #else data.frame(case_id =NA,resault='ручной',object=NA,data=NA,check=NA)
  })

    observeEvent(input$Add_row_head,{
     d=format(if(input$d1 != "new"){  Sys.time() } else '' ,'%d.%m.%Y %H:%M:%S')
     if (nrow(x$dt)>0) {
    new_row<-data.frame(case_id=c(max(x$resault$case_id) +1),resault='ручной',object=tail(x$dt["object"], n=1)
                        ,data=tail(x$dt["data"], n=1)
                        ,check=NA,in_time=NA,tp=NA,op=NA
                        ,d=d,s=NA,com=NA)
    x$dt<-rbind(x$dt,new_row)
   } else 
     x$dt<- data.frame(case_id=0,resault=NA,object=NA,data=NA,check=NA,in_time=NA,tp=NA,op=NA,d=NA,s=NA,com=NA,stringsAsFactors=FALSE)
   
  })
  
  observeEvent(input$Del_row_head,{
    
    if (length(input$x5_rows_selected)) {
      x$dt <- x$dt[-input$x5_rows_selected, ]
    } else 
      x$dt
  }
  )
  ### Save
  
  
  ####
  output$x5 <-  DT::renderDataTable(server = FALSE,{  
   if(input$d1 != "new"){ x$dt$d<- format(Sys.time(),'%d.%m.%Y %H:%M:%S') }  

   if(input$op != "new"){ x$dt$op<- input$op}
   x$dt$s<-input$s
   x$dt[is.na(x$dt$resault),'resault']<-c('ручной')
   x$dt$tp<-as.character(x$dt$tp)
   if(input$tp != "new" ){ 
     x$dt[x$dt$resault %in% c('ручной',NA) ,'tp'] <- input$tp
     } else x$dt[x$dt$resault %in% c('ручной',NA) ,'tp'] <- NA
  dt<- x$dt
  DT::datatable(dt, options = list(dom = 't')
            , editable = TRUE #,rownames = FALSE
            ,colnames = c('case_id','resault','Новый код',"Дата","Номер чека",'in_time','Тип фрода'
                          ,"Оператор","Кейс открыт","Статус",'Комментарий')
            )
  })
##
  observeEvent(input$x5_cell_edit, {
    info = input$x5_cell_edit
    str(info)
    i = info$row
    j = info$col 
    v = as.character(info$value)
    if (j %in% c(3,4,5,6,11)) { x$dt[i, j] <- isolate(DT::coerceValue(v, x$dt[i, j]))}
  })

  observeEvent(input$Save,{
    if (nrow(x$dt)>0) {
    dt<-x$dt
    dt$d2<- format(Sys.time(),'%H:%M:%S')
    dt[is.na(dt$case_id) || dt$case_id==0,'case_id']<-ifelse(max(x$resault$case_id)<=0,1,max(x$resault$case_id) +1)
    x$resault <- rbind(x$resault,dt)

    d=format(if(input$d1 != "new"){  Sys.time() } else '' ,'%d.%m.%Y %H:%M:%S')
   
     cc<-dt[dt$resault!='ручной' & !is.na(dt$resault) & x$dt$tp =='Отложенные' 
            ,c('case_id','object','data','check','s')]%>% unique()

     if(nrow(cc)>0){
      p<- inner_join(x$otl,cc,by = c('object'='object','data'='data','check'='check') ,suffix = c("", ".y"))   %>%
              select(c('rowname','case_id.y','s'))
      x$otl[x$otl$rowname %in% p$rowname,c('case_id','resault')] <- p[,c('case_id.y','s')]
     }
     cc1<-dt[dt$resault!='ручной' & !is.na(dt$resault) & x$dt$tp =='Аннулированные' 
            ,c('case_id','object','data','in_time','s')]%>% unique()
     if(nrow(cc1)>0){
       p<- inner_join(x$an,cc1,by = c('object'='object','data'='data','in_time'='in_time') ,suffix = c("", ".y"))   %>%
         select(c('rowname','case_id.y','s'))
       x$an[x$an$rowname %in% p$rowname,c('case_id','resault')] <- p[,c('case_id.y','s')]
     } 
    x$dt<-data.frame(case_id=0,resault=NA,object=NA,data=NA,check=NA,in_time=NA,tp=NA,op=NA,d=d,s=NA,com=NA,stringsAsFactors=FALSE)
   
    }
  }) 
  ##
  output$x6 <-  DT::renderDataTable(server = FALSE,{  
    dt<-x$resault
    #dt<-dt[,c(1,2,3,4,5,6,7,8,9,12,10,11)]
    datatable(dt
              ,rownames = FALSE
              ,colnames = c("case_id",'Старый статус','Новый код',"Дата","Номер чека",'in_time','Тип фрода',"Оператор","Кейс открыт","Статус",'Комментарий',"Кейс закрыт")
    ) 
    })
  
  output$x7 <-  DT::renderDataTable(server = FALSE,{

     cc<-x$dt[x$dt$resault!='ручной'& !is.na(x$dt$resault) & x$dt$tp =='Отложенные',c('object','data','check')]
     dt<-  if(nrow(cc)>0){
       inner_join(x$otl,cc,by = c('object'='object','data'='data','check'='check'))
     } else x$otl[0,]

    datatable(dt,rownames = FALSE
              , colnames = col_otl_old
               ) %>% formatStyle(0,target = "row", fontWeight = styleEqual(nrow(dt)+1, "bold")) %>%
      formatStyle("id",target = "row", backgroundColor = styleEqual(1, c('LightGrey')))%>%
      formatStyle("resault","resault", backgroundColor = styleEqual(c('Нет фрода', 'Фрод'), c('LightGreen', 'Orange')))
  })
  output$x8 <-  DT::renderDataTable(server = FALSE,{
    
    cc<-x$dt[x$dt$resault!='ручной'& !is.na(x$dt$resault) & x$dt$tp =='Аннулированные',c('object','data','in_time')]
    dt<-  if(nrow(cc)>0){
      inner_join(x$an,cc,by = c('object'='object','data'='data','in_time'='in_time'))
    } else x$an[0,]
    
    datatable(dt,rownames = FALSE
              , colnames = col_an_old
    ) %>% formatStyle(0,target = "row", fontWeight = styleEqual(nrow(dt)+1, "bold")) %>%
      formatStyle("id",target = "row", backgroundColor = styleEqual(1, c('LightGrey')))%>%
      formatStyle("resault","resault", backgroundColor = styleEqual(c('Нет фрода', 'Фрод'), c('LightGreen', 'Orange')))
  })
})
shinyApp(ui = ui, server = server)



