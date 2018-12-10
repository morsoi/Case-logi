setwd("D:/Documents/SQL script/R/Logi")

d<-read.xlsx2(file="Exampl.xlsx",sheetName='All',startRow=1,stringsAsFactors = FALSE,colClasses="character")
col_old<-colnames(d)
colnames(d)<- c('operator','type','object','data','start_t','end_t','check','pay','date_vie','start_vie','end_vie'
                ,'delta_t_vie','s_com','com','resault','pre_type','id_user_fraud','fio_user_fraud','respon','zer')
d$start_t<-as.POSIXct((as.numeric(d$data) + as.numeric(d$start_t))* (60*60*24), origin="1899-12-30", tz="GMT")
d$end_t<-as.POSIXct((as.numeric(d$data) + as.numeric(d$end_t))* (60*60*24), origin="1899-12-30", tz="GMT")
d$data<-as.Date(as.numeric(d$data), origin="1899-12-30")

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
                ),tabPanel("Case",
                          fluidRow(
                            column(2, 
                                   selectInput('op', 'Оператор',c("new", unique(d$operator)))),
                            column(2, #offset = 3,
                                   selectInput('d1', 'Дата разбора',c("new",'сейчас'))),
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
                          h4('Детали чека кейса'),
                          DT::dataTableOutput("x7")
    )
    #,tabPanel("video",tags$video(src = "video.mp4", type = "video/mp4", autoplay = NA, controls = NA)  )  
    )

server <- shinyServer(function(input, output, session){
  
  observeEvent(input$submit, {
    updateNavbarPage(session, "nav-page", "Case")
  })
  
  
  ########## OTL
  ####
  x <- reactiveValues()
  x$dt<- data.frame(case_id=0,resault=NA,object=NA,data=NA,check=NA,op=NA,d=NA,s=NA,com=NA,stringsAsFactors=FALSE)
  x$resault<-data.frame(case_id=0,resault=NA,object=NA,data=NA,check=NA,op=NA,d=NA,d2=NA,s=NA,com=NA,stringsAsFactors=FALSE)
  x$otl<-otl
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
  
  
  ###Case

  ###
  observeEvent(input$submit,{
    data <- dataInput()

    s4 <- input$x4_rows_selected
    dt<- if (length(s4)>0) {
      unique(data[s4,c('case_id','resault','object','data','check')] )
    } else data.frame(case_id =NA,resault='ручной',object=NA,data=NA,check=NA)
    
    dt[is.na(dt$case_id|| dt$case_id==0),'case_id']<-c(ifelse(max(x$resault$case_id)<=0,1,max(x$resault$case_id) +1))
    
    op<-if(input$op != "new"){  input$op} else ''
    d<-format(if(input$d1 != "new"){  Sys.time() } else '' ,'%d.%m.%Y %H:%M:%S')
    s<-input$s
    dt<-cbind(dt,op=c(op),d=c(d),s=c(s),com=c(NA))
    # dt<- if (nrow(dt[,"object"!='']) >0){ rbind(dt,dt1) } else dt1  
    x$dt<-dt
  })
 
   observeEvent(input$Add_row_head,{
     d=format(if(input$d1 != "new"){  Sys.time() } else '' ,'%d.%m.%Y %H:%M:%S')
     if (nrow(x$dt)>0) {
    new_row<-data.frame(case_id=c(max(x$resault$case_id) +1),resault='ручной',object=tail(x$dt["object"], n=1),data=tail(x$dt["data"], n=1),check=NA,op=NA
                        ,d=d,s=NA,com=NA)
    x$dt<-rbind(x$dt,new_row)
   } else 
     x$dt<- data.frame(case_id=0,resault=NA,object=NA,data=NA,check=NA,op=NA,d=d,s=NA,com=NA,stringsAsFactors=FALSE)
   
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
  
   if(input$op != "new"){ x$dt$op<- input$op}
   x$dt$s<-input$s
   x$dt[is.na(x$dt$resault),'resault']<-c('ручной')
  dt<- x$dt
  DT::datatable(dt, options = list(dom = 't')
            , editable = TRUE #,rownames = FALSE
            ,colnames = c('case_id','resault','Новый код',"Дата","Номер чека","Оператор","Кейс открыт","Статус",'Комментарий')
            )
  })
##
  observeEvent(input$x5_cell_edit, {
    info = input$x5_cell_edit
    str(info)
    i = info$row
    j = info$col 
    v = as.character(info$value)
    if (j %in% c(3,4,5,9)) { x$dt[i, j] <- isolate(DT::coerceValue(v, x$dt[i, j]))}
  })

  observeEvent(input$Save,{
    if (nrow(x$dt)>0) {
    dt<-x$dt
    dt$d2<- format(Sys.time(),'%H:%M:%S')
    dt<-dt[,c(1,2,3,4,5,6,7,10,8,9)]
    dt[is.na(dt$case_id) || dt$case_id==0,'case_id']<-ifelse(max(x$resault$case_id)<=0,1,max(x$resault$case_id) +1)
    x$resault <- rbind(x$resault,dt)
    #x$dt<-x$dt[-c(1:nrow(x$dt)),]
    d=format(if(input$d1 != "new"){  Sys.time() } else '' ,'%d.%m.%Y %H:%M:%S')
    
     cc<-dt[dt$resault!='ручной' & !is.na(dt$resault) ,c('case_id','object','data','check','s')]%>% unique()
     # if(nrow(cc)>0){
     # x$otl[x$otl$object %in% cc$object & x$otl$data %in% cc$data &
     #        x$otl$check %in% cc$check ,c('case_id','resault')]<-cc[cc$object %in% x$otl$object & cc$data %in% x$otl$data   &
     #                                                                cc$check %in% x$otl$check ,c('case_id','s')]
     # }
     if(nrow(cc)>0){
      p<- inner_join(x$otl,cc,by = c('object'='object','data'='data','check'='check') ,suffix = c("", ".y"))   %>%
              select(c('rowname','case_id.y','s'))
      x$otl[x$otl$rowname %in% p$rowname,c('case_id','resault')] <- p[,c('case_id.y','s')] 
     }
    x$dt<-data.frame(case_id=0,resault=NA,object=NA,data=NA,check=NA,op=NA,d=d,s=NA,com=NA,stringsAsFactors=FALSE)
   
    }
  }) 
  ##
  output$x6 <-  DT::renderDataTable(server = FALSE,{  
    dt<-x$resault
    
    datatable(dt
              ,rownames = FALSE
              ,colnames = c("case_id",'Старый статус','Новый код',"Дата","Номер чека","Оператор","Кейс открыт","Кейс закрыт","Статус",'Комментарий')
    ) 
    })
  
  output$x7 <-  DT::renderDataTable(server = FALSE,{ 
   
     cc<-x$dt[x$dt$resault!='ручной'& !is.na(x$dt$resault),c('object','data','check')]
     dt<-  if(nrow(cc)>0){
       inner_join(x$otl,cc,by = c('object'='object','data'='data','check'='check'))
     } else x$otl[0,]
    datatable(dt,rownames = FALSE
              , colnames = col_otl_old
               ) %>% formatStyle(0,target = "row", fontWeight = styleEqual(nrow(dt)+1, "bold")) %>%
      formatStyle("id",target = "row", backgroundColor = styleEqual(1, c('LightGrey')))%>% 
      formatStyle("resault","resault", backgroundColor = styleEqual(c('Нет фрода', 'Фрод'), c('LightGreen', 'Orange')))
  })
  
})
shinyApp(ui = ui, server = server)



