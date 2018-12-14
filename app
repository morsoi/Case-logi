


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
                tabPanel("Сводная",
                         fluidRow(
                  column(2, 
                         selectInput('o0', 'Оператор',c("All", unique(d$operator)))),
                  column(2, #offset = 3,
                         selectInput('tp0',multiple = TRUE, 'Направление',c(unique(d$type)))),
                  column(2, #offset = 3,
                         selectInput('d0', 'Дата',c("All",sort(unique(substr(d$data,1,10)))))),
                  column(2, #offset = 3,
                         selectInput('t0', 'Статус',multiple = TRUE,c(unique(d$resault))))
                ),
                hr(),
                         fluidRow(
                           column(width=6,h4('Сводная по направлениям'),
                                  DT::dataTableOutput("x1", height = 300)),
                           column(width=3,h4('Сводная по операторам'),
                                  DT::dataTableOutput("x2", height = 300))
                         ), hr(),
                         h4('Детальная информация по кейсам'),
                         DT::dataTableOutput("x3")
                ),
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
                                   selectInput('tp', 'Тип фрода',c("new",'Другое',"отложенные",'аннулированные'))),
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
                          #DT::dataTableOutput("x7"),
                          h4('Детали чека, аннулированные')
                         # DT::dataTableOutput("x8")
    )
    #,tabPanel("video",tags$video(src = "video.mp4", type = "video/mp4", autoplay = NA, controls = NA)  )  
    )

server <- shinyServer(function(input, output, session){
  
  
  ########## OTL
  ####
  x <- reactiveValues()
  x$dt0 <- data.frame(operator=NA,type=NA,object=NA,data=NA,in_time=NA,end_time=NA,check=NA,pay=0
                     ,date_vie=as.Date(Sys.time()),start_vie=format(Sys.time(),'%H:%M:%S')
                     ,end_vie=NA,delta_t_vie=NA,s_com=NA,com=NA,resault=as.character(NA)
                     ,pre_type=NA,user_id=NA,user_name=NA,respon=NA,zer=NA,case_id =0,t=1,stringsAsFactors=FALSE)
  
  x$dt <- data.frame(operator=NA,type=NA,object=NA,data=NA,in_time=NA,end_time=NA,check=NA,pay=0
                      ,date_vie=as.Date(Sys.time()),start_vie=format(Sys.time(),'%H:%M:%S')
                      ,end_vie=NA,delta_t_vie=NA,s_com=NA,com=NA,resault=as.character(NA)
                      ,pre_type=NA,user_id=NA,user_name=NA,respon=NA,zer=NA,case_id =0,t=1,stringsAsFactors=FALSE)
  x$d<-d
  x$otl<-otl
  x$an<-an
  
  ### x1
  dataInput_d<- reactive({  
    data<-x$d
    if(input$o0 != "All"){  data<-filter(data,operator %in% input$o0) }
    if(length(input$tp0)){  data<-filter(data,type %in% input$tp0) } 
    if(input$d0 != "All"){  data<-filter(data,substr(data,1,10) %in% input$d0) }
    if(length(input$t0)){  data<-filter(data,resault %in% input$t0) }
    data
    
  })

  ## x1
  output$x1 <-  DT::renderDataTable(server = FALSE,{
  d<-dataInput_d()
      dt<- d %>% group_by(type) %>% 
        summarise (all = n(),f = sum(ifelse(resault=='Фрод',1,0)),nf = sum(ifelse(resault=='Нет фрода',1,0))
                   ,w = sum(ifelse(resault=='В работе',1,0)),c = sum(ifelse(resault=='Закрыто',1,0)))
    
    datatable(dt
              , colnames = c('Направление', 'Кол-во всего', 'Кол-во фрода', 'Кол-во не фрода','В работе','Закрыто')
              ,options = list(dom = 't'),rownames = FALSE
    ) %>% formatRound(1:6,0)  %>% 
      formatStyle(0,target = "row", fontWeight = styleEqual(nrow(dt)+1, "bold"))
    
  })
  ### x2 
  output$x2 <-  DT::renderDataTable(server = FALSE,{
    d<-dataInput_d()
    dt<-d%>% group_by(operator) %>% 
      summarise (pay = sum(pay, na.rm = TRUE),n = n(),c_object = n_distinct(object)
                 ,fraud = sum(ifelse(resault=='Фрод',1,0))
                 ,not_fraud = sum(ifelse(resault=='Нет фрода',1,0))
                 ,pr = sum(ifelse(resault=='Фрод',1,0)) / n() )
    
    datatable(dt
              , colnames = c('Оператор', 'Сумма ущерба', 'Кол-во случаев', 'Кол-во магазинов','Фрод','Нет фрода','% выявления')
              ,options = list(dom = 't'),rownames = FALSE
    ) %>% formatRound(1:4,0)  %>%  formatPercentage(7,0)  %>% 
      formatStyle(0,target = "row", fontWeight = styleEqual(nrow(dt)+1, "bold"))
    
  })

  #### x3
  output$x3 <-  DT::renderDataTable(server = FALSE,{
    du <- dataInput_d()
    
    datatable(du
              , colnames = col_d_old,rownames = FALSE
              ,extensions =c ('ColReorder', 'Buttons', 'FixedHeader') 
              , options=list(fixedHeader = TRUE, 
                             # columnDefs = list(list(width = '60%', targets = c(13,14,16,20),
                             #                                            render = JS("function(data, type, row, meta) {",
                             #                                                        "return type === 'display' && data.length > 20 ?",
                             #                                                        "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                             #                                                        "}"
                             #                                            ))),
                             dom = 'Blfrtip',   
                             buttons = 
                               list( I('colvis'), list(
                                 extend = 'collection',
                                 buttons = c('csv', 'excel','pdf'),
                                 text = 'Download'
                               )),
                             colReorder = TRUE)
    )%>% formatRound(8,0) %>% formatDate(c(5,9), "toLocaleDateString") %>%
      # %>% formatDate(c(5,6,10,11), "toLocaleTimeString", params="GMT")
      formatStyle(0,target = "row", fontWeight = styleEqual(nrow(du)+1, "bold")) %>% 
      formatStyle("resault","resault", backgroundColor = styleEqual(c('Нет фрода', 'Фрод'), c('LightGreen', 'Orange')))
    
  })
 #####
  updateSelectInput(session, "t",
                    #choices = s_options,
                    selected = ,c("All", unique(an$resault))
  )
  
  
  ### Общие фильтры
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
              , options=list(fixedHeader = TRUE
                             , columnDefs = list(list(width = '60%', targets = c(10,11,12),
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
  
  observeEvent(input$submit, {
    updateNavbarPage(session, "nav-page", "Case")
  })
  observeEvent(input$submit2, {
    updateNavbarPage(session, "nav-page", "Case")
  })
  ###
  observeEvent(input$submit,{
    data <- dataInput()

    s4 <- input$x4_rows_selected
     if (length(s4)>0) {
     dt<- unique(data[s4,c('object','data','check','pay','user_id','user_name','resault','case_id')] )
     dt<-cbind(dt,operator=c(NA),type=c('отложенные'),in_time=c(NA),end_time=c(NA)
               ,date_vie=as.Date(Sys.time())
               ,start_vie=format(Sys.time(),'%H:%M:%S')
               ,end_vie=c(NA),delta_t_vie=c(NA),s_com=c(NA),com=c(NA),pre_type=c(NA)
               ,respon=c(NA),zer=c(NA),t=c(0)) %>% 
       select(c('operator','type','object','data','in_time','end_time','check','pay','date_vie','start_vie','end_vie'
                ,'delta_t_vie','s_com','com','resault','pre_type','user_id','user_name','respon','zer','case_id','t'))
 
      # dt[is.na(dt$case_id) || dt$case_id==0,'case_id']<-c(max(x$d$case_id) + 1)
       
      if(nrow(x$dt[x$dt$t!=1 & !is.na(x$dt$t),])>0) { x$dt<-rbind(x$dt,dt)} else x$dt<-dt
      } 
  })
  ###
  observeEvent(input$submit2,{
    data <- x$an

    s5 <- input$xa_rows_selected
    if (length(s5)>0) {
      dt<- unique(data[s5,c('object','data','in_time','end_time','pay','user_id','resault','case_id')] )
      dt<-cbind(dt,operator=c(NA),type=c('аннулированные'),check=c(NA),user_name=c(NA)
                ,date_vie=as.Date(Sys.time())
                ,start_vie=format(Sys.time(),'%H:%M:%S')
                ,end_vie=c(NA),delta_t_vie=c(NA),s_com=c(NA),com=c(NA),pre_type=c(NA)
                ,respon=c(NA),zer=c(NA),t=c(0)) %>% 
        select(c('operator','type','object','data','in_time','end_time','check','pay','date_vie','start_vie','end_vie'
                 ,'delta_t_vie','s_com','com','resault','pre_type','user_id','user_name','respon','zer','case_id','t'))

      # dt[is.na(dt$case_id || dt$case_id==0),'case_id']<-c(max(x$d$case_id) + 1)
      if(nrow(x$dt[x$dt$t!=1 & !is.na(x$dt$t),])>0) { x$dt<-rbind(x$dt,dt)} else x$dt<-dt
    } #else data.frame(case_id =NA,resault='ручной',object=NA,data=NA,check=NA)
  })

    observeEvent(input$Add_row_head,{

     #case_id<-c( ifelse(max(x$dt$case_id)>=max(x$d$case_id),max(x$dt$case_id) + 1,max(x$d$case_id) +1))
     new_row<- x$dt0
     
     if (nrow(x$dt)>0) {
    x$dt<-rbind(x$dt,new_row)
   } else 
     x$dt<- new_row
   
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
    
    if(input$op != "new"){ x$dt$operator<- input$op}
    if(nrow(x$dt)>0){x$dt$resault<-input$s}
    x$dt$type<-as.character(x$dt$type)
    if(nrow(x$dt)>0){x$dt[is.na(x$dt$type),'type']<-c('другое')}

    if(input$tp != "new" ){ 
      x$dt[x$dt$t %in% c(1,NA) ,'type'] <- input$tp} 

  dt<- x$dt
  DT::datatable(dt, options = list(dom = 't')
            , editable = TRUE ,rownames = FALSE
            ,colnames = col_d_old
            )
  })
##
  observeEvent(input$x5_cell_edit, {
    info = input$x5_cell_edit
    str(info)
    i = info$row
    j = info$col 
    v = as.character(info$value)
    if (j %in% c(4,5,6,7,8,9,13,14,15,17,18,19,20,21)) { x$dt[i, j] <- isolate(DT::coerceValue(v, x$dt[i, j]))}
  })
  observeEvent(input$Save,{

    if (nrow(x$dt)>0) {
    dt<-x$dt
    dt$end_vie<- format(Sys.time(),'%H:%M:%S')
    dt$data<-as.Date(dt$data,'%d.%m.%Y') 
    dt$start_vie<-as.character(dt$start_vie)
   dt$pay<-as.numeric(dt$pay)
   dt$type<-as.character(dt$type)
     if(nrow(dt[dt$case_id >0,])>0){
       
       c_a<-dt[dt$type =='аннулированные' & dt$case_id >0,]%>% unique() 
       c_o<-dt[dt$type =='отложенные' & dt$case_id >0,]%>% unique()
       x$an[x$an$case_id %in% c_o$case_id,c('resault')] <- c_a$resault 
       x$otl[x$otl$case_id %in% c_o$case_id,c('resault')] <- c_o$resault
    
       x$d[x$d$case_id %in% dt[dt$case_id >0,c('case_id')],] <- dt[dt$case_id >0,]%>% unique() 
       
     }
     
     # if(nrow(cc[dt$case_id ==0,])>0){
     #   cc[dt$case_id ==0,'case_id']<-max(x$d$case_id) +1
     #  p<- inner_join(x$otl,cc,by = c('object'='object','data'='data','check'='check') ,suffix = c("", ".y"))   %>%
     #          select(c('rowname','case_id.y','s'))
     #  x$otl[x$otl$rowname %in% p$rowname,c('case_id','resault')] <- p[,c('case_id.y','s')]
     # }

      
      
      # cc3<-filter(dt,resault %in% 'ручной') %>% unique()
      # if(nrow(cc3)>0){
      #   cc4<- data.frame(operator=cc3$op
      #                    ,type=cc3$op
      #                    ,object=cc3$object
      #                    ,data=cc3$data
      #                    ,start_t=cc3$in_time
      #                    ,end_t=NA
      #                    ,check=cc3$check
      #                    ,pay=NA,date_vie=cc3$d
      #                    ,start_vie=cc3$d
      #                    ,end_vie=d
      #                    ,delta_t_vie=NA,s_com=cc3$com,com=NA
      #                    ,resault=cc3$s,pre_type=NA,id_user_fraud=NA,fio_user_fraud=NA,respon=NA,zer=NA )
      #   x$d <- rbind(x$d,cc4)
    }
    x$dt <- x$dt0
    
  })
  output$x6 <-  DT::renderDataTable(server = FALSE,{  
    (str(x$dt))
  })
  # ##
  # output$x6 <-  DT::renderDataTable(server = FALSE,{  
  #   dt<- filter(x$resault,case_id >0)
  #   #dt<-dt[,c(1,2,3,4,5,6,7,8,9,12,10,11)]
  #   datatable(dt
  #             ,rownames = FALSE
  #             ,colnames = c("case_id",'Старый статус','Новый код',"Дата","Номер чека",'in_time','Тип фрода',"Оператор","Кейс открыт","Статус",'Комментарий',"Кейс закрыт")
  #   ) 
  #   })
  # 
  # output$x7 <-  DT::renderDataTable(server = FALSE,{
  # 
  #    cc<-x$dt[x$dt$resault!='ручной'& !is.na(x$dt$resault) & x$dt$tp =='Отложенные',c('object','data','check')]
  #    dt<-  if(nrow(cc)>0){
  #      inner_join(x$otl,cc,by = c('object'='object','data'='data','check'='check'))
  #    } else x$otl[0,]
  # 
  #   datatable(dt,rownames = FALSE
  #             , colnames = col_otl_old
  #              ) %>% formatStyle(0,target = "row", fontWeight = styleEqual(nrow(dt)+1, "bold")) %>%
  #     formatStyle("id",target = "row", backgroundColor = styleEqual(1, c('LightGrey')))%>%
  #     formatStyle("resault","resault", backgroundColor = styleEqual(c('Нет фрода', 'Фрод'), c('LightGreen', 'Orange')))
  # })
  # output$x8 <-  DT::renderDataTable(server = FALSE,{
  #   
  #   cc<-x$dt[x$dt$resault!='ручной'& !is.na(x$dt$resault) & x$dt$tp =='Аннулированные',c('object','data','in_time')]
  #   dt<-  if(nrow(cc)>0){
  #     inner_join(x$an,cc,by = c('object'='object','data'='data','in_time'='in_time'))
  #   } else x$an[0,]
  #   
  #   datatable(dt,rownames = FALSE
  #             , colnames = col_an_old
  #   ) %>% formatStyle(0,target = "row", fontWeight = styleEqual(nrow(dt)+1, "bold")) %>%
  #     formatStyle("id",target = "row", backgroundColor = styleEqual(1, c('LightGrey')))%>%
  #     formatStyle("resault","resault", backgroundColor = styleEqual(c('Нет фрода', 'Фрод'), c('LightGreen', 'Orange')))
  # })
  
})
shinyApp(ui = ui, server = server)



