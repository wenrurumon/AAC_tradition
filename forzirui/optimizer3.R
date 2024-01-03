
rm(list=ls())
library(dplyr)
library(reshape2)
library(data.table)
library(ggplot2)
library(openxlsx)
library(shiny)
library(DT)

################################################
# 数据库
################################################

setwd('/Users/huzixin/Documents/mcd/moma/version1231')
load('curves.rda')
load('roisummary.rda')
load('benchmarks.rda')
load('strategy3.rda')

################################################
# 前端
################################################

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # numericInput("budget_target","Budget Target:",value = 7644355,min = 0,max = Inf),
      # numericInput("sales_uplift","Sales Effectiveness Target:",value = 1,min = 0,max = Inf),
      # numericInput("user_uplift","User Effectiveness Target:",value = 1,min = 0,max = Inf),
      # numericInput("reach_uplift","Reach Effectiveness Target:",value = 1,min = 0,max = Inf),
      sliderInput("budget_target","Budget Target:",value = 7644355,min = 4000000,max = 10000000),
      sliderInput("sales_uplift","Sales Effectiveness Target:",value = 1,min = 0,max = 2,step=0.1),
      sliderInput("user_uplift","User Effectiveness Target:",value = 1,min = 0,max = 2,step=0.1),
      sliderInput("reach_uplift","Reach Effectiveness Target:",value = 1,min = 0,max = 2,step=0.1),
      radioButtons("min_budget","Minimize Budget under Constrains",choices = c("Yes","No")),
      conditionalPanel(
        condition = "input.min_budget == 'No'",
        sliderInput("sales_weight","Objective Weights on Sales:",value = 0.6,min = 0,max = 1),
        sliderInput("user_weight","Objective Weights on User:",value = 0.2,min = 0,max = 1),
        sliderInput("reach_weight","Objective Weights on Reach:",value = 0.2,min = 0,max = 1)
      )
    ),
    mainPanel(
      hr(),
      tags$h3("Simulation Summary"),
      hr(),
      plotOutput("output3"),
      hr(),
      tags$h3("Simulation Decomp"),
      hr(),
      DT::dataTableOutput("output1"),
      hr(),
      plotOutput("output2"),
      hr(),
    )
  ),
)

################################################
# 服务端
################################################

server <- function(input,output,session){
  
  
  max.benchmark <- reactive({
    
    req(input$budget_target)
    
    budget_target <- input$budget_target
    
    data.table(
      max.reachroi = (strategy.maxreach %>% filter(spd<=budget_target) %>% arrange(desc(spd)))$dreach[1],
      max.salesroi = (strategy.maxsales %>% filter(spd<=budget_target) %>% arrange(desc(spd)))$dsales[1],
      max.userroi = (strategy.maxuser %>% filter(spd<=budget_target) %>% arrange(desc(spd)))$duser[1]
    ) %>%
      mutate(
        max.reach = max.reachroi * budget_target,
        max.sales = max.salesroi * budget_target,
        max.user = max.userroi * budget_target
      ) %>%
      mutate(max.reach = ifelse(max.reach>100,100,max.reach))
    
  })
  
  strategy1 <- reactive({
    
    req(input$budget_target)
    req(input$sales_uplift)
    req(input$user_uplift)
    req(input$reach_uplift)

    budget_target <- input$budget_target
    targets.uplift <- c(reach=input$reach_uplift,
                       sales=input$sales_uplift,
                       user=input$user_uplift)
    
    targets <- budget_target*benchroi$roi*targets.uplift
    
    strategyi <- strategy3 %>%

      filter(spd<=budget_target,reach>=targets[1],sales>=targets[2],user>=targets[3]) %>%
      arrange(spd)
    
    pathi <- path3 %>% filter(code==strategyi$code[1]) %>% arrange(desc(spd))
    
    rbind(
      pathi %>%
        summarise(x='Total',spd=sum(spd),
                  drive_Reach=sum(drive_Reach),drive_Sales=sum(drive_Sales),drive_User=sum(drive_User)) %>%
        mutate(roi_Reach=drive_Reach/spd,roi_Sales=drive_Sales/spd,roi_User=drive_User/spd),
      pathi %>% select(-code)
    ) %>%
      select(`Media Type`=x,Spending=spd,
             Sales=drive_Sales,User=drive_User,Reach=drive_Reach,
             roi_Sales,roi_User,roi_Reach)
    
  })
  
  strategy2 <- reactive({
    
    req(input$budget_target)
    req(input$sales_uplift)
    req(input$user_uplift)
    req(input$reach_uplift)
    req(input$sales_weight)
    req(input$user_weight)
    req(input$reach_weight)
    
    budget_target <- input$budget_target
    
    max.benchmark <- max.benchmark()
    
    targets.uplift <- c(reach=input$reach_uplift,
                       sales=input$sales_uplift,
                       user=input$user_uplift)
    weights <- c(reach=input$reach_weight,
                 sales=input$sales_weight,
                 user=input$user_weight)
    
    targets <- budget_target*benchroi$roi*targets.uplift
    targets.prop <- targets/unlist(max.benchmark)[4:6]
    
    strategyi <- strategy3 %>%
      filter(spd<=budget_target,reach>=targets[1],sales>=targets[2],user>=targets[3]) %>%
      mutate(score_reach=reach/unlist(max.benchmark)[4],
             score_sales=sales/unlist(max.benchmark)[5],
             score_user=user/unlist(max.benchmark)[6]) %>%
      mutate(score=score_reach*weights[1]+score_sales*weights[2]+score_user*weights[3]) %>%
      arrange(desc(score))
    
    pathi <- path3 %>% filter(code==strategyi$code[1]) %>% arrange(desc(spd))
    rbind(
      pathi %>%
        summarise(x='Total',spd=sum(spd),
                  drive_Reach=sum(drive_Reach),drive_Sales=sum(drive_Sales),drive_User=sum(drive_User)) %>%
        mutate(roi_Reach=drive_Reach/spd,roi_Sales=drive_Sales/spd,roi_User=drive_User/spd),
      pathi %>% select(-code)
    ) %>%
      select(`Media Type`=x,Spending=spd,
             Sales=drive_Sales,User=drive_User,Reach=drive_Reach,
             roi_Sales,roi_User,roi_Reach)
    
  })
  
  output$output1 <- renderDataTable({
    req(input$min_budget)
    
    if(input$min_budget=='Yes'){
      output1 <- strategy1() %>% as.data.frame
    } else {
      output1 <- strategy2() %>% as.data.frame
    }
    
    out <- data.table(
      output1[,1,drop=F],
      apply(output1[,2:6],2,function(x){prettyNum(round(x,2),big.mark=',')}),
      apply(output1[,7:8],2,function(x){format(x,scientific=T)})
    )
    
    datatable(out,options = list(pageLength = nrow(iris), paging = FALSE, dom='t'))
    
  })
  
  output$output2 <- renderPlot({
    
    req(input$min_budget)
    
    if(input$min_budget=='Yes'){
      pathi <- strategy1() %>% as.data.frame
    } else {
      pathi <- strategy2() %>% as.data.frame
    }
    
    spt %>%
      select(x,average_spending=mean,executive_spending=exe) %>%
      merge(pathi %>% select(x=1,suggested_spending=Spending) %>% filter(x!='Total'),all=T) %>%
      melt() %>%
      ggplot() + 
      geom_point(aes(x=value/1000000,y=x,colour=variable,shape=variable),size=3) +
      theme_bw() + 
      labs(x='Spending (MRMB)',y='',colour='',shape='') +
      theme(text=element_text(size=15))
    
  })
  
  output$output3 <- renderPlot({
    
    req(input$min_budget)
    
    if(input$min_budget=='Yes'){
      pathi <- strategy1() %>% as.data.frame
    } else {
      pathi <- strategy2() %>% as.data.frame
    }
    
    out <- unlist(pathi %>% filter(`Media Type`=='Total') %>% select(-1))/
      c(benchroi$spd[1],benchroi[c(2,3,1),]$drive,benchroi[c(2,3,1),]$roi)
    
    out <- round(out,2)
    
    data.table(Attribute=names(out),Change=out) %>%
      mutate(Attribute=factor(Attribute,rev(names(out)))) %>%
      mutate(class=c(1,2,3,4,2,3,4)) %>%
      ggplot() +
      geom_col(aes(y=Attribute,x=Change,fill=paste(class)),colour='black') +
      geom_label(aes(y=Attribute,x=Change,
                     label=ifelse(Change>1,paste0('+',round(Change,2)*100-100,'%'),paste0(round(Change,2)*100-100,'%'))),
                 size=5) +
      theme_bw() + 
      theme(text=element_text(size=15,face='bold'),legend.position='none',axis.text.x=element_text(size=0)) +
      labs(x='',y='')
    
  })
  
}

shinyApp(ui, server,options = list(host = "0.0.0.0",port = 2222))
