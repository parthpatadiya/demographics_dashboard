#server_demographics
library(shiny)
library(plotly)
library(reactablefmtr)
library(tidyverse)
library(gt)
library(gtsummary)
library(rintrojs)
library(huxtable)

rm(list = ls())
source("data.R")
shinyServer(function(input,output,session){
  
  steps <- data.frame(element = c("#filter_main_box",
                                  ".nav-tabs li:nth-child(1)",
                                  ".nav-tabs li:nth-child(2)",
                                  ".nav-tabs li:nth-child(3)"),
                      intro = c(
                        "This is a Sidebar for Filters. You can customize chart based on requirements and also can filter the data through out all charts & tables to see selected data only",
                        "This is Visulization tab where you can see the Demographical data as different stacked bar chart and box plots",
                        "This is detailed data table which is used to build this dashboard",
                        "This is Summarized tabuler Viwe of Demographics data by Study Site."
                        
                      )
  )
  
  introjs(session,options = list(steps=steps))
  
  
  
  
  colmn_names=list("SiteNumber"="Study Site Id","AGE_GROUP"="Age Group","ETHNIC"="Ethnicity","RACIAL_GROUP"="Race","SEX"="Gender")
  
  observeEvent(input$xaxis,{
    
    output$Subjects_per_Study_Site_Identifier_by_x<-renderText({paste("Subjects per",colmn_names[input$xaxis],"by",colmn_names[input$color])})
  })
  
  demog_react<-reactive({
    final_data<-as.data.frame(dm2_data)
    
    if((!(is.null(input$xaxis)))){
      column_filter<-input$xaxis
      final_data$xaxis=final_data[[column_filter]]
    }
    if((!(is.null(input$color)))){
      color_filter<-input$color
      final_data$color=final_data[[color_filter]]
    } 
    
    if(!(is.null(input$Site))){
      final_data<-final_data%>%filter(`Site` %in% input$Site)
    }
    
    if(!(is.null(input$subid))){
      final_data<-final_data%>%filter(`Subject` %in% input$subid)
    }
    
    return(final_data)
  })
  
  summary_table_react<-reactive({
    validate(need(nrow(demog_react())>0,'Data not available'))
    
    age_grp_tbl=demog_react() %>%tbl_cross(row =AGE_GROUP , col = StudyEnvSiteNumber, percent = "cell") %>%bold_labels()
    gender_tbl=demog_react() %>%tbl_cross(row =SEX , col = StudyEnvSiteNumber, percent = "cell") %>%bold_labels()
    ethnic_tbl=demog_react() %>%tbl_cross(row =ETHNIC , col = StudyEnvSiteNumber, percent = "cell") %>%bold_labels()
    racial_tbl=demog_react() %>%tbl_cross(row =RACIAL_GROUP , col = StudyEnvSiteNumber, percent = "cell") %>%bold_labels()
    summary_table=tbl_stack(list(age_grp_tbl,gender_tbl,racial_tbl,ethnic_tbl))|>
      modify_spanning_header(all_stat_cols() ~ "**Study Site Number**") 
    
    return(summary_table)
  })
  
  
  observeEvent(input$reset_filters,{
    
    updateRadioButtons(inputId = "xaxis",choiceValues =c("SiteNumber","AGE_GROUP","ETHNIC","RACIAL_GROUP","SEX") 
                       ,choiceNames = c("Study Site Id","Age Group","Ethnicity","Race","Gender"),selected ="SiteNumber" )
    updateRadioButtons(inputId = "color",choiceValues =c("Site","AGE_GROUP","ETHNIC","RACIAL_GROUP","SEX") ,choiceNames = c("Study Site","Age Group","Ethnicity","Race","Gender"),selected ="SEX" )
    updateCheckboxGroupInput(inputId = "Site",choices =  unique(dm2_data$Site ))
    updateCheckboxGroupInput(inputId = "subid",choices = unique(dm2_data$Subject))  
    
    
  })
  #filter_selection_checkboxes
  updateRadioButtons(inputId = "xaxis",choiceValues =c("SiteNumber","AGE_GROUP","ETHNIC","RACIAL_GROUP","SEX") ,choiceNames = c("Study Site Id","Age Group","Ethnicity","Race","Gender"),selected ="SiteNumber" )
  updateRadioButtons(inputId = "color",choiceValues =c("Site","AGE_GROUP","ETHNIC","RACIAL_GROUP","SEX") ,choiceNames = c("Study Site","Age Group","Ethnicity","Race","Gender"),selected ="SEX" )
  updateCheckboxGroupInput(inputId = "Site",choices =  unique(dm2_data$Site ))
  updateCheckboxGroupInput(inputId = "subid",choices = unique(dm2_data$Subject))  
  
  #stacked_bar_chart
  output$stacked_bar <- renderPlotly({
    req(demog_react()$xaxis)
    validate(need(nrow(demog_react())>0,'Data not available'))
    trns <- transform(demog_react(), `xaxis`= as.character(demog_react()$xaxis))
    group <- aggregate(trns$Subject, by=list(Category=trns$xaxis, cat2=trns$color), FUN=length)
    validate(need(nrow(group)>0,'Data not available'))
    
    
    bar <- plot_ly(x = group$Category, y = group$x, type = 'bar',name = group$cat2, color = group$cat2)%>%
      layout(yaxis = list(title = 'Number of Subjects'), xaxis = list(title = input$xaxis, categoryorder = "total descending"), 
             barmode = "stack")%>% config(displayModeBar = F)
    
    bar
    
  })
  
  #boxplot_1
  output$age_by_ss <- renderPlotly({
    req(demog_react())
    validate(need(nrow(demog_react())>0,'Data not available'))
    
    box1 <- plot_ly(
      y = demog_react()$AGE,
      x = demog_react()$SiteNumber,
      type = "box",
      boxmean=TRUE
    )%>%
      layout(xaxis = list(title = 'Study Site Id'), yaxis = list(title = 'Age'))%>% config(displayModeBar = F)
    box1
    
  })
  
  #boxplot_2
  
  output$ss_by_gender <- renderPlotly({
    req(demog_react())
    validate(need(nrow(demog_react())>0,'Data not available'))
    
    box2 <- plot_ly(
      y = demog_react()$AGE,
      x = demog_react()$SEX,
      type = "box"
    )%>%
      layout(yaxis = list(title = 'Age'), xaxis = list(title = 'Gender'))%>% config(displayModeBar = F)
    box2
    
  })
  
  #table_details
  
  output$dem_details <- renderReactable({
    req(demog_react())
    validate(need(nrow(demog_react())>0,'Data not available'))
    
    dt <- reactable(data = demog_react()[c("project","Subject","SiteNumber","Site","StudyEnvSiteNumber","AGE","SEX","ETHNIC","AGE_GROUP","RACIAL_GROUP")], 
                    
                    showSortable = TRUE,
                    searchable = TRUE,
                    striped = TRUE,
                    highlight = TRUE,
                    bordered = TRUE,
                    resizable = TRUE,
                    pagination = T,defaultPageSize = 12,
                    fullWidth = TRUE,
                    width = "auto",
                    defaultColDef = colDef(
                      align = "center"),
                    theme = reactableTheme(
                      headerStyle = tags$style("asdad:asad; color: #f8ffff;  background-color: #66b1b3; font-weight: bold;")
                      ,tableStyle = tags$style("asdad:asad;     font-size: smaller;") ,
                      borderColor = "#dfe2e5",
                      stripedColor = "#f6f8fa",
                      highlightColor = "#f2ffff",
                      searchInputStyle = list(width = "100%"),
                      style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
                    )
    )
    dt        
    
  })  
  
  
  output$summary_table<- render_gt(
    expr = summary_table_react()%>%as_gt(),width = "100%",height = "100%"
  )
  
  save__summary_report <- reactive({
    
    outfile <- tempfile(fileext = ".rtf") 
    tb_hux=as_hux_table(summary_table_react())
    
    huxtable::quick_rtf(
      tb_hux,
      file = outfile
    )
    
    outfile
    
  })
  
  output$download_report <- downloadHandler(
    filename = "summary_report.rtf",
    
    content = function(file) {
      file.copy(save__summary_report(), file)
      
    }
  )

  
})
