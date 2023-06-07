library(shiny)
library(shinydashboard)
library(shinyBS)
library(plotly)
library(shinythemes)
library(reactable)
library(shinycssloaders)
library(gt)
require(rintrojs)
dbHeader <- headerPanel(title =  
                          tags$nav( tags$head(tags$title("Demographics"))
                                    
                                    ,tags$h2("Demographics", style="font-size: larger;
                        vertical-align: -webkit-baseline-middle;
                        line-height: normal;
                        padding: 0px;
                        margin:0px;
                        color: #3e9fb5;
                        font-weight: bold;
                      text-align: -webkit-center;")
                                    
                                    
                          )%>%tagAppendAttributes(class = 'header_panel') )

dbBody <-dashboardBody(
  introjsUI(),
           fluidRow( height="80vh",
                 
                     #checkboxes_for_filter_selection
                 
             column(width = 2, height = "80vh",
                    box(title="Filter Selection",width = "100%", align="center",
                        actionButton(inputId = "reset_filters",label = "Reset Filter",icon = icon("refresh",verify_fa = FALSE)),
                        
                         
                        radioButtons(inline = T,inputId = "xaxis",label = "X Axis", 
                                            choices = c(""))%>%tagAppendAttributes(class = 'filter_chk_box'),
                       
                        radioButtons(inline = T,inputId = "color",label = "Color By", 
                                     choices = c(""))%>%tagAppendAttributes(class = 'filter_chk_box'),
                        
                        checkboxGroupInput(inline = T,inputId = "Site",label = "Site", 
                                           choices = c(""))%>%tagAppendAttributes(class = 'filter_chk_box'),
                        
                        checkboxGroupInput(inline = T,inputId = "subid",label = "Unique Subject ID",
                                           choices = c(""))%>%tagAppendAttributes(class = 'filter_chk_box')
                    ),

                        
                   )%>%tagAppendAttributes(id = 'filter_main_box'),#filter column 
             
             tabBox(
               title = "",
               id = "tabset", height = "78vh", width = 10,
               tabPanel("Visulization",type = "pills",
                        #stacked_bar_chart
                        column(width = 7,
                               fluidRow(h2(textOutput(outputId = "Subjects_per_Study_Site_Identifier_by_x",inline = F),class="box-title",align="center",),
                                        plotlyOutput(height="75vh",width ="100%",outputId = "stacked_bar")%>%withSpinner(color="#0dc5c1",hide.ui = FALSE,type = 6))%>%tagAppendAttributes(id = 'plot_box')
                               
                        ),
                        
                        #boxplots
                        column(width = 5,
                               fluidRow(h2("Age by Study Type Identifier",class="box-title",align="center"),
                                        plotlyOutput(height="34vh",width ="100%",outputId = "age_by_ss")%>%withSpinner(color="#0dc5c1",hide.ui = FALSE,type = 6))%>%tagAppendAttributes(id = 'plot_box'),
                               fluidRow(h2("Age by Gender",class="box-title",align="center"),
                                        plotlyOutput(height="34vh",width ="100%",outputId = "ss_by_gender")%>%withSpinner(color="#0dc5c1",hide.ui = FALSE,type = 6))%>%tagAppendAttributes(id = 'plot_box')
                               
                        ) 
                        
                        
                        ),
               tabPanel("Table View", 
                        column(width = 12,
                        fluidRow( h2("Demographics Details",class="box-title",align="center"), height = "75vh",width=12,reactableOutput(outputId = "dem_details",height = "75vh")%>%withSpinner(color="#0dc5c1",hide.ui = FALSE,type = 6))%>%tagAppendAttributes(id = 'plot_box')
                        )
                        
                        ),
               tabPanel("Summary Table",
                        column(width = 12,
                               fluidRow( h2("Demographics Summary",class="box-title",align="center"), height = "75vh",width=12,gt_output(outputId = "summary_table")%>%withSpinner(color="#0dc5c1",hide.ui = FALSE,type = 6))%>%tagAppendAttributes(id = 'plot_box')
                        )
                        
                        )
             ),
             
          
             
           ),

           #details_table
           
         # fluidRow( h2("Details",class="box-title",align="center"), height = "89vh",width=12,reactableOutput(outputId = "dem_details"))%>%tagAppendAttributes(id = 'plot_box')
)

shinyUI( fluidPage(theme = shinytheme("cerulean"),dbHeader,dbBody,
                   
                   
                   
    #css_tags_for_styling               
                   
                   
                   tags$style("
                   
                   .header_panel{
                          box-shadow: 4px 5px 12px -1px #8888;
                          width: 104%;
                          margin-left: -29px;
                          margin-bottom: 10px;
                          padding-bottom: 6px;
                    }
                   
                   ::-webkit-scrollbar {
                                width: 10px;
                              }

                              /* Track */
                              ::-webkit-scrollbar-track {
                                background: #f1f1f1;
                              }

                              /* Handle */
                              ::-webkit-scrollbar-thumb {
                                background: #888;
                              }

                              /* Handle on hover */
                              ::-webkit-scrollbar-thumb:hover {
                                background: #555;
                              }
                      #filter_main_box{ min-height:48vh;
    
}
                              
                              .box-title{
                                  font-size: xx-large;
    background-color: #5f9da0;
    color: white;
    height: 37px;
    margin-top: 5px;
    text-align: center;
    vertical-align: middle;
                              }
                              
                              #filter_main_box .box{
                              padding-top: 0px !important;
                              margin-top: 0px !important;
                                  height: 89vh;
                                  overflow: auto;
    margin-bottom: 4px !important;
                              }

                               .filter_chk_box .box{
                              padding-top: 0px !important;
                              }

                   .filter_chk_box .box .box-header .box-title{
                              /* background-color: #3f8c8c; */
                              margin-top: 0px;
                              color:#faebd7;

                   }
                   .filter_chk_box .control-label{
                       color: white;
    background-color: cadetblue;
    width: -webkit-fill-available;
                   }
                   
                   
                   .nav-tabs>li.active>a:focus {
                   color: #555555;
                   }


                         .container-fluid .col-sm-12 h1{
                         margin:0px;
                         }

                         .small-box {margin-top:5px;
                         height: auto;}

                         #last_visit_date{
                         width:auto;
                         }

                         .small-box .inner {
                         padding: 9px 7px 7px 7px;
                         min-width: 20vh;
                         }

.filter_chk_box .box .box-header .box-title{
                              background-color: #3f8c8c;
                              margin-top: 0px;
                              color:#faebd7;

                   }

                         .plot-container .svg-container .main-svg{

                         background: rgba(255,255,255,0.0) !important;
                         }

                         .col-sm-6 {
                            padding-left: 1px !important;
                            padding-right: 1px !important;

                         }

                        .box , #plot_box {
                            border-radius: 4px;
                            margin-right: 6px !important;
                             box-shadow: 4px 5px 12px -1px #8888;
                        }
                        
                               .filter_chk_box{
                                
                               }
                              
                              .radio, .checkbox {
                                position: relative;
                                display: block;
                                margin-top: -3px;
                                margin-bottom: 1px;
                                color: black;
                              }
                              #Study .checkbox,
                              #Site .checkbox,
                              #xaxis .checkbox,
                              #color .checkbox,
                              #subid .checkbox
                              {
                                text-align: left;
                                padding-left: 10px;
                                padding-right: 2px;
                              }
                              #Study,
                              #Site,
                              #Planned_arm,
                              #xaxis,
                              #color,
                              #subid
                              {
                                max-height: 20vh;
                                width: 100%;
                                overflow: auto;
                                text-align: left;
                                box-shadow: 4px 5px 12px -1px #8888;
                              } 
                              
                              #Site-label,
                              #xaxis-label,
                              #color-label,
                              #subid-label
                              {
                              text-align:center;
                              }
")))
                   