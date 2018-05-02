
#devtools::install_github("timelyportfolio/d3treeR")

navbarPage(title='Analysis of Open Payments Made by Pharma to Healthcare Providers',collapsible=TRUE,
           id='nav',
           theme=shinytheme('slate'),
           
           tabPanel('State View',
                   #div(class='outer'),
                    
                    fluidRow(column(12,align="center",
                            selectizeInput("Specialty_final", "Select Specialty",
                                                                 c('All', 'Cardiology','Endocrinology, Diabetes & Metabolism',
                                                                   'Gastroenterology','Orthopaedics','Oncology', 'Psychiatry & Neurology',
                                                                   'Surgery'
                                                                 ))        
                      
                    )),
                    fluidRow(box(
                        plotlyOutput(outputId = 'State',width = '100%',height = '100%'), height=400),
                        box(
                          plotlyOutput(outputId = 'State_Avg',width = '100%',height = '100%'),height = 400))),
                      
              
            tabPanel('Companies and Products',
                    fluidRow(
                      column(2,
                             br(),
                             align="left",
                             radioButtons("cmpny","Select Specialty", 
                                          choices =  c('All', 'Cardiology','Endocrinology, Diabetes & Metabolism',
                                                                              'Gastroenterology','Orthopaedics','Oncology', 'Psychiatry & Neurology',
                                                                              'Surgery'))
                             ),
                      column(8,
                             br(),
                             plotOutput(outputId="Company"),
                             offset = 0.5
                      )),
                      # column(7,
                      #        br(), 
                      #        plotOutput(outputId="Nature"),
                      #        offset = 0.5
                      # )),

                    fluidRow(
                      column(2,
                             br(),
                             selectizeInput("productcheckbox","Select Product Type",
                                            choices = c('Drugs','Medical Supplies'))),

                        column(8,
                               br(),
                        plotlyOutput(outputId = 'Product'),offset = 0.5))
                    
           )

)
