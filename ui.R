
navbarPage(title='Analysis of Open Payments Made by Pharma to Healthcare Providers',collapsible=TRUE,
           id='nav',
           theme=shinytheme('slate'),
           
           tabPanel('State View',  #creating 1st tab for dashboard
                    fluidRow(column(12,align="center",
                                    selectizeInput("Specialty_final", "Select Specialty",
                                                   c('All', 'Cardiology','Endocrinology, Diabetes & Metabolism',
                                                     'Gastroenterology','Orthopaedics','Oncology',
                                                     'Psychiatry & Neurology', 'Surgery')
                                                   ) #dropdown creation for Specialty selection
                                    )
                             ),
                    
                    fluidRow(box(
                        plotlyOutput(outputId = 'State',width = '100%',height = '100%'), height=400),
                        box(
                          plotlyOutput(outputId = 'State_Avg',width = '100%',height = '100%'),height = 400))), #creating two side by side maps for total and avg payments
                      
              
            tabPanel('Companies and Products',  #creating 2nd tab for dashboard
                    fluidRow(
                      column(2,
                             br(),
                             align="left",
                             radioButtons("cmpny","Select Specialty", 
                                          choices =  c('All', 'Cardiology','Endocrinology, Diabetes & Metabolism',
                                                                              'Gastroenterology','Orthopaedics','Oncology', 'Psychiatry & Neurology',
                                                                              'Surgery')) #creating radiobuttons for treemap
                             ),
                      column(8,
                             br(),
                             plotOutput(outputId="Company"), #treemap for company totals
                             offset = 0.5
                      )),
                      

                    fluidRow(
                      column(2,
                             br(),
                             selectizeInput("productcheckbox","Select Product Type",
                                            choices = c('Drugs','Medical Supplies'))),  #dropdown for bar graph

                        column(8,
                               br(),
                        plotlyOutput(outputId = 'Product'),offset = 0.5)) #bargraph for top 10 products
                    
           )

)
