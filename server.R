

function(input, output,session){
  style <-theme(
    plot.background = element_rect(
      fill = '#282B30', colour = '#282B30'),
    panel.background = element_rect(
      fill = "#282B30", colour = '#686868'),
    panel.grid.major = element_line(colour = '#686868'),
    panel.grid.minor = element_line(colour = '#686868'),
    axis.title = element_text(color = '#989898', size = 15), 
    axis.text = element_text(color = '#989898', size = 10),
    plot.title = element_text(
      color = '#989898', size = 18,hjust = 0.5), 
    legend.background = element_rect(
      fill = '#282B30', colour = '#282B30'),
    legend.text = element_text(color = '#989898', size = 10),
    legend.title = element_text(color = "#989898",size = 12) )

  
  output$State <- renderPlotly ({


    plot_geo(
      filter(state_pymnt_final, state_pymnt_final$Specialty_Final==input$Specialty_final),
      locationmode = 'USA-states') %>%
      add_trace(
        z = ~Pymnt_Mn, text = ~hover, locations = ~Recipient_State,
        color = ~Pymnt_Mn, colors=c("lightyellow","red")
      ) %>%
      colorbar(title ="Million USD") %>%
      layout(
        title = 'Total Open Payments to Healthcare Providers by State',margin=list(t=50), 
        # legend=list(font=list(size = 20), 
         geo = g)
  })
  
  
  output$State_Avg <- renderPlotly ({
    
      plot_geo(
      filter(state_pymnt_final, state_pymnt_final$Specialty_Final==input$Specialty_final),
      locationmode = 'USA-states') %>%
      add_trace(
        z = ~Avg_Pymnt_HCP, text = ~hover_avg, locations = ~Recipient_State,
        color = ~Avg_Pymnt_HCP, colors=c("lightgreen","blue")
      ) %>%
      colorbar(title ="USD") %>%
      layout(
        title = 'Avg. Open Payment Per Healthcare Provider by State',margin=list(t=50), legend=list(font=list(size = 20)),geo = g)
  })
  
  Company <- reactive({
    validate(
      need(input$cmpny != "", "Please select at least one specialty")
    )
    Company_final %>% filter(Specialty_Final==input$cmpny)
    }) 
  
    output$Company <- renderPlot({
    
    treemap(Company(),
            index=c("Company"),  #A list of your categorical variables
            vSize = "Payments",  #This is your quantitative variable
            vColor = "PaymentPerProvider",
            title.legend = "Average Payment Per Provider",
            type="dens", #Type sets the organization and color scheme of your treemap
            palette = "YlGnBu",  #Select your color palette from the RColorBrewer presets or make your own.
            title="Top 20 Companies by Payments Made", #Customize your title
            fontsize.title = 16
            
            )

  })

  # output$Nature <- renderPlot({
  #   set.seed(32)
  #   # wordcloud2(data = Nature_Pymnts_Table)
  #              # size=1, fontWeight = 'bold', color = brewer.pal(8, "Dark2"),shuffle = FALSE)
  #   # wordcloud(words = nature_pymnt_word.Clean, random.order=FALSE, rot.per=0.3,
  #   #         colors=brewer.pal(8, "Dark2"),scale = c(3,0.5), width="900px", height="500px")
  #
  #
  #   ggplot(aes(fill=Nature_of_pymnt,label="Form of Payment",size=Payments,y=c(.8,1.2,.9,1.1,.8,1.2),x=1:6),
  #          data=Nature_Pymnts_Table)+
  #     geom_jitter(shape=21,width = .00,height = .1,alpha=1)+
  #     scale_size_area(
  #       limits=c(0,1),max_size = 32
  #     )+
  #     scale_fill_manual(values=brewer.pal(6,"Set1"))+
  #     ylim(.6,1.4)+xlim(0,7)+
  #     theme(
  #       axis.ticks=element_blank(),
  #       axis.title.y=element_blank(),
  #       axis.text=element_blank()
  #     )+
  #     guides(fill=FALSE,size=F)
  #
  #
  # })
  
  
  Products_final_rctv = Products_final
   output$Product <- renderPlotly({
     Products_final_rctv = Products_final_rctv %>% filter(Product_type==input$productcheckbox)
     plot_ly(data = arrange(Products_final_rctv, desc(Payments)), x = ~Payments, y = ~Product ,type = 'bar',
            orientation = 'h',marker = list(color = 'rgba(222,45,38,0.8)'))  %>% layout(margin=list(l = 200), yaxis=list(tickprefix="", tickangle = 'auto'))

  })
   
   
   
   
   }


