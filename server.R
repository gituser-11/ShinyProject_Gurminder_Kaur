

function(input, output,session){
  output$State <- renderPlotly ({ #defining map for state totals
    plot_geo(
      filter(state_pymnt_final, state_pymnt_final$Specialty_Final==input$Specialty_final), #using user input to filter data
      locationmode = 'USA-states') %>%
      add_trace(
        z = ~Pymnt_Mn, text = ~hover, locations = ~Recipient_State,
        color = ~Pymnt_Mn, colors=c("lightyellow","red")
      ) %>%
      colorbar(title ="Million USD") %>%
      layout(
        title = 'Total Open Payments to Healthcare Providers by State',margin=list(t=50), 
           geo = list(
           scope = 'usa',
           projection = list(type = 'choropleth'),
           showlakes = TRUE,
           lakecolor = toRGB('white')
         ))
  })
  
  
  output$State_Avg <- renderPlotly ({ #defining map for state averages
    
      plot_geo(
      filter(state_pymnt_final, state_pymnt_final$Specialty_Final==input$Specialty_final),
      locationmode = 'USA-states') %>%
      add_trace(
        z = ~Avg_Pymnt_HCP, text = ~hover_avg, locations = ~Recipient_State,
        color = ~Avg_Pymnt_HCP, colors=c("lightgreen","blue")
      ) %>%
      colorbar(title ="USD") %>%
      layout(
        title = 'Avg. Open Payment Per Healthcare Provider by State',margin=list(t=50), legend=list(font=list(size = 20)),
        geo=list(
          scope = 'usa',
          projection = list(type = 'choropleth'),
          showlakes = TRUE,
          lakecolor = toRGB('white')
          ))
  })
  
  Company <- reactive({ #radiobox reactive 
    validate(
      need(input$cmpny != "", "Please select at least one specialty")
    )
    Company_final %>% filter(Specialty_Final==input$cmpny)
    }) 
  
    output$Company <- renderPlot({  #treemap for company totals
    
    treemap(Company(),
            index=c("Company"),  
            vSize = "Payments",  
            vColor = "PaymentPerProvider",
            title.legend = "Average Payment Per Provider",
            type="dens", 
            palette = "YlGnBu",  
            title="Top 20 Companies by Payments Made", 
            fontsize.title = 16
            
            )

  })

  
  Products_final_rctv = Products_final
   output$Product <- renderPlotly({ #creating barchart for top 10 products
     Products_final_rctv = Products_final_rctv %>% filter(Product_type==input$productcheckbox)
     plot_ly(data = arrange(Products_final_rctv, desc(Payments)), x = ~Payments, y = ~Product ,type = 'bar',
            orientation = 'h',marker = list(color = 'rgba(222,45,38,0.8)'))  %>% layout(margin=list(l = 200), yaxis=list(tickprefix="", tickangle = 'auto'))

  })
   
}


