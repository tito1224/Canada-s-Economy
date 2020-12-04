#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define server logic required to draw a histogram
server <- function(input, output) {
    gdp_n <- reactive(
        subset(gdp, Industry == input$industry & Date > input$date)
    )
    
    gdpmin <- reactive(subset(gdp_n(), Date > "2019-01-01",GDP))
    
    # plotting GDP over time and by industry
    output$gdpplot <- renderPlot(ggplot(gdp_n(), aes(x = Date, y = GDP, colour = Industry)) + geom_line()
                                 + labs(title = "Canada's GDP By Industry")
                                 + annotate(geom="point", x=as.Date("2020-03-18"), y=min(gdpmin(), na.rm = TRUE), size=10, shape=21, fill="transparent")+ theme_classic() +
                                     annotate(geom="text", x=as.Date("2019-01-01"), y=min(gdpmin()-100, na.rm = TRUE),
                                              label="Canada Introduces \n Lockdowns"))
    
    # Data wrangling for the gdp graphs
    
    gdp_c <- reactive(gdp %>%
                          filter(Industry == input$industry, Date > "2019-08-01")
    )
    
    gdp_bar <- reactive({
        fm %>%
            filter(Industry %in% input$industry)
    })
    
    gdp_line <- reactive({
        f %>%
            filter(Industry %in% input$industry)
    })
    
    # gdp for gdp tab in economic indicators page
    gdp_all_r <- reactive({
        gdp_all %>%
            filter(symbol %in% input$countrygdp, date >= input$gdpdate_n[1], date <= input$gdpdate_n[2])
    })
    
    gdp_all_mm_r <- reactive({
        gdp_all_mm %>%
            filter(symbol %in% input$countrygdp)
    })
    
    
    
    
    
    
    # getting stock data
    stock_data <- reactive(tq_get(c("^GSPTSE",input$stocksymbol,input$stocksymbol2, input$stocksymbol3),from = input$stockdate,to=input$stockdatenow,get = "stock.prices"))
    stock_data_g <- reactive(tq_get(c("^GSPTSE","AAPL"),from = input$stockdate,get = "stock.prices"))
    
    #summary stock data graph
    ss_g <- reactive( stock_data_g() %>%
                          group_by(symbol,month = floor_date(date,"month")) %>%
                          mutate(pct_change = (close - lag(close))/close) %>%
                          summarise(`Average Monthly Returns` = mean(pct_change, na.rm = TRUE),`Price Volatility` = sqrt(var(close, na.rm = TRUE)), `Average Volume` = mean(volume, na.rm = TRUE)))
    
    ss <-  reactive(stock_data() %>%
                        group_by(symbol,month = floor_date(date,"month")) %>%
                        mutate(pct_change = (close - lag(close))/close) %>%
                        summarise(`Average Monthly Returns` = mean(pct_change, na.rm = TRUE),`Price Volatility` = sqrt(var(close, na.rm = TRUE)), `Average Volume` = mean(volume, na.rm = TRUE)))
    
    
    # zoomed in gdp lolz
    output$zoom <- renderPlot(
        ggplot(gdp_c(), aes(x = Date,y = GDP, colour = Industry)) + geom_line()
        + labs(title = "GDP (2019-2020)")
        + theme_light())
    
    #plot for percent change in gdp
    output$compare <- renderPlot({
        ggplot(data = gdp_bar(), aes(x = pct_change, y = values, fill = Industry)) + geom_bar(stat = "identity", position = "dodge") +
            labs(title = "Largest Monthly Percent Change in GDP (Year 2020) ", x = "Maximum and Minimum Percent Change", y = "Percent Change") + theme_light()
    })
    
    #plot for percent change line/bubble graph
    output$severe <- renderPlot({
        ggplot(data = gdp_line(), aes(x = Date, y = pct_change, color = Industry)) + geom_line() +
            labs(title = "Monthly Percent Change in GDP", x = "Date", y = "Percent Change") + theme_light()
    })
    
    #create gdp summary data
    summary_data <- reactive({
        f %>%
            filter(Industry %in% input$industry) %>%
            group_by(Industry) %>%
            summarise(`Maximum GDP` = max(GDP, na.rm = TRUE), `Minimum GDP` = min(GDP, na.rm = TRUE), `Average GDP` = mean(GDP), `Largest Percent Increase` = max(pct_change, na.rm = TRUE), `Largest Percent Decrease` = min(pct_change, na.rm = TRUE), `Variance of GDP` = var(GDP))
    })
    
    #output gdp summary data
    output$summary <- renderDataTable({summary_data()})
    
    #gdp plot for other countries
    output$ngdpl <- renderPlot({
        ggplot(data = gdp_all_r(), aes(x = date, y = pct_change, color = symbol)) + geom_line() + scale_color_discrete(name = "Country",
                                                                                                                       labels = c(input$countrygdp)) +
            theme_classic() + labs(title = paste("GDP Percentage Change From",as.character(input$gdpdate_n[1]), "to",as.character(input$gdpdate_n[2])), x = "Date", y = "Percentage Change in GDP")
    })
    
    #gdp barplot for other countries
    output$ngdpb <- renderPlot({
        ggplot(data = gdp_all_mm_r(), aes(x = `Percent Change`, y = Values, fill = symbol)) + geom_bar(stat = "identity", position = "dodge") + scale_color_discrete(name = "Country",
                                                                                                                                                                     labels = c(input$countrygdp)) +
            theme_classic() + labs(title = paste("GDP Percentage Change From",as.character(input$gdpdate_n[1]), "to",as.character(input$gdpdate_n[2])))
    })
    
    
    # UNEMPLOYMENT WORK
    
    
    # unemployment line graph
    
    unemployment <- eventReactive(input$submitlabor,{labor_c %>%
            filter(labor.force.type == input$typel, sex == input$sexdf, age.group == input$age, geo %in% input$geodf, date > input$labordate[1] & date < input$labordate[2])})
    
    # action button for the labor stuff
    # action button for the title lol
    
    titles <- eventReactive(input$submitlabor, {
        paste(as.character(input$typel), "for Age Group", as.character(input$age))
    })
    
    output$unemployment <- renderPlot({
        ggplot(unemployment()) + geom_line(aes(x = date, y = value, color = geo)) + theme_minimal() + labs(x = "Date", y = "Value", title = titles())
        
    })
    
    
    # second panel where we want to do a facet wrap (based on geography) - so we don't filter for that
    # i want to do color by gender - so also don't filter for that
    # so what we filter for is the labor type - unemployment, participation, full time employment (only select one)
    # we also filter for age group - only select one and date obv lol
    
    unemployment2 <- eventReactive(input$submitlabor,{labor_c %>%
            filter(labor.force.type == input$typel, age.group == input$age,date > input$labordate[1] & date < input$labordate[2],
                   sex %in% c("Males", "Females"), geo %in% input$geodf) # want to only see males and females
    })
    
    output$regiongender <- renderPlot({
        ggplot(data = unemployment2(), aes(x = date, y = value)) + geom_area(aes(fill = sex)) +
            scale_fill_viridis_d(alpha = 0.7, begin = 0.5, end = 0.75) + theme_minimal() + facet_wrap(~geo) + theme(panel.spacing = unit(0.1,"lines")) + labs(x = "Date", y = "Value", title = titles())
    })
    
    #intermediate step
    unemployment3 <- eventReactive(input$submitlabor,{labor_c %>%
            filter(labor.force.type == input$typel,date > input$labordate[1] & date < input$labordate[2], sex == input$sexdf)
        
    })
    
    unemployment4 <- reactive(subset(unemployment3(), geo == input$streamprovince)) # what I actually want
    
    output$agegroup <- renderStreamgraph({
        
        streamgraph(unemployment4(),key = age.group, value = value, date = date) %>%
            sg_axis_x(1, "year", "%Y") %>%
            sg_legend(show = TRUE,label = "Age group: ")
    })
    
    
    
    
    # STOCK STUFF UGH
    
    #output$stocksummary <- renderDataTable({ss()})
    
    # action button details
    v <- reactiveValues(plot = NULL,data =NULL, plot2 = NULL, plot3 = NULL,plot4 = NULL,plot5 = NULL)
    
    observeEvent(input$stockupdate,{
        v$plot <- ggplot(data = stock_data(), aes(x = date, y = close, color = symbol)) + geom_line() +
            labs(title = paste("Stock Prices from",as.character(input$stockdate),"to", as.character(input$stockdatenow)), x = "Date", y = "Stock Price") +
            facet_wrap(~symbol, scales = "free_y") + theme_minimal()
        
        #stock price with volume
        v$plot2 <- ggplot(data = stock_data(), aes(x = date, y = close, size = volume, color = symbol)) + geom_point(alpha = 0.5) +
            labs(title = paste("Stock Prices from",as.character(input$stockdate),"to", as.character(input$stockdatenow)), x = "Date", y = "Stock Price") +
            facet_wrap(~symbol, scales = "free_y") + theme_minimal() + scale_size_area(max_size = 5)
        
        # stock returns
        v$plot3 <- ggplot(data = ss(), aes(x = month, y = `Average Monthly Returns`, fill = symbol)) + geom_bar(stat = "identity", position = "dodge") + theme_minimal() +
            labs(x = "Date",title = paste("Stock Prices from",as.character(input$stockdate),"to", as.character(input$stockdatenow)))
        
        # stock volatility
        
        v$plot4 <- ggplot(data = ss(), aes(x = month, y = `Average Monthly Returns`, color = symbol, size = `Price Volatility`)) + geom_point() +
            facet_wrap(~symbol, scales = "free_y") + scale_size_area(max_size = 5) + theme_minimal() + labs(x = "Date",title = paste("Stock Prices from",as.character(input$stockdate),"to", as.character(input$stockdatenow)))
        
        v$plot5 <- ggplot(data = ss(), aes(x = month, y = `Price Volatility`, color = symbol, size = `Average Volume`)) + geom_point() +
            facet_wrap(~symbol, scales = "free_y") + scale_size_area(max_size = 5) + theme_minimal() + labs(x = "Date",title = paste("Stock Prices from",as.character(input$stockdate),"to", as.character(input$stockdatenow)))
        
        v$data <-
            if(weekdays(input$stockdatenow) == "Saturday") {
                subset(stock_data(),date > input$stockdatenow-2 & date < input$stockdatenow)
            } else if (weekdays(input$stockdatenow) == "Sunday") {
                subset(stock_data(),date > input$stockdatenow-3 & date <= input$stockdatenow-1)
            } else if(weekdays(input$stockdatenow) == "Monday") {
                subset(stock_data(),date > input$stockdatenow-4 & date <= input$stockdatenow-2)
            }
        else {
            subset(stock_data(),date == input$stockdatenow-1) }
        
        
        
        
    })
    
    # base case using basic values
    base <- reactive({
        if(weekdays(input$stockdatenow) == "Saturday") {
            subset(stock_data_g(),date > input$stockdatenow-2 & date < input$stockdatenow)
        } else if (weekdays(input$stockdatenow) == "Sunday") {
            subset(stock_data_g(),date > input$stockdatenow-3 & date <= input$stockdatenow-1)
        } else if(weekdays(input$stockdatenow) == "Monday") {
            subset(stock_data_g(),date > input$stockdatenow-4 & date <= input$stockdatenow-2)
        }
        else {
            subset(stock_data_g(),date == input$stockdatenow - 1)
        }
        
    })
    
    
    
    
    
    
    
    
    
    #STOCK STUFF - OUTPUT
    
    # plotting stock price
    output$stockprice <- renderPlot({
        if(is.null(v$plot)) return(
            ggplot(data = stock_data_g(), aes(x = date, y = close,color = symbol))  + geom_line() +
                labs(title = paste("Stock Prices from",as.character(input$stockdate),"to", as.character(input$stockdatenow)), x = "Date", y = "Stock Price") +
                facet_wrap(~symbol, scales = "free_y") + theme_minimal()
        )
        v$plot
    })
    
    #plotting stock price with volume
    output$stockvolume <- renderPlot({
        if(is.null(v$plot2)) return (
            ggplot(data = stock_data_g(), aes(x = date, y = close, size = volume, color = symbol)) + geom_point(alpha = 0.3)  +
                labs(title = paste("Stock Prices from",as.character(input$stockdate),"to", as.character(input$stockdatenow)), x = "Date", y = "Stock Price") +
                facet_wrap(~symbol, scales = "free_y") + theme_minimal() + scale_size_area("Volume (Shares Traded/Day)", max_size = 7) + theme_minimal()
        )
        v$plot2
    })
    
    #plotting stock returns
    output$stockreturns <- renderPlot({
        if(is.null(v$plot3)) return (
            ggplot(data = ss_g(), aes(x = month, y = `Average Monthly Returns`, fill = symbol)) + geom_bar(stat = "identity", position = "dodge") + theme_minimal() +
                labs(x = "Date",title = paste("Stock Prices from",as.character(input$stockdate),"to", as.character(input$stockdatenow)))
        )
        v$plot3
    })
    
    #plotting price volatility
    output$stockvol <- renderPlot({
        if(is.null(v$plot4)) return(
            ggplot(data = ss_g(), aes(x = month, y = `Average Monthly Returns`, color = symbol, size = `Price Volatility`)) + geom_point() +
                facet_wrap(~symbol, scales = "free_y") + scale_size_area(max_size = 5) + theme_minimal() + labs(x = "Date",title = paste("Stock Prices from",as.character(input$stockdate),"to", as.character(input$stockdatenow)))
        )
        v$plot4
    })
    
    
    #plotting stock volatility vs volume
    output$volvol <- renderPlot({
        if(is.null(v$plot5)) return (
            ggplot(data = ss_g(), aes(x = month, y = `Price Volatility`, color = symbol, size = `Average Volume`)) + geom_point() +
                facet_wrap(~symbol, scales = "free_y") + scale_size_area(max_size = 5) + theme_minimal() + labs(x = "Date",title = paste("Stock Prices from",as.character(input$stockdate),"to", as.character(input$stockdatenow)))
        )
        
        v$plot5
    })
    
    
    
    
    #output$stockvol
    
    output$stocksummary <- renderDataTable({
        if(is.null(v$data)) return (base())
        v$data
    })
    
}
