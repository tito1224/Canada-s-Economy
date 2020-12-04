# Title     : Shiny Website
# Objective : Create a webapp using shiny package
# Created by: titom
# Created on: 11/22/2020






ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Economic Tracker"),
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("GDP Inputs",tabName = "gdpcontrols", icon = icon("money-bill"),
                                     selectInput("industry", "Select Industries", unique_industry, multiple = TRUE, selected = c("Durable manufacturing industries [T012]" ,"Non-durable manufacturing industries [T011]")),
                                     dateInput("date","Start Date",min = "2000-01-01", max = "2020-08-01", value = "2000-01-01", startview = "year")),
                            menuItem(" GDP Analysis (Canada)", tabName = "gdpanalysis", icon = icon("canadian-maple-leaf")),
                            menuItem("Economic Indicators", icon = icon("chart-area"), tabName = "indicate"),
                            menuItem("National GDP Inputs", icon = icon("globe"),
                                     checkboxGroupInput("countrygdp","Choose a country",
                                                        choices = unique(gdp_all$symbol), selected = "United States"),
                                     dateRangeInput("gdpdate_n", "Choose A Date Range",start = "2010-01-01",end = today(),min = "2010-01-01"), startview = "year"),
                            menuItem("Citations", icon = icon("book"), tabName = "cite")
                        )
                        
                    ),
                    
                    
                    dashboardBody(tags$style(js),
                                  tabItems(
                                      tabItem("gdpanalysis",
                                              h2("Brief Overview of Canada's GDP"),
                                              br(),
                                              p("There has been a steady increase in Canada's GDP over time. However we can see that the major effect covid
          had on it's growth in the year 2020. Major industries were impacted - some more than others. But on a positive note
          there has been a bounce back in the more recent months. To compare the change in GDP contribution for different industries,
          change the values and dates within the", tags$strong("GDP Inputs tab.")),
                                              br(),
                                              fluidRow(
                                                  
                                                  # plotting the gdp line graph
                                                  box(plotOutput("gdpplot"), width = 12, color = "black"),
                                                  
                                                  
                                                  # adding a bottom row where we can have all the controls we want to manipulate the graph
                                                  # think of the box() function as the column() function in shiny
                                                  
                                                  
                                                  # display the percentage drop (zoomed in)
                                                  tabBox(
                                                      
                                                      tabPanel("2019 - 2020 GDP",plotOutput("zoom"), width = 4),
                                                      tabPanel("Summary Statistics (Year 2020)", dataTableOutput("summary"), width = 4)),
                                                  tabBox(
                                                      
                                                      tabPanel("Maximum/Minimum Percent Change",plotOutput("compare"), width = 4),
                                                      tabPanel("Monthly Percent Change", plotOutput("severe"), width = 4))
                                                  
                                              )
                                      ),
                                      tabItem("indicate",
                                              navbarPage("Economic Indicators",
                                                         tabPanel("Stock Market Analysis",
                                                                  h2("Analyzing The Stock Market"),
                                                                  br(),
                                                                  p("The stock market is a leading economic indicator and is an important aspect to consider when evaluating the state of the economy.
          The ",tags$strong("^GSPTSE ticker") ,"stands for the stock prices of the S&P/TSX Index which is the", tags$strong("benchmark Canadian Index"),". It represents around 70%
          of the total market cap on the Toronto Stock Exchange. We want to see how the S&P/TSX Index performed compared to other indices/stocks.
          Please input a valid stock ticker from", tags$strong("Yahoo Finance"), "to compare the perfomance of the S&P/TSX Index with your desired indices/companies."),
                                                                  p("Popular ticker names include:",tags$strong("^GSPC"), "(for the S&P500 Index)", tags$strong("^IXIC"), "(for the NASDAQ Index)",tags$strong("^N225"), "(for the Nikkei 225 Index)."),
                                                                  p(tags$strong("Note: "), "Please give a few seconds for the graphs to load."),
                                                                  br(),
                                                                  fluidRow(tabBox(width = 12,
                                                                                  tabPanel("Stock Prices Over Time",plotOutput("stockprice")),
                                                                                  tabPanel("Stock Prices And Volume Over Time", plotOutput("stockvolume")),
                                                                                  tabPanel("Returns", plotOutput("stockreturns")),
                                                                                  tabPanel("Volatility",plotOutput("stockvol")),
                                                                                  tabPanel("Volatility  x  Average Volume", plotOutput("volvol")))),
                                                                  fluidRow(box(title = "Inputs",status = "success", solidHeader = TRUE,collapsible = TRUE,
                                                                               textInput("stocksymbol", "Input A Stock Ticker For Comparison", "AAPL"),
                                                                               textInput("stocksymbol2", "(Optional): Input Another Stock Ticker"),
                                                                               textInput("stocksymbol3", "(Optional): Input Another Stock Ticker"),
                                                                               dateInput("stockdate","Start Date",min ="2000-01-01",value = "2010-01-01",startview = "year"),
                                                                               dateInput("stockdatenow","End Date",min ="2000-01-02",value = today(),startview = "year"),
                                                                               actionButton("stockupdate","Update")
                                                                  ),
                                                                  box(title = "Data Table", status = "success", solidHeader = TRUE, collapsible = TRUE,
                                                                      dataTableOutput("stocksummary"))
                                                                  
                                                                  )),
                                                         tabPanel("National GDP (other Countries)",
                                                                  h2("National GDP"),
                                                                  br(),
                                                                  p("Gross Domestic Product (ie GDP) is the market value of all the goods and services that are
                   produced in a country within a specific time period. It is a common metric to judge the overall health
                   of a nation's economy."),
                                                                  p("This section focuses on a select few countries and graphs the GDP Percent Change over time. Please use the ", tags$strong("National GDP Inputs tab"),
                                                                    "on the sidebar to change and select inputs."),
                                                                  br(),
                                                                  fluidRow(box(plotOutput("ngdpl")),
                                                                           box(plotOutput("ngdpb")),
                                                                  )
                                                         ),
                                                         tabPanel("Labor Market", h2("Labor Market"),
                                                                  br(),
                                                                  p("Analysing the labor market is vital to gaining a better picture of a nation's economic health. In 2020, there has been record levels of
                   unemployment both provincially and nationwide within Canada. Use this tab to explore and analyze trends in the labor market over time."),
                                                                  
                                                                  p(tags$em("IMPORTANT:")),
                                                                  p("Please", tags$strong("click the UPDATE button"), "within the Inputs box below to create a graph, and give it a few seconds to load after clicking UPDATE."),
                                                                  fluidRow(tabBox(width = 12,
                                                                                  tabPanel(title = "By Region",plotOutput("unemployment")),
                                                                                  tabPanel(title = "By Gender", plotOutput("regiongender")),
                                                                                  tabPanel(title = "By Age Group",
                                                                                           selectInput("streamprovince", "Choose A Provice For Specifically This Graph",choices = unique_geo[-1], selected  = "Ontario",multiple = FALSE)
                                                                                           ,streamgraphOutput("agegroup"))
                                                                  )),
                                                                  fluidRow(box(title = "Inputs",width = 12 ,collapsible = TRUE, column(width=3,
                                                                                                                                       pickerInput("geodf", "Choose A Region",choices = unique_geo ,multiple = TRUE,options = list(`actions-box` = TRUE), selected = unique_geo),
                                                                                                                                       selectInput("typel", "Choose A Statistic",choices = unique_labor, selected  = "Unemployment rate",multiple = FALSE)),
                                                                               column(width = 3,radioButtons("sexdf","Select Sex", choices = unique_sex,selected = "Both sexes")),
                                                                               column(width = 3,selectInput("age","Select An Age Group", choices = unique_age, multiple = FALSE, selected = "15 years and over")),
                                                                               column(width = 3,dateRangeInput("labordate", "Select A Date Range",start = "2015-01-01" ,end = "2020-10-01", max = "2020-10-01", min = "2015-01-01",startview = "year"),
                                                                                      actionButton("submitlabor","Update"))
                                                                  ))
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                         ) #,# remember to add a comma if we put the tab back!!  # unemployment, weekly intitial claims for unemployment, etc
                                                         # tabPanel("Consumer Price Index"), # measures inflation, interest rate, for later
                                                         # tabPanel("Production & Business Activity") #Total Retail Trade & Manufacturing Sales, etc
                                                         
                                              )
                                      ),
                                      tabItem("cite",h2("Data Sources"),
                                              br(),
                                              p(" Statistics Canada.",tags$a(href = "https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1410028703","Table 14-10-0287-03 Labour force characteristics by province, monthly, seasonally adjusted")),
                                              p("Stock Data Pulled From", tags$a(href = "https://ca.finance.yahoo.com/","Yahoo Finance."), "Originally Retrieved: December 3, 2020,", "Data Last Updated:",as.character(today())),
                                              p("National GDP Data retrieved from FRED, Federal Reserve Bank of St. Louis;",tags$a("https://fred.stlouisfed.org/series/GDPC1"), ",Originally Retrieved: December 3, 2020,", "Data Last Updated:",as.character(today()))
                                      )
                                  ))
)