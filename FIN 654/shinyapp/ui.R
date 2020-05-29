source("Final Project.R")
library(markdown)
library(shiny)
library(DT)
library(plotly)
library(htmltools)

shinyUI(fluidPage(
  theme = "bootstrap.css",
  
  tags$head(
    tags$style(
      " .title { background:url('banner.jpg'); background-repeat: no-repeat; background-size: 100% 200%; color: white;
      font-family: Optima, Segoe, 'Segoe UI', Candara, Calibri, Arial, sans-serif;
      height: 100px;
      font-size: 24px;
      font-style: normal;
      font-variant: normal;
      font-weight: 2000;
      line-height: 26.4px} "
    ),
    tags$style(
      ".intro{font-size:16px; border:1px; padding:15px; background:#d7e3f5;}"
    ),
    tags$style(".def{border:1px; padding:15px; background:#d7e3f5;}"),
    tags$style(".cor{border:1px; padding:15px; background:#d7e3f5;}")
    ),
  tags$head(
    tags$style(type = "text/css",
               "#image img {max-width: 100%; width: auto; height: auto}")
  ),
  
  titlePanel(
    windowTitle = "Final Project",
    title =
      div("FIN 654 - Final Project - Group 4", class = 'title')
  ),
  
  navbarPage(
    id = "Navbar",
    fluid = TRUE,
    theme = shinythemes::shinytheme("spacelab"),
    tags$style(type = "text/css", "body {padding-top: 0px;}"),
    
    tabPanel("Introduction",
             fluidRow(
               column(
                 12,
                 tags$p(
                   class = "intro",
                   "Tourism across the world is expanding and United States companies are looking at increasing their ability to respond to the growing demand.
                   New research from Tourism Economics shows that international travel to the United States grew by only 2%, however travel to Europe reached 715 million growing by 6% in 2018 over an exceptionally strong 2017."
                 ),
                 tags$p(""),
                 tags$p(
                   "Our company is looking to invest ",
                   tags$b("$250M"),
                   " dollars in the tourism market. We will examine two airline stock prices, two hospitality chains that are publicly traded and the foreign exchange rate of the Euro and the British Pound."
                 ),
                 tags$p(
                   "Reasons we are looking to invest in tourism include:",
                   tags$ol(
                     tags$li(
                       "Tourism and hospitality is now the second fastest-growing industry in terms of foreign direct investment"
                     ),
                     tags$li(
                       "The travel and tourism sector accounts for more than 10% of global GDP accounting for $7.6 trillion dollars."
                     ),
                     tags$li(
                       "Tourism is the second largest-job generator supporting 292 million jobs or one in ten jobs in 2016."
                     ),
                     tags$li(
                       "In January 2018 European airline carriers showed fastest growing international passenger demand of all regions (7.7% year-on-year)"
                     )
                   )
                 ),
                 tags$p(
                   "Accor S.A. (ACCYY), uses the brand name AccorHotels which is a French multinational hospitality company that owns, hotels, resorts, and vacation properties.
                   It is the largest hotel group in the world outside the United States. The company was founded in 1967 and is headquartered in Issy-les-Moulineaux, France and employees 22,935."
                 ),
                 tags$p(
                   "International Hotel Group (IHG) is a company that operates hotels, resorts, and restaurants under the names Crown Plaza, Holiday Inn, Staybridge Suites, and others.
                   As of March 2019 it had approximately 5,600 hotels with 837,000 guest rooms. It operates in the consumer cyclical sector and is in the lodging industry with 12,812 full time employees."
                 ),
                 tags$p(
                   "Deutsche Lufthansa (DLAKY) operates as an aviation company in Germany and internationally. It operates through Network Airlines, Point-to-Point Airlines, Logistics, MRO, and Catering segments.
                   As of December 31, 2017 it had a fleet of 728 aircraft"
                 ),
                 tags$p(
                   "International Consolidated Airlines Group (ICAGY) engages in the provision of passenger and cargo transportation services. As of Feb 12, 2018 it had a fleet of 547 aircraft flying to 268 destinations. The company was incorporated in 2010 and is based in Madrid, Spain.
                   The company also operates in the Industrial sector in the airlines industry with a total full-time employees of 63,531."
                 ),
                 tags$p(
                   "We will also examine and analyze the foreign exchange rates of the Euro and the British Pound"
                 ),
                 tags$hr(style = "border-color: red;border-top: 3px solid #446e9b;"),
                 tags$h5(tags$b("Data Analysis Support")),
                 tags$p(
                   "AccorHotels last year’s revenue was $3.61B. The company is in the consumer cyclicals sector and operates in the lodging industry."
                 ),
                 tags$p("Potential reasons to invest in ACCYY include:"),
                 tags$p(tags$ol(
                   tags$li("Increase in sales from 1.94B in 2017 to 3.61B in 2018")
                 )),
                 tags$p(
                   "International Hotel Group (IHG) operates in the Industrials sector and is in the Airlines industry.  There are a total of 96,229 employed with the company.  IHG generated 3.17B in revenue in 2018."
                 ),
                 tags$p("Potential reasons to invest in IHB include:"),
                 tags$ol(
                   tags$li("Increase in revenue from 1.7B in 2017 to 3.17B in 2018."),
                   tags$li("Projected earnings growth of 5% per year")
                 ),
                 tags$p(
                   "Deutsche Lufthansa (DLAKY) generated 35.92B in 2018 operating in the Industrials sector in the airlines Industry.  Deutsche Lufthansa is traded on German Xetra Market."
                 ),
                 tags$p("Potential reasons to invest:"),
                 tags$ol(
                   tags$li("Increased bottom line by 38.40%"),
                   tags$li("27.99% Return on Equity")
                 ),
                 tags$p(
                   "International Consolidated Airlines Group generated 24.41B Euro of revenue in 2018 and is traded on the London Stock exchange."
                 ),
                 tags$p("Potential reasons to invest:"),
                 tags$ol(
                   tags$li("P/E ratio of 7.3x, which is lower than industry average of 10.3x")
                 ),
                 tags$hr(style = "border-color: red;border-top: 3px solid #446e9b;"),
                 tags$h5(tags$b("Key Business Questions")),
                 tags$ol(
                   tags$li("Is this the market we should invest in?"),
                   tags$li("Are these the companies we want to invest in?"),
                   tags$li("What is our expected return?"),
                   tags$li("How risky of a market is this?")
                 ),
                 tags$hr(style = "border-color: red;border-top: 3px solid #446e9b;"),
                 tags$h5(tags$b("Team Members")),
                 tags$ol(
                   tags$li(
                     "Ketki Potdar – Applied Data Science Program – Constructed Shiny App"
                   ),
                   tags$li(
                     "Graham Latsa – Applied Data Science Program – Construction of code, Analysis"
                   ),
                   tags$li("Kevin Hennings – MBA Program – Analysis"),
                   tags$li("Brett Jones – MBA Program - Introduction")
                 ),
                 tags$br("")
                 )
                 )),
    
    
    tabPanel("Data",
             fluidRow(column(
               4,
               div(
                 class = "def",
                 tags$p(tags$b("Data Definition")),
                 tags$p(
                   "IHG: Daily Price of InterContinental Hotels Group (British hospitality company)"
                 ),
                 tags$p("ACCYY: Daily Price of AccorHotels (French hopsitality company)"),
                 tags$p("DLAKY: Daily Price of Deutsche Lufthansa AG (German airline)"),
                 tags$p(
                   "ICAGY: Daily Price of International Consolidated Airlines Group (British airline)"
                 ),
                 tags$p("EUR: Euro - Official currency of EU"),
                 tags$p("GBP: Pound Sterling - Official currency of UK")
               )
             ),
             column(
               8,
               div(class = "price",
                   #tags$h5(tags$b("Stock Price Percent Changes")),
                   plotOutput("plotNorm"))
             )),
             fluidRow(column(
               6, plotlyOutput("plotPriceChanges")
             ))),
             
             tabPanel(
               "Exploratory Analysis",
               fluidRow(column(6, div(
                 class = "cor",
                 tags$ol(
                   tags$li(
                     "There is strong positive correlation between IHG(British hospitality company), DLAKY(German airlines) and ICAGY(British airlines)"
                   ),
                   tags$li(
                     "Intercontinental Hotels Group (IHG) has wide portfolio of covering different segments of the hotel market with both luxury and budget customers.
                     So strong relationship in this hotel chain and british and German airlines is explainable"
                   ),
                   tags$li(
                     "There is also strong negative correlation between both the airlines stock prices and Euro."
                   )
                   )
               )),
               column(6, plotOutput("plotCorr"))),
               fluidRow(sidebarLayout(
                 sidebarPanel(
                   div(class = "slider2"),
                   sliderInput(
                     "window",
                     label = "Window(days) for rolling correlation:",
                     min = 30,
                     max = 100,
                     value = 90,
                     step = 10
                   )
                 ),
                 mainPanel(tabsetPanel(
                   type = "tabs",
                   tabPanel(
                     "DLAKY & ICAGY rolling correlation",
                     plotlyOutput("plotDlakyIcagy")
                   ),
                   tabPanel(
                     "ICAGY & EURO rolling correlation",
                     plotlyOutput("plotICAGYEuro")
                   )
                 ))
               ))
             ),
             
             
             tabPanel(
               "Market Risk",
               fluidRow(sidebarLayout(
                 sidebarPanel(
                   div(
                     class = "slider1",
                     tags$p(
                       "A quantile divides the returns distribution into two groups. For example 75% of all returns may fall below a return value of 10%. The distribution is thus divided into returns above 10% and below 10% at the 75% quantile."
                     ),
                     tags$p(
                       "Pull slide to the right to measure the risk of returns at desired quantile levels. The minimum risk quantile is 75%. The maximum risk quantile is 99%."
                     )
                   ),
                   sliderInput(
                     "alpha.q",
                     label = "Risk Measure quantiles - Stocks:",
                     min = 0.75,
                     max = 0.99,
                     value = 0.75,
                     step = 0.01
                   )
                 ),
                 mainPanel(
                   tabsetPanel(
                     type = "tabs",
                     tabPanel("ACCYY Returns Distribution", plotlyOutput("plotVarAccyy")),
                     tabPanel("ICAGY Returns Distribution", plotlyOutput("plotVarICAGY")),
                     tabPanel("DLAKY Returns Distribution", plotlyOutput("plotVarDLAKY")),
                     tabPanel("IHG Returns Distribution", plotlyOutput("plotVarIHG"))
                   )
                 )
               )),
               tags$hr(style = "border-color: red;border-top: 3px solid #446e9b;"),
               tags$br(""),
               fluidRow(sidebarLayout(
                 sidebarPanel(
                   sliderInput(
                     "alpha.q2",
                     label = "Risk Measure quantiles - Currencies:",
                     min = 0.75,
                     max = 0.99,
                     value = 0.75,
                     step = 0.01
                   )
                 ),
                 mainPanel(tabsetPanel(
                   type = "tabs",
                   tabPanel("EUR Returns Distribution", plotlyOutput("plotVarEUR")),
                   tabPanel("GBP Returns Distribution", plotlyOutput("plotVarGBP"))
                 ))
               ))
             ),
             
             tabPanel(
               "Extremes",
               tabsetPanel(
                 type = "tabs",
                 tabPanel("Mean Excess Loss", plotlyOutput("plotExcessLoss")),
                 tabPanel("GPD fit", plotlyOutput("plotGPD")),
                 tabPanel("Confidence and Risk Measures", plotOutput("plotConf"))
               )
             ),
             
    tabPanel(
      "Portfolio",
      #fluidRow(includeMarkdown("Rmarkdownfiles.Rmd")),
      tabsetPanel(
        type = "tabs",
        tabPanel("Markowitz Model: Default", plotOutput("plotMarkovSHort")),
        tabPanel("Markowitz Model: No short", plotOutput("plotMarkovNoShort")),
        tabPanel("Portfolio Allocation", 
                 fluidRow(
                   column(6, dataTableOutput('table')),
                   column(6, plotOutput("plotAllocation"))
                 )),
        tabPanel("Simulation",
                 fluidRow(
                   column(6, plotOutput("plotCumReturns")),
                   column(6, plotOutput("plotDrawDown"))
                 )),
        tabPanel("Risk Offset", plotOutput("plotRisk"))
      )
    ),
             
             tabPanel(
               "Conclusion",
               tags$h5(tags$b("Skills and Tools")),
               tags$ol(
                 tags$li(
                   "Packages: ggplot, scales, quadprog, quantreg, shiny, flexdashboard, qrmdata, xts, matrixStats, zoo, QRM, plotly, and psych"
                 ),
                 tags$li(
                   "Created time series plots with limit functions to plot the rise/fall of the commodity prices over time in both real and absolute values."
                 )
               ),
               tags$h5(tags$b("For the working capital accounts:")),
               tags$p(
                 "We will be using the weights calculated from the GVM model. Therefore, we will need to comprise our portfolio with approximately 2.47 % into IHG and 84.13% into EUR and 13.39 into GBP."
               ),
               tags$ol(
                 tags$li("250 million denominated USD"),
                 tags$li("210.325 million in EUR"),
                 tags$li("33.475 million in GB"),
                 tags$li("6.175 million in IHG")
               ),
               tags$h5(tags$b("Risk Offset")),
               tags$ol(
                 tags$li(
                   "Objective: To fund the new contract at this tolerance for risk and with these reserves."
                 ),
                 tags$li("Risky contract + collateral = portfolio value."),
                 tags$p("250 + .8464*(250) = 461.6 Million Dollars"),
                 tags$li(
                   "Since we didn't hear anything in the project description, we assumed 100 % of the portfolio was the risky contract"
                 ),
                 tags$li("Portfolio value = 250 million dollars"),
                 tags$li("Collateral value = 211.6 million dollars")
               ),
               tags$h5(tags$b("Business Remarks")),
               tags$ol(
                 tags$li(
                   "This is a high-risk portfolio. As our risk measure quantiles increase, the expected shortfall and VaR grow larger."
                 ),
                 tags$li(
                   "A risk-free interest rate is a rate an investor can expect to earn without risk. This rate compensates investors with the time value of money. Treasury Bills are one of the safest investments since they are backed up by the Treasury Department of the Unites States. Treasury Bills are sold at a discount rate which is referred to as the par value. The team since this was the safest interest free opportunity possible. The difference between the face value of bill and the par value is called the discount rate, which is usually calculated as a percentage. In this case we are using the daily treasury yield curve CMT rates of 2.49% for 4 weeks coupon equivalent rate taken from the U.S Department of Treasury page and divide it by 253 trading days in a year."
                 )
               ),
               tags$br("")
             ),
             tabPanel("Personal Contribution",
                      tags$ol(tags$h1("Graham Latsa:"),
                              tags$p(""),
                              tags$ol("For this project I performed three major tasks for the team, 1: Coming up with the Project idea, 2: Coordinating the Project, 3: Initilizing the code."),
                              tags$p(""),
                              tags$p("When the team first started talking about the project the team was alittle confused about what we could do, so I helped do a Brainstorm, and then"),
                              tags$p(""),
                              tags$p("once we all decided on the idea I submitted my idea to the professor to see if we were on the right track.  From there I helped tweek the idea and"),
                              tags$p(""),
                              tags$p("then we finalized on this idea for the project.  Next, I was per-say the project manager, I set up the meetings, I helped assign tasks to others, and I "),
                              tags$p(""),
                              tags$p("reviewed others work.  It was a team effort and my role was to coordinate the team to be successful Finally, I initalized the code.  I figured out how to"),
                              tags$p(""),
                              tags$p("download all the data, figured out where the gaps and wholes were in the data, I solved how to get 6 stocks into the Markowitz model and get the"),
                              tags$p(""),
                              tags$p("distribution, and then got the data in an approriate way so the rest of the team could create their charts and models.")
                              )),
             
             tabPanel(
               "References",
               tags$a("https://shiny.rstudio.com/tutorial/"),
               tags$p(""),
               tags$a("https://github.com/DCMSstats/tourism-dashboard"),
               tags$p(""),
               tags$a(
                 "https://turing.manhattan.edu/~wfoote01/finalytics/_site/topics.html"
               ),
               tags$p(""),
               tags$a(
                 "http://www.ece.ust.hk/~palomar/MAFS6010R_lectures/week%2010/Rsession_risk_parity_portfolio.html"
               ),
               tags$p(""),
               tags$a(
                 "http://past.rinfinance.com/agenda/2009/xts_quantmod_workshop.pdf"
               ),
               tags$p(""),
               tags$a("http://past.rinfinance.com/agenda/2009/ChicagoWuertz.pdf"),
               tags$p(""),
               tags$a(
                 "https://cran.r-project.org/web/packages/portfolio/vignettes/portfolio.pdf"
               ),
               tags$p(""),
               tags$a(
                 "https://blogs.worldbank.org/psd/20-reasons-you-should-integrate-tourism-your-development-agenda"
               ),
               tags$p(""),
               tags$a(
                 "https://www.iata.org/publications/economics/Reports/pax-monthly-analysis/passenger-analysis-jan-2019.pdf"
               ),
               tags$p(""),
               tags$a(
                 "https://skift.com/2019/02/01/international-travel-slowdown-hits-u-s/"
               ),
               tags$p(""),
               tags$a("https://finance.yahoo.com/quote/ACCYY/"),
               tags$p(""),
               tags$a(
                 "https://www.iata.org/publications/economics/Reports/pax-monthly-analysis/passenger-analysis-jan-2019.pdf"
               ),
               tags$p(""),
               tags$a("https://finance.yahoo.com/quote/LHA.DE/"),
               tags$p(""),
               tags$a("https://finance.yahoo.com/quote/ICAGY/"),
               tags$p(""),
               tags$a("https://finance.yahoo.com/quote/IHG?ltr=1"),
               tags$p(""),
               tags$a(
                 "https://finance.yahoo.com/m/584ad02e-52f6-3840-bc52-5deb84e7ae8e/ss_do-institutions-own-design.html"
               ),
               tags$p(""),
               tags$a("https://finance.yahoo.com/quote/IHG?ltr=1"),
               tags$p(""),
               tags$a(
                 "https://simplywall.st/stocks/gb/consumer-services/lse-ihg/intercontinental-hotels-group-shares/news/what-do-analysts-think-about-intercontinental-hotels-group-plcs-lonihg-future/"
               )
             )
             
             )
             ))
  