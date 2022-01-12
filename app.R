#####################################################################################
###
### Power calculator for serology studies
###   -dashboardpage option
###   Not having the finite population correction factor
###
### Denver 16, 2021
### Amy J. Davis
###
#####################################################################################

### Libraries
library(shiny)
library(shinyjs)
library(shinycssloaders)
library(viridis)
library(viridisLite)
library(shinydashboard)
library(pwr)
library(shinyWidgets)
library(fields)
library(plyr)


# Define UI f
ui <- dashboardPage(
  
  skin='green',
  title="SeroprevalenceApp",
  header = dashboardHeader(titleWidth='100%',
                           # Set height of dashboardHeader
                           tags$li(class = "dropdown",
                                   tags$style(".main-header {max-height: 100px}"),
                                   tags$style(".main-header .logo {height: 100px;}"),
                                   tags$style(".sidebar-toggle {height: 100px; padding-top: 1px !important;}"),
                                   tags$style(".navbar {min-height:100px !important}")
                           ),
                           title=span(tags$img(src='USDA_bw_transparent.png', style='margin-top:8px;',height=90,width=120,align="left"),
                                      column(10, class="title-box", 
                                             tags$h1(class="primary-title", style='margin-top:25px;font-size=50px',
                                                     "Power Analyses for Seroprevalence Studies")))),
  
  # Sidebar  
  dashboardSidebar(
    tags$style(".left-side, .main-sidebar {padding-top: 110px}"),
    width=450,
    title=tags$h2(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"Study type",align='left'),
    useShinyjs(),
    # This makes web page load the JS file in the HTML head.
    # The call to singleton ensures it's only included once
    # in a page. It's not strictly necessary in this case, but
    # it's good practice.
    singleton(tags$head(tags$script(src = "message-handler.js"))),
    
    
    tags$h5("The power analysis will change depending on the type of study you want to conduct." ,style='margin-top:8px;margin-left:15px;', align='left'),
    
    ##### Trail Info Inputs:
    selectInput("whichpn", "What kind of study are you doing?",
                choices = c("  ","A) Estimate single seroprevalence (either pre or post baiting)",
                            "B) Compare seroprevalence in two conditions (either different treatments or change in one treatment after baiting)",
                            "C) Compare changes in seroprevalence between two treatments"),
                selected = "  ",width = "100%"),
    tabItem("User Inputs",selectInput("sorp","Which do you want to estimate?",choices=
                                        c("Sample size","Power"),selected = "Power")),
    HTML("<hr>"),
    sidebarMenuOutput("inputmenu")
  ),
  
  # Show output
  dashboardBody(
    uiOutput("dynamic_tabs")
    
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  output$inputmenu <- renderMenu({
    if(input$whichpn=="  "){
      
    }else if(input$whichpn=="A) Estimate single seroprevalence (either pre or post baiting)"&input$sorp=="Power"){
      sidebarMenu(
        menuItem(tags$h2(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"User Inputs",align='left'),
                 tabItem("User Inputs",sliderInput("seroprev", "True seroprevalence (S)", min = 0, max = 1,value = 0.2)),
                 tabItem("User Inputs",sliderInput("samplea", "# of animals sampled", min = 0, max = 200,value = 30)),
                 tags$h3("Power analysis options"),
                 tabItem("User Inputs",sliderInput("serocomp","What seroprevalence (S0) do you want be able to say your value is different from?",min=0,max=1,value=0)),
                 tabItem("User Inputs",selectInput("compdirection","What comparison do you want to test?",choices=
                                                     c("Greater than S0","Different from S0","Less than S0"),selected = "Different from S0")),
                 tabItem("User Inputs",radioButtons("alpha","What type I error are you willing to accept (alpha)?",
                                                    choiceNames = list("0.01","0.05","0.10","0.20"),
                                                    choiceValues = list("0.01","0.05","0.10","0.20"),selected = "0.05",inline = TRUE))
        )
      )
    }else if(input$whichpn=="A) Estimate single seroprevalence (either pre or post baiting)"&input$sorp=="Sample size"){
      sidebarMenu(
        menuItem(tags$h2(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"User Inputs",align='left'),
                 tabItem("User Inputs",sliderInput("seroprev", "True seroprevalence (S)", min = 0, max = 1,value = 0.2)),
                 tags$h3("Power analysis options"),
                 tabItem("User Inputs",sliderInput("powera", "Power you want", min = 0.05, max = 0.95,value = 0.8)),
                 tabItem("User Inputs",sliderInput("serocomp","What seroprevalence (S0) do you want be able to say your value is different from?",min=0,max=1,value=0)),
                 tabItem("User Inputs",selectInput("compdirection","What comparison do you want to test?",choices=
                                                     c("Greater than S0","Different from S0","Less than S0"),selected = "Different from S0")),
                 tabItem("User Inputs",radioButtons("alpha","What type I error are you willing to accept (alpha)?",
                                                    choiceNames = list("0.01","0.05","0.10","0.20"),
                                                    choiceValues = list("0.01","0.05","0.10","0.20"),selected = "0.05",inline = TRUE))
        )
      )
    }else if(input$whichpn=="B) Compare seroprevalence in two conditions (either different treatments or change in one treatment after baiting)"&input$sorp=="Power"){
      sidebarMenu(
        menuItem(tags$h2(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"User Inputs",align='left'),
                 tabItem("User Inputs",sliderInput("seroprev1", "True seroprevalence treatment A", min = 0, max = 1,value = 0.2)),
                 tabItem("User Inputs",sliderInput("seroprev2", "True seroprevalence treatment B", min = 0, max = 1,value = 0.5)),
                 tabItem("User Inputs",sliderInput("samplea1", "# of animals sampled treatment A", min = 0, max = 200,value = 30)),
                 tabItem("User Inputs",sliderInput("samplea2", "# of animals sampled treatment B", min = 0, max = 200,value = 30)),
                 tags$h3("Power analysis options"),
                 tabItem("User Inputs",radioButtons("alpha","What type I error are you willing to accept (alpha)?",
                                                    choiceNames = list("0.01","0.05","0.10","0.20"),
                                                    choiceValues = list("0.01","0.05","0.10","0.20"),selected = "0.05",inline = TRUE))
        )
      )
    }else if(input$whichpn=="B) Compare seroprevalence in two conditions (either different treatments or change in one treatment after baiting)"&input$sorp=="Sample size"){
      sidebarMenu(
        menuItem(tags$h2(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"User Inputs",align='left'),
                 tabItem("User Inputs",sliderInput("seroprev1", "True seroprevalence treatment A", min = 0, max = 1,value = 0.2)),
                 tabItem("User Inputs",sliderInput("seroprev2", "True seroprevalence treatment B", min = 0, max = 1,value = 0.5)),
                 tags$h3("Power analysis options"),
                 tabItem("User Inputs",sliderInput("powera", "Power you want", min = 0.05, max = 0.95,value = 0.8)),
                 tabItem("User Inputs",radioButtons("alpha","What type I error are you willing to accept (alpha)?",
                                                    choiceNames = list("0.01","0.05","0.10","0.20"),
                                                    choiceValues = list("0.01","0.05","0.10","0.20"),selected = "0.05",inline = TRUE))
        )
      )
    }else if(input$whichpn=="C) Compare changes in seroprevalence between two treatments"&input$sorp=="Power"){
      sidebarMenu(
        menuItem(tags$h2(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"User Inputs",align='left'),
                 tabItem("User Inputs",sliderInput("seroprevA1", "Expected seroprevalence pre-bait treatment A", min = 0, max = 1,value = 0.2)),
                 tabItem("User Inputs",sliderInput("seroprevA2", "Expected seroprevalence post-bait treatment A", min = 0, max = 1,value = 0.3)),
                 tabItem("User Inputs",sliderInput("seroprevB1", "Expected seroprevalence pre-bait treatment B", min = 0, max = 1,value = 0.2)),
                 tabItem("User Inputs",sliderInput("seroprevB2", "Expected seroprevalence post-bait treatment B", min = 0, max = 1,value = 0.5)),
                 tags$h3("Power analysis options"),
                 tabItem("User Inputs",sliderInput("sampleaA1", "# of animals sampled pre-bait treatment A", min = 0, max = 200,value = 30)),
                 tabItem("User Inputs",sliderInput("sampleaA2", "# of animals sampled post-bait treatment A", min = 0, max = 200,value = 30)),
                 tabItem("User Inputs",sliderInput("sampleaB1", "# of animals sampled pre-bait treatment B", min = 0, max = 200,value = 30)),
                 tabItem("User Inputs",sliderInput("sampleaB2", "# of animals sampled post-bait treatment B", min = 0, max = 200,value = 30)),
                 tabItem("User Inputs",radioButtons("alpha","What type I error are you willing to accept (alpha)?",
                                                    choiceNames = list("0.01","0.05","0.10","0.20"),
                                                    choiceValues = list("0.01","0.05","0.10","0.20"),selected = "0.05",inline = TRUE))
        )
      )
    }else if(input$whichpn=="C) Compare changes in seroprevalence between two treatments"&input$sorp=="Sample size"){
      sidebarMenu(
        menuItem(tags$h2(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"User Inputs",align='left'),
                 tabItem("User Inputs",sliderInput("seroprevA1", "Expected seroprevalence pre-bait treatment A", min = 0, max = 1,value = 0.2)),
                 tabItem("User Inputs",sliderInput("seroprevA2", "Expected seroprevalence post-bait treatment A", min = 0, max = 1,value = 0.3)),
                 tabItem("User Inputs",sliderInput("seroprevB1", "Expected seroprevalence pre-bait treatment B", min = 0, max = 1,value = 0.2)),
                 tabItem("User Inputs",sliderInput("seroprevB2", "Expected seroprevalence post-bait treatment B", min = 0, max = 1,value = 0.5)),
                 tags$h3("Power analysis options"),
                 tabItem("User Inputs",sliderInput("powera", "Power you want", min = 0.05, max = 0.95,value = 0.8)),
                 tabItem("User Inputs",radioButtons("alpha","What type I error are you willing to accept (alpha)?",
                                                    choiceNames = list("0.01","0.05","0.10","0.20"),
                                                    choiceValues = list("0.01","0.05","0.10","0.20"),selected = "0.05",inline = TRUE))
        )
      )
    }
    
  })
  
  observe({
    n=input$samplea
    p=input$seroprev
    p0=input$serocomp
    alpha=as.numeric(input$alpha)
    dval=ifelse(input$compdirection=="Different from S0",2,1)
    
    nx80=p*(1-p)*((qnorm(1-alpha/dval)+qnorm(1-0.2))/(p-p0))^2
    nmax=max(200,round_any(nx80,100,ceiling))
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session, "samplea",
                      min = 0, max = nmax)
  })
  
  
  observe({
    pA=input$seroprev1
    pB=input$seroprev2
    alpha=as.numeric(input$alpha)
    betax=ifelse(input$sorp=="Power",0.2,1-input$powera)
    dval=1
    D0=0
    kappa=1
    n12=(pA*(1-pA)/kappa+pB*(1-pB))*((qnorm(1-alpha/dval)+qnorm(1-betax))/(pA-pB))^2
    nmax=max(200,round_any(n12,100,ceiling))
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session, "samplea1",
                      min = 0, max = nmax)
    updateSliderInput(session, "samplea2",
                      min = 0, max = nmax)
  })
  
  observe({
    pA1=input$seroprevA1
    pA2=input$seroprevA2
    pB1=input$seroprevB1
    pB2=input$seroprevB2
    alpha=as.numeric(input$alpha)
    betax=ifelse(input$sorp=="Power",0.2,1-input$powera)
    dval=1
    D0=0
    k1=1
    k2=1
    k3=1
    nA1=((pA1*(1-pA1)*k1*k2*k3+pA2*(1-pA2)*k2*k3+pB1*(1-pB1)*k1*k3+pB2*(1-pB2)*k1*k2)/(k1*k2*k3))*((qnorm(1-alpha)+qnorm(1-betax))/abs(max(pA2-pA1,0)-max(pB2-pB1)))^2
    
    nmax=max(200,round_any(nA1,100,ceiling))
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session, "sampleaA1",
                      min = 0, max = nmax)
    updateSliderInput(session, "sampleaA2",
                      min = 0, max = nmax)
    updateSliderInput(session, "sampleaB1",
                      min = 0, max = nmax)
    updateSliderInput(session, "sampleaB2",
                      min = 0, max = nmax)
  })
  
  #####
  ###
  ### TRying to add dynamic tabs
  ###
  #####
  
  output$dynamic_tabs <-renderUI({
    
    if(input$whichpn=="  "){
      tabsetPanel(
        tabPanel("How to",
                 box(width=12,title=span("How to use this seroprevalence power calculator",style="color:green;font-size:28px"),status="success",
                     # 
                     column(6,p("This power calculator is designed to help with planning seroprevalence studies. The power calculator options and results will change depending on the type of study you want to conduct.  There are three options for study types in this app:",style="font-size:130%;"),
                            p(strong("     A) Estimate single seroprevalence (either pre or post baiting)",style="font-size:130%;")),
                            p(strong("     B) Compare seroprevalence in two conditions (either different treatments or change in one treatment after baiting)",style="font-size:130%;")),
                            p(strong("     C) Compare changes (from pre- to post-bait) in seroprevalence between two treatments",style="font-size:130%;")),
                            p("You will need to select which type of study you would like to conduct from the panel on the left. You also need to determine if you want to estimate how many samples you will need (for a set Power level) or if you want to calculate the Power from sample sizes that you set.  Once you make those selections you will be able to change the values to fit your expected study and see the results from those selections. ",style="font-size:130%;"),
                            p("A quick primer on power analyses is shown on the next tab.",style="font-size:130%;"))
                 )     
        ),
        tabPanel("What is Power?",
                 tags$iframe(style="height:1000px; width:100%; scrolling=yes", 
                             src="What is statistical Power.pdf"))
      )
      
    }else if(input$whichpn=="A) Estimate single seroprevalence (either pre or post baiting)"){
      tabsetPanel(
        tabPanel("Quick Results",
                 box(width=12,title=span("Single season seroprevalence estimatation",style="color:green;font-size:28px"),status="success",
                     # 
                     column(6,p("This option of the seroprevalence power calculator app is used to examine how many samples you would need to estimate a single season (either pre- or post- baiting) seroprevalence estimate. Change the User Inputs on the left panel to see how different conditions would change the uncertainty in your seroprevalence estimates (S) and power to detect change. You need to set a baseline value you want to test your seroprevalence against (S0), the default value for this is zero (assuming a naïve population). ",style="font-size:130%;"),
                            p("Note: an error will occur if you set S greater than S0 and select a Less-than test or if you set S less than S0 and select a Greater-than test.",style="color:red;font-size:130%;"))
                 ), 
                 fluidRow(
                   box(width=4,title=span("User specified data",style="color:white;font-size:28px"),background="black"),
                   box(width=4,title=span("Analysis results",style="color:white;font-size:28px"),background="black")
                 ),
                 fluidRow(
                   infoBoxOutput("samplebox"),
                   valueBoxOutput("powerbox")
                 ),
                 fluidRow(
                   infoBoxOutput("diffbox"),
                   valueBoxOutput("samp80box")
                 ),
                 fluidRow(
                   infoBoxOutput("alphabox"),   
                   valueBoxOutput("errorbox")
                 )
                 
        ),      
        tabPanel("Uncertainty",
                 box(width=12,title=span("Sample uncertainty",style="color:green;font-size:28px"),status="success",
                     column(8,p("This tab shows the relationship between different factors and the estimated uncertainty around seroprevalence estimates.  There are several factors that will influence the certainty you will have around the estimate of seroprevalence: the number of animals you sample (n) and the seroprevalence of the population (s).  The figures here demonstrate these relationships.  You can play around with these three values to see how the uncertainty will change depending on different conditions.  ",style="font-size:130%;"))   
                 ),
                 fluidRow(
                   valueBoxOutput("moebox")
                 ),
                 fluidRow(
                   box(width=12,title=span("Relationship between seroprevalence, sample size, and uncertainty",style="color:green;font-size:28px"),status="success",
                       column(6,plotOutput('seroerrplot')),
                       column(6,plotOutput('samperrplot')),
                       column(1,tags$br(),tags$br(),dropdownButton(label = "How to interpret plots",icon = icon("info"),
                                                                   status = "primary",
                                                                   circle = TRUE,size="xs",tooltip=TRUE,
                                                                   textOutput("info.sero.error")))))
        ),
        
        tabPanel("Power Analysis",
                 box(width=12,title=span("Power Analysis",style="color:green;font-size:28px"),status="success",
                     # varImp Plot
                     column(6,plotOutput('powerplot')),
                     column(6,p("The power analysis for this single season seroprevalence estimates requires a value of seroprevalence that you want to test your estimate against (this will be denoted as S0, a value used to compare the seroprevalence you expect in your null hypothesis H0). To conduct this power analysis you need to specify the value for this null hypothesis. You also need to specify if you want to be able to say your value of seroprevalence is greater than, different than (two sided), or less than the null hypothesis value of seroprevalence.  You also need to specify the type I error (alpha - significance (alpha) level) that you are willing to accept.  The options are limited to standard values that people use.  The results will show the power you would have to detect difference specified.  Typically, a power of 0.8 is regarded as good, but the higher the better.  Play around with the values to see where the limitations are. ",style="font-size:130%;")),
                     column(1,tags$br(),tags$br(),dropdownButton(label = "How to interpret plots",icon = icon("info"),
                                                                 status = "primary",
                                                                 circle = TRUE,size="xs",tooltip=TRUE,
                                                                 textOutput("info.pow.plotA")))
                 ), 
                 fluidRow(
                   valueBoxOutput("powresbox"),
                   infoBoxOutput("power80box",width = 6)
                 ),
                 box(width=12,title=span("Visual depiction of Power",style="color:green;font-size:28px"),status="success",
                     # varImp Plot
                     column(6,plotOutput('powerplotdist')),
                     column(6,p("The figure to the left is a visual depiction of what power is. The light blue distribution represents the null hypothesis (essentially that there is no difference between your seroprevalence of interest -S- and the value you are testing against – S0).  The shaded red distribution is the alternative distribution (S-S0). The dark black represents the regions of the distribution where Type I error might occur based on the significance (alpha) level you set. The bold red area is the area of the alternative distribution that represents the power. The larger this region is the greater the power.  You can see how changing the significance (alpha) level changes the power as the significance (alpha) level sets where the line is drawn for both the Type I and Type II errors.  ",style="font-size:130%;")),
                     column(1,tags$br(),tags$br(),dropdownButton(label = "How to interpret plots",icon = icon("info"),
                                                                 status = "primary",
                                                                 circle = TRUE,size="xs",tooltip=TRUE,
                                                                 textOutput("info.pow.distA")))
                 ) 
                 
        )
      )
    }else if(input$whichpn=="B) Compare seroprevalence in two conditions (either different treatments or change in one treatment after baiting)"){
      tabsetPanel(
        tabPanel("Quick Results",
                 box(width=12,title=span("Single season two treatment or two season single treatment seroprevalence estimatation",style="color:green;font-size:28px"),status="success",
                     # 
                     column(6,p("This option of the seroprevalence power calculator app is used to examine how many samples you would need to compare two seroprevalences estimates.  These two estimates can either be from two different treatments (different populations) or from the same population but comparing before and after baiting. Change the User Inputs on the left panel to see how different conditions would change the uncertainty in your seroprevalence estimates and power to detect change.",style="font-size:130%;"),
                            p("In this option we are only looking at a one-sided comparison where one treatment is greater than the other. ",style="font-size:130%;"))
                 ), 
                 fluidRow(
                   box(width=4,title=span("User specified data",style="color:white;font-size:28px"),background="black"),
                   box(width=4,title=span("Analysis results",style="color:white;font-size:28px"),background="black")
                 ),
                 fluidRow(
                   infoBoxOutput("sampleboxB"),
                   valueBoxOutput("powerboxB")
                 ),
                 fluidRow(
                   infoBoxOutput("diffboxB"),
                   valueBoxOutput("samp80boxB")
                 ),
                 fluidRow(
                   infoBoxOutput("alphaboxB"),   
                   valueBoxOutput("errorboxB")
                 ),
                 box(width=12,title=span("Model Metrics",style="color:green;font-size:28px"),status="success",
                     #confusion matrix, model accuracy metrics
                     column(6,withSpinner(plotOutput('propplot2B'))),
                     column(6,p("This is a visual depiction of the uncertainty around a seroprevalence estimates given the User Inputs.  These boxplots show the theoretical differences in the estimates of seroprevalence given the User Input conditions. The less overlap in the boxes the greater the power to conclude they are different.  ",style="font-size:130%;"))   
                 )
                 
        ),      
        tabPanel("Uncertainty",
                 box(width=12,title=span("Sample uncertainty",style="color:green;font-size:28px"),status="success",
                     column(6,plotOutput('propplotB')),
                     column(6,p("This option of the seroprevalence power calculator app is used to examine how many samples you would need to compare two seroprevalences estimates.  These two estimates can either be from two different treatments (different populations) or from the same population but comparing before and after baiting.  To understand how different samples sizes (n) and seroprevalences (s) would impact the uncertainty around the estimates, you can change the values in the User Input panel to the left.  The uncertainty is directly related to the power to detect a difference, so understand how these factors influence uncertainty can help with planning for your anticipated situation. ",style="font-size:130%;"))   
                 ),
                 fluidRow(
                   valueBoxOutput("moeboxB")
                 )
        ),
        
        tabPanel("Power Analysis",
                 box(width=12,title=span("Power Analysis",style="color:green;font-size:28px"),status="success",
                     # varImp Plot
                     column(width = 6,withSpinner(plotOutput('powerplotB',height = "600px"))),
                     column(6,p("This option of the seroprevalence power calculator app is used to examine how many samples you would need to compare two seroprevalences estimates.  These two estimates can either be from two different treatments (different populations) or from the same population but comparing before and after baiting.  Depending on the expected seroprevalence values in the two conditions you may need more samples in one site or time period compared to another. You can play around with different sampling options when you estimate power, but when you estimate sample size this app assumes constant sampling across treatments.",style="font-size:130%;"))), 
                 fluidRow(
                   valueBoxOutput("powresboxB"),
                   infoBoxOutput("power80boxB",width = 6)
                 ),
                 box(width=12,title=span("Visual depiction of Power",style="color:green;font-size:28px"),status="success",
                     # varImp Plot
                     column(6,plotOutput('powerplotdistB')),
                     column(6,p("The figure to the left is a visual depiction of what power is. The light blue distribution represents the null hypothesis (essentially that there is no difference between your seroprevalence of interest -S- and the value you are testing against – S0).  The shaded red distribution is the alternative distribution (S-S0). The dark black represents the regions of the distribution where Type I error might occur based on the significance (alpha) level you set. The bold red area is the area of the alternative distribution that represents the power. The larger this region is the greater the power.  You can see how changing the significance (alpha) level changes the power as the significance (alpha) level sets where the line is drawn for both the Type I and Type II errors.  ",style="font-size:130%;"))
                 ) 
        )
      )
    }else if(input$whichpn=="C) Compare changes in seroprevalence between two treatments"){
      tabsetPanel(
        tabPanel("Quick Results",
                 box(width=12,title=span("Some fun tests",style="color:green;font-size:28px"),status="success",
                     # 
                     column(6,p("This option of the seroprevalence power calculator app is used to examine how many samples you would need to compare the change in seroprevalence (pre- to post- bait) in two treatments. This option looks at the impact of different treatments while accounting for the baseline seroprevalence in each area prior to treatment. Change the User Inputs on the left panel to see how different conditions would change the uncertainty in your seroprevalence estimates and power to detect change.",style="font-size:130%;"),
                            p("In this version we are comparing the differences in pre- to post- bait changes in seroprevalence between to treatments. We are only interested in positive differences assuming that baiting will increase seroprevalence.  These differences are compared using a one-sided comparison where one difference is greater than another.",style="font-size:130%;"))
                 ), 
                 fluidRow(
                   box(width=4,title=span("User specified data",style="color:white;font-size:28px"),background="black"),
                   box(width=4,title=span("Analysis results",style="color:white;font-size:28px"),background="black")
                 ),
                 fluidRow(
                   infoBoxOutput("sampleboxD"),
                   valueBoxOutput("powerboxD")
                 ),
                 fluidRow(
                   infoBoxOutput("diffboxD"),
                   valueBoxOutput("power80boxD")
                 ),
                 fluidRow(
                   infoBoxOutput("alphaboxD"),   
                   valueBoxOutput("errorboxD")
                 ),
                 box(width=12,title=span("Model Metrics",style="color:green;font-size:28px"),status="success",
                     #confusion matrix, model accuracy metrics
                     column(6,withSpinner(plotOutput('propplot2D'))),
                     column(6,p("This is a visual depiction of the uncertainty around a seroprevalence estimates given the User Inputs.  These boxplots show the theoretical differences in the estimates of seroprevalence given the User Input conditions.  ",style="font-size:130%;"))   
                 )
                 
        ),      
        tabPanel("Uncertainty",
                 box(width=12,title=span("Sample uncertainty",style="color:green;font-size:28px"),status="success",
                     column(6,p("This option of the seroprevalence power calculator app is used to examine how many samples you would need to compare the change in seroprevalence (pre- to post- bait) in two treatments.  Therefore, the values of interest here will be the differences in seroprevalence in one treatment vs the other.  It might help to visualize this difference instead of the box plots of the actual seroprevalences (the right plot below). ",style="font-size:130%;")),   
                     column(6,p("The margin of error is calculated for the test statistic that uses all four seroprevalence estimates in this case. You can see how changing the sample size or seroprevalence for one treatment and time period will impact the margin of error by changes the values in the User Input panel. ",style="font-size:130%;"))   
                 ),
                 box(width=12,title=span("Uncertainty plots",style="color:green;font-size:28px"),status="success",
                     column(6,plotOutput('propplotD')),
                     column(6,plotOutput("Difftrtplot"))
                 ),
                 fluidRow(
                   valueBoxOutput("moeboxD")
                 )
        ),
        
        tabPanel("Power Analysis",
                 box(width=12,title=span("Power Analysis",style="color:green;font-size:28px"),status="success",
                     # varImp Plot
                     column(width = 6,plotOutput('powerplotCA',height = "600px")),
                     column(width = 6,plotOutput('powerplotCB',height = "600px"))
                 ),
                 box(width=12,title=span("Power Analysis",style="color:green;font-size:28px"),status="success",
                     column(8,p("This option of the seroprevalence power calculator app is used to examine how many samples you would need to compare the change in seroprevalence (pre- to post- bait) in two treatments. Depending on the expected seroprevalence values in the two conditions you may need more samples in one site or time period compared to another. You can play around with different sampling options when you estimate power, but when you estimate sample size this app assumes constant sampling across treatments.",style="font-size:130%;"))), 
                 fluidRow(
                   valueBoxOutput("powresboxC"),
                   infoBoxOutput("power80boxC",width = 6)
                 ),
                 box(width=12,title=span("Visual depiction of Power",style="color:green;font-size:28px"),status="success",
                     # varImp Plot
                     column(6,plotOutput('powerplotdistC')),
                     column(6,p("The figure to the left is a visual depiction of what power is. The light blue distribution represents the null hypothesis (essentially that there is no difference between your seroprevalence of interest -S- and the value you are testing against – S0).  The shaded red distribution is the alternative distribution (S-S0). The dark black represents the regions of the distribution where Type I error might occur based on the significance (alpha) level you set. The bold red area is the area of the alternative distribution that represents the power. The larger this region is the greater the power.  You can see how changing the significance (alpha) level changes the power as the significance (alpha) level sets where the line is drawn for both the Type I and Type II errors.  ",style="font-size:130%;"))
                 )
        )
      )
    }
  })
  
  
  ##############################################################################
  ###
  ### Tabs for option A
  ###
  ##############################################################################
  
  #####
  ###
  ### Quick results tab
  ###
  #####
  output$samplebox <- renderInfoBox({
    if(input$sorp=="Power"){
      infoBox(
        value=paste0(input$samplea), title="Sample size", icon = icon("otter"),
        color = "black"
      )
    }else {
      infoBox(
        value=paste0(input$powera), title="Desired power", icon = icon("earlybirds"),
        color = "black"
      )
    }
    
  })
  
  output$diffbox <- renderInfoBox({
    
    infoBox(
      value=paste0(abs(input$seroprev-input$serocomp)), title="Difference of interest", icon = icon("balance-scale-left"),
      color = "black"
    )
  })
  
  output$alphabox <- renderInfoBox({
    
    infoBox(
      value=as.numeric(paste0(input$alpha)), title="alpha value", icon = icon("font"),
      color = "black"
    )
  })
  
  output$powerbox <- renderValueBox({
    
    if(input$sorp=="Power"){
      p=input$seroprev
      p0=input$serocomp
      n=input$samplea
      alpha=as.numeric(input$alpha)
      dval=ifelse(input$compdirection=="Different from S0",2,1)
      betax=1-input$powera
      
      z=(p-p0)/sqrt(p*(1-p)/n)
      powerx=pnorm(z-qnorm(1-alpha/dval))+pnorm(-z-qnorm(1-alpha/dval))
      
      powval=ifelse((input$compdirection== "Greater than S0"&p0>p)|(input$compdirection== "Less than S0"&p0<p),"Error",paste0(round(powerx,2)))
      
      valueBox(
        value=powval,subtitle= "Power", icon = icon("earlybirds"),
        color = "orange"
      )
    }else {
      p=input$seroprev
      p0=input$serocomp
      alpha=as.numeric(input$alpha)
      dval=ifelse(input$compdirection=="Different from S0",2,1)
      betax=1-input$powera
      
      nx=p*(1-p)*((qnorm(1-alpha/dval)+qnorm(1-betax))/(p-p0))^2
      
      numval=ifelse((input$compdirection== "Greater than S0"&p0>p)|(input$compdirection== "Less than S0"&p0<p),"Error",paste0(ceiling(nx)))
      
      valueBox(
        value=numval,subtitle= "Sample size needed", icon = icon("otter"),
        color = "orange"
      )
    }
    
  })
  
  output$samp80box <- renderValueBox({
    p=input$seroprev
    p0=input$serocomp
    alpha=as.numeric(input$alpha)
    dval=ifelse(input$compdirection=="Different from S0",2,1)
    betax=1-0.8
    
    nx80=p*(1-p)*((qnorm(1-alpha/dval)+qnorm(1-betax))/(p-p0))^2
    
    num80val=ifelse((input$compdirection== "Greater than S0"&p0>p)|(input$compdirection== "Less than S0"&p0<p),"Error",ceiling(nx80))
    
    valueBox(
      subtitle = "Sample size needed for a power of 0.8", value = num80val, icon = icon("deezer"),
      color = "red"
    )
  })
  
  output$errorbox <- renderValueBox({
    p=input$seroprev
    n=input$samplea
    alpha=as.numeric(input$alpha)
    dval=ifelse(input$compdirection=="Different from S0",2,1)
    
    
    ### Margin of error
    me1=qnorm(1-alpha/dval)*sqrt(p*(1-p)/n)
    valueBox(
      subtitle="Margin of error",value =  round(me1,2), icon = icon("diagnoses"),
      color = "blue"
    )
  })
  
  
  
  
  
  #####
  ###
  ### Uncertainty tab
  ###
  #####
  
  
  output$moebox <- renderValueBox({
    p=input$seroprev
    n=input$samplea
    alpha=as.numeric(input$alpha)
    dval=ifelse(input$compdirection=="Different from S0",2,1)
    
    
    ### Margin of error
    me1=qnorm(1-alpha/dval)*sqrt(p*(1-p)/n)
    valueBox(
      subtitle="Margin of error",value =  round(me1,2), icon = icon("diagnoses"),
      color = "blue"
    )
  })
  
  
  output$seroerrplot <- renderPlot({
    
    ### Lets look at the simple binomial error by seroprevalence
    pseq=seq(0,1,0.01)
    p=input$seroprev
    n=input$samplea
    alpha=as.numeric(input$alpha)
    dval=ifelse(input$compdirection=="Different from S0",2,1)
    
    
    ### Margin of error
    me=qnorm(1-alpha/dval)*sqrt(pseq*(1-pseq)/n)
    me1=qnorm(1-alpha/dval)*sqrt(p*(1-p)/n)
    
    plot(pseq,me,type="l",xlab="Seroprevalence",ylab="Margin of error",lwd=2,main="Relationship between seroprevalence and uncertainty")
    points(p,me1,pch=16,col=2,cex=2)
    
  })
  
  output$samperrplot <- renderPlot({
    n=input$samplea
    p=input$seroprev
    p0=input$serocomp
    alpha=as.numeric(input$alpha)
    dval=ifelse(input$compdirection=="Different from S0",2,1)
    
    nx80=p*(1-p)*((qnorm(1-alpha/dval)+qnorm(1-0.2))/(p-p0))^2
    nseq=seq(1,max(200,round_any(nx80,100,ceiling)),1)
    
    men=qnorm(1-alpha/dval)*sqrt(p*(1-p)/nseq)
    men1=qnorm(1-alpha/dval)*sqrt(p*(1-p)/n)
    
    ### Plot showing how sample size impacts error
    plot(nseq,men,type="l",xlab="Sample Size",ylab="Margin of error",lwd=2,main="Relationship between sample size and uncertainty")
    points(n,men1,pch=16,col=2,cex=3)
    lines(c(0,n),c(men1,men1),col=2)
    lines(c(n,n),c(0,men1),col=2)
    
  })
  
  output$info.sero.error=renderText({
    "The plots show how the uncertainty around the seroprevalence estimate changes with the seroprevalence and the sample size."
  })
  
  
  
  
  #####
  ###
  ### Power tab
  ###
  #####
  output$powerplot <- renderPlot({
    p=input$seroprev
    p0=input$serocomp
    n=input$samplea
    alpha=as.numeric(input$alpha)
    alphad=ifelse(input$compdirection=="Different from S0",1-alpha/2,
                  ifelse(input$compdirection=="Greater than S0",1-alpha,1-alpha))
    betax=1-input$powera
    
    nx=p*(1-p)*((qnorm(alphad)+qnorm(1-betax))/(p-p0))^2
    nx80=p*(1-p)*((qnorm(alphad)+qnorm(1-0.2))/(p-p0))^2
    
    nseq=seq(1,max(200,round_any(nx80,100,ceiling)),1)
    zseq=(p-p0)/sqrt(((p*(1-p))/(nseq)))
    powerxseq=pnorm(zseq-qnorm(alphad))+pnorm(-zseq-qnorm(alphad))
    z=(p-p0)/sqrt(((p*(1-p))/(n)))
    powerx=pnorm(z-qnorm(alphad))+pnorm(-z-qnorm(alphad))
    
    if(input$compdirection== "Greater than S0"&p0>p){
      plot(0,0,xaxt="n",yaxt="n",xlab="",ylab="",col="white",bty="n")
      text(0,0,"Error: S is less than S0 and you have selection a Greater-than test",col="red",cex=1.3)
    } else if(input$compdirection== "Less than S0"&p>p0){
      plot(0,0,xaxt="n",yaxt="n",xlab="",ylab="",col="white",bty="n")
      text(0,0,"Error: S is greater than S0 and you have selection a Less-than test",col="red",cex=1.3)
    } else if(input$sorp=="Power"){
      plot(nseq,powerxseq,type="l",lwd=2,ylab="Power level",xlab="Number of animals sampled",main=paste("A sample size of ",n," has a power of ",round(powerx,2)),ylim=c(0,1))
      points(n,powerx,pch=16,col=2,cex=2)
      abline(h=0.8,lty=2,col=3)
    }else {
      plot(nseq,powerxseq,type="l",lwd=2,ylab="Power level",xlab="Number of animals sampled",main=paste("A sample size of ",ceiling(nx)," has a power of ",round(input$powera,2)),ylim=c(0,1))
      points(nx,input$powera,pch=16,col=2,cex=2)
      abline(h=0.8,lty=2,col=3)
    }
    
    
  })
  
  output$info.pow.plotA=renderText({
    "This plot shows how changes in samples sizes would change the power based on the User Input conditions provided. "
  })
  
  output$powresbox <- renderValueBox({
    
    if(input$sorp=="Power"){
      p=input$seroprev
      p0=input$serocomp
      n=input$samplea
      alpha=as.numeric(input$alpha)
      dval=ifelse(input$compdirection=="Different from S0",2,1)
      betax=1-input$powera
      
      z=(p-p0)/sqrt(p*(1-p)/n)
      powerx=pnorm(z-qnorm(1-alpha/dval))+pnorm(-z-qnorm(1-alpha/dval))
      
      powval=ifelse((input$compdirection== "Greater than S0"&p0>p)|(input$compdirection== "Less than S0"&p0<p),"Error",paste0(round(powerx,2)))
      
      valueBox(
        value=powval,subtitle= "Power", icon = icon("earlybirds"),
        color = "orange"
      )
    }else {
      p=input$seroprev
      p0=input$serocomp
      alpha=as.numeric(input$alpha)
      dval=ifelse(input$compdirection=="Different from S0",2,1)
      betax=1-input$powera
      
      nx=p*(1-p)*((qnorm(1-alpha/dval)+qnorm(1-betax))/(p-p0))^2
      
      numval=ifelse((input$compdirection== "Greater than S0"&p0>p)|(input$compdirection== "Less than S0"&p0<p),"Error",paste0(ceiling(nx)))
      
      valueBox(
        value=numval,subtitle= "Sample size needed", icon = icon("otter"),
        color = "orange"
      )
    }
  })
  
  output$power80box <- renderInfoBox({
    p=input$seroprev
    p0=input$serocomp
    alpha=as.numeric(input$alpha)
    alphad=ifelse(input$compdirection=="Different from S0",1-alpha/2,
                  ifelse(input$compdirection=="Greater than S0",1-alpha,alpha))
    betax=1-0.8
    
    nx80=p*(1-p)*((qnorm(1-alpha/2)+qnorm(1-betax))/(p-p0))^2
    num80val=ifelse((input$compdirection== "Greater than S0"&p0>p)|(input$compdirection== "Less than S0"&p0<p),"Error",ceiling(nx80))
    
    infoBox(
      "Sample size needed for a power of 0.8", num80val, icon = icon("deezer"),
      color = "red"
    )
  })
  
  
  output$powerplotdist <- renderPlot({
    p=input$seroprev
    p0=input$serocomp
    alpha=as.numeric(input$alpha)
    alphad=ifelse(input$compdirection=="Different from S0",1-alpha/2,
                  ifelse(input$compdirection=="Greater than S0",1-alpha,alpha))
    betax=ifelse(input$sorp=="Power",0.2,1-input$powera)
    
    nx=p*(1-p)*((qnorm(alphad)+qnorm(1-betax))/(p-p0))^2
    n=ifelse(input$sorp=="Power",input$samplea,nx)
    
    stdp=sqrt((p*(1-p))/n)
    p0vals=rnorm(100000,p0,stdp)
    p0den=density(p0vals,na.rm = TRUE)
    pvals=rnorm(100000,(p),stdp)
    pden=density(pvals,na.rm=TRUE)
    
    plot(p0den,xlab="Difference in seroprevalence",main="Power depiction",xlim=c(min(p0den$x,pden$x),max(p0den$x,pden$x)))
    lines(pden,col=2)
    polygon(p0den, col = rgb(0.78, 0.89, 1, alpha = 0.6))
    polygon(pden, col = rgb(0.78, 0.04, 0.2, alpha = 0.3))
    
    if(input$compdirection=="Different from S0"){
      # Fill area for values greater or equal to 1
      value <-quantile(p0vals,1-alpha/2)
      value2 <-quantile(p0vals,alpha/2)
      
      #abline(v=value)
      polygon(c(pden$x[pden$x >= value ], value),
              c(pden$y[pden$x >= value ], 0),
              col = 2,
              border = 1)
      polygon(c(pden$x[pden$x < value2 ], value2),
              c(pden$y[pden$x < value2 ], 0),
              col = 2,
              border = 1)
      polygon(c(p0den$x[p0den$x >= value ], value),
              c(p0den$y[p0den$x >= value ], 0),
              col = "black",
              border = 1)
      
      polygon(c(p0den$x[p0den$x < value2 ], value2),
              c(p0den$y[p0den$x < value2 ], 0),
              col = "black",
              border = 1)
      
    }else if(input$compdirection=="Greater than S0"){
      value <-quantile(p0vals,1-alpha)
      #abline(v=value)
      polygon(c(pden$x[pden$x >= value ], value),
              c(pden$y[pden$x >= value ], 0),
              col = 2,
              border = 1)
      polygon(c(p0den$x[p0den$x >= value ], value),
              c(p0den$y[p0den$x >= value ], 0),
              col = "black",
              border = 1)
    }else if(input$compdirection=="Less than S0"){
      value <-quantile(p0vals,alpha)
      #abline(v=value)
      polygon(c(pden$x[pden$x < value ], value),
              c(pden$y[pden$x < value ], 0),
              col = 2,
              border = 1)
      polygon(c(p0den$x[p0den$x < value ], value),
              c(p0den$y[p0den$x < value ], 0),
              col = "black",
              border = 1)
    }
    
    
    legend("topright",legend=c("H0 distribution","Ha distribution","Type I error","Power"),
           fill = c(rgb(0.78, 0.89, 1, alpha = 0.6),rgb(0.78, 0.04, 0.2, alpha = 0.3),"black",2))
  })
  
  output$info.pow.distA=renderText({
    "This plot shows how power is actually calculated. The distributions of both the null hypothesis and alternative hypothesis are shown. Since we are interested in the difference in proportions we use a Normal distribution. The wideness or narrowness of the distribution depends on the sample size, and the difference between the null and alternative hypothesis depends on the difference of interest. The narrower the distribution (larger samples) or the greater the difference of interest the greater chance you have of telling the distributions apart (greater power).  "
  })
  
  ##############################################################################
  ###
  ### Tabs for option B
  ###
  ##############################################################################
  
  #####
  ###
  ### Quick results tab
  ###
  #####
  output$sampleboxB <- renderInfoBox({
    if(input$sorp=="Power"){
      infoBox(
        value=paste0(input$samplea1," and ",input$samplea2), title="Sample sizes", icon = icon("otter"),
        color = "black"
      )
    }else {
      infoBox(
        value=paste0(input$powera), title="Desired power", icon = icon("earlybirds"),
        color = "black"
      )
    }
    
  })
  
  output$diffboxB <- renderInfoBox({
    
    infoBox(
      value=paste0(input$seroprev1," vs ",input$seroprev2), title="Seroprevalence comparison", icon = icon("balance-scale-left"),
      color = "black"
    )
  })
  
  output$alphaboxB <- renderInfoBox({
    
    infoBox(
      value=as.numeric(paste0(input$alpha)), title="alpha value", icon = icon("font"),
      color = "black"
    )
  })
  
  output$powerboxB <- renderValueBox({
    
    if(input$sorp=="Power"){
      pA=input$seroprev1
      pB=input$seroprev2
      n1=input$samplea1
      n2=input$samplea2
      
      alpha=as.numeric(input$alpha)
      dval=1
      D0=0
      kappa=n2/n1
      z=abs(pA-pB)/sqrt(pA*(1-pA)/n2*kappa+pB*(1-pB)/n2)
      powerx=pnorm(z-qnorm(1-alpha/dval))+pnorm(-z-qnorm(1-alpha/dval))
      
      valueBox(
        value=paste0(round(powerx,2)),subtitle= "Power", icon = icon("earlybirds"),
        color = "orange"
      )
    }else {
      pA=input$seroprev1
      pB=input$seroprev2
      alpha=as.numeric(input$alpha)
      betax=ifelse(input$sorp=="Power",0.2,1-input$powera)
      dval=1
      D0=0
      kappa=1
      n12=(pA*(1-pA)/kappa+pB*(1-pB))*((qnorm(1-alpha/dval)+qnorm(1-betax))/(pA-pB))^2
      valueBox(
        value=paste0(ceiling(n12)),subtitle= "Sample size needed (per treatment/time period)", icon = icon("otter"),
        color = "orange"
      )
    }
    
  })
  
  output$samp80boxB <- renderValueBox({
    pA=input$seroprev1
    pB=input$seroprev2
    alpha=as.numeric(input$alpha)
    dval=1
    betax1=1-0.8
    D0=0
    kappa=1
    
    nx80=(pA*(1-pA)/kappa+pB*(1-pB))*((qnorm(1-alpha/dval)+qnorm(1-betax1))/(pA-pB))^2
    
    valueBox(
      subtitle = "Sample size needed for a power of 0.8", value = ceiling(nx80), icon = icon("deezer"),
      color = "red"
    )
  })
  
  output$errorboxB <- renderValueBox({
    pA=input$seroprev1
    pB=input$seroprev2
    alpha=as.numeric(input$alpha)
    betax1=ifelse(input$sorp=="Power",0.2,1-input$powera)
    dval=1
    D0=0
    kappa=1
    nx80=(pA*(1-pA)/kappa+pB*(1-pB))*((qnorm(1-alpha/dval)+qnorm(1-betax1))/(pA-pB))^2
    n1=ifelse(input$sorp=="Power",input$samplea1,nx80)
    n2=ifelse(input$sorp=="Power",input$samplea2,nx80)
    
    mediff=qnorm(1-alpha/dval)*sqrt((pA*(1-pA)/n1)+(pB*(1-pB)/n2))
    
    
    ### Margin of error
    valueBox(
      subtitle="Margin of error",value =  round(mediff,2), icon = icon("diagnoses"),
      color = "blue"
    )
  })
  
  output$propplot2B <- renderPlot({
    pA=input$seroprev1
    pB=input$seroprev2
    alpha=as.numeric(input$alpha)
    betax1=ifelse(input$sorp=="Power",0.2,1-input$powera)
    dval=1
    D0=0
    kappa=1
    nx80=(pA*(1-pA)/kappa+pB*(1-pB))*((qnorm(1-alpha/dval)+qnorm(1-betax1))/(pA-pB))^2
    n1=ifelse(input$sorp=="Power",input$samplea1,nx80)
    n2=ifelse(input$sorp=="Power",input$samplea2,nx80)
    
    popA=rbinom(10000,ceiling(n1),pA)/n1
    popB=rbinom(10000,ceiling(n2),pB)/n2
    
    boxplot(cbind(popA,popB),names=c("Trt A or pre-bait","Trt B or post-bait"),ylab="Seroprevalence",ylim=c(0,1),main="Uncertainty around seroprevalence estimates",col=viridis(2))
    
    
  })
  
  
  
  
  #####
  ###
  ### Uncertainty tab
  ###
  #####
  
  output$propplotB <- renderPlot({
    pA=input$seroprev1
    pB=input$seroprev2
    
    alpha=as.numeric(input$alpha)
    betax1=ifelse(input$sorp=="Power",0.2,1-input$powera)
    dval=1
    D0=0
    
    kappa=1
    nx80=(pA*(1-pA)/kappa+pB*(1-pB))*((qnorm(1-alpha/dval)+qnorm(1-betax1))/(pA-pB))^2
    
    n1=ifelse(input$sorp=="Power",input$samplea1,nx80)
    n2=ifelse(input$sorp=="Power",input$samplea2,nx80)
    
    popA=rbinom(10000,ceiling(n1),pA)/n1
    popB=rbinom(10000,ceiling(n2),pB)/n2
    
    boxplot(cbind(popA,popB),names=c("Trt A or pre-bait","Trt B or post-bait"),ylab="Seroprevalence",ylim=c(0,1),main="Uncertainty around seroprevalence estimates",col=viridis(2))
    
    
  })
  
  output$moeboxB <- renderValueBox({
    pA=input$seroprev1
    pB=input$seroprev2
    alpha=as.numeric(input$alpha)
    betax1=ifelse(input$sorp=="Power",0.2,1-input$powera)
    dval=1
    D0=0
    kappa=1
    nx80=(pA*(1-pA)/kappa+pB*(1-pB))*((qnorm(1-alpha/dval)+qnorm(1-betax1))/(pA-pB))^2
    n1=ifelse(input$sorp=="Power",input$samplea1,nx80)
    n2=ifelse(input$sorp=="Power",input$samplea2,nx80)
    
    mediff=qnorm(1-alpha/dval)*sqrt((pA*(1-pA)/n1)+(pB*(1-pB)/n2))
    
    valueBox(
      subtitle="Margin of error",value =  round(mediff,2), icon = icon("diagnoses"),
      color = "blue"
    )
  })
  
  
  
  
  #####
  ###
  ### Power tab
  ###
  #####
  output$powerplotB <- renderPlot({
    
    pA=input$seroprev1
    pB=input$seroprev2
    
    alpha=as.numeric(input$alpha)
    dval=1
    D0=0
    kappa=1
    nx80=(pA*(1-pA)/kappa+pB*(1-pB))*((qnorm(1-alpha/dval)+qnorm(1-0.2))/(pA-pB))^2
    n1=ifelse(input$sorp=="Power",input$samplea1,nx80)
    n2=ifelse(input$sorp=="Power",input$samplea2,nx80)
    
    N1=N2=max(200,round_any(nx80,100,ceiling))
    n1seq=matrix(seq(1,N1,1),N1,N2,byrow=FALSE)
    n2seq=matrix(seq(1,N2,1),N1,N2,byrow=TRUE)
    kappa=n2seq/n1seq
    zseq=abs(pA-pB)/sqrt(pA*(1-pA)/n2seq*kappa+pB*(1-pB)/n2seq)
    powerxseq=pnorm(zseq-qnorm(1-alpha/dval))+pnorm(-zseq-qnorm(1-alpha/dval))
    
    z=abs(pA-pB)/sqrt(pA*(1-pA)/n1+pB*(1-pB)/n2)
    powerx=pnorm(z-qnorm(1-alpha/dval))+pnorm(-z-qnorm(1-alpha/dval))
    
    if(input$sorp=="Power"){
      image.plot(seq(1,N1,1),seq(1,N2,1),powerxseq,xlab="Sample size population A",ylab="Sample size population B",col =viridis(10),main=paste("Power when n1 = ",n1," and n2 = ",n2," is ",round(powerx,2)))
      points(n1,n2,pch=16,col="red",cex=2)
    }else{
      image.plot(seq(1,N1,1),seq(1,N2,1),powerxseq,xlab="Sample size population A",ylab="Sample size population B",col =viridis(10),main=paste("To get a power of 0.8 with equal sample size you need n = ",ceiling(nx80)))
      points(n1,n2,pch=16,col="red",cex=2)
    }
    
    
  })
  
  output$powresboxB <- renderValueBox({
    if(input$sorp=="Power"){
      pA=input$seroprev1
      pB=input$seroprev2
      n1=input$samplea1
      n2=input$samplea2
      
      alpha=as.numeric(input$alpha)
      dval=1
      D0=0
      kappa=n2/n1
      z=abs(pA-pB)/sqrt(pA*(1-pA)/n2*kappa+pB*(1-pB)/n2)
      powerx=pnorm(z-qnorm(1-alpha/dval))+pnorm(-z-qnorm(1-alpha/dval))
      
      valueBox(
        value=paste0(round(powerx,2)),subtitle= "Power", icon = icon("earlybirds"),
        color = "orange"
      )
    }else {
      pA=input$seroprev1
      pB=input$seroprev2
      alpha=as.numeric(input$alpha)
      betax=ifelse(input$sorp=="Power",0.2,1-input$powera)
      dval=1
      D0=0
      kappa=1
      n12=(pA*(1-pA)/kappa+pB*(1-pB))*((qnorm(1-alpha/dval)+qnorm(1-betax))/(pA-pB))^2
      valueBox(
        value=paste0(ceiling(n12)),subtitle= "Sample size needed (per treatment/time period)", icon = icon("otter"),
        color = "orange"
      )
    }
  })
  
  output$power80boxB <- renderInfoBox({
    pA=input$seroprev1
    pB=input$seroprev2
    alpha=as.numeric(input$alpha)
    dval=1
    D0=0
    kappa=1
    nx80=(pA*(1-pA)/kappa+pB*(1-pB))*((qnorm(1-alpha/dval)+qnorm(1-0.2))/(pA-pB))^2
    
    valueBox(
      subtitle = "Sample size needed for a power of 0.8", value = ceiling(nx80), icon = icon("deezer"),
      color = "red"
    )
  })
  
  
  output$powerplotdistB <- renderPlot({
    pA=input$seroprev1
    pB=input$seroprev2
    alpha=as.numeric(input$alpha)
    betax=ifelse(input$sorp=="Power",0.2,1-input$powera)
    
    kappa=1
    nx=(pA*(1-pA)/kappa+pB*(1-pB))*((qnorm(1-alpha)+qnorm(1-0.2))/(pA-pB))^2
    nA=ifelse(input$sorp=="Power",input$samplea1,nx)
    nB=ifelse(input$sorp=="Power",input$samplea2,nx)
    
    ### Power depiction
    p0vals=rnorm(100000,0,(sqrt((pA*(1-pA)/nA)+(pB*(1-pB)/nB))))
    p0den=density(p0vals)
    pvals=rnorm(100000,abs(pA-pB),(sqrt((pA*(1-pA)/nA)+(pB*(1-pB)/nB))))
    pden=density(pvals)
    
    plot(p0den,xlab="Difference in seroprevalence",main="Power depiction",xlim=c(min(p0den$x,pden$x),max(p0den$x,pden$x)))
    lines(pden,col=2)
    polygon(p0den, col = rgb(0.78, 0.89, 1, alpha = 0.6))
    polygon(pden, col = rgb(0.78, 0.04, 0.2, alpha = 0.3))
    # Fill area for values greater or equal to 1
    value <-quantile(p0vals,1-alpha)
    #abline(v=value)
    polygon(c(pden$x[pden$x >= value ], value),
            c(pden$y[pden$x >= value ], 0),
            col = 2,
            border = 1)
    polygon(c(p0den$x[p0den$x >= value ], value),
            c(p0den$y[p0den$x >= value ], 0),
            col = "black",
            border = 1)
    
    legend("topright",legend=c("H0 distribution","Ha distribution","Type I error","Power"),
           fill = c(rgb(0.78, 0.89, 1, alpha = 0.6),rgb(0.78, 0.04, 0.2, alpha = 0.3),"black",2))
    
  })
  
  
  ##############################################################################
  ###
  ### Tabs for option C
  ###
  ##############################################################################
  
  
  #####
  ###
  ### Quick results tab
  ###
  #####
  output$sampleboxD <- renderInfoBox({
    if(input$sorp=="Power"){
      infoBox(
        value=paste0("A: (",input$sampleaA1,", ",input$sampleaA2,"); B: (",input$sampleaB1,", ",input$sampleaB2,")"), title="Sample sizes", icon = icon("otter"),
        color = "black"
      )
    }else {
      infoBox(
        value=paste0(input$powera), title="Desired power", icon = icon("earlybirds"),
        color = "black"
      )
    }
    
  })
  
  output$diffboxD <- renderInfoBox({
    
    infoBox(
      value=paste0("A: (",input$seroprevA1,", ",input$seroprevA2,") v.s. B: (",input$seroprevB1,", ",input$seroprevB2,")"), title="Seroprevalence comparison", icon = icon("balance-scale-left"),
      color = "black"
    )
  })
  
  output$alphaboxD <- renderInfoBox({
    
    infoBox(
      value=as.numeric(paste0(input$alpha)), title="alpha value", icon = icon("font"),
      color = "black"
    )
  })
  
  output$powerboxD <- renderValueBox({
    
    if(input$sorp=="Power"){
      pA1=input$seroprevA1
      pA2=input$seroprevA2
      pB1=input$seroprevB1
      pB2=input$seroprevB2
      nA1=input$sampleaA1
      k1=input$sampleaA2/input$sampleaA1
      k2=input$sampleaB1/input$sampleaA1
      k3=input$sampleaB2/input$sampleaA1
      alpha=as.numeric(input$alpha)
      
      z=(abs(max(pA2-pA1,0)-max(pB2-pB1))/sqrt((pA1*(1-pA1))/nA1+(pA2*(1-pA2))/(k1*nA1)+(pB1*(1-pB1))/(k2*nA1)+(pB2*(1-pB2))/(k3*nA1)))
      powerx=pnorm(z-qnorm(1-alpha))+pnorm(-z-qnorm(1-alpha))
      powerx
      
      valueBox(
        value=paste0(round(powerx,2)),subtitle= "Power", icon = icon("earlybirds"),
        color = "orange"
      )
    }else {
      pA1=input$seroprevA1
      pA2=input$seroprevA2
      pB1=input$seroprevB1
      pB2=input$seroprevB2
      alpha=as.numeric(input$alpha)
      betax=ifelse(input$sorp=="Power",0.2,1-input$powera)
      dval=1
      D0=0
      k1=1
      k2=1
      k3=1
      nA1=((pA1*(1-pA1)*k1*k2*k3+pA2*(1-pA2)*k2*k3+pB1*(1-pB1)*k1*k3+pB2*(1-pB2)*k1*k2)/(k1*k2*k3))*((qnorm(1-alpha)+qnorm(1-betax))/abs(max(pA2-pA1,0)-max(pB2-pB1)))^2
      
      valueBox(
        value=paste0(ceiling(nA1)),subtitle= "Sample size needed (per treatment/time period)", icon = icon("otter"),
        color = "orange"
      )
    }
    
  })
  
  output$power80boxD <- renderValueBox({
    pA1=input$seroprevA1
    pA2=input$seroprevA2
    pB1=input$seroprevB1
    pB2=input$seroprevB2
    alpha=as.numeric(input$alpha)
    betax=0.2
    dval=1
    D0=0
    k1=1
    k2=1
    k3=1
    nx80=((pA1*(1-pA1)*k1*k2*k3+pA2*(1-pA2)*k2*k3+pB1*(1-pB1)*k1*k3+pB2*(1-pB2)*k1*k2)/(k1*k2*k3))*((qnorm(1-alpha)+qnorm(1-betax))/abs(max(pA2-pA1,0)-max(pB2-pB1)))^2
    
    valueBox(
      subtitle = "Sample size needed for a power of 0.8", value = ceiling(nx80), icon = icon("deezer"),
      color = "red"
    )
  })
  
  output$errorboxD <- renderValueBox({
    pA1=input$seroprevA1
    pA2=input$seroprevA2
    pB1=input$seroprevB1
    pB2=input$seroprevB2
    alpha=as.numeric(input$alpha)
    
    if(input$sorp=="Power"){
      nA1=input$sampleaA1
      k1=input$sampleaA2/input$sampleaA1
      k2=input$sampleaB1/input$sampleaA1
      k3=input$sampleaB2/input$sampleaA1
    }else if(input$sorp=="Sample size"){
      betax=1-input$powera
      k1=1
      k2=1
      k3=1
      nA1=((pA1*(1-pA1)*k1*k2*k3+pA2*(1-pA2)*k2*k3+pB1*(1-pB1)*k1*k3+pB2*(1-pB2)*k1*k2)/(k1*k2*k3))*((qnorm(1-alpha)+qnorm(1-betax))/abs(max(pA2-pA1,0)-max(pB2-pB1)))^2
    }
    
    
    mediff=qnorm(1-alpha)*sqrt((pA1*(1-pA1))/nA1+(pA2*(1-pA2))/(k1*nA1)+(pB1*(1-pB1))/(k2*nA1)+(pB2*(1-pB2))/(k3*nA1))
    
    
    ### Margin of error
    valueBox(
      subtitle="Margin of error",value =  round(mediff,2), icon = icon("diagnoses"),
      color = "blue"
    )
  })
  
  output$propplot2D <- renderPlot({
    pA1=input$seroprevA1
    pA2=input$seroprevA2
    pB1=input$seroprevB1
    pB2=input$seroprevB2
    if(input$sorp=="Power"){
      nA1=input$sampleaA1
      nA2=input$sampleaA2
      nB1=input$sampleaB1
      nB2=input$sampleaB2
      alpha=as.numeric(input$alpha)
    }else if(input$sorp=="Sample size"){
      alpha=as.numeric(input$alpha)
      betax=0.2
      dval=1
      D0=0
      k1=1
      k2=1
      k3=1
      nA1=((pA1*(1-pA1)*k1*k2*k3+pA2*(1-pA2)*k2*k3+pB1*(1-pB1)*k1*k3+pB2*(1-pB2)*k1*k2)/(k1*k2*k3))*((qnorm(1-alpha)+qnorm(1-betax))/abs(max(pA2-pA1,0)-max(pB2-pB1)))^2
      nA2=nA1
      nB1=nA1
      nB2=nA1
    }
    
    popA1=rbinom(10000,ceiling(nA1),pA1)/nA1
    popA2=rbinom(10000,ceiling(nA2),pA2)/nA2
    popB1=rbinom(10000,ceiling(nB1),pB1)/nB1
    popB2=rbinom(10000,ceiling(nB2),pB2)/nB2
    
    boxplot(cbind(popA1,popA2,popB1,popB2),ylab="Seroprevalence",col=rep(viridis(2),each=2),
            ylim=c(0,1),main="Uncertainty around seroprevalence estimates",names=c("Pre-bait TrtA","Post-bait TrtA","Pre-bait TrtB","Post-bait TrtB"))
    
  })
  
  
  
  
  #####
  ###
  ### Uncertainty tab
  ###
  #####
  
  output$propplotD <- renderPlot({
    pA1=input$seroprevA1
    pA2=input$seroprevA2
    pB1=input$seroprevB1
    pB2=input$seroprevB2
    if(input$sorp=="Power"){
      nA1=input$sampleaA1
      nA2=input$sampleaA2
      nB1=input$sampleaB1
      nB2=input$sampleaB2
      alpha=as.numeric(input$alpha)
    }else if(input$sorp=="Sample size"){
      alpha=as.numeric(input$alpha)
      betax=0.2
      dval=1
      D0=0
      k1=1
      k2=1
      k3=1
      nA1=((pA1*(1-pA1)*k1*k2*k3+pA2*(1-pA2)*k2*k3+pB1*(1-pB1)*k1*k3+pB2*(1-pB2)*k1*k2)/(k1*k2*k3))*((qnorm(1-alpha)+qnorm(1-betax))/abs(max(pA2-pA1,0)-max(pB2-pB1)))^2
      nA2=nA1
      nB1=nA1
      nB2=nA1
    }
    
    popA1=rbinom(10000,ceiling(nA1),pA1)/nA1
    popA2=rbinom(10000,ceiling(nA2),pA2)/nA2
    popB1=rbinom(10000,ceiling(nB1),pB1)/nB1
    popB2=rbinom(10000,ceiling(nB2),pB2)/nB2
    
    boxplot(cbind(popA1,popA2,popB1,popB2),ylab="Seroprevalence",col=rep(viridis(2),each=2),
            ylim=c(0,1),main="Uncertainty around seroprevalence estimates",names=c("Pre-bait TrtA","Post-bait TrtA","Pre-bait TrtB","Post-bait TrtB"))
    
  })
  
  output$Difftrtplot <- renderPlot({
    pA1=input$seroprevA1
    pA2=input$seroprevA2
    pB1=input$seroprevB1
    pB2=input$seroprevB2
    if(input$sorp=="Power"){
      nA1=input$sampleaA1
      nA2=input$sampleaA2
      nB1=input$sampleaB1
      nB2=input$sampleaB2
      alpha=as.numeric(input$alpha)
    }else if(input$sorp=="Sample size"){
      alpha=as.numeric(input$alpha)
      betax=0.2
      dval=1
      D0=0
      k1=1
      k2=1
      k3=1
      nA1=((pA1*(1-pA1)*k1*k2*k3+pA2*(1-pA2)*k2*k3+pB1*(1-pB1)*k1*k3+pB2*(1-pB2)*k1*k2)/(k1*k2*k3))*((qnorm(1-alpha)+qnorm(1-betax))/abs(max(pA2-pA1,0)-max(pB2-pB1)))^2
      nA2=nA1
      nB1=nA1
      nB2=nA1
    }
    
    popA1=rbinom(10000,ceiling(nA1),pA1)/nA1
    popA2=rbinom(10000,ceiling(nA2),pA2)/nA2
    popB1=rbinom(10000,ceiling(nB1),pB1)/nB1
    popB2=rbinom(10000,ceiling(nB2),pB2)/nB2
    
    poplowA=rbinom(10000,ceiling(nA1),0.5)/nA1-rbinom(10000,ceiling(nA2),0.5)/nA2
    poplowB=rbinom(10000,ceiling(nB1),0.5)/nB1-rbinom(10000,ceiling(nB2),0.5)/nB2
    
    popAd=if(pA1>pA2){poplowA}else{popA2-popA1}
    popBd=if(pB1>pB2){poplowB}else{popB2-popB1}
    
    boxplot(cbind(popAd,popBd),ylab="Change in seroprevalence (Pre to Post bait)",col=viridis((2)),names=c("Trt A","Trt B"),
            main="Difference we are testing for between treatments",ylim=c(min(min(popAd,popBd),-0.5),1))
    
  })
  
  output$moeboxD <- renderValueBox({
    pA1=input$seroprevA1
    pA2=input$seroprevA2
    pB1=input$seroprevB1
    pB2=input$seroprevB2
    alpha=as.numeric(input$alpha)
    
    if(input$sorp=="Power"){
      nA1=input$sampleaA1
      k1=input$sampleaA2/input$sampleaA1
      k2=input$sampleaB1/input$sampleaA1
      k3=input$sampleaB2/input$sampleaA1
    }else if(input$sorp=="Sample size"){
      betax=1-input$powera
      k1=1
      k2=1
      k3=1
      nA1=((pA1*(1-pA1)*k1*k2*k3+pA2*(1-pA2)*k2*k3+pB1*(1-pB1)*k1*k3+pB2*(1-pB2)*k1*k2)/(k1*k2*k3))*((qnorm(1-alpha)+qnorm(1-betax))/abs(max(pA2-pA1,0)-max(pB2-pB1)))^2
    }
    
    mediff=me1=qnorm(1-alpha)*sqrt((pA1*(1-pA1))/nA1+(pA2*(1-pA2))/(k1*nA1)+(pB1*(1-pB1))/(k2*nA1)+(pB2*(1-pB2))/(k3*nA1))
    
    valueBox(
      subtitle="Margin of error",value =  round(mediff,2), icon = icon("diagnoses"),
      color = "blue"
    )
  })
  
  
  #####
  ###
  ### Power tab
  ###
  #####
  output$powerplotCA <- renderPlot({
    if(input$sorp=="Power"){
      pA1=input$seroprevA1
      pA2=input$seroprevA2
      pB1=input$seroprevB1
      pB2=input$seroprevB2
      nA1=input$sampleaA1
      nA2=input$sampleaA2
      nB1=input$sampleaB1
      nB2=input$sampleaB2
      k1=1
      k2=1
      k3=1
      alpha=as.numeric(input$alpha)
      
      z=(abs(max(pA2-pA1,0)-max(pB2-pB1))/sqrt((pA1*(1-pA1))/nA1+(pA2*(1-pA2))/(nA2)+(pB1*(1-pB1))/(nB1)+(pB2*(1-pB2))/(nB2)))
      powerx=pnorm(z-qnorm(1-alpha))+pnorm(-z-qnorm(1-alpha))
      
      nAB=((pA1*(1-pA1)*k1*k2*k3+pA2*(1-pA2)*k2*k3+pB1*(1-pB1)*k1*k3+pB2*(1-pB2)*k1*k2)/(k1*k2*k3))*((qnorm(1-alpha)+qnorm(1-0.2))/abs(max(pA2-pA1,0)-max(pB2-pB1)))^2
      
      
      nmax=max(200,round_any(max(nA1,nA2,nB1,nB2,nAB),100,ceiling))
      n1seq=matrix(seq(1,nmax,1),nmax,nmax,byrow=FALSE)
      n2seq=matrix(seq(1,nmax,1),nmax,nmax,byrow=TRUE)
      zAseq=(abs(max(pA2-pA1,0)-max(pB2-pB1))/sqrt((pA1*(1-pA1))/n1seq+(pA2*(1-pA2))/(n2seq)+(pB1*(1-pB1))/(nB1)+(pB2*(1-pB2))/(nB2)))
      powerxAseq=pnorm(zAseq-qnorm(1-alpha))+pnorm(-zAseq-qnorm(1-alpha))
      
      
      ### two figs 
      image.plot(seq(1,nmax,1),seq(1,nmax,1),powerxAseq,xlab="Sample size pre-bait Trt A",ylab="Sample size post-bait Trt A",col =viridis(10),main="Power for changes in sampling for Treatment A",zlim=c(0,1))
      points(nA1,nA2,pch=16,col="red",cex=2)
      
    }else if(input$sorp=="Sample size"){
      pA1=input$seroprevA1
      pA2=input$seroprevA2
      pB1=input$seroprevB1
      pB2=input$seroprevB2
      alpha=as.numeric(input$alpha)
      betax=1-input$powera
      dval=1
      D0=0
      k1=1
      k2=1
      k3=1
      nA1=((pA1*(1-pA1)*k1*k2*k3+pA2*(1-pA2)*k2*k3+pB1*(1-pB1)*k1*k3+pB2*(1-pB2)*k1*k2)/(k1*k2*k3))*((qnorm(1-alpha)+qnorm(1-betax))/abs(max(pA2-pA1,0)-max(pB2-pB1)))^2
      
      z=(abs(max(pA2-pA1,0)-max(pB2-pB1))/sqrt((pA1*(1-pA1))/nA1+(pA2*(1-pA2))/(k1*nA1)+(pB1*(1-pB1))/(k2*nA1)+(pB2*(1-pB2))/(k3*nA1)))
      powerx=pnorm(z-qnorm(1-alpha))+pnorm(-z-qnorm(1-alpha))
      
      nmax=max(200,round_any(nA1,100,ceiling))
      nseq=seq(1,nmax,1)
      zseq=(abs(max(pA2-pA1,0)-max(pB2-pB1))/sqrt((pA1*(1-pA1))/nseq+(pA2*(1-pA2))/(k1*nseq)+(pB1*(1-pB1))/(k2*nseq)+(pB2*(1-pB2))/(k3*nseq)))
      powerxseq=pnorm(zseq-qnorm(1-alpha))+pnorm(-zseq-qnorm(1-alpha))
      powerxseq
      
      plot(nseq,powerxseq,type="l",xlab="Sample size per site per period",ylab="Power")
      abline(h=0.8,lty=2,col=3)
      points(nA1,powerx,pch=16,col="red",cex=2)
    }
  })
  
  output$powerplotCB <- renderPlot({
    if(input$sorp=="Power"){
      pA1=input$seroprevA1
      pA2=input$seroprevA2
      pB1=input$seroprevB1
      pB2=input$seroprevB2
      nA1=input$sampleaA1
      nA2=input$sampleaA2
      nB1=input$sampleaB1
      nB2=input$sampleaB2
      D0=0
      k1=1
      k2=1
      k3=1
      alpha=as.numeric(input$alpha)
      
      z=(abs(max(pA2-pA1,0)-max(pB2-pB1))/sqrt((pA1*(1-pA1))/nA1+(pA2*(1-pA2))/(nA2)+(pB1*(1-pB1))/(nB1)+(pB2*(1-pB2))/(nB2)))
      powerx=pnorm(z-qnorm(1-alpha))+pnorm(-z-qnorm(1-alpha))
      
      nAB=((pA1*(1-pA1)*k1*k2*k3+pA2*(1-pA2)*k2*k3+pB1*(1-pB1)*k1*k3+pB2*(1-pB2)*k1*k2)/(k1*k2*k3))*((qnorm(1-alpha)+qnorm(1-0.2))/abs(max(pA2-pA1,0)-max(pB2-pB1)))^2
      
      nmax=max(200,round_any(max(nA1,nA2,nB1,nB2,nAB),100,ceiling))
      
      n1seq=matrix(seq(1,nmax,1),nmax,nmax,byrow=FALSE)
      n2seq=matrix(seq(1,nmax,1),nmax,nmax,byrow=TRUE)
      zAseq=(abs(max(pA2-pA1,0)-max(pB2-pB1))/sqrt((pA1*(1-pA1))/n1seq+(pA2*(1-pA2))/(n2seq)+(pB1*(1-pB1))/(nB1)+(pB2*(1-pB2))/(nB2)))
      powerxAseq=pnorm(zAseq-qnorm(1-alpha))+pnorm(-zAseq-qnorm(1-alpha))
      
      zBseq=(abs(max(pA2-pA1,0)-max(pB2-pB1))/sqrt((pA1*(1-pA1))/nA1+(pA2*(1-pA2))/(nA2)+(pB1*(1-pB1))/(n1seq)+(pB2*(1-pB2))/(n2seq)))
      powerxBseq=pnorm(zBseq-qnorm(1-alpha))+pnorm(-zBseq-qnorm(1-alpha))
      
      ### two figs 
      image.plot(seq(1,nmax,1),seq(1,nmax,1),powerxBseq,xlab="Sample size pre-bait Trt B",ylab="Sample size post-bait Trt B",col =viridis(10),main="Power for changes in sampling for Treatment B",zlim=c(0,1))
      points(nB1,nB2,pch=16,col="red",cex=2)
    }else{
      
    }
  })
  
  output$powresboxC <- renderValueBox({
    if(input$sorp=="Power"){
      pA1=input$seroprevA1
      pA2=input$seroprevA2
      pB1=input$seroprevB1
      pB2=input$seroprevB2
      nA1=input$sampleaA1
      k1=input$sampleaA2/input$sampleaA1
      k2=input$sampleaB1/input$sampleaA1
      k3=input$sampleaB2/input$sampleaA1
      alpha=as.numeric(input$alpha)
      
      z=(abs(max(pA2-pA1,0)-max(pB2-pB1))/sqrt((pA1*(1-pA1))/nA1+(pA2*(1-pA2))/(k1*nA1)+(pB1*(1-pB1))/(k2*nA1)+(pB2*(1-pB2))/(k3*nA1)))
      powerx=pnorm(z-qnorm(1-alpha))+pnorm(-z-qnorm(1-alpha))
      powerx
      
      valueBox(
        value=paste0(round(powerx,2)),subtitle= "Power", icon = icon("earlybirds"),
        color = "orange"
      )
    }else {
      pA1=input$seroprevA1
      pA2=input$seroprevA2
      pB1=input$seroprevB1
      pB2=input$seroprevB2
      alpha=as.numeric(input$alpha)
      betax=ifelse(input$sorp=="Power",0.2,1-input$powera)
      dval=1
      D0=0
      k1=1
      k2=1
      k3=1
      nA1=((pA1*(1-pA1)*k1*k2*k3+pA2*(1-pA2)*k2*k3+pB1*(1-pB1)*k1*k3+pB2*(1-pB2)*k1*k2)/(k1*k2*k3))*((qnorm(1-alpha)+qnorm(1-betax))/abs(max(pA2-pA1,0)-max(pB2-pB1)))^2
      
      valueBox(
        value=paste0(ceiling(nA1)),subtitle= "Sample size needed (per treatment/time period)", icon = icon("otter"),
        color = "orange"
      )
    }
  })
  
  output$power80boxC <- renderInfoBox({
    pA1=input$seroprevA1
    pA2=input$seroprevA2
    pB1=input$seroprevB1
    pB2=input$seroprevB2
    alpha=as.numeric(input$alpha)
    betax=0.2
    dval=1
    D0=0
    k1=1
    k2=1
    k3=1
    nx80=((pA1*(1-pA1)*k1*k2*k3+pA2*(1-pA2)*k2*k3+pB1*(1-pB1)*k1*k3+pB2*(1-pB2)*k1*k2)/(k1*k2*k3))*((qnorm(1-alpha)+qnorm(1-betax))/abs(max(pA2-pA1,0)-max(pB2-pB1)))^2
    
    valueBox(
      subtitle = "Sample size needed for a power of 0.8", value = ceiling(nx80), icon = icon("deezer"),
      color = "red"
    )
  })
  
  
  output$powerplotdistC <- renderPlot({
    pA1=input$seroprevA1
    pA2=input$seroprevA2
    pB1=input$seroprevB1
    pB2=input$seroprevB2
    alpha=as.numeric(input$alpha)
    betax=0.2
    dval=1
    D0=0
    k1=ifelse(input$sorp=="Power",input$sampleaA2/input$sampleaA1,1)
    k2=ifelse(input$sorp=="Power",input$sampleaB1/input$sampleaA1,1)
    k3=ifelse(input$sorp=="Power",input$sampleaB2/input$sampleaA1,1)
    nA1=ifelse(input$sorp=="Power",input$sampleaA1,((pA1*(1-pA1)*k1*k2*k3+pA2*(1-pA2)*k2*k3+pB1*(1-pB1)*k1*k3+pB2*(1-pB2)*k1*k2)/(k1*k2*k3))*((qnorm(1-alpha)+qnorm(1-betax))/abs(max(pA2-pA1,0)-max(pB2-pB1)))^2)
    
    
    ### Power depiction
    p0vals=rnorm(100000,0,sqrt((pA1*(1-pA1))/nA1+(pA2*(1-pA2))/(k1*nA1)+(pB1*(1-pB1))/(k2*nA1)+(pB2*(1-pB2))/(k3*nA1)))
    p0den=density(p0vals)
    pvals=rnorm(100000,abs(max(pA2-pA1,0)-max(pB2-pB1,0)),sqrt((pA1*(1-pA1))/nA1+(pA2*(1-pA2))/(k1*nA1)+(pB1*(1-pB1))/(k2*nA1)+(pB2*(1-pB2))/(k3*nA1)))
    pden=density(pvals)
    
    plot(p0den,xlab="Difference in seroprevalence",main="Power depiction",xlim=c(min(p0den$x,pden$x),max(p0den$x,pden$x)))
    lines(pden,col=2)
    polygon(p0den, col = rgb(0.78, 0.89, 1, alpha = 0.6))
    polygon(pden, col = rgb(0.78, 0.04, 0.2, alpha = 0.3))
    # Fill area for values greater or equal to 1
    value <-quantile(p0vals,1-alpha)
    #abline(v=value)
    polygon(c(pden$x[pden$x >= value ], value),
            c(pden$y[pden$x >= value ], 0),
            col = 2,
            border = 1)
    polygon(c(p0den$x[p0den$x >= value ], value),
            c(p0den$y[p0den$x >= value ], 0),
            col = "black",
            border = 1)
    
    legend("topright",legend=c("H0 distribution","Ha distribution","Type I error","Power"),
           fill = c(rgb(0.78, 0.89, 1, alpha = 0.6),rgb(0.78, 0.04, 0.2, alpha = 0.3),"black",2))
    
  })
  
  
  session$onSessionEnded(stopApp)
  
  
}

# Run the application 
shinyApp(ui = ui, server = server,options=list(height=1080))
