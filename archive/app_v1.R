require(shiny)
require(dplyr)
require(tidyr)
require(shinythemes)
require(shinycssloaders)
require(DT)
require(stringr)
require(ldbounds)
require(shinyjs)
require(bslib)
require(shinydashboard)


crossprob<-function(theta,tadapt,z,futimes,fubounds){ 
  # computes the conditional crossing probabilities by 
  # futimes, given Z(tadapt)=z
  # theta is drift parameter
  # tadapt is time after which we adapt
  # z=Z(tadapt)
  # futimes is vector of future times
  # fubounds is vector of future boundaries 
  numfu<-length(futimes) # number of future times    
  dvec<-(fubounds*sqrt(futimes)-z*sqrt(tadapt) -theta*(futimes-tadapt))/sqrt(futimes-tadapt)
  uvec<-(futimes-tadapt)/(1-tadapt)
  crossingprob<-ldPower(za=rep(-8,numfu),zb=dvec,t=uvec,drift=0)$cum.exit  # conditional probability 
  # of crossing by times futimes, given Z(tadapt)=z
  return(crossingprob)
}

# a function using Simpson’s rule to perform integration written by Mike P.
simpson<-function(f,dx){
  # Approximates integrals using Simpson's rule.
  # f is a vector of function values at 2m+1 equally-spaced x values (comprising 2m intervals of length dx)
  m<-(length(f)-1)/2
  evens<-2*(1:m); odds<-2*(1:m)+1; last<-2*m+1
  int<-(4*sum(f[evens])+2*sum(f[odds])+f[1]-f[last])*(dx/3)
  return(int)
}


iuseType <- c("O'Brien Fleming","Pocock","Power family", "Hwang-Shih-DeCani family")


ui <- fluidPage(
  
  theme = bs_theme(version = 4, bootswatch = "united"), # yeti, minty,etc.
  
  titlePanel("Explore boundary and conditional power after adding interim analysis times ",windowTitle = "Add interim looks"),
  hr(),
  
  navlistPanel(id="test",
               widths = c(1.5,10),
               ##############################
               ### 
               ### introduction
               ###
               ##############################
               tabPanel("Introduction",
                        fluidRow(
                          h4("This R Shiny app shows boundary and conditional power plots and allows the user to select the added interm analysis time and power parameter value phi."),
                          br(),
                          h4("The following figure (Figure 1 from the paper) demonstrates the procedure for altering the timing of interim analysis using the conditional error principle."),
                          br(),
                          br(),
                          column(12,
                                 h3("7 - step procedure"),
                                 imageOutput("figure1"))
                        )
               ),
               ##############################
               ### 
               ### Parameter setup 
               ###
               ##############################
               
               tabPanel("Boundary & Power",
                        wellPanel(
                          
                          shinyjs::useShinyjs(),
                          id = "reset-input",
                          
                          h4(strong("Parameter setup"), style="color:	dodgerblue"),
                          fluidRow(
                            column(3,textInput(inputId = "planned_times",
                                                  label = "Enter the protocol-specified schedule:",
                                                  value="0.5, 1"),
                                     textInput(inputId = "actual_toNow",
                                                  label = "Enter the actual looks up to now (time of adaptation):",
                                                  value= "0.5"),
                                     textInput(inputId = "newfutimes",
                                                  label = "Enter the new schedule of future look times as a vector (e.g. 0.75, 0.85,0.9, 1); must be after adapt time",
                                                  value="0.75, 1")),
                            column(3,numericInput(inputId = "z",
                                                  label = "Enter the observed z-score at time of adaptation:",
                                                  value=2.5, min = 0, max=1),
                                     numericInput(inputId = "alpha",
                                                  label = "Enter 1-sided type 1 error rate:",
                                                  value=0.025, min = 0, max=1)),
                            column(3, selectInput("iuse", "Select the type of alpha spending function to generate original boundaries:", iuseType),
                                      conditionalPanel(
                                         condition = "input.iuse =='Power family'",
                                         numericInput(inputId ="phi_orig_power",
                                                   label = "This spending function requires a phi value. Enter a positive phi value here:",
                                                   value = 1)),
                                      conditionalPanel(
                                         condition = "input.iuse == 'Hwang-Shih-DeCani family'",
                                         numericInput(inputId ="phi_orig_Hwang",
                                                      label = "This spending function requires a phi value. Enter a non-zero phi value here:",
                                                      value = 2))
                                      ),
                            column(3,radioButtons(inputId ="HypoType", 
                                                  label = "Choose one to be used for conditional power calculation:", 
                                                  c("Under the current trend" = "current", 
                                                    "Under the original hypothesized treatment effect"="original")
                            ),
                            conditionalPanel(
                              condition = "input.HypoType =='original'",
                              numericInput(inputId ="Power_orig",
                                           label = "Enter power incorporating the original monitoring schedule:",
                                           value = 0.9))
                            ),
                            
                            column(2, offset=0, 
                                   actionButton("reset_input", "Reset to initial inputs.",
                                                icon = icon("refresh"), style="color: #fff; background-color: #778899	; border-color: #2e6da4")
                            ),
                            column(2, offset = 0, 
                                   actionButton("clickthis","Click to calculate!",
                                                icon = icon("area-chart"),style="color: #fff; background-color: #778899	; border-color: #2e6da4")
                                   )
                            
                          )
                        ),
                        br(),
       
                        ##############################
                        ### 
                        ### output plots 
                        ###
                        ##############################
                        wellPanel(style = "color: #fff; background-color: #778899; border-color: #2e6da4",
                          fluidRow(
                          column(4,
                                 plotOutput("figure2")%>%withSpinner(),
                                 textOutput("figure2Note"),
                                 tags$head(tags$style("#figure2Note{color: White;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                                 ))
                                 ),
                          column(4,
                                 plotOutput("figure3")%>%withSpinner(),
                                 textOutput("figure3Note"),
                                 tags$head(tags$style("#figure3Note{color: White;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                                 ))
                          )
                          ,
                          column(4,
                                 plotOutput("figure4")%>%withSpinner(),
                                 textOutput("figure4Note"),
                                 tags$head(tags$style("#figure4Note{color: White;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                                 )))
                          )),
                        br(),
                        
                        wellPanel(
                          h4(strong("Boundary at the power parameter phi"), style="color:	dodgerblue"),
                          fluidRow(
                            column(10,numericInput(inputId = "user_phi",
                                               label = "Enter user-defined phi, then press 'Go!':",
                                               value=1.5, min=0.01, max=3)),
                            column(2, offset = 0, 
                                   actionButton("clickthis2","Go!",
                                                icon = icon("calculator"),
                                                style="color: #fff; background-color: #778899	; border-color: #2e6da4"))
                          )
                        ),
                        br(),

                        
                        wellPanel(style = "color: #fff; background-color: #778899; border-color: #2e6da4",
                          fluidRow(
                            column(4,
                                   textOutput("BoundaryResult"),
                                   tags$head(tags$style("#BoundaryResult{color: White;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                                   ))),
                            column(4,
                                   textOutput("CPResult"),
                                   tags$head(tags$style("#CPResult{color: White;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                                   ))),
                            column(4,
                                   textOutput("PPResult"),
                                   tags$head(tags$style("#PPResult{color: White;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                                   )))
         
               ))
               
),

  tabPanel("R Codes",
         fluidRow(
           column(10, offset=1,
                     div(icon("github")),
                     a("View code on GitHub",href="https://github.com/wangj2727/ChangeBoundary"),
                     h5("If the above link didn't work, try type this link, https://github.com/wangj2727/ChangeBoundary, to the url."),
                     h5("R version: 4.1.1 (2021-08-10)"),
                     h5("Key package version:"),
                     HTML("<ul><li>ldbounds_2.0.0</li><li>shiny_1.7.1</li><li>dplyr_1.0.7</li><li>shinycssloaders_1.0.0</li><li>shinyjs_2.1.0</li><li>bslib_0.3.1</li></ul>")
           )
           ))
))

server <- function(input, output, session) {


  ### reactive script used for reset parameter to default
  observeEvent(input$reset_input, {
    shinyjs::reset("reset-input")
  })
  
  ### 7-step image output
  output$figure1 <- renderImage({
    list(
      src = file.path("7stepproc2.png"),  ### might want to update it 
      contentType = "image/png",
      width = 960,
      height =640
    )
  }, deleteFile = FALSE)
  
  

  ##############################
  ### 
  ### assign parameters 
  ###
  ##############################
  selectedPhi <- reactive({ifelse(input$iuse == "Power family", input$phi_orig_power,
                        ifelse(input$iuse == "Hwang-Shih-DeCani family", input$phi_orig_Hwang,1))})
  
  selectedIuse <- reactive(which(iuseType==input$iuse))
  
  times_planned<-reactive(as.numeric(unlist(str_split(input$planned_times, ",")))) 
  times_actual <- reactive(as.numeric(unlist(str_split(input$actual_toNow, ",")))) 
  newfutimes<-reactive(as.numeric(unlist(str_split(input$newfutimes, ","))))
  
  z<- reactive(input$z)
  alpha <- reactive(input$alpha)
  HypoType <- reactive(input$HypoType)
  user_phi <- reactive(input$user_phi)
  Power_orig<-reactive(input$Power_orig)
  
  ###########################################
  ### 
  ### compute boundaries and CPs for plotting 
  ###
  ##########################################
  
  temp <- reactiveValues(data = NULL)
  
  observeEvent(input$clickthis, {
   
      
      times <- unique(c(times_actual(), times_planned()[times_planned()>times_actual()]))
      times_update <- c(times_actual(), newfutimes()[newfutimes()>times_actual()])
      tadapt<- times_actual()[length(times_actual())]
      
      boundary<-ldBounds(t=times,iuse=selectedIuse(), alpha=alpha(), sides = 1, phi = selectedPhi())$upper.bounds
      
      
      futimes<-times_planned()[times_planned()>tadapt] # future times, orig.
      fubounds<-boundary[times_planned()>tadapt] # orig future bound.
      
      theta<-0 # use null to compute CRP (cond. rej. prob.)
      CRP<-crossprob(theta,tadapt,z(),futimes,fubounds)
      
      # Now consider the new schedule of future look times
      u1u2<-(newfutimes()-tadapt)/(1-tadapt) # times for new standard Brownian motion B0(t)
      
      if(HypoType() =='original'){
        # calculate upper bounds given original alpha level, actual schedule up to now + new future schedules after adaptation time, and pre-specified spending function
        zb_est <- ldBounds(t=times_update, iuse=selectedIuse(), alpha=alpha(), sides = 1, phi = selectedPhi())$upper.bounds
        
        # calculate original hypothesized treatment effect if the corresponding check box was selected by the users
        theta_orig<-ldPower(za=rep(-8,length(times_update)),zb=zb_est,t=times_update,pow=Power_orig(), drift=NULL)$drift
      }
      
      ### for calculation predictive power
      L <- (z() - 5) /sqrt(tadapt)  # lower limits of the integral
      U <- (z() + 5) /sqrt(tadapt)  # upper limits of the integral
      
      # create equally-spaced values between L and U
      intervalNum <- 50
      dx <- (U-L)/50  ### increment
      inbetween <- seq(L, U, by=dx)
      
      #compute revised Z(t) boundaries and condition powers using series of phi values to be used for plotting
      infomat<-c()
      for(i in 1:30){
        
        d1d2<-ldBounds(t=u1u2,iuse=3,phi=0.1*i, alpha=CRP[length(CRP)], sides=1)$upper.bounds
        # power spending function with power parameter phi.  Generates new boundary d1,d2 yielding the given CRP
        
        c1c2<-(d1d2*sqrt(newfutimes()-tadapt)+z()*sqrt(tadapt))/sqrt(newfutimes()) # revised Z(t) boundaries for future looks that yield the given CRP
        
        if(HypoType() =='original'){
          # Compute conditional power under the original hypothesized treatment effect
          cptheta_orig<-crossprob(theta_orig,tadapt,z(),newfutimes(),c1c2)
          newrow<-c(.1*i,c1c2,cptheta_orig)
        }else{
          # Compute conditional power under current trend
          theta<-z()/sqrt(tadapt)
          cptheta<-crossprob(theta,tadapt,z(),newfutimes(),c1c2)
          
          newrow<-c(.1*i,c1c2,cptheta)  
        }
        
        #####
        # compute predictive power
        pp<-NULL
        for(f in inbetween){
          pp<-rbind(pp,crossprob(f,tadapt,z(),newfutimes(),c1c2))
        }

        y<- -(tadapt/2)*(inbetween-z()/sqrt(tadapt))^2
        f<-sqrt(tadapt/(2*pi))*pp*exp(y)

        simpOut<-NULL
        for(ff in 1:ncol(f)){
          simpOut <- c(simpOut,simpson(f[,ff], dx))
        }
        ######

        newrow <- c(newrow,simpOut)
        infomat<-rbind(infomat,newrow)
      }
      temp$data <- infomat
  })
  
  # ##### test
  # output$testText<-renderTable({temp$data})
  
  ###########################################
  ### 
  ### Boundary plot
  ###
  ##########################################
    output$figure2 <-  renderPlot({
 
      if (input$clickthis == 0)
        return()
      
      isolate({
      tadapt<- times_actual()[length(times_actual())]
      numpara <- length(newfutimes())
      mycol <- RColorBrewer::brewer.pal(n = numpara, name = "Set2")
      
      plot(NULL,xlim=c(0,3),ylim=c(0,5),bty="l",xlab=bquote(~phi), ylab="Boundary",main=paste0("Z(",tadapt,")=",z()))
      grid(nx=NULL,ny=NULL)
      
      for(i in 1:numpara){
        
         if(sum(temp$data[,i+1]<abs(qnorm(0.025)), na.rm=TRUE)){
           lines(temp$data[,1],temp$data[,i+1],col="red",lwd=2, lty=3)
         }else{
          lines(temp$data[,1],temp$data[,i+1],col=mycol[i],lwd=2)
        }
        
        text(2.5, temp$data[nrow(temp$data), i+1], paste0("at t=",newfutimes()[i]))
      }
      mtext(bquote("Spending Function"~alpha*t^phi),side=3)
      })
      
  },res = 96)

    output$figure2Note <- renderText({
      if (input$clickthis == 0)
        return()
      
      isolate({
      paste0("This figure shows boundary values as a function of the power parameter phi when looks are at t= ",
             toString(newfutimes())," after observating a z-score of ", z()," at the adaptation time")
      })
    })
    ###########################################
    ### 
    ### Boundary notes 
    ###
    ##########################################
    output$BoundaryResult <- renderText({
      
      if (input$clickthis2 == 0)
        return()
      
      isolate({
      times <- unique(c(times_actual(), times_planned()[times_planned()>times_actual()]))
      tadapt<- times_actual()[length(times_actual())]
      
      boundary<-ldBounds(t=times,iuse=selectedIuse(), alpha=alpha(), sides = 1, phi = selectedPhi())$upper.bounds  # boundaries at original times, 0.5 and 1
      
      futimes<-times_planned()[times_planned()>tadapt] 
      fubounds<-boundary[times_planned()>tadapt]
      
      theta<-0 
      CRP<-crossprob(theta,tadapt,z(),futimes,fubounds)
      
      # Now consider the new schedule of future look times
      u1u2<-(newfutimes()-tadapt)/(1-tadapt) # times for new standard Brownian motion B0(t)
      
      d1d2<-ldBounds(t=u1u2,iuse=3,phi=user_phi(), alpha=CRP[length(CRP)], sides=1)$upper.bounds 
      
      c1c2<-(d1d2*sqrt(newfutimes()-tadapt)+z()*sqrt(tadapt))/sqrt(newfutimes())
      pval <- round(2*(1-pnorm(c1c2)), 4)
      c1c2 <- round(c1c2, 4)
      
      paste0("At the selected phi value, Z-score (nominal two-sided p-value) boundaries = ", toString(paste0(c1c2, " (", pval, ")"))," by t= ", toString(newfutimes()))
      })
    })
    
    ###########################################
    ### 
    ### Power plot 
    ###
    ##########################################
    output$figure3 <- renderPlot({
      
      if (input$clickthis == 0)
        return()
      
      isolate({
      tadapt<- times_actual()[length(times_actual())]
      numpara <- length(newfutimes())
      mycol <- RColorBrewer::brewer.pal(n = numpara, name = "Set2")
      
      plot(NULL,xlim=c(0,3),ylim=c(0,1.05),bty="l",xlab=bquote(~phi), ylab="Conditional Power",main=paste0("Z(",tadapt,")=",z()))
      grid(nx=NULL,ny=NULL)
      
      
      for(i in 1:numpara){
        lines(temp$data[,1],temp$data[,1+numpara+i],col=mycol[i],lwd=2)
        text(2.5, temp$data[nrow(temp$data), 1+numpara+i]-0.01, paste0("by t=",newfutimes()[i]))
      }
      mtext(bquote("Spending Function"~alpha*t^phi),side=3)   
      })
      
    },res = 96)
    
    output$figure3Note <- renderText({
      
      if (input$clickthis == 0)
        return()
      
      isolate({
      tadapt<- times_actual()[length(times_actual())]
      paste0("This figure shows conditional power values by t=",toString(newfutimes())," as a function of the power parameter phi when looks are at t= ", 
             toString(newfutimes()), " after observing Z(",tadapt,")= ",z())
      })
    })
    
    ###############################
    ### 
    ### Conditional Power notes
    ###
    ###############################
    output$CPResult <- renderText({
      
      if (input$clickthis2 == 0)
        return()
      
      isolate({
      times <- unique(c(times_actual(), times_planned()[times_planned()>times_actual()]))
      times_update <- c(times_actual(), newfutimes()[newfutimes()>times_actual()])
      tadapt<- times_actual()[length(times_actual())]
      
      boundary<-ldBounds(t=times,iuse=selectedIuse(), alpha=alpha(), sides = 1, phi = selectedPhi())$upper.bounds  # boundaries at original times, 0.5 and 1
      
      futimes<-times_planned()[times_planned()>tadapt] # future times, orig.
      fubounds<-boundary[times_planned()>tadapt] # orig future bound.
      
      theta<-0 # use null to compute CRP (cond. rej. prob.)
      CRP<-crossprob(theta,tadapt,z(),futimes,fubounds)
      
      # Now consider the new schedule of future look times
      u1u2<-(newfutimes()-tadapt)/(1-tadapt) # times for new 
      # standard Brownian motion B0(t)
      
      d1d2<-ldBounds(t=u1u2,iuse=3,phi=user_phi(), alpha=CRP[length(CRP)], sides=1)$upper.bounds 
      
      c1c2<-(d1d2*sqrt(newfutimes()-tadapt)+z()*sqrt(tadapt))/sqrt(newfutimes()) 
      
      theta<-z()/sqrt(tadapt)    
      fubounds<-c1c2
      cptheta<-crossprob(theta,tadapt,z(),newfutimes(),fubounds)
      
      zb_est <- ldBounds(t=times_update, iuse=selectedIuse(), alpha=alpha(), sides = 1, phi = selectedPhi())$upper.bounds
      theta_orig<-ldPower(za=rep(-8,length(times_update)),zb=zb_est,t=times_update,pow=Power_orig(), drift=NULL)$drift 
      cptheta_orig<-crossprob(theta_orig,tadapt,z(),newfutimes(),fubounds)
      
      if(HypoType() == "current"){
        paste0("At the selected phi value, conditional power = ",
               toString(round(cptheta, 4)), " by t= ", toString(newfutimes()))
      }else{
        paste0("At the selected phi value, conditional power = ",
               toString(round(cptheta_orig, 4)), " by t= ", toString(newfutimes()))
      }
      })
    })
    
    ##########################################
    ##
    ## Predictive Power plot
    ##
    #########################################
    output$figure4 <- renderPlot({

      if (input$clickthis == 0)
        return()

      isolate({
        tadapt<- times_actual()[length(times_actual())]
        numpara <- length(newfutimes())
        mycol <- RColorBrewer::brewer.pal(n = numpara, name = "Set2")

        plot(NULL,xlim=c(0,3),ylim=c(0,1.05),bty="l",xlab=bquote(~phi), ylab="Predictive Power",main=paste0("Z(",tadapt,")=",z()))
        grid(nx=NULL,ny=NULL)


        for(i in 1:numpara){
          lines(temp$data[,1],temp$data[,3+numpara+i],col=mycol[i],lwd=2)
          text(2.5, temp$data[nrow(temp$data), 3+numpara+i]-0.01, paste0("by t=",newfutimes()[i]))
        }
        mtext(bquote("Spending Function"~alpha*t^phi),side=3)
      })

    },res = 96)

    output$figure4Note <- renderText({

      if (input$clickthis == 0)
        return()

      isolate({
        tadapt<- times_actual()[length(times_actual())]
        paste0("This figure shows predictive power values by t=",toString(newfutimes())," as a function of the power parameter phi when looks are at t= ",
               toString(newfutimes()))
      })
    })
    
    ###############################
    ### 
    ### Predictive Power notes
    ###
    ###############################
    output$PPResult <- renderText({
      
      if (input$clickthis2 == 0)
        return()
      
      isolate({
        times <- unique(c(times_actual(), times_planned()[times_planned()>times_actual()]))
        times_update <- c(times_actual(), newfutimes()[newfutimes()>times_actual()])
        tadapt<- times_actual()[length(times_actual())]
        
        boundary<-ldBounds(t=times,iuse=selectedIuse(), alpha=alpha(), sides = 1, phi = selectedPhi())$upper.bounds  # boundaries at original times, 0.5 and 1
        
        futimes<-times_planned()[times_planned()>tadapt] # future times, orig.
        fubounds<-boundary[times_planned()>tadapt] # orig future bound.
        
        theta<-0 # use null to compute CRP (cond. rej. prob.)
        CRP<-crossprob(theta,tadapt,z(),futimes,fubounds)
        
        # Now consider the new schedule of future look times
        u1u2<-(newfutimes()-tadapt)/(1-tadapt) # times for new 
        # standard Brownian motion B0(t)
        
        ### for calculation predictive power
        L <- (z() - 5) /sqrt(tadapt)  # lower limits of the integral
        U <- (z() + 5) /sqrt(tadapt)  # upper limits of the integral
        
        # create equally-spaced values between L and U
        intervalNum <- 50
        dx <- (U-L)/50  ### increment
        inbetween <- seq(L, U, by=dx)
        
        d1d2<-ldBounds(t=u1u2,iuse=3,phi=user_phi(), alpha=CRP[length(CRP)], sides=1)$upper.bounds 
        
        c1c2<-(d1d2*sqrt(newfutimes()-tadapt)+z()*sqrt(tadapt))/sqrt(newfutimes()) 
        
        pp<-NULL
        for(f in inbetween){
          pp<-rbind(pp,crossprob(f,tadapt,z(),newfutimes(),c1c2))
        }
        
        y<- -(tadapt/2)*(inbetween-z()/sqrt(tadapt))^2
        f<-sqrt(tadapt/(2*pi))*pp*exp(y)
        
        simpOut<-NULL
        for(ff in 1:ncol(f)){
          simpOut <- c(simpOut,simpson(f[,ff], dx))
        }
        
        paste0("At the selected phi value, predictive power = ",
                 toString(round(simpOut, 4)), " by t= ", toString(newfutimes()))

      })
    })
    
}

shinyApp(ui, server)