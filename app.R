#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(rhandsontable)
library(NetBlotch)
library(ggtern)
library(tidyr)
library(plotly)
#library(Ternary)

# Define UI for application
ui <- fluidPage(
    
    titlePanel("Net Blotch model"),
    
    tabsetPanel(
        tabPanel("Overview",
            sidebarLayout(
                sidebarPanel(
                    h1("Introduction"),
                    p("This page gives a brief overview of the research question this app was designed for.")
                ),
                mainPanel(
                    h2("Net Blotch"),
                    p("Net blotch (*Pyrenohora teres*) is a fungal pathogen of barley and can cause up to 30% yield losses in Australia."),
                    p("Control of net blotch relies to a great extent on fungicides - particularly DMI ans SDHI fungicides - as well as resistant cultivars and crop rotations."),
                    p("However, in Western Australia, reduced sensitivity to several DMI fungicides was detected in 2013 in Westnern Australia, while resistance to SDHI fungicides was reported in 2020."),
                    h2("Fungicide resistance management"),
                    p("Fungicide resistance management strategies are regularly explored to delay the development of resistance management."),
                    p("One strategy that is frequently recommended is to use mixtures of different fungicide modes of action."),
                    p("The addition of a low-risk fungicide to a high-risk fungicide reduces the growth rate of the entire pathogen population, which reduces selection for isolates resistant to the high-risk fungicide."),
                    p("However, while mixtures work in isolation, do they work when the high-risk fungicides are also used solo in the landscape?"),
                    h2("Research question:"),
                    p(strong("Is the use of solo fungicides detrimental to fungicide resistance management?")),
                    h2("Model"),
                    p("This app is the front-end for a model designed to answer this question."),
                    p("The model simulates selection for fungicide resistant pathogen isolates in a landscape, consisting of 3 fields, one using a mixture, and the other two using solo products."),
                    p("3 fungicides are included:"),
                    tags$li("Azole, parameterised for propiconazole."),
                    tags$li("SDHI, parameterised with fluxapyroxad."),
                    tags$li("a low-risk mixing partner."),
                    p("The app allows the user to explore how fast resistance develops to the azole and the SDHI under different management scenarios."),
                    h2("App layout"),
                    p("The app is divided into several tabs."),
                    tags$li(strong(em("Results:")), "displays the result of varying the proportions of fields 2 and 3 in the landscape."),
                    tags$li(strong(em("Extended results:")),"allows the model to be run for all landscape combinations."),
                    tags$li(strong(em("Management:")), "specifies the fungicide spray program used in each field."),
                    tags$li(strong(em("Single realisation:")),"this tab allows the user to run a simulation for a single landscape, and explore the results."),
                    tags$li(strong(em("Crop parameters:")), "this tab allows the user to change the parameters related to crop growth and senescence."),
                    tags$li(strong(em("Pathogen parameters:")),"this tab allows the user to change the parameters related to the rate of growth of a sensitive pathogen population"),
                    tags$li(strong(em("Fungicide parameters:")), "this tab allows the user to change the effectiveness of each fungicide on a sensitive pathogen population"),
                    tags$li(strong(em("Resistance parameters:")),"this tab allows the user to change the strength of the fungicide insensitive isolates."),
                    br(),
                    p("Once parameters are changed, the results can be re-run.")
                )
            )
        ),
        tabPanel("Results",sidebarLayout(
                sidebarPanel(
                    numericInput("Results.n_steps","Number of steps:",value = 11,max = Inf,min=1),
                    numericInput("Results.n_years","Years:",value=50,min=1,max=Inf),
                    actionButton("RUNRESULT", "Run"),
                    helpText(span(textOutput("time_results"),style="color:red"))                ),
                mainPanel(
                    plotOutput("varying_field_2_and_3")
                )
        )),
        tabPanel("Extended results",
            sidebarLayout(
                sidebarPanel(
                    h3("Ternary plot"),
                    # First, the number of years to run the simulation
                    numericInput("years","Number of years:",min = 1,max = Inf,value = 10),
                    p("The figure displays the number of years that there is effective control in each field."),
                    p("The size of each field is varied regularly."),
                    p("The following input specifies the resolution of the grid search. For example, if steps = 2, then only proportions of 0 and 1 are simulated for each field, whereas if steps = 3, then proportions of 0, 0.5, and 1 are simulated for each field."),
                    numericInput("triangle_steps","Steps:",min=2,max=Inf,value=5),
                    actionButton("RUNTRIANGLE", "Run all proportions"),
                    helpText(span(textOutput("time_triangle"),style="color:red")),
                    p("To download the data as a csv file, press the following button:"),
                    downloadButton("downloadTernary","Download")
                ),
                mainPanel(fluidPage(
                    fluidRow(
                        plotOutput("triangle_plot"),
                        h4("Figure. The lifespan of effective control in each field, in different landscapes. Each point represents a different proportion of the three fields. Hover over the points to see the values."))
                ))
            )
        ),
        tabPanel("Management",
            sidebarLayout(
                sidebarPanel(
                    h3("Management"),
                    p("Here, you can set the fungicide programme in each field."),
                    p("The default (starting) settings are:"),
                    tags$li("Field 1: SDHI seed treatment; azole foliar spray"),
                    tags$li("Field 2: Low-risk seed treatment; azole foliar spray"),
                    tags$li("Field 3: SDHI seed-treatment; low-risk foliar spray"),
                    p("Feel free to change this using the tables below."),
                    p("The efficacy of each fungicide can be altered in the 'Fungicide parameters' tab"),
                    p("If you need more sprays per year, change it here (this will reset the values in the tables)"),
                    numericInput("nSpraysPerYear","Number of sprays:",value=3,min=0,max=Inf,step=1),
                    p("To re-run the graphs on this page, use the button here:"),
                    # At the top, have an action button. Press this to run the simulation
                    actionButton("RUNFIELDS", "Run"),
                    helpText(
                        tags$li("Fungicide 1: Azole"),
                        tags$li("Fungicide 2: SDHI"),
                        tags$li("Fungicide 3: Low-risk")
                    ),
                    h3("Field 1:"),
                    # First, management in field 1 (default is azole + SDHI)
                    rHandsontableOutput("management_field1"),
                    h3("Field 2:"),
                    # Second, management in field 2 (default is azole + mixing)
                    rHandsontableOutput("management_field2"),
                    h3("Field 3:"),
                    # Third, management in field 3 (default is SDHI + mixing)
                    rHandsontableOutput("management_field3"),
                    h3("Effective life:"),
                    p("The following table gives the HAD for each field on a susceptible or resistant pathogen population."),
                    p("For comparison, the threshold for effective control is: ", textOutput(outputId = "threshold_HAD",inline=TRUE)),
                    br(),
                    tableOutput("HAD_table")
                ),
                mainPanel(
                    fluidRow(column(4, h2("Field 1", align = "center")),
                             column(4, h2("Field 2", align = "center")),
                             column(4, h2("Field 3", align = "center"))),
                    fluidRow(
                        column(4, plotOutput("field1_SLIR")),
                        column(4, plotOutput("field2_SLIR")),
                        column(4, plotOutput("field3_SLIR"))
                    ),
                    fluidRow(
                        column(4, plotOutput("field1_sev")),
                        column(4, plotOutput("field2_sev")),
                        column(4, plotOutput("field3_sev"))
                    )
                )
            )
        ),
        # The simulation panel allows the user to run a single simulation
        tabPanel(
            "Single realisation",
            sidebarLayout(
                sidebarPanel(
                    h3("Run a single simulation"),
                    p("This tab allows you to run a simulation for a landscape for a single set of field trials, which you specify below."),
                    p("The graphs on the right will display two things:"),
                    tags$li("The HAD over time in each field."),
                    tags$li("The resistance frequency of each resistance gene in each field."),
                    br(),
                    # Press this to run a single simulation
                    actionButton("SINGLEREALISATION", "Run"),
                    p("Specify the size of each field, as a proportion:"),
                    # Field size
                    rHandsontableOutput("field_sizes"),
                    # Number of years for the single realisation
                    numericInput("years_SR","Number of years:",min = 1,max = Inf,value = 20,step = 1),
                    helpText("This input changes the number of years to run this simulation for; it doesn't change the number of years in any other tab.")
                ),
                mainPanel(
                    fluidRow(column(8,plotOutput("HAD"))),
                    fluidRow(column(4,plotOutput("field1_resistance_plot")),
                             column(4,plotOutput("field2_resistance_plot")),
                             column(4,plotOutput("field3_resistance_plot")))
                )
            )
        ),
        # The crop panel allows the user to change the crop in the absence of the pathogen
        tabPanel("Crop parameters",
                 sidebarLayout(
                     sidebarPanel(
                         tabsetPanel(
                             tabPanel("Parameters",
                                      h3("Crop parameters"),
                                      p("In this section, you can change the parameters for the crop model."),
                                      p("Change any of the parameters, and re-run the model using the button below."),
                                      # At the top, have an action button. This only reruns the crop model
                                      actionButton("RUNCROP", "Re-run the model"),
                                      numericInput("maxAreaIndex","Maximum area index",min = 0,max = Inf,value = 2.75),
                                      numericInput("cropGrowthRate","Crop growth rate",min = 0,max = Inf,value = 0.01),
                                      numericInput("cropGrowthMidPoint","Crop growth mid-point",min = -Inf,max = Inf,value = 550),
                                      numericInput("cropSenesMidPoint","Crop senescence mid-point",min = -Inf,max = Inf,value = 1050),# TODO: Ensure cropSenesMidPoint > cropGrowthMidPoint
                                      tags$hr(),
                                      selectInput("crop_plot_x_axis","X-axis",choices=c("Calendar days","Degree days"),selected = "Degree days")
                             ),
                             tabPanel("Fitting",
                                      # Fit to data
                                      h3("Fit to data"),
                                      p("A file may be uploaded to fit the model to."),
                                      p("The file should have the following columns:"),
                                      tags$li("Degree days - the time at which observations are taken, in degree days,"),
                                      tags$li("HAI - the healthy area index at each time point,"),
                                      tags$li("Group (optional), specifying groups of variables."),
                                      fileInput("file_crop","Choose CSV file",multiple=FALSE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                      actionButton("FITCROP","Fit"),
                                      helpText("This may take some time. On my PC takes ~ 3 minutes per group."),
                                      tableOutput("crop_fit")
                             )
                         )
                     ),
                     mainPanel(
                         fluidRow(
                             column(7,plotOutput("plot_crop"))
                             ))
                 )),
        tabPanel(
            "Pathogen parameters",
            sidebarLayout(
                sidebarPanel(
                    h3("Pathogen parameters"),
                    tabsetPanel(
                        tabPanel("Parameters",
                                 p("The following parameter values specify the values used in the simulations in other tabs."),
                                 p("Change any of the parameters, and re-run the model using the button below to see changes."),
                                 actionButton("RUNPATHOGEN", "Re-run the model"),
                                 numericInput("transmissionRate","Transmission rate",min = 0,max = Inf,value = 0.3),
                                 numericInput("latentPeriod","Latent period",min = 0,max = Inf,value = 225),
                                 numericInput("infectiousPeriod","Infectious period",min = 0,max = Inf,value = 225),
                                 numericInput("inoculumShape","Inoculum shape parameter",min = 0,max = Inf,value = 1),
                                 numericInput("inoculumScale","Inoculum scale parameter",min = 0,max = Inf,value = 40),
                                 numericInput("initialInoculum","Initial inoculum",min = 0,max = Inf,value = 0.003),
                                 numericInput("dispersalProportion","Dispersal proportion",min = 0,max = 1,value = 0.0),
                        ),
                        tabPanel("Fitting",
                                 # Fit to data
                                 h3("Fit to data"),
                                 p("A file may be uploaded to fit the model to."),
                                 p("The optimisation will attempt to fit the transmission rate, latent period, infectious period, and inoculum parameters"),
                                 p("The file should have the following columns:"),
                                 tags$li("Degree days - the time at which observations are taken, in degree days,"),
                                 tags$li("Severity - the severity (as a percentage) at each time point,"),
                                 tags$li("Group (optional), specifying groups of variables."),
                                 fileInput("file_pathogen","Choose CSV file",multiple=FALSE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                 actionButton("FITPATHOGEN","Fit"),
                                 helpText("This may take some time. On my PC takes ~ 10 minutes per group."),
                                 tableOutput("pathogen_fit"),
                                 p("Once parameters are fit, the selected parameters should be set in the 'Parameters' tab to set them in the model.")
                        )
                    )
                ),
                mainPanel(
                fluidRow(column(4,plotOutput("pathogen_SLIR_plot")),
                         column(4,plotOutput("pathogen_sev_plot"))),
                fluidRow(column(4,plotOutput("primary_inoculum_plot")))
            ))
        ),
        tabPanel("Fungicide parameters",
                 sidebarLayout(
                     sidebarPanel(
                         p("This tab allows you to change how the fungicides affect a sensitive pathogen strain."),
                         h3("Fungicide parameters"),
                         p("The following table specifies the dose-response parameters of each fungicide on a sensitive pathogen strain, as well as the decay rate of a fungicide when applied as a foliar."),
                         rHandsontableOutput("sensitive_fungicide_parms"),
                         h3("Seed treatment"),
                         p("The following parameters adjust the amount of dose released from an application of seed treatmnet through the season."),
                         rHandsontableOutput("seed_treatment"),
                         h3("Test"),
                         p("To test the efficacy of the above parameters, set the spray times, and run the simulation on a susceptible pathogen population"),
                         rHandsontableOutput("control_sprays"),
                         br(),
                         actionButton("TEST_CONTROL","Run test")
                     ),
                     mainPanel(
                        h3("Parameterisation:"),
                        fluidRow(
                            column(5,plotOutput("dose_response_plot")),
                            column(5,plotOutput("seed_treatment_plot"))
                        ),
                        h3("In-field result (generated by pressing 'Run test' button):"),
                        fluidRow(
                            column(5,plotOutput("control_SLIR")),
                            column(5,plotOutput("control_sev"))
                        )
                     )
                 )),
        tabPanel("Fungicide resistance parameters",
                 sidebarLayout(
                     sidebarPanel(
                         h3("Fungicide resistance"),
                         p("Fungicide resistance pathogen strains have lower transmission rate, and longer latent period."),
                         h3("Dose-response curve"),
                         p("The following parameters specify the degree of fungicide resistance to each of the fungicides."),
                         rHandsontableOutput("dose_response_resistant"),
                         
                         h3("Initial frequency"),
                         p("Specify the initial frequency of resistance in the population for each gene."),
                         p("A confers resistance to the azole, B confers resistance to the SDHI"),
                         rHandsontableOutput("initial_resistance_frequency"),
                         helpText("Double-click on the values above to see their actual value - R only displays a rounded value."),
                         h3("Mutation rate"),
                         p("The model also includes a mutation rate; this is the probability that any spore will have mutated in one of the two alleles."),
                         numericInput("mutationRate","Mutation rate",min = 0,max = 1,value = 1e-10)
                     ),
                     mainPanel(plotOutput("resistance_plot"))
                 ))
    )
)

# Define server logic to draw the stuff
server <- function(input, output,session) {
    
    # This is a flag:
        # DEBUG = 0: no info printed
        # DEBUG = 1: each top-level function name is printed
        # DEBUG = 2: Printing contents of things
    DEBUG = 0
    
    ## *************** RESULTS TAB **************** ##
    
    run_varying_2_and_3 = eventReactive(input$RUNRESULT,{
        
        if(DEBUG >= 1) print("Running results for 'Results' tab")
        
        # First calculate maximum HAD
        max_HAD = get_crop_HAD()
        
        df = data.frame(prop_field_2 = seq(0.0,0.5,length.out = input$Results.n_steps),EL.1 = NA,EL.2 = NA, EL.3 = NA)
        df$prop_field_3 = 0.5 - df$prop_field_2
        
        withProgress(message = "Running simulations:", {
            for(i in seq_len(nrow(df))){
            
                if(i==1){ 
                    incProgress(amount = 0,detail = paste(i, "/", nrow(df), sep = ""))
                } else if(i > 1) incProgress(amount = 1 / nrow(df),detail = paste(i, "/", nrow(df), sep = ""))
                
                if(DEBUG >= 1) print(paste("\tField proportions:",df$prop_field_2[i],df$prop_field_3[i]))
                
                # This has to be run again because of how initial inoculum works...
                set_PARAMETERS()
                
                # Set the field sizes
                PARAMETERS$field_size = c(0.5,
                                          df$prop_field_2[i],
                                          df$prop_field_3[i])
                
                abc = simulate(input$Results.n_years)
                
                EL = calculate_effective_life(abc, max_HAD = max_HAD)
                
                df$EL.1[i] = EL[1]
                df$EL.2[i] = EL[2]
                df$EL.3[i] = EL[3]
                
            }
        })
        
        return(df)
        
    })
    
    output$varying_field_2_and_3 <- renderPlot({
        
        df = run_varying_2_and_3()
        
        if(DEBUG == 1) print("Plotting 'Results' tab output")
        if(DEBUG == 2) print(head(df))
        
        plot(df$EL.1,x=df$prop_field_2,type="l",xlab="Field 2 size (proportion)",ylab="Effective life",ylim=c(0,max(df[,c("EL.1","EL.2","EL.3")],na.rm=TRUE)))
        lines(df$EL.2,x=df$prop_field_2,lty=2)
        lines(df$EL.3,x=df$prop_field_2,lty=3)
        legend("bottomleft",lty=c(1,2,3),title="Field:",c("1","2","3"))
        
    })
    
    ## *************** RESULTS 2 TAB **************** ##
    
    # Run a single field with just a solo azole or SDHI, and a mixture of them both
    run_azole_solo <- eventReactive(input$RUNSINGLE, {

        set_PARAMETERS()
        
        n_years = input$years
        
        # Specify management as a solo azole
        PARAMETERS$management = data.frame(Field = 1,Fungicide = c(1, 3),Year = 1,Time = c(0, 500),Dose = 1)
        tmp_df = PARAMETERS$management
        for (i in 2:n_years) {
            tmp_df$Year = i
            PARAMETERS$management = rbind(PARAMETERS$management, tmp_df)
        }
        
        # Run for a single year
        abc = simulate(n_years)
        
        return(abc)
        
    })
    
    # Run a single field with just a solo SDHI
    run_SDHI_solo <- eventReactive(input$RUNSINGLE, {
        
        set_PARAMETERS()
        
        n_years = input$years
        
        # Specify management as a solo SDHI
        PARAMETERS$management = data.frame(Field = 1,Fungicide = c(2, 3),Year = 1,Time = c(0, 500),Dose = 1)
        tmp_df = PARAMETERS$management
        for (i in 2:n_years) {
            tmp_df$Year = i
            PARAMETERS$management = rbind(PARAMETERS$management, tmp_df)
        }
        
        # Run for a single year
        abc = simulate(n_years)
        
        return(abc)
        
    })
    
    # Run a single field with a mixture of azole and SDHI
    run_mixture <- eventReactive(input$RUNSINGLE, {

        set_PARAMETERS()
        
        n_years = input$years
        
        # Specify management as a mixture of azole and SDHI
        PARAMETERS$management = data.frame(Field = 1,Fungicide = c(1, 2),Year = 1,Time = c(0, 500),Dose = 1)
        tmp_df = PARAMETERS$management
        for (i in 2:n_years) {
            tmp_df$Year = i
            PARAMETERS$management = rbind(PARAMETERS$management, tmp_df)
        }
        
        # Run for a single year
        abc = simulate(n_years)
        
        return(abc)
        
    })
    
    # Run a set of simulations, with various amounts of solo SDHI and solo azole
    run_triangle <- eventReactive(input$RUNTRIANGLE, {
        
        # First calculate maximum HAD
        solo_crop = run_crop()
        max_HAD = as.numeric(calculate_HAD(solo_crop)[2])
        
        df = data.frame(prop_azole = numeric(),prop_SDHI = numeric(),prop_mixture = numeric(),EL_f1 = numeric(),EL_f2 = numeric(),EL_f3 = numeric())
        
        loop = seq(0, 1, length.out = input$triangle_steps)
        
        # Populate df first, so that withProgress can be incremented easily
        for (azole_i in seq_along(loop)) {
            for (SDHI_i in seq(1, length(loop) - azole_i + 1)) {
                prop_azole = loop[azole_i]
                prop_SDHI = loop[SDHI_i]
                prop_mix = 1 - prop_azole - prop_SDHI
                
                df = rbind(df,
                           data.frame(prop_azole = prop_azole,prop_SDHI = prop_SDHI,prop_mixture = prop_mix,EL_f1 = NA,EL_f2 = NA,EL_f3 = NA)
                )
                
            }
        }
        
        withProgress(message = "Running combinations:", {
            for (i in 1:nrow(df)) {
                
                if(i==1){ 
                    incProgress(amount = 0,detail = paste(i, "/", nrow(df), sep = ""))
                } else if(i > 1) incProgress(amount = 1 / nrow(df),detail = paste(i, "/", nrow(df), sep = ""))
                
                # This has to be run again because of how initial inoculum works...
                set_PARAMETERS()
                
                # Set the field sizes
                PARAMETERS$field_size = c(df$prop_mixture[i],
                                          df$prop_azole[i],
                                          df$prop_SDHI[i])
                
                abc = simulate(input$years)
                
                EL = calculate_effective_life(abc, max_HAD = max_HAD)
                
                df$EL_f1[i] = EL[1]
                df$EL_f2[i] = EL[2]
                df$EL_f3[i] = EL[3]
                
            }
        })
        
        return(df)
        
    })
    
    triangle_data <- reactive({
        
        # Run a search with an increasing level of solo azole, an increasing level of solo SDHI, and an increasing level of both
        df = run_triangle()
        
        # Switch to long format
        # TODO: Switch from prop_SDHI to Field 2, etc...
        df.long = df %>% pivot_longer(cols = starts_with("EL"), names_to = "Field")
        df.long$Field = as.factor(df.long$Field)
        levels(df.long$Field) = c("Field 1", "Field 2", "Field 3")
        colnames(df.long)[1:3] = c("Field 1", "Field 2", "Field 3")
        colnames(df.long)[5] = "Effective life"
        
        return(df.long)
        
    })
    
    output$triangle_plot <- renderPlot({
        
        # Run a search with an increasing level of solo azole, an increasing level of solo SDHI, and an increasing level of both
        df = triangle_data()
        
        # Using the Ternary package
        # df = run_triangle()
        # TernaryPlot("Field 1","Field 2","Field 3",grid.lines=5,grid.minor.lines = 4)
        # TernaryPoints(coordinates = triangle_graph[,1:3],col = rainbow(30)[triangle_graph$EL_f1],pch=16,cex=2)
        # legend("topleft",pch=16,cex=1.1,col=rainbow(30)[seq(1,21,length.out=5)],paste(seq(1,21,length.out=5)))
        
        # Alternatively could use ggtern package:
        p = ggtern(data = df,aes(x = `Field 1`,y = `Field 2`,z = `Field 3`,col = `Effective life`)) + 
                  geom_point(size = 10) + theme_bw() +   geom_text(aes(label=`Effective life`),col="white") + facet_wrap(vars(Field))
        print(p)
        
        # # Or using plot_ly (from https://rpubs.com/tskam/ternary_plot)
        # I'm not sure how to facet this though.
        # plot_ly(
        #     df.long,
        #     a = ~prop_azole,
        #     b = ~prop_SDHI,
        #     c = ~prop_mixture,
        #     color = ~value,
        #     type = "scatterternary"
        # )
        
    })
    
    # Plot the results of using each fungicide in a single field
    output$single_field <- renderPlot({
        withProgress(message = "Running model...", {
            incProgress(amount = 0, detail = "Running azole solo...")
            
            solo_azole = run_azole_solo()
            solo_azole_rf = calculate_resistance_frequency(solo_azole)
            solo_azole_rf$Time = (solo_azole_rf$Year - 1) + solo_azole_rf$time / (max(solo_azole_rf$time) + 1)
            
            incProgress(amount = 1 / 3, detail = "Running SDHI solo...")
            
            solo_SDHI = run_SDHI_solo()
            solo_SDHI_rf = calculate_resistance_frequency(solo_SDHI)
            solo_SDHI_rf$Time = (solo_SDHI_rf$Year - 1) + solo_SDHI_rf$time / (max(solo_SDHI_rf$time) + 1)
            
            incProgress(amount = 1 / 3, detail = "Running mixture...")
            
            mixture = run_mixture()
            mixture_rf = calculate_resistance_frequency(mixture)
            mixture_rf$Time = (mixture_rf$Year - 1) + mixture_rf$time / (max(mixture_rf$time) + 1)
            
        })
        
        par(mfrow = c(1, 2))
        plot(
            100 * solo_azole_rf$rf_field1_gene1,
            x = solo_azole_rf$Time,
            type = "l",
            xlab = "Time (years)",
            ylab = "Resistance frequency (%)",
            main = "Azole",
            ylim = range(
                100 * c(
                    solo_azole_rf$rf_field1_gene1,
                    mixture_rf$rf_field1_gene1
                ),
                na.rm = TRUE
            )
        )
        lines(100 * mixture_rf$rf_field1_gene1,
              x = mixture_rf$Time,
              lty = 2)
        legend("topleft", lty = c(1, 2), c("Solo", "Mixture"))
        plot(
            100 * solo_SDHI_rf$rf_field1_gene2,
            x = solo_SDHI_rf$Time,
            type = "l",
            xlab = "Time (years)",
            ylab = "Resistance frequency (%)",
            main = "SDHI",
            ylim = range(
                100 * c(
                    solo_SDHI_rf$rf_field1_gene2,
                    mixture_rf$rf_field1_gene2
                ),
                na.rm = TRUE
            )
        )
        lines(100 * mixture_rf$rf_field1_gene2,
              x = mixture_rf$Time,
              lty = 2)
        legend("topleft", lty = c(1, 2), c("Solo", "Mixture"))
        
    })
    
    ## ********** MANAGEMENT TAB ********** ##
    
    # Run the control strategies in each field
    run_management_susceptible <- eventReactive(input$RUNFIELDS,{
        
        incProgress(amount = 0,detail = "Susceptible")
        
        set_PARAMETERS()
        PARAMETERS$initial_resistance_frequency = c(0.0, 0.0)
        
        print(sapply(ls(PARAMETERS), function(x) get(x, envir = PARAMETERS)))
        
        abc = simulate(1)
        
    })
    # Run the control strategies in each field
    run_management_resistant <- eventReactive(input$RUNFIELDS,{
        
        incProgress(amount = 1/2,detail = "Resistant")
        
        set_PARAMETERS()
        PARAMETERS$initial_resistance_frequency = c(1.0, 1.0)
        
        abc = simulate(1)
            
    })
    
    HAD_each_field <- reactive({
        
        withProgress(message = "Running...",{
        
        # Create a table, with the HAD for each field in a susceptible or resistant pathogen population
        HAD_1_S = calculate_HAD(run_management_susceptible())$field.1[1]
        HAD_2_S = calculate_HAD(run_management_susceptible())$field.2[1]
        HAD_3_S = calculate_HAD(run_management_susceptible())$field.3[1]
        HAD_1_R = calculate_HAD(run_management_resistant())$field.1[1]
        HAD_2_R = calculate_HAD(run_management_resistant())$field.2[1]
        HAD_3_R = calculate_HAD(run_management_resistant())$field.3[1]
        
        HAD = data.frame(Field1 = c(HAD_1_S,HAD_1_R),
                         Field2 = c(HAD_2_S,HAD_2_R),
                         Field3 = c(HAD_3_S,HAD_3_R))
        colnames(HAD) = c("Field 1","Field 2","Field 3")
        rownames(HAD) = c("Susceptible","Resistant")
        
        return(HAD)
        
        })
        
    })
    
    output$HAD_table = renderTable({
            
            HAD_each_field()

    },rownames=TRUE)
    
    output$threshold_HAD = renderText({
        
        abc = 0.95 * calculate_HAD(run_crop())$field.1[1]
        
        return(abc)
        
    })
    
    output$field1_SLIR <- renderPlot({
        field1 = run_management_susceptible()
        plot_SLIR(field1, 1)
        
    })
    output$field2_SLIR <- renderPlot({
        field2 = run_management_susceptible()
        plot_SLIR(field2, 2)
        
    })
    output$field3_SLIR <- renderPlot({
        field3 = run_management_susceptible()
        plot_SLIR(field3, 3)
        
    })
    output$field1_sev <- renderPlot({
        field1 = run_management_susceptible()
        field1_sev = calculate_severity(field1)
        
        par(mar = c(4, 4, 1, 1) + 0.1)
        plot(
            100 * field1_sev$field.1,
            x = field1_sev$time,
            type = "l",
            xlab = "Time (degree days)",
            ylab = "Severity",
            ylim = c(0, 100)
        )
        
    })
    output$field2_sev <- renderPlot({
        field2 = run_management_susceptible()
        field2_sev = calculate_severity(field2)
        
        par(mar = c(4, 4, 1, 1) + 0.1)
        plot(
            100 * field2_sev$field.2,
            x = field2_sev$time,
            type = "l",
            xlab = "Time (degree days)",
            ylab = "Severity",
            ylim = c(0, 100)
        )
        
    })
    output$field3_sev <- renderPlot({
        field3 = run_management_susceptible()
        field3_sev = calculate_severity(field3)
        
        par(mar = c(4, 4, 1, 1) + 0.1)
        plot(
            100 * field3_sev$field.3,
            x = field3_sev$time,
            type = "l",
            xlab = "Time (degree days)",
            ylab = "Severity",
            ylim = c(0, 100)
        )
        
    })
    
    ## ********** SINGLE REALISATION TAB ********** ##
    
    # Run a single realization of the model over however many years
    run_single_realisation <- eventReactive(input$SINGLEREALISATION, {
        
        set_PARAMETERS()
        
        print(PARAMETERS$dose_response_shape)
        
        # Make sure PARAMETERS$management follows the same number of years as years_SR, not just years
        field1 = management_parameters$field1
        field2 = management_parameters$field2
        field3 = management_parameters$field3
        df = data.frame(Field = 1,Fungicide = field1$Fungicide,Year = 1,Time = field1$Time,Dose = field1$Dose)
        df = rbind(df,data.frame(Field = 2,Fungicide = field2$Fungicide,Year = 1,Time = field2$Time,Dose = field2$Dose))
        df = rbind(df,data.frame(Field = 3,Fungicide = field3$Fungicide,Year = 1,Time = field3$Time,Dose = field3$Dose))
        tmp_df = df
        for (i in 2:input$years_SR) {
            tmp_df$Year = i
            df = rbind(df, tmp_df)
        }
        PARAMETERS$management = df
        
        time.start = Sys.time()
        # Run the simulation
        abc = simulate(input$years_SR)
        time.end = Sys.time()
        times$t_single_iteration = time.end - time.start
        times$n_years_single_iteration = input$years_SR
        
        return(abc)
        
    })
    
    # Plot the HAD over time for the three fields
    output$HAD <- renderPlot({
        
        withProgress(message = "Running...", {
            
            full_sim = run_single_realisation()
            
            full_sim_HAD = calculate_HAD(full_sim)
            
            plot(full_sim_HAD$field.1,x = full_sim_HAD$Year,type = "o",xlab = "Time (years)",ylab = "HAD",ylim=c(0,max(full_sim_HAD[,-1],na.rm=TRUE)))
            lines(full_sim_HAD$field.2,x = full_sim_HAD$Year,type = "o",lty = 2)
            lines(full_sim_HAD$field.3,x = full_sim_HAD$Year,type = "o",lty = 3)
            legend("bottomleft",lty = c(1, 2, 3),c("Field 1", "Field 2", "Field 3"))
            
        })
        
    })
    
    output$field1_resistance_plot <- renderPlot({
        
        full_sim = run_single_realisation()
        
        full_rf = calculate_resistance_frequency(full_sim)
        
        full_rf$Time = full_rf$Year - 1 + full_rf$time / (max(full_rf$time) + 1)
        
        par(mar=c(4,4,1,1)+0.1)
        plot(100*full_rf$rf_field1_gene1,x = full_rf$Time,ylim = c(0, 100),type = "l",
             xlab = "Time (years)",ylab = "Resistance frequency (%)",main = "Field 1")
        lines(100*full_rf$rf_field1_gene2,x=full_rf$Time, col = 2)
        legend("topleft",lty=1,col=c(1,2),c("Azole","SDHI"))
        
    })
    output$field2_resistance_plot <- renderPlot({
        
        full_sim_2 = run_single_realisation()
        
        full_rf_2 = calculate_resistance_frequency(full_sim_2)
        
        full_rf_2$Time = full_rf_2$Year - 1 + full_rf_2$time / (max(full_rf_2$time) + 1)
        
        par(mar=c(4,4,1,1)+0.1)
        plot(100*full_rf_2$rf_field2_gene1,x = full_rf_2$Time,ylim = c(0, 100),type = "l",
             xlab = "Time (years)",ylab = "Resistance frequency (%)",main = "Field 2")
        lines(100*full_rf_2$rf_field2_gene2,x=full_rf_2$Time, col = 2)
        legend("topleft",lty=1,col=c(1,2),c("Azole","SDHI"))
        
    })
    output$field3_resistance_plot <- renderPlot({
        
        full_sim = run_single_realisation()
        
        full_rf = calculate_resistance_frequency(full_sim)
        
        full_rf$Time = full_rf$Year - 1 + full_rf$time / (max(full_rf$time) + 1)
        
        par(mar=c(4,4,1,1)+0.1)
        plot(100*full_rf$rf_field3_gene1,x = full_rf$Time,ylim = c(0, 100),type = "l",
             xlab = "Time (years)",ylab = "Resistance frequency (%)",main = "Field 3")
        lines(100*full_rf$rf_field3_gene2,x=full_rf$Time, col = 2)
        legend("topleft",lty=1,col=c(1,2),c("Azole","SDHI"))
        
    })
    
    ## ***************  CROPS TAB  **************** ##
    
    run_crop <- eventReactive(input$RUNCROP | input$RUNTRIANGLE,{
        
        incProgress(amount = 0.1,detail="Running crop simulation")
        
        set_PARAMETERS()
        
        if(DEBUG >= 1) print("run_crop()")
        
        # Turn off the pathogen in all fields
        PARAMETERS$initial_inoculum = rep(0, 3)
        PARAMETERS$management$Dose = 0.0
        
        t_start = Sys.time()
        abc = simulate(1)
        t_end = Sys.time()
        times$t_crop = t_end - t_start
        
        return(abc)
        
    },
    ignoreNULL = TRUE)
    
    get_crop_HAD <- reactive({
        
        HAD = NA
        crop_sim = run_crop()
        HAD = calculate_HAD(crop_sim)$field.1
        HAD
        
    })
    
    fit_crop <- eventReactive(input$FITCROP,{
        
        req(input$file_crop)

        # Wants to return a data.frame of each crop parameter, with each row being a different group
        crop.data = crop_data()
        
        # A function that returns the sum of squared differences between the data and the simulation
        crop_ssd<-function(params,data){
            
            set_PARAMETERS()
            
            # Set crop parameters
            PARAMETERS$crop_growth_midpoint = params[["c_g_m"]]
            PARAMETERS$crop_growth_rate = params[["c_g_r"]]
            PARAMETERS$crop_max_area_index = params[["c_m_a_i"]]
            PARAMETERS$crop_senesence_midpoint = params[["c_s_m"]]
            
            # Turn off the pathogen in all fields
            PARAMETERS$initial_inoculum = rep(0, 3)
            
            # Simulate the epidemic
            abc = simulate(1)
            
            # Extract the simulated observations at the time of the observed points
            sim.cut = abc$H.F1[abc$time %in% data$degree.days]
            
            # Calculate the sum of squared differences
            SSD = sum((sim.cut - data$hai)^2)
            
            if(DEBUG >= 1) print(c(params,SSD))
            
            return(SSD)
            
        }
        
        # A data.frame to store each of the fits
        fit.df = data.frame(Group = seq_len(length(levels(crop.data$group))),
                            growth_rate=NA,
                            growth_midpoint = NA,
                            senescence_midpoint = NA,
                            max_area_index = NA,
                            # This last column is TRUE / FALSE of whether convergence was achieved
                            fit = NA)
        
        withProgress(message = "Fitting crop data...",{
            
            for(i in 1:length(levels(crop.data$group))){
                
                if(i==1){
                    incProgress(amount=0,detail="Group 1")
                } else incProgress(amount=1/length(levels(crop.data$group)),detail=paste("Group", i))
                
                # Extract the data for this group
                obs = subset(crop.data,crop.data$group == levels(crop.data$group)[i])
                
                PARAMETERS$crop_max_area_index = crop_parameters$maxAreaIndex
                PARAMETERS$crop_growth_rate = crop_parameters$cropGrowthRate
                PARAMETERS$crop_growth_midpoint = crop_parameters$cropGrowthMidPoint
                PARAMETERS$crop_senesence_midpoint = crop_parameters$cropSenesMidPoint
                
                parms = c(crop_parameters$cropGrowthMidPoint,
                          crop_parameters$cropGrowthRate,
                          crop_parameters$maxAreaIndex,
                          crop_parameters$cropSenesMidPoint)
                names(parms) = c("c_g_m","c_g_r","c_m_a_i","c_s_m")
                
                fit = optim(par = parms,data = obs,fn=crop_ssd,control=list(reltol = 0.1))
                
                if(DEBUG >= 2) print(fit)
                
                fit.df$growth_rate[i] = fit$par[2]
                fit.df$growth_midpoint[i] = fit$par[1]
                fit.df$senescence_midpoint[i] = fit$par[4]
                fit.df$max_area_index[i] = fit$par[3]
                # If convergence == 0, then the fit was found
                fit.df$fit[i] = !as.logical(fit$convergence)
                
            }
            
        })
        
        if(DEBUG >= 2) print(fit.df)
        return(fit.df)
        
    })
    
    # This sorts out the crop data if it's been uploaded
    # TODO: Finish this off
    crop_data <- reactive({
        
        crop.data = NA
        
        if(!is.null(input$file_crop)){
            
            crop.data <- read.csv(input$file_crop$datapath)
            names(crop.data) = tolower(names(crop.data))
            
            # TODO: Check that the file is in the right format
            # That the column names are corrects
            
            # Make the grouping variable into a factor
            if(!("group" %in% names(crop.data))) crop.data$group = 1
            crop.data$group = factor(crop.data$group)
            
        }
        
        return(crop.data)
        
    })
    
    # Plot the crop by itself
    output$plot_crop <- renderPlot({
        
        withProgress(message = "Running crop simulation", {
            
            # Run the crop reactive event
            crop_sim = run_crop()
            
        })
        
        # Read in crop data if it's available
        crop.data = crop_data()
        
        par(mar = c(4, 4, 1, 1))
        
        if(input$crop_plot_x_axis == "Degree days"){
            
            YLIM = NULL
            if(!is.null(input$file_crop)) YLIM = range(c(crop_sim$H.F1,crop.data$hai),na.rm=TRUE)
            
            # Plot the healthy crop
            plot(crop_sim$H.F1,x = crop_sim$time,xlab = "Time since emegence (degree days)",ylab = "Leaf area index",type = "l",ylim=YLIM)
            
            if(!is.null(input$file_crop)){
                
                for(j in 1:length(levels(crop.data$group))){
                    .data = subset(crop.data,group == levels(crop.data$group)[j])
                    points(.data$hai,x=.data$degree.day,pch=15+j)
                }
                legend("topleft",pch=15+seq(1,length(levels(crop.data$group))),levels(crop.data$group),title="Group:")
                
            }
            
        } else if(input$crop_plot_x_axis == "Calendar days") {
            
            # The model runs in degree days since emergence, so to plot in calendar days, have to convert.
            dd.df = data.frame(Date = seq.Date(from=as.Date("2010-07-01"),to=as.Date("2011-06-30"),by="day"))
            # We have previously assumed 15 degree days per calendar day, but this is very naive.
            # Instead, we can use a sin function to roughly estimate it (I fit this by eye to data from Kamali & Boyd, 2000)
            dd.df$day_degrees = 5 * sin((seq(1,365) + 280) * 2*pi/366) + 15
            dd.df$cum_degrees = cumsum(dd.df$day_degrees)
            
            # Now need to calculate the average H.F1 per day
            dd.df$HAI = NA
            for(i in 1:nrow(dd.df)){
                # Get the elements that are between cum_degrees[i-1] and cum_degrees[i]
                index = which(crop_sim$time > dd.df$cum_degrees[i] & crop_sim$time <= dd.df$cum_degrees[i+1])
                if(length(index) > 0) dd.df$HAI[i] = mean(crop_sim$H.F1[index],na.rm=TRUE)
            }
            
            YLIM = NULL
            if(!is.null(input$file_crop)) YLIM = range(c(crop_sim$H.F1,crop.data$hai),na.rm=TRUE)
            
            plot(dd.df$HAI,x = dd.df$Date,xlab = "Date",ylab = "Leaf area index",type = "l",xlim=range(dd.df$Date[!is.na(dd.df$HAI)],na.rm=TRUE),ylim=YLIM)
            
            # TODO: This doesn't work yet - need to convert degree days to calendar days for the data
            if(!is.null(input$file_crop)){
                for(j in 1:length(levels(crop.data$group))){
                    .data = subset(crop.data,group == levels(crop.data$group)[j])
                    points(.data$hai,x=.data$degree.day,pch=15+j)
                }
                legend("topleft",pch=15+seq(1,length(levels(crop.data$group))),levels(crop.data$group),title="Group:")
            }
        }
        
    })
    
    output$crop_fit <- renderTable({
        
        df = fit_crop()
        
        df$growth_rate = format(df$growth_rate,scientific=TRUE)
        df$growth_midpoint = format(df$growth_midpoint,digits=1,nsmall=0)
        df$senescence_midpoint = format(df$senescence_midpoint,digits=1,nsmall=0)
        df$max_area_index = format(df$max_area_index,nsmall=2)
        
        names(df) = c("Group","GR","GM","SM","MAI","Fit?")
        
        df
        
    })
    
    ## ************** PATHOGEN TAB **************** ##
    
    run_pathogen <- eventReactive(input$RUNPATHOGEN | input$RUNTRIANGLE, {
        
        if(DEBUG >= 1) print("run_pathogen()")
        set_PARAMETERS()
        
        PARAMETERS$initial_resistance_frequency = rep(0,2)
        PARAMETERS$management$Dose = 0
        
        t_start = Sys.time()
        abc = simulate(1)
        t_end = Sys.time()
        times$t_pathogen = t_end - t_start
        
        return(abc)
        
    },
    ignoreNULL = TRUE)
    
    pathogen_data <- reactive({
        
        path.data = NA
        
        if(!is.null(input$file_pathogen)){
            
            path.data <- read.csv(input$file_pathogen$datapath)
            names(path.data) = tolower(names(path.data))
            
            # TODO: Check that the file is in the right format
            # That the column names are corrects
            
            # Make the grouping variable into a factor
            if(!("group" %in% names(path.data))) path.data$group = 1
            path.data$group = factor(path.data$group)
            
        }
        
        return(path.data)
        
    })
    
    # Plot the pathogen dynamics
    output$pathogen_SLIR_plot <- renderPlot({
        
        withProgress(message = "Running pathogen simulation", {
            # Run the pathogen reactive event
            pathogen_sim = run_pathogen()
        })
        
        plot_SLIR(pathogen_sim,field_index = 1)
        
    })
    output$pathogen_sev_plot <- renderPlot({
        
        pathogen_sim = run_pathogen()
        
        pathogen_sev = calculate_severity(pathogen_sim)
        
        par(mar=c(4,4,1,1)+0.1)
        plot(100*pathogen_sev$field.1,x = pathogen_sev$time,type = "l",xlab = "Time (degree days)",ylab = "Severity (%)",ylim=c(0,100))
        
        path.data = pathogen_data()
        
        if(!is.null(input$file_pathogen)){
            for(j in 1:length(levels(path.data$group))){
                .data = subset(path.data,group == levels(path.data$group)[j])
                points(.data$severity,x=.data$degree.day,pch=15+j)
            }
            legend("topleft",pch=15+seq(1,length(levels(path.data$group))),levels(path.data$group),title="Group:")
            
        }
        
    })
    
    # Plot the shape of primary inoculum over time
    output$primary_inoculum_plot <- renderPlot({
        
        set_PARAMETERS()
        
        times = seq(0,PARAMETERS$max_time)
        
        inoculum = PARAMETERS$initial_inoculum[1] * (times^(PARAMETERS$inoculum_shape-1) * exp(- times / PARAMETERS$inoculum_scale)) / (gamma(PARAMETERS$inoculum_shape) * PARAMETERS$inoculum_scale^PARAMETERS$inoculum_shape)
        
        par(mar=c(4,4,1,1)+0.1)
        plot(inoculum,x=times,type="l",xlab="Time (degree days)",ylab="Inoculum influx",main="")
        
    })
    
    fit_pathogen <- eventReactive(input$FITPATHOGEN,{
        
        req(input$file_pathogen)
        
        # Wants to return a data.frame of each crop parameter, with each row being a different group
        path.data = pathogen_data()
        
        # A function that returns the sum of squared differences between the data and the simulation
        path_ssd<-function(params,data){
            
            set_PARAMETERS()
            
            # Susceptible pathogen population, no resistance
            PARAMETERS$mutation_rate = 0.0
            PARAMETERS$initial_resistance_frequency = c(0.0,0.0)
            # No fungicide
            PARAMETERS$management$Dose = 0
            # Turn off dispersal - just focus on one field
            PARAMETERS$dispersal_proportion = 0.0
            
            # Set variables
            PARAMETERS$transmission_rate = params[["t_r"]]
            PARAMETERS$latent_period = params[["l_p"]]
            PARAMETERS$infectious_period = params[["i_p"]]
            PARAMETERS$inoculum_shape = params[["i_shape"]]
            PARAMETERS$inoculum_scale = params[["i_scale"]]
            PARAMETERS$initial_inoculum = rep(params[["i_i"]],3)
            
            if(DEBUG >= 2) print(params)
            
            if(PARAMETERS$transmission_rate < 0) return(NA)
            if(params[["l_p"]] < 0) return(NA)
            if(params[["i_p"]] < 0) return(NA)
            if(params[["i_shape"]] < 0) return(NA)
            if(params[["i_scale"]] < 0) return(NA)
            if(params[["i_i"]] < 0) return(NA)
            
            # Simulate the epidemic
            abc = try(simulate(1))
            if(class(abc) == "try-error") return(NA)
            
            abc.sev = 100 * calculate_severity(abc)
            
            # Extract the simulated observations at the time of the observed points
            sev.cut = abc.sev$field.1[data$degree.days]
            
            # Calculate the sum of squared differences
            SSD = sum((sev.cut - data$severity)^2)
            
            if(DEBUG >= 2) print(SSD)
            
            return(SSD)
            
        }
        
        # A data.frame to store each of the fits
        fit.df = data.frame(Group = seq_len(length(levels(path.data$group))),
                            t_r=NA,
                            l_p = NA,
                            i_p = NA,
                            i_shape = NA,
                            i_scale = NA,
                            i_i = NA,
                            # This last column is TRUE / FALSE of whether convergence was achieved
                            fit = NA)
        
        withProgress(message = "Fitting pathogen data...",{
            
            for(i in 1:length(levels(path.data$group))){
                
                if(i==1){
                    incProgress(amount=0,detail="Group 1")
                } else incProgress(amount=1/length(levels(path.data$group)),detail=paste("Group", i))
                
                # Extract the data for this group
                obs = subset(path.data,path.data$group == levels(path.data$group)[i])
                
                parms = c(pathogen_parameters$transmissionRate,
                          pathogen_parameters$latentPeriod,
                          pathogen_parameters$infectiousPeriod,
                          pathogen_parameters$inoculumShape,
                          pathogen_parameters$inoculumScale,
                          pathogen_parameters$initialInoculum)
                names(parms) = c("t_r","l_p","i_p","i_shape","i_scale","i_i")
                
                fit = optim(par = parms,data = obs,fn=path_ssd,control=list(reltol = 0.01))
                
                if(DEBUG >= 2) print(fit)
                
                fit.df$t_r[i] = fit$par[1]
                fit.df$l_p[i] = fit$par[2]
                fit.df$i_p[i] = fit$par[3]
                fit.df$i_shape[i] = fit$par[4]
                fit.df$i_scale[i] = fit$par[5]
                fit.df$i_i[i] = fit$par[6]
                fit.df$fit[i] = !as.logical(fit$convergence)
                
            }
            
        })
            
        if(DEBUG >= 2) print(fit.df)
        return(fit.df)
        
    })
    
    output$pathogen_fit <- renderTable({
        
        df = fit_pathogen()
        
        names(df) = c("Group","Transmission rate","Latent period","Infectious period",
                      "Inoculum shape","Inoculum scale","Initial inoculum","Fit?")
        
        df
        
    })
    
    ## ************** CONTROL TAB  **************** ##
    
    # RUNCONTROL runs the model with different spray programs, and presents the result as a barchart
    run_control <- eventReactive(input$TEST_CONTROL,{
        
        req(input$control_sprays)
        
        if(DEBUG >= 1) print("run_control()")
        
        set_PARAMETERS()
        PARAMETERS$initial_resistance_frequency = c(0,0)
        
        # Set management parameters based on side panel of control window
        ctrl_sprays = hot_to_r(input$control_sprays)
        
        PARAMETERS$management = PARAMETERS$management[1:3, ]
        PARAMETERS$management$Fungicide = ctrl_sprays$Fungicide
        PARAMETERS$management$Dose = ctrl_sprays$Dose
        PARAMETERS$management$Time = ctrl_sprays$Time
        PARAMETERS$management$Year = 1
        PARAMETERS$management$Field = 1
        
        # Run for a single year
        abc = simulate(1)
        
        return(abc)
        
    })
    
    # Run a field with just seed treatment, to get the dose over time of the seed treatment application
    run_seed_treatment <- reactive({
        
        set_PARAMETERS()
        
        if(DEBUG >= 1) print("run_seed_treatment()")
        
        # Add a seed treatment
        PARAMETERS$management = data.frame(Field = 1,Fungicide = 1,Year = 1,Time = 0,Dose = 1)
        
        # Run the model
        cba = simulate(1)
        
        return(cba)
        
    })
    
    # Plot dose-response curves and dose of seed treatment based on the parameters given in control panel
    output$dose_response_plot <- renderPlot({
        
        # TODO: These equations aren't right.
        # TODO: Also need to correct the resistant dose response parameters, not sure they're read in correctly.
        
        if(DEBUG >= 1) print("dose_response_plot()")
        
        set_PARAMETERS()
        
        dose = seq(0, 1, 0.01)

        plot(1 - PARAMETERS$dose_response_asymptote[1,1] + PARAMETERS$dose_response_asymptote[1,1] * exp(-PARAMETERS$dose_response_shape[1,1] * dose),x = dose,type = "l",col = 2,main = "Dose-response curves",xlab = "Dose",ylab = "Effect",ylim=c(0,1))
        lines(1 - PARAMETERS$dose_response_asymptote[2,1] + PARAMETERS$dose_response_asymptote[2,1] * exp(-PARAMETERS$dose_response_shape[2,1] * dose),x = dose,col = 3)
        lines(1 - PARAMETERS$dose_response_asymptote[3,1] + PARAMETERS$dose_response_asymptote[3,1] * exp(-PARAMETERS$dose_response_shape[3,1] * dose),x = dose,col = 4)
        legend("topright",lty = 1,col = c(2, 3, 4),c("Azole", "SDHI", "Mix"))
        
    })
    
    # Plot the dose of a seed treatment application over time
    output$seed_treatment_plot <- renderPlot({
        
        if(DEBUG >= 1) print("seed_treatment_plot()")
        
        ST_sim = run_seed_treatment()
        
        plot(ST_sim$D1.F1,x = ST_sim$time,type = "l",xlab = "Time (degree days)",ylab = "Effective dose",main="Effective seed treatment dose")
        
    })
    
    # For control, show the effect of different spray programs
    output$control_SLIR <- renderPlot({
        
        withProgress(message = "Running simulation", {
            # Run the control
            control_sim = run_control()
        })
        
        par(mar = c(4, 4, 1, 1))
        
        plot_SLIR(control_sim,field_index = 1)
        
    })
    output$control_sev <- renderPlot({
        
        control_sim = run_control()
        control_sev = calculate_severity(control_sim)
        
        par(mar = c(4, 4, 1, 1))
        plot(100*control_sev$field.1,x = control_sev$time,type = "l",
             xlab = "Time (degree days)",ylab = "Severity (%)",ylim=c(0,100))
        
    })
    
    ## ************ RESISTANCE TAB **************** ##
    
    output$resistance_plot <- renderPlot({
        
        req(input$sensitive_fungicide_parms,input$dose_response_resistant)
        
        # Dose response plot
        DR_parms = hot_to_r(input$dose_response_resistant)
        
        dose = seq(0, 1, 0.01)
        
        par(mfrow = c(1, 2))
        plot(1 - DR_parms$Azole_S[1] + DR_parms$Azole_S[1] * exp(-DR_parms$Azole_S[2] * dose),x = dose,type = "l",col = 2,main = "Azole",xlab = "Dose",ylab = "Effect",ylim=c(0,1))
        lines(1 - DR_parms$Azole_R[1] + DR_parms$Azole_R[1] * exp(-DR_parms$Azole_R[2] * dose),x = dose,lty = 2,col = 2)
        legend("topright",col=2,lty=c(1,2),c("Sensitive","Resistant"))
        
        plot(1 - DR_parms$SDHI_S[1] + DR_parms$SDHI_S[1] * exp(-DR_parms$SDHI_S[2] * dose),x = dose,type = "l",col = 3,main = "SDHI",xlab = "Dose",ylab = "Effect",ylim=c(0,1))
        lines(1 - DR_parms$SDHI_R[1] + DR_parms$SDHI_R[1] * exp(-DR_parms$SDHI_R[2] * dose),x = dose,col = 3,lty = 2)
        legend("topright",col=3,lty=c(1,2),c("Sensitive","Resistant"))
        
    })
    
    ### ******************** RHandsontable ***************************
    
    # Field 1 is using SDHI and azole together
    output$management_field1 <- renderRHandsontable({
        # Create an example data.frame
        if(input$nSpraysPerYear == 2){
        initial_management_1 = data.frame(
            Fungicide = as.integer(c(2, 1)),
            Dose = 1,
            Time = c(0, 600)
        )} else if(input$nSpraysPerYear == 3){
            initial_management_1 = data.frame(
                Fungicide = as.integer(c(2, 1, 1)),
                Dose = 1,
                Time = c(0, 500, 700)
            )            
        } else if(input$nSpraysPerYear == 4){
            initial_management_1 = data.frame(
                Fungicide = as.integer(c(2, 1, 1, 1)),
                Dose = 1,
                Time = c(0, 500, 700, 900)
            )            
        } else {
            initial_management_1 = data.frame(
                Fungicide = as.integer(rep(1,input$nSpraysPerYear)),
                Dose = 1,
                Time = rep(500,input$nSpraysPerYear)
            )
        }
        rhandsontable(initial_management_1, rowHeaders = NULL)
    })
    # Field 2 is azole + mixing partner
    output$management_field2 <- renderRHandsontable({
        # Create an example data.frame
        if(input$nSpraysPerYear == 2){
            initial_management_2 = data.frame(
                Fungicide = as.integer(c(3, 1)),
                Dose = 1,
                Time = c(0, 600)
            )} else if(input$nSpraysPerYear == 3){            
                initial_management_2 = data.frame(
                Fungicide = as.integer(c(3, 1, 1)),
                Dose = 1,
                Time = c(0, 500, 700)
            )
            } else if(input$nSpraysPerYear == 4){
                initial_management_2 = data.frame(
                    Fungicide = as.integer(c(3, 1, 1, 1)),
                    Dose = c(1,1,1,0),
                    Time = c(0, 500, 700,1000)
                )
            } else {
                initial_management_2 = data.frame(
                    Fungicide = as.integer(rep(1,input$nSpraysPerYear)),
                    Dose = 1,
                    Time = rep(500,input$nSpraysPerYear)
                )
            }
        rhandsontable(initial_management_2, rowHeaders = NULL)
    })
    # Field 3 is SDHI + mixing partner
    output$management_field3 <- renderRHandsontable({
        # Create an example data.frame
        if(input$nSpraysPerYear == 2){
            initial_management_3 = data.frame(
                Fungicide = as.integer(c(2, 3)),
                Dose = 1,
                Time = c(0, 600)
            )
        } else if(input$nSpraysPerYear == 3){
            initial_management_3 = data.frame(
                Fungicide = as.integer(c(2, 3, 3)),
                Dose = 1,
                Time = c(0, 500,700)
            )
        } else if(input$nSpraysPerYear == 4){
            initial_management_3 = data.frame(
                Fungicide = as.integer(c(2, 3, 3, 3)),
                Dose = c(1,1,1,0),
                Time = c(0, 500,700,1000)
            )
        } else {
            initial_management_3 = data.frame(
                Fungicide = as.integer(rep(1,input$nSpraysPerYear)),
                Dose = 1,
                Time = rep(500,input$nSpraysPerYear)
            )
        }
        rhandsontable(initial_management_3, rowHeaders = NULL)
    })
    
    # Dose-response parameters
    output$sensitive_fungicide_parms <- renderRHandsontable({
        # Need the asymptote, shape, and decay rate for each fungicide
        # TODO: Why am I specifying these twice - here and when specifying fungicide_parameters()
        dose_response_parameters = data.frame(
            Azole = c(1.0, 10.0, 0.006),
            SDHI = c(1.0, 30.0, 0.00513),
            Mixing = c(1.0, 20.0, 0.006)
        )
        rownames(dose_response_parameters) = c("Asymptote", "Shape", "Decay rate")
        
        rhandsontable(dose_response_parameters, rowHeaderWidth = 200)
        
    })
    output$dose_response_resistant <- renderRHandsontable({
        
        isolate({
            set_fungicide_parameters()
            df.asymp = fungicide_parameters$dose_response_asymptote
            df.shape = fungicide_parameters$dose_response_shape
        })
        
        df_all = data.frame(
            Azole_S = c(df.asymp[1,1],df.shape[1,1]),
            Azole_R = c(1.0, 1.0),
            SDHI_S = c(df.asymp[2,2],df.shape[2,1]),
            SDHI_R = c(1.0, 3.0)
        )
        
        row.names(df_all) = c("Asymptote","Shape")
        
        rhandsontable(df_all) %>%
            hot_col("Azole_S",readOnly = TRUE) %>%
            hot_col("SDHI_S",readOnly = TRUE)
        
    })
    
    output$seed_treatment <- renderRHandsontable({
        # Need the asymptote, shape, and decay rate for each fungicide
        seed_treatments_parameters = data.frame(Shape = 4,
                                                Scale = 20,
                                                Coefficient = 0.05031)
        rhandsontable(seed_treatments_parameters, rowHeaders = NULL)
        
    })
    
    output$field_area <- renderRHandsontable({
        # Need a field size for each field
        field_area = data.frame(A = 0.6, B = 0.3, C = 0.1)
        rhandsontable(field_area, rowHeaders = NULL)
    })
    
    output$initial_resistance_frequency <- renderRHandsontable({
        
        # Initial values
        irf = data.frame(A = 1e-10, B = 1e-10)
        
        format(irf$A,scientific = TRUE)
        format(irf$B,scientific = TRUE)
        
        rhandsontable(irf, rowHeaders = NULL)
        
    })
    
    output$control_sprays <- renderRHandsontable({
        control_management = data.frame(
            Fungicide = as.integer(c(1, 1, 1)),
            Dose = c(1, 0, 0),
            Time = c(0, 500, 800)
        )
        rhandsontable(control_management, rowHeaders = NULL)
    })
    
    output$field_sizes <- renderRHandsontable({
        fieldSize = data.frame(Field1 = 0.8,
                               Field2 = 0.1,
                               Field3 = 0.1)
        rhandsontable(fieldSize, rowHeaders = NULL)
    })
    
    # ***************** Parameters ******************
    
    # These reactive values get changed if I MANUALLY change the PARAMETERS environment - not if the input changes.
    # This invalidates the reactive, and makes PARAMETERS get re-set
    landscape_parameters = reactiveValues(
        field_sizes = PARAMETERS$field_size
    )
    crop_parameters = reactiveValues(
        maxAreaIndex = PARAMETERS$crop_max_area_index,
        cropGrowthRate = PARAMETERS$crop_growth_rate,
        cropGrowthMidPoint = PARAMETERS$crop_growth_midpoint,
        cropSenesMidPoint = PARAMETERS$crop_senesence_midpoint
    )
    pathogen_parameters = reactiveValues(
        transmissionRate = PARAMETERS$transmission_rate,
        latentPeriod = PARAMETERS$latent_period,
        infectiousPeriod = PARAMETERS$infectious_period,
        inoculumShape = PARAMETERS$inoculum_shape,
        inoculumScale = PARAMETERS$inoculum_scale,
        dispersalProportion = PARAMETERS$dispersal_proportion,
        initialInoculum = PARAMETERS$initial_inoculum,
        mutationRate = PARAMETERS$mutation_rate,
        initialResistanceFrequency = PARAMETERS$initial_resistance_frequency
    )
    management_parameters = reactiveValues(
        # TODO: Why do these magic numbers exist? Need to be the same as the input parameters... Bit odd.
        field1 = data.frame(Fungicide = as.integer(c(2,1,1)),
                            Dose = 1,
                            Time = c(0,500,700)),
        field2 = data.frame(Fungicide = as.integer(c(3,1,1)),
                            Dose = c(1,1,1),
                            Time = c(0,500,700)),
        field3 = data.frame(Fungicide = as.integer(c(2,3,3)),
                            Dose = c(1,1,1),
                            Time = c(0,500,700))
    )
    fungicide_parameters = reactiveValues(
        dose_response_asymptote = PARAMETERS$dose_response_asymptote,
        dose_response_shape = matrix(c(10,0.1,10,0.1,30,30,0.3,0.3,20,20,20,20),byrow=TRUE,ncol=4,nrow=3), # I've changed these from the default parameters
        decay_rate = PARAMETERS$fungicide_decay_rate,
        seed_treatment_coefficient = PARAMETERS$seed_treatment_coefficient,
        seed_treatment_shape = PARAMETERS$seed_treatment_shape,
        seed_treatment_scale = PARAMETERS$seed_treatment_scale
    )
    
    set_landscape_parameters<- reactive({
        if(!is.null(input$field_sizes)) landscape_parameters$field_sizes = as.numeric(hot_to_r(input$field_sizes)[1,])
    })
    
    set_crop_parameters <- reactive({
        crop_parameters$maxAreaIndex = input$maxAreaIndex
        crop_parameters$cropGrowthRate = input$cropGrowthRate
        crop_parameters$cropGrowthMidPoint = input$cropGrowthMidPoint
        crop_parameters$cropSenesMidPoint = input$cropSenesMidPoint
    })
    
    set_pathogen_parameters <- reactive({
        pathogen_parameters$transmissionRate = input$transmissionRate
        pathogen_parameters$latentPeriod = input$latentPeriod
        pathogen_parameters$infectiousPeriod = input$infectiousPeriod
        pathogen_parameters$inoculumShape = input$inoculumShape
        pathogen_parameters$inoculumScale = input$inoculumScale
        pathogen_parameters$dispersalProportion = input$dispersalProportion
        pathogen_parameters$initialInoculum = input$initialInoculum
        if(isTruthy(input$mutationRate)) pathogen_parameters$mutationRate = input$mutationRate
        if(!is.null(input$initial_resistance_frequency)){
            cba = hot_to_r(input$initial_resistance_frequency)
            pathogen_parameters$initialResistanceFrequency = as.numeric(cba[1,])
        }
    })
    
    set_management_parameters <- reactive({
        if(!is.null(input$management_field1)) management_parameters$field1 = hot_to_r(input$management_field1)
        if(!is.null(input$management_field2)) management_parameters$field2 = hot_to_r(input$management_field2)
        if(!is.null(input$management_field3)) management_parameters$field3 = hot_to_r(input$management_field3)
    })
    
    set_fungicide_parameters <- reactive({
        
        if(!is.null(input$sensitive_fungicide_parms)){
            abc = hot_to_r(input$sensitive_fungicide_parms)
            # Azole
            fungicide_parameters$dose_response_asymptote[1,c(1,3)] = abc$Azole[1]
            fungicide_parameters$dose_response_shape[1,c(1,3)] = abc$Azole[2]
            fungicide_parameters$decay_rate[1] = abc$Azole[3]
            # SDHI
            fungicide_parameters$dose_response_asymptote[2,c(1,2)] = abc$SDHI[1]
            fungicide_parameters$dose_response_shape[2,c(1,2)] = abc$SDHI[2]
            fungicide_parameters$decay_rate[2] = abc$SDHI[3]
            # Mixing partner
            fungicide_parameters$dose_response_asymptote[3,] = abc$Mixing[1]
            fungicide_parameters$dose_response_shape[3,] = abc$Mixing[2]
            fungicide_parameters$decay_rate[3] = abc$Mixing[3]
        }
        if(!is.null(input$dose_response_resistant)){
            abc = hot_to_r(input$dose_response_resistant)
            # Azole
            fungicide_parameters$dose_response_asymptote[1,c(2,4)] = abc$Azole_R[1]
            fungicide_parameters$dose_response_shape[1,c(2,4)] = abc$Azole_R[2]
            # SDHI
            fungicide_parameters$dose_response_asymptote[2,c(3,4)] = abc$SDHI_R[1]
            fungicide_parameters$dose_response_shape[2,c(3,4)] = abc$SDHI_R[2]
        }
        if(!is.null(input$seed_treatment)){
            abc = hot_to_r(input$seed_treatment)
            fungicide_parameters$seed_treatment_coefficient = abc$Coefficient[1]
            fungicide_parameters$seed_treatment_shape = abc$Shape[1]
            fungicide_parameters$seed_treatment_scale = abc$Scale[1]
        }
    })
    
    # Calling set_parameters resets all parameters to the reactiveValues
    set_parameters <- reactive({
        
        if(DEBUG >= 1) print("set_parameters()")
        set_landscape_parameters()
        set_crop_parameters()
        set_pathogen_parameters()
        set_management_parameters()
        set_fungicide_parameters()
        
    })
    
    # set_PARAMETERS assigns the reactiveValues to PARAMETERS
    set_PARAMETERS <- function(){
        
        if(DEBUG >= 1) print("set_PARAMETERS()")
        # Update the reactiveValues if necessary
        set_parameters()
        
        # PARAMETERS should not be reactive, so isolate it
        isolate({
            
            # Landscape
            PARAMETERS$field_size = landscape_parameters$field_sizes
            
            # Crop
            PARAMETERS$crop_max_area_index = crop_parameters$maxAreaIndex
            PARAMETERS$crop_growth_rate = crop_parameters$cropGrowthRate
            PARAMETERS$crop_growth_midpoint = crop_parameters$cropGrowthMidPoint
            PARAMETERS$crop_senesence_midpoint = crop_parameters$cropSenesMidPoint
            
            # Pathogen
            PARAMETERS$transmission_rate = pathogen_parameters$transmissionRate
            PARAMETERS$latent_period = pathogen_parameters$latentPeriod
            PARAMETERS$infectious_period = pathogen_parameters$infectiousPeriod
            PARAMETERS$inoculum_shape = pathogen_parameters$inoculumShape
            PARAMETERS$inoculum_scale = pathogen_parameters$inoculumScale
            PARAMETERS$dispersal_proportion = pathogen_parameters$dispersalProportion
            PARAMETERS$initial_inoculum = rep(pathogen_parameters$initialInoculum, 3)
            PARAMETERS$mutation_rate = pathogen_parameters$mutationRate
            PARAMETERS$initial_resistance_frequency = pathogen_parameters$initialResistanceFrequency
            PARAMETERS$genotype_proportions = NA
            
            # Management
            field1 = management_parameters$field1
            field2 = management_parameters$field2
            field3 = management_parameters$field3
            df = data.frame(Field = 1,Fungicide = field1$Fungicide,Year = 1,Time = field1$Time,Dose = field1$Dose)
            df = rbind(df,data.frame(Field = 2,Fungicide = field2$Fungicide,Year = 1,Time = field2$Time,Dose = field2$Dose))
            df = rbind(df,data.frame(Field = 3,Fungicide = field3$Fungicide,Year = 1,Time = field3$Time,Dose = field3$Dose))
            tmp_df = df
            for (i in 2:input$years) {
                tmp_df$Year = i
                df = rbind(df, tmp_df)
            }
            PARAMETERS$management = df
            
            # Fungicide
            PARAMETERS$dose_response_asymptote = fungicide_parameters$dose_response_asymptote
            PARAMETERS$dose_response_shape = fungicide_parameters$dose_response_shape
            PARAMETERS$fungicide_decay_rate = fungicide_parameters$decay_rate
            PARAMETERS$seed_treatment_coefficient = fungicide_parameters$seed_treatment_coefficient
            PARAMETERS$seed_treatment_shape = fungicide_parameters$seed_treatment_shape
            PARAMETERS$seed_treatment_scale = fungicide_parameters$seed_treatment_scale
            
        })
        
    }
    
    # ***************** Download ******************
    
    output$downloadTernary<-downloadHandler(
        filename = function(){
            "EffectiveLife.csv"
        },
        content = function(file) {
            write.csv(triangle_data(),file,row.names=FALSE)
        }
    )
    
    # ***************** Time stuff ******************
    
    # Use this to store the amount of time it takes to re-run different parts of the model
    times <- reactiveValues()
    # Time to run one replicate of the triangle
    times$t_one_rep = NULL
    # Time to run the crop by itself
    times$t_crop = NULL
    # Time to run the pathogen by itself
    times$t_pathogen = NULL
    # Time to run a single iteration
    times$t_single_iteration = NULL
    # The number of years that t_single_iteration was worked out for
    times$n_years_single_iteration = NULL
    
    output$time_triangle <- renderText({
        
        # Number of simulations: this is the sum of 1 to length(loop), from run_triangle
        n_sim = sum(seq(1,input$triangle_steps))
        
        # Estimate the time
        # This first line sets the default time for a single replicate as 2 minutes
        time_est = as.numeric(as.difftime("0:2:0") * n_sim,units="hours")
        if(!is.null(times$t_crop)) time_est = as.numeric(times$t_crop * input$years * n_sim * 2,units="hours")
        if(!is.null(times$t_pathogen)) time_est = as.numeric(times$t_pathogen * input$years * n_sim * 1.5,units="hours")
        if(!is.null(times$t_single_iteration)) time_est = as.numeric(times$t_single_iteration * n_sim * input$years / times$n_years_single_iteration,units="hours")
        
        paste("WARNING: This is likely to take",
            # Create a time difference, by default 10 minutes per run
            ifelse(time_est > 0.5,paste(format(time_est,digits=2), "hours"),paste(format(time_est*60,digits=2), "minutes")),"to complete.")
    })
    
    output$time_results <- renderText({
      
      # Number of simulations
      n_sim = input$Results.n_steps
      
      # Estimate the time
      # This first line sets the default time for a single replicate as 2 minutes
      time_est = as.numeric(as.difftime("0:2:0") * n_sim,units="hours")
      if(!is.null(times$t_crop)) time_est = as.numeric(times$t_crop * input$years * n_sim * 2,units="hours")
      if(!is.null(times$t_pathogen)) time_est = as.numeric(times$t_pathogen * input$years * n_sim * 1.5,units="hours")
      if(!is.null(times$t_single_iteration)) time_est = as.numeric(times$t_single_iteration * n_sim * input$years / times$n_years_single_iteration,units="hours")
      
      paste("WARNING: This is likely to take",
            # Create a time difference, by default 10 minutes per run
            ifelse(time_est > 0.5,paste(format(time_est,digits=2), "hours"),paste(format(time_est*60,digits=2), "minutes")),"to complete.")
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
