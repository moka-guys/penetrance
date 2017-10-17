#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(exact2x2)

# Constants

##rosenfeld, guy's, control incidences
#Rosenfeld et al. incidence	Guy's incidence	Control incidence
#0.17% (85/48,637)  	0.11% (35/31,269)* 	(10/22,246) 0.04%
#0.29% (97/33,226) 	0.12% (39/31,269)**	(6/22,246) 0.03%
#0.20% (68/33,226) 	0.06% (18/31,269)**	(6/22,246) 0.03%
#0.15% (50/33,226) 	0.07% (21/31,269)*	(12/22,246) 0.05%
#0.19% (62/33,226) 	0.21% (66/31,269)	(16/22,246) 0.07%
#0.14% (46/33,226) 	0.10% (31/31,269)	(1/22,246) 0.005%
#0.11% (35/33,226) 	0.06% (18/31,269)*	(10/22,246) 0.04%
#0.44% (146/33,226)	0.32% (99/31,269)*	(6/22,246) 0.03%
#0.28% (93/33,226) 	0.21% (65/31,269)	(9/22,246) 0.04%
#0.09% (29/33,226) 	0.05% (17/31,269)	(2/22,246) 0.01%
#0.11% (37/33,226) 	0.09% (29/31,269)	(5/22,246) 0.02%
#0.28% (136/48,637) 	0.16% (50/31,269)**	(12/22,246) 0.05%

controls_total <-22246
dup_prox_1q21_controls_with <- 10
dup_prox_1q21_controls_without <- controls_total - dup_prox_1q21_controls_with
del_dist_1q21_controls_with <- 6
del_dist_1q21_controls_without <- controls_total - del_dist_1q21_controls_with
dup_dist_1q21_controls_with <- 6
dup_dist_1q21_controls_without <- controls_total - dup_dist_1q21_controls_with
del_16p13_controls_with <- 12
del_16p13_controls_without <- controls_total - del_16p13_controls_with
del_16p12_controls_with <- 16
del_16p12_controls_without <- controls_total - del_16p12_controls_with
del_dist_16p11_controls_with <- 1
del_dist_16p11_controls_without <- controls_total - del_dist_16p11_controls_with
dup_dist_16p11_controls_with <- 10
dup_dist_16p11_controls_without <- controls_total - dup_dist_16p11_controls_with
del_prox_16p11_controls_with <- 6
del_prox_16p11_controls_without <- controls_total - del_prox_16p11_controls_with
dup_prox_16p11_controls_with <- 9
dup_prox_16p11_controls_without <- controls_total - dup_prox_16p11_controls_with
del_17q12_controls_with <- 2
del_17q12_controls_without <- controls_total - del_17q12_controls_with
dup_17q12_controls_with <- 5
dup_17q12_controls_without <- controls_total - dup_17q12_controls_with
dup_22q11_controls_with <- 12
dup_22q11_controls_without <- controls_total - dup_22q11_controls_with

rosen_total1 <- 48637
rosen_total2 <- 33226
dup_prox_1q21_rosen_with <- 85
dup_prox_1q21_rosen_without <- rosen_total1 - dup_prox_1q21_rosen_with
del_dist_1q21_rosen_with <- 97
del_dist_1q21_rosen_without <- rosen_total2 - del_dist_1q21_rosen_with
dup_dist_1q21_rosen_with <- 68
dup_dist_1q21_rosen_without <- rosen_total2 - dup_dist_1q21_rosen_with
del_16p13_rosen_with <- 50
del_16p13_rosen_without <- rosen_total2 - del_16p13_rosen_with
del_16p12_rosen_with <- 62
del_16p12_rosen_without <- rosen_total2 - del_16p12_rosen_with
del_dist_16p11_rosen_with <- 46
del_dist_16p11_rosen_without <- rosen_total2 - del_dist_16p11_rosen_with
dup_dist_16p11_rosen_with <- 35
dup_dist_16p11_rosen_without <- rosen_total2 - dup_dist_16p11_rosen_with
del_prox_16p11_rosen_with <- 146
del_prox_16p11_rosen_without <- rosen_total2 - del_prox_16p11_rosen_with
dup_prox_16p11_rosen_with <- 93
dup_prox_16p11_rosen_without <- rosen_total2 - dup_prox_16p11_rosen_with
del_17q12_rosen_with <- 29
del_17q12_rosen_without <- rosen_total2 - del_17q12_rosen_with
dup_17q12_rosen_with <- 37
dup_17q12_rosen_without <- rosen_total2 - dup_17q12_rosen_with
dup_22q11_rosen_with <- 136
dup_22q11_rosen_without <- rosen_total1 - dup_22q11_rosen_with

dup_prox_1q21_rosen_inci <- dup_prox_1q21_rosen_with / dup_prox_1q21_rosen_without
del_dist_1q21_rosen_inci <- del_dist_1q21_rosen_with / del_dist_1q21_rosen_without
dup_dist_1q21_rosen_inci <- dup_dist_1q21_rosen_with / dup_dist_1q21_rosen_without
del_16p13_rosen_inci <- del_16p13_rosen_with / del_16p13_rosen_without
del_16p12_rosen_inci <- del_16p12_rosen_with / del_16p12_rosen_without
del_dist_16p11_rosen_inci <- del_dist_16p11_rosen_with / del_dist_16p11_rosen_without
dup_dist_16p11_rosen_inci <- dup_dist_16p11_rosen_with / dup_dist_16p11_rosen_without
del_prox_16p11_rosen_inci <- del_prox_16p11_rosen_with / del_prox_16p11_rosen_without
dup_prox_16p11_rosen_inci <- dup_prox_16p11_rosen_with / dup_prox_16p11_rosen_without
del_17q12_rosen_inci <- del_17q12_rosen_with / del_17q12_rosen_without
dup_17q12_rosen_inci <- dup_17q12_rosen_with / dup_17q12_rosen_without
dup_22q11_rosen_inci <- dup_22q11_rosen_with / dup_22q11_rosen_without



# Define UI
ui <- fluidPage(
   
  # Application title
  titlePanel("Penetrance (incidence) calculator"),
  
  sidebarLayout(
    sidebarPanel(

      "As incidence is key to the estimate of penetrance, this app calculates incidence for your data and tests for difference from that presented by Rosenfeld et al. Penetrance estimates to follow.",
      br(),br(),
      "Instructions:",
      br(),
      "- select the locus of interest",
      br(),
      "-  enter the total number of patients tested",
      br(),
      "- enter the number of patients carrying the CNV",
      br(),br(),
      
      # select box
      selectInput("selectsyndrome", label = h3("Select syndrome"), 
                  choices = list("Proximal 1q21.1 (RBM8A) DUP", "Distal 1q21.1 (GJA5) DEL",
                                 "Distal 1q21.1 (GJA5) DUP", "16p13.11 (MYH11) DEL",
                                 "16p12.1 (CDR2) DEL", "Distal 16p11.2 (SH2B1) DEL",
                                 "Distal 16p11.2 (SH2B1) DUP", "Proximal 16p11.2 (TBX6) DEL",
                                 "Proximal 16p11.2 (TBX6) DUP", "17q12 (HNF1B) DEL",
                                 "17q12 (HNF1B) DUP", "22q11.2 (TBX1) DUP"), 
                  selected = 1),
      
      # Copy the line below to make a number input box into the UI.
      numericInput("total", label = h3("Total number of patients tested"), value = 30000),
      
      # Copy the line below to make a number input box into the UI.
      numericInput("withcnv", label = h3("Number of patients with the syndromic CNV"), value = 70),
      
      br(),br()
      
    ), #endsidebarpanel
    
    mainPanel(
      
      br(),br(),br(),hr(),
      h3(textOutput('dynamicText1')),
      
      h3(textOutput('dynamicText2')),
      
      conditionalPanel(
        condition = "input.selectsyndrome == 'Proximal 1q21.1 (RBM8A) DUP'",
        h3(textOutput('dup_prox_1q21_rosen_inci'))
      ),
      conditionalPanel(
        condition = "input.selectsyndrome == 'Proximal 1q21.1 (RBM8A) DUP'",
        h3(textOutput('comp_dup_prox_1q21_inci'))
      ),
      
      conditionalPanel(
        condition = "input.selectsyndrome == 'Distal 1q21.1 (GJA5) DEL'",
        h3(textOutput('del_dist_1q21_rosen_inci'))
      ),
      conditionalPanel(
        condition = "input.selectsyndrome == 'Distal 1q21.1 (GJA5) DEL'",
        h3(textOutput('comp_del_dist_1q21_inci'))
      ),
      
      conditionalPanel(
        condition = "input.selectsyndrome == 'Distal 1q21.1 (GJA5) DUP'",
        h3(textOutput('dup_dist_1q21_rosen_inci'))
      ),
      conditionalPanel(
        condition = "input.selectsyndrome == 'Distal 1q21.1 (GJA5) DUP'",
        h3(textOutput('comp_dup_dist_1q21_inci'))
      ),
      
      conditionalPanel(
        condition = "input.selectsyndrome == '16p13.11 (MYH11) DEL'",
        h3(textOutput('del_16p13_rosen_inci'))
      ),
      conditionalPanel(
        condition = "input.selectsyndrome == '16p13.11 (MYH11) DEL'",
        h3(textOutput('comp_del_16p13_inci'))
      ),
      
      conditionalPanel(
        condition = "input.selectsyndrome == '16p12.1 (CDR2) DEL'",
        h3(textOutput('del_16p12_rosen_inci'))
      ),
      conditionalPanel(
        condition = "input.selectsyndrome == '16p12.1 (CDR2) DEL'",
        h3(textOutput('comp_del_16p12_inci'))
      ),
      
      conditionalPanel(
        condition = "input.selectsyndrome == 'Distal 16p11.2 (SH2B1) DEL'",
        h3(textOutput('del_dist_16p11_rosen_inci'))
      ),
      conditionalPanel(
        condition = "input.selectsyndrome == 'Distal 16p11.2 (SH2B1) DEL'",
        h3(textOutput('comp_del_dist_16p11_inci'))
      ),
      
      conditionalPanel(
        condition = "input.selectsyndrome == 'Distal 16p11.2 (SH2B1) DUP'",
        h3(textOutput('dup_dist_16p11_rosen_inci'))
      ),
      conditionalPanel(
        condition = "input.selectsyndrome == 'Distal 16p11.2 (SH2B1) DUP'",
        h3(textOutput('comp_dup_dist_16p11_inci'))
      ),
      
      conditionalPanel(
        condition = "input.selectsyndrome == 'Proximal 16p11.2 (TBX6) DEL'",
        h3(textOutput('del_prox_16p11_rosen_inci'))
      ),
      conditionalPanel(
        condition = "input.selectsyndrome == 'Proximal 16p11.2 (TBX6) DEL'",
        h3(textOutput('comp_del_prox_16p11_inci'))
      ),
      
      conditionalPanel(
        condition = "input.selectsyndrome == 'Proximal 16p11.2 (TBX6) DUP'",
        h3(textOutput('dup_prox_16p11_rosen_inci'))
      ),
      conditionalPanel(
        condition = "input.selectsyndrome == 'Proximal 16p11.2 (TBX6) DUP'",
        h3(textOutput('comp_dup_prox_16p11_inci'))
      ),
      
      conditionalPanel(
        condition = "input.selectsyndrome == '17q12 (HNF1B) DEL'",
        h3(textOutput('del_17q12_rosen_inci'))
      ),
      conditionalPanel(
        condition = "input.selectsyndrome == '17q12 (HNF1B) DEL'",
        h3(textOutput('comp_del_17q12_inci'))
      ),
      
      conditionalPanel(
        condition = "input.selectsyndrome == '17q12 (HNF1B) DUP'",
        h3(textOutput('dup_17q12_rosen_inci'))
      ),
      conditionalPanel(
        condition = "input.selectsyndrome == '17q12 (HNF1B) DUP'",
        h3(textOutput('comp_dup_17q12_inci'))
      ),
      
      conditionalPanel(
        condition = "input.selectsyndrome == '22q11.2 (TBX1) DUP'",
        h3(textOutput('dup_22q11_rosen_inci'))
      ),
      conditionalPanel(
        condition = "input.selectsyndrome == '22q11.2 (TBX1) DUP'",
        h3(textOutput('comp_dup_22q11_inci'))
      ),
      br(),hr()
      
    )# end main panel
    
  )# end sidebarlayout
)


# Define server logic
server <- function(input, output) {
      
  output$dynamicText1 <- renderText({
    sprintf('Selected syndrome: %s', input$selectsyndrome)
  })

  output$dynamicText2 <- renderText({
    sprintf('Your incidence: %s', round(input$withcnv / input$total, digits = 4))
  })
  

  #incidences
  output$dup_prox_1q21_rosen_inci <-renderText({
    sprintf('Rosenfeld incidence: %s', round(dup_prox_1q21_rosen_inci, digits = 4))
  })
  output$comp_dup_prox_1q21_inci <-renderText({
    comp_inci <- matrix(c(dup_prox_1q21_rosen_with, input$withcnv, dup_prox_1q21_rosen_without, input$total), 2, 2)
    comp <- exact2x2(comp_inci, tsmethod = "minlike")
    for(i in 1:length(comp)){
      ##first extract the object value
      tempobj=comp[[i]]
      ##now create a new variable with the original name of the list item
      eval(parse(text=paste(names(comp)[[i]],"= tempobj")))
    }
    sprintf('Your incidence compared to Rosenfeld (where p < 0.05 indicates a difference), 
            Fisher\'s exact test p-value: %s', round(p.value, digits = 4))
  })  
  
  output$del_dist_1q21_rosen_inci <-renderText({
    sprintf('Rosenfeld incidence: %s', round(del_dist_1q21_rosen_inci, digits = 4))
  })
  output$comp_del_dist_1q21_inci <-renderText({
    comp_inci <- matrix(c(del_dist_1q21_rosen_with, input$withcnv, del_dist_1q21_rosen_without, input$total), 2, 2)
    comp <- exact2x2(comp_inci, tsmethod = "minlike")
    for(i in 1:length(comp)){
      ##first extract the object value
      tempobj=comp[[i]]
      ##now create a new variable with the original name of the list item
      eval(parse(text=paste(names(comp)[[i]],"= tempobj")))
    }
    sprintf('Your incidence compared to Rosenfeld (where p < 0.05 indicates a difference), 
            Fisher\'s exact test p-value: %s', round(p.value, digits = 4))
  })  

  output$dup_dist_1q21_rosen_inci <-renderText({
    sprintf('Rosenfeld incidence: %s', round(dup_dist_1q21_rosen_inci, digits = 4))
  })
  output$comp_dup_dist_1q21_inci <-renderText({
    comp_inci <- matrix(c(dup_dist_1q21_rosen_with, input$withcnv, dup_dist_1q21_rosen_without, input$total), 2, 2)
    comp <- exact2x2(comp_inci, tsmethod = "minlike")
    for(i in 1:length(comp)){
      ##first extract the object value
      tempobj=comp[[i]]
      ##now create a new variable with the original name of the list item
      eval(parse(text=paste(names(comp)[[i]],"= tempobj")))
    }
    sprintf('Your incidence compared to Rosenfeld (where p < 0.05 indicates a difference), 
            Fisher\'s exact test p-value: %s', round(p.value, digits = 4))
  })

  output$del_16p13_rosen_inci <-renderText({
    sprintf('Rosenfeld incidence: %s', round(del_16p13_rosen_inci, digits = 4))
  })
  output$comp_del_16p13_inci <-renderText({
    comp_inci <- matrix(c(del_16p13_rosen_with, input$withcnv, del_16p13_rosen_without, input$total), 2, 2)
    comp <- exact2x2(comp_inci, tsmethod = "minlike")
    for(i in 1:length(comp)){
      ##first extract the object value
      tempobj=comp[[i]]
      ##now create a new variable with the original name of the list item
      eval(parse(text=paste(names(comp)[[i]],"= tempobj")))
    }
    sprintf('Your incidence compared to Rosenfeld (where p < 0.05 indicates a difference), 
            Fisher\'s exact test p-value: %s', round(p.value, digits = 4))
  })
  
  output$del_16p12_rosen_inci <-renderText({
    sprintf('Rosenfeld incidence: %s', round(del_16p12_rosen_inci, digits = 4))
  })
  output$comp_del_16p12_inci <-renderText({
    comp_inci <- matrix(c(del_16p12_rosen_with, input$withcnv, del_16p12_rosen_without, input$total), 2, 2)
    comp <- exact2x2(comp_inci, tsmethod = "minlike")
    for(i in 1:length(comp)){
      ##first extract the object value
      tempobj=comp[[i]]
      ##now create a new variable with the original name of the list item
      eval(parse(text=paste(names(comp)[[i]],"= tempobj")))
    }
    sprintf('Your incidence compared to Rosenfeld (where p < 0.05 indicates a difference), 
            Fisher\'s exact test p-value: %s', round(p.value, digits = 4))
  })
  
  output$del_dist_16p11_rosen_inci <-renderText({
    sprintf('Rosenfeld incidence: %s', round(del_dist_16p11_rosen_inci, digits = 4))
  })
  output$comp_del_dist_16p11_inci <-renderText({
    comp_inci <- matrix(c(del_dist_16p11_rosen_with, input$withcnv, del_dist_16p11_rosen_without, input$total), 2, 2)
    comp <- exact2x2(comp_inci, tsmethod = "minlike")
    for(i in 1:length(comp)){
      ##first extract the object value
      tempobj=comp[[i]]
      ##now create a new variable with the original name of the list item
      eval(parse(text=paste(names(comp)[[i]],"= tempobj")))
    }
    sprintf('Your incidence compared to Rosenfeld (where p < 0.05 indicates a difference), 
            Fisher\'s exact test p-value: %s', round(p.value, digits = 4))
  })
  
  output$dup_dist_16p11_rosen_inci <-renderText({
    sprintf('Rosenfeld incidence: %s', round(dup_dist_16p11_rosen_inci, digits = 4))
  })
  output$comp_dup_dist_16p11_inci <-renderText({
    comp_inci <- matrix(c(dup_dist_16p11_rosen_with, input$withcnv, dup_dist_16p11_rosen_without, input$total), 2, 2)
    comp <- exact2x2(comp_inci, tsmethod = "minlike")
    for(i in 1:length(comp)){
      ##first extract the object value
      tempobj=comp[[i]]
      ##now create a new variable with the original name of the list item
      eval(parse(text=paste(names(comp)[[i]],"= tempobj")))
    }
    sprintf('Your incidence compared to Rosenfeld (where p < 0.05 indicates a difference), 
            Fisher\'s exact test p-value: %s', round(p.value, digits = 4))
  })
  
  output$del_prox_16p11_rosen_inci <-renderText({
    sprintf('Rosenfeld incidence: %s', round(del_prox_16p11_rosen_inci, digits = 4))
  })
  output$comp_del_prox_16p11_inci <-renderText({
    comp_inci <- matrix(c(del_prox_16p11_rosen_with, input$withcnv, del_prox_16p11_rosen_without, input$total), 2, 2)
    comp <- exact2x2(comp_inci, tsmethod = "minlike")
    for(i in 1:length(comp)){
      ##first extract the object value
      tempobj=comp[[i]]
      ##now create a new variable with the original name of the list item
      eval(parse(text=paste(names(comp)[[i]],"= tempobj")))
    }
    sprintf('Your incidence compared to Rosenfeld (where p < 0.05 indicates a difference), 
            Fisher\'s exact test p-value: %s', round(p.value, digits = 4))
  })
  
  output$dup_prox_16p11_rosen_inci <-renderText({
    sprintf('Rosenfeld incidence: %s', round(dup_prox_16p11_rosen_inci, digits = 4))
  })
  output$comp_dup_prox_16p11_inci <-renderText({
    comp_inci <- matrix(c(dup_prox_16p11_rosen_with, input$withcnv, dup_prox_16p11_rosen_without, input$total), 2, 2)
    comp <- exact2x2(comp_inci, tsmethod = "minlike")
    for(i in 1:length(comp)){
      ##first extract the object value
      tempobj=comp[[i]]
      ##now create a new variable with the original name of the list item
      eval(parse(text=paste(names(comp)[[i]],"= tempobj")))
    }
    sprintf('Your incidence compared to Rosenfeld (where p < 0.05 indicates a difference), 
            Fisher\'s exact test p-value: %s', round(p.value, digits = 4))
  }) 

  output$del_17q12_rosen_inci <-renderText({
    sprintf('Rosenfeld incidence: %s', round(del_17q12_rosen_inci, digits = 4))
  })
  output$comp_del_17q12_inci <-renderText({
    comp_inci <- matrix(c(del_17q12_rosen_with, input$withcnv, del_17q12_rosen_without, input$total), 2, 2)
    comp <- exact2x2(comp_inci, tsmethod = "minlike")
    for(i in 1:length(comp)){
      ##first extract the object value
      tempobj=comp[[i]]
      ##now create a new variable with the original name of the list item
      eval(parse(text=paste(names(comp)[[i]],"= tempobj")))
    }
    sprintf('Your incidence compared to Rosenfeld (where p < 0.05 indicates a difference), 
            Fisher\'s exact test p-value: %s', round(p.value, digits = 4))
  })

  output$dup_17q12_rosen_inci <-renderText({
    sprintf('Rosenfeld incidence: %s', round(dup_17q12_rosen_inci, digits = 4))
  })
  output$comp_dup_17q12_inci <-renderText({
    comp_inci <- matrix(c(dup_17q12_rosen_with, input$withcnv, dup_17q12_rosen_without, input$total), 2, 2)
    comp <- exact2x2(comp_inci, tsmethod = "minlike")
    for(i in 1:length(comp)){
      ##first extract the object value
      tempobj=comp[[i]]
      ##now create a new variable with the original name of the list item
      eval(parse(text=paste(names(comp)[[i]],"= tempobj")))
    }
    sprintf('Your incidence compared to Rosenfeld (where p < 0.05 indicates a difference), 
            Fisher\'s exact test p-value: %s', round(p.value, digits = 4))
  })

  output$dup_22q11_rosen_inci <-renderText({
    sprintf('Rosenfeld incidence: %s', round(dup_22q11_rosen_inci, digits = 4))
  })
  output$comp_dup_22q11_inci <-renderText({
    comp_inci <- matrix(c(dup_22q11_rosen_with, input$withcnv, dup_22q11_rosen_without, input$total), 2, 2)
    comp <- exact2x2(comp_inci, tsmethod = "minlike")
    for(i in 1:length(comp)){
      ##first extract the object value
      tempobj=comp[[i]]
      ##now create a new variable with the original name of the list item
      eval(parse(text=paste(names(comp)[[i]],"= tempobj")))
    }
    sprintf('Your incidence compared to Rosenfeld (where p < 0.05 indicates a difference), 
            Fisher\'s exact test p-value: %s', round(p.value, digits = 4))
  })
  
  
  
  
  output$coolplot <- renderPlot({
    plot(rnorm(input$total[1]))
  })
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)

