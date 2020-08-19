### Linear A dashboard server page


library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)

#doc id and the symbols per document as a single string per doc
df1 <- read_csv("linear_A_docs_text.csv")

# Individual symbols from entire corpus as a character string 
df2 <- unlist(strsplit(df1$text, ""))

##frequency table of all symbols
df3 <- data.frame(table(df2))
#order the symbols by frequency
df3 <- df3[order(-df3$Freq),]
#lock in the symbols as factor order to assist with ordering.
df3$df2 <- factor(df3$df2, levels = df3$df2)

dfPair <- read_csv("all_pairs.csv") #has unk
dfPair$Var1 <- factor(dfPair$Var1, levels = dfPair$Var1)

dfPairUnk <-  read_csv("all_pairs_filt.csv")# no unks
dfPairUnk$Var1 <- factor(dfPairUnk$Var1, levels = dfPairUnk$Var1)

dfTrio <- read_csv("all_trios.csv") #has unk
dfTrio$Var1 <- factor(dfTrio$Var1, levels = dfTrio$Var1)

dfTrioUnk <- read_csv("all_trios_filt.csv") #no unks
dfTrioUnk$Var1 <- factor(dfTrioUnk$Var1, levels = dfTrioUnk$Var1)

entropy_table <- read_csv("entropy_table.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Analysis of Linear A"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Entropy", tabName = "entropy", icon = icon("dashboard")),
      menuItem("Frequent Symbol", tabName = "fs1", icon = icon("glasses")),
      menuItem("Topic Modeling 1", tabName = "tm1", icon = icon("grip-horizontal")),
      #,
      menuItem("Conditional Probabilities", tabName = "cp1", icon = icon("grip-horizontal"))
      ),
     menuItem("Project Write Up", tabName = "writeup", icon = icon("file-pdf"))

    #we need to also plot 15 cluster csv
    #axis the categories by name and y axis the clustering value
    #### have symbol parsing page.  options for chosing which comparison to use. 
    #### the explanation of each comparison will change based on the choices of the user.
    #### Have comparison side by side or above below.  
    #### figure out how to highlight the maximum value in each row.
    
  ),
  page1 <- dashboardBody(
    tabItems(
      tabItem(tabName = "entropy",
    fluidRow(
      box(tableOutput('ent_table'),
          title = 'Entropy of Linear A'
          ),
      box(sliderInput( "sym", label = "Number of Symbols:",
                       min = 1, max= 250, value = 35),
          fluidRow( 
            box(radioButtons('symbolDistIn1', label = 'Symbol Distribution 1', 
                       choices = c("Single Symbol" = "SinSym", "Single No unknown" = "SinNoUnk",
                                   "Symbol Pairs" = "SymPairs", "Symbol Pairs No unk" = "SymPairNoUnk",
                                   "Symbol Trios" = "SymTrio", "Symbol Trios No unk" = "SymTrioNoUnk"))),
            box(radioButtons('symbolDistIn2', label = 'Symbol Distribution 2', 
                       choices = c("Single Symbol" = "SinSym", "Single No unknown" = "SinNoUnk",
                                   "Symbol Pairs" = "SymPairs", "Symbol Pairs No unk" = "SymPairNoUnk",
                                   "Symbol Trios" = "SymTrio", "Symbol Trios No unk" = "SymTrioNoUnk")))
            )
          )
                       
          ),
      
    fluidRow(
      box(plotOutput('DistPlot1'), title = "Distribution of Linerar A symbols 1"),
          
                    
      box(plotOutput('DistPlot2'), title = 'Distribution of Linerar A Symbols 2'))
  ),
  tabItem(tabName = "tm1", h2("Topic Modeling"),
          fluidRow(box(radioButtons("tm_compare", label = "Model Grouping Comparison ",
                                    choices = c(   "Academic ~ k-means 5 topics" = "AKM_5",
                                                   "Topic Model ~ Academic 15 Topics" = "ATM_15",
                                                   "Topic Model ~ Academic 5 Topics" = "ATM_5",
                                                   "Topic Model ~ k-Means 5 Topics" = "TMKM_5",
                                                   "Topic Model ~ k-Means 12 Topics" = "TMKM_12")))
          ),
          fluidRow(
            box(tableOutput('tm_table')),
            box(tableOutput('doclegend'),
                title = 'Document Grouping Codes'
            )
          )),
  tabItem(tabName = "cp1", h5("Future extention: 
Create a page that has conditional probabilities of the various pairs and trios of n-grams allow the user to choose the number of possible choices for each n-gram.
Create a way for the user to choose which symbol to look at.Look at both solving for the before and after sequence of unknown symbols. 
                              Visualize the probabilites in a powerful way.")
          ),
  
  tabItem(tabName = "fs1", h4(paste("A problem with the Linear A corpus is the high number of \n
                              unreadable symbols.", signif(df3$Freq[1]/sum(df3$Freq),4)*100, "percent of the corpus is unreadable")),
          fluidRow(
            box(sliderInput("fs_n", label = "Number of Rows in Tables:",
                            min = 1, max = 35, value = 5)),
            box(radioButtons("fs_button1", label = "Table 1",
                             choices = c("Common n-gram" = "common_n",
                                         "Following Unknown Symbol" = "after_pair",
                                         "Unknown Following Symbol" = "before_pair"
                             )
                             
            ),
            radioButtons("fs_button2", label = "Table 2",
                         choices = c("Common n-gram" = "common_n",
                                     "Following Unknown Symbol" = "after_pair",
                                     "Unknown Following Symbol" = "before_pair"
                         )
                          
            ))
                
                ),
          fluidRow(
            tableOutput('fs_table_1')
          ),
          fluidRow(
            tableOutput('fs_table_2')
          )
          ),
    tabItem(tabName = "writeup", h2( "Project write up" ),
            fluidRow(h6("add the PDF document from the repository here") ))
)
)
)
server <- function(input, output){
  common_n_grams <- read_csv("Common_n_grams.csv")
  before_pair_df <- read_csv("before_pair_df.csv")
  after_pair_df <- read_csv("after_pair_df.csv")
  legend1 <- readxl::read_xlsx("montecci_linearA_groupings.xlsx", sheet = 2)
  legend2 <- readxl::read_xlsx("montecci_linearA_groupings.xlsx", sheet = 3)
  #topic model output comparisons
  AKM_5_dat <- read_csv("academic_to_kmeans_5.csv")
  ATM_15_dat <- read_csv("tm_academic_full.csv")
  ATM_5_dat <- read_csv("tm_academic_condensed.csv")
  TMKM_5_dat<- read_csv("tm_kmeans_5.csv")
  TMKM_12_dat <- read_csv("tm_kmeans_12.csv")
  
  
  output$tm_table <- renderTable(({
    tm_compare <- switch(input$tm_compare,
                         AKM_5 = print(AKM_5_dat),
                         ATM_15  = print(ATM_15_dat),
                         ATM_5 = print(ATM_5_dat),
                         TMKM_5 = print(TMKM_5_dat),
                         TMKM_12 = print(TMKM_12_dat)
                         )
  }))
  
  
  output$doclegend <- renderTable({
    tm_compare <- switch(input$tm_compare,
                         AKM_5 = print(legend2),
                         ATM_15  = print(legend1),
                         ATM_5 = print(legend2),
                         TMKM_5 =  print(data.frame(NULL)),
                         TMKM_12 =  print(data.frame(NULL))
    )
  })
   
  output$ent_table <-  renderTable(print(entropy_table))
  
  output$fs_table_1 <- renderTable({
    n_lines = input$fs_n
    t1 = head(common_n_grams, n = n_lines)#    n_gram_tab1 <- head(common_n_grams, n = n_lines)
    t2 = head( before_pair_df, n = n_lines)
    t3 = head( after_pair_df, n = n_lines)
    
    fs_button1 <- switch(input$fs_button1,
                         common_n = print(t1),
                         after_pair = print(t2),
                         before_pair = print(t3)
                         )
#    
#    n_gramIn1 <- switch (input$n_gramIn1, 
#                         common_n = print(n_gram_tab1))
  })
  output$fs_table_2 <- renderTable({
    n_lines = input$fs_n
    t1 = head(common_n_grams, n = n_lines)#    n_gram_tab1 <- head(common_n_grams, n = n_lines)
    t2 = head( before_pair_df, n = n_lines)
    t3 = head( after_pair_df, n = n_lines)
    
    fs_button2 <- switch(input$fs_button2,
                         common_n = print(t1),
                         after_pair = print(t2),
                         before_pair = print(t3)
    )
    #    n_gram_tab1 <- head(common_n_grams, n = n_lines)
    #    
    #    n_gramIn1 <- switch (input$n_gramIn1, 
    #                         common_n = print(n_gram_tab1))
  })
  
  output$DistPlot1 <- renderPlot({
    numlines = input$sym
    p1 <- ggplot(df3[1:numlines,], aes(x = df2, y = Freq) )+
      geom_bar(stat = "identity")+
      xlab("Linear A symbol")+
      ylab("Frequency")+
      theme(
        panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
        axis.text.x = element_text(family = "Noto Sans Linear A")
      )
    p2 <- ggplot(df3[2:numlines,], aes(x = df2, y = Freq) )+
      geom_bar(stat = "identity")+
      xlab("Linear A symbol")+
      ylab("Frequency")+
      theme(
        panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
      axis.text.x = element_text(family = "Noto Sans Linear A")
      )
    p3 <- ggplot(dfPair[1:numlines,], aes(x = Var1, y = Freq) )+
      geom_bar(stat = "identity")+
      xlab("Linear A symbol")+
      ylab("Frequency")+
      theme(
        panel.background = element_rect(fill = "lightgreen",
                                        colour = "lightgreen",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
        axis.text.x = element_text(family = "Noto Sans Linear A")
      )
    p4 <- ggplot(dfPairUnk[1:numlines,], aes(x = Var1, y = Freq) )+
      geom_bar(stat = "identity")+
      xlab("Linear A symbol")+
      ylab("Frequency")+
      theme(
        panel.background = element_rect(fill = "lightgreen",
                                        colour = "lightgreen",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
        axis.text.x = element_text(family = "Noto Sans Linear A")
      )
    p5 <- ggplot(dfTrio[1:numlines,], aes(x = Var1, y = Freq) )+
      geom_bar(stat = "identity")+
      xlab("Linear A symbol")+
      ylab("Frequency")+
      theme(
        panel.background = element_rect(fill = "plum2",
                                        colour = "plum2",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
        axis.text.x = element_text(family = "Noto Sans Linear A")
      )
    p6 <- ggplot(dfTrioUnk[1:numlines,], aes(x = Var1, y = Freq) )+
      geom_bar(stat = "identity")+
      xlab("Linear A symbol")+
      ylab("Frequency")+
      theme(
        panel.background = element_rect(fill = "plum2",
                                        colour = "plum2",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
        axis.text.x = element_text(family = "Noto Sans Linear A")
      )
  
    symbolDistIn1 <- switch(input$symbolDistIn1,
                            SinSym = print(p1),
                              SinNoUnk = print(p2),
                              SymPairs = print(p3),
                              SymPairNoUnk = print(p4),
                              SymTrio = print(p5),
                              SymTrioNoUnk = print(p6)
                            )
    
    

  })
  output$DistPlot2 <- renderPlot({
    numlines = input$sym
    p1 <- ggplot(df3[1:numlines,], aes(x = df2, y = Freq) )+
      geom_bar(stat = "identity")+
      xlab("Linear A symbol")+
      ylab("Frequency")+
      theme(
        panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
        axis.text.x = element_text(family = "Noto Sans Linear A")
      )
    p2 <- ggplot(df3[2:numlines,], aes(x = df2, y = Freq) )+
      geom_bar(stat = "identity")+
      xlab("Linear A symbol")+
      ylab("Frequency")+
      theme(
        panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
        axis.text.x = element_text(family = "Noto Sans Linear A")
      )
    p3 <- ggplot(dfPair[1:numlines,], aes(x = Var1, y = Freq) )+
      geom_bar(stat = "identity")+
      xlab("Linear A symbol")+
      ylab("Frequency")+
      theme(
        panel.background = element_rect(fill = "lightgreen",
                                        colour = "lightgreen",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
        axis.text.x = element_text(family = "Noto Sans Linear A")
      )
    p4 <- ggplot(dfPairUnk[1:numlines,], aes(x = Var1, y = Freq) )+
      geom_bar(stat = "identity")+
      xlab("Linear A symbol")+
      ylab("Frequency")+
      theme(
        panel.background = element_rect(fill = "lightgreen",
                                        colour = "lightgreen",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
        axis.text.x = element_text(family = "Noto Sans Linear A")
      )
    p5 <- ggplot(dfTrio[1:numlines,], aes(x = Var1, y = Freq) )+
      geom_bar(stat = "identity")+
      xlab("Linear A symbol")+
      ylab("Frequency")+
      theme(
        panel.background = element_rect(fill = "plum2",
                                        colour = "plum2",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
        axis.text.x = element_text(family = "Noto Sans Linear A")
      )
    p6 <- ggplot(dfTrioUnk[1:numlines,], aes(x = Var1, y = Freq) )+
      geom_bar(stat = "identity")+
      xlab("Linear A symbol")+
      ylab("Frequency")+
      theme(
        panel.background = element_rect(fill = "plum2",
                                        colour = "plum2",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),axis.text.x = element_text(family = "Noto Sans Linear A")
      )
    
    symbolDistIn2 <- switch(input$symbolDistIn2,
                            SinSym = print(p1),
                              SinNoUnk = print(p2),
                              SymPairs = print(p3),
                              SymPairNoUnk = print(p4),
                              SymTrio = print(p5),
                              SymTrioNoUnk = print(p6)
                            )
    
  })

}
shinyApp(ui,server)


