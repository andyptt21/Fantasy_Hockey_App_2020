library(shiny)
library(plotrix)
library(RColorBrewer)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(leaflet)
library(plotly)
#library(reticulate)

# For shiny.io only
#use_python("/usr/bin/python3/")

Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoiYW5keXB0dDIxIiwiYSI6ImNqcmM3YTc4MzA2d3Q0M3MxN3Ziazkwd3kifQ.7mUkjFMzgz7F4TlEorW2Mg')
setwd("~/Documents/Fantasy_App_2020/")
load("week3.Rda")

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Nisky & Friends VII"),
                    dashboardSidebar(sidebarMenu(
                      menuItem("Power Rankings", tabName = "Upload_File", icon = icon("flash")),
                      menuItem("This Week's Results", tabName = "weekly_results", icon = icon("calendar")),
                      menuItem("Seasonal Stats", tabName = "Rankings", icon = icon("bar-chart"))
                    )),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                      ),
                      tabItems(
                      tabItem(tabName = "Upload_File",
                              fluidRow(box(width = 12,
                                           title = strong("Power Rankings"),
                                           solidHeader = T,
                                           status = "primary",
                                           sidebarLayout(sidebarPanel(
                                             selectInput("power_rank_plot_selection","Category",choices = c("Power Rankings",
                                                                                                            "Standings",
                                                                                                            "Power Rank vs. Standing",
                                                                                                            "Skater Rank",
                                                                                                            "Goalie Rank"),
                                                         selected = "Power Rank vs. Standing"),
                                             tableOutput('power_rankings_table')
                                           ),
                                           mainPanel(
                                               plotlyOutput('power_rankings_barplot',height = "500px"),
                                               actionButton("saveSeasonPowerPlot", "Save Seasonal Ranking Image")
                                           )
                                           ))),
                              fluidRow(box(width = 12,
                                           title = strong("Seasonal Heatmap"),
                                           solidHeader = T,
                                           status = "primary",
                                           column(width=12,
                                                  plotOutput('seasonalHeatmap',height='700px'))
                                           )),
                              fluidRow(
                              box(width = 12,
                                  title = strong("Leaderboard"),
                                  solidHeader = T,
                                  status = "primary",
                                          fluidRow(
                                            #column(width=8,offset=1,tableOutput('contents'))
                                            #titlePanel("Rankings Breakdown"),
                                            column(width=12,
                                              dataTableOutput('category_rankings')
                                            )
                                          )
                                             ))
                      ),
                      tabItem(tabName = "Rankings",
                              tags$head(
                                tags$style(type="text/css", ".dataTables_filter {display: none;    }"
                                )
                              ),
                              fluidRow(box(width = 12,
                                           title = strong("Team Barplots"),
                                           solidHeader = T,
                                           status = "primary",
                              sidebarLayout(
                                sidebarPanel(width = 4,
                                             selectInput('team_selection',"Pick your team",choices = as.character(dataframe_z[,1]))
                                ),
                                mainPanel(
                                  plotlyOutput('team_barplot',height = "500px")
                                )
                              ))),
                              fluidRow(box(width = 12,
                                           title = strong("Category Standings"),
                                           solidHeader = T,
                                           status = "primary",
                                           sidebarLayout(
                                             sidebarPanel(width = 4,
                                                          selectInput('cat_selection',"Scoring Category",
                                                                      choices = colnames(season_data))
                                             ),
                                             mainPanel(
                                               plotlyOutput('cat_barplot',height = "500px")
                                             )
                                           ))),
                              fluidRow(box(width = 12,
                                           title = strong("Category Z Score Distribution"),
                                           solidHeader = T,
                                           status = "primary",
                              fluidRow(
                                column(width = 12,
                                plotlyOutput('total_boxplot',height = "500px")
                              ))))

                      ),
                      tabItem(tabName = "weekly_results",
                              fluidRow(box(width = 12,
                                           title = strong("This Week's Results"),
                                           solidHeader = T,
                                           status = "primary",
                              sidebarLayout(sidebarPanel(tableOutput('weekly_power_table')),
                                            mainPanel(plotlyOutput('weekly_power_barplot',height='500px'),
                                                      actionButton("saveWeeklyPowerPlot", "Save Weekly Ranking Image"))))),
                              fluidRow(box(width = 12,
                                           title = strong("Weekly Heatmap"),
                                           solidHeader = T,
                                           status = "primary",
                                           column(width=12,
                                                  plotOutput('weeklyHeatmap',height='700px'))
                                           )),
                              fluidRow(box(width = 12,
                                           title = strong("Weekly Team Barplots"),
                                           solidHeader = T,
                                           status = "primary",
                              sidebarLayout(sidebarPanel(selectInput('weeklyTeamSelect','Select your team',choices=rownames(weekly_team_z))),
                                            mainPanel(plotlyOutput('weeklyTeamBarplot',height = "500px")))))
                              )
                      )
                    )
)

server <- function(input, output,session) {

  teamSelected <- reactive({
    match(input$team_selection,rownames(season_data))
  })

  catSelected <- reactive({
    match(input$cat_selection,colnames(season_data))
  })

  output$contents <- renderTable(
    season_data, hover=TRUE
  )

  output$z_table <- renderDataTable(
    dataframe_z,options=list(paging=FALSE,searching=FALSE)
  )

  output$total_boxplot <- renderPlotly({
    z_2<-dataframe_z
    rownames(z_2)<-dataframe_z[,1]
    z_2[,ncol(z_2)] = -as.numeric(z_2[,ncol(z_2)])
    z_2<-z_2[,-1]
    boxplot_list<-list()
    for(i in 1:nrow(z_2)){
      boxplot_list[[i]]<-as.numeric(z_2[i,])
    }
    boxplot_list_medians<-c()
    for(i in 1:length(boxplot_list)){
      boxplot_list_medians[i]<-median(boxplot_list[[i]])
    }
    boxplot_list<-boxplot_list[order(boxplot_list_medians,decreasing = TRUE)]
    # par(mar=c(15,6,1,1))
    # boxplot(boxplot_list, xaxt='n',col=teamColors[order(boxplot_list_medians,decreasing = TRUE)])
    # labels<-rownames(z_2)[order(boxplot_list_medians,decreasing = TRUE)]
    # text(seq(1, nrow(z_2)), par("usr")[3] - 0.2, labels = labels,
    #      srt = 60, pos = 2, xpd = TRUE,cex=1.2)
    # abline(h=0,lty=2,lwd=2)
    boxplot
    p <- plot_ly(type = "box") %>%
      add_boxplot(y = ~boxplot_list[[1]],name = rownames(z_2)[order(boxplot_list_medians,decreasing = TRUE)][1]) %>%
      add_boxplot(y = ~boxplot_list[[2]],name = rownames(z_2)[order(boxplot_list_medians,decreasing = TRUE)][2]) %>%
      add_boxplot(y = ~boxplot_list[[3]],name = rownames(z_2)[order(boxplot_list_medians,decreasing = TRUE)][3]) %>%
      add_boxplot(y = ~boxplot_list[[4]],name = rownames(z_2)[order(boxplot_list_medians,decreasing = TRUE)][4]) %>%
      add_boxplot(y = ~boxplot_list[[5]],name = rownames(z_2)[order(boxplot_list_medians,decreasing = TRUE)][5]) %>%
      add_boxplot(y = ~boxplot_list[[6]],name = rownames(z_2)[order(boxplot_list_medians,decreasing = TRUE)][6]) %>%
      add_boxplot(y = ~boxplot_list[[7]],name = rownames(z_2)[order(boxplot_list_medians,decreasing = TRUE)][7]) %>%
        add_boxplot(y = ~boxplot_list[[8]],name = rownames(z_2)[order(boxplot_list_medians,decreasing = TRUE)][8]) %>%
              add_boxplot(y = ~boxplot_list[[9]],name = rownames(z_2)[order(boxplot_list_medians,decreasing = TRUE)][9]) %>%
      layout(title = "",
             xaxis = list(title = "",tickangle = 45),
             yaxis = list(title = "Z Score"),
             margin = list(b = 160, r = 100))
  })

  output$team_barplot <- renderPlotly({
    z_2<-dataframe_z
    rownames(z_2)<-dataframe_z[,1]
    z_2[,ncol(z_2)] = -as.numeric(z_2[,ncol(z_2)])
    z_2<-z_2[,-1]
    colors<-c()
    for(i in 1:ncol(z_2)){
      if(as.numeric(z_2[as.numeric(teamSelected()),])[i]>0){
        colors<-c(colors,"red")
      } else {
        colors<-c(colors,"blue")
      }
    }
    team_frame<-data.frame(colnames(z_2),as.numeric(z_2[as.numeric(teamSelected()),]))
    colnames(team_frame)<-c("Category","Value")
    team_frame$Category<-factor(team_frame$Category,levels = team_frame$Category)
    p <- plot_ly(team_frame,
                 x = ~Category, y = ~Value,
                 type = 'bar',
                 marker = list(color = colors)) %>%
      layout(title = rownames(z_2)[as.numeric(teamSelected())],
             shapes=list(type='line', x0= 10.51, x1= 10.51, y0=-2, y1=2, line=list(width=1)),
             xaxis = list(title = "",tickangle = 45),
             yaxis = list(title = "Z score")#,
             #margin = list(b = 160, r = 100)
             )

  })

  output$cat_barplot <- renderPlotly({
    z_2<-dataframe_z
    rownames(z_2)<-dataframe_z[,1]
    z_2[,ncol(z_2)] = -as.numeric(z_2[,ncol(z_2)])
    z_2<-z_2[,-1]
    cat_frame<-data.frame(rownames(z_2)[order(as.numeric(z_2[,as.numeric(catSelected())]),decreasing = TRUE)],sort(as.numeric(z_2[,as.numeric(catSelected())]),decreasing = TRUE))
    colnames(cat_frame)<-c("Team","Value")
    cat_frame$Team<-factor(cat_frame$Team,levels = cat_frame$Team)
    p <- plot_ly(cat_frame,
                 x = ~Team, y = ~Value,
                 type = 'bar',
                 marker = list(color = gg_color_hue(12))
                 ) %>%
      layout(title = colnames(z_2)[as.numeric(catSelected())],
             xaxis = list(title = "",tickangle = 45),
             yaxis = list(title = "Z score"),
             margin = list(b = 160, r = 100)
             )
  })

  cat_rankings<-reactive({
    z_2<-dataframe_z
    z_2[,ncol(z_2)] = -as.numeric(z_2[,ncol(z_2)])
    z_2<-z_2[,-1]
    cat_rankings<-z_2
    for(i in 1:ncol(z_2)){
      cat_rankings[,i]<-as.numeric(rank(-as.numeric(z_2[,i]),ties.method = "average"))
    }
    cat_rankings_2<-matrix(0,ncol=ncol(z_2),nrow=nrow(z_2))
    for(i in 1:ncol(z_2)){
      for(j in 1:nrow(z_2)){
        cat_rankings_2[j,i]<-as.numeric(cat_rankings[j,i])
      }
    }
    means<-c()
    for(i in 1:nrow(cat_rankings_2)){
      means<-c(means,as.numeric(round(mean(as.numeric(cat_rankings_2[i,])),digits=2)))
    }
    skater_means<-c()
    for(i in 1:nrow(cat_rankings_2)){
      skater_means<-c(skater_means,as.numeric(round(mean(as.numeric(cat_rankings_2[i,1:11])),digits=2)))
    }
    goalie_means<-c()
    for(i in 1:nrow(cat_rankings_2)){
      goalie_means<-c(goalie_means,as.numeric(round(mean(as.numeric(cat_rankings_2[i,12:17])),digits=2)))
    }
    final_matrix<-data.frame(dataframe_z[,1],cat_rankings_2,skater_means,goalie_means,means)
    colnames(final_matrix)<-c("Team",colnames(z_2),"Skater Rank","Goaltending Rank","Average Rank")

    for(i in 2:19){
      final_matrix[,i]<-as.numeric(final_matrix[,i])
    }
    final_matrix
  })

  cat_rankings_brief<-reactive({
    records_order<-match(as.vector(cat_rankings()[,1]),names(records))
    records<-as.numeric(records)
    final_matrix<-cbind(cat_rankings()[,1],records[records_order],cat_rankings()[,c("Average Rank","Skater Rank","Goaltending Rank")])
    colnames(final_matrix)[c(1:3)]<-c("Team", "Points","Power Rank")
    final_matrix
  })

  output$category_rankings<-renderDataTable(
    cat_rankings_brief(),
    options = list(bPaginate=F)
  )

  power_rankings<-reactive({
    if(input$power_rank_plot_selection=="Power Rankings"||input$power_rank_plot_selection=="Power Rank vs. Standing"){
    z_2<-dataframe_z
    z_2[,ncol(z_2)] = -as.numeric(z_2[,ncol(z_2)])
    z_2<-z_2[,-1]
    rownames(z_2)<-rownames(season_data)
    cat_rankings<-z_2
    for(i in 1:ncol(z_2)){
      cat_rankings[,i]<-rank(-as.numeric(z_2[,i]),ties.method = "average")
    }
    means<-c()
    for(i in 1:nrow(cat_rankings)){
      means<-c(means,mean(as.numeric(cat_rankings[i,])))
    }
    means_ranking<-round(rank(means,ties.method = "average"),digits = 0)
    final_matrix<-matrix(ncol=1,nrow=length(means))
    final_matrix[,1]<-sort(means_ranking,decreasing = FALSE)
    final_matrix_2<-cbind(rownames(z_2)[order(means_ranking)],final_matrix)
    colnames(final_matrix_2)<-c("Team","Power Ranking")
    final_matrix_2
    } else if(input$power_rank_plot_selection=="Skater Rank"){
      power_rankings<-data.frame(cat_rankings()[,1],as.numeric(cat_rankings()[,"Skater Rank"]))
      power_rankings[,2]<-round(rank(as.numeric(power_rankings[,2]),ties.method = "average"),digits = 0)
      power_rankings<-power_rankings[order(power_rankings[,2],decreasing=FALSE),]
      colnames(power_rankings)<-c("Team","Skater Rank")
      as.matrix(power_rankings)
    } else if(input$power_rank_plot_selection=="Goalie Rank"){
      power_rankings<-data.frame(cat_rankings()[,1],as.numeric(cat_rankings()[,"Goaltending Rank"]))
      power_rankings[,2]<-round(rank(as.numeric(power_rankings[,2]),ties.method = "average"),digits = 0)
      power_rankings<-power_rankings[order(power_rankings[,2],decreasing=FALSE),]
      colnames(power_rankings)<-c("Team","Goalie Rank")
      as.matrix(power_rankings)
    }else if(input$power_rank_plot_selection=="Standings"){
      records_order<-match(as.vector(cat_rankings()[,1]),names(records))
      records<-as.numeric(records)
      power_rankings<-data.frame(cat_rankings()[,1],records[records_order])
      power_rankings[,2]<-round(rank(-as.numeric(power_rankings[,2]),ties.method = "average"),digits = 0)
      power_rankings<-power_rankings[order(power_rankings[,2],decreasing=FALSE),]
      colnames(power_rankings)<-c("Team","Standings Rank")
      as.matrix(power_rankings)
    }
  })


  output$power_rankings_table<-renderTable(
    power_rankings(),hover = TRUE
  )

    output$power_rankings_barplot<-renderPlotly({
    if(input$power_rank_plot_selection=="Power Rankings"){
      power_rankings<-data.frame(cat_rankings()[,1],as.numeric(cat_rankings()[,"Average Rank"]))
    }else if(input$power_rank_plot_selection=="Skater Rank"){
      power_rankings<-data.frame(cat_rankings()[,1],as.numeric(cat_rankings()[,"Skater Rank"]))
    }else if(input$power_rank_plot_selection=="Goalie Rank"){
      power_rankings<-data.frame(cat_rankings()[,1],as.numeric(cat_rankings()[,"Goaltending Rank"]))
    }
    if(input$power_rank_plot_selection=="Power Rankings"||input$power_rank_plot_selection=="Skater Rank"||input$power_rank_plot_selection=="Goalie Rank"){
        f1 <- list(
            family = "Arial, sans-serif",
            size = 18,
            color = "black")
        colnames(power_rankings)<-c("Team","AverageRank")
        power_rankings$Team<-factor(power_rankings$Team,
                                    levels = power_rankings$Team[order(power_rankings$AverageRank,decreasing = FALSE)])
        p <- plot_ly(power_rankings, x = ~Team, y = ~AverageRank, type = 'bar',
                     marker = list(color = gg_color_hue(12))) %>%
            layout(title = input$power_rank_plot_selection,
                   xaxis = list(title = "",tickangle = 45, tickfont=f1),
                   yaxis = list(title = "Average Rank"),
                   margin = list(b = 160, r = 100))

    }else if(input$power_rank_plot_selection=="Power Rank vs. Standing"){
      records_order<-match(as.vector(cat_rankings()[,1]),names(records))
      records<-as.numeric(records)
      power_rankings<-data.frame(cat_rankings()[,"Team"],as.numeric(cat_rankings()[,"Average Rank"]),records[records_order])
      colnames(power_rankings)<-c("Team","PowerRank","Points")
      power_rankings$Team<-factor(power_rankings$Team,
                                  levels = power_rankings$Team[order(power_rankings$Points,decreasing = TRUE)])

      p <- plot_ly(power_rankings, x = ~Points) %>%
        add_trace(y = ~PowerRank, text = ~Team,
                   marker = list(color = gg_color_hue(12), size = 20), showlegend = FALSE,
                  name = " ",mode='text',text = ~Team, textposition = 'middle right') %>%
        add_lines(y = ~fitted(loess(PowerRank ~ Points)),
                  line = list(color = 'black'), name = "Loess Smoother",
                  showlegend = FALSE) %>%
        layout(title = input$power_rank_plot_selection,
               xaxis = list(title = "Points"),
               yaxis = list(title = "Power Rank")#,
               #margin = list(b = 160, r = 100)
               )

    }else if(input$power_rank_plot_selection=="Standings"){
        f1 <- list(
            family = "Arial, sans-serif",
            size = 18,
            color = "black")
        records_order<-match(as.vector(cat_rankings()[,1]),names(records))
        records<-as.numeric(records)
        power_rankings<-data.frame(cat_rankings()[,1],records[records_order])
        colnames(power_rankings)<-c("Team","AverageRank")
        power_rankings$Team<-factor(power_rankings$Team,
                                    levels = power_rankings$Team[order(power_rankings$AverageRank,decreasing = TRUE)])
        p <- plot_ly(power_rankings, x = ~Team, y = ~AverageRank, type = 'bar',
                     marker = list(color = gg_color_hue(12))) %>%
            layout(title = input$power_rank_plot_selection,
                   xaxis = list(title = "",tickangle = 45,tickfont=f1),
                   yaxis = list(title = "Average Rank"),
                   margin = list(b = 160, r = 100))
    }
  })

  observeEvent(input$saveSeasonPowerPlot, {
          if(input$power_rank_plot_selection=="Power Rankings"){
      power_rankings<-data.frame(cat_rankings()[,1],as.numeric(cat_rankings()[,"Average Rank"]))
    }else if(input$power_rank_plot_selection=="Skater Rank"){
      power_rankings<-data.frame(cat_rankings()[,1],as.numeric(cat_rankings()[,"Skater Rank"]))
    }else if(input$power_rank_plot_selection=="Goalie Rank"){
      power_rankings<-data.frame(cat_rankings()[,1],as.numeric(cat_rankings()[,"Goaltending Rank"]))
    }
    if(input$power_rank_plot_selection=="Power Rankings"||input$power_rank_plot_selection=="Skater Rank"||input$power_rank_plot_selection=="Goalie Rank"){
        f1 <- list(
            family = "Arial, sans-serif",
            size = 18,
            color = "black")
        colnames(power_rankings)<-c("Team","AverageRank")
        power_rankings$Team<-factor(power_rankings$Team,
                                    levels = power_rankings$Team[order(power_rankings$AverageRank,decreasing = FALSE)])
        p <- plot_ly(power_rankings, x = ~Team, y = ~AverageRank, type = 'bar',
                     marker = list(color = gg_color_hue(12))) %>%
            layout(title = input$power_rank_plot_selection,
                   xaxis = list(title = "",tickangle = 45, tickfont=f1),
                   yaxis = list(title = "Average Rank"),
                   margin = list(b = 160, r = 100))

    }else if(input$power_rank_plot_selection=="Power Rank vs. Standing"){
      records_order<-match(as.vector(cat_rankings()[,1]),names(records))
      records<-as.numeric(records)
      power_rankings<-data.frame(cat_rankings()[,1],as.numeric(cat_rankings()[,"Goaltending Rank"]),records[records_order])
      colnames(power_rankings)<-c("Team","PowerRank","Points")
      power_rankings$Team<-factor(power_rankings$Team,
                                  levels = power_rankings$Team[order(power_rankings$Points,decreasing = TRUE)])

      p <- plot_ly(power_rankings, x = ~Points) %>%
        add_trace(y = ~PowerRank, text = ~Team,
                   marker = list(color = gg_color_hue(12), size = 20), showlegend = FALSE,
                  name = " ") %>%
        add_lines(y = ~fitted(loess(PowerRank ~ Points)),
                  line = list(color = 'black'), name = "Loess Smoother",
                  showlegend = FALSE) %>%
        layout(title = input$power_rank_plot_selection,
               xaxis = list(title = "Points"),
               yaxis = list(title = "Power Rank")#,
               #margin = list(b = 160, r = 100)
               )

    }else if(input$power_rank_plot_selection=="Standings"){
        f1 <- list(
            family = "Arial, sans-serif",
            size = 18,
            color = "black")
        records_order<-match(as.vector(cat_rankings()[,1]),names(records))
        records<-as.numeric(records)
        power_rankings<-data.frame(cat_rankings()[,1],records[records_order])
        colnames(power_rankings)<-c("Team","AverageRank")
        power_rankings$Team<-factor(power_rankings$Team,
                                    levels = power_rankings$Team[order(power_rankings$AverageRank,decreasing = TRUE)])
        p <- plot_ly(power_rankings, x = ~Team, y = ~AverageRank, type = 'bar',
                     marker = list(color = gg_color_hue(12))) %>%
            layout(title = input$power_rank_plot_selection,
                   xaxis = list(title = "",tickangle = 45,tickfont=f1),
                   yaxis = list(title = "Average Rank"),
                   margin = list(b = 160, r = 100))
    }
          orca(p=p,file="../../The_Scientific_Shrimper/Images/seasonalPlot.png",height=500,width=1000)
  })

  output$weekly_power_barplot<-renderPlotly({
    power_rankings<-data.frame(rownames(mean_weekly_cat_rankings),as.numeric(mean_weekly_cat_rankings))
    colnames(power_rankings)<-c("Team","AverageRank")
    power_rankings$Team<-factor(power_rankings$Team,
                                levels = power_rankings$Team[order(power_rankings$AverageRank,decreasing = FALSE)])
    f1 <- list(
        family = "Arial, sans-serif",
        size = 18,
        color = "black")
    p <- plot_ly(power_rankings, x = ~Team, y = ~AverageRank, type = 'bar',
                 marker = list(color = gg_color_hue(12))) %>%
        layout(title = "Weekly Power Ranking",
               xaxis = list(title = "",tickangle = 45, tickfont = f1),
               yaxis = list(title = "Average Rank"),
               margin = list(b = 160, r = 100)
               )
  })

  observeEvent(input$saveWeeklyPowerPlot, {
          power_rankings<-data.frame(rownames(mean_weekly_cat_rankings),as.numeric(mean_weekly_cat_rankings))
    colnames(power_rankings)<-c("Team","AverageRank")
    power_rankings$Team<-factor(power_rankings$Team,
                                levels = power_rankings$Team[order(power_rankings$AverageRank,decreasing = FALSE)])
    f1 <- list(
        family = "Arial, sans-serif",
        size = 18,
        color = "black")
    p <- plot_ly(power_rankings, x = ~Team, y = ~AverageRank, type = 'bar',
                 marker = list(color = gg_color_hue(12))) %>%
        layout(title = "Weekly Power Ranking",
               xaxis = list(title = "",tickangle = 45, tickfont = f1),
               yaxis = list(title = "Average Rank"),
               margin = list(b = 160, r = 100)
               )
          orca(p=p,file="../../The_Scientific_Shrimper/Images/weeklyPlot.png",height=500,width=1000)
  })

  weekly_power_rankings<-reactive({
    means_weekly_ranking<-data.frame(Team=rownames(weekly_cat_rankings),
                                     Ranking=round(as.numeric(rank(mean_weekly_cat_rankings,ties.method = "average"),digits = 0)))
    means_weekly_ranking<-means_weekly_ranking[order(means_weekly_ranking[,2],decreasing = FALSE),]
    as.matrix(means_weekly_ranking)
  })
  output$weekly_power_table<-renderTable(
    weekly_power_rankings(), hover = TRUE
  )
  weekly_team_selected<-reactive({
    match(input$weeklyTeamSelect,rownames(weekly_team_z))
  })
  output$weeklyTeamBarplot<-renderPlotly({
    colors<-c()
    for(i in 1:ncol(weekly_team_z)){
      if(as.numeric(weekly_team_z[weekly_team_selected(),])[i]>0){
        colors<-c(colors,"red")
      } else {
        colors<-c(colors,"blue")
      }
    }
    weekly_team_frame<-data.frame(colnames(weekly_team_z),as.numeric(weekly_team_z[as.numeric(weekly_team_selected()),]))
    colnames(weekly_team_frame)<-c("Category","Value")
    weekly_team_frame$Category<-factor(weekly_team_frame$Category,levels = weekly_team_frame$Category)
    p <- plot_ly(weekly_team_frame,
                 x = ~Category, y = ~Value,
                 type = 'bar',
                 marker = list(color = colors)) %>%
      layout(title = input$weeklyTeamSelect,
             shapes=list(type='line', x0= 10.51, x1= 10.51, y0=-2, y1=3, line=list(width=1)),
             xaxis = list(title = "",tickangle = 45),
             yaxis = list(title = "Z score")
             )
  })
  output$weeklyHeatmap<-renderPlot({weekly_heatmap})
  output$seasonalHeatmap<-renderPlot({seasonal_heatmap})
}
shinyApp(ui = ui, server = server)