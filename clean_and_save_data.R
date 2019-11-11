library(plotrix)
library(RColorBrewer)
library(rvest)
library(shinythemes)
library(ggplot2)
library(lubridate)
library(plotly)
library(reticulate)
library(tidyverse)
library(Polychrome)

setwd("~/Documents/Fantasy_App_2020/")

week <- ceiling(as.numeric(difftime(strptime(gsub("-",".",Sys.Date()), format = "%Y.%m.%d"),
                 strptime("14.10.2019", format = "%d.%m.%Y"),units="weeks")) + 1)

date <- format(Sys.time(),"%b %d, %Y, at %X")

z_score<-function(x,mean,sd){
  z<-(x-mean)/sd
  return(z)
}

toSeconds <- function(x){
  if (!is.character(x)) stop("x must be a character string of the form H:M:S")
  if (length(x)<=0)return(x)

  unlist(
    lapply(x,
           function(i){
             i <- as.numeric(strsplit(i,':',fixed=TRUE)[[1]])
             if (length(i) == 3)
               i[1]*3600 + i[2]*60 + i[3]
             else if (length(i) == 2)
               i[1]*60 + i[2]
             else if (length(i) == 1)
               i[1]
           }
    )
  )
}

cat_palette <- createPalette(17,"#0000ff")
team_palette <- createPalette(9,"#0000ff")

names(cat_palette) = names(team_palette) = NULL

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


teamColors<-rainbow(n=12,s=.25)

####################################

############################################################################
## Scrape data                                                            ##
############################################################################

use_python("/usr/local/lib/python3.7/")

py_run_file("scraper.py")

matchup_data <- py$matchup_df

class(matchup_data$matchup) = "numeric"

records_df <- t(sapply(unique(matchup_data$Team), function(x){
    matchups <- matchup_data %>%
        filter(Team==x) %>%
        ## filter(matchup != max(matchup)) %>%
        select(CatPts)
    Wins=0
    Losses=0
    Ties=0
    for(i in 1:nrow(matchups)){
        if(matchups[i,]>8.5){
            Wins = Wins + 1 
        }else if(matchups[i,]<8.5){
            Losses = Losses + 1
        }else{
            Ties = Ties + 1
        }
    }
    return(c(Wins,Losses,Ties))
}))
records_df<-cbind(rownames(records_df),records_df)
colnames(records_df)<-c("Team","Wins","Losses","Ties")

records <- apply(records_df,1,function(x) return(as.numeric(x[2])*2 + as.numeric(x[4])))
names(records) <- records_df[,1]

matchup_data$GAA <- as.numeric(matchup_data$GAA)*-1
matchup_data$GAA[is.na(matchup_data$GAA)] <- 0

matchup_data$ATOI <- toSeconds(matchup_data$ATOI)
matchup_data$G.TOI <- toSeconds(matchup_data$G.TOI)

## Need to adjust stats in longer scoring periods (cannot apply to average stats)
matchup_data[which(matchup_data$matchup==1),c(5:12,14:16,18:21)] =
    matchup_data[which(matchup_data$matchup==1),c(5:12,14:16,18:21)]*(7/12)

season_data <- matchup_data %>%
    ## filter(matchup!=max(matchup)) %>%
    group_by(Team) %>%
    summarise_all(sum) %>%
    as.data.frame() %>%
    column_to_rownames("Team")

games_for_average_stats <- sapply(rownames(season_data), function(x){
    games<-length(which(matchup_data %>%
                        ## filter(matchup!=max(matchup)) %>%
                        select(Team)==x))
    return(games)
})

season_data <- py$season_df %>%
    column_to_rownames("Team")
season_data$GAA <- as.numeric(season_data$GAA)*-1
season_data$ATOI <- toSeconds(season_data$ATOI)
season_data$G.TOI <- toSeconds(season_data$G.TOI)

dataframe_z <- apply(season_data,2,function(x) return(scale(as.numeric(x))))
rownames(dataframe_z) <- rownames(season_data)
dataframe_z<-cbind(rownames(dataframe_z),dataframe_z)

matchup_data_all<-matchup_data

matchup_data_all_z<-matchup_data_all %>%
    select(-c(CatWins,CatLosses,CatTies,CatPts)) %>%
    distinct()

matchup_data_all_z <- sapply(unique(matchup_data_all_z$matchup),
                             function(x){
                                 out<-matchup_data_all_z %>%
                                     filter(matchup==x) %>%
                                     mutate_at(-c(18,19),funs(c(scale(.))))
                                 return(out)
                             })
matchup_data_all_z <- do.call("rbind",apply(matchup_data_all_z,2,function(x){
    return(as.data.frame(x))
}))

matchup_data_all_z[is.na(matchup_data_all_z)] <- 0

matchup_data <- matchup_data %>%
    filter(matchup==max(matchup)) %>%
    select(-c(CatWins,CatLosses,CatTies,CatPts,matchup)) %>%
    distinct() %>%
    column_to_rownames("Team")

weekly_cat_rankings<-matchup_data
for(i in 1:ncol(weekly_cat_rankings)){
  weekly_cat_rankings[,i]<-rank(-as.numeric(weekly_cat_rankings[,i]),ties.method = "average")
}

mean_weekly_cat_rankings<-matrix(0,ncol=1,nrow=nrow(weekly_cat_rankings))
rownames(mean_weekly_cat_rankings)<-rownames(weekly_cat_rankings)
for(i in 1:nrow(weekly_cat_rankings)){
  mean_weekly_cat_rankings[i]<-mean(as.numeric(weekly_cat_rankings[i,]))
}

sd_weekly_cat_rankings<-matrix(0,ncol=1,nrow=nrow(weekly_cat_rankings))
rownames(sd_weekly_cat_rankings)<-rownames(weekly_cat_rankings)
for(i in 1:nrow(weekly_cat_rankings)){
  sd_weekly_cat_rankings[i]<-sd(as.numeric(weekly_cat_rankings[i,]))
}


weekly_team_z <- apply(matchup_data,2,function(x) return(scale(as.numeric(x))))
weekly_team_z[is.na(weekly_team_z)] <- 0
rownames(weekly_team_z) <- rownames(matchup_data)
#############################
# Functions for growth curves
#############################

result <- function(x){
    if(x<8.5){return("Loss")}
    else if(x>8.5){return("Win")}
    else{return("Tie")}
}
result=Vectorize(result)

plot_category_curve<-function(matchup_data_all_z,
                              team,category){
    drop_indices<-duplicated(matchup_data_all[,-c(1:4)])
    temp<-data.frame(matchup_data_all_z,CatPts=matchup_data_all[!drop_indices,"CatPts"])
    if(category!="All"&category!="Average Weekly Z Score"){
        plot_df<-temp %>%
            filter(Team==team) %>%
            select(category,CatPts,matchup) %>%
            mutate(CatPts=result(CatPts))
        colnames(plot_df)<-c("value","result","Week")
        mytitle=paste0(team,", ", category)
        ggplot(plot_df, aes(x = Week, y = value,color = result)) +
            geom_smooth(aes(group = 1), colour = "black") +
            geom_point(size = 5) +
            theme_bw() +
            ylab(label="Z score") +
            xlab("Week Number") +
            ggtitle(mytitle) +
            geom_hline(yintercept = 0)
    }else if(category=="Average Weekly Z Score"){
        plot_df<-temp %>%
            filter(Team==team) %>%
            mutate(CatPts=result(CatPts)) %>%
            gather(key,value,-CatPts,-matchup,-Team)
        mytitle=paste0(team,", ", category)
        ggplot(plot_df, aes(x = matchup, y = value,color = key)) +
            geom_smooth(aes(group = 1), colour = "black") +
            ##geom_point(size = 5) +
            theme_bw() +
            ylab(label="Z score") +
            xlab("Week Number") +
            ggtitle(mytitle) +
            geom_hline(yintercept = 0)
    }
    else if(category=="All"){
        plot_df<-temp %>%
            filter(Team==team) %>%
            mutate(CatPts=result(CatPts)) %>%
            gather(key,value,-CatPts,-matchup,-Team)
        mytitle=paste0(team,", All categories")
        ggplot(plot_df, aes(x = matchup, y = value,
                            group=key,color = key)) +
            geom_line(size=1.5) +
            scale_color_manual(values=cat_palette) +
            theme_bw() +
            ylab(label="Z score") +
            xlab("Week Number") +
            ggtitle(mytitle) +
            geom_hline(yintercept = 0)        
    }
}

plot_category_curve_all<-function(matchup_data_all_z){
    plot_df<-temp %>%
        mutate(CatPts=result(CatPts)) %>%
        gather(key,value,-CatPts,-matchup,-Team) %>%
        group_by(Team,matchup) %>%
        summarize(mean = mean(value))
    mytitle=paste0("All teams average performance over time")
    ggplot(plot_df, aes(x = matchup, y = mean,color = Team,group = Team)) +
        geom_smooth(size = 3) +
        ##geom_line(size = 3) +
        scale_color_manual(values = team_palette) +
        theme_bw() +
        ylab(label="Z score") +
        xlab("Week Number") +
        ggtitle(mytitle) +
        geom_hline(yintercept = 0)
}
#############################
ord <- hclust( dist(weekly_team_z, method = "euclidean"), method = "ward.D" )$order
library(reshape)
weekly_team_z.m<-melt(weekly_team_z)
weekly_team_z.m$X1 <- factor(weekly_team_z.m$X1, levels = rownames(weekly_team_z)[ord])
## weekly_team_z.m$X2 <- factor(weekly_team_z.m$X2, levels = rev(colnames(matchup_data)))
weekly_team_z.m$X2 <- factor(weekly_team_z.m$X2, levels = rev(c("Goals","Assists",
                                                                "PPP","SHP","Points",
                                                                "PlusMinus","PIM","SOG",
                                                                "Hits","ATOI","Blocks",
                                                                "G.TOI","Wins","GAA","Saves",
                                                                "G.Points","G.PIM")))
vlines=data.frame(vlines= seq(1.5,11.5,by=1))

weekly_heatmap<-ggplot(weekly_team_z.m, aes(X1, X2)) +
    geom_tile(aes(fill = value),colour = "white") +
    scale_fill_distiller(palette="RdBu") +
    theme_classic(base_size = 12) + labs(x = "",y = "") +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    geom_hline(aes(yintercept = 6.5),size=2.4,alpha=0.74)+
    geom_hline(aes(yintercept = 12.5),size=2.4,alpha=0.74)+
    geom_vline(aes(xintercept = vlines),data=vlines,size=1.4,alpha=0.74)+
    guides(fill=guide_legend(title="Z Score")) +
    theme(axis.text.x = element_text(size = 20,
                                     angle = 330, hjust = 0),
          axis.text.y = element_text(size=20,face = "bold"))

#############################
season_data_z <- as.matrix(dataframe_z)[,-1]
##season_data_z[,17] <- as.numeric(season_data_z[,17])*-1
class(season_data_z)<-"numeric"
ord <- hclust( dist(season_data_z, method = "euclidean"), method = "ward.D" )$order
season_data_z.m<-melt(season_data_z)
season_data_z.m$X1 <- factor(season_data_z.m$X1, levels = rownames(season_data_z)[ord])
season_data_z.m$X2 <- factor(season_data_z.m$X2, levels = rev(c("Goals","Assists",
                                                                "PPP","SHP","Points",
                                                                "PlusMinus","PIM","SOG",
                                                                "Hits","ATOI","Blocks",
                                                                "G.TOI","Wins","GAA","Saves",
                                                                "G.Points","G.PIM")))

vlines=data.frame(vlines= seq(1.5,11.5,by=1))

seasonal_heatmap<-ggplot(season_data_z.m, aes(X1, X2)) +
    geom_tile(aes(fill = value),colour = "white") +
    scale_fill_distiller(palette="RdBu") +
    theme_classic(base_size = 12) + labs(x = "",y = "") +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    geom_hline(aes(yintercept = 6.5),size=2.4,alpha=0.74)+
    geom_hline(aes(yintercept = 12.5),size=2.4,alpha=0.74)+
    geom_vline(aes(xintercept = vlines),data=vlines,size=1.4,alpha=0.74)+
    guides(fill=guide_legend(title="Z Score")) +
    theme(axis.text.x = element_text(size = 20,
                                     angle = 330, hjust = 0),
          axis.text.y = element_text(size=20,face = "bold"))

save(list = ls(),file=paste0("week",week,".Rda"))
