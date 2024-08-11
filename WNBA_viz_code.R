## load packages
library(tidyverse)
library(rvest)
library(ggh4x)
library(gt)
library(gtExtras)

##############################################
##
##      create data sets
##
##############################################

# dates for games (YearMonthDay)
date <- c(20240514:20240531,20240601:20240630,20240701:20240717)

# create data frame for game id and URLs
schedule.raw=data.frame()

for(i in 1:length(date)){
  url <- paste0("https://www.espn.com/wnba/scoreboard/_/date/",date[i])
  page <- read_html(url) %>%
    html_nodes("a") %>%
    html_attr("href")
  if(length(grep("boxscore",page)) > 0){
    schedule.x <- data.frame(
      date = str_extract(url,"[0-9]+"),
      gameid = page[grep("boxscore",page)] %>%
        str_extract(.,"[0-9]+"))
    schedule.raw <- rbind(schedule.raw,schedule.x)
  }
}

schedule=schedule.raw
schedule <- schedule[-which(schedule$gameid=="401677672"),] # remove commissioner's cup game

schedule <- schedule %>%
  mutate(matchup= paste0("https://www.espn.com/wnba/matchup/_/gameId/",gameid))

# create data frame for game attendance and scores

team.score <- data.frame()
for(i in 1:nrow(schedule)) {
  url.match <- schedule$matchup[i]
  match.data <- read_html(url.match)
  team.score.x <- data.frame(
    home.team = match.data %>%
      html_node(".Table__even+ .Table__even .Table__TD") %>%
      html_text(),
    home.score = match.data %>%
      html_node(".Table__even+ .Table__even .Table__TD:nth_child(6)") %>%
      html_text(),
    away.team = match.data %>%
      html_node(".Table__TD") %>%
      html_text(),
    away.score = match.data %>%
      html_node(".Table__TD:nth_child(6)") %>%
      html_text(),
    attendance = match.data %>%
      html_node(".Attendance__Numbers") %>%
      html_text() %>%
      sub(",","",.) %>%
      str_extract(.,"[0-9]+")
  )
  team.score <- rbind(team.score,team.score.x)
}

# create data frame for arena capacities
url <- "https://en.wikipedia.org/wiki/Women%27s_National_Basketball_Association"
wiki.tab <- (read_html(url) %>%
               html_nodes("table") %>%
               html_table())[[3]] %>%
  drop_na()
wiki.tab <- wiki.tab %>%
  mutate(team.name=c("ATL","CHI","CONN","IND","NY","WSH",
                     "DAL","LV","LA","MIN","PHX","SEA")) %>%
  mutate(Capacity=str_replace(Capacity,"\\[a]","")) %>%
  mutate(Capacity=as.numeric(str_replace(Capacity,",","")))


# create main data frame with all data

team.score.x <- team.score %>%
  mutate(date=as.Date(schedule$date,"%Y%m%d"))

team.score.x <- team.score.x %>%
  left_join(wiki.tab %>% select(team.name,Capacity),by=c("home.team"="team.name")) %>%
  mutate(diff=as.numeric(home.score)-as.numeric(away.score),
         attendance=as.numeric(attendance)) %>%
  mutate(attendance=ifelse(is.na(attendance),9025,attendance)) %>%
  mutate(opponent=ifelse(away.team=="IND" | home.team=="IND","IND",NA))

team.score.x <- team.score.x %>% 
  pivot_longer(cols=c(home.team,away.team),names_to="team",values_to="team.name") %>%
  mutate(win.loss=
           case_when(
             team=="home.team" & diff>0 ~ "W",
             team=="home.team" & diff<0 ~ "L",
             team=="away.team" & diff>0 ~ "L",
             team=="away.team" & diff<0 ~ "W")
  )

med.att <- team.score.x %>% filter(team=="home.team") %>%
  group_by(team.name) %>%
  summarize(median=median(attendance))

team.opp <- team.score.x %>% select(team.name) %>%
  mutate(id=rep(1:nrow(team.score),each=2),id2=rep(1:2,nrow(team.score))) %>%
  arrange(id,desc(id2))

team.colors <- data.frame(
  team.name = c("ATL","CHI","CONN","DAL","IND","LV","LA","MIN","NY","PHX","SEA","WSH"),
  colors = c("#C8102E","#418FDE","#FC4C02","#C4D600",
             "#041E42","#010101","#702F8A","#236192",
             "#010101","#CB6015","#2C5234","#C8102E")) %>%
  left_join(med.att) %>%
  arrange(1/median)

team.score.x <- team.score.x %>%
  left_join(med.att) %>%
  mutate(team.name=fct_reorder(team.name,1/median),
         opp=team.opp$team.name,
         per.cap=round(100*(attendance/Capacity),1))
median_per_cap <- team.score.x %>% filter(team=="away.team",per.cap<150) %>% group_by(team.name) %>%
  summarize(med.pc=median(per.cap))
team.colors2 <- data.frame(
  team.name = c("ATL","CHI","CONN","DAL","IND","LV","LA","MIN","NY","PHX","SEA","WSH"),
  colors = c("#C8102E","#418FDE","#FC4C02","#C4D600",
             "#041E42","#010101","#702F8A","#236192",
             "#010101","#CB6015","#2C5234","#C8102E")) %>%
  left_join(median_per_cap) %>%
  arrange(med.pc)

# get attendance data for 2022 and 2023 seasons (download csv files from https://acrossthetimeline.com/index.html)
teams <- c("ATL","CHI","CONN","DAL","IND","LV","LA","MIN","NY","PHX","SEA","WSH")
att_2023 <- data.frame()
for(i in 1:length(teams)) {
  x.dat <- read.csv(paste0(teams[i],"_2023.csv"))
  x.dat <- x.dat %>% mutate(Team = teams[i])
  att_2023 <- rbind(att_2023, x.dat)
}

att_2022 <- data.frame()
for(i in 1:length(teams)) {
  x.dat <- read.csv(paste0(teams[i],"_2022.csv"))
  x.dat <- x.dat %>% mutate(Team = teams[i])
  att_2022 <- rbind(att_2022, x.dat)
}

# combine with main data frame
att_all <- rbind(
  team.score.x %>% filter(team=="home.team") %>%
    select(team.name,opp,attendance) %>%
    `colnames<-`(c("Team","Opponent","Attendance")) %>%
    mutate(Season=2024),
  att_2023 %>% select(Team,Opponent,Attendance) %>%
    mutate(Season=2023),
  att_2022 %>% select(Team,Opponent,Attendance) %>%
    mutate(Season=2022)
) %>% mutate(
  Opponent=case_when(
    Opponent=="Atlanta Dream" ~ "ATL",
    Opponent=="Chicago Sky" ~ "CHI",
    Opponent=="Connecticut Sun" ~ "CONN",
    Opponent=="Dallas Wings" ~ "DAL",
    Opponent=="Indiana Fever" ~ "IND",
    Opponent=="Las Vegas Aces" ~ "LV",
    Opponent=="Los Angeles Sparks" ~ "LA",
    Opponent=="Minnesota Lynx" ~ "MIN",
    Opponent=="New York Liberty" ~ "NY",
    Opponent=="Phoenix Mercury" ~ "PHX",
    Opponent=="Seattle Storm" ~ "SEA",
    Opponent=="Washington Mystics" ~ "WAS",
    TRUE ~ Opponent
  )
)

# data frame without IND games
att_all2 <- rbind(
  team.score.x %>% filter(team=="home.team") %>%
    select(team.name,opp,attendance) %>%
    `colnames<-`(c("Team","Opponent","Attendance")) %>%
    mutate(Season=2024) %>% filter(Opponent != "IND"),
  att_2023 %>% select(Team,Opponent,Attendance) %>%
    mutate(Season=2023),
  att_2022 %>% select(Team,Opponent,Attendance) %>%
    mutate(Season=2022)
) %>% mutate(
  Opponent=case_when(
    Opponent=="Atlanta Dream" ~ "ATL",
    Opponent=="Chicago Sky" ~ "CHI",
    Opponent=="Connecticut Sun" ~ "CONN",
    Opponent=="Dallas Wings" ~ "DAL",
    Opponent=="Indiana Fever" ~ "IND",
    Opponent=="Las Vegas Aces" ~ "LV",
    Opponent=="Los Angeles Sparks" ~ "LA",
    Opponent=="Minnesota Lynx" ~ "MIN",
    Opponent=="New York Liberty" ~ "NY",
    Opponent=="Phoenix Mercury" ~ "PHX",
    Opponent=="Seattle Storm" ~ "SEA",
    Opponent=="Washington Mystics" ~ "WAS",
    TRUE ~ Opponent
  )
)



# create team labels for facet plot strips
wiki.tab <- wiki.tab %>%
  left_join(med.att) %>%
  mutate(label=paste0(Team,"\n",
                      Arena,
                      "\nArena Capacity: ",
                      format(Capacity,big.mark=",")
                      )
         )

team_labels <- as_labeller(
  c(`ATL`=wiki.tab$label[wiki.tab$team.name=="ATL"],
    `CHI`=wiki.tab$label[wiki.tab$team.name=="CHI"],
    `CONN`=wiki.tab$label[wiki.tab$team.name=="CONN"],
    `DAL`=wiki.tab$label[wiki.tab$team.name=="DAL"],
    `IND`=wiki.tab$label[wiki.tab$team.name=="IND"],
    `LV`=wiki.tab$label[wiki.tab$team.name=="LV"],
    `LA`=wiki.tab$label[wiki.tab$team.name=="LA"],
    `MIN`=wiki.tab$label[wiki.tab$team.name=="MIN"],
    `NY`=wiki.tab$label[wiki.tab$team.name=="NY"],
    `PHX`=wiki.tab$label[wiki.tab$team.name=="PHX"],
    `SEA`=wiki.tab$label[wiki.tab$team.name=="SEA"],
    `WSH`=wiki.tab$label[wiki.tab$team.name=="WSH"]
  ))

# get logos for tables
wnba.png.logos <- c(
  "https://raw.githubusercontent.com/follisj/WNBA_Attendance/main/logos/ATL.png",
  "https://raw.githubusercontent.com/follisj/WNBA_Attendance/main/logos/CHI.png",
  "https://raw.githubusercontent.com/follisj/WNBA_Attendance/main/logos/CONN.png",
  "https://raw.githubusercontent.com/follisj/WNBA_Attendance/main/logos/DAL.png",
  "https://raw.githubusercontent.com/follisj/WNBA_Attendance/main/logos/IND.png",
  "https://raw.githubusercontent.com/follisj/WNBA_Attendance/main/logos/LV.png",
  "https://raw.githubusercontent.com/follisj/WNBA_Attendance/main/logos/LA.png",
  "https://raw.githubusercontent.com/follisj/WNBA_Attendance/main/logos/MIN.png",
  "https://raw.githubusercontent.com/follisj/WNBA_Attendance/main/logos/NY.png",
  "https://raw.githubusercontent.com/follisj/WNBA_Attendance/main/logos/PHX.png",
  "https://raw.githubusercontent.com/follisj/WNBA_Attendance/main/logos/SEA.png",
  "https://raw.githubusercontent.com/follisj/WNBA_Attendance/main/logos/WSH.png"
)

##############################################
##
##      create visualizations
##
##############################################

##############################################
###     WNBA Attendance Table
##############################################
team.cols <- c("#C8102E","#418FDE","#FC4C02","#C4D600",
               "#041E42","#010101","#702F8A","#236192",
               "#010101","#CB6015","#2C5234","#C8102E")

wnba_att_tab <-
  att_all %>% 
  rename(team.name=Team) %>%
  left_join(wiki.tab %>% select(team.name,Capacity,Team)) %>%
  group_by(Team,Season) %>%
  summarize(avg.att=round(mean(Attendance),0),sd.att=round(sd(Attendance),1)) %>%
  ungroup() %>%
  pivot_wider(names_from=Season,values_from=c(avg.att,sd.att)) %>%
  left_join(wiki.tab %>% select(Team,Capacity)) %>%
  mutate(team.cols=team.cols,image=wnba.png.logos) %>%
  gt() %>%
  tab_spanner(
    label="Season",
    columns=c(avg.att_2022,sd.att_2022,avg.att_2023,sd.att_2023,avg.att_2024,sd.att_2024)
  ) %>%
  gt_merge_stack(col1=avg.att_2022,col2=sd.att_2022,
                 font_size = c("15px","13px"),
                 palette=c("gray20","gray40"),
                 font_weight=c("normal","normal")) %>%
  gt_merge_stack(col1=avg.att_2023,col2=sd.att_2023,
                 font_size = c("16px","13px"),
                 palette=c("gray20","gray40"),
                 font_weight=c("normal","normal")) %>%
  gt_merge_stack(col1=avg.att_2024,col2=sd.att_2024,
                 font_size = c("17px","13px"),
                 palette=c("gray20","gray40"),
                 font_weight=c("normal","normal")) %>%
  cols_move(
    columns = image,
    after=Team
  ) %>%
  cols_move(
    columns = Team,
    after=image
  ) %>%
  tab_style(
    style = "padding-left:12px;padding-right:20px;",
    locations = cells_body(columns = c(Capacity))
  ) %>%
  tab_style(
    style = "padding-left:15px;",
    locations = cells_body(columns = c(image))
  ) %>%
  tab_style(
    style = "padding-right:20px;",
    locations = cells_body(columns = c(Team))
  ) %>%
  tab_style(
    style = "padding-left:12px;padding-right:20px;",
    locations = cells_column_labels(columns = c(Capacity))
  ) %>%
  tab_style(
    style = cell_text(size=px(18)),
    locations = cells_body(columns = Team)
  ) %>%
  cols_label(
    Team="",
    avg.att_2022="2022",
    avg.att_2023="2023",
    avg.att_2024="2024",
    Capacity=md("Arena<br>Capacity<br>(2024)"),
    image=""
  ) %>%
  cols_hide(team.cols) %>%
  text_transform(
    locations=cells_body(columns=image),
    fn=function(x) {
      web_image(
        url=x,
        height=35
      )
    }
  ) %>%
  tab_header(
    title=html("WNBA Home Attendance")
  ) %>%
  gt_highlight_rows(fill="#F1F1F1") %>%
  tab_style(
    style=cell_text(color=from_column("team.cols")),
    locations=cells_body(columns=everything())
  ) %>%
  
  tab_style(
    style=cell_text(color="black",
                    weight="normal"),
    locations=cells_body(columns=Capacity)
  ) %>%
  tab_options(
    table.background.color = "#F1F1F1",
    column_labels.background.color = "#F1F1F1",
    heading.background.color = "#F1F1F1",
    heading.border.bottom.color = "#F1F1F1",
    table.border.top.color = "#F1F1F1",
    heading.title.font.size=px(24),
    heading.padding=px(5)
  )
gtsave(wnba_att_tab,"wnba_att_tab.png")

##############################################
###   boxplot 2024 home attendance by team
##############################################
team.score.x %>%
  filter(team=="home.team") %>%
  left_join(wiki.tab %>% select(Team,team.name)) %>%
  mutate(team.name=fct_reorder(team.name,1/median)) %>%
  ggplot(aes(team.name,attendance))+
  geom_boxplot(aes(color=team.name),width=.35,size=.5)+
  ggrepel::geom_text_repel(data= . %>% filter(team.name != "IND"),
                           aes(label=opponent),hjust=-.5,vjust=-.5,size=5)+
  geom_point(aes(col=team.name),size=2)+
  geom_vline(aes(xintercept=team.name,col=team.name),lty=3,alpha=.5)+
  geom_label(aes(y=-2000,label=Team,col=team.name),hjust=0,size=5.5)+
  scale_y_continuous(sec.axis=dup_axis(),expand=c(0,0),limits=c(-2200,22500))+
  scale_color_manual(values=c(team.colors$colors))+
  scale_fill_manual(values=c(team.colors$colors))+
  labs(title="2024 WNBA Home Attendance by Team\n",
       #subtitle="The Caitlin Clark Effect",
       #caption="Data from espn.com"
       )+
  theme_minimal()+
  theme(legend.position="none",
        plot.title=element_text(size=20,hjust=.5),
        plot.subtitle=element_text(size=15,hjust=.5),
        plot.caption=element_text(size=10,hjust=.5),
        axis.title=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(size=15),
        plot.background=element_rect(fill="#F0F0F0"),
        panel.grid.major.x=element_line(color="gray80"),
        plot.margin=margin(1,1,1,1,"cm"))+
  coord_flip()
ggsave("2024_home_attendance.png",width=12,height=12,dpi=320)

##############################################
###     boxplots - seasons 2022-20244
##############################################
att_all %>%
  ggplot(aes(Attendance,as.factor(Season),group=Season))+
  geom_boxplot(aes(col=Team),width=.35)+
  geom_point(aes(col=Team))+
  
  ggrepel::geom_text_repel(data=att_all %>% group_by(Team,Season) %>% 
                             slice_max(Attendance,n=2),
                           aes(x=Attendance,label=Opponent),hjust=.75,
                           max.overlaps=4)+
  
  scale_color_manual(values=team.colors$colors)+
  xlim(0,21000)+
  labs(
    title="WNBA Attendance by Team",
    subtitle="2022 - 2024"
  )+
  facet_wrap2(.~Team,nrow=4,labeller=team_labels,
              strip = strip_themed(
                background_x = list(element_rect(fill = "#041E42"),
                                    element_rect(fill = "#010101"),
                                    element_rect(fill = "#010101"),
                                    element_rect(fill = "#702F8A"),
                                    element_rect(fill = "#CB6015"),
                                    element_rect(fill = "#2C5234"),
                                    element_rect(fill = "#418FDE"),
                                    element_rect(fill = "#FC4C02"),
                                    element_rect(fill = "#236192"),
                                    element_rect(fill = "#C4D600"),
                                    element_rect(fill = "#C8102E"),
                                    element_rect(fill = "#C8102E")
                )))+
  theme_bw()+
  theme(legend.position="none",
        strip.text.x = element_text(color="#F1F1F1",size=15),
        plot.title=element_text(size=24,hjust=.5),
        plot.subtitle=element_text(size=18,hjust=.5),
        axis.title=element_blank(),
        axis.text = element_text(size=12,face="bold"),
        plot.background = element_rect(fill = "#F1F1F1"),
        plot.margin=margin(1,1,1,1,"cm"))
ggsave("wnba_attendance_22_24.png",width=15,height=15,units="in")

##############################################
### WNBA Attendance Table, no Indiana 2024
##############################################
wnba_att_tab_no_ind <-
  att_all %>% 
  mutate(Attendance=ifelse((Opponent=="IND" & Season==2024),NA,Attendance)) %>%
  rename(team.name=Team) %>%
  left_join(wiki.tab %>% select(team.name,Capacity,Team)) %>%
  #filter(Attendance <= Capacity) %>%
  group_by(Team,Season) %>%
  summarize(avg.att=round(mean(Attendance,na.rm=T),0),sd.att=round(sd(Attendance,na.rm=T),1)) %>%
  ungroup() %>%
  pivot_wider(names_from=Season,values_from=c(avg.att,sd.att)) %>%
  left_join(wiki.tab %>% select(Team,Capacity)) %>%
  mutate(team.cols=team.cols,image=wnba.png.logos) %>%
  gt() %>%
  tab_spanner(
    label="Season",
    columns=c(avg.att_2022,sd.att_2022,avg.att_2023,sd.att_2023,avg.att_2024,sd.att_2024)
  ) %>%
  gt_merge_stack(col1=avg.att_2022,col2=sd.att_2022,
                 font_size = c("15px","13px"),
                 palette=c("gray20","gray40"),
                 font_weight=c("normal","normal")) %>%
  gt_merge_stack(col1=avg.att_2023,col2=sd.att_2023,
                 font_size = c("16px","13px"),
                 palette=c("gray20","gray40"),
                 font_weight=c("normal","normal")) %>%
  gt_merge_stack(col1=avg.att_2024,col2=sd.att_2024,
                 font_size = c("17px","13px"),
                 palette=c("gray20","gray40"),
                 font_weight=c("normal","normal")) %>%
  cols_move(
    columns = image,
    after=Team
  ) %>%
  cols_move(
    columns = Team,
    after=image
  ) %>%
  tab_style(
    style = "padding-left:12px;padding-right:20px;",
    locations = cells_body(columns = c(Capacity))
  ) %>%
  tab_style(
    style = "padding-left:15px;",
    locations = cells_body(columns = c(image))
  ) %>%
  tab_style(
    style = "padding-right:20px;",
    locations = cells_body(columns = c(Team))
  ) %>%
  tab_style(
    style = "padding-left:12px;padding-right:20px;",
    locations = cells_column_labels(columns = c(Capacity))
  ) %>%
  tab_style(
    style = cell_text(size=px(18)),
    locations = cells_body(columns = Team)
  ) %>%
  cols_label(
    Team="",
    avg.att_2022="2022",
    avg.att_2023="2023",
    avg.att_2024="2024",
    Capacity=md("Arena<br>Capacity<br>(2024)"),
    image=""
  ) %>%
  cols_hide(team.cols) %>%
  text_transform(
    locations=cells_body(columns=image),
    fn=function(x) {
      web_image(
        url=x,
        height=35
      )
    }
  ) %>%
  tab_header(
    title=html("WNBA Home Attendance"),
    subtitle="(Excluding 2024 Indiana Fever games)"
  ) %>%
  gt_highlight_rows(fill="#F1F1F1") %>%
  tab_style(
    style=cell_text(color=from_column("team.cols")),
    locations=cells_body(columns=everything())
  ) %>%
  tab_style(
    style=cell_text(color="black",
                    weight="normal"),
    locations=cells_body(columns=Capacity)
  ) %>%
  tab_options(
    table.background.color = "#F1F1F1",
    column_labels.background.color = "#F1F1F1",
    heading.background.color = "#F1F1F1",
    heading.border.bottom.color = "#F1F1F1",
    table.border.top.color = "#F1F1F1",
    heading.title.font.size=px(24),
    heading.subtitle.font.size = px(16),
    #column_labels.padding = px(5),
    heading.padding=px(5)
  )
gtsave(wnba_att_tab_no_ind,"wnba_att_tab_no_ind.png")

##############################################
### boxplots - seasons 2022-2024 without IND 2024
##############################################
att_all2 %>%
  ggplot(aes(Attendance,as.factor(Season),group=Season))+
  geom_boxplot(aes(col=Team),width=.35)+
  geom_point(aes(col=Team))+
  scale_color_manual(values=team.colors$colors)+
  xlim(0,21000)+
  labs(
    title="WNBA Attendance by Team",
    subtitle="2022 - 2024\n(excluding Indiana Fever games)"
  )+
  facet_wrap2(.~Team,nrow=4,labeller=team_labels,
              strip = strip_themed(
                background_x = list(element_rect(fill = "#041E42"),
                                    element_rect(fill = "#010101"),
                                    element_rect(fill = "#010101"),
                                    element_rect(fill = "#702F8A"),
                                    element_rect(fill = "#CB6015"),
                                    element_rect(fill = "#2C5234"),
                                    element_rect(fill = "#418FDE"),
                                    element_rect(fill = "#FC4C02"),
                                    element_rect(fill = "#236192"),
                                    element_rect(fill = "#C4D600"),
                                    element_rect(fill = "#C8102E"),
                                    element_rect(fill = "#C8102E")
                )))+
  theme_bw()+
  theme(legend.position="none",
        strip.text.x = element_text(color="#F1F1F1",size=15),
        plot.title=element_text(size=24,hjust=.5),
        plot.subtitle=element_text(size=18,hjust=.5),
        axis.title=element_blank(),
        axis.text = element_text(size=12,face="bold"),
        plot.background = element_rect(fill = "#F1F1F1"),
        plot.margin=margin(1,1,1,1,"cm"))
ggsave("wnba_attendance_22_24_no_ind.png",width=15,height=15,units="in")


##############################################
### road attendance 2024 - percent capacity
##############################################
median_per_cap <- team.score.x %>% filter(team=="away.team") %>% group_by(team.name) %>%
  summarize(med.pc=median(per.cap))

team.score.x %>%
  filter(team=="away.team") %>%
  left_join(wiki.tab %>% select(Team,team.name)) %>%
  left_join(median_per_cap) %>%
  mutate(team.name=fct_reorder(team.name,med.pc)) %>%
  filter(opp != "IND") %>%
  ggplot(aes(team.name,per.cap))+
  geom_boxplot(aes(color=team.name),width=.35,size=.5)+
  geom_point(aes(col=team.name),size=2)+
  geom_vline(aes(xintercept=team.name,col=team.name),lty=3,alpha=.5)+
  geom_label(aes(y=-40,label=Team,col=team.name),hjust=0,size=5.5)+
  geom_text(data=team.score.x %>% filter(team=="away.team",per.cap < 175) %>%
              group_by(team.name) %>% slice_max(per.cap) %>%
              select(team.name,opp,attendance,per.cap),
            aes(team.name,per.cap,label=opp),
            hjust=-.5,size=5)+
  geom_text(data=team.score.x %>% filter(team=="away.team") %>%
              group_by(team.name) %>% slice_min(per.cap) %>%
              select(team.name,opp,attendance,per.cap),
            aes(team.name,per.cap,label=opp),
            hjust=1.5,size=4.5)+
  annotate("label",x=12,y=110,
           label="IND road games\n @LV (170%), @WSH (484%)\n @ATL (502%) not shown",
           size=4.5,hjust=0,col="#041E42")+
  annotate("label",x=4,y=117.5,
           label="PHX road game\n @WSH (300%)\n not shown",
           size=4.5,hjust=0,col="#CB6015")+
  annotate("label",x=10,y=117.5,
           label="CHI road game\n @WSH (238%)\n not shown",
           size=4.5,hjust=0,col="#418FDE")+
  scale_y_continuous(sec.axis=dup_axis(),expand=c(0,0),limits=c(-40,150), #limits=c(-40,205),
                     breaks=seq(0,100,25),labels=scales::percent_format(scale=1))+
  scale_color_manual(values=c(team.colors2$colors))+
  scale_fill_manual(values=c(team.colors2$colors))+
  labs(title="2024 WNBA Road Attendance by Team",
       subtitle="Percent of Arena Capacity\n "
       )+
  theme_minimal()+
  theme(legend.position="none",
        plot.title=element_text(size=20,hjust=.5),
        plot.subtitle=element_text(size=15,hjust=.5),
        plot.caption=element_text(size=10,hjust=.5),
        axis.title=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(size=15),
        plot.background=element_rect(fill="#F0F0F0"),
        panel.grid.major.x=element_line(color="gray80"),
        plot.margin=margin(1,1,1,1,"cm"))+
  coord_flip()
ggsave("2024_road_attendance2.png",width=14,height=12,dpi=320)

##############################################
###     line plot
##############################################
team.score.x %>% filter(team=="home.team") %>%
  left_join(team.colors) %>%
  left_join(wiki.tab %>% select(team.name,Capacity,Team)) %>%
  mutate(team.name=fct_reorder(team.name,1/median)) %>%
  mutate(Team=fct_reorder(Team,1/median)) %>%
  ggplot(aes(date,attendance))+
  geom_point(aes(col=team.name))+
  geom_line(aes(col=team.name),linewidth=.5)+
  geom_hline(data=. %>% group_by(team.name) %>%
               summarize(mean.cap=mean(Capacity)),aes(yintercept=mean.cap),
             col="gray50",lty=2)+
  geom_text(data=. %>% filter(opp=="IND"),aes(date,attendance,label=opp),vjust=-.65)+
  scale_color_manual(values=team.colors$colors)+
  labs(title="WNBA Attendance",
       subtitle="Arena Capacity indicated by dashed line",
       col="Team",
       caption="Source: Wikipedia")+
  ylim(0,22000)+
  xlim(as.Date("2024-05-12"),as.Date("2024-07-19"))+
  facet_wrap2(.~team.name,nrow=3,labeller=team_labels,
              strip = strip_themed(
                background_x = list(element_rect(fill = "#041E42"),
                                    element_rect(fill = "#010101"),
                                    element_rect(fill = "#010101"),
                                    element_rect(fill = "#702F8A"),
                                    element_rect(fill = "#CB6015"),
                                    element_rect(fill = "#2C5234"),
                                    element_rect(fill = "#418FDE"),
                                    element_rect(fill = "#FC4C02"),
                                    element_rect(fill = "#236192"),
                                    element_rect(fill = "#C4D600"),
                                    element_rect(fill = "#C8102E"),
                                    element_rect(fill = "#C8102E")
                )))+
  theme_bw()+
  theme(legend.position="none",
        strip.text.x = element_text(color="#F1F1F1",size=13),
        plot.title=element_text(size=24,hjust=.5),
        plot.subtitle=element_text(size=12,hjust=.5),
        axis.text.y = element_text(size=12,face="bold"),
        axis.text.x = element_text(size=10,face="bold",angle=90),
        plot.background = element_rect(fill = "#F1F1F1"),
        axis.title=element_blank(),
        plot.margin=margin(1,1,1,1,"cm"))
ggsave("2024_attendance_line.png",width=12,height=12,dpi=320)













































