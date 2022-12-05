library(jsonlite)
library(httr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(DT)

#-----------   Read json file  -------------------------#
tl_lol <- read_json(path="E:/challenges/team_liquid/tl_lol.json", simplifyVector = TRUE)
tl_vid <- read_json(path="E:/challenges/team_liquid/tl_vid.json", simplifyVector = TRUE)

#-------------------------------------------------------------------#
#  Get video id from json file, and put in a data frame             #
#-------------------------------------------------------------------#
videoid <- as_tibble(tl_lol$items$id$videoId) %>%
           rename(videoid = value)

#-------------------------------------------------------------------#
#  Get the video's metadata from json file and put in data frame    #
#-------------------------------------------------------------------#
video_meta <- as_tibble(tl_lol$items$snippet) %>%
              select(publishedAt,channelId,title)

#--------------------------------------------------------------------#
#  Append video id and metadata to a single data frame               #
#--------------------------------------------------------------------#
tl_vids <- bind_cols(videoid,video_meta) %>%
           mutate(short_title = str_sub(title,1,15))

#---------------------------------------------------------------------#
#   Individual video statistics                                       #
#---------------------------------------------------------------------#
vids_id <- as_tibble(tl_vid$items$id) %>%
            rename(videoid = value)
vids_stats <- as_tibble(tl_vid$items$statistics) %>%
              mutate(views = as.integer(viewCount),
                     likes = as.integer(likeCount),
                     comments = as.integer(commentCount)) %>%
              select(views,likes,comments)

#----------------------------------------------------------------------#
#  Append video id to statistics to form a single data frame           #
#----------------------------------------------------------------------#
vids_all <- bind_cols(vids_id,vids_stats)

#----------------------------------------------------------------------#
#   Create engagement variables for engagement metric                  #
#----------------------------------------------------------------------#
vid_engagement <- vids_all %>%
                  mutate(engage_likes = likes/views) %>%
                  mutate(engage_comments = comments/views)

#-----------------------------------------------------------------------#
#  Principal components model                                           #
#-----------------------------------------------------------------------#
engagement <- vid_engagement %>%
              select(engage_likes,engage_comments)

pcamodel <- princomp(engagement,cor=TRUE,scores=TRUE)
scores <- as_tibble(pcamodel$scores) %>%
          rename(rawscore = 1) %>%
          select(rawscore)

#----------  Append video id to scores -------------------------------#
engagement_scores <- bind_cols(vids_id,scores)

#---------------------------------------------------------------------#
#  Complete data frame with all data columns                          #
#---------------------------------------------------------------------#

#------  Engagement scores & statistics  ----------------------#
engage_stats <- inner_join(vid_engagement,engagement_scores,by=c("videoid"))

#---------   Add all metadata  ---------------------------------#
engagement_all <- inner_join(engage_stats,tl_vids,by=c("videoid")) %>%
                  arrange(desc(rawscore))

#----------------------------------------------------------------------#
#  Zero One min max normalization for the scores                       #
#----------------------------------------------------------------------#
minscore <- min(engagement_all$rawscore)
maxscore <- max(engagement_all$rawscore)

engagement_df <- engagement_all %>%
                 mutate(engagement_score = (rawscore-minscore)/(maxscore - minscore)) 

#-----------------------------------------------------------------------#
#  Check the percentile range for views                                 #
#-----------------------------------------------------------------------#

viewpct <- quantile(engagement_df$views,  probs = c(5, 10, 25, 50, 75, 90, 95,  NA)/100)

#------   Top 10 percent of views  ------------------#
popularvids <- viewpct[6]

popular_videos <- engagement_df %>%
                  filter(views >= popularvids) %>%
                  select(title,views,likes,comments)

#----------   Interactive data table -----------------------#
datatable(popular_videos)

                          
#-----------------------------------------------------------------------#
#   Horizontal lollipop ggplot                                          #
#-----------------------------------------------------------------------#
m <- ggplot(engagement_df, aes(x=short_title, y=engagement_score)) +
     geom_segment( aes(x=short_title, xend=short_title, y=0, yend=engagement_score), color="skyblue") +
     geom_point( color="blue", size=4, alpha=0.6) +
     theme_light() +
     coord_flip() +
     theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()
      )

m
