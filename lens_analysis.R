# Lens Analysis
library(dplyr)
library(scales)
library(magick)
library(ggdark)
library(cowplot)
library(lubridate)
library(extrafont)
library(showtext)
library(tidyverse)
library(RColorBrewer)
loadfonts()
# disable scientific notation
options(scipen=999)
# import lens logo to add to plots
lens_png = image_read("lens_logo.png")
# font
# font_add_google("Average Sans", "average")
# showtext_auto()


# Daily Stats -------------------------------------------------------------

daily_stats = read_csv('data/lens_stats_daily.csv')

# Visualize data
totals_viz = ggplot(daily_stats, aes(x=day)) + 
  geom_line(aes(y = totalPosts, colour = "Posts"), size=1.2) +
  geom_line(aes(y = totalComments, colour = "Comments"), size=1.2) + 
  # geom_line(aes(y = totalProfiles, colour = "Profiles")) + 
  # geom_line(aes(y = totalMirror, colour = "Mirrors")) +
  labs(color = 'Type') +
  # geom_line(aes(y = totalPublications, colour = "Total Publications")) + 
  dark_theme_minimal() +
  ggtitle('Comment and Post Counts',
          subtitle=paste0('From ', as.Date(min(daily_stats$day)), ' To ', as.Date(max(daily_stats$day)))) +
  ylab('Volume (USD)') +
  xlab('Date') +
  scale_y_continuous(labels=comma) +
  theme(text=element_text(size=12,  family="Comic Sans MS"),
        axis.title.y=element_blank(),
        axis.title.x=element_blank()) + 
  scale_colour_manual(values = c("#00501f", "#abfe2d"))
# add logo
totals_viz = ggdraw() +
  draw_plot(totals_viz) +
  draw_image(lens_png, x = 0.4, y = 0.36, scale = 0.09)
totals_viz

# Just profiles
profiles_data = filter(daily_stats, is.na(totalProfiles)==F) 
totals_viz = ggplot(profiles_data, aes(x=day,y=totalProfiles)) + 
  geom_line(color='#0ac255', size=1.2) +
  dark_theme_minimal() +
  ggtitle('Profiles Count',
          subtitle=paste0('From ', as.Date(min(profiles_data$day)), ' To ', as.Date(max(profiles_data$day)))) +
  ylab('Accounts') +
  xlab('Date') +
  scale_y_continuous(labels=comma) +
  theme(text=element_text(size=12,  family="Comic Sans MS"),
        axis.title.y=element_blank(),
        axis.title.x=element_blank()) 
# add logo
totals_viz = ggdraw() +
  draw_plot(totals_viz) +
  draw_image(lens_png, x = 0.4, y = 0.25, scale = 0.09)
totals_viz


# Activity by platform ----------------------------------------------------

posts = read_csv('data/posts_unioned_oct17.csv')

# number of posts by platform over time (cumulative sum)
posts_platform = posts %>%
  arrange(timestamp)%>% 
  mutate(date = as.Date(as.POSIXct(timestamp, origin='1970-01-01'))) %>% 
  group_by(date, app_id) %>% 
  summarize(daily_posts = n()) %>% 
  group_by(app_id) %>% 
  mutate(posts_cumulative = cumsum(daily_posts), max_posts = max(posts_cumulative)) %>% 
  filter(is.na(app_id)==F, max_posts > 300)
# viz
totals_viz = ggplot(posts_platform, aes(x=date, y=posts_cumulative, color=app_id)) + 
  geom_line(size=1.2) +
  labs(color = 'App') +
  # geom_line(aes(y = totalPublications, colour = "Total Publications")) + 
  dark_theme_minimal() +
  ggtitle('Posts by Platform (at least 300 posts)',
          subtitle=paste0('From ', as.Date(min(posts_platform$date)), ' To ', as.Date(max(posts_platform$date)))) +
  ylab('Volume (USD)') +
  xlab('Date') +
  scale_y_continuous(labels=comma) +
  theme(text=element_text(size=12,  family="Comic Sans MS"),
        axis.title.y=element_blank(),
        axis.title.x=element_blank()) #+ 
  # scale_colour_manual(values = c("#00501f", "#abfe2d"))
# add logo
totals_viz = ggdraw() +
  draw_plot(totals_viz) +
  draw_image(lens_png, x = 0.4, y = 0.36, scale = 0.09)
totals_viz

comments = read_csv('data/comments_unioned_oct17.csv')

# number of posts by platform over time (cumulative sum)
comments_platform = comments %>%
  arrange(timestamp)%>% 
  mutate(date = as.Date(as.POSIXct(timestamp, origin='1970-01-01'))) %>% 
  group_by(date, app_id) %>% 
  summarize(daily_posts = n()) %>% 
  group_by(app_id) %>% 
  mutate(posts_cumulative = cumsum(daily_posts), max_posts = max(posts_cumulative)) %>% 
  filter(is.na(app_id)==F, max_posts > 300)
# viz
totals_viz = ggplot(comments_platform, aes(x=date, y=posts_cumulative, color=app_id)) + 
  geom_line(size=1.2) +
  labs(color = 'App') +
  # geom_line(aes(y = totalPublications, colour = "Total Publications")) + 
  dark_theme_minimal() +
  ggtitle('Comments by Platform (at least 300 comments)',
          subtitle=paste0('From ', as.Date(min(posts_platform$date)), ' To ', as.Date(max(posts_platform$date)))) +
  ylab('Volume (USD)') +
  xlab('Date') +
  scale_y_continuous(labels=comma) +
  theme(text=element_text(size=12,  family="Comic Sans MS"),
        axis.title.y=element_blank(),
        axis.title.x=element_blank()) #+ 
# scale_colour_manual(values = c("#00501f", "#abfe2d"))
# add logo
totals_viz = ggdraw() +
  draw_plot(totals_viz) +
  draw_image(lens_png, x = 0.4, y = 0.36, scale = 0.09)
totals_viz


# Storage Analysis --------------------------------------------------------

# ADD


# Sentiment Analysis ----------------------------------------------------------

nlp_posts = read_csv('data/nlp_data_posts.csv')

nlp_posts = nlp_posts %>% filter(is.na(post_sentiment_score)==F)

# Average sentiment over time
nlp_posts_viz = nlp_posts %>%
  arrange(timestamp)%>% 
  mutate(date = as.Date(as.POSIXct(timestamp, origin='1970-01-01'))) %>% 
  group_by(date) %>% 
  summarize(avg_sentiment = mean(post_sentiment_score))

ggplot(nlp_posts_viz, aes(x=date, y=avg_sentiment)) + geom_line()

# viz
totals_viz = ggplot(nlp_posts_viz, aes(x=date, y=avg_sentiment, color=avg_sentiment)) + 
  geom_line(size=0.8) +
  geom_point(size=1.2) +
  labs(color = 'Sentiment') +
  # geom_line(aes(y = totalPublications, colour = "Total Publications")) + 
  dark_theme_minimal() +
  ggtitle('Posts Average Daily Sentiment',
          subtitle=paste0('From ', as.Date(min(posts_platform$date)), ' To ', as.Date(max(posts_platform$date)))) +
  ylab('Daily Average Sentiment') +
  xlab('Date') +
  scale_y_continuous(labels=comma) +
  theme(text=element_text(size=12,  family="Comic Sans MS"),
        # axis.title.y=element_blank(),
        axis.title.x=element_blank()) + 
  scale_colour_continuous(low = "#ba0101",
                     high = "#abfe2d")
# add logo
totals_viz = ggdraw() +
  draw_plot(totals_viz) +
  draw_image(lens_png, x = 0.4, y = 0.36, scale = 0.09)
totals_viz



# Sentiment by category - posts ---------------------------------------------------

# only include those with good confidence
nlp_posts_cat = nlp_posts %>% filter(post_nlp_category_confidence > 0.5, is.na(post_nlp_category_confidence)==F)
# only keep first category
nlp_posts_cat = nlp_posts_cat %>% mutate(category_trimmed = str_split(post_nlp_category, "/"))
nlp_posts_cat$category_trimmed_one = NA
# SLOW (but necessary):
for (i in 1:nrow(nlp_posts_cat)){
  print(i)
  nlp_posts_cat[i,]$category_trimmed_one = nlp_posts_cat[i,]$category_trimmed[[1]][2]
}

# Average sentiment by category
nlp_posts_viz = nlp_posts_cat %>%
  group_by(category_trimmed_one) %>% 
  summarize(avg_sentiment = mean(post_sentiment_score),
            count = n())
# check results
nlp_posts_viz = nlp_posts_viz %>% arrange(desc(count)) %>% head(10)
# viz
viz = ggplot(nlp_posts_viz, aes(x=reorder(category_trimmed_one, avg_sentiment), y=avg_sentiment, fill=count)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  coord_flip() + 
  scale_fill_continuous(type = "viridis") + 
  xlab('Predicted Category') +
  ylab('Average Sentiment') +
  labs(fill = 'Count') +
  scale_y_continuous(labels=comma) +
  theme(text=element_text(size=12,  family="Comic Sans MS")) +
  dark_theme_minimal() +
  ggtitle('Average Sentiment by Predicted Category') +
  theme(text=element_text(size=12,  family="Comic Sans MS"))
# add logo
viz = ggdraw() +
  draw_plot(viz) +
  draw_image(lens_png, x = 0.4, y = 0.36, scale = 0.09)
viz


# Sentiment by topic mentioned --------------------------------------------

nlp_posts_topic = nlp_posts %>% mutate(topic = case_when(
  grepl("btc",content) | grepl("BTC",content) | grepl("Bitcoin",content) ~ "Bitcoin",
  grepl("eth",content) | grepl("ETH",content) | grepl("Ethereum",content) ~ "Ethereum",
  # grepl("solana",content) | grepl("SOL",content) | grepl("Solana",content) ~ "Solana",
  # grepl("merge",content) | grepl("Merge",content) ~ "Ethereum Merge",
  grepl("lens",content) | grepl("Lens",content) ~ "Lens",
  grepl("inflation",content) | grepl("Inflation",content) ~ "Inflation",
  grepl("democrat",content) | grepl("Democrat",content) ~ "Democrat",
  grepl("republican",content) | grepl("Republican",content) ~ "Republican",
  grepl("ukrain",content) | grepl("Ukrain",content) ~ "Ukraine",
  grepl("russia",content) | grepl("Russia",content) ~ "Russia",
  # grepl("photo",content) | grepl("Photo",content) ~ "Photography",
  grepl("blockchain",content) | grepl("Blockchain",content) ~ "Blockchain",
  grepl("crypto",content) | grepl("Crypto",content) ~ "Crypto",
  grepl("travel",content) | grepl("Travel",content) ~ "Travel",
  grepl("twitter",content) | grepl("Twitter",content) ~ "Twitter",
  grepl("elon",content) | grepl("Elon",content) | grepl("musk",content) | grepl("Musk",content) ~ "Elon Musk",
  grepl("vitalik",content) | grepl("Vitalik",content) | grepl("Buterin",content) | grepl("Buterin",content) ~ "Vitalik Buterin",
  grepl("metaverse",content) | grepl("Metaverse",content) ~ "Metaverse",
  TRUE ~ "Other Topics"))

# summarize
nlp_posts_topic_viz = nlp_posts_topic %>% 
  group_by(topic) %>% 
  summarize(avg_sentiment = mean(post_sentiment_score),
            count = n()) %>% 
  arrange(desc(count)) %>% 
  head(10)

# visualize
viz = ggplot(nlp_posts_topic_viz, aes(x=reorder(topic, avg_sentiment), y=avg_sentiment, fill=count)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  coord_flip() + 
  scale_fill_continuous(type = "viridis") + 
  xlab('Topic') +
  ylab('Average Sentiment') +
  labs(fill = 'Count') +
  scale_y_continuous(labels=comma) +
  theme(text=element_text(size=12,  family="Comic Sans MS")) +
  dark_theme_minimal() +
  ggtitle('Average Sentiment by Topic Mentioned') +
  theme(text=element_text(size=12,  family="Comic Sans MS"))
# add logo
viz = ggdraw() +
  draw_plot(viz) +
  draw_image(lens_png, x = 0.4, y = 0.36, scale = 0.09)
viz



# Posts by storage medium  --------------------------------------------------------------------

storage_p = posts %>% select(contentURI, app_id) %>% mutate(Type='posts')
storage_c = comments %>% select(contentURI, app_id) %>% mutate(Type='comments')
# union
storage = rbind(storage_p, storage_c)

# label storage medium
storage_viz = storage %>% 
  mutate(storage_medium = case_when(
    grepl("arweave",contentURI) ~ "Arweave",
    grepl("ipfs",contentURI)~ "IPFS",
    grepl("lens.phaver",contentURI)~ "Phaver API",
    TRUE ~ substr(contentURI, 1, 20)))

# totals
storage_viz = storage_viz %>% group_by(storage_medium, Type) %>% count() %>% arrange(desc(n)) %>% filter(n > 200)

# viz
viz = ggplot(storage_viz, aes(fill=Type, y=n, x=storage_medium)) + 
  geom_bar(position="dodge", stat="identity") + 
  # coord_flip() + 
  xlab('Storage Medium') +
  ylab('Counts') +
  scale_y_continuous(labels=comma) +
  theme(text=element_text(size=12,  family="Comic Sans MS")) +
  dark_theme_minimal() +
  ggtitle('Counts by Storage Medium') +
  theme(text=element_text(size=12,  family="Comic Sans MS"))
# add logo
viz = ggdraw() +
  draw_plot(viz) +
  draw_image(lens_png, x = 0.4, y = 0.36, scale = 0.09)
viz


# Percentage accessible content by storage medium -------------------------

# mark those accessible vs not accessible
storage_viz = storage %>% 
  mutate(accessed = case_when(
    is.na(app_id)==T ~ 0,
    TRUE ~ 1),
    storage_medium = case_when(
      grepl("arweave",contentURI) ~ "Arweave",
      grepl("ipfs",contentURI)~ "IPFS",
      grepl("lens.phaver",contentURI) ~ "Phaver API",
      TRUE ~ substr(contentURI, 1, 20))) %>% 
  group_by(storage_medium) %>% 
  summarize(success_rate = sum(accessed)/n(),
            count = n()) %>% 
  arrange(desc(count)) %>% 
  filter(count > 200)

# viz
positions = c("Phaver API", "Arweave", "IPFS")

viz = ggplot(storage_viz, aes(y=success_rate, x=storage_medium, fill=success_rate)) + 
  geom_bar(position="dodge", stat="identity") + 
  # coord_flip() + 
  xlab('Storage Medium') +
  scale_x_discrete(limits = positions) +
  ylab('Successfully Accessed (in 0.5 seconds)') +
  scale_fill_continuous(low = "#ba0101",
                        high = "#abfe2d") +
  labs(fill = 'Success Rate') +
  scale_y_continuous(labels=scales::percent) +
  theme(text=element_text(size=12,  family="Comic Sans MS")) +
  dark_theme_minimal() +
  ggtitle('Percent of Successfully Retrieved Content by Storage Medium') +
  theme(text=element_text(size=12,  family="Comic Sans MS"))
# add logo
viz = ggdraw() +
  draw_plot(viz) +
  draw_image(lens_png, x = 0.4, y = 0.36, scale = 0.09)
viz

