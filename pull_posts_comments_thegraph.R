# Data Pulls from new subgraph

library(ghql)
library(tidyverse)
library(httr)
library(R.utils) #imports withTimeout
setwd("~/Documents/GitHub/web3fridays/september_socialmedia")

# set subgraph url
subgraph_url = 'https://api.thegraph.com/subgraphs/name/rickyesclapon/lens-protocol-posts-only'
# connect to the endpoint
con = ghql::GraphqlClient$new(
  url = subgraph_url
)
# initialize a new query
graphql_request = ghql::Query$new()
# query
graphql_request$query('mydata', paste0('{
  comments(orderBy: timestamp, orderDirection: desc){
    id
    pubId
    contentURI
    profileIdPointed
    pubIdPointed
    timestamp
  }
}'))
# Run query (pull data)
comments = con$exec(graphql_request$queries$mydata)
# convert results to JSON
comments = jsonlite::fromJSON(comments)
# extract result
comments = tibble::as_tibble(comments$data$comments) #ideally could do something like data$data[entity]
# extract profile id and handle
# comments$profile_id = comments$fromProfile$profileId
# comments = select(comments, -fromProfile)

# make adjustments for ipfs.infura.io to new url of infura-ipfs.io & ipfs://
comments = mutate(comments, contentURI=gsub("ipfs.infura.io", "infura-ipfs.io", contentURI),
                  contentURI=gsub("ipfs://", "https://infura-ipfs.io/ipfs/", contentURI),
                  contentURI=gsub(".json", "", contentURI))

# get content from contentURI 
# set content to empty string
comments$content = ""
comments$app_id = ""
comments$locale = ""
comments$content_focus = ""
# iterate through rows
for(i in 1:nrow(comments)){
  print(i)
  print(comments$contentURI[i])
  # Sys.sleep(1)
  tryCatch({ # when ipfs file takes more than 0.5 seconds ignore
    r = withTimeout(GET(comments$contentURI[i]),timeout = 0.5)
    comments$content[i] = jsonlite::fromJSON(content(r, "text"))$content
    comments$app_id[i] = jsonlite::fromJSON(content(r, "text"))$appId
    comments$locale[i] = jsonlite::fromJSON(content(r, "text"))$locale
    comments$content_focus[i] = jsonlite::fromJSON(content(r, "text"))$mainContentFocus
  },error=function(e) {})
}

# Iterate over more dates
# write.csv(comments, 'comments_oct17.csv', row.names = F)
comments = read_csv('comments_oct17.csv')
# comments = filter(comments, timestamp > 1653807990)
max_timestamp = max(as.numeric(comments$timestamp),na.rm=T)
min_timestamp = min(as.numeric(comments$timestamp),na.rm=T)
min_timestamp_limit = 1663468947
while(min(comments$timestamp, na.rm = T) > min_timestamp_limit){
  print(min_timestamp)
  # connect to the endpoint
  con = ghql::GraphqlClient$new(
    url = subgraph_url
  )
  # initialize a new query
  graphql_request = ghql::Query$new()
  # query
  graphql_request$query('mydata', paste0('{
  comments(orderBy: timestamp, orderDirection: desc, where:{timestamp_lt:',min_timestamp,'}, first:1000){
    id
    pubId
    contentURI
    profileIdPointed
    pubIdPointed
    timestamp
  }
  }'))
  # Run query (pull data)
  temp = con$exec(graphql_request$queries$mydata)
  # convert results to JSON
  temp = jsonlite::fromJSON(temp)
  # extract result
  temp = tibble::as_tibble(temp$data$comments) #ideally could do something like data$data[entity]
  # extract profile id and handle
  # temp$profile_id = temp$fromProfile$profileId
  # temp = select(temp, -fromProfile)
  
  # make adjustments for ipfs.infura.io to new url of infura-ipfs.io & ipfs://
  temp = mutate(temp, contentURI=gsub("ipfs.infura.io", "infura-ipfs.io", contentURI),
                contentURI=gsub("ipfs://", "https://infura-ipfs.io/ipfs/", contentURI),
                contentURI=gsub(".json", "", contentURI))
  
  # get content from contentURI 
  # set content to empty string
  temp$content = ""
  temp$app_id = ""
  temp$locale = ""
  temp$content_focus = ""
  # iterate through rows
  for(i in 1:nrow(temp)){
    print(i)
    print(temp$contentURI[i])
    # Sys.sleep(1)
    tryCatch({ # when ipfs file takes more than 0.5 seconds ignore
      r = withTimeout(GET(temp$contentURI[i]),timeout = 1.2)
      temp$content[i] = jsonlite::fromJSON(content(r, "text"))$content
      temp$app_id[i] = jsonlite::fromJSON(content(r, "text"))$appId
      temp$locale[i] = jsonlite::fromJSON(content(r, "text"))$locale
      temp$content_focus[i] = jsonlite::fromJSON(content(r, "text"))$mainContentFocus
    },error=function(e) {})
  }
  # union datasets
  comments = rbind(comments, temp)
  # reset min timestamp
  min_timestamp = min(as.numeric(temp$timestamp),na.rm=T)
  # write csv
  write_csv(comments, 'comments_oct17.csv')
  # read csv (to enable parallel processing)
  comments = read_csv('comments_oct17.csv')
}

# convert timestamp
comments = dplyr::mutate(comments, timestamp=as.POSIXct(as.numeric(timestamp), origin="1970-01-01"))

# write csv
write_csv(comments, 'comments_oct17.csv')


# Posts -------------------------------------------------------------------

# connect to the endpoint
con = ghql::GraphqlClient$new(
  url = subgraph_url
)
# initialize a new query
graphql_request = ghql::Query$new()
# query
graphql_request$query('mydata', paste0('{
  posts(orderBy: timestamp, orderDirection: desc){
    id
    pubId
    contentURI
    timestamp
  }
}'))
# Run query (pull data)
posts = con$exec(graphql_request$queries$mydata)
# convert results to JSON
posts = jsonlite::fromJSON(posts)
# extract result
posts = tibble::as_tibble(posts$data$posts) #ideally could do something like data$data[entity]
# extract profile id and handle
# posts$profile_id = posts$fromProfile$profileId
# posts = select(posts, -fromProfile)

# make adjustments for ipfs.infura.io to new url of infura-ipfs.io & ipfs://
posts = mutate(posts, contentURI=gsub("ipfs.infura.io", "infura-ipfs.io", contentURI),
               contentURI=gsub("ipfs://", "https://infura-ipfs.io/ipfs/", contentURI),
               contentURI=gsub(".json", "", contentURI))

# get content from contentURI 
# set content to empty string
posts$content = ""
posts$app_id = ""
posts$locale = ""
posts$content_focus = ""
# iterate through rows
for(i in 1:nrow(posts)){
  print(i)
  print(posts$contentURI[i])
  # Sys.sleep(1)
  tryCatch({ # when ipfs file takes more than 0.5 seconds ignore
    r = withTimeout(GET(posts$contentURI[i]),timeout = 0.5)
    posts$content[i] = jsonlite::fromJSON(content(r, "text"))$content
    posts$app_id[i] = jsonlite::fromJSON(content(r, "text"))$appId
    posts$locale[i] = jsonlite::fromJSON(content(r, "text"))$locale
    posts$content_focus[i] = jsonlite::fromJSON(content(r, "text"))$mainContentFocus
  },error=function(e) {})
}


# Iterate over more dates
posts = read_csv('posts_oct17.csv')
# posts = posts %>% filter(timestamp > 1653803936)
max_timestamp = max(as.numeric(posts$timestamp),na.rm=T)
min_timestamp = min(as.numeric(posts$timestamp),na.rm=T)
min_timestamp_limit = 1663468947
while(min(posts$timestamp, na.rm=T) > min_timestamp_limit){
  print(min_timestamp)
  # connect to the endpoint
  con = ghql::GraphqlClient$new(
    url = subgraph_url
  )
  # initialize a new query
  graphql_request = ghql::Query$new()
  # query
  graphql_request$query('mydata', paste0('{
  posts(orderBy: timestamp, orderDirection: desc, where:{timestamp_lt:',min_timestamp,'}, first:1000){
    id
    pubId
    contentURI
    timestamp
  }
  }'))
  # Run query (pull data)
  temp = con$exec(graphql_request$queries$mydata)
  # convert results to JSON
  temp = jsonlite::fromJSON(temp)
  # extract result
  temp = tibble::as_tibble(temp$data$posts)
  # extract profile id and handle
  # temp$profile_id = temp$fromProfile$profileId
  # temp = select(temp, -fromProfile)
  
  # make adjustments for ipfs.infura.io to new url of infura-ipfs.io & ipfs://
  temp = mutate(temp, contentURI=gsub("ipfs.infura.io", "infura-ipfs.io", contentURI),
                contentURI=gsub("ipfs://", "https://infura-ipfs.io/ipfs/", contentURI),
                contentURI=gsub(".json", "", contentURI))
  
  # get content from contentURI 
  # set content to empty string
  temp$content = ""
  temp$app_id = ""
  temp$locale = ""
  temp$content_focus = ""
  # iterate through rows
  for(i in 1:nrow(temp)){
    print(i)
    print(temp$contentURI[i])
    # Sys.sleep(1)
    tryCatch({ # when ipfs file takes more than 0.5 seconds ignore
      r = withTimeout(GET(temp$contentURI[i]),timeout = 1)
      temp$content[i] = jsonlite::fromJSON(content(r, "text"))$content
      temp$app_id[i] = jsonlite::fromJSON(content(r, "text"))$appId
      temp$locale[i] = jsonlite::fromJSON(content(r, "text"))$locale
      temp$content_focus[i] = jsonlite::fromJSON(content(r, "text"))$mainContentFocus
    },error=function(e) {})
  }
  # union datasets
  posts = rbind(posts, temp)
  # reset min timestamp
  min_timestamp = min(as.numeric(temp$timestamp),na.rm=T)
  # write csv
  write.csv(posts, 'posts_oct17.csv',row.names = F)
  # read csv (to enable parallel processing)
  posts = read.csv('posts_oct17.csv')
}

# convert timestamp
posts = dplyr::mutate(posts, timestamp=as.POSIXct(as.numeric(timestamp), origin="1970-01-01"))

# write csv
write_csv(posts, 'posts_oct17.csv')






