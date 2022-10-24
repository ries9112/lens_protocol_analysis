library(ghql)
library(tidyverse)
setwd("~/Documents/GitHub/web3fridays/september_socialmedia")

# Connect to endpoint:
graphql_conn = GraphqlClient$new(
  url = "https://api.thegraph.com/subgraphs/name/rickyesclapon/matic-block-timetable"
)
# Initialize empty query
qry = Query$new()
# Example Query
qry$query('mydata', '{
  dayBlocks(where:{day_gt:1652803936, endBlockNum_lt:33243199}, orderBy: day, orderDirection: desc, first:1000){
    day
    endBlockNum
  }
}')

# Query to object
x = graphql_conn$exec(qry$queries$mydata)
# parse query
blocks_daily = jsonlite::fromJSON(x)
# extract result
blocks_daily = blocks_daily$data$dayBlocks
# convert day to timestamp
blocks_daily$day = as.POSIXct(as.numeric(blocks_daily$day), origin = '1970-01-01')

# Stats data - template ---------------------------------------------

# Connect to endpoint:
graphql_conn = GraphqlClient$new(
  url = "https://api.thegraph.com/subgraphs/name/rtomas/lens-subgraph"
)
# Initialize empty query
qry = Query$new()
# Example Query
qry$query('mydata', '{
          stats{
            totalProfiles
            totalPosts
            totalComments
            totalMirror
            totalPublications
          }
}')

# Query to object
x = graphql_conn$exec(qry$queries$mydata)
# parse query
stats_daily = jsonlite::fromJSON(x)
# extract result
stats_daily = stats_daily$data$stats

# delete all rows (template for columns)
stats_daily = stats_daily[0,]


# Stats data for loop -----------------------------------------------

# Iterate over dates
for (b in as.numeric(blocks_daily$endBlockNum)){
  print(paste("Now on block:", b))
  # Connect to endpoint:
  graphql_conn = GraphqlClient$new(
    url = "https://api.thegraph.com/subgraphs/name/rtomas/lens-subgraph"
  )
  # Initialize empty query
  qry = Query$new()
  # Example Query
  qry$query('mydata', paste0('{
  stats(block:{number:',b,'}, first:1000){
    totalProfiles
    totalPosts
    totalComments
    totalMirror
    totalPublications
  }
}'))
  
  # Query to object
  x = graphql_conn$exec(qry$queries$mydata)
  # parse query
  temp = jsonlite::fromJSON(x)
  # extract result
  temp = temp$data$stats
  # add as of date
  temp$as_of_block =  filter(blocks_daily, endBlockNum == b)$endBlockNum
  temp$day = filter(blocks_daily, endBlockNum == b)$day
  
  # union data
  stats_daily = rbind(stats_daily, temp)
} # closes blocks for loop



# New subgraph data -------------------------------------------------------

# Connect to endpoint:
graphql_conn = GraphqlClient$new(
  url = "https://api.thegraph.com/subgraphs/name/rickyesclapon/matic-block-timetable"
)
# Initialize empty query
qry = Query$new()
# Example Query
qry$query('mydata', '{
  dayBlocks(where:{endBlockNum_gt:33243199}, orderBy: day, orderDirection: desc, first:1000){
    day
    endBlockNum
  }
}')

# Query to object
x = graphql_conn$exec(qry$queries$mydata)
# parse query
blocks_daily = jsonlite::fromJSON(x)
# extract result
blocks_daily = blocks_daily$data$dayBlocks
# convert day to timestamp
blocks_daily$day = as.POSIXct(as.numeric(blocks_daily$day), origin = '1970-01-01')


# Stats data for loop -----------------------------------------------

# get max 
max_posts = max(as.numeric(stats_daily$totalPosts))
max_comments = max(as.numeric(stats_daily$totalComments))
max_publications = max(as.numeric(stats_daily$totalPublications))
# convert types
stats_daily = mutate(stats_daily, 
                     totalProfiles=as.numeric(totalProfiles),
                     totalPosts=as.numeric(totalPosts),
                     totalComments=as.numeric(totalComments),
                     totalMirror=as.numeric(totalMirror),
                     totalPublications=as.numeric(totalPublications))
# Iterate over dates
for (b in as.numeric(blocks_daily$endBlockNum)){
  print(paste("Now on block:", b))
  # Connect to endpoint:
  graphql_conn = GraphqlClient$new(
    url = "https://api.thegraph.com/subgraphs/name/rickyesclapon/lens-protocol-posts-only"
  )
  # Initialize empty query
  qry = Query$new()
  # Example Query
  qry$query('mydata', paste0('{
  stats(block:{number:',b,'}, first:1000){
    totalPosts
    totalComments
    totalPublications
  }
}'))
  
  # Query to object
  x = graphql_conn$exec(qry$queries$mydata)
  # parse query
  temp = jsonlite::fromJSON(x)
  # extract result
  temp = temp$data$stats
  # add previous totals
  temp$totalPosts = as.numeric(temp$totalPosts)+max_posts
  temp$totalComments = as.numeric(temp$totalComments)+max_comments
  temp$totalPublications = as.numeric(temp$totalPublications)+max_publications
  # add missing columns
  temp$totalProfiles = NA
  temp$totalMirror = NA
  # add as of date
  temp$as_of_block =  filter(blocks_daily, endBlockNum == b)$endBlockNum
  temp$day = filter(blocks_daily, endBlockNum == b)$day
  
  # union data
  stats_daily = rbind(stats_daily, temp)
} # closes blocks for loop



# Save data -----------------------------------------------------------

# prep data
stats_daily = arrange(stats_daily, as_of_block)

# write file
write.csv(stats_daily, 'lens_stats_daily.csv', row.names = F)




