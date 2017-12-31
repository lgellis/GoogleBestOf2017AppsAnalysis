install.packages("rvest")
install.packages("plyr")
install.packages("alluvial")
install.packages("ggplot2")
install.packages("plotrix")
install.packages("treemap")
install.packages("plotly")


library(rvest)
library(plyr)
library(alluvial)
library(ggplot2)
library(plotrix)
library(treemap)
library(plotly)
#source for scraping: https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/

## Talk about screen scraping 
## Talk about functions

###########  CREATE Function for Screen Scrape ################

scrapeGoogleReviews <- function(url, categoryName ){
  #Specifying the url for desired website to be scrapped
  webpage <- read_html(url)
  df <-data.frame(
    app_title = html_text(html_nodes(webpage,'.id-app-title')),
    rating_count = html_text(html_nodes(webpage,'.rating-count')),
    download_count = html_text(html_nodes(webpage,'.download-count')),
    content_rating = html_text(html_nodes(webpage,'.content-rating-title')),
    write_up = html_text(html_nodes(webpage,'.editorial-snippet')),
    category = categoryName)
  df
  return(df)
}

###########  CALL Function for Screen Scrape ################

df1 <-scrapeGoogleReviews('https://play.google.com/store/apps/topic?id=campaign_editorial_apps_bestof2017&hl=en','Winner')
df2 <-scrapeGoogleReviews('https://play.google.com/store/apps/topic?id=campaign_editorial_apps_entertaining_bestof2017&hl=en','Most Entertaining')
df3 <-scrapeGoogleReviews('https://play.google.com/store/apps/topic?id=campaign_editorial_apps_social_bestof2017&hl=en','Best Social')
df4 <-scrapeGoogleReviews('https://play.google.com/store/apps/topic?id=campaign_editorial_apps_productivity_bestof2017&hl=en','Daily Helper')
df5 <-scrapeGoogleReviews('https://play.google.com/store/apps/topic?id=campaign_editorial_apps_innovative_bestof2017&hl=en','Most Innovative')
df6 <-scrapeGoogleReviews('https://play.google.com/store/apps/topic?id=campaign_editorial_apps_Hiddengem_bestof2017&hl=en','Hidden Gem')
df7 <-scrapeGoogleReviews('https://play.google.com/store/apps/topic?id=campaign_editorial_apps_kids_bestof2017&hl=en','Best for Kids')
df8 <-scrapeGoogleReviews('https://play.google.com/store/apps/topic?id=campaign_editorial_3002de9_apps_bestof_mostpopular2017&hl=en','Most Popular')

fulldf <- rbind(df1,df2, df3, df4, df5, df6, df7, df8)
fulldf

###########  Extra formatting ################

# Remove commas and convert to numeric
fulldf$rating_count_numeric <- as.numeric(gsub("[^0-9]", "", fulldf$rating_count))
fulldf$download_count_numeric <- as.numeric(gsub("[^0-9]", "", fulldf$download_count))

attach(fulldf)

#Binning by downloads
breaks <- c(0,10000,1000000,10000000, 100000000)
fulldf$download_total_ranking = findInterval(download_count_numeric,breaks)
fulldf

#Binning by rating totals
breaks2 <- c(10,100,1000,100000, 10000000)
fulldf$rating_total_ranking = findInterval(rating_count_numeric,breaks2)
fulldf
attach(fulldf)

# Add percent downloads for pie chart
totalDownload <- sum(download_count_numeric)
totalDownload
fulldf$percentDownloadApp <- round(download_count_numeric/totalDownload *100, 2)
attach(fulldf)
fulldf

###########  Visualize App Stats  ################

#pie

plot_ly(fulldf, labels =fulldf$app_title, values =ifelse(fulldf$percentDownloadApp>1,fulldf$percentDownloadApp,''), type = 'pie') %>%
  layout(title = 'Percentage of App Downloads by App',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



#treemap by category
treemap(fulldf, #Your data frame object
        index=c( "category", "app_title" ),  #A list of your categorical variables
        vSize = "download_count_numeric",  #This is your quantitative variable
        type="index", #Type sets the organization and color scheme of your treemap
        palette = "Blues",  #Select your color palette from the RColorBrewer presets or make your own.
        title="Treemap", #Customize your title
        fontsize.title = 14 #Change the font size of the title
)
#treemap without category
treemap(fulldf, #Your data frame object
        index=c("app_title" ),  #A list of your categorical variables
        vSize = "download_count_numeric",  #This is your quantitative variable
        type="index", #Type sets the organization and color scheme of your treemap
        palette = "Blues",  #Select your color palette from the RColorBrewer presets or make your own.
        title="Treemap", #Customize your title
        fontsize.title = 14 #Change the font size of the title
)

# Bar Plot of downloads by app
g <- ggplot(fulldf, aes(app_title, download_count_numeric))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="Bar Chart", 
       subtitle="Applications", 
       caption="Downloads of apps") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


## Bubble chart  for only those with downloads greater than 1000000

ggplot(data = fulldf[download_count_numeric>1000000, ], mapping = aes(x = category, y = rating_count_numeric)) +
  geom_point(aes(size = download_count_numeric), alpha = 1/3) +
  geom_point(aes(colour = download_count_numeric)) +
  geom_text(aes(label=app_title), size=2.5, vjust = -2) #vjust moves text up



attach(fulldf)
head(fulldf)


###

totalDownload



###########  Visualize Category Stats ################

totalDownload <- sum(fulldf$download_count_numeric)
totalDownload


#Some numerical summaries
statCatTable <- ddply(fulldf, c("category"), summarise,
                   N    = length(app_title), 
                   sumDownload  = sum(download_count_numeric),
                   sumOfRatingsCompleted = sum(rating_count_numeric),
                   minDownload = min(download_count_numeric),
                   maxDownload = max(download_count_numeric),
                   avgDownload = mean(download_count_numeric),
                   minRatingsCompleted = min(rating_count_numeric),
                   maxRatingsCompleted = max(rating_count_numeric),
                   avgRatingsCompleted = mean(rating_count_numeric),
                   sdDownload = sd(download_count_numeric),
                   sdRatings = sd(rating_count_numeric),
                   percentRatingPerDownload = round(sum(rating_count_numeric)/sum(download_count_numeric)*100,2),
                   percentDownload = round(sum(download_count_numeric)/totalDownload * 100,2)
)
statCatTable

attach(statCat)


# Looking at percent ratings per download

#look at the values
statCatTable[, c(1, 13) ]

#visualize percent ratings

#bar
ggplot(statCatTable, aes(x = factor(category), y=percentRatingPerDownload,fill=factor(category)) ) + 
  geom_bar(width = 1,stat="identity")

#circular - caution the use b/c often the middle is visually smallest
ggplot(statCatTable, aes(x = factor(category), y=percentRatingPerDownload,fill=factor(category)) ) + 
  geom_bar(width = 1,stat="identity")+coord_polar(theta = "y") 
#radial
radial.pie(statCatTable$percentRatingPerDownload, labels=statCatTable$category,radlab=FALSE, label.prop=1.2)

ggplot(data=fulldf,aes(x=download_count_numeric, group=category, fill=category)) + 
  geom_density(adjust=1.5, position="fill")

#complex dotplot

#pick r colors - http://data.library.virginia.edu/setting-up-color-palettes-in-r/
colorsList = brewer.pal(8, "Dark2")
colorsList

x <- fulldf
x$color[fulldf$category=='Winner'] <- "#1B9E77"
x$color[fulldf$category=='Most Innovative'] <- "#D95F02"
x$color[fulldf$category=='Best for Kids'] <- "#7570B3"
x$color[fulldf$category=='Best Social'] <- "#E7298A"
x$color[fulldf$category=='Daily Helper'] <- "#66A61E"
x$color[fulldf$category=='Hidden Gem'] <- "#E6AB02"
x$color[fulldf$category=='Most Entertaining'] <- "#A6761D"
x$color[fulldf$category=='Most Popular'] <- "#666666"

dotchart(fulldf$download_count_numeric,labels=app_title,cex=.7,groups= fulldf$category,
         main="Downloads by App",
         xlab="Downloads", gcolor="black", color=x$color) 

#visualize sum of downloads

treemap(statCatTable, #Your data frame object
        index=c("category"),  #A list of your categorical variables
        vSize = "sumDownload",  #This is your quantitative variable
        type="index", #Type sets the organization and color scheme of your treemap
        palette = "Set2",  #Select your color palette from the RColorBrewer presets or make your own.
        title="Treemap", #Customize your title
        fontsize.title = 14 #Change the font size of the title
)

#pie

p <- plot_ly(statCatTable, labels = ~category, values = ~percen, type = 'pie') %>%
  layout(title = 'United States Personal Expenditures by Categories in 1960',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p

# Bar Plot
g <- ggplot(statCatTable, aes(category, sumDownload))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="Bar Chart", 
       subtitle="Applications", 
       caption="Downloads of apps") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

###########  Visualize Cat + Content Summaries  ################

#What is driving the most downloads??
catsum <- ddply(fulldf, c("category", "content_rating","rating_total_ranking", "download_total_ranking"), summarise,
                N    = length(app_title),
                sumDownload = sum(download_count_numeric),
                sumRatings = sum(rating_count_numeric)
)

catsum

## Do visualization to see what is driving a high number of downloads

alluvial(catsum[c(1:2,4)] , freq=catsum$N,
         col = ifelse(catsum$download_total_ranking == 5, "green", "grey"),
         border = ifelse(catsum$download_total_ranking == 5, "green", "grey"),
         cex = 0.7
)

alluvial(catsum[c(1:2,4)] , freq=catsum$N,
         col = ifelse(catsum$content_rating == 'Teen', "blue", "grey"),
         border = ifelse(catsum$content_rating == 'Teen', "blue", "grey"),
         cex = 0.7
)
#Some numerical summaries
statTable <- ddply(fulldf, c("category", "content_rating","rating_total_ranking", "download_total_ranking"), summarise,
                   N    = length(app_title), 
                   sumDownload  = sum(download_count_numeric),
                   sumOfRatingsCompleted = sum(rating_count_numeric),
                   minDownload = min(download_count_numeric),
                   maxDownload = max(download_count_numeric),
                   avgDownload = mean(download_count_numeric),
                   minRatingsCompleted = min(rating_count_numeric),
                   maxRatingsCompleted = max(rating_count_numeric),
                   avgRatingsCompleted = mean(rating_count_numeric),
                   sdDownload = sd(download_count_numeric),
                   sdRatings = sd(rating_count_numeric),
                   percentRatingPerDownload = round(sum(rating_count_numeric)/sum(download_count_numeric)*100,2)
                   
)

statTable
attach(statTable)
head(statTable)

ggplot(data = statTable, mapping = aes(x = category, y = sumOfRatingsCompleted)) +
  geom_point(aes(size = sumDownload), alpha = 1/3) +
  geom_point(aes(colour = content_rating))

##Stackedbar by category
ggplot(catsum, aes(x=category, y=sumDownload, fill=content_rating)) + 
  geom_bar(stat="identity") +
  xlab("\nCategory") +
  ylab("Download Count\n") +
  theme_bw()
## Futures - could be used for sentiment analysis
