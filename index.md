#Analyzing Social Media Response#
###BY RACHEL FELDMAN, JENIFFER SOTO-PEREZ, AND CASEY SALVADOR###

##About Me##
![](http://i.imgur.com/2Mn2vgqt.jpg)My name is Rachel Feldman and I am a student at the College of Charleston.  I concentrate in creating and imagining audience experiences of the highest caliber in the arts starting from the first interaction online to the post-event sentiment.  The following project is a part of Dr. Paul Anderson's Data 101 class.  For questions or comments contact me via email at feldmanrm@g.cofc.edu

##The Competition##
From the competition website:
Dialogues on social media can provide tremendous insight into the behaviors, desires, pains, and thoughts of consumers. We'd like your help in developing a repeatable process that identifies, classifies, and extracts the underlying drivers of consumer financial conversations and comments in social media data. 

##Preparing the Data##
In order to pull useful information from the data given, we put the dataset in R studio.  We mined the tweets and posts through R Studio, getting rid of blank spaces, messages that did not tag a particular bank.  Additionally, we spell checked the information.  

##Approach and Methodology##

Once we prepared the tweets and posts for analysis, we used an agile methodology so we could test small amounts of data at a time to quicken the process. 
An agile methodology was chosen so we could start to answer smaller questions and work our way up to bigger ones that would lead to bigger and broader results.  Our methods could be repeated and compared for each bank. Through our process, we tested 1000 random samples of the entire data set and then 1000 random samples of only bank A to compare and contrast differences.
Below, find a graphical representation of the analytic and developmental approach taken.  It will show first cleaning the data where the team got rid of any additional room that was taking up space.  The team then tested the data overall to find positive and negative sentiment words as well as high-frequency words.  This gave us the framework to look at the data as a whole before looking into the context of each bank.  This step also allowed for the creation of two separate files for negative and positive posts.  
The team indexed each bank to then replicate the tests: positive and negative as well as word frequency. 
The results were then tested against each bank's and the data set as a whole.  Finally, we tried to pull conclusions about what our results may tell us about the bank industry's presence in social media as a whole.

![Method](http://i.imgur.com/XI5Eh29.jpg)

Our methodology mirrors the first three steps of the DIKW pyramid.  The pyramid shows data at the bottom, then information, knowledge, and wisdom.  With our approach, we are converting the data into information that means something.  With insights, comparing, and classification methods, we hope to convert the information into knowledge.  This knowledge we hope will be able to predict and make assumptions about the different banks and if supported for a time, later be applied to practices to be wisdom.  

![DIKW Pyramid](http://www.allthingy.com/wp-content/uploads/2014/07/Wisdom-Knowledge-Information-Data-Pyramid15.png)

###Approaching the Data: Banks within Social Media###
As the data given was real, it was important to find out more about banks' part in social media before we began cleaning, organizing and gaining insights from it.  As Social Balkers, a social media research facility and consulting firm submits trends yearly, their report was used to see where the bank posts lay within the span of the greater social media.  

First, engagement rate typically is determined by size:

![Engagement Rate by Page Size](http://www.socialbakers.com//storage/www/2014-mar/finding-er-3.png)

We then looked at the top banks facebook likes.  While we did not have the specific bank per bank ID, this information is useful to gage what percent of users are talking.  Through Facebook, we saw that Wells Fargo has 850,000 likes, Bank of America has 2.3 million likes. JPMorgan and Chase, and Citigroup, two other national banks do not have national facebook pages, thus their numbers are much lower -in the thousands- but have many more pages.

Comparing these numbers to the above graph, it is evident that each bank may be going in different directions of how much engagement they are getting. While Bank of America potentially has a low engagement of .28% and .11%, according to this graph pages like Citigroup and JPMorgan potentially has a higher engagement of .94% and .65% 

Next, Social Balkers published these graphs to show where different industries compare to each other. In post engagement and post interaction.

![](http://www.socialbakers.com//storage/www/2014-mar/finding-er-2.png)

![](http://www.socialbakers.com//storage/www/2014-mar/finding-er-5.png)

For the top graph, an engagement is counted if it is a direct comment, like or share, and then is divided by unique page impressions (the people who have seen it).

Interactions are straight likes, shares, and comments. According to these graphs, Finance has a relatively mid-level engagement rate, and a low overall interaction number.  This shows that fewer people overall are seeing finance's posts, but the ones who do are 0.30% likely to comment, like or share what they do see.

In approaching the data, our team took these numbers slightly into consideration to give the data more context.



##Social Conversation Drivers##

To find the social conversation drivers, we started to index the data.  First we did this by separating the positive and negative posts.  We then asked the questions of what started the conversation and what variables, from the given data, connect.

Questions that connect to this are:
* Is there a correlation between positive/ negative remarks on different days of the week or times of the month?  
* Is there a different sort of sentiment on twitter compared to facebook?

If given additional information, more questions that could be answered are:
* Is location a factor in and of these posts? 
* Can we geolocate, through social media, a branch that is doing really well vs poorly? 
* Does location drive a certain feeling toward one bank or another?  
* Is time is a factor on overall sentiment, topic, or substance.  
8 Finally, an interesting factor into all of these questions would be looking at the ages of the different posters and seeing if there is any correction between age, sentiment, topic, substance, location, and time of day.

After answering these questions for a specific bank and finding numbers, comparing these to the overall internet averages would be beneficial to see where the banking world falls in social conversation compared to other industries. 


##Code##


```R
# Load data set ('df.Rda')
load("df.Rda")
df$FullText = as.character(df$FullText)

# Grab just the texts, so you can load them in the Corpus
df.texts = as.data.frame(df[,ncol(df)])
colnames(df.texts) = 'FullText'

# Remove non-ascii characters
df.texts.clean = as.data.frame(iconv(df.texts$FullText, "latin1", 
                                     "ASCII", sub=""))
colnames(df.texts.clean) = 'FullText'

df$FullText = df.texts.clean$FullText

# To test on 10000 samples using df.10000
idx.10000 = sample(1:nrow(df),10000)
df.10000 = df[idx.10000,]

df.entire = df
df = df.10000

# Load in corpus form using the tm library
library(tm) 
docs <- Corpus(DataframeSource(as.data.frame(df[,6])))   

# Perform pre-processing
docs <- tm_map(docs, PlainTextDocument)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removeWords,c("Name","and","for")) 
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, stopwords(kind="SMART"))

save.image('docs.preprocessed.Rda')
load('docs.preprocessed.Rda')

# Documents containing each bank to reference when wanting to compare different banks
bankA.idx = which(sapply(df$FullText,function(x) grepl("BankA",x)))
bankB.idx = which(sapply(df$FullText,function(x) grepl("BankB",x)))
bankC.idx = which(sapply(df$FullText,function(x) grepl("BankC",x)))
bankD.idx = which(sapply(df$FullText,function(x) grepl("BankD",x)))

df$BankID = vector(mode="numeric", length = nrow(df))
df$BankID[bankA.idx] = "BankA"
df$BankID[bankB.idx] = "BankB"
df$BankID[bankC.idx] = "BankC"
df$BankID[bankD.idx] = "BankD"

bankA.docs = docs[bankA.idx]
bankB.docs = docs[bankB.idx]
bankC.docs = docs[bankC.idx]
bankD.docs = docs[bankD.idx]

summary(docs)

## Repeat these processes for every bank
## Create document term matrix
dtm <- DocumentTermMatrix(docs[bankA.idx])

## Transpose this matrix
tdm <- TermDocumentMatrix(docs[bankA.idx])

## Remove sparse terms
dtm = removeSparseTerms(dtm, 0.98)

## Organize terms by frequency
findFreqTerms(dtm,50)
freq <- colSums(as.matrix(dtm))  
ord <- order(freq)   
freq[head(ord)]  
freq[tail(ord)]

wf <- data.frame(word=names(freq), freq=freq)   
head(wf)

## Plot word frequencies
library(ggplot2)   
p <- ggplot(subset(wf, freq>100), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p

## To get a word cloud of the 100 most frequent words 
library(wordcloud)
set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=25, rot.per=0.2, colors=dark2)

# Sentiment analysis
#Positive and Negative Sentiment Values
pos <- scan('positive-words.txt',what='character',comment.char=';')
neg <- scan('negative-words.txt',what='character',comment.char=';')

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)

  scores = laply(sentences, function(sentence, pos.words, neg.words) {

    # Clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = tolower(sentence)

    # Split into words. You need the stringr package
    word.list = str_split(sentence, '\\s+')
    # Sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)

    # Compare words to positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)

    # match() returns the position of the matched term or NA
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)

    # TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)

    return(score)
  }, pos.words, neg.words, .progress=.progress )

  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

# Very positive and negative
df.sentiment = df[bankA.idx,]

scores = score.sentiment(df.sentiment$FullText, pos, neg, .progress='text')
scores$very.pos = as.numeric(scores$score >= 2)
scores$very.neg = as.numeric(scores$score <= -2)

pos.tweets = which(scores$very.pos == 1)
neg.tweets = which(scores$very.neg == 1)
write.csv(df.sentiment[pos.tweets,],file='pos.texts.csv')##creates positive
write.csv(df.sentiment[neg.tweets,],file='neg.texts.csv')##creates negative

# Creating a classifier for pos.texts
load('df.Rda')
df$FullText = as.character(df$FullText)
pos.texts = read.csv('pos.texts.csv',header=T)
pos.texts$FullText = as.character(pos.texts$FullText)
colnames(pos.texts)
docs <- Corpus(DataframeSource(as.data.frame(pos.texts[,9])))   
docs <- tm_map(docs, PlainTextDocument)

dtm <- DocumentTermMatrix(docs)

tdm <- TermDocumentMatrix(docs)

m = as.matrix(dtm)

df.classification = as.data.frame(m) #dataframe
df.classification$Relevant = pos.texts$Relevant
df.classification$Relevant

# Grow a tree
library(rpart)
fit<-rpart(Relevant ~ FullText + BankID, data = pos.texts, method = "class")
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# Creating a classifier for neg.texts
load('df.Rda')
df$FullText = as.character(df$FullText)
neg.texts = read.csv('neg.texts.csv',header=T)
neg.texts$FullText = as.character(neg.texts.texts$FullText)
colnames(neg.texts)
docs <- Corpus(DataframeSource(as.data.frame(neg.texts[,9])))   
docs <- tm_map(docs, PlainTextDocument)

dtm <- DocumentTermMatrix(docs)

tdm <- TermDocumentMatrix(docs)

m = as.matrix(dtm)

df.classification = as.data.frame(m) #dataframe
df.classification$Relevant = neg.texts$Relevant
df.classification$Relevant
colnames(neg.texts)

# Grow a tree
library(rpart)
fit<-rpart(Relevant ~ FullText + BankID, data = neg.texts, method = "class")
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

## Cluster Dendrogram
# Useless words
docs <- tm_map(docs, removeWords,c("Name", "and","for", "name", "this", "are","from", "just", "get", "ret_twit", "name_resp", "twit_hndl", "twit_handl:", "twit_hndl_banka","ly/")) 
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, stopwords(kind="SMART"))

dtm <- DocumentTermMatrix(docs)
tdm <- TermDocumentMatrix(docs)

# remove sparse terms
tdm2 <- removeSparseTerms(tdm, sparse = 0.97)
m2 <- as.matrix(tdm2)
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")

plot(fit)
rect.hclust(fit, k = 6)

##Cluster Dendrogram
docs <- tm_map(docs, removeWords,c("Name", "and","for", "name", "this", "are","from", "just", "get", "ret_twit", "name_resp", "twit_hndl", "twit_handl:", "twit_hndl_banka","ly/")) 
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, stopwords(kind="SMART"))

dtm <- DocumentTermMatrix(docs)
tdm <- TermDocumentMatrix(docs)

##############
```
##Topics Discussed##
Top Topics discussed: 
* Customer Service
1. ATM
2. Security
3. Privacy
4. App
5. Account Access
6. Account Request
7. Card
8. Customer Loss
9. Fees

The three main topics discussed were customer service, fees and ATM.
ATM
* Customer dislikes interacting with ATM due to lack of privacy, security, and customer service. 
* Card often goes unread at ATM’s and stores.

Customer Service 
* Customer is leaving bank because of rude bankers.
* Customer Service was not helpful in explaining fees.
* Customer dislikes ATM due to lack of customer service

Fees
* Customer  leaving bank  because of excessive fees.
* Customer has found more favorable conditions  at different bank .
* Too many fees withdrawing money at ATMs 


##Insights##

###Classification to find comments of substance. ###
We started creating a code that would classify the tweets and posts into spam or real.  We did this for the positive and negative posts separately to see the differences and similarities between what can be responded to, both on social media, as well as from a business stand-point.  In doing so, and testing parts of the data, we created two decision trees that showed what key words (aside from names, twitter handle, and the bank name)  
Our suggestion would be to have the people responding to tweets/ posts putting if they are spam or not so the algorithm becomes more sophisticated.  In doing so, when looking at the tweets later on, the data will be better minned if there was an algorithm that took out nonrelevant or ‘spam’ tweets and posts. 

|Positive Classification Relevancy |Negative Classification Relevancy|
|----------------------------------|---------------------------------|
|![Positive Classification](http://i.imgur.com/L8f24Az.png) | ![Negative Classification ](http://i.imgur.com/ul8pehC.png)|


According to our tree diagram, this diagram displays the positive and negative relevant words. This is significant because here we are able to plot the relevancy of positive sentiments as it shows overall bank's performance within the industry. When banks are acknowledged for their service they receive positive statements within a .5 relevancy.  With a less than .5 relevancy, the post may not be worth the time of the analyzer on the next social media analysis.  This can be helpful to decrease wasted space and a faster analysis process because there will be a significant amount less posts and tweets.  In our research, out of 301 posts, only 137 were relevant. 


### Frequent Topics and Related Items###
![Cluster Dendrogram](http://i.imgur.com/roYZq10.png)
This dedrogram, created by a member or my team shows some of the hierarchical pairings of words commonly found together.  This dendrogram makes come interesting connections:
* At the smaller connections, money and phone are linked suggestion mobile app use, or customer service ties
* BankD is commonly linked to url links(represented by 'internet')
* BankA is clustered with twitter handles which shows that there was a high percent of the posts tweets about bank A
* Program, full, learn, and small are all linked.  As these have less to do with banking topics, with more time, we would dig into these key-words to see what the conversation was about
* Account and nameresp are connected which means that there are frequent conversations on facebook about users' accounts.
* Today is closely clustered with business and street.  This may imply that the users had an urgency in posting that day and that the experience was in person- a business or a street addressed.  This can either mean a walk in bank or ATM location. 


Regardless of sentiment value, the four banks were surveyed separately and together to find the most common topics for each, represented in a word cloud.

![Word Cloud](http://i.imgur.com/CnUjkLu.png)  ![Histogram](http://i.imgur.com/0yhd2Wy.png)

Above shows the frequent words in the entire dataset.  This can be compared to the most common words for the top four banks shown below. 

|Bank A | Bank B | Bank C | Bank D |
|-------|--------|--------|--------|
|![Bank A](http://i.imgur.com/dM6is8e.png)|![Bank B](http://i.imgur.com/JtEpnSh.png)|![Bank C](http://i.imgur.com/JO61606.png)|![Bank D](http://i.imgur.com/0IhqiY0.png)|

Bank A included service, account, money, check, help, customer service, and phone.  These words show that most of the posts do have to do with the bank and the conversations are about aspects of the bank.  Also, it is of note that bankB is commonly mentioned so bankA and bankB may be close competitors.  With a high frequency of the twitter handle, it shows that many of the interactions were on twitter.

Bank B top comments included account, team, credit, share, service, customer, thanks.
Through the cop words in bankB’s tweets and posts, it looks like they may have a higher sentiment value than some of the other banks.  There was not a single topic that was predominantly present.  The inclusion of Chicago shows that there may be a hub or large bank present in the city. 

Bank C top comments included credit, pay, global, financial, banking, rating, credit, and business.
Through the top topics for Bank C, it is evident that their connections are the most involved with banking.  The bank must not be as involved with the community as other banks.  

Bank D shows a high connection to the community around them.  Whether it is Bank D that exists in another form other than the bank, it seems that bank D sponsors many things. With words like apply, program, and business, the bank shows more engagement words than the other banks.


##Conclusion##
Social Media is not only a place to talk to friends or a marketing ploy.  It is a place where our society continues.  Thus, it is important to understand its importance and what people on it are saying about companies.  Through this analysis we have seen many things.  It is probable that all banks have a range of polarity in sentiment.  This report shows suggestions in how it can be analyzed in the future:
* While going through tweets and posts declaring their relevancy to be able to use classification in the future
* Seeing the top topics discussed using word frequency
* Using clustering to determine the closeness of different words. 

With the results we processed, we saw a range of engagement and interactions in comments.  With more information like likes and shares, we would be able to better compare our results with the research at the beginning on engagement and interactions.  Overall, our results show that all banks seem to have a range of sentiment about all of the topics that we outlined.  

In conclusion, with the data given to us by Wells Fargo. We were able to manipulate it to figure out the social drivers that a bank may obtain from their customers through the use of social media. Using R and data mining we were able to classify and determine the relevancy using sentiment analysis and the proper algorithms and arithmetic to provide the outcomes of this data competition. As social media continues to progress and is widely used, we can most likely count on them to retrieve the data we need to figure out the likely causes of customer service in an industry.