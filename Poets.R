######################################################
######### Downloading Packages and Libraries##########
######################################################
install.packages("textdata")
install.packages("wordcloud")
library(textdata)
library(dplyr)
library(stringr)
library(tidyr)
library(scales)
library(tidytext)
library(tm)
library(ggplot2)
library(wordcloud)

######################################################
################# Creating Data ######################
######################################################

William_Blake <- c("Tyger Tyger, burning bright", 
                   "In the forests of the night",
                   "What immortal hand or eye",
                   "Could frame thy fearful symmetry?",
                   "In what distant deeps or skies.",
                   "Burnt the fire of thine eyes?",
                   "On what wings dare he aspire?",
                    "What the hand, dare seize the fire?",
                    "And what shoulder, & what art,",
                    "Could twist the sinews of thy heart?",
                    "And when thy heart began to beat,",
                    "What dread hand? & what dread feet?",
                    "What the hammer? what the chain," ,
                    "In what furnace was thy brain?",
                    "What the anvil? what dread grasp,", 
                    "Dare its deadly terrors clasp!" ,
                    "When the stars threw down their spears", 
                    "And water'd heaven with their tears:" ,
                    "Did he smile his work to see?",
                    "Did he who made the Lamb make thee?",
                    "Tyger Tyger burning bright" ,
                    "In the forests of the night:" ,
                    "What immortal hand or eye,",
                    "Dare frame thy fearful symmetry?",
                    "Little lamb, who made thee?",
                    "Dost thou know who made thee,",
                    "Gave thee life, and bid thee feed",
                    "By the stream and o'er the mead;",
                    "Gave thee clothing of delight,",
                    "Softest clothing, woolly, bright;",
                    "Gave thee such a tender voice,",
                    "Making all the vales rejoice?",
                    "Little lamb, who made thee?",
                    "Dost thou know who made thee?",
                    "Little lamb, I'll tell thee;",
                    "Little lamb, I'll tell thee:",
                    "He is called by thy name,",
                    "For He calls Himself a Lamb.",
                    "He is meek, and He is mild,",
                    "He became a little child.",
                    "I a child, and thou a lamb,",
                    "We are called by His name.",
                    "Little lamb, God bless thee!",
                    "Little lamb, God bless thee!")
  
William_Shakespare <- c("Shall I compare thee to a summer’s day?",
                         "Thou art more lovely and more temperate",
                         "Rough winds do shake the darling buds of May",
                         "And summer’s lease hath all too short a date",
                         "Sometime too hot the eye of heaven shines",
                         "And often is his gold complexion dimm’d",
                         "And every fair from fair sometime declines",
                         "By chance or nature’s changing course untrimm’d",
                         "But thy eternal summer shall not fade",
                         "Nor lose possession of that fair thou ow’st;",
                         "Nor shall death brag thou wander’st in his shade",
                         "When in eternal lines to time thou grow’st",
                         "So long as men can breathe or eyes can see",
                         "So long lives this, and this gives life to thee",
                         "Let me not to the marriage of true minds",
                         "Admit impediments. Love is not love",
                         "Which alters when it alteration finds",
                         "Or bends with the remover to remove",
                         "O no! it is an ever-fixed mark",
                         "That looks on tempests and is never shaken;",
                         "It is the star to every wand'ring bark,",
                         "Whose worth's unknown, although his height be taken.",
                         "Love's not Time's fool, though rosy lips and cheeks",
                         "Within his bending sickle's compass come",
                         "Love alters not with his brief hours and weeks",
                         "But bears it out even to the edge of doom",
                         "If this be error and upon me proved",
                         "I never writ, nor no man ever loved")

William_Wordsworth <- c("Earth has not anything to show more fair:",
                        "Dull would he be of soul who could pass by",
                        "A sight so touching in its majesty:",
                        "This City now doth, like a garment, wear",
                        "The beauty of the morning; silent, bare,",
                        "Ships, towers, domes, theatres, and temples lie",
                        "Open unto the fields, and to the sky;",
                        "All bright and glittering in the smokeless air.",
                        "Never did sun more beautifully steep",
                        "In his first splendour, valley, rock, or hill;",
                        "Ne’er saw I, never felt, a calm so deep!",
                        "The river glideth at his own sweet will:",
                        "Dear God! the very houses seem asleep;",
                        "And all that mighty heart is lying still!",
                        "A slumber did my spirit seal",
                        "I had no human fears:",
                        "She seemed a thing that could not feel",
                        "The touch of earthly years.",
                        "No motion has she now, no force;",
                        "She neither hears nor sees;",
                        "Rolled round in earth’s diurnal course,",
                        "With rocks, and stones, and trees.") 

#######################################################
##########Creating Dataframes################
#######################################################

William_Blake_df <- data.frame(line=20:63, text=William_Blake)
William_Shakespare_df <- data.frame(line=65:92, text=William_Shakespare)
William_Wordsworth_df <- data.frame(line=94:115, text=William_Wordsworth)

#make sure to run above code separately(line by line) and before the below code (otherwise it throws an error)
All_poets <- bind_rows(mutate(William_Blake_df, author="William Blake"),
                          mutate(William_Shakespare_df, author= "William Shakespare"),
                          mutate(William_Wordsworth_df, author="William Wordsworth"))

#######################################################
#################Tokenization################
#######################################################

All_poets_tokens <- All_poets %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE) 

William_Blake_tokens <- William_Blake_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

William_Shakespare_tokens <- William_Shakespare_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

William_Wordsworth_tokens <- William_Wordsworth_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE) 


##########################################
############# Correlograms ###############
##########################################

correl <- bind_rows(mutate(William_Blake_tokens, author="William Blake"),
                       mutate(William_Shakespare_tokens, author= "William Shakespare"),
                       mutate(William_Wordsworth_tokens, author="William Wordsworth")
)%>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion,`William Shakespare`,`William Wordsworth`)


####Plotting my correlagram

ggplot(correl, aes(x=proportion, y=`William Blake`, 
                      color = abs(`William Blake`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "William Blake", x=NULL)

##########################################
##doing the cor.test() ################
##########################################

cor.test(data=correl[correl$author == "William Shakespare",],
         ~proportion + `William Blake`)

cor.test(data=correl[correl$author == "William Wordsworth",],
         ~proportion + `William Blake`)


##########################################
######## Frequency Histograms ######
##########################################

freq_hist_bl <- William_Blake_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE) %>%
  top_n(10) %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n))+
  geom_col(fill="orange")+
  xlab(NULL)+  #label
  coord_flip() #Coordinates flipped
print(freq_hist_bl)

freq_hist_sh <- William_Shakespare_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE) %>%
  mutate(word=reorder(word, n)) %>%
  top_n(7) %>%
  ggplot(aes(word, n))+
  geom_col(fill="lightblue")+
  xlab(NULL)+  #label
  coord_flip() #Coordinates flipped
print(freq_hist_sh)


freq_hist_wrd <- William_Wordsworth_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE) %>%
  top_n(1) %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n))+
  geom_col(fill="darkgreen")+
  xlab(NULL)+  #label
  coord_flip() #Coordinates flipped
print(freq_hist_wrd)


##########################################################
################ tf_idf Analysis#########################
##########################################################

All_poets_idf <- bind_rows(mutate(William_Blake_df, author="William Blake"),
                       mutate(William_Shakespare_df, author= "William Shakespare"),
                       mutate(William_Wordsworth_df, author="William Wordsworth")) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(author, word, sort=TRUE) %>%
  ungroup()
  
total_words <- All_poets_idf %>%
    group_by(author) %>%
    summarize(total=sum(n))
  
poets_words <- left_join(All_poets_idf, total_words)
  
print(poets_words)
  
ggplot(poets_words, aes(n/total, fill = author))+
    geom_histogram(show.legend=FALSE)+
    xlim(NA, 0.1) +
    facet_wrap(~author, ncol=2, scales="free_y")  
  
######################################
########## ZIPF's law ################
######################################  
  
freq_by_rank <- poets_words %>%
  group_by(author) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank

# plot ZIPF's Law
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=author))+
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  #let's add a tangent line , the first derivative, and see what the slop is
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

poets_words_idf <-poets_words %>%
  bind_tf_idf(word, author, n) 

poets_words_idf %>%
  arrange(desc(tf_idf))

poets_words_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(author) %>%
  filter(n<10) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=author))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~author, ncol=2, scales="free")+
  coord_flip()


######################################
########## WordClouds ################
###################################### 
install.packages(("reshape2"))
library(reshape2)

Cloud_Blake <- William_Blake_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>% #token per sentiment
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("pink", "blue"),
                   max.words=50, scale=c(1.5, 0.8),
                   title.size=1.5)


Cloud_Shakespeare <- William_Shakespare_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>% #token per sentiment
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("purple", "darkgreen"),
                   max.words=50, scale=c(1.5, 0.8),
                   title.size=1.5)


Cloud_Wordsworth <- William_Wordsworth_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>% #token per sentiment
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("blue", "orange"),
                   max.words=50, scale=c(1.5, 0.8),
                   title.size=1.5)