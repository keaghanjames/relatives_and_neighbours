
#################################################
    #########################################
    ### Analysis of tonality and humidity ###
    #########################################
#################################################

#change the following path to wherever the data is stored
setwd('~/cultural_causal/tonal_languages')

#these packages will need to be installed before being load
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(ggthemes)
require(tidyr)

#we will start by mapping our humidity variable
#first we extract a world map from ggplot2
world <- map_data("world")
class(world)

#next we will read in Kalnay et al.'s (2022) humidity data
humidity <- read.csv('Humidity.csv', header = T, row.names = NULL)
head(humidity)
dim(humidity)

#these are point estimates of monthly humidity estimates for regularly spaced
#locations across the planet. But these don't necessarily match up to our actual languages
#we are going to make a humidity raster so we can estimate the mean humidity for any language in our 
#dataset

require(raster)
require(sp)
#we define a projection
credef <- CRS('+proj=longlat +datum=WGS84')
#and combine our longs and lats together so we can group the dataframe by location
humidity$location <- paste0(humidity$degree_east, '_', humidity$degree_north)

#we group the humidity estimates by location and take the mean of each group
require(dplyr)
humidity2 <- humidity %>% 
  group_by(location) %>%
  summarise(degree_east = mean(degree_east), degree_north = mean(degree_north), MHS = mean(unitless))
humidity <- humidity2
#note we are not actually taking the mean degree_east or north, all groups are the same for this variable
#we are simply using mean as a convenient way of preserving the structure of the original data

#now we create a spatial points object from the grouped data
spatial_points <- SpatialPoints(humidity[,2:3], proj4string = credef)

#as a sanity check we will plot them
plot(spatial_points)
#looks good, redgularly spaced point estimates of mean annual humidity
#lets make a new dataframe that is our mean humidity for each of these points
humidity <- data.frame(ID = 1:nrow(humidity), MHS = humidity$MHS)
#and combine it with our spatial points object to creat a spatial points dataframe
spatial_data_frame <- SpatialPointsDataFrame(spatial_points, data = humidity)
str(spatial_data_frame)
showDefault(spatial_data_frame)

#as a sanity check we will plot this
spplot(spatial_data_frame, 'MHS')

#next we will convert these points into a raster using distance weighted interpolation
#this will allow us to extract mean humidity estimates for any location on the planet based
#on the 18k point estimates

require(gstat)
gs <- gstat(formula = MHS~1, locations = spatial_data_frame)
gs

#we create an empty raster,  this tells the algorithm how many gridcells
#to divide the world into. Here we are going to do one degree by one degree which is consistent
#with out humidity data. We also specify the formula and the projection
ras_dom<-raster(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90,
                crs=credef)

#finally, we run the interpolation. This can be quite slow if your computer is not powerful enough
#you can simply use the following to load the raster and skip the next line
#load('idw_interpolation.Rdata')
system.time(idw <- interpolate(ras_dom, gs))

#we can plot this out too, and now we see we have global estimates of mean humidity 
#in one degree by one degree gridcells
plot(idw)

#next we need to import our language data
df <- as_tibble(read.csv('WALS_data.csv', header = T))
head(df)
#the value column is a numeric category for tonality
#we are just going to change its colname so we don't get confused 
#down the track
colnames(df)[3] <- 'tonality'

#next we will make two new biunary variables, one will describe is 
#a language has tonality or not and the other if it has complex tonality or not
df$ton_none_or_some <- 0
df$ton_none_or_some[which(df$tonality == 2 | df$tonality == 3)] <- 1
df$ton_complex <- 0
df$ton_complex[which(df$tonality == 3)] <- 1

#now we are ready to get our humidity estimates for each of our languages
#fist we create an object with the longitude and latitude that is recorded against 
#each of our languages in the WALS data
long_lat <- df[,c(6,5)]
head(long_lat)

#we use this to create a spatial points object with the same projection as our raster
spatial_points <- SpatialPoints(long_lat, proj4string = credef)
#and we use the function extract to get humidity estimates at each of these points
extract <- raster::extract(x = idw, y = spatial_points, sp = T)

#brilliant, lets visualize both the tonality of our languages and their mean humidity
#first we add the humidity data to our original data frame
df <- cbind(df, extract@data$var1.pred)
colnames(df)[ncol(df)] <- 'predict_MHS'

#and then we will add a second description column just to help with our plots
df$description2 <- 'No tones'
df$description2[which(df$ton_none_or_some == 1)] <- 'Tone system'

#okay lets plot it
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region), 
    col = rgb(240,240,240, 1,maxColorValue = 255), fill = 'lightgrey', lwd = .1
  ) + 
  theme_map() +
  geom_point(data = df, aes(x = longitude, y = latitude, shape = description2, col = predict_MHS, size = description2)) +
  scale_shape_manual(values = c(4,17), name = 'Tonality') +
  scale_size_manual(values = c(1,3))+
  scale_color_gradient(low = '#32CBFF', high = 'red', name = 'Mean Humidity') +
  guides(size = FALSE) +
  theme(legend.position = 'bottom')

#the shape of the point indicates whether or not it has a tonal system while the colour represents 
#the predicted humidity at the point associated with the language. As we can see the tonal languages 
#are spatially clustered

###########################
### Regression analysis ###
###########################

#next we will try to fit a logistic regression model to see if mean humidity predicts tonality
#the first thing we will do is scale and center our predictor variable, humidity
df$scaled_MHS <- scale(df$predict_MHS)

require(nlme)
require(lme4)

#and now we fit the logistic regression
#we set the family to binmoial because that reflects the underlying binary structure of our predictor, 
#our languages either are or are not tonal
glm <- glm(ton_none_or_some ~ scaled_MHS, df, family = 'binomial')
#lets look at the summary output of this analysis
summary(glm)
#looks like there is a significant relationship between humidity and whether or not a language is tonal
#because this is logistic regression we can just take the exponent of the coefficient to get the odds-ratio
exp(coef(glm)[2])
#okay for an increase of one standard deviation in humidity the odds of a language being 
#tonal are 1.351 times that of it not being tonal

#lets just check that this is a better fit than an intercept only model
glm0 <- glm(ton_none_or_some ~ 1, df, family = 'binomial')
summary(glm0)

require(lmtest)
lrtest(glm0, glm)

#and yes, it is. the inclusion of tonality represents a significant improve in model fit compared to the intercept 
#only model

#lets map our the residuals of our analysis
df$resid_glm <- resid(glm, type = 'pearson')

m1 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region), 
    col = rgb(240,240,240, 1,maxColorValue = 255), fill = 'black', lwd = .1
  ) + 
  theme_map() +
  geom_point(data = df, aes(x = longitude, y = latitude, col = resid_glm, shape = as.logical(ton_none_or_some))) +
  scale_shape_manual(values = c(16,17), name = 'Tonal') +
  scale_color_gradient2(limits = c(-2,2), low = '#32CBFF', mid = 'purple', high = 'red', name = 'Pearson residual') +
  theme(legend.position = 'bottom')
m1

#okay we can see that our residuals show a really clear pattern of spatial clustering
#specifically there are a lot of tonal languages with unexplained variance concentrated
#in Sub Saharan Africa and South East Asia

#this suggest that our data may violate the assumption of independence 
#we can formally test this using Moran'
require(fields)
#we get a distance matrix of great circle distances between our languages
language.dists <- as.matrix(rdist.earth(cbind(df$longitude, df$latitude), miles = F))
language.dists.inv <- 1/language.dists #and take its inverse
diag(language.dists.inv) <-  0 #we fix the diagonal to 0
language.dists.inv[1:5, 1:5]
require(ape)
#and then we run a Moran's I test
Moran.I(df$ton_none_or_some, language.dists.inv)

#okay, so under the null hypothesis of Moran's I which that these is no spatial clustering 
#in the distibrution of tonal and non-tonal languages, the expectation is an I index of -0.002
#in fact we observed an I index of 0.216, with a signficant p-value
#thus our tonality variable is significantly more spatially clustered than expected under the null 
#we can conclude from this that we do have spatial autocorrelation in our data
#note we get a very similar results for the residuals
Moran.I(df$resid_glm, language.dists.inv)

############################################
### Spatial Explicit Logistic Regression ###
############################################

#we will use the R package glmmTMB which allows for generalised linear models, like logistic regression, 
#to be fit with a variety of spatial decay processes made explicit in the models error term
#we will fit an exponential decay process here but multiple process can be fit and compared using model 
#selection criterion such as AIC or BIC
library(glmmTMB)
#we first create numeric factor recording of the coordinates of each language
df$pos <- numFactor((df$longitude), (df$latitude))
#and we need an ID so we can tell the function that all of our variables belong to the same group
df$ID <- factor(rep(1, nrow(df)))

#then we simply fit our model again but with the exponential spatial decay process made explicit 
e_tmb <- glmmTMB(ton_none_or_some ~ scaled_MHS + exp(pos + 0 | ID), df, family = 'binomial') # take some time to fit
#this can take some time
summary(e_tmb)
#so under this model we no longer find a significant relationship between humidity and tonality

#lets compare it to the intercept only equivelant
e_tmb0 <- glmmTMB(ton_none_or_some ~ 1 + exp(pos + 0 | ID), df, family = 'binomial') # take some time to fit
lrtest(e_tmb, e_tmb0)
#and the model which includes humidity is no longer a significantly better explanation for our observations
#than an intercept only model 

#lets map the residuals of this analysis
df$resid_e_tmb <- resid(e_tmb, 'pearson')

m2 <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region), 
    col = rgb(240,240,240, 1,maxColorValue = 255), fill = 'black', lwd = .1
  ) + 
  theme_map() +
  geom_point(data = df, aes(x = longitude, y = latitude, col = resid_e_tmb, shape = as.logical(ton_none_or_some))) +
  scale_shape_manual(values = c(16,17), name = 'Tonal') +
  scale_color_gradient2(limits = c(-2,2), low = '#32CBFF', mid = 'purple', high = 'red', name = 'Pearson residual') +
  theme(legend.position = 'bottom')
m2

#we can see now that the resdiuals are much smaller under the spatial explict model 
#and those points with high residual values are not spatially clustered

#we can test this formally
Moran.I(df$resid_e_tmb, language.dists.inv)
#and we no longer detect significant spatial clustering in the 
#residuals of our analysis

#what this tells us is that the apparent relationship between tonality and humidity
#is not strong enough to be detected beyond the covariance we already expect just from the
#spatial distribution of our data points



#################################################
### Other correlates of tonality and humidity ###
#################################################


#amphibian species richness

r.polys <- readRDS('amphibian_richness_raster.rds') #load in our raster of amphibian species richness 
extract <- raster::extract(x = r.polys, y = coordinates(spatial_points)) #extract species richness for each tonal language
df$scale_amp_rich <- scale(extract) #scale it
glm <- glm(ton_none_or_some ~ scale_amp_rich, df, family = 'binomial')
summary(glm)

e_tmb_amph <- glmmTMB(ton_none_or_some ~ scale_amp_rich + exp(pos + 0 | ID), df, family = 'binomial') #we can fit a spatially explicit analysis
summary(e_tmb_amph) #the effect of amphibian species richness remains significant

#passive voice
passive <- read.csv('passiveWALS.csv') #we read in the WALS data for passive voice
passive
lat_long <- passive[,c(6,5)]
lat_long
#and now we will get predicted humidity using our raster
spatial_points <- SpatialPoints(lat_long, proj4string = credef)
extract <- raster::extract(x = idw, y = spatial_points, sp = T)
extract
spplot(extract)
#we add this to our dataframe
passive <- cbind(passive, extract@data$var1.pred)
colnames(passive)[ncol(passive)] <- 'predict_MHS'
#scale it for analysis
passive$scaled_MHS <- scale(passive$predict_MHS)

#for some reason passive absent is recorded as 2
passive$value[which(passive$value == 2)] <- 0

#we fit out model 
glm <- glm(value ~ scaled_MHS, passive, family = 'binomial')
summary(glm)


#################################################
    #########################################
    ### Analysis of hand/feet endangerment ##
    #########################################
#################################################

rm(list = ls())
setwd('~/culture_causal/hand_finger') #again the path will need to be changed

#read in the WALS hand/finger database
df <- read.csv('hand_finger.csv')
head(df)
#and then the supplementary data from Bromham et al. 2022
endangerment_data <- read.csv('Supplementary_data_S1.csv')
#this is a big dataframe so we will subset it to just the variables we need ISE and AES
endangerment_data <- endangerment_data[,c(1,4)]

#next we need to match the ISOs to those in the WALS database and bind the endgangerment information
df <- cbind(df, endangerment_data[match(df$wals.code,endangerment_data$ISO),])
#and we will remove all the languages for which we do not have endangerment data
df <- df[which(is.na(df$AES) == F),]
dim(df)
#so we now have a sample size of 436 languages 
table(df$description)
#of which 382 use different words for hand and finger and 54 use the same word

#we are going to be using a  linear model to see if having a single category 
#for hand/finger predicts endangerment, therefore we are going to need to convert our 
#AES levels into an ordered numer variable
#first we make them a factor
df$AES <- as.factor(df$AES)
levels(df$AES)

#then we make that factor ordered and specify the order
df$AES <- factor(df$AES, ordered = T, levels = levels(df$AES)[c(4,6,5,2,3,1)])

#and then we create a numeric version of that ordered factor
df$AES_numeric <- as.numeric(df$AES)

#finally we just need to alter how the hand/finger variable is recorded so we can treat it as a 
#binary variable 
df$value <- df$value-1
head(df)

#now we can see that a 1 indicates a language has different words for hand/finger while a 0 indicates 
#that they have the same word

#brilliant, lets fit a linear model where numeric AES is the outcome variable and our predictor 
require(nlme)
lm <- lm(df$AES_numeric ~ as.logical(df$value))
summary(lm)

#we can also fit this using a gls as our AES data violates the assumption of normality
gls <- gls(AES_numeric ~ as.logical(value), df)
summary(gls)

#the coefficients are very similar

#okay, we have a significant negative relationship between endangerment and the hand/finger variable
#so languages which had two separate words are less likely to be endangered

#lets look at our residuals
df$lm_resid <- resid(lm, 'pearson')

world <- map_data("world")
class(world)

ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region), 
    col = rgb(240,240,240, 1,maxColorValue = 255), fill = 'black', lwd = .1
  ) +   #coord_map("moll") +
  theme_map() +
  geom_point(data = df, aes(x = longitude, y = latitude, shape = description, col = lm_resid, size = description)) +
  scale_shape_manual(values = c(4,17), name = 'Hand/Finger') +
  scale_size_manual(values = c(1,3))+
  scale_color_gradient2(low = '#32CBFF', mid = 'white', high = 'red', name = 'AES', midpoint = 0) +
  guides(size = FALSE) +
  theme(legend.position = 'bottom')

#the spatial clustering is not as apparent in the residuals of this analysis as the tonality analysis
#so lets calculate Moran's I
language.dists <- as.matrix(rdist.earth(cbind(df$longitude, df$latitude), miles = F))
language.dists.inv <- 1/language.dists #and take its inverse
diag(language.dists.inv) <-  0 #we fix the diagonal to 0
language.dists.inv[1:5, 1:5]
Moran.I(df$lm_resid, language.dists.inv)
#nonetheless we still find that the residuals are more clustered than can be explained by the null


#lets fit spatially explicit linear models
#we first create numeric factor recording of the coordinates
df$pos <- numFactor((df$longitude), (df$latitude))
df$ID <- factor(rep(1, nrow(df)))

#next we fit our spatially explicit linear model
e_tmb <- glmmTMB(AES_numeric ~ value + exp(pos + 0 | ID), df) # take some time to fit
summary(e_tmb)
#and we no longer find a statistically significant association between hand/finger and endangerment

#we can also fit this in a GLS framework which may be more appropriate as the AES values violate 
#the assumption of normality for a linear model response variable
spatialCor.glsExp <- gls(AES_numeric ~ value, data = df,
                              correlation = corExp(form = ~latitude + longitude, nugget = TRUE),
                              method = "REML")
summary(spatialCor.glsExp)
#as we can see the coefficient is very similar between the glm and the gls and the p-value remains above 0.05







