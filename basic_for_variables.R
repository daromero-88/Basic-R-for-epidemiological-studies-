####BASICS FOR EPIDEMIOLOGICAL STUDIES IN R-----------------------------

#Author: Daniel Romero-Alvarez

#Description: 
#' In the following script I summary the steps I usually undergo whenever
#' I am creating a manuscript based on clinical data for a epidemiological 
#' descriptive study. It is not complete and it is dynamic meaning that I will 
#' be adding examples, good practices, nice packages, and hopefully... nice functions...

#####PACKAGES AND LIBRARIES------------------------------------------------
#install.packages("htmlwidgets")

#' Following you can see multiple packages, they are useful for multiple stuff
#' they might be useful for multiple purposes such as reading and writing shape files, 
#' creating dynamic maps, etc. 
#' 

###SPSS TO CSV: 
#library(haven) #sav to csv 
#library(readr) #sav to csv 

###GENERAL: 
#library (dplyr)

###CREATING FIGURES
#library (ggplot2) #a new era in life... 

#library (animation) #to read the function im.converter that create gif files 

###GEOGRAPHIC INFORMATION SYSTEMS: 
#library (devtools)
#library(raster)
#library(sp)
#library(rgeos)
#library (rgdal)
#library (maptools)
#library(mapdata)
#library (animation) #allows the developing of .gif animations

###DYNAMIC MAP CREATION: 
#library (leaflet) #creating interactive maps
#library(htmlwidgets) #saving leaflet maps as widgets 



######WORKING DIRECTORY----------------------------------

setwd ( "/Users/daniel/Documents/GitHub/Basic-R-for-epidemiological-studies-") #change accordingly 

######READING DATABASE----------------------------------

#' File is usually read as a .txt file exported directly from excel 
#' Previous to reading the file make sure of the following: 
#' - the headers lack special characters (dots, commas, accents, tildas, etc)
#' this is specially important because the presence of this stuff will prevent 
#' the reading of the entire table. You migh want to ready it with header = F
#' but then you will need to add the name of the variables manually via: 
#' --> reading first row as a vector 
#' --> use this vector for naming the columns 
#' --> eliminating the first row. But better avoid all this by following the recommendation 
#' - the levels of each variable are unique
#' - there is no blank spaces across the table 
#' - 

es1 = read.table ('mod2.txt', 
                  sep = '\t', fill = F, header = T, 
                  encoding =  "UTF-8")

#' Notice encoding = 'UTF-8" which is the portion of the script that allows 
#' you to read weird characters across the table. 
#' 
#' Sometimes you want to avoid the presence of automatically tranformed
#' factors because you want to preserve the integer nature of the variable. 
#' For that you can use stringAsFactors = F: 

##aa = read.table ('vertical1.txt', sep = '\t', header= F, 
##                 encoding =  "UTF-8", fill = F, 
##                 stringsAsFactors = F)


#MANIPULATING THE DATABASE----------------------------

#' You might want to check the variables available for you 
#' 

colnames (es1)

#' You can easily create counts of qualitative data using table: 

table (es1$edad_grupo) #where the dummy variables represent different age groups
table (es1$ocupacion) #dummy variables have not been added here

#' To check the nature of automatically added dummy variables we 
#' can use the function CONTRASTS, it only works in factors because
#' dummy variables by nature only appear in this kind of variables: 
#' 

contrasts (as.factor(es1$edad_grupo)) #here, as.factor is transforming integers in factors

#' We can create tables for multiple variables at the same time, for example: 

table (es1$sexo, es1$edad_grupo) #we will use this as porpuses of visualization


#AGE AND SEX-----------------------------------------

#' because the original database is using dummy variables as age groups we create a vector
#' with the actual names of each dummy variables so we can add it in the corresponding plot 

edades = c('0-10', 
           '11-20', 
           '21-30', 
           '31-40', 
           '41-50', 
           '51-60', 
           '60-70', 
           '>70')

#sex and age group

barplot (table(es1$sexo, es1$edad_grupo), #table itself 
         main = 'Género y edad', ylab = 'Frecuencia',  #title and y axis names 
         names = edades, las = 2, cex.names = 1, #notice the argument NAMES, adding the names for the plot 
         legend = c('Hombres', 'Mujeres')) #notice the argument LEGEND for plotting 

#' In order to export this image as a pdf and therefore as a vector image that I can manipulate
#' in image editing programs for any particular purpose, I will use the following script: 
#'  

#' this line is creating and empty PDF with a LETTER format and size. 
#' Notice that the first argument is also adding the title of the plot
#' Make sure to read from pdf to dev.off() in order to see the picture in the working dir.

pdf (paste ('genero_edad', '.pdf', sep = ''), width = 10.5, height = 8, paper = 'letter')

#' this line is showing how the plot it is going to be written in terms of its margins
#' it uses the order ##bottom, left, top, right##
#' 
par(mar = c(10,4,3,3)) #so a lot of space at the bottom for letters, and almost equal otherwise

barplot (table(es1$sexo, es1$edad_grupo), 
         main = 'Género y edad', ylab = 'Frecuencia', 
         names = edades, las = 2, cex.names = 1, 
         legend = c('Hombres', 'Mujeres'))
dev.off() #this line is crucial so the plot it is written as PDF file in your working directory 

#' following more examples with particular details: 

#YEARS AND SEVERITY-------------------------------

#' Here, I am using the args.legend argument inside barplot to actually control 
#' the position of the legend, specifically, to make sure it appears in the topleft 
#' corner and move it a little to the right so not overlaps with the axis (inset = c(0.11, 0))

table (es1$gravedad, es1$year) #table itself 

pdf (paste ('Year-severity', '.pdf', sep = ''), width = 10.5, height = 8, paper = 'letter') #importation
par(mar = c(10,4,3,3)) #controlling margins 
barplot (table (es1$gravedad, es1$year), 
         main = 'Casos por año y severidad', las = 3, 
         legend = c('Leve', 'Moderado', 'Severo'), 
         args.legend = list(x = "topleft",
                            inset = c(0.11, 0))) #use of args.legend to move the legend inset
dev.off()


#MONTH OF SNAKE BITE ----------------------------------

#' We want this plot to be ordered from January to December (in Spanish). Thus, first we 
#' create a dataframe from the table matrix. Then, we add an ID column with the numbers
#' of each month that I will use to order the entire dataframe. Finally will manually 
#' control de x axis in order to depict the names of each month as were ordered. 
#' NOTICE that because I am using a dataframe, I will use PLOT instead of barplot... 
#' and also because it is a timeline so I wanted to depict it as a curve instead of bars.

#dataframe only for months: 
months= data.frame(table (es1$mes)) 

#adding ID corresponding to month number
months$m_id = c(4, 8, 12, 1, 2, 7, 6, 3, 5, 11, 10, 9)

#ordering datafram by ID 
months= months[order(months$m_id),]

#plotting: 
plot (months$Freq, type= 'b', pch = 17, 
      las = 2, ylim = c(5, 25), ylab = 'Frecuencia', xlab = '',
      main = 'Casos por mes (2017-2020)', 
      xaxt = 'n', col = 'red') #NOTICE xaxt to eliminate the x axis to control it manually
axis(1, at = 1:12, labels = months$Var1, las =2) #here I am adding the 12 names of the month using at and labels.

#' at and labels can be modify in order to add any particular description in the x axis, 
#' for example changing at = seq (1,10, 2) I will have five numbers and then I will need 
#' five names. 

##e.g., seq (1,100, by = 5)

#Definitive figure: 
pdf (paste ('mes_tiempo', '.pdf', sep = ''), width = 10.5, height = 8, paper = 'letter')
par(mar = c(10,4,3,3))
plot (months$Freq, type= 'b', pch = 17, 
      las = 2, ylim = c(5, 25), ylab = 'Frecuencia', xlab = '',
      main = 'Casos por mes (2017-2020)', 
      xaxt = 'n', col = 'red')
axis(1, at = 1:12, labels = months$Var1, las =2)
dev.off()


###DEFINITIVE FIGURE 1-----------------------------------

#' This figure is combining the the first two plots done previously and adding extra details: 
#' NOTICE THAT I AM NOT EXPORTING AS PDF these files because for my purposes is better
#' to increase the size of the plots manually—dev.new () if they are not shwoing up—and then 
#' editing the screenshots manually.
#' Fot the legends to appear correctly I should call the figure again after making it larger
#' so I can preserve their original format. 
#' NEW ARGUMENTS included here are: 
#' - cex.names and cex.axis, both established at 1.5 controling the font size of axis and names
#' - bty = 'n' inside the list of args.legend which assures that the legends are not inside 
#' a frame. 
#' cex = 1.5 inside the args.legend list so the fonts of the legend also have a correct 
#' size when calling the script. 

par (mfrow = c(1,2))

barplot (table (es1$gravedad, es1$year), 
         main = 'Casos por año y severidad', 
         cex.names = 1.5,cex.axis = 1.5, ylab = 'Casos',
         legend = c('Leve', 'Moderado', 'Severo'),  
         args.legend = list(x = "topleft", bty = 'n',
                            inset = c(0.11, 0), cex=1.5))

barplot (table(es1$sexo, es1$edad_grupo), 
         main = 'Género y edad', ylab = '', 
         names = edades, las = 2,
         cex.names = 1.5,cex.axis = 1.5,
         legend = c('Hombres', 'Mujeres'), 
         args.legend = list (bty ='n', cex = 1.5))


#' Writing the timeline figure but making sure that ylim, starts in 5 so it can be 
#' appreciated that cases never reach 0. Again notice cex.axis as 1.5 as a feature for good
#' figures. I expanded this horizontally for the final figure. 

par (mfrow = c(1,1))
par(mar = c(10,4,3,3))
plot (months$Freq, type= 'b', pch = 17, 
      las = 2, ylim = c(5, 25), ylab = 'Casos', xlab = '', 
      main = 'Casos por mes (2017-2020)', cex.axis= 1.5,
      xaxt = 'n', col = 'red')
axis(1, at = 1:12, labels = months$Var1, las =2, cex.axis = 1.5)


#' Apparenlty I can create figures with the three plots at the same 
#' time changing the orientation and size of all of them...need to explore: 
#' https://www.datamentor.io/r-programming/subplot/ 
#' 


#WHAT DATA/FIGURES SHOULD BE PRESENTED------------------------------------
#' To answer this question a good approach can be to actually see the variables themselves
#' in percentages and counts of their independent levels. A quick proxy can be to use the 
#' function STR: 
#' 

str (es1)

#' But for this particular excersice I decided to export each variable as an independent
#' csv and also with their independent barplot so I can see what is happening in excel and
#' visually to then refine my approach.

#SUMMARIES CUALITATIVES------------------------------

#' For this visual summaries I will create a folder to store the outputs, then, 
#' I will use a loop to export independent .csvs and also independent barplots per variable 
#' So I can: 
#' - write them down as a table for the final paper 
#' - check what varaibles are presenting interesting information 
#' - write them in descending order so I can see which levels are more important 
#' inside each variable... 

##folder for dataframe outputs
dir.create ('vars1') ##applied just once to create the folder

colnames(es1) #select variables to analyze 

##vector of variables to be visualized: 
ll = c(2, 5, 6, 7, 8, 9, 10, 11, 12, 14, 16, 21, 22, 27, 29, 32) #this vector represent the indexes of the variables to be processed


##LOOP for creating the output files: 
##i = ll[2] ##debugging

for (i in ll){
  #dataframes
  a = data.frame (table(es1[,i])) #creating independent dataframes per variable
  a= a[order(a[,2],decreasing = T),] #ordering according to most important
  ss = sum(as.numeric(a[,2])) #obtaining the totals
  a$perc = (as.numeric(a[,2])/ss)*100 #obtaing local percentages of each variable
  vc = c(Var1 =  'total', Freq = ss, perc = (ss/ss)*100) #row of totals
  lr2 = t(as.data.frame(vc)) #transforming the row vector into a dataframe for correctly doing merging the files
  a2= rbind (a, lr2) #merging totals row with dataframe 
  write.csv (a2, paste0('./vars1/', colnames(es1)[i], '.csv'), row.names= F) #writing the databases
  
  #figures: 
  #For them, I work with the dataframe before adding the totals, so basically 'a' instead of 'a2'
  pdf (paste0 ('./vars1/', colnames(es1)[i], '.pdf'), width = 10.5, height = 8, paper = 'letter')
  par(mar = c(10,4,3,3))
  barplot (a[,2], names = a[,1], las = 2, 
           main = paste0(colnames(es1)[i]), 
           ylab = 'Frecuencia')
  dev.off()
}

#' notice that the errors showing up are refereing to the words
#' albanil and pequena which are Spanish characters for the letter ñ not recognized 
#' in English. 


#SUMMARIES QUALITATIVES FOR STATISTICAL ANALYSIS------------------------------

#' In this section, I am creating databases as previously depicted but showing them 
#' against a variable of interest. In this case, any particular variable versus severity
#' so the dataframe created is having totals for the variable of interest and then 
#' dividing that variable per the crossing variable. Because severity here is expressed
#' as mild, moderate and severe, we will have a total of four columns. 
#' Then I can also add the percetanges per column to have a clearer picture of the contribution 
#' of each level. 


colnames(es1) #assessing what variables are important

dir.create ('vars2') #creating corresponding folder

#variables to be analyzed: 

vars = c(2, 5, 6, 7, 8, 9, 10, 11, 12, 14, 32)

#j = vars[1] ##debugging

for (j in vars){
  b = table(es1[,j], es1$gravedad) #crossing variable 
  nm = colnames(es1)[j] #collecting variable name 
  c = cbind (var_tot = table(es1[,j]), b) #adding the column of variable totals 
  d = colSums(c) #creating a row of totals 
  percs = mapply('/', data.frame(c), d)*100 #dividing each column by a vector
  colnames(percs) = paste0(colnames(percs), '_','%')
  percs_sums = colSums(percs) #everything adds to 100. 
  dd= rbind (c, tot = d) #adding the row of totals to the counts database
  ee = rbind (percs, tot = percs_sums) #adding to row of totals to the percentage database
  ed = cbind (dd, ee) #merging counts and percentages 
  write.csv (ed, paste0('./vars2/', nm, '.csv'), row.names = T)
}

#' These tables can be use for chi2 tests. I would love to do that directly in R
#' and have a super nice table with the chi2 per variable but I cannot do it...YET :) 

#' Meanwhile, I can use: 
#' https://www.icalcu.com/stat/chisqtest.html
#' in order to paste the section to be analyzed and then ask for chi2 manually 
#' to create the statistical table. 
#' The nice thing of this website resource is that it can handle contingency 
#' tables larger thatn 5x5.



#LOCAL AND SYSTEMIC SYMPTOMS - NICER BARPLOTS (Figure 4)--------------------------------

#' For this section we will use different databases that were created to analyze through 
#' barplots. We will order them again to present information HORIZONTALLY. 

#LOCAL SYMPTOMS: 
sin_loc = read.csv ('sin_locales.csv') #reading raw data
sin_loc1 = data.frame (table (sin_loc$sin_locales)) #counting the data
sin_loc1 = sin_loc1[order(sin_loc1$Freq, decreasing = T),] #odering the data (notice decreasing = T, so outputs are ordered from high to low frequencies)
sin_loc1$perc = (sin_loc1$Freq/sum(sin_loc1$Freq))*100 #calculating percentages and adding a column with this info
tots = sum(sin_loc1$Freq) #vector with totals 
vc = c(Var1 =  'total', Freq = tots, perc = (tots/tots)*100) #vector with name and totals for each column 
sin_loc2 = rbind (sin_loc1, t(data.frame(vc)))  #merging the row to the corresponding datacolumn 

write.csv (sin_loc2, './vars1/sin_locales.csv', row.names = F) #writing the new dataframe

#For the figure we will work with the dataframe before adding the totals: 
sin_loc1= sin_loc1[order(sin_loc1$Freq, decreasing = F),] #NOTICE ORDERING in FALSE (default) because I need this for an horizontal plot 

#plotting: 
pdf (paste ('sin_locales', '.pdf', sep = ''), width = 10.5, height = 8, paper = 'letter') #same page arrangement as before
par(mar = c(8,8,6,3)) #changing margin borders because the labels are larger 
barplot (sin_loc1$Freq, col = '#1c3bf9', #adding color 
         main = 'Síntomas locales', xlab = 'Frecuencia', 
         names = sin_loc1$Var1, las = 1, cex.names = 1, horiz = T, 
         ylab = '')
dev.off()


#SYSTEMIC SYMPTOMS
#' Same process with different database: 

sin_sist = read.csv ('sin_sistem.csv')
sin_sist1 = data.frame (table (sin_sist[,3]))
sin_sist1 = sin_sist1[order(sin_sist1$Freq, decreasing = T),]
sin_sist1$perc = (sin_sist1$Freq/sum(sin_sist1$Freq))*100
tots = sum(sin_sist1$Freq)
vc = c(Var1 =  'total', Freq = tots, perc = (tots/tots)*100)
sin_sist2 = rbind (sin_sist1, t(data.frame(vc)))

write.csv (sin_sist2, './vars1/sin_sistemicos.csv', row.names = F)

#For the figure we will work with the dataframe before adding the totals: 
#ordering by ID 
sin_sist1= sin_sist1[order(as.integer(sin_sist1$Freq), decreasing = F),]

#plotting: 
pdf (paste ('sin_sistemicos', '.pdf', sep = ''), width = 10.5, height = 8, paper = 'letter')
par(mar = c(8,8,6,3))
barplot (as.integer(sin_sist1$Freq), col = '#1c3bf9', 
         main = 'Síntomas sistémicos', xlab = 'Frecuencia', 
         names = sin_sist1$Var1, las = 1, cex.names = 1, horiz = T, 
         ylab = '')
dev.off()


#DEFINITIVE PLOT (BOTH AT THE SAME TIME)

#' Saved as a screenshot to maximize the vector image capacity of dev.new ()

#pdf (paste ('sintomas_both', '.pdf', sep = ''), width = 10.5, height = 8, paper = 'letter') 

par(mfrow = c(1,2)) #to have both plots at the same time
par(mar = c(4,8,4,9))
barplot (sin_loc1$Freq, col = '#1c3bf9',
         main = 'Síntomas locales', xlab = 'Frecuencia', 
         names = sin_loc1$Var1, las = 1, cex.names = 1, horiz = T, 
         ylab = '')
barplot (as.integer(sin_sist1$Freq), col = '#1c3bf9', 
         main = 'Síntomas sistémicos', xlab = 'Frecuencia', 
         names = sin_sist1$Var1, las = 1, cex.names = 1, horiz = T, 
         ylab = '')

#dev.off() #because I am not using pdf function then I don't need this 


#TREATMENT AND SURGERIES (Figure 5)------------------------------------

#' For this section I am following the steps as in the previous section without differences

#TREATMENTS
trat1 = read.csv ('tratamiento.csv')
trat11 = data.frame (table (trat1[,4]))
trat11 = trat11[order(trat11$Freq, decreasing = T),]
trat11 = trat11[-1,] #eliminating serum because everybody is receiving it 
trat11$perc = (trat11$Freq/sum(trat11$Freq))*100
tots = sum(trat11$Freq)
vc = c(Var1 =  'total', Freq = tots, perc = (tots/tots)*100)
trat12 = rbind (trat11, t(data.frame(vc)))

write.csv (trat12, './vars1/ttos.csv', row.names = F)

#For the figure we will work with the dataframe before adding the totals: 
#ordering by ID 
trat11= trat11[order(as.integer(trat11$Freq), decreasing = F),]
trat11$Var1 = c('Gasolina',  #Here I am adding the correct names for the variable explored
                'Infusión', 
                'Inmovilización',
                'Limón caliente',
                'Frío local',
                'Succión', 
                'Incisión o corte', 
                'Hierbas', 
                'Shaman/Curandero', 
                'Brebaje de hiel de serpiente', 
                'Torniquete')

#plotting: 
pdf (paste ('ttos', '.pdf', sep = ''), width = 10.5, height = 8, paper = 'letter')
par(mar = c(8,12,6,3))
barplot (as.integer(trat11$Freq), col = '#1c3bf9', 
         main = 'Tratamiento empírico', xlab = 'Frecuencia', 
         names = trat11$Var1, las = 1, cex.names = 1, horiz = T, 
         ylab = '')
dev.off()

#CIRUGIAS
cir1 = read.csv ('cirugia1.csv')
cir11 = data.frame (table (cir1[,3]))
cir11 = cir11[order(cir11$Freq, decreasing = T),]
cir11$perc = (cir11$Freq/sum(cir11$Freq))*100
tots = sum(cir11$Freq)
vc = c(Var1 =  'total', Freq = tots, perc = (tots/tots)*100)
cir12 = rbind (cir11, t(data.frame(vc)))
write.csv (cir12, './vars1/cirugia2.csv', row.names = F)

#For the figure we will work with the dataframe before adding the totals: 
#ordering by ID 
cir11= cir11[order(as.integer(cir11$Freq), decreasing = F),]
cir11$Var1 = c('Drenaje de absceso',  #Here I am adding the correct names for the variable explored
               'Prostatectomía', 
               'Necrectomía', 
               'Limpieza quirúrgica', 
               'Amputación', 
               'Fasciotomía')

#plotting: 
pdf (paste ('cirus', '.pdf', sep = ''), width = 10.5, height = 8, paper = 'letter')
par(mar = c(8,12,6,3))
barplot (as.integer(cir11$Freq), col = '#1c3bf9', 
         main = 'Intervenciones quirúrgicas', xlab = 'Frecuencia', 
         names = cir11$Var1, las = 1, cex.names = 1, horiz = T, 
         ylab = '')
dev.off()


#DEFINITIVE PLOT (BOTH AT THE SAME TIME)

#' Saved as a screenshot to maximize the vector image capacity of dev.new ()


par(mar = c(8,12,6,3))
par (mfrow = c(1,2))
barplot (as.integer(trat11$Freq), col = '#1c3bf9', 
         main = 'Tratamiento empírico', xlab = 'Frecuencia', 
         names = trat11$Var1, las = 1, cex.names = 1, horiz = T, 
         ylab = '')
barplot (as.integer(cir11$Freq), col = '#1c3bf9', 
         main = 'Intervenciones quirúrgicas', xlab = 'Frecuencia', 
         names = cir11$Var1, las = 1, cex.names = 1, horiz = T, 
         ylab = '')

#QUANTITATIVE VARIABLES --------------------------------

#' SOME THEORY: 
#' Quantitative variabels are explored using SUMMARY and also depicting them 
#' in histograms and boxplots 

#Age
summary (es1$edad)
boxplot (es1$edad)
hist(es1$edad)

#Hospitalization time (days)
summary (es1$dias_hospi)
boxplot (es1$dias_hospi)
hist(es1$dias_hospi)

#' We can see the distribution by simply plotting the variables 
#' 

plot (es1$dias_hospi)
plot (es1$edad)


#' We can instantly watch the categorization of quantiative variables considering a different
#' variable using boxplots, for example: 
#'

boxplot(edad ~ gravedad, data = es1) #age categorized by severity 
boxplot (dias_hospi ~gravedad, data = es1) #hospitalization time categorized by severity
boxplot (dias_hospi ~year, data = es1) #hospitalization time and year 


#' In order to actually compare if there is a statistical difference
#' between a quantitative variable ACROSS CATEGORIES, and I have more than two categories 
#' I need to perform an ANOVA... an example of how to do an anova can be found here
#' https://stackoverflow.com/questions/47546658/logistic-regression-on-factor-error-in-evalfamilyinitialize-y-values-must
#' The essence is to create the model in an object and then use SUMMARY to check 
#' for the p-values and the interpret... in this examples I am comparing
#' how sex and severity is related with hospitalization time... 
#' no differences where found... Thus: 

#Hospital time and sex: 
sexo = aov(tiempo_hospital ~ sexo, data = es1)
summary (sexo)

#Hospital time and age group: 
grupo_edad = aov(tiempo_hospital ~ edad_grupo, data = es1)
summary(grupo_edad)

#Hospital time and ethnicity:  
etnias = aov(tiempo_hospital ~ raza_etnia, data = es1)
summary (etnias)

#Hospital time and severity: 
gravedad = aov(tiempo_hospital ~ gravedad, data = es1)
summary (gravedad)

#' Through the summary I can see that any of the p values is less than 0.05 
#' and therefore there are not statistical significant differences between this 
#' comparisons. For ANOVA, I need to report the F value and the P value... potentially
#' I can assess this directly with a loop and a table of results as follows: 

#defining variables: 
colnames(es1)
vrs1 = c(5, 6, 7, 30)


res1 = NULL #dataframe to collect the results

#st = vrs1[1] #debugging 

for (st in vrs1){
  a_test = aov (tiempo_hospital ~ es1[,st], data = es1) #time to hospital versus all variables 
  nms = colnames(es1)[st] #defining names 
  f_val = summary(a_test)[[1]][1, 4:5] #collecting the F stat and the p value: https://stackoverflow.com/questions/3366506/extract-p-value-from-aov
  pre1 = cbind (nms, f_val) #creating previous dataframe
  res1 = rbind (res1, pre1) #adding the results to the dataframe
}

write.csv (res1, './res_quanti.csv', row.names = F)

#as it can be seen no statistical differences can be demosntrated


#' If I need to perform a comparison of quantitative variables between just two categories
#' for example hospitalization time between males and females, I can use a student t test. 
#' Remember that an ANOVA is basically a t test but with more than tree categories 
#' I can use the same semantic structure as for creating the boxplots:  
#'  

#visualization: 
boxplot (tiempo_hospital ~ sexo, data = es1)

#student t test: 
t.test(tiempo_hospital ~ sexo, data = es1,
       var.equal = TRUE, alternative = "two.sided")

#' the parameters of the student t test: var.equal = T and alternative = 'greater' 
#' came from the assumptions of this statistical test and they are explored in this 
#' website: https://statsandr.com/blog/student-s-t-test-in-r-and-by-hand-how-to-compare-two-groups-under-different-scenarios/
#' with far more information on how to report and present this data... 
#' briefly the var.equal = T is because the variance in our sample is different (the default is that the variance is the same)
#' and the greater represent that we want a one-tail test towards greater than rather than different (two.sided versus greater or less than)



#ABOUT LOGISTIC REGRESSIONS-----------------------------------------------
#' If I am interested in compare a qualitative variable against a 
#' quantitative varaible, then we can use logistic regressions
#' only at that time... so for example here we are compparing 
#' if sex can be influenced by the time people was in the hospital 
#' same as before, I need to put together a model and then call for the 
#' summary to actually obtain the results... 

ex1 = glm(formula = as.factor(es1$sexo) ~ es1$dias_hospi, family = 'binomial')
summary (ex1)



#DAILY AND CUMULATIVE NUMBER OF CASES PLOT-------------------------------

#' This example comes from databases of early COVID-19 in Ecuador


#PLOTS WITH A LOT OF PANELS-------------------------------

#' This example comes from databases of early COVID-19 in Ecuador





#####STUFF TO DO#######
#' add the example of how to create the plots with errors bars for OROV 
#' add the stuff of multiple panels from the coronavirus excess death stuff... 
#' add the part of the maps... 
#' create plots with different orientations 
#' Chi square and statistics and definitive table automatically 
#' Figures through GGPLOT 2 for sure
#' Definitive boxplots 
#' Boxplots with uncertainties 
#' ADD INFORMATION FROM THE CT VALUES PAPER, PLOTS THERE WHERE BEAUTIFUL :)
#' ADD MANUEL VILLANUEVA CODE  
#' PLOTS DEPICTING NUMBERS: https://stackoverflow.com/questions/53134623/text-in-barplot-in-r
#' 



