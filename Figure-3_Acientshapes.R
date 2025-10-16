#load packages
install.packages("Momocs")
library(Momocs)
library(dplyr)
#import jpegs
input2022_nopetiole<-import_jpg()

-----------------------------------------------------------------  
#newshape dataframe
#grepl to parse out specific groups for the ancient varieties.
  View(input2022_nopetiole[grepl("Banpeiyu|Mato|Egami|Suisho|Tahitian|Yemen|Mexican_citron|Buddha's hand|C_hystrix|C_micrantha|Cabuyo|Sun Chu Sha|cleopatra|Dancy|Imperial|Willow|C_ichangensis|Flying|Rubidoux_T|Pomeroy|M australasica|polyandra|Marumi|Yunnanese cit|Nordmann|C_lycoperisica", names(input2022_nopetiole))])
ancienttypes<-(input2022_nopetiole[grepl("Banpeiyu|Mato|Egami|Suisho|Tahitian|Yemen|Mexican_citron|Buddha's hand|C_hystrix|C_micrantha|Cabuyo|Sun Chu Sha|cleopatra|Dancy|Imperial|Willow|C_ichangensis|Flying|Rubidoux_T|Pomeroy|M australasica|polyandra|Marumi|Yunnanese cit|Nordmann|C_lycoperisica", names(input2022_nopetiole))])
ancienttypes2<-(input2022_nopetiole[grepl("Banpeiyu|Mato|Egami|Sushio|Tahitian|Yemen|Mexican_citron|
                               Buddha|C_hystrix|C_jatipes|C_micrantha|Cabuyo|Sun Chu Sha|cleopatra
                                         |Dancy|Imperial|Willow|C_ichangensis|Flying|Rubidoux_Trifol
                                         |Pomeroy|M australasica", names(input2022_nopetiole))],input_groupfixed[grepl("Nordmann")])

#plot with no labels
#sanity check to see if data is viable
ancientOut<-Out(ancienttypes)
ancientpca<-efourier(ancientOut)%>%PCA()
plot_PCA(ancientpca)
Out(ancienttypes)%>%efourier()%>%plot_PCA()
--------------------------------------------------------------------------------------  
#tibble creation
#You create a csv that has the names of each sample and any other 
#category or identifier you may want to use.
write.table(names(ancienttypes), file = "~/Desktop/Output/ancienttypes3.csv", append = FALSE, quote = FALSE, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = TRUE,
              col.names = TRUE, qmethod = c("escape", "double"))
#check names
names(ancienttypes)
#Build different tibbles to fit your needs
tibbleancient=tibble(Category=ancienttypes2$Category,Variety=ancienttypes2$Variety,IID=ancienttypes2$IID, Leaf_number=ancienttypes2$x,)
tibbleancient3=tibble(leaf_number=ancienttypes3$x,Variety=ancienttypes3$Variety,Category=ancienttypes3$Category)
tibbleancient4=tibble(leaf_number=ancienttypes3$x,Variety=ancienttypes3$Variety,Category=ancienttypes3$Category)
--------------------------------------------------------------------------------------
 #make the plot
ancientOut3<-Out(ancienttypes, fac=dplyr::data_frame(tibbleancient3))
ancientOut<-Out(ancienttypes, fac=tibbleancient)
efancient3<-efourier(ancientOut3)
ancientpca3<-PCA(efancient3)
efourier(ancientOut)%>%PCA()
plot_PCA(ancientpca,~Category,labelpoints = TRUE, labelgroups = TRUE )
plot_PCA(ancientpca, ~Category) #had some label problems due to "ie" mispellings
?plot_PCA()


--------------------------------------------------------------------------------------
#meanshape
  mshapeancient3<-MSHAPES(ancientpca3)
?MSHAPES
#MSHAPES(efancient, ~Category)%>%plot_MSHAPES() #worked
#MSHAPES(efancient, ~Vareity)
ancientmshape3<-MSHAPES(ancientpca3, ~Variety)#worked
ancientmshape3<-MSHAPES(efancient3, ~Category)
ancientoutmshape3=Out(ancientmshape3, fac=dplyr::data_frame(tibblemshapeancient3))
names(ancientmshape$shp)#worked
names(ancientmshape3$shp
#Here we once again repeat creating a csv on your own computer 
#to edit and add your own labels and metadata in a dataframe/tibble
#this also give you the abiolity to fix any errors.
      write.table(tibblemshapeancient3, file = "~/Desktop/Output/tibblemshapeancient3.csv", append = FALSE, quote = FALSE, sep = ",",
                  eol = "\n", na = "NA", dec = ".", row.names = TRUE,
                  col.names = TRUE, qmethod = c("escape", "double"))
#shapes name list
      ancientmshape$shp
      ancientmshape2<-ancientmshape$shp
      ancientmshape31<-ancientmshape3$shp
#convert to out
      ancientoutmshape3=Out(ancientmshape31, fac=dplyr::data_frame(tibblemshapeancient3))
      ancientoutmshape3=Out(ancientmshape3, fac=dplyr::data_frame(tibblemshapeancient3))
      ancientmshape3$shp
#Panel Viewing quality check
      panel(ancientoutmshape3, borders="black", names = TRUE)
      efancientmshape3<-efourier(ancientoutmshape3)
      pcamshapeancient3<-PCA(efancientmshape3)
#Plotting     
      plot_PCA(pcamshapeancient3,~Category, zoom = 1.5,legend=TRUE)
      plot_PCA(pcamshapeancient3,~Variety, legend=TRUE, zoom = 2.0)
      plot_PCA(pcamshapeancient, ~Category,chull=FALSE, labelgroups = FALSE,zoom=1.75, morphospace_position = "PC coordinates")
      plot_PCA(pcamshapeancient, ~Category,chull=FALSE, labelgroups = FALSE,legend=TRUE,zoom=1.00, morphospace_position = "PC coordinates")
      plot_PCA(pcamshapeancient, ~names(ancientmshape$shp)
               plot_PCA(pcamshapeancient, morphospace_position = "PC coordinates")
               ?plot_PCA
               PCcontrib(pcamshapeancient3)
   