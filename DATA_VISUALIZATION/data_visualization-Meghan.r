library(dplyr)
library(stringr)
library(ggplot2)
library(ggfortify)
library(grid)
library(gridExtra)
library(cowplot)




mdat<-read.csv("data_analysis-Kristen.csv")

#full is chla ~ fluor + cond + pH + temp + rad

#get y axis residuals
chla_nofluor<- lm(chla ~ cond + pH + temp + rad, data = mdat)
chla_nocond<- lm(chla ~ fluor + pH + temp + rad, data = mdat)
chla_nopH<- lm(chla ~ fluor + cond + temp + rad, data = mdat)
chla_notemp<- lm(chla ~ fluor + cond + pH + rad, data = mdat)
chla_norad<- lm(chla ~ fluor + cond + pH + temp, data = mdat)

#get x axis residuals
fluor_others<- lm(fluor ~ cond + pH + temp + rad, data = mdat)
cond_others<- lm(cond ~ fluor + pH + temp + rad, data = mdat)
pH_others<- lm(pH ~ fluor + cond + temp + rad, data = mdat)
temp_others<- lm(temp ~ fluor + cond + pH + rad, data = mdat)
rad_others<- lm(rad ~ fluor + cond + pH + temp, data = mdat)

#add the residuals of these models to the dataframe
mdat<- mdat %>% mutate(r_chla_nofluor = residuals(chla_nofluor), 
                       r_chla_nocond = residuals(chla_nocond),
                       r_chla_nopH = residuals(chla_nopH), 
                       r_chla_notemp = residuals(chla_notemp), 
                       r_chla_norad = residuals(chla_norad),
                       r_fluor_others = residuals(fluor_others), 
                       r_cond_others = residuals(cond_others),
                       r_pH_others = residuals(pH_others), 
                       r_temp_others = residuals(temp_others), 
                       r_rad_others = residuals(rad_others))
#e.g. r_fluor_others = residuals(fluor_others), r_chla_nofluor = residuals(chla_nofluor)
  
#example plot
fluor_plot<-ggplot(mdat, aes(x = r_fluor_others, y = r_chla_nofluor))+
    geom_point(size = 2.5, colour = "orangered2")+
    scale_x_continuous(name = "Residuals for log(Fluorescence)")+
    scale_y_continuous(name = "Residuals of Chlorophyll a,\n no Fluorescence")+
    annotate(geom= "text", x = 1, y = 5, 
             label= "italic(R)^2 == -0.038", size= 5, colour = "black", fontface = 1, parse = TRUE)+
    geom_smooth(method="lm", se=FALSE, colour="black")+ theme_classic()
fluor_plot  
  
cond_plot<-ggplot(mdat, aes(x = r_cond_others, y = r_chla_nocond))+
  geom_point(size = 2.5, colour = "orangered2")+
  scale_x_continuous(name = "Residuals for Conductivity")+
  scale_y_continuous(name = "Residuals of Chlorophyll a,\n no Conductivity")+
  annotate(geom= "text", x = 10, y = 5, 
           label= "italic(R)^2 == 2.51", size= 5, colour = "black", fontface = 1, parse = TRUE)+
  geom_smooth(method="lm", se=FALSE, colour="black")+ theme_classic()


pH_plot<-ggplot(mdat, aes(x = r_pH_others, y = r_chla_nopH))+
  geom_point(size = 2.5, colour = "orangered2")+
  scale_x_continuous(name = "Residuals for pH")+
  scale_y_continuous(name = "Residuals of Chlorophyll a,\n no pH")+
  annotate(geom= "text", x = 0.5, y = 5, 
           label= "italic(R)^2 == -0.213", size= 5, colour = "black", fontface = 1, parse = TRUE)+
  geom_smooth(method="lm", se=FALSE, colour="black")+ theme_classic()


temp_plot<-ggplot(mdat, aes(x = r_temp_others, y = r_chla_notemp))+
  geom_point(size = 2.5, colour = "orangered2")+
  scale_x_continuous(name = "Residuals for Temperature")+
  scale_y_continuous(name = "Residuals of Chlorophyll a,\n no Temperature")+
  annotate(geom= "text", x = 1, y = 5, 
           label= "italic(R)^2 == -1.74", size= 5, colour = "black", fontface = 1, parse = TRUE)+
  geom_smooth(method="lm", se=FALSE, colour="black")+ theme_classic()


rad_plot<-ggplot(mdat, aes(x = r_rad_others, y = r_chla_norad))+
  geom_point(size = 2.5, colour = "orangered2")+
  scale_x_continuous(name = "Residuals for Square Root(Radiation)")+
  scale_y_continuous(name = "Residuals of Chlorophyll a,\n no Radiation")+
  annotate(geom= "text", x = 1, y = 5, 
           label= "italic(R)^2 == -0.028", size= 5, colour = "black", fontface = 1, parse = TRUE)+
  geom_smooth(method="lm", se=FALSE, colour="black")+ theme_classic()


#bring the plots together
plot_grid(fluor_plot, cond_plot, pH_plot, temp_plot, rad_plot,   
    align = "hv",
    axis = "tblr",
    ncol = 2,
    labels = "AUTO",
    label_size = 15,
    hjust = -4,
    vjust = 2.25,
    rel_widths = c(0.94,1,1))

    