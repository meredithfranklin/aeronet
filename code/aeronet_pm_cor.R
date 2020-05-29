#Calculate AOD coincident 440nm correlation with PM2.5

library(data.table)
library(dplyr)
library(ggplot2)

#load data
#inv.pm.spec <- feead("G:\\My Drive\\AERONET-MISR\\INV data\\QGS\\inv.spec.data.csv")
inv.pm.spec <- fread("/Volumes/GoogleDrive/My Drive/AERONET-MISR/Paper#2-RSoE/aeronet/aeronet/data/inv.pm.data.csv")

#Calculate correlation coefficient
aodpm <- inv.pm.spec%>%
  group_by(Site_Name)%>%
  summarise(R=cor(PM25,`VolC-F`)^2,Lat=mean(`Latitude(Degrees)`),
            Long=mean(`Longitude(Degrees)`),N=sum(N))
setDT(aodpm)
mean(aodpm$R)
#plot by R value by site
aodpm %>%
  arrange(Long) %>%
  mutate(Site_Name=factor(Site_Name, levels=Site_Name)) %>%
  #color by R, size by N
  ggplot(aes(x=Site_Name,y=R))+
  geom_line(group=1)+
  geom_point(aes(color=N, size=R))+
  theme_minimal()+
  scale_color_viridis_c(option="C")+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))
mean(aodpm$R)
#ggsave("G:\\My Drive\\AERONET-MISR\\INV data\\figures\\aod440.pm.png", width=8,height=5)  

ggsave("/Volumes/GoogleDrive/My Drive/AERONET-MISR/Paper#2-RSoE/aeronet/aeronet/results/aod675.pm.png")

#Calculate correlation coefficient
aodpm2 <- inv.pm.spec%>%
  group_by(Site_Name)%>%
  summarise(R=cor(PM25,`REff-T`)^2,Lat=mean(`Latitude(Degrees)`),
            Long=mean(`Longitude(Degrees)`),N=sum(N))
setDT(aodpm2)
mean(aodpm2$R)

#plot by R value by site
aodpm2 %>%
  arrange(Long) %>%
  mutate(Site_Name=factor(Site_Name, levels=Site_Name)) %>%
  #color by R, size by N
  ggplot(aes(x=Site_Name,y=R))+
  geom_line(group=1)+
  geom_point(aes(color=N, size=R))+
  theme_minimal()+
  scale_color_viridis_c(option="C")+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))

ggsave("/Volumes/GoogleDrive/My Drive/AERONET-MISR/Paper#2-RSoE/aeronet/aeronet/results/extaod870.pm.png")
