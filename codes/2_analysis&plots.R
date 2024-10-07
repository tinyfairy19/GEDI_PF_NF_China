library(tidyverse)
library(tidyr)
library(ggplot2)
library(spdep)
library(sp)
library(ggpubr)
library(colorspace)
library(ggpmisc)
library(broom)
library(agricolae)
library(car)

#prepare data
setwd('D:/pku/UES/GlobalForestVerticalStructure/China_plantation_natural_forest/0702')

#read
gdf_result <- read.csv('gdf_new_factors_norep_xy2.csv')
colnames(gdf_result)

gdf_result$rh98_diff <- gdf_result$n_rh98_mean - gdf_result$p_rh98_mean
gdf_result$fhd_diff <- gdf_result$n_fhd_mean - gdf_result$p_fhd_mean
gdf_result$pai_diff <- gdf_result$n_pai_mean - gdf_result$p_pai_mean
gdf_result$cov_diff <- gdf_result$n_cov_mean - gdf_result$p_cov_mean

#1 paired t test
rh98_ttest <- t.test(gdf_result$n_rh98_mean,gdf_result$p_rh98_mean,paired = T)
fhd_ttest <- t.test(gdf_result$n_fhd_mean,gdf_result$p_fhd_mean,paired = T)
pai_ttest <- t.test(gdf_result$n_pai_mean,gdf_result$p_pai_mean,paired = T)
cov_ttest <- t.test(gdf_result$n_cov_mean,gdf_result$p_cov_mean,paired = T)

rh98_ratio_n <- sum(gdf_result$n_rh98_mean>gdf_result$p_rh98_mean)/nrow(gdf_result)
rh98_ratio_p <- sum(gdf_result$n_rh98_mean<gdf_result$p_rh98_mean)/nrow(gdf_result)
fhd_ratio_n <- sum(gdf_result$n_fhd_mean>gdf_result$p_fhd_mean)/nrow(gdf_result)
fhd_ratio_p <- sum(gdf_result$n_fhd_mean<gdf_result$p_fhd_mean)/nrow(gdf_result)
pai_ratio_n <- sum(gdf_result$n_pai_mean>gdf_result$p_pai_mean)/nrow(gdf_result)
pai_ratio_p <- sum(gdf_result$n_pai_mean<gdf_result$p_pai_mean)/nrow(gdf_result)
cov_ratio_n <- sum(gdf_result$n_cov_mean>gdf_result$p_cov_mean)/nrow(gdf_result)
cov_ratio_p <- sum(gdf_result$n_cov_mean<gdf_result$p_cov_mean)/nrow(gdf_result)

rh98_ratio_n <- scales::percent(rh98_ratio_n, 0.01)
rh98_ratio_p <- scales::percent(rh98_ratio_p, 0.01)
fhd_ratio_n <-  scales::percent(fhd_ratio_n , 0.01)
fhd_ratio_p <-  scales::percent(fhd_ratio_p , 0.01)
pai_ratio_n <-  scales::percent(pai_ratio_n , 0.01)
pai_ratio_p <-  scales::percent(pai_ratio_p , 0.01)
cov_ratio_n <-  scales::percent(cov_ratio_n , 0.01)
cov_ratio_p <-  scales::percent(cov_ratio_p , 0.01)

rh98_mean_diff <- round(mean(gdf_result$n_rh98_mean - gdf_result$p_rh98_mean),digits = 2)
fhd_mean_diff <- round(mean(gdf_result$n_fhd_mean - gdf_result$p_fhd_mean),digits = 2)
pai_mean_diff <- round(mean(gdf_result$n_pai_mean - gdf_result$p_pai_mean),digits = 2)
cov_mean_diff <- round(mean(gdf_result$n_cov_mean - gdf_result$p_cov_mean),digits = 2)
rh98_mean_sd <- round(sd(gdf_result$n_rh98_mean - gdf_result$p_rh98_mean),digits = 2)
fhd_mean_sd <- round(sd(gdf_result$n_fhd_mean - gdf_result$p_fhd_mean),digits = 2)
pai_mean_sd <- round(sd(gdf_result$n_pai_mean - gdf_result$p_pai_mean),digits = 2)
cov_mean_sd <- round(sd(gdf_result$n_cov_mean - gdf_result$p_cov_mean),digits = 2)

gdf_stat1 <- gdf_result %>%
  summarize(n_rh98_m = mean(n_rh98_mean),
            n_rh98_sd = sd(n_rh98_mean),
            p_rh98_m = mean(p_rh98_mean),
            p_rh98_sd = sd(p_rh98_mean),)

p1 <- ggplot(data=gdf_result, aes(x = p_rh98_mean, y = n_rh98_mean,fill = rh98_diff)) +
  geom_point(shape=21,alpha=0.8,size=2.5,color='grey50') +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")+
  annotate("text",x=5,y=2,hjust=0,label = paste0("MD:",rh98_mean_diff,"±",rh98_mean_sd,"\nP < 0.01"),size= 3)+
  annotate("text",x=-Inf,y=Inf,hjust=-0.5,vjust=2.5,label = rh98_ratio_n,size= 4,color = "#009E73")+
  annotate("text",x=Inf,y=-Inf,hjust=1.4,vjust=-2.5,label = rh98_ratio_p,size= 4,color = "#56B4E9")+
  scale_fill_gradient2(low = "#56B4E9",mid='grey95',high = "#009E73")+
  #labs(x = "Plantation Forest", y = "Natural Forest")+
  xlim(0,35)+
  ylim(0,35)+
  labs(x = NULL, y = NULL)+
  ggtitle("RH98")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size=12),legend.title=element_blank(),
        legend.position="right",panel.grid=element_blank())+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))
p1

p2 <- ggplot(data=gdf_result, aes(x = p_fhd_mean, y = n_fhd_mean,fill = fhd_diff)) +
  geom_point(shape=21,alpha=0.8,size=2.5,color='grey50') +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")+
  annotate("text",x=1.25,y=1.1,hjust=0,label = paste0("MD:",fhd_mean_diff,"±",fhd_mean_sd,"\nP < 0.01"),size= 3)+
  annotate("text",x=-Inf,y=Inf,hjust=-0.5,vjust=2.5,label = fhd_ratio_n,size= 4,color = "#009E73")+
  annotate("text",x=Inf,y=-Inf,hjust=1.4,vjust=-2.5,label = fhd_ratio_p,size= 4,color = "#56B4E9")+
  scale_fill_gradient2(low = "#56B4E9",mid='grey95',high = "#009E73")+
  #labs(x = "Plantation Forest", y = "Natural Forest")+
  xlim(1,3.5)+
  ylim(1,3.5)+
  labs(x = NULL, y = NULL)+
  ggtitle("FHD")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size=12),legend.title=element_blank(),
        legend.position="right",panel.grid=element_blank())+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))
p2

p3 <- ggplot(data=gdf_result, aes(x = p_pai_mean, y = n_pai_mean,fill = pai_diff)) +
  geom_point(shape=21,alpha=0.8,size=2.5,color='grey50') +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")+
  annotate("text",x=0,y=-0.3,hjust=0,label = paste0("MD:",pai_mean_diff,"±",pai_mean_sd,"\nP < 0.01"),size= 3)+
  annotate("text",x=-Inf,y=Inf,hjust=-0.5,vjust=2.5,label = pai_ratio_n,size= 4,color = "#009E73")+
  annotate("text",x=Inf,y=-Inf,hjust=1.4,vjust=-2.5,label = pai_ratio_p,size= 4,color = "#56B4E9")+
  scale_fill_gradient2(low = "#56B4E9",mid='grey95',high = "#009E73")+
  #labs(x = "Plantation Forest", y = "Natural Forest")+
  labs(x = NULL, y = NULL)+
  xlim(-0.5,4)+
  ylim(-0.5,4)+
  ggtitle("PAI")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size=12),legend.title=element_blank(),
        legend.position="right",panel.grid=element_blank())+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))
p3

p4 <- ggplot(data=gdf_result, aes(x = p_cov_mean, y = n_cov_mean,fill = cov_diff)) +
  geom_point(shape=21,alpha=0.8,size=2.5,color='grey50') +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")+
  annotate("text",x=0,y=-0.12,hjust=0,label = paste0("MD:",cov_mean_diff,"±",cov_mean_sd,"\nP < 0.01"),size= 3)+
  annotate("text",x=-Inf,y=Inf,hjust=-0.5,vjust=2.5,label = cov_ratio_n,size= 4,color = "#009E73")+
  annotate("text",x=Inf,y=-Inf,hjust=1.4,vjust=-2.5,label = cov_ratio_p,size= 4,color = "#56B4E9")+
  scale_fill_gradient2(low = "#56B4E9",mid='grey95',high = "#009E73")+
  #labs(x = "Plantation Forest", y = "Natural Forest")+
  labs(x = NULL, y = NULL)+
  xlim(-0.2,1)+
  ylim(-0.2,1)+
  ggtitle("Cover")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size=12),legend.title=element_blank(),
        legend.position="right",panel.grid=element_blank())+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))
p4

plot1 <-  ggarrange(p1,p2,p3,p4,ncol=2,nrow=2,align = "v",
                    labels = c('(a)','(b)','(c)','(d)'),label.x = 0.1,
                    font.label = list(size = 10, face = "bold"))
plot1 <-  annotate_figure(plot1,bottom = text_grob("Planted Forest", color = "black", size = 12),
                          left = text_grob("Natural Forest", color = "black", size = 12, rot = 90))
plot1
ggsave(plot1,filename='./figs3/fig1-1.pdf',width = 8,height = 6,dpi=300)
ggsave(plot1,filename='./figs3/fig1-1.jpg',width = 8,height = 6,dpi=300)

#2 difference in tids
hcl_palettes('qualitative',plot=T)
hcl_palettes('Diverging',plot=T)
qualitative_hcl(8, palette = "Harmonic")

#func
plot_jitter_rank <- function(data,x,y,ys,title,ylimc){
  means <- aggregate(gdf_result_1_6[,ys], list(gdf_result_1_6$TID), FUN=mean)
  colnames(means) <- c('TID','mean')
  means <- means[order(means$mean,decreasing = T),]
  means$i <- c(1:6)
  print(means)
  p <- ggplot(data=data,aes(x={{x}},y={{y}}))+
    geom_jitter(aes(colour = factor({{x}}),reorder({{x}},-{{y}},FUN=mean)),alpha=0.2,
                position=position_jitterdodge(jitter.width = 1, 
                                              jitter.height = 0, 
                                              dodge.width = 0.8))+
    geom_hline(yintercept = 0,linetype = 2)+
    geom_hline(yintercept = mean(data[,ys],na.rm=T),linetype = 1,color = "red")+
    geom_point(data= means,aes(x=i,y=mean),shape=1,size=3)+
    scale_color_discrete_qualitative(palette = 'Harmonic')+
    labs(x = NULL,y=NULL) + 
    theme_bw() +
    ggtitle(title) + 
    theme(plot.title = element_text(hjust = 0.5,size=12)) +
    #ylim(-2,2) +
    theme(legend.position="none")+
    scale_x_discrete(breaks=seq(1,6,1))+
    theme(axis.text = element_text(size = 10,colour = "black"),panel.grid.major=element_blank(),panel.grid.minor=element_blank())
  return(p)
}

plot_jitter_rank2 <- function(data,x,y,xs,ys,title,label_ps){
  means <- aggregate(gdf_result_1_6[,ys], list(gdf_result_1_6$TID), FUN=mean)
  colnames(means) <- c('TID','mean')
  means <- means[order(means$mean,decreasing = T),]
  means$i <- c(1:6)
  print(means)
  
  f1 <- as.formula(paste(ys,'~',xs))
  aov_result <- aov(f1,data = data)
  LSD_result <- LSD.test(aov_result,'TID',p.adj = 'bonferroni')
  
  mark <- data.frame(LSD_result$groups)
  mark$group = row.names(mark)
  print(mark)
  
  p <- ggplot(data=data,aes(x={{x}},y={{y}}))+
    geom_jitter(aes(colour = factor({{x}}),reorder({{x}},-{{y}},FUN=mean)),alpha=0.2,
                position=position_jitterdodge(jitter.width = 1, 
                                              jitter.height = 0, 
                                              dodge.width = 0.8))+
    scale_color_discrete_qualitative(palette = 'Harmonic')+
    geom_hline(yintercept = 0,linetype = 2)+
    geom_hline(yintercept = mean(data[,ys],na.rm=T),linetype = 1,color = "red")+
    geom_point(data = means,aes(x=i,y=mean),shape=1,size=3)+
    geom_text(data=mark,
              aes(x=group,y=label_ps,label=groups),
              color="black",
              size = 4,
              fontface = "bold")+
    labs(x = NULL,y=NULL) + 
    theme_bw() +
    ggtitle(title) + 
    theme(plot.title = element_text(hjust = 0.5,size=12)) +
    #ylim(-2,2) +
    theme(legend.position="none")+
    scale_x_discrete(breaks=seq(1,6,1))+
    theme(axis.text = element_text(size = 10,colour = "black"),panel.grid.major=element_blank(),panel.grid.minor=element_blank())
  return(p)
}
gdf_result_1_6 <- gdf_result[gdf_result$TID %in% c('1','2','3','4','5','6'), ]
gdf_result_1_6_stat <- gdf_result_1_6 %>%
  group_by(TID) %>%
  summarise(rh98_diff_mean = mean(rh98_diff),
            fhd_diff_mean = mean(fhd_diff),
            pai_diff_mean = mean(pai_diff),
            cov_diff_mean = mean(cov_diff))

p21 <- plot_jitter_rank2(gdf_result_1_6,TID,rh98_diff,'TID','rh98_diff','RH98',8)
p22 <- plot_jitter_rank2(gdf_result_1_6,TID,fhd_diff,'TID','fhd_diff','FHD',0.5)
p23 <- plot_jitter_rank2(gdf_result_1_6,TID,pai_diff,'TID','pai_diff','PAI',1.2)
p24 <- plot_jitter_rank2(gdf_result_1_6,TID,cov_diff,'TID','cov_diff','Cover',0.25)


plot2 <-  ggarrange(p21,p22,p23,p24,ncol=2,nrow=2,align = "v",
                    labels = c('(a)','(b)','(c)','(d)'),label.x = 0.1,
                    font.label = list(size = 10, face = "bold"))
plot2 <-  annotate_figure(plot2,bottom = text_grob("Vegetation Region", color = "black", size = 12),
                          left = text_grob("Difference between \nNatural and Planted Forest", color = "black", size = 12, rot = 90))
plot2
ggsave(plot2,filename='./figs3/fig2.jpg',width = 8,height = 6,dpi=300)
ggsave(plot2,filename='./figs3/fig2.pdf',width = 8,height = 6,dpi=300)


#3 maps
library(tidyverse) 
library(sf) 
library(ggspatial)
library(cowplot) 
library(ggnewscale)

china_area <- read_sf("./veg_class/china_country.json")
China_veg <- read_sf("./veg_class/china_veg_region_WGS84.shp")
gdf_json <- read_sf("./gdf_new_factors_norep2.geojson")

st_crs(gdf_json)
gdf_json <- st_transform(gdf_json,'EPSG:3857')

gdf_json$rh98_diff <- gdf_json$n_rh98_mean - gdf_json$p_rh98_mean
gdf_json$fhd_diff  <- gdf_json$n_fhd_mean -  gdf_json$p_fhd_mean
gdf_json$pai_diff  <- gdf_json$n_pai_mean -  gdf_json$p_pai_mean
gdf_json$cov_diff  <- gdf_json$n_cov_mean -  gdf_json$p_cov_mean


nhmap <- ggplot(china_area)+
  geom_sf(color='black',fill=NA,size=0.8)+
  coord_sf(ylim = c(-4028017,-1877844),xlim = c(117131.4,2115095),
           crs = "+proj=laea +lat_0=40 +lon_0=104")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.line = element_blank(),
        legend.position = c(0.1,0.3),
        legend.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(linewidth = 1))+
  labs(x='', y='')

cols2<-c("#C7A76C", "#A4B266", "#76BB80", "#44BEA5", "#43BBC7", "#7DB0DD","grey80","grey80")
cols3<-c("#C7A76C", "#A4B266", "#76BB80", "#44BEA5", "#43BBC7", "#7DB0DD","#C87A8A" ,"#A782C3")

draw_map <- function(fill_var,title,breaks){
  title <- substitute(Delta~t, list(t = title))
  labels2 <- list(paste0('<',breaks[2]),breaks[2],breaks[3],breaks[4],breaks[5],paste0('>',breaks[5]))

  China_veg_map <- ggplot(China_veg)+
    geom_sf(color='grey50',aes(fill="white"),size=2,alpha=1,legend = F)+
    geom_sf(data = china_area,color='black',fill=NA,size=2)+
    scale_fill_manual(values="white",guide = "none")+
    #labs(fill='TID')+
    theme(legend.position = 'right')+
    new_scale_colour()+
    new_scale_fill()

  China_veg_map <- China_veg_map +
    geom_sf(data = gdf_json,color=NA,aes_string(fill=fill_var),alpha = 1,legend = T)+
    scale_fill_binned_diverging(palette= "Blue-Red 2",mid=0,breaks = breaks)+
    labs(x=NULL, y=NULL, color=NULL, title = title)+ #'Difference between Natural and Planted Forest'
    theme_bw()+
    coord_sf(crs = st_crs(3857),ylim = c(2000000,7000000))+
    theme(axis.text = element_blank(),
          axis.title.x = element_text(size=14),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank(),
          plot.title = element_text(hjust = 0.5,size=14),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.key.width = unit(55, "pt")
    )
    #coord_sf(crs = st_crs(3857)) #,ylim = c(15,55)

  plot_final <- ggdraw(China_veg_map) +
    draw_plot(nhmap, x = 0.7, y = 0.15, hjust  = 0 ,vjust = 0,width = 0.16, height = 0.3)
  return(plot_final)
}

hist(gdf_json$rh98_diff)
hist(gdf_json$fhd_diff)
hist(gdf_json$pai_diff)
hist(gdf_json$cov_diff)

p31 <- draw_map('rh98_diff','RH98',c(-Inf, -2,-0.5, 0.5, 2, Inf))
p32 <- draw_map('fhd_diff','FHD',c(-Inf, -0.2,-0.1, 0.1, 0.2, Inf))
p33 <- draw_map('pai_diff','PAI',c(-Inf, -0.4,-0.2, 0.2, 0.4, Inf))
p34 <- draw_map('cov_diff','Cover',c(-Inf, -0.1,-0.05, 0.05, 0.1, Inf))

p35 <-  ggplot(China_veg)+
  geom_sf(data = china_area,color='grey50',fill='grey50',size=0.8)+
  geom_sf(color='grey20',aes(fill=factor(TID)),size=0.8,alpha=0.8,legend = T)+
  scale_fill_manual(values=cols3)+
  labs(fill='TID')+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(hjust = 0.5,size=12),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(20, "pt"))+
  guides(fill = guide_legend(nrow  = 1, byrow = TRUE))+
  coord_sf(crs = st_crs(3857),ylim = c(2000000,7000000))
p35 <-  ggdraw(p35) +
  draw_plot(nhmap, x = 0.75, y = 0.1, hjust  = 0 ,vjust = 0,width = 0.16, height = 0.26)

ggsave(p35,filename='./figs3/fig2_map.jpg',width = 7,height = 5,dpi=300)
ggsave(p35,filename='./figs3/fig2_map.pdf',width = 7,height = 5,dpi=300)
# 
# p35 <-  ggplot(China_veg)+
#   geom_sf(color='grey20',aes(fill=factor(TID)),size=0.8,alpha=0.8,legend = T)+
#   scale_fill_manual(values=cols2)+
#   theme_pubclean()+
#   theme(axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid = element_blank(),
#         axis.line = element_blank(),
#         plot.title = element_text(hjust = 0.5,size=12),
#         legend.position = "right",
#         legend.title = element_blank(),
#         legend.key.width = unit(20, "pt"),
#         )+
#   guides(fill = guide_legend( ncol = 1, byrow = TRUE))+
#   coord_sf(crs = st_crs(3857),ylim = c(0,0),xlim = c(0,0))

plot3 <-  ggarrange(p31,p32,p33,p34,ncol=2,nrow=2,align = "v",
                    labels = c('(a)','(b)','(c)','(d)'),label.x = 0.1,
                    font.label = list(size = 12, face = "bold"))

ggsave(plot3,filename='./figs3/fig3-1.pdf',width = 10,height = 8,dpi=300)
ggsave(plot3,filename='./figs3/fig3-1.jpg',width = 10,height = 8,dpi=300)


#4 lme
#PCA
library(tidyverse)
library(broom)
library(vegan)
library(psych)
library(FactoMineR)
library(factoextra)
library(corrplot)

gdf_result_1_6$age_diff <- gdf_result_1_6$n_age - gdf_result_1_6$p_age

ggplot(gdf_result_1_6) +
  aes(x = age_diff, y = rh98_diff) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  facet_wrap(~TID)
ggplot(gdf_result_1_6) +
  aes(x = age_diff, y = fhd_diff) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  facet_wrap(~TID)
ggplot(gdf_result_1_6) +
  aes(x = age_diff, y = pai_diff) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  facet_wrap(~TID)
ggplot(gdf_result_1_6) +
  aes(x = age_diff, y = cov_diff) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  facet_wrap(~TID)


gdf_result_envs <- gdf_result_1_6[,c('b1','b4','b5','b6',
                                     'b7','b10','b11','b12',
                                     'b13','b14','b15','b16','b17')]
# mean of soil
gdf_result_envs$nitrogen_all_mean <- rowMeans(gdf_result_1_6[,c(35:40)])
gdf_result_envs$phh2o_all_mean <- rowMeans(gdf_result_1_6[,c(41:46)])
gdf_result_envs$clay_all_mean <- rowMeans(gdf_result_1_6[,c(47:52)])
gdf_result_envs$soc_all_mean <- rowMeans(gdf_result_1_6[,c(53:58)])

gdf_result_envs.scale <- scale(gdf_result_envs)
gdf_result_envs.cor <- cor(gdf_result_envs.scale)

#correlation of variables
cor.plot(gdf_result_envs.cor)
colnames(gdf_result_envs.scale) <- c("bio01","bio04","bio05","bio06","bio07","bio10","bio11","bio12","bio13","bio14","bio15","bio16","bio17",
                                     "soil nitrogen","soil pH","soil clay","soil SOC")
pca <- PCA(gdf_result_envs.scale)

#scree plot
fviz_eig(pca, addlabels = TRUE)

var <- get_pca_var(pca)
#variables contribution
plot.new()
corrplot(var$contrib,is.corr=F)

#to which extend the variable could represent the axis
corrplot(var$cos2, is.corr=FALSE)

fviz_pca_biplot(pca,
                label = "var" 
)

p_pca_vars <- fviz_pca_var(pca,  
             col.var="contrib",
             labelsize = 4,
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             alpha = 0.8,
             repel = T,
             title = '')+
  labs(color = 'Contribution')

ggsave(p_pca_vars,filename='./figs3/figs_pca.pdf',width = 7,height = 5,dpi=300)
ggsave(p_pca_vars,filename='./figs3/figs_pca.jpg',width = 7,height = 5,dpi=300)

pca3 <- pca$ind$cos2[,c(1,2,3)]

gdf_pca <- cbind(gdf_result_1_6[,c(3,148,150:156)],pca3)
gdf_pca[,c(5:12)] <- scale(gdf_pca[,c(5:12)])
nrow(gdf_pca)
gdf_pca2 <- filter(gdf_pca,!is.na(gdf_pca$age_diff))
nrow(gdf_pca2)

colMeans(gdf_pca2[,5:12])
apply(gdf_pca2[,5:12],2,sd)

#mixed linear model
library(nlme);
library(MuMIn)

#3 - interaction
#dim1 & 2
lme1_spatial1_ir <- lme(rh98_diff~age_diff*(Dim.1+Dim.2),data=gdf_pca2,random=~1|TID,correlation = corGaus(form = ~x+y))
lme1_spatial2_ir <- lme(rh98_diff~age_diff*(Dim.1+Dim.2),data=gdf_pca2,random=~1|TID,correlation = corExp(form = ~x+y))
lme1_spatial3_ir <- lme(rh98_diff~age_diff*(Dim.1+Dim.2),data=gdf_pca2,random=~1|TID,correlation = corLin(form = ~x+y))
lme1_spatial4_ir <- lme(rh98_diff~age_diff*(Dim.1+Dim.2),data=gdf_pca2,random=~1|TID,correlation = corRatio(form = ~x+y))
lme1_spatial5_ir <- lme(rh98_diff~age_diff*(Dim.1+Dim.2),data=gdf_pca2,random=~1|TID,correlation = corSpher(form = ~x+y))

lme2_spatial1_ir <- lme(fhd_diff~age_diff*(Dim.1+Dim.2),data=gdf_pca2,random=~1|TID,correlation = corGaus(form = ~x+y))
lme2_spatial2_ir <- lme(fhd_diff~age_diff*(Dim.1+Dim.2),data=gdf_pca2,random=~1|TID,correlation = corExp(form = ~x+y))
lme2_spatial3_ir <- lme(fhd_diff~age_diff*(Dim.1+Dim.2),data=gdf_pca2,random=~1|TID,correlation = corLin(form = ~x+y))
lme2_spatial4_ir <- lme(fhd_diff~age_diff*(Dim.1+Dim.2),data=gdf_pca2,random=~1|TID,correlation = corRatio(form = ~x+y))
lme2_spatial5_ir <- lme(fhd_diff~age_diff*(Dim.1+Dim.2),data=gdf_pca2,random=~1|TID,correlation = corSpher(form = ~x+y))

lme3_spatial1_ir <- lme(pai_diff~age_diff*(Dim.1+Dim.2),data=gdf_pca2,random=~1|TID,correlation = corGaus(form = ~x+y))
lme3_spatial2_ir <- lme(pai_diff~age_diff*(Dim.1+Dim.2),data=gdf_pca2,random=~1|TID,correlation = corExp(form = ~x+y))
lme3_spatial3_ir <- lme(pai_diff~age_diff*(Dim.1+Dim.2),data=gdf_pca2,random=~1|TID,correlation = corLin(form = ~x+y))
lme3_spatial4_ir <- lme(pai_diff~age_diff*(Dim.1+Dim.2),data=gdf_pca2,random=~1|TID,correlation = corRatio(form = ~x+y))
lme3_spatial5_ir <- lme(pai_diff~age_diff*(Dim.1+Dim.2),data=gdf_pca2,random=~1|TID,correlation = corSpher(form = ~x+y))


lme4_spatial1_ir <- lme(cov_diff~age_diff*(Dim.1+Dim.2),data=gdf_pca2,random=~1|TID,correlation = corGaus(form = ~x+y))
lme4_spatial2_ir <- lme(cov_diff~age_diff*(Dim.1+Dim.2),data=gdf_pca2,random=~1|TID,correlation = corExp(form = ~x+y))
lme4_spatial3_ir <- lme(cov_diff~age_diff*(Dim.1+Dim.2),data=gdf_pca2,random=~1|TID,correlation = corLin(form = ~x+y))
lme4_spatial4_ir <- lme(cov_diff~age_diff*(Dim.1+Dim.2),data=gdf_pca2,random=~1|TID,correlation = corRatio(form = ~x+y))
lme4_spatial5_ir <- lme(cov_diff~age_diff*(Dim.1+Dim.2),data=gdf_pca2,random=~1|TID,correlation = corSpher(form = ~x+y))

#
lme1_spatial4_ir2 <- lme(rh98_diff~age_diff*(Dim.1+Dim.2),data=gdf_pca2,random=~1|TID,correlation = corRatio(form = ~x+y))
lme2_spatial2_ir2 <- lme(fhd_diff~age_diff*(Dim.1+Dim.2),data=gdf_pca2,random=~1|TID,correlation = corExp(form = ~x+y))
lme3_spatial4_ir2 <- lme(pai_diff~age_diff*(Dim.1+Dim.2),data=gdf_pca2,random=~1|TID,correlation = corRatio(form = ~x+y))
lme4_spatial4_ir2 <- lme(cov_diff~age_diff*(Dim.1+Dim.2),data=gdf_pca2,random=~1|TID,correlation = corRatio(form = ~x+y))

hist(gdf_pca2$rh98_diff)
hist(gdf_pca2$fhd_diff)
hist(gdf_pca2$pai_diff)
hist(gdf_pca2$cov_diff)

a5 <- AIC(lme1_spatial1_ir,lme1_spatial2_ir,lme1_spatial3_ir,lme1_spatial4_ir,lme1_spatial5_ir)
a6 <- AIC(lme2_spatial1_ir,lme2_spatial2_ir,lme2_spatial3_ir,lme2_spatial4_ir,lme2_spatial5_ir)
a7 <- AIC(lme3_spatial1_ir,lme3_spatial2_ir,lme3_spatial3_ir,lme3_spatial4_ir,lme3_spatial5_ir)
a8 <- AIC(lme4_spatial1_ir,lme4_spatial2_ir,lme4_spatial3_ir,lme4_spatial4_ir,lme4_spatial5_ir)
a22 <- rbind(a5,a6,a7,a8)

s5 <- summary(lme1_spatial4_ir)
s6 <- summary(lme2_spatial2_ir)
s7 <- summary(lme3_spatial4_ir)
s8 <- summary(lme4_spatial4_ir)

aov5 <- anova(lme1_spatial4_ir)
aov6 <- anova(lme2_spatial2_ir)
aov7 <- anova(lme3_spatial4_ir)
aov8 <- anova(lme4_spatial4_ir)

aov5$Values <- s5$coefficients$fixed
aov6$Values <- s6$coefficients$fixed
aov7$Values <- s7$coefficients$fixed
aov8$Values <- s8$coefficients$fixed

aov5
aov6
aov7
aov8

r.squaredGLMM(lme1_spatial2_ir)
r.squaredGLMM(lme2_spatial2_ir)
r.squaredGLMM(lme3_spatial4_ir)
r.squaredGLMM(lme4_spatial4_ir)

# # all for corRatio to comparison
# s5 <- summary(lme1_spatial4_ir)
# s6 <- summary(lme2_spatial4_ir)
# s7 <- summary(lme3_spatial4_ir)
# s8 <- summary(lme4_spatial4_ir)
# 
# aov5 <- anova(lme1_spatial4_ir)
# aov6 <- anova(lme2_spatial4_ir)
# aov7 <- anova(lme3_spatial4_ir)
# aov8 <- anova(lme4_spatial4_ir)
# 
# aov5$Values <- s5$coefficients$fixed
# aov6$Values <- s6$coefficients$fixed
# aov7$Values <- s7$coefficients$fixed
# aov8$Values <- s8$coefficients$fixed
# 
# aov5
# aov6
# aov7
# aov8
# 
# r.squaredGLMM(lme1_spatial4_ir)
# r.squaredGLMM(lme2_spatial4_ir)
# r.squaredGLMM(lme3_spatial4_ir)
# r.squaredGLMM(lme4_spatial4_ir)
# 
# ##
# s5 <- summary(lme1_spatial4_ir2)
# s6 <- summary(lme2_spatial2_ir2)
# s7 <- summary(lme3_spatial4_ir2)
# s8 <- summary(lme4_spatial4_ir2)
# 
# aov5 <- anova(lme1_spatial4_ir2)
# aov6 <- anova(lme2_spatial2_ir2)
# aov7 <- anova(lme3_spatial4_ir2)
# aov8 <- anova(lme4_spatial4_ir2)
# 
# aov5$Values <- s5$coefficients$fixed
# aov6$Values <- s6$coefficients$fixed
# aov7$Values <- s7$coefficients$fixed
# aov8$Values <- s8$coefficients$fixed
# 
# aov5
# aov6
# aov7
# aov8
# 
# r.squaredGLMM(lme1_spatial4_ir2)
# r.squaredGLMM(lme2_spatial2_ir2)
# r.squaredGLMM(lme3_spatial4_ir2)
# r.squaredGLMM(lme4_spatial4_ir2)


## SI 
#0 NFs PFs
library(raster)
NFPF_map <- raster("D:/pku/UES/GlobalForestVerticalStructure/references/LCJ_2021/compressed_0.1.tif")
new_crs <- CRS("+init=epsg:3857")
NFPF_map_df <- projectRaster(NFPF_map, crs=new_crs,method = 'ngb')
NFPF_map_df <- as.data.frame(NFPF_map_df,xy=T)

colnames(NFPF_map_df)=c("x","y","type")
NFPF_map_df <- NFPF_map_df %>%
  filter(type != 0) %>%
  mutate(type = ifelse(type == 1, 'Planted', type)) %>%
  mutate(type = ifelse(type == 2, 'Natural', type))
  
NFPF_map_df$type <- factor(NFPF_map_df$type)

China_nfpf_map <- ggplot(china_area)+
  geom_sf(data = china_area,color='black',fill=NA,size=2)+
  coord_sf(crs = st_crs(3857),ylim = c(2000000,7000000))+
  geom_raster(data = NFPF_map_df,aes(x = x,y = y,fill = type))+
  scale_fill_manual(values=c('Planted'="#56B4E9",'Natural'="#009E73"))+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(hjust = 0.5,size=12),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(20, "pt"))+
  guides(fill = guide_legend( nrow  = 1, byrow = TRUE))

China_nfpf_map_nh <-  ggdraw(China_nfpf_map) +
  draw_plot(nhmap, x = 0.75, y = 0.1, hjust  = 0 ,vjust = 0,width = 0.16, height = 0.26)

ggsave(China_nfpf_map_nh,filename='./figs3/China_nfpf_map_nh.jpg',width = 7,height = 5,dpi=300)
ggsave(China_nfpf_map_nh,filename='./figs3/China_nfpf_map_nh.pdf',width = 7,height = 5,dpi=300)

#1
fit <- lm(p_fhd_mean ~ poly(p_age, 1,raw=TRUE),data=gdf_result)
fit <- lm(p_fhd_mean ~ p_age,data=gdf_result)

color = "#009E73"
color = "#56B4E9"

si_p1 <- ggplot(gdf_result, aes(x = p_age, y = p_fhd_mean)) +
  geom_point(color = "#56B4E9",alpha=0.4) +
  geom_point(aes(x = p_age, y = p_fhd_mean),data = gdf_result[gdf_result$fhd_diff< -0.1,],color = "red",alpha=0.4) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2, raw=TRUE), se = FALSE, color = "#FC4E07") +
  stat_poly_eq(aes(label = paste(..rr.label..,..p.value.label..,sep='~~~')),
               formula = y ~ poly(x, 2, raw=TRUE),
               parse = TRUE, label.x.npc = "left", label.y.npc = "top", 
               size = 3, color = "black")+
  theme_bw()+
  ylim(1.5,3)+
  labs(title = "Planted forest",
       x = NULL,
       y = "FHD") +
  theme(plot.title = element_text(hjust = 0.5,size=12),panel.grid=element_blank())
si_p2 <- ggplot(gdf_result, aes(x = p_age, y = p_pai_mean)) +
  geom_point(color = "#56B4E9",alpha=0.4) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2, raw=TRUE), se = FALSE, color = "#FC4E07") +
  stat_poly_eq(aes(label = paste(..rr.label..,..p.value.label..,sep='~~~')),
               formula = y ~ poly(x, 2, raw=TRUE),
               parse = TRUE, label.x.npc = "left", label.y.npc = "top", 
               size = 3, color = "black")+
  theme_bw()+
  ylim(0,4)+
  labs(title = NULL,
       x = NULL,
       y = "PAI") +
  theme(plot.title = element_text(hjust = 0.5,size=12),panel.grid=element_blank())

si_p3 <- ggplot(gdf_result, aes(x = n_age, y = n_fhd_mean)) +
  geom_point(color = "#009E73",alpha=0.4) +
  # geom_point(aes(x = n_age, y = n_fhd_mean),data = gdf_result[gdf_result$fhd_diff< -0.1,],color = "red",alpha=0.4) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2, raw=TRUE), se = FALSE, color = "#FC4E07") +
  stat_poly_eq(aes(label = paste(..rr.label..,..p.value.label..,sep='~~~')),
               formula = y ~ poly(x, 2, raw=TRUE),
               parse = TRUE, label.x.npc = "left", label.y.npc = "top", 
               size = 3, color = "black")+
  theme_bw()+
  ylim(1.5,3)+
  labs(title = "Natural forest",
       x = NULL,
       y = NULL) +
  theme(plot.title = element_text(hjust = 0.5,size=12),panel.grid=element_blank())
si_p4 <- ggplot(gdf_result, aes(x = n_age, y = n_pai_mean)) +
  geom_point(color = "#009E73",alpha=0.4) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2, raw=TRUE), se = FALSE, color = "#FC4E07") +
  stat_poly_eq(aes(label = paste(..rr.label..,..p.value.label..,sep='~~~')),
               formula = y ~ poly(x, 2, raw=TRUE),
               parse = TRUE, label.x.npc = "left", label.y.npc = "top", 
               size = 3, color = "black")+
  theme_bw()+
  ylim(0,4)+
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  theme(plot.title = element_text(hjust = 0.5,size=12),panel.grid=element_blank())

plot_si_1 <-  ggarrange(si_p1,si_p3,si_p2,si_p4,ncol=2,nrow=2,align = "v",
                    labels = c('(a)','(b)','(c)','(d)'),
                    font.label = list(size = 10, face = "bold"))
plot_si_1 <-  annotate_figure(plot_si_1,bottom = text_grob("Forest age", color = "black", size = 12))
ggsave(plot_si_1,filename='./figs2/figs3.pdf',width = 8,height = 6,dpi=300)
ggsave(plot_si_1,filename='./figs2/figs3.jpg',width = 8,height = 6,dpi=300)
