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
library(nlme)

#### data preparation ####
#prepare data
setwd('D:/pku/UES/GlobalForestVerticalStructure/China_plantation_natural_forest/revise_data_20251210/50km')

#read
gdf_result <- read.csv('gdf_new_factors_norep_xy3_agematch_unweighted_samplenum_v2.csv')
colnames(gdf_result)

#sample nums
gdf_result %>% 
  select(nat_sample_size,plt_sample_size) %>% 
  colSums()
nrow(gdf_result)
unique(gdf_result$TID)


#### fig1 compare differences ####
vars <- c("rh98", "fhd_normal", "pai", "cover")

# differences
for (v in vars) {
  gdf_result[[paste0(v, "_diff")]] <- gdf_result[[paste0("nat_", v)]] - gdf_result[[paste0("plt_", v)]]
}

# ratios
ratios <- lapply(vars, function(v) {
  n_col <- gdf_result[[paste0("nat_", v)]]
  p_col <- gdf_result[[paste0("plt_", v)]]
  ratio_n <- scales::percent(sum(n_col > p_col) / nrow(gdf_result), accuracy = 0.01)
  ratio_p <- scales::percent(sum(n_col < p_col) / nrow(gdf_result), accuracy = 0.01)
  c(ratio_n = ratio_n, ratio_p = ratio_p)
})
names(ratios) <- vars

#gls spatial auto correlation statistic

#gls - 1 spatial correlation structure selection
gls_select_spatial <- function(df, diff_var, x = "x", y = "y") {
  
  f <- as.formula(paste0(diff_var, " ~ 1"))
  
  models <- list(
    corGaus  = gls(f, correlation = corGaus(form = as.formula(paste0("~", x, "+", y))), data = df, na.action = na.omit),
    corExp   = gls(f, correlation = corExp(form  = as.formula(paste0("~", x, "+", y))), data = df, na.action = na.omit),
    corLin   = gls(f, correlation = corLin(form  = as.formula(paste0("~", x, "+", y))), data = df, na.action = na.omit),
    corRatio = gls(f, correlation = corRatio(form= as.formula(paste0("~", x, "+", y))), data = df, na.action = na.omit),
    corSpher = gls(f, correlation = corSpher(form= as.formula(paste0("~", x, "+", y))), data = df, na.action = na.omit)
  )
  print('model finished')
  res_df <- do.call(rbind, lapply(names(models), function(name) {
    data.frame(
      variable = diff_var,
      spatial_structure = name,
      AIC = AIC(models[[name]])
    )
  }))
  
  return(res_df)
}

diff_vars <- c("rh98_diff","fhd_normal_diff","pai_diff","cover_diff")
aic_results <- do.call(rbind, lapply(diff_vars, function(v) gls_select_spatial(gdf_result, v)))
aic_results %>% arrange(variable,AIC)

#gls - 2 mean difference and confidence intervals
gls_diff_stats <- function(df, diff_var, x = "x", y = "y") {
  
  f <- as.formula(paste0(diff_var, " ~ 1"))
  
  gls_model <- gls(f, correlation = corRatio(form = as.formula(paste0("~", x, "+", y))),
                   data = df, na.action = na.omit)
  
  # p-value
  gls_summary <- summary(gls_model)
  # print(gls_summary)
  pval <- gls_summary$tTable["(Intercept)", "p-value"]
  
  ci <- intervals(gls_model, level = 0.95)$coef
  est <- ci["(Intercept)", "est."]
  ci_lower <- ci["(Intercept)", "lower"]
  ci_upper <- ci["(Intercept)", "upper"]
  # print(ci)
  
  return(data.frame(
    variable = diff_var,
    mean = round(est, 2),
    CI_lower = round(ci_lower, 2),
    CI_upper = round(ci_upper, 2),
    p_value = signif(pval, 2)
  ))
}

diff_vars <- c("rh98_diff","fhd_normal_diff","pai_diff","cover_diff")
gls_results <- do.call(rbind, lapply(diff_vars, function(v) gls_diff_stats(gdf_result, v, x="x", y="y")))
gls_results <- gls_results %>%
  mutate(p_label = ifelse(p_value < 0.01, "p < 0.01", paste0("p = ", signif(p_value, 2))))

#check
gdf_result[,c('grid_id','rh98_diff')] %>% 
  arrange(rh98_diff)

gdf_stat1 <- gdf_result %>%
  summarize(n_rh98_m = mean(nat_rh98),
            n_rh98_sd = sd(nat_rh98),
            p_rh98_m = mean(plt_rh98),
            p_rh98_sd = sd(plt_rh98),)

plot_pairwise_diff <- function(data, 
                               var_name,         
                               x_col, y_col,     
                               fill_col,         
                               title = NULL,
                               xlim_range = c(0, 35),
                               ylim_range = c(0, 35),
                               # md_text_pos = c(5, 2),
                               p_label = "P < 0.01",
                               tick_breaks = c(),
                               tick_limits = c(),
                               ratio_n = NULL,
                               ratio_p = NULL) {
  
  md_str <- gls_results %>% 
    filter(variable == paste0(var_name,'_diff')) %>% 
    reframe(
      mean = format(mean , nsmall = 2),
      ci_lwr = round(CI_lower, 2),
      ci_upr = round(CI_upper, 2),
      pval = p_label,
      label = paste0("MD: ", mean, 
                     " (", ci_lwr, ", ", ci_upr, ")\n",
                     pval)
    ) %>%
    pull(label)
  
  print(md_str)
  if (is.null(ratio_n)) ratio_n <- ratios[[var_name]]["ratio_n"]
  if (is.null(ratio_p)) ratio_p <- ratios[[var_name]]["ratio_p"]
  
  md_text_pos <- c(xlim_range[1] + 0.1 * diff(xlim_range),ylim_range[1] + 0.05 * diff(ylim_range))
  print(md_text_pos)
  
  ggplot(data = data, aes_string(x = x_col, y = y_col, fill = fill_col)) +
    geom_point(shape = 21, alpha = 0.8, size = 2, color = 'grey50') +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    annotate("text", x = md_text_pos[1], y = md_text_pos[2], hjust = 0,
             label = md_str, size = 3) +
    annotate("text", x = -Inf, y = Inf, hjust = -0.5, vjust = 2.5, 
             label = ratio_n, size = 4, color = "#009E73") +
    annotate("text", x = Inf, y = -Inf, hjust = 1.4, vjust = -2.5, 
             label = ratio_p, size = 4, color = "#56B4E9") +
    scale_fill_gradient2(low = "#56B4E9", mid = 'grey95', high = "#009E73",
                         breaks = tick_breaks,
                         limits = tick_limits,
                         oob = scales::squish,
                         name = NULL) +
    xlim(xlim_range) +
    ylim(ylim_range) +
    labs(x = NULL, y = NULL) +
    ggtitle(title) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 12),
          legend.text = element_text(color = "black"),
          legend.title = element_blank(),
          legend.position = "right",
          panel.grid = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
          axis.ticks = element_line(color = 'black',linewidth = 0.25),
          axis.text = element_text(color = 'black',size = 10)) +
    guides(fill = guide_colourbar(barwidth = 0.75,
                                  barheight = 10
                                  ))
}

p1 <- plot_pairwise_diff(
  data = gdf_result,
  var_name = "rh98",
  x_col = "plt_rh98",
  y_col = "nat_rh98",
  fill_col = "rh98_diff",
  title = "RH98 (m)",
  xlim_range = c(0, 35),
  ylim_range = c(0, 35),
  # md_text_pos = c(5, 2),
  tick_breaks = seq(-8,8,4),
  tick_limits = c(-8,8)
)
p1
p2 <- plot_pairwise_diff(
  data = gdf_result,
  var_name = "fhd_normal",
  x_col = "plt_fhd_normal",
  y_col = "nat_fhd_normal",
  fill_col = "fhd_normal_diff",
  title = "FHD",
  xlim_range = c(1.5, 3.5),
  ylim_range = c(1.5, 3.5),
  # md_text_pos = c(1.75,1.6),
  tick_breaks = seq(-0.5,0.5,0.25),
  tick_limits = c(-0.5,0.5)
)
p2
p3 <- plot_pairwise_diff(
  data = gdf_result,
  var_name = "pai",
  x_col = "plt_pai",
  y_col = "nat_pai",
  fill_col = "pai_diff",
  title = "PAI",
  xlim_range = c(-0.5, 4.5),
  ylim_range = c(-0.5, 4.5),
  # md_text_pos = c(0.5,0),
  tick_breaks = seq(-1,1,0.5),
  tick_limits = c(-1,1)
)
p3
p4 <- plot_pairwise_diff(
  data = gdf_result,
  var_name = "cover",
  x_col = "plt_cover",
  y_col = "nat_cover",
  fill_col = "cover_diff",
  title = "Canopy cover",
  xlim_range = c(0, 1),
  ylim_range = c(0, 1),
  # md_text_pos = c(0.2,0.1),
  tick_breaks = seq(-0.2,0.2,0.1),
  tick_limits = c(-0.2,0.2)
)
p4

plot1 <-  ggarrange(p1,p2,p3,p4,ncol=2,nrow=2,
                    labels = c('(a)','(b)','(c)','(d)'),label.x = 0.1,
                    align = 'hv',
                    font.label = list(size = 10, face = "bold"))
plot1 <-  annotate_figure(plot1,bottom = text_grob("Tree plantations", color = "black", size = 12),
                          left = text_grob("Natural forest", color = "black", size = 12, rot = 90))
plot1
ggsave(plot1,filename='./figs/fig1-1.pdf',width = 8,height = 6,dpi=300)
ggsave(plot1,filename='./figs/fig1-1.jpg',width = 8,height = 6,dpi=300)

#### 2 difference in tids ####
hcl_palettes('qualitative',plot=T)
hcl_palettes('Diverging',plot=T)
qualitative_hcl(8, palette = "Harmonic")

#func

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
              aes(x=group,y=Inf,label=groups,vjust = 2),
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
    theme(plot.title = element_text(hjust = 0.5, size = 12),
          panel.grid = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
          axis.ticks = element_line(color = 'black',linewidth = 0.25),
          axis.text = element_text(color = 'black',size = 10))
  return(p)
}

gdf_result_1_6 <- gdf_result[gdf_result$TID %in% c('1','2','3','4','5','6'), ]

#check minimum value
gdf_result_1_6[which.min(gdf_result_1_6$rh98_diff), c('nat_rh98','plt_rh98')]

gdf_result_1_6_stat <- gdf_result_1_6 %>%
  group_by(TID) %>%
  summarise(rh98_diff_mean = mean(rh98_diff),
            fhd_normal_diff_mean = mean(fhd_normal_diff),
            pai_diff_mean = mean(pai_diff),
            cover_diff_mean = mean(cover_diff))

p21 <- plot_jitter_rank2(gdf_result_1_6,TID,rh98_diff,'TID','rh98_diff','RH98 (m)',8)
p22 <- plot_jitter_rank2(gdf_result_1_6,TID,fhd_normal_diff,'TID','fhd_normal_diff','FHD',0.5)
p23 <- plot_jitter_rank2(gdf_result_1_6,TID,pai_diff,'TID','pai_diff','PAI',1.2)
p24 <- plot_jitter_rank2(gdf_result_1_6,TID,cover_diff,'TID','cover_diff','Canopy cover',0.25)


plot2 <-  ggarrange(p21,p22,p23,p24,ncol=2,nrow=2,align = "v",
                    labels = c('(a)','(b)','(c)','(d)'),label.x = 0.1,
                    font.label = list(size = 10, face = "bold"))
plot2 <-  annotate_figure(plot2,bottom = text_grob("Vegetation region", color = "black", size = 12),
                          left = text_grob("Difference between \nnatural forests and tree plantations", color = "black", size = 12, rot = 90))
plot2
ggsave(plot2,filename='./figs/fig2_unit.jpg',width = 8,height = 6,dpi=300)
ggsave(plot2,filename='./figs/fig2_unit.pdf',width = 8,height = 6,dpi=300)


#### 3 maps ####
library(tidyverse) 
library(sf) 
library(ggspatial)
library(cowplot) 
library(ggnewscale)
library(paletteer)

china_area <- read_sf("../veg_class/china_country.json")
China_veg <- read_sf("../veg_class/china_veg_region_WGS84.shp")
gdf_json <- read_sf("./gdf_new_factors_norep3_agematch_unweighted_samplenum.geojson")

st_crs(gdf_json)
gdf_json <- st_transform(gdf_json,'EPSG:3857')

gdf_json$rh98_diff <- gdf_json$nat_rh98 - gdf_json$plt_rh98
gdf_json$fhd_diff  <- gdf_json$nat_fhd_normal -  gdf_json$plt_fhd_normal
gdf_json$pai_diff  <- gdf_json$nat_pai -  gdf_json$plt_pai
gdf_json$cov_diff  <- gdf_json$nat_cover -  gdf_json$plt_cover


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
    scale_fill_binned_diverging(palette= "Blue-Red 3",mid=0,breaks = breaks)+
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
          legend.text = element_text(size=12),
          legend.key.width = unit(55, "pt")
    )
    #coord_sf(crs = st_crs(3857)) #,ylim = c(15,55)

  plot_final <- ggdraw(China_veg_map) +
    draw_plot(nhmap, x = 0.7, y = 0.15, hjust  = 0 ,vjust = 0,width = 0.16, height = 0.3)
  return(plot_final)
}

rev(paletteer_c("ggthemes::Green-Blue Diverging", 5))
draw_map <- function(fill_var,title,breaks){
  title <- substitute(Delta~t, list(t = title))

  labels2 <- c(
    paste0('<', breaks[2]),
    breaks[2],
    breaks[3],
    breaks[4],
    breaks[5],
    paste0('>', breaks[5])
  )
  
  my_pal <- rev(paletteer_c("ggthemes::Red-Blue Diverging",length(breaks)-1))
  print(my_pal)
  China_veg_map <- ggplot(China_veg)+
    geom_sf(color='grey50', aes(fill="white"), size=2, alpha=1, show.legend = FALSE)+
    geom_sf(data = china_area, color='black', fill=NA, size=2)+
    scale_fill_manual(values="white", guide = "none")+
    theme(legend.position = 'right')+
    new_scale_colour()+
    new_scale_fill()
  
  China_veg_map <- China_veg_map +
    geom_sf(data = gdf_json, color=NA, aes_string(fill=fill_var), alpha = 0.8, show.legend = TRUE) +
    scale_fill_stepsn(
      colors = my_pal,
      breaks = breaks[2:(length(breaks)-1)],
      labels = breaks[2:(length(breaks)-1)],
      limits = c(breaks[1], breaks[length(breaks)]),
      na.value = "grey80"
    ) +
    labs(x=NULL, y=NULL, color=NULL, title = title)+
    theme_bw()+
    coord_sf(crs = st_crs(3857), ylim = c(2000000,7000000))+
    theme(axis.text = element_blank(),
          axis.title.x = element_text(size=14),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank(),
          plot.title = element_text(hjust = 0.5, size=14),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.key.width = unit(50, "pt"),
          legend.key.height = unit(10, "pt")
    )
  
  plot_final <- ggdraw(China_veg_map) +
    draw_plot(nhmap, x = 0.7, y = 0.15, hjust  = 0 ,vjust = 0, width = 0.16, height = 0.3)
  return(plot_final)
}


hist(gdf_json$rh98_diff)
hist(gdf_json$fhd_diff)
hist(gdf_json$pai_diff)
hist(gdf_json$cov_diff)

p31 <- draw_map('rh98_diff','RH98 (m)',c(-4,-2,-1,1,2,4)) #c(-Inf, -1,-0.5, 0.5, 1, Inf)
p32 <- draw_map('fhd_diff','FHD',c(-0.4, -0.2,-0.1, 0.1, 0.2, 0.4)) # c(-Inf, -0.2,-0.1, 0.1, 0.2, Inf)
p33 <- draw_map('pai_diff','PAI',c(-0.8, -0.4,-0.2, 0.2, 0.4, 0.8)) #c(-Inf, -0.4,-0.2, 0.2, 0.4, Inf)
p34 <- draw_map('cov_diff','Cover',c(-0.2, -0.1,-0.05, 0.05, 0.1, 0.2))

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

ggsave(p35,filename='./figs/fig2_map.jpg',width = 7,height = 5,dpi=300)
ggsave(p35,filename='./figs/fig2_map.pdf',width = 7,height = 5,dpi=300)
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

ggsave(plot3,filename='./figs/fig3-1_unit.pdf',width = 10,height = 8,dpi=300)
ggsave(plot3,filename='./figs/fig3-1_unit.jpg',width = 10,height = 8,dpi=300)

#### 4 lme ####
#PCA
library(tidyverse)
library(broom)
library(vegan)
library(psych)
library(FactoMineR)
library(factoextra)
library(corrplot)

# get grid level soil and bioclimate variables
vars1 <- c("grid_id","TID", "TName","x","y","min_sample_num",
           "rh98_diff","fhd_normal_diff","pai_diff","cover_diff")
gdf_result_1_6_simple <- gdf_result_1_6[,vars1]

colnames(gdf_result_1_6_simple)

# env vars
# footprint mean
colnames(gdf_result)

excluded_bases <- c("rh98", "pai", "fhd_normal", "cover", "sample_size", 
                    "delta_time","digital_elevation_model","forest_age",
                    "b2","b3","b8","b9","b18","b19") 
all_nat_cols <- colnames(gdf_result)[grep("^nat_", colnames(gdf_result))]
base_names <- sub("nat_", "", all_nat_cols)
feature_bases <- base_names[!base_names %in% excluded_bases]

gdf_result_1_6_envvars <- gdf_result_1_6 %>% 
    select(grid_id, TID, x, y)

for (base in feature_bases) {
  nat_col_name <- paste0("nat_", base)
  plt_col_name <- paste0("plt_", base)

  diff_col_name <- paste0(base)

  if (nat_col_name %in% colnames(gdf_result_1_6) && plt_col_name %in% colnames(gdf_result_1_6)) {

     mean_values <- (gdf_result_1_6[[nat_col_name]] + gdf_result_1_6[[plt_col_name]])/2

    gdf_result_1_6_envvars[[diff_col_name]] <- mean_values
  }
}

colnames(gdf_result_1_6_envvars)
gdf_result_1_6_envvars$nitrogen_all_mean <- rowMeans(gdf_result_1_6_envvars[,vars_nitrogen])
gdf_result_1_6_envvars$phh2o_all_mean <- rowMeans(gdf_result_1_6_envvars[,vars_phh2o])
gdf_result_1_6_envvars$clay_all_mean <- rowMeans(gdf_result_1_6_envvars[,vars_clay])
gdf_result_1_6_envvars$soc_all_mean <- rowMeans(gdf_result_1_6_envvars[,vars_soc])

gdf_result_envs.scale <- scale(gdf_result_1_6_envvars[,vars_bioclim_soil])
gdf_result_envs.cor <- cor(gdf_result_envs.scale)


#correlation of variables
cor.plot(gdf_result_envs.cor)
colnames(gdf_result_envs.scale) <- c("bio01","bio04","bio05","bio06","bio07","bio10",
                                     "bio11","bio12","bio13","bio14","bio15","bio16","bio17",
                                     "soil nitrogen","soil pH","soil clay","soil SOC")
pca <- PCA(gdf_result_envs.scale)

#scree plot
png(
  filename = "./figs/pca_scree_plot.png",
  width = 7,
  height = 4,
  units = "in",
  res = 300
)

fviz_eig(pca, addlabels = TRUE, title = NULL)+ theme(plot.title = element_blank())

dev.off()

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

pca3 <- pca$ind$cos2[,c(1,2,3,4,5)]
gdf_pca <- cbind(gdf_result_1_6_simple,pca3) # the grids were ranked in order of grids_id
gdf_pca[,c(7:13)] <- scale(gdf_pca[,c(7:13)])
nrow(gdf_pca)

colMeans(gdf_pca[,c(7:13)])
apply(gdf_pca[,c(7:13)],2,sd)


#mixed linear model
library(nlme)
library(MuMIn)

#
plot_scatter <- function(df, x, y) {
  ggplot(df, aes(x = !!sym(x), y = !!sym(y))) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    stat_poly_eq(
      aes(label = paste(after_stat(eq.label), 
                        after_stat(rr.label), 
                        after_stat(p.value.label), 
                        sep = "~~")),
      formula = y ~ x,
      parse = TRUE,
      size = 2
    )
}

gdf_pca_for_model <- gdf_pca %>% 
  filter(min_sample_num > 0)

plot_scatter(gdf_pca_for_model,'Dim.1','rh98_diff')
plot_scatter(gdf_pca_for_model,'Dim.2','rh98_diff')

plot_scatter(gdf_pca_for_model,'Dim.1','fhd_normal_diff')
plot_scatter(gdf_pca_for_model,'Dim.2','fhd_normal_diff')

plot_scatter(gdf_pca_for_model,'Dim.1','pai_diff')
plot_scatter(gdf_pca_for_model,'Dim.2','pai_diff')

plot_scatter(gdf_pca_for_model,'Dim.1','cover_diff')
plot_scatter(gdf_pca_for_model,'Dim.2','cover_diff')

lme1_ir2 <- lme(rh98_diff~(Dim.1+Dim.2),data=gdf_pca_for_model,random=~1|TID)
lme2_ir2 <- lme(fhd_normal_diff~(Dim.1+Dim.2),data=gdf_pca_for_model,random=~1|TID)
lme3_ir2 <- lme(pai_diff~(Dim.1+Dim.2),data=gdf_pca_for_model,random=~1|TID)
lme4_ir2 <- lme(cover_diff~(Dim.1+Dim.2),data=gdf_pca_for_model,random=~1|TID)

summary(lme1_ir2)
summary(lme2_ir2)
summary(lme3_ir2)
summary(lme4_ir2)

lme1_spatial4_ir2 <- lme(rh98_diff~(Dim.1+Dim.2),data=gdf_pca_for_model,random=~1|TID,correlation = corRatio(form = ~x+y))
lme2_spatial4_ir2 <- lme(fhd_normal_diff~(Dim.1+Dim.2),data=gdf_pca_for_model,random=~1|TID,correlation = corRatio(form = ~x+y))
lme3_spatial4_ir2 <- lme(pai_diff~(Dim.1+Dim.2),data=gdf_pca_for_model,random=~1|TID,correlation = corRatio(form = ~x+y))
lme4_spatial4_ir2 <- lme(cover_diff~(Dim.1+Dim.2),data=gdf_pca_for_model,random=~1|TID,correlation = corRatio(form = ~x+y))

summary(lme1_spatial4_ir2)
summary(lme2_spatial4_ir2)
summary(lme3_spatial4_ir2)
summary(lme4_spatial4_ir2)

s5 <- summary(lme1_spatial4_ir2)
s6 <- summary(lme2_spatial4_ir2)
s7 <- summary(lme3_spatial4_ir2)
s8 <- summary(lme4_spatial4_ir2)

aov5 <- anova(lme1_spatial4_ir2)
aov6 <- anova(lme2_spatial4_ir2)
aov7 <- anova(lme3_spatial4_ir2)
aov8 <- anova(lme4_spatial4_ir2)

aov5$Values <- s5$coefficients$fixed
aov6$Values <- s6$coefficients$fixed
aov7$Values <- s7$coefficients$fixed
aov8$Values <- s8$coefficients$fixed

aov5
aov6
aov7
aov8

r.squaredGLMM(lme1_spatial4_ir2)
r.squaredGLMM(lme2_spatial4_ir2)
r.squaredGLMM(lme3_spatial4_ir2)
r.squaredGLMM(lme4_spatial4_ir2)

models <- list(
  lme1_spatial4_ir2,
  lme2_spatial4_ir2,
  lme3_spatial4_ir2,
  lme4_spatial4_ir2
)

model_names <- c("RH98", "FHD", "PAI", "Cover")

results_list <- list()

for(i in seq_along(models)) {
  mod <- models[[i]]
  mod_name <- model_names[i]
  
  s <- summary(mod)
  # a <- anova(mod)
  r2 <- r.squaredGLMM(mod)
  
  coef_df <- as.data.frame(s$tTable)
  coef_df$Parameter <- rownames(coef_df)
  coef_df <- coef_df %>%
    mutate(
      Model = mod_name,
      Marginal_R2 = sprintf("%.2f", r2[1]),
      Conditional_R2 = sprintf("%.2f", r2[2]),
      Value = sprintf("%.2f", Value),
      `p-value` = ifelse(`p-value` < 0.01, "<0.01", sprintf("%.2f", `p-value`))
    ) %>%
    select(Model, Marginal_R2, Conditional_R2, Parameter, DF, Value, `p-value`)
  
  results_list[[i]] <- coef_df
}

results_table <- bind_rows(results_list)
write.csv(results_table,'./gdf_new_factors_norep_xy3_agematch_unweighted_model_v2_footprint_envvar.csv')

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
  mutate(type = ifelse(type == 1, 'Tree plantations', type)) %>%
  mutate(type = ifelse(type == 2, 'Natural forests', type))
  
NFPF_map_df$type <- factor(NFPF_map_df$type)

China_nfpf_map <- ggplot(china_area)+
  geom_sf(data = china_area,color='black',fill=NA,size=2)+
  coord_sf(crs = st_crs(3857),ylim = c(2000000,7000000))+
  geom_raster(data = NFPF_map_df,aes(x = x,y = y,fill = type))+
  scale_fill_manual(values=c('Tree plantations'="#56B4E9",'Natural forests'="#009E73"))+
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

ggsave(China_nfpf_map_nh,filename='./figs/China_nfpf_map_nh.jpg',width = 7,height = 5,dpi=300)
ggsave(China_nfpf_map_nh,filename='./figs/China_nfpf_map_nh.pdf',width = 7,height = 5,dpi=300)

# sensitive test
min_samples <- seq(50, 500, by = 50)

sensitivity_results <- list()

for (n_min in min_samples) {
  print(n_min)
  df_filtered <- gdf_result %>% filter(min_sample_num >= n_min)
  
  if(nrow(df_filtered) == 0) next
  
  gls_tmp <- lapply(diff_vars, function(v) { #diff_vars
    gls_diff_stats(df_filtered, v, x="x", y="y")
  })
  
  df_tmp <- do.call(rbind, gls_tmp)
  df_tmp$min_sample <- n_min
  
  sensitivity_results[[as.character(n_min)]] <- df_tmp
}

df_sensitivity <- do.call(rbind, sensitivity_results)

df_plot <- df_sensitivity %>%
  pivot_longer(
    cols = c("mean", "p_value"),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    y_lab = ifelse(metric == "mean", "Mean Difference (MD)", "p-value"),
    variable_label = dplyr::recode(variable,
                            rh98_diff = "RH98",
                            fhd_normal_diff = "FHD",
                            pai_diff = "PAI",
                            cover_diff = "Cover")
  )

df_md <- df_plot %>% filter(metric == "mean")
df_p <- df_plot %>% filter(metric == "p_value")

p_md <- ggplot(df_md, aes(x = min_sample, y = value, color = variable_label)) +
  geom_line(alpha = 0.7, size = 1) +
  geom_point(alpha = 0.7, size = 2) +
  scale_x_continuous(breaks = c(50, 150, 250, 350, 450), limits = c(0, 550)) +
  labs(x = "Minimum sample size", y = "Mean Difference (MD)", color = "") +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "bottom"
  )

p_p <- ggplot(df_p, aes(x = min_sample, y = value, color = variable_label)) +
  geom_line(alpha = 0.7, size = 1) +
  geom_point(alpha = 0.7, size = 2) +
  scale_x_continuous(breaks = c(50, 150, 250, 350, 450), limits = c(0, 550)) +
  scale_y_continuous(breaks = seq(0,1e-12,3e-13),labels = scales::scientific_format(digits = 2))+
  labs(x = "Minimum sample size", y = "p-value", color = "") +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "bottom"
  )

plot_sensitive_test <- ggarrange(p_md,p_p, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
ggsave(plot_sensitive_test,filename='./figs/plot_sensitive_test.png',width = 7,height = 4,dpi=300)
ggsave(plot_sensitive_test,filename='./figs/plot_sensitive_test.pdf',width = 7,height = 4,dpi=300)

# moran i test
library(spdep)

moran_compare_plot <- function(gls_model, ols_model, df, x = "x", y = "y", k_range = 4:8, title = NULL) {
  
  resid_gls <- resid(gls_model, type = "normalized")
  resid_ols <- resid(ols_model)
  
  coords <- cbind(df[[x]], df[[y]])
  res <- data.frame()
  
  for (k in k_range) {
    nb <- knn2nb(knearneigh(coords, k = k))
    lw <- nb2listw(nb, style = "W")
    
    moran_gls <- moran.test(resid_gls, lw)$estimate[["Moran I statistic"]]
    moran_ols <- moran.test(resid_ols, lw)$estimate[["Moran I statistic"]]
    
    res <- rbind(res, data.frame(
      k = k,
      Moran_GLS = moran_gls,
      Moran_OLS = moran_ols
    ))
  }
  
  print(res)
  
  p <- ggplot(res, aes(x = k)) +
    geom_line(aes(y = Moran_GLS, color = "With spatial autocorrelation correction")) +
    geom_point(aes(y = Moran_GLS, color = "With spatial autocorrelation correction")) +
    geom_line(aes(y = Moran_OLS, color = "Without spatial autocorrelation correction")) +
    geom_point(aes(y = Moran_OLS, color = "Without spatial autocorrelation correction")) +
    labs(y = "Moran's I", x = "k", color = "Model", title = title) +
    # ylim(0,0.3)+
    scale_color_manual(values = c("With spatial autocorrelation correction" = "#77A2BB", "Without spatial autocorrelation correction" = "#E28394"))+
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_blank(),
          legend.position = c(0.95, 0.5),
          legend.justification = c("right", "center"),
          legend.background = element_blank())
  
  return(p)
}

## gls test
gls1 <- gls(rh98_diff ~ 1, correlation = corRatio(form = ~x+y),
            data = gdf_result, na.action = na.omit)
gls2 <- gls(fhd_normal_diff ~ 1, correlation = corRatio(form = ~x+y),
            data = gdf_result, na.action = na.omit)
gls3 <- gls(pai_diff ~ 1, correlation = corRatio(form = ~x+y),
            data = gdf_result, na.action = na.omit)
gls4 <- gls(cover_diff ~ 1, correlation = corRatio(form = ~x+y),
            data = gdf_result, na.action = na.omit)

#check
summary(gls1)
summary(gls2)
summary(gls3)
summary(gls4)


ols1 <- gls(rh98_diff ~ 1,
            data = gdf_result, na.action = na.omit)
ols2 <- gls(fhd_normal_diff ~ 1,
            data = gdf_result, na.action = na.omit)
ols3 <- gls(pai_diff ~ 1,
            data = gdf_result, na.action = na.omit)
ols4 <- gls(cover_diff ~ 1,
            data = gdf_result, na.action = na.omit)

moran_compare_plot_rh98_ols <- moran_compare_plot(gls1,ols1,gdf_result,title = 'RH98')
moran_compare_plot_fhd_ols <- moran_compare_plot(gls2,ols2,gdf_result,title = 'FHD')
moran_compare_plot_pai_ols <- moran_compare_plot(gls3,ols3,gdf_result,title = 'PAI')
moran_compare_plot_cover_ols <- moran_compare_plot(gls4,ols4,gdf_result,title = 'Cover')

plot_moran_compare_ols_all <- ggarrange(moran_compare_plot_rh98_ols,
                                        moran_compare_plot_fhd_ols,
                                        moran_compare_plot_pai_ols,
                                        moran_compare_plot_cover_ols,
                                    ncol=2,nrow=2,
                                    labels = c('(a)','(b)','(c)','(d)'),label.x = 0.1,
                                    align = 'hv',
                                    font.label = list(size = 10, face = "bold"),
                                    common.legend = TRUE,
                                    legend = 'bottom')
ggsave(plot_moran_compare_ols_all,filename='./figs/plot_moran_compare_ols_all.jpg',width = 7,height = 5,dpi=300)
ggsave(plot_moran_compare_ols_all,filename='./figs/plot_moran_compare_ols_all.pdf',width = 7,height = 5,dpi=300)


## lme test
moran_compare_plot_rh98 <- moran_compare_plot(lme1_spatial4_ir2,lme1_ir2,gdf_pca,title = 'RH98')
moran_compare_plot_fhd <- moran_compare_plot(lme2_spatial4_ir2,lme2_ir2,gdf_pca,title = 'FHD')
moran_compare_plot_pai <- moran_compare_plot(lme3_spatial4_ir2,lme3_ir2,gdf_pca,title = 'PAI')
moran_compare_plot_cover <- moran_compare_plot(lme4_spatial4_ir2,lme4_ir2,gdf_pca,title = 'Cover')

plot_moran_compare_all <- ggarrange(moran_compare_plot_rh98,moran_compare_plot_fhd,moran_compare_plot_pai,moran_compare_plot_cover,
                                    ncol=2,nrow=2,
                                    labels = c('(a)','(b)','(c)','(d)'),label.x = 0.1,
                                    align = 'hv',
                                    font.label = list(size = 10, face = "bold"),
                                    common.legend = TRUE,
                                    legend = 'bottom')

ggsave(plot_moran_compare_all,filename='./figs/plot_moran_compare_all.jpg',width = 7,height = 5,dpi=300)
ggsave(plot_moran_compare_all,filename='./figs/plot_moran_compare_all.pdf',width = 7,height = 5,dpi=300)



#check for specific site
# fujian longyan
gdf_result_1_6 %>% 
  filter(grid_id == '260,57') %>% 
  select(rh98_diff)
