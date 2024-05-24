library(tidyverse)
library(cowplot)

#读入数据
data  = readxl::read_xlsx("data.xlsx")

#先定义好各列因子的顺序，防止绘图时按照默认首字母排序排乱了
data$`Bile Acides` <- factor(data$`Bile Acides`, levels = data$`Bile Acides` %>% unique() %>% rev())
data$TFactor <- factor(data$TFactor, levels = data$TFactor %>% unique())
data$Type <- factor(data$Type, levels = data$Type %>% unique())

data[which(data$Significant == "yes"),"sig"] <- '*'


# 绘图

p <- ggplot(data, aes(TFactor, `Bile Acides`)) +
  geom_tile(aes(fill = `Fold Change`), color = "black") +
  scale_fill_gradientn(colours = c("#6FCFCF", "white", "#DD6048"),
                       values = c(0, 0.15, 1),
                       name="Fold\nChange", 
                       breaks = c(0, 1, 2),
                       labels = c(expression(10^0), expression(10^1), expression(10^2))) +
  scale_y_discrete(breaks = levels(data$`Bile Acides`), 
                   limits = c(levels(data$`Bile Acides`)[1:33], "skip", 
                              levels(data$`Bile Acides`)[34:40])) +
  geom_rect(aes(xmin = 0.5, xmax = 8.5, ymin = 0.5, ymax = 41.5), 
            fill = NA, color = "black", linewidth = 0.6) +
  scale_x_discrete(expand = c(0, 0)) +
  geom_text(aes(label = sig), 
            color = ifelse(data$`Fold Change` < 0, "#145444", "#7C2115"), 
            vjust = 0.75,
            size = 4) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(angle = 45, size = 9, color = "black", hjust = 1),
        axis.text.y = element_text(size = 9, color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(linewidth = 0.6),
        plot.margin = margin(0.5, 0.5, 0.5, 0, 'cm')) +
  guides(fill = guide_colorbar(position = "top",
                               barwidth = 8,
                               barheight = 0.8,
                               direction = "horizontal",
                               title.vjust = 1,
                               title.theme = element_text(hjust = 0),
                               ticks.colour = "black",
                               ticks.outside = TRUE,
                               frame.colour = "black"))

# 添加自定义文本
anno <- ggdraw() + 
  draw_label("Conventional\nBile Acides", y = 0.8, 
                              angle = 90, size = 9) +
  draw_label("BBAAs", y = 0.43, 
             angle = 90, size = 9)

plot_grid(anno, p, ncol = 2, rel_widths = c(0.1, 0.9))


ggsave("heatmap-significant.png", width = 3.5, height = 7, dpi = 600)
ggsave("heatmap-significant.pdf", width = 3.5, height = 7)
  


        