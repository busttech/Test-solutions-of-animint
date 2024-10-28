library(animint2)
WorldBank1975 <- subset(WorldBank, year==1975)
WorldBankBefore1975 <- subset(WorldBank, 1970 <= year & year <= 1975)
income.colors <- 
  c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33")
WorldBank$income = factor(WorldBank$income,levels = c("High income: nonOECD","High income: OECD","Upper middle income","Lower middle income","Low income","Not classified"))
names(income.colors) <- levels(WorldBank$income)
scatter <- ggplot()+
  scale_color_manual(values=income.colors)+
  geom_point(
    mapping=aes(x=life.expectancy, y=fertility.rate, color=income),
    data=WorldBank1975)
two.layers <- scatter+
  geom_path(aes(
    x=life.expectancy, 
    y=fertility.rate, 
    color=income,
    group=country),
    data=WorldBankBefore1975)
viz.two.layers <- animint(two.layers,out.dir="easy-test-solution1")
viz.two.layers
