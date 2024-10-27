library(animint2)
WorldBank1975 <- subset(WorldBank, year==1975)
WorldBankBefore1975 <- subset(WorldBank, 1970 <= year & year <= 1975)
income.colors <- 
  c("#422680", "#341671", "#280659", "#660f56", "#ae2d68", "#f54952")
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
(viz.two.layers <- animint(two.layers,out.dir="easy-test-solution1"))
