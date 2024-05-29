# multiobjective function example
x = c(1.5, 2.0, 5.0, 8.0, 9.0)
y = c(0.0, 6.0, 2.0, 1.5, 0.0)
y2 = c(0.0, 1.5, 4.0, 1.5, 0.0)
ggplot(dat, aes(x=x,y=y))+geom_smooth(se=F)+theme_bw()+annotate('point', x=2.7, y = 7.9, size=2, color='black') +labs(y='') +geom_smooth(aes(y=y2),color='red')+annotate('point', x=5, y=4, color='black',size=2)
