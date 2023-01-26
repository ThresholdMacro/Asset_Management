library(tidyverse)
library(cowplot)
library(gifski)
library(gganimate)
library(tweenr)
# tickers

# set current T10Y2YM and T10Y3MM spread

TBill10s <- -1.26
T2s10s <- -0.66


tickers <- c(#"T10Y3MM",
  #"T10Y2YM",
  "GS10",
  "GS2",
  "TB3MS",
  "USREC")

df <- tidyquant::tq_get(tickers,get="economic.data",from="1953-01-01")
# Then do a bit of wrangling:
  
df2 <- 
  df %>% 
  spread(symbol,price) %>% 
  mutate(rec12=lead(USREC,12),
         T10Y3MM=GS10-TB3MS,
         T10Y2YM=GS10-GS2) 

df_rec <- filter(df, symbol=="USREC") %>% 
  mutate(rec12=lead(price,12)) %>% 
  mutate(recind=ifelse(rec12==1,"Recession in 12 months","No Recession in 12 months"))

df3 <- df2 %>%
  select(date,T10Y3MM,T10Y2YM) %>%
  gather(symbol,price,-date) %>% 
  mutate(var=case_when(symbol=="T10Y3MM"~ "Spread 10-year minus 3-month Treasury",
                       symbol=="T10Y2YM"~ "Spread 10-year minus 2-year Treasury",
                       T ~ "Recession Indicator")) %>%
  left_join(select(df_rec, date,recind), by="date") %>% 
  # hard coded based on Treasury data from April 26, 2022
  # https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yield
  mutate(vlast = ifelse(symbol=="T10Y3MM",TBill10s, T2s10s))
# Make plots:
  
 ggplot(data=filter(df3,!is.na(recind)), aes(x=price, fill=recind,color=recind))+
  geom_density(alpha=0.5,color=NA)+
  facet_wrap(recind~var)+
  geom_rug(sides="b",alpha=0.25)+
  scale_fill_manual(name="Recession 12 months from now? ",values=c("#4575b4","#d73027"))+
  scale_color_manual(name="Recession 12 months from now? ",values=c("#4575b4","#d73027"))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  theme_minimal_vgrid()+
  geom_vline(aes(xintercept=vlast), linetype=2)+
  geom_text(data=. %>% filter(date==max(date)),color="black",hjust=1,
            aes(y=0.2,label=paste0("24-01-2023: ", vlast," "), x=vlast))+
  labs(x="Spread in percentage points",y="Estimated Density Function",
       title="U.S. Treasury Yield Curve and Recessions",
       caption="Source: U.S. Treasury, monthly averages, NBER recessions")+
  theme(legend.position="top",
        plot.caption=element_text(hjust=0),
        plot.title=element_text(face="bold"),
        strip.text=element_text(face="bold"))+
    tidyquant::theme_tq()
  