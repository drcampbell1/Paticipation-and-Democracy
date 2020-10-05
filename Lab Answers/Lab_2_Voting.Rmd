---
title: "Lab 2: Electoral Turnout"
author: "Ross Campbell, PhD"
date: "`r format(Sys.Date(), '%d %B %Y')`"
output: html_document
---
<style>
#header {
  color: #F08080;
  font-family: Calibri;
  font-size: 20px;
}
#title {
  color: #800000;
  font-family : Calibri ;
 }
#body {
  color: #708090;
  font-family: Calibri;
  font-size: 11px;
}
</style>

```{r setup, include=FALSE}
ESS <- foreign::read.dta("/Users/rosscampbell/Desktop/Participation/data/ESS.dta", convert.factors=TRUE)
library(tidyverse)
```

## Introduction

This document provides the tables and graphs for the lab session on Electoral Turnout. In the introductory sessions to this module, we've recognised that although this is an important form of participation, studies suggest electoral turnout is in decline. Are these fears justified? Does the evidence fit the "facts"? These are the key questions we answered in the **second** lab. The exercises enabled us to give evidence-based answers that went directly to the data itself. They are provided here for you to consolidate your knowledge. Rather than produce colour graphs, I've made them black and white for readability.

### Turnout in Europe

We start by looking at turnout across our six democracies *combined*: (Austria, Switzerland, France, Germany, U.K. and the Netherlands). What are the overall levels of participation? They are provided below:

```{r, echo=FALSE}
ESS %>% filter(!is.na(vote1)) %>% count(vote1) %>%
  mutate("%" = round(n/sum(n) * 100, digits=1)) %>%
  knitr::kable("simple", caption = "Electoral Turnout in European Democracies",
  col.names = c('voted', 'N', '%'), align="lccc")
```

We develop this further, breaking the figures down for the six countries:

```{r, echo = FALSE}
ESS %>% group_by(cntry) %>% filter(!is.na(vote1)) %>%
  count(vote1) %>% mutate("%" = round(n/sum(n) * 100, digits=1)) %>%
  knitr::kable("pandoc", caption = "Electoral Turnout in European Democracies",
  col.names = c('Country', 'Voted', 'N', '%'), align="cccc")
```

We also presented this graphically to make it clear. Should you have difficulty seeing colour, I've included a black and white version along side it:

```{r, echo=FALSE, out.width = "50%"}
ESS %>% group_by(cntry) %>% filter(!is.na(vote1)) %>%
  count(vote1) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!vote1=="did not vote") %>%
  ggplot(aes(x=reorder(cntry, -prop), y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 1: Turnout by Country", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)
```

## Testing Theories with evidence

In the lab, we also looked at the evidence about our theories. Here are the outputs, beginning with **socio-economic** resources:

```{r, echo = FALSE}
ESS%>% group_by(educat) %>% filter(!is.na(vote1), !is.na(educat)) %>%
  count(vote1) %>% mutate("%" =n/sum(n)*100) %>%
  knitr::kable("simple", caption = "Education and Electoral Turnout",
  col.names = c('Education', 'Voted', 'N', '%'), align="ccccc", digits=1)
```
We also graphed these relationships to make the key points stand out:

```{r, echo = FALSE, out.width = '50%'}

ESS%>% group_by(educat) %>% filter(!is.na(vote1), !is.na(educat)) %>%
  count(vote1) %>% mutate(prop=prop.table(n*100)) %>% mutate("%" =prop*100) %>%
  ggplot(aes(x=educat, y=prop, fill=vote1))+
  labs(x="", y="", title="Figure 2a: Turnout by Education", caption="ESS 2016")+
  scale_fill_manual(values=c("#CCCCCC", "#333333"))+
  geom_bar(stat="identity", position="dodge")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")
```

We also looked at how this varied across countries and political systems:

```{r, echo = FALSE}

ESS%>% group_by(educat, cntry) %>% filter(!is.na(vote1), !is.na(educat)) %>%
  count(vote1) %>% mutate(prop=prop.table(n*100)) %>% mutate("%" =prop*100) %>%
  ggplot(aes(x=educat, y=prop, fill=vote1))+
  labs(x="", y="", title="Figure 3: Turnout by Education and Country", caption="ESS 2016")+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=c("#CCCCCC", "#333333"))+
  facet_grid(~cntry)+
  scale_x_discrete(labels = c("Basic" = "B", "University degree" = "D", "postgrad" = "PG"))+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")
```


### Does rationality matter?

```{r, echo = FALSE}
ESS %>% group_by(econsat) %>% filter(!is.na(vote1), !is.na(econsat)) %>%
  count(vote1) %>% mutate(prop= n /sum(n)*100) %>%
  knitr::kable("simple", caption = "Perceptions of the Economy and Electoral Turnout",
  col.names=c('Economic Perceptions', 'Voted', 'N', '%'), align="ccccc", digits=1)
```

Again, we graphed these relationships to make them clearer:

```{r, echo = FALSE}

ESS%>% group_by(econsat) %>% filter(!is.na(vote1), !is.na(econsat)) %>%
  count(vote1) %>% mutate(prop=prop.table(n*100)) %>% mutate("%" =prop*100) %>%
  ggplot(aes(x=econsat, y=prop, fill=vote1))+
  labs(x="", y="", title="Figure 4: Turnout by Perceptions of the Economy", caption="ESS 2016")+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=c("#CCCCCC", "#333333"))+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")+
  scale_x_discrete(labels=c("dissatisfied" = "dis", "neither dissatisfied nor satisfied" = "neither", "satisfied" = "sat"))
```

We looked at if this varied between countries. Please note: to make the x-axis of the following graphs clearer, I have made some abbreviations. Dissatisfied = **D**, neither dissatisfied nor satisfied = **N**, and satisfied = **S**.


```{r, echo = FALSE}

ESS%>% group_by(econsat, cntry) %>% filter(!is.na(vote1), !is.na(econsat)) %>%
  count(vote1) %>% mutate(prop=prop.table(n*100)) %>% mutate("%" =prop*100) %>%
  ggplot(aes(x=econsat, y=prop, fill=vote1))+
  labs(x="", y="", title="Figure 5: Turnout by Economic Satisfaction and Country", caption="ESS 2016")+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=c("#CCCCCC", "#333333"))+
  facet_grid(~cntry)+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")+
  scale_x_discrete(labels=c("dissatisfied" = "D", "neither dissatisfied nor satisfied" = "N", "satisfied" = "S"))
```

### Finally, we looked at if culture matters:

```{r, echo =FALSE}
ESS %>% group_by(ptrust) %>% filter(!is.na(vote1), !is.na(ptrust)) %>%
  count(vote1) %>% mutate(prop= n / sum(n)*100) %>%
  knitr::kable("pandoc", caption = "Trust in Politics and Electoral Turnout",
  col.names=c('Trust', 'Voted', 'N', '%'), align="ccccc", digits=1)
```

We graphed this to make it clearer:

```{r, echo = FALSE}

ESS%>% group_by(ptrust) %>% filter(!is.na(vote1), !is.na(ptrust)) %>%
  count(vote1) %>% mutate(prop=prop.table(n*100)) %>%
  ggplot(aes(x=ptrust, y=prop, fill=vote1))+
  labs(x="", y="", title="Figure 6: Turnout by Trust in Politics", caption="ESS 2016")+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=c("#CCCCCC", "#333333"))+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")+
  scale_x_discrete(labels=c("low" = "low", "medium" = "medium", "high" = "high"))
```


And we looked at the data across countries to track any variation. Again, to make the x-axis readable it was abbreviated to Low = **l**, medium = **m**, high = **h**.


```{r, echo = FALSE}
ESS%>% group_by(ptrust, cntry) %>% filter(!is.na(vote1), !is.na(ptrust)) %>%
  count(vote1) %>% mutate(prop=prop.table(n*100)) %>%
  ggplot(aes(x=ptrust, y=prop, fill=vote1))+
  labs(x="", y="", title="Figure 7: Turnout by Trust in Politics", caption="ESS 2016")+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=c("#CCCCCC", "#333333"))+
  facet_grid(~cntry)+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")+
  scale_x_discrete(labels=c("low" = "l", "medium" = "m", "high" = "h"))
```

### Conclusion
Clearly, all theories work. But do they work *equally* well in all countries? To answer this, you're going to have to review the data for yourself and make some decisions. This demonstrates that the exercise is as much *subjective* as it is *objective*. The data can provide a basis from which you can offer an interpretation. But the interpretation is your own.
