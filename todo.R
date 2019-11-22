
## Direct: efficiency
There is no need to analyze the effectiveness of direct funding in terms of go-decisions seeing that the core idea of direct funding is to move no-/go-decisions into the hands of the benefactor.
While in reality, the benefactor may strategically choose not to pursue all opportunities we will for the sake of calculation assume that all opportunities are pursued.
Direct funding can thus be thought of as having the same effect (in terms of go-rate) as a go-rate of approximately 100%.
However, to compute the efficiency of direct funding, so that we can compare it to the efficiency of indirect funding, we must identify the cost of direct funding.

<div class="alert alert-danger">
TODO: Actually we need to analyze effectiveness. Think: attrition rate but for time. Go-rate + probability = how many outputs per entry. But go-rate + probability + time = time until output per entry.
</div>

We begin by applying the sampled inefficiencies to our original dataset, and then converting the resulting dataset into the same year-based format used for the analysis of indirect funding.
We assume that public sector inefficiencies have detrimental effects on development costs and times but not probabilities of success.
In other words, we assume that the likelihood of succeeding in turning a hypothetical NCE into a market ready antibiotic is the same for both the public and the private sector (costs and development times aside).
In even other words, we assume that the public sector eventually get to the point that the private do.
But the time it takes the public sector to get there may be longer and during that time they may accumulate a higher cost.

We model this inefficiency as an "efficiency factor" that increases costs and times, while reducing probabilities.
If the efficiency factor, e.g. is 75% then a cost of 10 million will be transformed into a cost of 17.5 million, because 10 \* (1 + 0.75) = 17.5.

```{r, add-inefficiency}
inefficient <- intervened %>%
  mutate(cost = cost * (1 + inefficiency),
         time = time * (1 + inefficiency))
inefficient_years <- to_years(inefficient)
inefficient_years <- inefficient_years %>%
  group_by(src, subject, intervention) %>% # TODO: Group by inefficiency?
  arrange(phase, phase_year) %>%
  mutate(time_to = cumsum(time) - time)
```

We then compute the ENPV of directly funding each individual project:

```{r, compute-inefficiency}
inefficient_years <- inefficient_years %>%
  arrange(time_to) %>%
  group_by(src, subject, inefficiency) %>%
  mutate(
         tot_cost = sum(cost),
         tot_prob = prod(prob),
         prob_to = cumprod(prob) / prob, # TODO: Is this correct?
         enpv_dir = cumsum((-cost / ((1 + discount_rate_publ) ^ time_to)) * prob_to),
         ) %>%
  select(-prob_to)
```

and extract the ENPVs of taking each project from pre-clinical to market.

```{r, inefficient-finals}
inefficient_finals <- inefficient_years %>%
  arrange(time_to) %>%
  group_by(src, subject, inefficiency, tot_prob, tot_cost) %>%
  summarise(enpv_dir = tail(enpv_dir, n=1))
```

Finally we can plot the expected cost (in terms of ENPV) per pre-clinical entry of directly funding each project without any financial returns.
When exploring direct funding we're not plotting cost against the go-rate of a prize size but instead aginst different public sector inefficiencies.
When plotting direct ENPV we present it as a positive number that express a cost as opposed to a negative number that express a cashflow.

```{r, inefficiency-vs-direct-enpv-per-entry, echo=FALSE}
inefficient_finals %>%
  ggplot(aes(inefficiency*100, -enpv_dir, color=src)) +
  geom_point(shape=1) +
  geom_smooth(method='lm', se=FALSE, color='black') +
  facet_wrap(. ~ src) +
  xlab('Public inefficiency (%)') +
  ylab('Direct ENPV per entry\n(million usd)') +
  scale_y_continuous(label = unit_format(scale = 1e-6, unit = '')) +
  theme(legend.position='none')
```

Just as with the cost of indirect funding, we can also convert the cost per entry to a cost per exit.

```{r, inefficiency-vs-direct-enpv-per-exit, echo=FALSE}
inefficient_finals %>%
  ggplot(aes(inefficiency*100, -enpv_dir/tot_prob, color=src)) +
  geom_point(shape=1) +
  geom_smooth(method='lm', se=FALSE, color='black') +
  facet_wrap(. ~ src) +
  xlab('Public inefficiency (%)') +
  ylab('Direct ENPV per output\n(million usd)') +
  scale_y_continuous(label = unit_format(scale = 1e-6, unit = '')) +
  theme(legend.position='none')
```


## Comparison
To compare our indirect intervention (non-dilutive prizes) with direct funding, we must compare the efficiency of both approaches.
Since the private sector does not always choose to pursuit all projects, we must be careful with comparing the efficiency of directly funding all projects with that of indirectly funding only those that reach a go-decision.
Since a systematically different set of projects reach private go-decisions under a given prize size, the average cost per output is not really representative of the average cost per output of funding all projects directly (since the private sector pressumably have dropped e.g. some of the most expensive and time-consuming projects).

When comparing the cost per exit of indirect funding with direct funding, we will therefore only compute the cost of directly fund those particular projects that reached a go-decision under the given indirect funding scheme.
It is important to remeber however that even though we are comparing the cost of pursuing the same projects there are still at least two important uncaptured differences between the approaches:
(1) Direct funding would have a different cost (which is reported in the direct-specific analysis section above) and hence go-rate if we chose to pursuit all projects.
(2) Public inefficiencies does not only alert the cost of direct funding but also the amount of time it takes for new antibiotics to reach market.

With these two caveats in mind, we begin by combining the direct dataset and the indirect dataset, and compute the difference in cost for every given go-ratio/inefficiency pair.

<div class="alert alert-danger">
TODO: Actually, not filtering doesn't seem to change anything in the output plot so maybe this isn't logically necessary since we're not looking at go-decisions apart from the go-rate. This needs to be better understood and explained.
</div>

```{r, combine-inefficient-and-efficient}
indirect <- finals %>% ungroup %>%
  select(src, subject, intervention, tot_prob, enpv_ind, go_pred_prize, enpv_prv_aft) %>%
  rename(prob_ind = tot_prob)

direct <- inefficient_finals %>% ungroup %>%
  select(src, subject, tot_prob, enpv_dir, inefficiency) %>%
  rename(prob_dir = tot_prob)

# TODO: Compute efficiencies before merging to avoid renaming
both <- inner_join(indirect, direct, by=c('src', 'subject')) %>%
  mutate(enpv_ind_per_exit = enpv_ind/prob_ind,
         enpv_dir_per_exit = enpv_dir/prob_dir,
         diff = (-enpv_dir_per_exit) - (-enpv_ind_per_exit)) %>%
  filter(go_pred_prize < 0.99)
```


### Cost per output
Then filter out all projects that did not reach a go-decision under the given prize size, and build a linear model that predicts the cost difference between indirect and direct funding from public inefficiency and go-ratio.

```{r, predict-cost-difference}
both_go <- both %>% filter(enpv_prv_aft >= 0)
mod_diff <- glm(diff ~ go_pred_prize * inefficiency * src * intervention, data=both_go)
both_go$diff_pred <- predict(mod_diff, type='response')
```

In the plot below, green indicates that indirect funding (i.e. stimulating private developers) on average is cheaper, white that they are equivalent, and red that direct funding (publicly funding development) on average is cheaper.

```{r, cost-difference-levelplot, echo=FALSE}
both_go %>%
  ggplot(aes(
    round(go_pred_prize*100/5)*5,
    round(inefficiency*100/40)*40,
    fill=diff_pred / 10^9)) +
  scale_fill_gradient2(high='darkgreen', mid='white', low='red', midpoint=0) +
  geom_raster(interpolate=FALSE) +
  facet_grid(rows=vars(src), cols=vars(intervention)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  xlab('Prize size yielding probability of go-decision (%)') +
  ylab('Public inefficiency (%)') +
  labs(fill = 'Difference in favor\nof indirect funding\n(billion usd)') +
  theme(legend.position='bottom')
```

These levelplots give an indication of the overall relationship between direct and indirect funding for various levels of go-ratio yielding prize sizes and public inefficiencies.
However, it contains no obvious information on how much cheaper or more expensive a given configuration is.

To allow determining the cost differences under various configurations we can simply plot the two previous efficiency plot side-by-side with aligned y-axes.
It is important to note however that in this plot we cannot compute the cost of directly funding only the projects that receive a go-decision, since to identify which projects reach a go-decision we must pair the inefficiency with a prize size.
Consequently, in the rightmost facet of the plot below we are looking at the cost per output/exit when directly funding every project that enters pre-clinical.
As stated before, this can be thought of as a go-rate of 100%.

Whether the direct or the indirect strategy will yield more projects per time unit is an alltogether different question left unanswered.
To approach that we would have to consider the reduced time efficiency of the direct approach and compare to the reduced go-rate of the indirect approach.

```{r, cost-difference-comparison, echo=FALSE, warning=FALSE}
# TODO: This plot yields lots of errors that I've supressed!
for (curr in sources) {
  sub <- both_go %>%
    filter(src == curr) %>%
    filter(go_pred_prize < go_lim_up)

  p1 <- sub %>%
    ggplot(aes(go_pred_prize*100, -enpv_ind_per_exit, group=intervention, color=intervention)) +
    geom_point(shape=20, alpha=0.5) +
    geom_smooth(method='loess', se=FALSE) +
    geom_line(stat='smooth', method='loess', color='black', alpha=0.2, size=1) +
    facet_wrap(. ~ src) +
    xlab('Prob of go-decision (%) under prize size') +
    ylab('Indirect ENPV per exit\n(million usd)') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    scale_y_continuous(label = unit_format(scale = 1e-6, unit = ''),
                       breaks = scales::pretty_breaks(n = 8)) +
    theme(legend.position = 'left') +
    ggtitle('Indirect funding')

  yscale <- layer_scales(p1, 1, 1)$y$range$range

  p2 <- inefficient_finals %>%
    filter(src == curr) %>%
    ggplot(aes(inefficiency*100, -enpv_dir/tot_prob)) +
    geom_point(shape=20, color='darkgrey', alpha=0.6) +
    geom_line(stat='smooth', method='lm', color='red', size=1) +
    geom_line(stat='smooth', method='loess', color='black', size=1) +
    facet_wrap(. ~ src) +
    xlab('Public inefficiency (%)') +
    ylab('Direct ENPV per exit\n(million usd)') +
    scale_y_continuous(
      label = unit_format(scale = 1e-6, unit = ''),
      breaks = scales::pretty_breaks(n = 8),
      position='right',
      limits=c(min(yscale), max(yscale))) +
    ggtitle('Direct funding')

  print(ggarrange(p1, p2, ncol=2, nrow=1, widths=c(1, 0.5), common.legend=TRUE))
}
```


### Output probability per input dollar
TODO:

```{r, temp1, echo=FALSE, warning=FALSE}
# TODO: WARNINGS SUPRESSED!

# TODO: These two mutations should be added much earlier.
finals <- finals %>%
  mutate(exit_per_enpv_ind =
         ifelse(enpv_prv_aft >= 0,
                tot_prob / (-enpv_ind),
                0))

inefficient_finals <- inefficient_finals %>%
  mutate(exit_per_enpv_dir = tot_prob / (-enpv_dir))

x <- finals %>%
  filter(exit_per_enpv_ind == Inf)
print(head(data.frame(x)))

finals %>%
 #filter(go_pred_prize < go_lim_up) %>%
 filter(src == curr) %>%
 #ggplot(aes(go_pred_prize*100, exit_per_enpv_ind, color = intervention)) +
 ggplot(aes(prizes, exit_per_enpv_ind, color = intervention)) +
 geom_point(shape=1) +
 geom_smooth(se=FALSE) +
 facet_wrap(. ~ src * intervention) +
 xlab('Prob of go-decision (%) under prize size') +
 ylab('Probability of exit per indirect ENPV') +
 scale_y_continuous(trans='log10',
                    breaks = c(1 %o% 10^(0:-12)),
                    minor_breaks = c(1:9 %o% 10^(0:-12))
                    ) +
 scale_x_continuous(trans='log10',
                    breaks = c(1 %o% 10^(0:4)) * 10^6,
                    minor_breaks = c(1:9 %o% 10^(0:4)) * 10^6,
                    label = unit_format(scale = 1e-6, unit = '')
                    ) +
 theme(legend.position = 'left',
       axis.text.x = element_text(angle = 90)) +
 ggtitle('Indirect funding')

finals %>%
 #filter(go_pred_prize < go_lim_up) %>%
 filter(src == curr) %>%
 #ggplot(aes(go_pred_prize*100, exit_per_enpv_ind, color = intervention)) +
 ggplot(aes(prizes, exit_per_enpv_ind, color = intervention)) +
 geom_point(shape=1) +
 geom_smooth(se=FALSE) +
 facet_wrap(. ~ src) +
 xlab('Prob of go-decision (%) under prize size') +
 ylab('Probability of exit per indirect ENPV') +
 scale_y_continuous(trans='log10',
                    breaks = c(1 %o% 10^(0:-12)),
                    minor_breaks = c(1:9 %o% 10^(0:-12))
                    ) +
 scale_x_continuous(trans='log10',
                    breaks = c(1 %o% 10^(0:4)) * 10^6,
                    minor_breaks = c(1:9 %o% 10^(0:4)) * 10^6,
                    label = unit_format(scale = 1e-6, unit = '')
                    ) +
 theme(legend.position = 'left') +
 ggtitle('Indirect funding')

finals %>%
 filter(go_pred_prize < go_lim_up) %>%
 filter(src == curr) %>%
 mutate(exit_per_enpv_ind2 = go_pred_prize * tot_prob / (-enpv_ind)) %>%
 #ggplot(aes(go_pred_prize*100, exit_per_enpv_ind, color = intervention)) +
 ggplot(aes(prizes, exit_per_enpv_ind2, color = intervention)) +
 geom_point(shape=1) +
 geom_smooth(se=FALSE) +
 facet_wrap(. ~ src) +
 ylab('Probability of exit per indirect ENPV (filtered?)') +
 scale_y_continuous(trans='log10',
                    breaks = c(1 %o% 10^(0:-12)),
                    minor_breaks = c(1:9 %o% 10^(0:-12))
                    ) +
 scale_x_continuous(trans='log10',
                    breaks = c(1 %o% 10^(0:4)) * 10^6,
                    minor_breaks = c(1:9 %o% 10^(0:4)) * 10^6,
                    label = unit_format(scale = 1e-6, unit = '')
                    ) +
 theme(legend.position = 'left') +
 ggtitle('Indirect funding')

finals %>%
 #filter(go_pred_prize < go_lim_up) %>%
 filter(src == curr) %>%
 mutate(exit_per_enpv_ind2 = go_pred_prize * tot_prob / (-enpv_ind)) %>%
 #ggplot(aes(go_pred_prize*100, exit_per_enpv_ind, color = intervention)) +
 ggplot(aes(prizes, exit_per_enpv_ind2, color = intervention)) +
 geom_point(shape=1) +
 geom_smooth(se=FALSE) +
 facet_wrap(. ~ src * intervention) +
 ylab('Probability of exit per indirect ENPV') +
 scale_y_continuous(trans='log10',
                    breaks = c(1 %o% 10^(0:-12)),
                    minor_breaks = c(1:9 %o% 10^(0:-12))
                    ) +
 scale_x_continuous(trans='log10',
                    breaks = c(1 %o% 10^(0:4)) * 10^6,
                    minor_breaks = c(1:9 %o% 10^(0:4)) * 10^6,
                    label = unit_format(scale = 1e-6, unit = '')
                    ) +
 theme(legend.position = 'left') +
 ggtitle('Indirect funding')
```

```{r, temp2, echo=FALSE}
for (curr in sources) {
  p1 <- finals %>%
    filter(src == curr) %>%
    filter(go_pred_prize < go_lim_up) %>%
    ggplot(aes(go_pred_prize*100, exit_per_enpv_ind, color = intervention)) +
    geom_smooth(method='loess', se=FALSE) +
    facet_wrap(. ~ src) +
    xlab('Prob of go-decision (%) under prize size') +
    ylab('Indirect ENPV per exit') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
    theme(legend.position = 'left') +
    ggtitle('Indirect funding')

  yscale <- layer_scales(p1, 1, 1)$y$range$range

  p2 <- inefficient_finals %>%
    filter(src == curr) %>%
    ggplot(aes(inefficiency*100, exit_per_enpv_dir, color=src)) +
    #geom_point(shape=1) +
    geom_smooth(method='lm', se=FALSE, color='black') +
    facet_wrap(. ~ src) +
    xlab('Public inefficiency (%)') +
    ylab('Direct ENPV per exit') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 8),
      position='right',
      limits=c(min(yscale), max(yscale))) +
    ggtitle('Direct funding')

  print(ggarrange(p1, p2, ncol=2, nrow=1, widths=c(1, 0.5), common.legend=TRUE))
}
```


# Conclusion
What's the qualitative value-add of stimulating private developers and entrepreneurs that isn't captured in this analysis?


