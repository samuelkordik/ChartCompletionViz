
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ChartCompletion

The goal of this project is to visualize chart completion rates over
time and drive understanding of what interventions led to improvements.
For demonstration purposes, all names have been replaced by fictional
names.

## Rationale

Documentation (charts) should be completed in a timely manner, ideally
within 24 hours. Delays in completion result in greater risk for
documentation errors, incomplete information provided for ongoing care,
delays in billing collection, and lost revenue. Staff typically work on
a four day rotation, so the key measures are the percent open more than
24 hours, more than four days, and more than eight days. The numerators
are the number of charts open for each period of time and the
denominator is the number of charts started within the past thirty days.

## Key Visualization

The key visualization charts these rates on a day-by-day basis using
rolling 30-day windows. The graph also highlights the most reecent rate
numbers in individual callout gems on the right. This chart is useful
when correlated with knowledge of external events to understand what
interventions drive significant improvements.

<img src="man/figures/README-performance_over_time-1.png" width="100%" />
Looking at individual employees highlights which individuals have
substantially worse performance as well as to understand individual
performance over time. This can be correlated with external events, such
as training periods, times of external stress such as illnesses or
divorces, and individual work loads. An individual with a season of poor
performance correlated with an external stressor that self-resolved is
typically not someone that needs additional help, while an individual
with significant poor performance likely needs help to catch up and
coaching to avoid getting behind in the future.
<img src="man/figures/README-per_provider_performance-1.png" width="100%" />

Using this approach can identify the worst performers, and these can be
removed to get a better sense of the overall performance.

<img src="man/figures/README-time_performance_filtered-1.png" width="100%" />

\`\`\`
