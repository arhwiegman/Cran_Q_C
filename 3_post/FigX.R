library(tidyverse)

# Your data as a dataframe
df <- read.table(header = TRUE, text = "
embFID EMBAY_NAME S1_active S1_NO3_as_TN Emb_L1_Load_as_S1_Fert Emb_Area_Draining_S1_Farms Embay_Area_Draining_S2_Farm S1_Area_Draining_S2_Farm
28	PlymouthDuxbury	55	18	1.5	38	5.7	15
50	Wareham	100	12	16.2	70	26.3	38
2	BarnstableHarbor	100	22	0.4	5	3.1	58
27	PleasantBay	100	29	0.2	3	1.1	40
49	WaquoitBay	17	24	0.1	40	35.1	89
31	PopponessetBay	95	26	0.8	5	3.3	61
48	ThreeBays	78	34	1.3	18	10.6	60
3	BassRiver	99	30	0.4	4	2.3	53
15	HerringRiverHarwich	100	23	3.4	18	5.3	29
16	LewisBay	100	42	0.2	2	2.5	100
10	GreatPond	100	20	1.5	46	45.9	100
42	ScortonHarbor	100	21	2.2	18	5.3	29
7	CentervilleRiver	100	64	0.3	11	4.1	36
40	SandwichHarbor	100	18	0.3	1	0.6	89
26	PhinneysHarborEelPondBackRiver	100	32	1.0	8	6.1	72
25	ParkersRiver	100	25	1.1	64	44.8	70
44	StageHarbor	100	26	0.2	0	0.1	99
53	WildHarbor	100	38	0.6	13	10.2	77
41	SaquatucketHarbor	100	60	2.5	74	74.1	100
12	GreenPond	100	29	3.2	60	59.6	100
21	NamskaketCreek	100	43	0.8	0	0.1	35
5	BournesPond	100	41	1.8	58	57.8	100
45	SulfurSpringsBucksCreek	100	32	0.9	5	4.2	91
54	WychmereHarbor	100	55	2.2	15	15.1	100
")

# Pivot to long format
df_long <- df %>%
  pivot_longer(cols = -c(embFID, EMBAY_NAME),
               names_to = "Variable", values_to = "Value")

# Plot: Horizontal bars faceted by variable
ggplot(df_long, aes(x = Value, y = fct_reorder(EMBAY_NAME, Value))) +
  geom_col(fill = "steelblue") +
  facet_wrap(~Variable, scales = "free_x") +
  labs(x = "Value", y = "Embayments") +
  theme_bw(base_size = 10) +
  theme(strip.text = element_text(face = "bold"))


# Define label mapping
var_labels <- tibble(
  Variable = c(
    "S1_active", "S1_NO3_as_TN", "Emb_L1_Load_as_S1_Fert",
    "Emb_Area_Draining_S1_Farms", "Embay_Area_Draining_S2_Farm",
    "S1_Area_Draining_S2_Farm"
  ),
  Label = c(
    "1. % Active ",
    "2. % NO3/TN (S1)",
    "3. % Fertilizer",
    "4. % AC/AT (S1)",
    "5. % AC/AT (S2)",
    "6. % AC_S2/AC_S1"
  )
)

# Pivot and join labels
df_long <- df %>%
  pivot_longer(cols = -c(embFID, EMBAY_NAME),
               names_to = "Variable", values_to = "Value") %>%
  left_join(var_labels, by = "Variable")

# Plot with numbered facet labels
ggplot(df_long, aes(x = Value, y = fct_reorder(EMBAY_NAME, Value))) +
  geom_col(fill = "steelblue") +
  facet_wrap(~Label,nrow=1) +
  labs(
    x = "Value",
    y = "Embayments",
    title = "Farm and Nitrogen Metrics by Embayment",
    subtitle = "Facet numbers correspond to metrics explained in the legend"
  ) +
  theme_bw(base_size = 10) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9)
  )+
  theme_bw()

