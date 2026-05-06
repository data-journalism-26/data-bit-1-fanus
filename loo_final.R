# Layers of Oppression
# Publication-ready editorial visualization with three aligned panels

library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(patchwork)
library(scales)
library(htmlwidgets)
library(htmltools)

# -----------------------------
# 1) Data setup
# -----------------------------
base_df <- data.frame(
  factor = c("Low income", "Traumatic event", "Female", "Rural", "Smoking"),
  share = c(81.0, 56.7, 54.3, 33.4, 4.0),
  anxiety = c(75.7, 79.4, 77.6, 78.7, 77.6),
  depression = c(76.1, 80.1, 78.4, 79.3, 79.1),
  stringsAsFactors = FALSE
)

# Order factors by population share for readability
base_df <- base_df %>%
  mutate(factor = factor(factor, levels = rev(c("Low income", "Traumatic event", "Female", "Rural", "Smoking")))) %>%
  mutate(
    share_label_x = ifelse(share < 8, share + 5, share - 2),
    share_label_hjust = ifelse(share < 8, 0, 1),
    anxiety_label_x = anxiety - 2,
    depression_label_x = depression - 2
  )

# Tidy long dataset for potential reuse/inspection
long_df <- base_df %>%
  pivot_longer(cols = c(share, anxiety, depression), names_to = "metric", values_to = "value")

# -----------------------------
# 2) Shared style
# -----------------------------
bg_col <- "#F7F3EE"
text_col <- "#172A3A"
muted_col <- "#5F6B73"
share_col <- "#C9A66B"
anx_col <- "#3C7A89"
dep_col <- "#B85C38"
bar_bg_col <- "#EEE7DE"

base_theme <- theme_minimal(base_family = "Arial") +
  theme(
    plot.background = element_rect(fill = bg_col, color = NA),
    panel.background = element_rect(fill = bg_col, color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = alpha(text_col, 0.08), linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14, color = text_col),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 16, face = "bold", color = text_col, margin = margin(b = 10), hjust = 0.5),
    plot.margin = margin(6, 6, 6, 6)
  )

# -----------------------------
# 3) Static combined editorial preview (optional reference object)
# -----------------------------
plot_share <- ggplot(base_df, aes(y = factor)) +
  geom_col(aes(x = 100), fill = bar_bg_col, width = 0.58) +
  geom_col(aes(x = share), fill = share_col, width = 0.58) +
  geom_text(aes(x = share_label_x, label = percent(share / 100, accuracy = 0.1), hjust = share_label_hjust),
            size = 3.8, color = text_col) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0))) +
  ggtitle("Who is exposed?") +
  base_theme

plot_anxiety <- ggplot(base_df, aes(y = factor)) +
  geom_col(aes(x = 100), fill = bar_bg_col, width = 0.58) +
  geom_col(aes(x = anxiety), fill = anx_col, width = 0.58) +
  geom_text(aes(x = anxiety_label_x, label = percent(anxiety / 100, accuracy = 0.1)),
            hjust = 1, size = 3.8, color = text_col) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0))) +
  ggtitle("Anxiety symptoms") +
  base_theme +
  theme(axis.text.y = element_blank())

plot_depression <- ggplot(base_df, aes(y = factor)) +
  geom_col(aes(x = 100), fill = bar_bg_col, width = 0.58) +
  geom_col(aes(x = depression), fill = dep_col, width = 0.58) +
  geom_text(aes(x = depression_label_x, label = percent(depression / 100, accuracy = 0.1)),
            hjust = 1, size = 3.8, color = text_col) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0))) +
  ggtitle("Depression symptoms") +
  base_theme +
  theme(axis.text.y = element_blank())

static_editorial <- plot_share + plot_anxiety + plot_depression +
  plot_layout(ncol = 3, widths = c(1.08, 1, 1))

# -----------------------------
# 4) Interactive plotly via ggplotly + subplot
# -----------------------------
hover_txt <- base_df %>%
  mutate(
    hover = paste0(
      "<b>", factor, "</b>",
      "<br>Share of sample: ", percent(share / 100, accuracy = 0.1),
      "<br>Anxiety symptoms within group: ", percent(anxiety / 100, accuracy = 0.1),
      "<br>Depression symptoms within group: ", percent(depression / 100, accuracy = 0.1),
      "<br><i>Groups may overlap; values are not causal effects.</i>"
    )
  )

p1 <- ggplot(base_df, aes(y = factor, text = hover_txt$hover)) +
  geom_col(aes(x = 100), fill = bar_bg_col, width = 0.58, alpha = 0.75) +
  geom_col(aes(x = share), fill = share_col, width = 0.58) +
  geom_text(aes(x = share_label_x, label = percent(share / 100, accuracy = 0.1), hjust = share_label_hjust),
            size = 3.8, color = text_col) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0))) +
  ggtitle("Who is exposed?") +
  base_theme

p2 <- ggplot(base_df, aes(y = factor, text = hover_txt$hover)) +
  geom_col(aes(x = 100), fill = bar_bg_col, width = 0.58, alpha = 0.75) +
  geom_col(aes(x = anxiety), fill = anx_col, width = 0.58) +
  geom_text(aes(x = anxiety_label_x, label = percent(anxiety / 100, accuracy = 0.1)),
            hjust = 1, size = 3.8, color = text_col) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0))) +
  ggtitle("Anxiety symptoms") +
  base_theme + theme(axis.text.y = element_blank())

p3 <- ggplot(base_df, aes(y = factor, text = hover_txt$hover)) +
  geom_col(aes(x = 100), fill = bar_bg_col, width = 0.58, alpha = 0.75) +
  geom_col(aes(x = depression), fill = dep_col, width = 0.58) +
  geom_text(aes(x = depression_label_x, label = percent(depression / 100, accuracy = 0.1)),
            hjust = 1, size = 3.8, color = text_col) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0))) +
  ggtitle("Depression symptoms") +
  base_theme + theme(axis.text.y = element_blank())

pp1 <- ggplotly(p1, tooltip = "text") %>% style(hovertemplate = "%{text}<extra></extra>")
pp2 <- ggplotly(p2, tooltip = "text") %>% style(hovertemplate = "%{text}<extra></extra>")
pp3 <- ggplotly(p3, tooltip = "text") %>% style(hovertemplate = "%{text}<extra></extra>")

interactive_plot <- subplot(pp1, pp2, pp3, nrows = 1, shareY = TRUE, titleX = FALSE, margin = 0.035) %>%
  layout(
    paper_bgcolor = bg_col,
    plot_bgcolor = bg_col,
    showlegend = FALSE,
    font = list(family = "Inter, Arial, sans-serif", size = 13, color = "#2B2B2B"),
    margin = list(l = 95, r = 20, t = 52, b = 12),
    title = list(text = ""),
    xaxis = list(range = c(0, 100), visible = FALSE, title = list(text = "")),
    xaxis2 = list(range = c(0, 100), visible = FALSE, title = list(text = "")),
    xaxis3 = list(range = c(0, 100), visible = FALSE, title = list(text = "")),
    yaxis = list(title = list(text = "")),
    annotations = list(
      list(text = "<b>Who is exposed?</b>", x = 0.145, y = 1.10, xref = "paper", yref = "paper",
           showarrow = FALSE, font = list(size = 14, color = text_col), xanchor = "center"),
      list(text = "<b>Anxiety symptoms</b>", x = 0.500, y = 1.10, xref = "paper", yref = "paper",
           showarrow = FALSE, font = list(size = 14, color = text_col), xanchor = "center"),
      list(text = "<b>Depression symptoms</b>", x = 0.855, y = 1.10, xref = "paper", yref = "paper",
           showarrow = FALSE, font = list(size = 14, color = text_col), xanchor = "center")
    )
  ) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE, responsive = TRUE)

# -----------------------------
# 5) Article wrapper and save as HTML
# -----------------------------
page <- browsable(tagList(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML(paste0(
      "html, body { margin:0; padding:0; background:", bg_col, "; color:", text_col, "; font-family:Georgia,'Times New Roman',serif; }",
      "body { border-top:2px solid rgba(23,42,58,.9); -webkit-font-smoothing:antialiased; text-rendering:optimizeLegibility; }",
      ".wrap { max-width:820px; margin:0 auto; padding:54px 34px 52px 34px; }",
      ".kicker { font-family:'Inter','Segoe UI',Arial,sans-serif; font-size:10.5px; text-transform:uppercase; letter-spacing:.13em; color:", muted_col, "; font-weight:700; margin-bottom:24px; }",
      "h1 { font-family:'Inter','Segoe UI',Arial,sans-serif; font-size:44px; line-height:1.02; margin:0 0 10px 0; font-weight:850; letter-spacing:-.045em; color:", text_col, "; max-width:760px; }",
      ".byline { font-family:'Inter','Segoe UI',Arial,sans-serif; color:", muted_col, "; font-size:12px; letter-spacing:.02em; margin-bottom:30px; }",
      "p {font-size:18px; line-height:1.75; margin:0 0 22px 0; color:", text_col, "; text-align: justify; hyphens: auto;}",
      "p:first-of-type::first-letter { float:left; font-family:Georgia,'Times New Roman',serif; font-size:58px; line-height:.86; padding-right:8px; color:", dep_col, "; }",
      ".chart { width:100%; margin:34px 0 18px 0; }",
      ".caption { font-family:'Inter','Segoe UI',Arial,sans-serif; color:", muted_col, "; font-size:11.5px; line-height:1.5; margin:0 0 30px 0; max-width:820px; }",
      ".sources { font-family:'Inter','Segoe UI',Arial,sans-serif; border-top:1px solid rgba(23,42,58,.18); margin-top:34px; padding-top:16px; color:", muted_col, "; font-size:11.5px; line-height:1.55; max-width:820px; }",
      ".sources strong { color:", text_col, "; }",
      "@media (max-width: 800px) { .wrap { padding:36px 22px 42px 22px; } h1 { font-size:34px; } p { font-size:16px; line-height:1.68; } p:first-of-type::first-letter { font-size:46px; } }"
    )))
  ),
  
  div(class = "wrap",
      div(class = "kicker", "Afghanistan · Neyazi et al. (2024) · n = 2,698 · DASS-21 survey"),
      tags$h1("Layers of Oppression – Mental Health in Afghanistan"),
      div(class = "byline", "Fanus Ghorjani · 06 May 2026"),
      
      tags$p("Oppression does not operate on a single level. It accumulates. Over time, it settles into different layers: material, social and psychological. In Afghanistan, decades of conflict, foreign intervention and economic instability have produced visible and less visible forms of control. These layers do not remain external. They shape how people live, relate and experience themselves. They shape the psyche."),
      
      tags$p("A recent study from Afghanistan shows that 72.05% of participants report symptoms of depression, 71.94% anxiety, and 66.49% high levels of stress. These numbers are not simply indicators of a health crisis. They show how deeply psychological distress has become embedded in everyday life. The chart breaks these results down into social layers, including gender, income, education and place of living."),
      
      tags$p("At first glance, one pattern dominates: levels of distress are high across all groups. Anxiety and depression rates remain around or above 70%, regardless of category. Mental distress is not confined to specific populations. It is widespread and shared."),
      
      div(class = "chart", interactive_plot),
      div(class = "caption", HTML("<strong>Figure:</strong> Percentages show group share and symptom prevalence within each group. Groups may overlap; values are descriptive and not causal.")),
      
      tags$p("The study shows differences between gender and class, but they do not contradict the overall pattern; they layer and intensify it. Where structural pressure is greater, distress becomes more severe. This is where the metaphor of layers becomes critical."),
      
      tags$p("The chart shows how multiple forms of pressure accumulate: economic hardship, restricted rights, insecurity and uncertainty do not act in isolation, but build on top of one another. Psychological distress emerges not from a single cause, but from the weight of these combined conditions."),
      
      tags$p("Frantz Fanon’s analysis of colonial violence offers a way to understand this accumulation. His point is not only that oppression causes suffering. It is that it reorganizes the psyche. The experience of being controlled and exposed to unpredictable force produces internal tension, anger and a constant anticipation of threat."),
      
      tags$p("In this sense violence is not only external, but becomes internalized, layered into how people think, feel and respond to the world. This study does not describe a collection of individual cases, but points to a structured condition."),
      
      tags$p("Mental health in Afghanistan is not only a medical issue. It reflects a collective condition formed through the accumulation of these layers. The chart makes this visible: distress is everywhere and in some layers, it becomes more intense."),
      
      tags$p("In contexts where oppression is continuous, psychological disorder is not an exception, but an expected outcome. There can be no return to normal when instability itself is the norm. Illness persists because the conditions that produce it remain."),
      
      div(class = "sources",
          HTML("<strong>Sources:</strong> Neyazi et al. (2024), <em>Depression, anxiety and stress among Afghans under Taliban government: a cross-sectional study</em>, <em>Discover Mental Health</em> 4:38, DOI: 10.1007/s44192-024-00090-5. Data shown are based on DASS-21 symptom categories, n = 2,698. Fanon, Frantz (1961/1963), <em>The Wretched of the Earth</em>, translated by Constance Farrington, Grove Press, chapter ‘Colonial War and Mental Disorders’."))
  )
))

htmltools::save_html(
  page,
  file = "index.html",
  libdir = "lib"
)


