load(file="/home/petter/data/ThesisCode/Topic Model/RData10/topic_models10_35_65_1_SK.Rdata")
load(file="/home/petter/data/ThesisCode/Topic Model/RData10/topic_models10_10_100_10_SK.Rdata")


#Model Diagnostics STM K = 10-100, by 10

sk_10_100_10 = topic_models10_10_100_10_SK$results 
sk_10_100_10 = sk_10_100_10 %>% select(K, exclus, residual, semcoh, heldout)

sk_10_100_10 %>% 
 transmute(K, `Exclusivity` = exclus,
            `Semantic Coherence` = semcoh,
            `Held-out Likelihood` = heldout,
            `Residuals` = residual) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(as.numeric(K), as.numeric(Value), color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       #title = "Model diagnostics by number of topics") +
  ) +
                 theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5, hjust=0.5),
    axis.text.y = element_text(size = 14), 
    axis.title = element_text(size = 14),
    strip.text = element_text(size=14))
       #subtitle = "These diagnostics indicate that a good number of topics would be around 60")
ggsave("/home/petter/data/ThesisCode/Topic Model/PNG10/SK_MD_10_100_10_test.png")


#Model Diagnostics K = 25-65, by 1
sk_35_65_1 = topic_models10_35_65_1_SK$results 
sk_35_66_1 = sk_35_65_1 %>% select(K, exclus, semcoh, residual,  heldout)

sk_35_65_1 %>% 
 transmute(K, `Exclusivity` = exclus,
            `Semantic Coherence` = semcoh,
            `Residuals` = residual,
            `Held-out Likelihood` = heldout) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(as.numeric(K), as.numeric(Value), color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL) +
                 theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5, hjust=0.5),
    axis.text.y = element_text(size = 14), 
    axis.title = element_text(size = 14),
    strip.text = element_text(size=14))
ggsave("/home/petter/data/ThesisCode/Topic Model/PNG10/sk_35_65_1.png")


'''jpeg(file="/home/petter/data/ThesisCode/Topic Model/RData10/topic_models21_35_65_1_SK.jpg")
plot(topic_models21_35_65_1_SK)
dev.off()
'''
sk_35_65_1 %>%
  select(K, exclus, semcoh) %>%
  unnest(cols = c(K, exclus, semcoh)) %>%
  ggplot(aes(exclus, semcoh)) +
  geom_density_2d(alpha = 0.5, show.legend = FALSE) +
  geom_point(color = "#525252", shape = 16,
             alpha = 0.8, show.legend = FALSE) +
  labs(x = "Semantic coherence", y = "Exclusivity") +
  facet_wrap(~K, ncol = 4) +

ggsave("/home/petter/data/ThesisCode/Topic Model/PNG10/ES_sk25_60_1_test.jpg")


sk_35_65_1 %>%
 transmute(K, `Exclusivity` = exclus,
            `Semantic Coherence` = semcoh,
            `Residuals` = residual,
            `Held-out Likelihood` = heldout) %>%
 gather(Metric, Value, -K) %>% 
   ggplot(aes(as.numeric(K), as.numeric(Value), color = Metric)) +
     geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  geom_density_2d(alpha = 0.5, show.legend = FALSE) +
  geom_point(color = "#525252", shape = 16,
             alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "Semantic coherence", y = "Exclusivity") 
ggsave("/home/petter/data/ThesisCode/Topic Model/PNG10/ES_sk3560_1.png")