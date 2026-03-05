# Zajecia_2_Data_Science_workflow.R

# 00 Start / reset ----
rm(list = ls())
options(stringsAsFactors = FALSE)

# 01 Folder roboczy ----
getwd()
# setwd("C:/Twoja/sciezka/do/folderu")  # ustaw u siebie

# 02 Pakiety ----
# install.packages(c("dplyr","ggplot2","writexl"))  # odpal raz, potem zakomentuj
library(dplyr)
library(ggplot2)
library(writexl)

# 03 Import danych ----
stopifnot(file.exists("kraje_makro_1.csv"), file.exists("kraje_makro_2.csv"))

kraje_1 <- read.csv("kraje_makro_1.csv", header = TRUE, sep = ",", dec = ".")
kraje_2 <- read.csv("kraje_makro_2.csv", header = TRUE, sep = ",", dec = ".")

# 04 Szybki podglad ----
head(kraje_1); head(kraje_2)
str(kraje_1); str(kraje_2)
summary(kraje_1); summary(kraje_2)

# 05 Przygotowanie danych ----
if ("X" %in% names(kraje_1)) kraje_1$X <- NULL
if ("X" %in% names(kraje_2)) kraje_2$X <- NULL

colnames(kraje_2) <- c("Kod_kraju","Nazwa","Region","Urbanizacja_proc","Internet_proc")

kraje_2$Region <- gsub("&", "and", as.character(kraje_2$Region))
kraje_2$Region <- as.factor(kraje_2$Region)

colSums(is.na(kraje_2))

# 06 Scalanie ----
kraje <- merge(kraje_1, kraje_2, by.x = "Kod", by.y = "Kod_kraju")
kraje$Nazwa <- NULL

# 07 Nowe zmienne ----
kraje <- kraje %>%
  mutate(
    Populacja_mln = Populacja / 1e6,
    PKB_per_capita = PKB / Populacja
  )

# 08 Podstawowa analiza ----
top_regiony <- kraje %>%
  group_by(Region) %>%
  summarise(
    n = n(),
    srednie_PKB_pc = mean(PKB_per_capita, na.rm = TRUE),
    sredni_internet = mean(Internet_proc, na.rm = TRUE),
    srednia_urbanizacja = mean(Urbanizacja_proc, na.rm = TRUE)
  ) %>%
  arrange(desc(srednie_PKB_pc))

print(head(top_regiony, 5))

# 09 Wykresy ----
p1 <- ggplot(kraje, aes(x = Urbanizacja_proc, y = PKB_per_capita, color = Region)) +
  geom_point(alpha = 0.7) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Urbanizacja a PKB per capita",
    x = "Urbanizacja (%)",
    y = "PKB per capita (USD, log10)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p1)

p2 <- ggplot(kraje, aes(x = Region)) +
  geom_bar() +
  labs(
    title = "Liczba krajów w regionach",
    x = "Region",
    y = "Liczba"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p2)

p3 <- ggplot(kraje, aes(x = reorder(Region, Internet_proc, FUN = median),
                        y = Internet_proc, fill = Region)) +
  geom_boxplot(alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Internet wg regionu",
    x = NULL,
    y = "Dostęp do internetu (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
print(p3)

p4 <- kraje %>%
  arrange(desc(PKB_per_capita)) %>%
  head(15) %>%
  ggplot(aes(x = reorder(Panstwo, PKB_per_capita),
             y = PKB_per_capita, fill = Region)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "TOP 15 PKB per capita (2016)",
    x = NULL,
    y = "PKB per capita (USD)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p4)

# 10 Eksport ----
write.csv(kraje, "kraje_analiza.csv", row.names = FALSE)
write_xlsx(kraje, "kraje_wynik.xlsx")
write.csv(top_regiony, "regiony_podsumowanie.csv", row.names = FALSE)

ggsave("wykres_01_urbanizacja_pkbpc.png", p1, width = 10, height = 6, dpi = 300)
ggsave("wykres_02_liczba_krajow_region.png", p2, width = 10, height = 6, dpi = 300)
ggsave("wykres_03_internet_boxplot.png", p3, width = 10, height = 6, dpi = 300)
ggsave("wykres_04_top15_pkbpc.png", p4, width = 10, height = 6, dpi = 300)

# 11 Koniec ----
getwd()
list.files()
