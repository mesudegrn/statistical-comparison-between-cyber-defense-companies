library(readxl)
library(dplyr)

path <- "C:/Users/bravo/Downloads/data.xlsx"
data <- read_excel(path, sheet = "data")

A <- filter(data, Company == "A")
B <- filter(data, Company == "B")

sum_by_org <- function(df, col) {
  df %>%
    group_by(Organization) %>%
    summarise(total = sum(.data[[col]], na.rm = TRUE), .groups = "drop") %>%
    pull(total)
}

A_ransom <- sum_by_org(A, "DetectionTime_Ransomware")
B_ransom <- sum_by_org(B, "DetectionTime_Ransomware")
A_ddos   <- sum_by_org(A, "DetectionTime_DDoS")
B_ddos   <- sum_by_org(B, "DetectionTime_DDoS")

ci99 <- function(x, y) t.test(x, y, conf.level = 0.99, var.equal = FALSE)$conf.int
ci_ransom <- ci99(A_ransom, B_ransom)
ci_ddos   <- ci99(A_ddos,   B_ddos)

cat(sprintf("99%% CI Ransomware: [%.1f, %.1f]\n", ci_ransom[1], ci_ransom[2]))
cat(sprintf("99%% CI DDoS:       [%.1f, %.1f]\n", ci_ddos[1],   ci_ddos[2]))
