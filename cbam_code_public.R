# =====================================================================
# CBAM 실증분석 전체 스크립트 (v2, 2026-11-15)
#
# [v1 -> v2 수정 사항]
#  FIX 1  4자리(HS4) CBAM 코드 54개(철강 전체, 알루미늄 괴·제품 일부, 암모니아,
#         비료 일부)가 "6자리 vs 4자리" 필터에서 수집 단계에 전부 탈락하던 버그 수정
#         -> HS6 일치 OR HS4(앞 4자리) 일치로 수집, 분류는 HS6 우선 -> HS4 순으로 부여
#  FIX 2  hs6_class, category_map 정의 추가 (v1 스크립트에는 정의가 빠져 있었음)
#  FIX 3  HSK10 -> HS6 x 국가 x 월 단위 집계 추가 (논문 식 (1)의 단위와 일치)
#         (끄려면 AGGREGATE_TO_HS6 <- FALSE)
#  FIX 4  MDE / 표 숫자 하드코딩 제거 -> 모델에서 자동 계산해서 csv로 저장
#  FIX 5  세 분류기준(보수/완화/절충) 모두 같은 함수로 기술통계 + 회귀
#  FIX 6  그래프 ggsave 추가 (source()로 돌려도 파일로 남도록)
#
# 실행 순서: 위에서 아래로. 섹션 5(데이터 수집)는 몇 분 걸림.
# =====================================================================

# ---------------------------------------------------------------------
# 0. 설정
# ---------------------------------------------------------------------

pkgs <- c("readxl", "dplyr", "tidyr", "tibble", "httr", "xml2", "lubridate",
          "fixest", "ggplot2", "modelsummary", "broom")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

MAP_DIR <- Sys.getenv("DATA_GO_KR_KEY")      # 분류표 엑셀 3개가 있는 폴더

# data.go.kr 인증키 (Decoding 버전). 기존 스크립트의 service_key 값을 그대로 붙여넣기
service_key <- ""

api_url <- "http://apis.data.go.kr/1220000/nitemtrade/getNitemtradeList"

eu_countries <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR",
                  "DE","GR","HU","IE","IT","LV","LT","LU","MT","NL",
                  "PL","PT","RO","SK","SI","ES","SE")
years <- 2022:2026

AGGREGATE_TO_HS6 <- TRUE   # TRUE: HSK10 행을 HS6 x 국가 x 월로 합산 (논문 식 (1) 단위)

# Annex I에서 4자리 heading 전체가 아니라 일부 CN8만 해당하는 경우,
# 해당하지 않는 HS6를 여기에 적어 제외 (7318, 7326, 7616 등은 Annex I 원문과 대조 필요)
hs6_exclude <- character(0)   # 예: c("731700", "761699")

# ---------------------------------------------------------------------
# 1. CN코드 매핑 불러오기 (보수적 / 완화적 / 절충적 3종)
# ---------------------------------------------------------------------

read_cbam_map <- function(path) {
  read_excel(path, sheet = "CN코드_매핑") %>%
    filter(!is.na(cn_code)) %>%
    transmute(
      cn_code_raw = as.character(cn_code),
      # 8자리(CN8)는 앞 6자리(HS6)로, 4자리(HS4)는 그대로 둠(뒤에서 접두어 매칭),
      # FeMn/FeCr/FeNi는 오타 수정 + 실제 HS6 하위코드로 확장, Hydrogen 9자리는 오타 수정
      hs_code = case_when(
        cn_code_raw == "72921"     ~ "720211,720219",
        cn_code_raw == "72024"     ~ "720241,720249",
        cn_code_raw == "72026"     ~ "720260",
        cn_code_raw == "280410000" ~ "280410",
        nchar(cn_code_raw) == 4    ~ cn_code_raw,
        nchar(cn_code_raw) == 6    ~ cn_code_raw,
        nchar(cn_code_raw) == 8    ~ substr(cn_code_raw, 1, 6),
        TRUE ~ NA_character_
      ),
      aggregated_category,
      # 전환기와 정의체제 분류가 동일함을 확인했으므로 전환기 값 사용
      classification = `classification_전환기(자동)`
    ) %>%
    separate_rows(hs_code, sep = ",")
}

map_cons_raw <- read_cbam_map(file.path(MAP_DIR, "cbam_classification_conservative.xlsx"))
map_len_raw  <- read_cbam_map(file.path(MAP_DIR, "cbam_classification_lenient.xlsx"))
map_mid_raw  <- read_cbam_map(file.path(MAP_DIR, "cbam_classification_middleground.xlsx"))

# 확인: 자릿수 분포 (4자리가 54개 있어야 정상)
table(nchar(map_cons_raw$hs_code), useNA = "always")
stopifnot(!any(is.na(map_cons_raw$hs_code)))

# 세 분류표의 코드 집합이 같은지 확인
if (!setequal(map_cons_raw$hs_code, map_mid_raw$hs_code) ||
    !setequal(map_cons_raw$hs_code, map_len_raw$hs_code)) {
  warning("세 분류표의 코드 집합이 다릅니다. 엑셀 확인 필요")
}

all_hs6 <- unique(map_cons_raw$hs_code[nchar(map_cons_raw$hs_code) == 6])
all_hs4 <- unique(map_cons_raw$hs_code[nchar(map_cons_raw$hs_code) == 4])
length(all_hs6); length(all_hs4)   # HS4가 54개 안팎이어야 정상

# ---------------------------------------------------------------------
# 2. 관세청 Open API 테스트 호출 (한 국가, 한 해)
# ---------------------------------------------------------------------

test_res <- GET(
  url = api_url,
  query = list(serviceKey = service_key, strtYymm = "202201",
               endYymm = "202212", cntyCd = "DE")
)
status_code(test_res)   # 200이어야 정상

parsed_test <- read_xml(content(test_res, as = "text", encoding = "UTF-8"))
items_test  <- xml_find_all(parsed_test, ".//item")
length(items_test)      # 한 번에 몇 건 받는지(=truncation 여부 점검용)

# ---------------------------------------------------------------------
# 3. 수집 함수  [FIX 1: HS6 일치 OR HS4 일치]
# ---------------------------------------------------------------------

fetch_log <- list()   # 호출별 원본 건수 기록 (응답이 잘리는지 점검)

fetch_one <- function(year, country) {
  res <- GET(
    url = api_url,
    query = list(serviceKey = service_key,
                 strtYymm = paste0(year, "01"),
                 endYymm  = paste0(year, "12"),
                 cntyCd   = country)
  )

  if (status_code(res) != 200) {
    message("실패: ", country, " ", year, " (status ", status_code(res), ")")
    return(NULL)
  }

  parsed <- read_xml(content(res, as = "text", encoding = "UTF-8"))
  items  <- xml_find_all(parsed, ".//item")
  if (length(items) == 0) return(NULL)

  df <- data.frame(
    hsCd   = xml_text(xml_find_first(items, ".//hsCd")),
    year   = xml_text(xml_find_first(items, ".//year")),
    statCd = xml_text(xml_find_first(items, ".//statCd")),
    expDlr = xml_text(xml_find_first(items, ".//expDlr")),
    expWgt = xml_text(xml_find_first(items, ".//expWgt")),
    stringsAsFactors = FALSE
  )
  n_raw <- nrow(df)

  df <- df %>%
    filter(statCd != "-", nchar(hsCd) == 10) %>%
    mutate(hs6 = substr(hsCd, 1, 6),
           hs4 = substr(hsCd, 1, 4)) %>%
    filter(hs6 %in% all_hs6 | hs4 %in% all_hs4) %>%   # <- v1은 hs6 %in% all_codes 뿐이었음
    select(-hs4)

  fetch_log[[paste(country, year)]] <<- c(n_raw = n_raw, n_kept = nrow(df))
  df
}

# 미니 테스트: 독일 2022 -> 철강(72xx) 행이 나오는지 확인
t1 <- fetch_one(2022, "DE")
table(substr(t1$hs6, 1, 2))   # "72"가 보여야 정상 (v1에서는 안 나왔음)

# ---------------------------------------------------------------------
# 4. 전체 국가 x 연도 수집  (몇 분 소요)
# ---------------------------------------------------------------------

fetch_log <- list()
results_list <- list()

for (y in years) {
  for (c in eu_countries) {
    key <- paste0(c, "_", y)
    message("가져오는 중: ", key)
    result <- tryCatch(fetch_one(y, c), error = function(e) {
      message("에러 발생: ", key, " - ", conditionMessage(e))
      NULL
    })
    results_list[[key]] <- result
    Sys.sleep(0.3)
  }
}

df_eu_hsk <- bind_rows(results_list)    # HSK10 단위 원자료(코드 매칭된 행만)
write.csv(df_eu_hsk, "df_eu_hsk_backup.csv", row.names = FALSE)

# 점검: 호출당 원본 건수. 최댓값이 1000, 10000 같은 '딱 떨어지는 수'에 걸려 있으면
# API가 응답을 잘라서 주고 있다는 뜻 -> 알려주세요
fetch_summary <- as.data.frame(do.call(rbind, fetch_log))
summary(fetch_summary$n_raw)
table(fetch_summary$n_raw == max(fetch_summary$n_raw))

nrow(df_eu_hsk)
table(df_eu_hsk$statCd)
table(substr(df_eu_hsk$hs6, 1, 2))   # 72(철강), 73, 76(알루미늄)이 보여야 정상

# ---------------------------------------------------------------------
# 5. 중복 점검 + HS6 x 국가 x 월 집계  [FIX 3]
# ---------------------------------------------------------------------

# 같은 (hsCd, year, statCd)가 두 번 이상 나오는 경우 점검
dup_keys <- df_eu_hsk %>% count(hsCd, year, statCd) %>% filter(n > 1)
nrow(dup_keys)

df_eu_hsk <- df_eu_hsk %>% distinct()    # 완전히 동일한 행만 제거

df_eu_num <- df_eu_hsk %>%
  mutate(expDlr = as.numeric(expDlr),
         expWgt = as.numeric(expWgt)) %>%
  filter(!hs6 %in% hs6_exclude)

if (AGGREGATE_TO_HS6) {
  df_eu_raw <- df_eu_num %>%
    group_by(hs6, year, statCd) %>%
    summarise(expDlr = sum(expDlr, na.rm = TRUE),
              expWgt = sum(expWgt, na.rm = TRUE),
              n_hsk  = n(),
              .groups = "drop")
} else {
  df_eu_raw <- df_eu_num %>% select(hs6, year, statCd, expDlr, expWgt)
}

nrow(df_eu_hsk); nrow(df_eu_raw)   # 집계 전/후 행 수 비교
write.csv(df_eu_raw, "df_eu_hs6_backup.csv", row.names = FALSE)

# ---------------------------------------------------------------------
# 6. 분류 매핑을 '실제 관측된 HS6'로 확장  [FIX 1, FIX 2]
#    - 6자리 코드는 정확히 일치하는 것을 우선 사용
#    - 나머지는 앞 4자리(HS4) heading으로 매칭
# ---------------------------------------------------------------------

obs_hs6 <- unique(df_eu_raw$hs6)

expand_to_hs6 <- function(map_raw, observed_hs6) {
  m  <- map_raw %>% filter(!is.na(hs_code))
  m6 <- m %>% filter(nchar(hs_code) == 6) %>%
    rename(hs6 = hs_code) %>% distinct(hs6, aggregated_category, classification)
  m4 <- m %>% filter(nchar(hs_code) == 4) %>%
    rename(hs4 = hs_code) %>% distinct(hs4, aggregated_category, classification)

  obs <- tibble(hs6 = observed_hs6, hs4 = substr(observed_hs6, 1, 4))

  out <- bind_rows(
    inner_join(obs, m6, by = "hs6"),
    anti_join(obs, m6, by = "hs6") %>% inner_join(m4, by = "hs4")
  ) %>% select(hs6, aggregated_category, classification)

  # 한 HS6가 두 카테고리/분류에 걸리는 경우 점검
  dup <- out %>% count(hs6) %>% filter(n > 1)
  if (nrow(dup) > 0) {
    warning("한 HS6가 복수 분류에 매칭됨: ", paste(dup$hs6, collapse = ", "))
    print(out %>% filter(hs6 %in% dup$hs6) %>% arrange(hs6))
    out <- out %>% group_by(hs6) %>% slice(1) %>% ungroup()
  }
  out
}

hs6_class   <- expand_to_hs6(map_cons_raw, obs_hs6)   # 보수적(기본) + aggregated_category
category_map <- hs6_class %>% select(hs6, aggregated_category)

cn_map_lenient <- expand_to_hs6(map_len_raw, obs_hs6) %>%
  select(hs6, classification_lenient = classification)
cn_map_middle  <- expand_to_hs6(map_mid_raw, obs_hs6) %>%
  select(hs6, classification_middle = classification)

# 점검: 매칭 안 된 HS6 (비어 있어야 정상) / 카테고리별 관측 HS6 개수
setdiff(obs_hs6, hs6_class$hs6)
hs6_class %>% count(aggregated_category, classification) %>% print(n = Inf)

# ---------------------------------------------------------------------
# 7. 패널 구성: Complex 라벨 + Post 3구간 (보수적 분류 기준)
# ---------------------------------------------------------------------

df_panel_full <- df_eu_raw %>%
  left_join(hs6_class, by = "hs6") %>%
  mutate(
    date   = ym(gsub("\\.", "-", year)),
    Complex = ifelse(classification == "complex good", 1, 0),
    log_value = log(expDlr + 1),
    log_price = ifelse(expWgt > 0, log((expDlr + 1) / expWgt), NA_real_),
    log_price_v2 = case_when(
      expWgt > 0 ~ log((expDlr + 1) / expWgt),
      expWgt == 0 & expDlr == 0 ~ 0,      # 무역 없음 -> 0
      TRUE ~ NA_real_
    )
  )

# 회귀분석용: 2023.10 이후, Post 3구간
df_panel <- df_panel_full %>%
  filter(date >= ym("2023-10")) %>%
  mutate(
    Post = case_when(
      date < ym("2024-07") ~ "0_transition1",
      date >= ym("2024-07") & date < ym("2026-01") ~ "1_cap",
      date >= ym("2026-01") ~ "2_definitive",
      TRUE ~ NA_character_
    ),
    Post  = factor(Post, levels = c("0_transition1", "1_cap", "2_definitive")),
    Post2 = factor(ifelse(date < ym("2024-07"), "0_nocap", "1_capORdefinitive"),
                   levels = c("0_nocap", "1_capORdefinitive"))
  )

table(df_panel$Complex, useNA = "always")
table(df_panel$Post, useNA = "always")
table(is.na(df_panel$log_price_v2))

# ---------------------------------------------------------------------
# 8. 세 분류기준별 패널 + 회귀 (동일 함수로 처리)
# ---------------------------------------------------------------------

build_panel <- function(df_base, class_map, class_col) {
  df_base %>%
    select(-classification, -Complex) %>%
    left_join(class_map, by = "hs6") %>%
    mutate(Complex = ifelse(.data[[class_col]] == "complex good", 1, 0))
}

df_panel_cons    <- df_panel    # Complex = 보수적 기준
df_panel_lenient <- build_panel(df_panel, cn_map_lenient, "classification_lenient")
df_panel_middle  <- build_panel(df_panel, cn_map_middle,  "classification_middle")

# 표본 점검: 그룹 x 구간별 관측치 / 무역실적 비율 (표본 소멸 여부)
check_scheme <- function(df_p) {
  df_p %>%
    mutate(group = ifelse(Complex == 1, "Complex", "Simple")) %>%
    group_by(group, Post) %>%
    summarise(n_codes = n_distinct(hs6), n = n(),
              n_nonzero = sum(expDlr > 0, na.rm = TRUE),
              pct_nonzero = round(100 * n_nonzero / n, 1),
              .groups = "drop")
}
check_scheme(df_panel_cons)
check_scheme(df_panel_lenient)
check_scheme(df_panel_middle)

run_did <- function(df_p) {
  list(
    value = feols(log_value ~ Post * Complex | hs6 + date, data = df_p, cluster = ~hs6),
    price = feols(log_price_v2 ~ Post * Complex | hs6 + date, data = df_p, cluster = ~hs6),
    value_2g = feols(log_value ~ Post2 * Complex | hs6 + date, data = df_p, cluster = ~hs6),
    price_2g = feols(log_price_v2 ~ Post2 * Complex | hs6 + date, data = df_p, cluster = ~hs6)
  )
}

m_mid  <- run_did(df_panel_middle)    # 주 분석 후보 (절충적)
m_cons <- run_did(df_panel_cons)      # 강건성 (보수적)
m_len  <- run_did(df_panel_lenient)   # 증거표용 (완화적)

# 논문에서 쓰던 이름 유지
model_value_middle <- m_mid$value
model_price_middle <- m_mid$price
model_value        <- m_cons$value
model_price_v2     <- m_cons$price
model_value_2g     <- m_cons$value_2g
model_price_2g     <- m_cons$price_2g

summary(model_value_middle)
summary(model_price_middle)
summary(model_value)
summary(model_price_v2)

# ---------------------------------------------------------------------
# 9. 결과 요약표 + MDE  [FIX 4: 하드코딩 제거, Table 6 숫자는 여기서 가져오기]
# ---------------------------------------------------------------------

collect_results <- function(fit, label) {
  ct <- as.data.frame(summary(fit)$coeftable)
  ct$term <- rownames(ct)
  ct <- ct[grepl(":", ct$term), , drop = FALSE]
  # tibble() 안에서 'model' 열이 인자를 가리지 않도록 미리 계산
  n_obs   <- nobs(fit)
  r2_all  <- as.numeric(r2(fit, "r2"))
  r2_with <- as.numeric(r2(fit, "wr2"))
  tibble(
    model = label,
    term = ct$term,
    estimate = ct$Estimate,
    se = ct$`Std. Error`,
    p_value = ct$`Pr(>|t|)`,
    mde_80power = 2.8 * ct$`Std. Error`,
    est_over_mde_pct = round(100 * abs(ct$Estimate) / (2.8 * ct$`Std. Error`), 1),
    nobs = n_obs,
    r2 = r2_all,
    r2_within = r2_with
  )
}

results_all <- bind_rows(
  collect_results(m_mid$value,  "Main (Middle) - log_value"),
  collect_results(m_mid$price,  "Main (Middle) - log_price"),
  collect_results(m_cons$value, "Robust (Conservative) - log_value"),
  collect_results(m_cons$price, "Robust (Conservative) - log_price"),
  collect_results(m_len$value,  "Lenient - log_value"),
  collect_results(m_len$price,  "Lenient - log_price"),
  collect_results(m_mid$value_2g,  "Main (Middle) 2-group - log_value"),
  collect_results(m_mid$price_2g,  "Main (Middle) 2-group - log_price")
)
print(results_all, n = Inf, width = Inf)
write.csv(results_all, "regression_results_summary.csv", row.names = FALSE)

# ---------------------------------------------------------------------
# 10. 기술통계표 (Table 3 / 4 / 5)
# ---------------------------------------------------------------------

desc_by_scheme <- function(df_p) {
  df_p %>%
    mutate(group = ifelse(Complex == 1, "Complex", "Simple")) %>%
    group_by(group, Post) %>%
    summarise(
      n_codes = n_distinct(hs6),
      n = n(),
      n_nonzero = sum(expDlr > 0, na.rm = TRUE),
      pct_nonzero = round(100 * n_nonzero / n, 1),
      mean_log_value = round(mean(log_value, na.rm = TRUE), 3),
      sd_log_value   = round(sd(log_value, na.rm = TRUE), 3),
      mean_log_price = round(mean(log_price_v2, na.rm = TRUE), 3),
      sd_log_price   = round(sd(log_price_v2, na.rm = TRUE), 3),
      .groups = "drop"
    )
}

desc_cons   <- desc_by_scheme(df_panel_cons)
desc_len    <- desc_by_scheme(df_panel_lenient)
desc_middle <- desc_by_scheme(df_panel_middle)

print(desc_cons); print(desc_len); print(desc_middle)
write.csv(desc_cons,   "table3_descriptive_stats_conservative.csv", row.names = FALSE)
write.csv(desc_len,    "table4_descriptive_stats_lenient.csv", row.names = FALSE)
write.csv(desc_middle, "table5_descriptive_stats_middle.csv", row.names = FALSE)

# 그룹별 HS6 개수 / 관측치 수 / 국가별 관측치 수 (본문 서술용)
df_panel_middle %>% distinct(hs6, Complex) %>% count(Complex)
df_panel_middle %>% count(statCd, sort = TRUE) %>% head(10)

# ---------------------------------------------------------------------
# 11. Table 6: 회귀표 (docx)
# ---------------------------------------------------------------------

models_list <- list(
  "log_value (Main, Middle Classification)" = model_value_middle,
  "log_price (Main, Middle Classification)" = model_price_middle,
  "log_value (Robustness, Conservative)"    = model_value,
  "log_price (Robustness, Conservative)"    = model_price_v2
)

modelsummary(
  models_list,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_map = c("nobs", "r.squared", "r2.within"),
  coef_rename = c(
    "Post1_cap:Complex" = "Cap Period x Complex",
    "Post2_definitive:Complex" = "Definitive Regime x Complex"
  ),
  title = "Table 6. DiD Regression Results: MRV Cost Disparity Test",
  output = "regression_table.docx"
)

# ---------------------------------------------------------------------
# 12. Figure 1: 평행추세 (pre-CBAM, 절충적 분류)
# ---------------------------------------------------------------------

trend_data_middle <- df_eu_raw %>%
  left_join(cn_map_middle, by = "hs6") %>%
  filter(!is.na(classification_middle)) %>%
  mutate(
    date = ym(gsub("\\.", "-", year)),
    group = ifelse(classification_middle == "complex good", "Complex", "Simple"),
    log_price_v2 = case_when(
      expWgt > 0 ~ log((expDlr + 1) / expWgt),
      expWgt == 0 & expDlr == 0 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(date < ym("2023-10")) %>%
  group_by(date, group) %>%
  summarise(mean_log_price = mean(log_price_v2, na.rm = TRUE),
            n_obs = n(), .groups = "drop")

fig_trend <- ggplot(trend_data_middle,
                    aes(x = date, y = mean_log_price,
                        color = group, linetype = group, shape = group)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  labs(
    title = "Pre-CBAM Export Unit Price (log) Trend to EU: Simple vs Complex Goods",
    subtitle = "2022.01 ~ 2023.09 (middle classification; zero-trade months coded as 0)",
    x = NULL, y = "Mean log unit price", color = "Category",
    linetype = "Category", shape = "Category"
  ) +
  scale_color_manual(values = c("Simple" = "black", "Complex" = "gray40")) +
  scale_linetype_manual(values = c("Simple" = "dashed", "Complex" = "solid")) +
  scale_shape_manual(values = c("Simple" = 17, "Complex" = 16)) +
  theme_minimal()
print(fig_trend)
ggsave("figure_1_parallel_trend_middle.png", fig_trend, width = 8, height = 5, dpi = 300)

# ---------------------------------------------------------------------
# 13. 완화적 분류기준 근거표 (표본 소멸 증거)
# ---------------------------------------------------------------------

lenient_evidence <- df_panel_lenient %>%
  mutate(group = ifelse(Complex == 1, "Complex", "Simple")) %>%
  group_by(group, Post) %>%
  summarise(n_codes = n_distinct(hs6), total_obs = n(),
            n_nonzero = sum(expDlr > 0, na.rm = TRUE),
            pct_nonzero = round(100 * n_nonzero / total_obs, 1),
            .groups = "drop")
print(lenient_evidence)
write.csv(lenient_evidence, "table_lenient_evidence.csv", row.names = FALSE)

# ---------------------------------------------------------------------
# 14. Figure 2: 계수 플롯
# ---------------------------------------------------------------------

extract_coefs <- function(model, model_name) {
  tidy(model, conf.int = TRUE) %>%
    filter(grepl(":", term)) %>%
    mutate(
      model = model_name,
      term_clean = case_when(
        grepl("1_cap", term) ~ "Cap Period x Complex",
        grepl("2_definitive", term) ~ "Definitive Regime x Complex",
        TRUE ~ term
      )
    )
}

coef_data <- bind_rows(
  extract_coefs(model_value_middle, "log_value (Main)"),
  extract_coefs(model_price_middle, "log_price (Main)"),
  extract_coefs(model_value,        "log_value (Robustness)"),
  extract_coefs(model_price_v2,     "log_price (Robustness)")
) %>%
  mutate(
    outcome = factor(ifelse(grepl("log_value", model), "Log value", "Log price"),
                     levels = c("Log value", "Log price")),
    spec    = factor(ifelse(grepl("Main", model), "Main", "Robustness"),
                     levels = c("Robustness", "Main")),          # puts Main on top
    term_clean = factor(term_clean,
                        levels = c("Definitive Regime x Complex",
                                   "Cap Period x Complex"))      # puts Cap on top
  )

fig_coef <- ggplot(coef_data, aes(x = term_clean, y = estimate,
                                  color = spec, shape = spec)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.5),
                  size = 0.4, linewidth = 0.7) +
  coord_flip() +
  facet_wrap(~ outcome) +
  scale_color_manual(values = c(Main = "black", Robustness = "gray55")) +
  scale_shape_manual(values = c(Main = 16, Robustness = 17)) +
  guides(color = guide_legend(reverse = TRUE),
         shape = guide_legend(reverse = TRUE)) +
  labs(x = NULL, y = "Estimated coefficient (95% CI)",
       color = NULL, shape = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.text = element_text(face = "bold"))

print(fig_coef)
ggsave("figure_2_coefficient_plot.pdf", fig_coef, width = 6.5, height = 3)

# ---------------------------------------------------------------------
# 15. 4.1 한국의 CBAM 노출도 (Table 1, Table 2, 그림)
#     expDlr = USD, expWgt = kg
# ---------------------------------------------------------------------

# Table 1: 카테고리별
exposure_by_category <- df_eu_raw %>%
  left_join(category_map, by = "hs6") %>%
  filter(!is.na(aggregated_category)) %>%
  group_by(aggregated_category) %>%
  summarise(
    total_export_usd = sum(expDlr, na.rm = TRUE),
    n_hs6 = n_distinct(hs6),
    n_months_with_trade = sum(expDlr > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_export_usd)) %>%
  mutate(
    total_export_usd_million = round(total_export_usd / 1e6, 1),
    pct_of_total = round(100 * total_export_usd / sum(total_export_usd), 1)
  ) %>%
  select(aggregated_category, total_export_usd_million, n_hs6,
         n_months_with_trade, pct_of_total)

print(exposure_by_category, n = Inf)
write.csv(exposure_by_category, "table_4_1_exposure_by_category.csv", row.names = FALSE)

# 점검: 철강이 들어왔는지
stopifnot(any(grepl("steel", exposure_by_category$aggregated_category, ignore.case = TRUE)))

# Table 2: 단순재 vs 복합재 (절충적 분류)
exposure_by_classification <- df_eu_raw %>%
  left_join(cn_map_middle, by = "hs6") %>%
  filter(!is.na(classification_middle)) %>%
  mutate(group = ifelse(classification_middle == "complex good",
                        "Downstream/Processed (Complex)", "Primary/Upstream (Simple)")) %>%
  group_by(group) %>%
  summarise(
    total_export_usd = sum(expDlr, na.rm = TRUE),
    n_codes = n_distinct(hs6),
    n_months_with_trade = sum(expDlr > 0, na.rm = TRUE),
    total_months_possible = n(),
    .groups = "drop"
  ) %>%
  mutate(
    total_export_usd_million = round(total_export_usd / 1e6, 1),
    pct_months_active = round(100 * n_months_with_trade / total_months_possible, 1),
    pct_of_total_value = round(100 * total_export_usd / sum(total_export_usd), 1)
  ) %>%
  select(group, total_export_usd_million, n_codes, pct_months_active, pct_of_total_value)

print(exposure_by_classification)
write.csv(exposure_by_classification, "table_4_1_primary_vs_processed.csv", row.names = FALSE)

# 본문 서술용: 수출액 상위 HS6 10개
top_hs6 <- df_eu_raw %>%
  left_join(category_map, by = "hs6") %>%
  group_by(hs6, aggregated_category) %>%
  summarise(total_export_usd_million = round(sum(expDlr, na.rm = TRUE) / 1e6, 1),
            .groups = "drop") %>%
  arrange(desc(total_export_usd_million)) %>%
  head(10)
print(top_hs6)

# 그림 (Figure 1을 논문에서 뺐다면 생략 가능)
fig_exposure <- ggplot(exposure_by_category %>% filter(total_export_usd_million > 0),
                       aes(x = reorder(aggregated_category, total_export_usd_million),
                           y = total_export_usd_million)) +
  geom_col(fill = "gray40") +
  coord_flip() +
  labs(title = "Korea's Exports to EU by CBAM Goods Category (2022.01-2026.07)",
       x = NULL, y = "Total Export Value (USD million)") +
  theme_minimal()
ggsave("figure_4_1_exposure_by_category.png", fig_exposure, width = 8, height = 6, dpi = 300)

# ---------------------------------------------------------------------
# 16. 부록: CN코드 -> HS 코드 매핑표 (절충적 분류)
#     4자리 코드는 heading 전체로 매칭되므로 '관측된 HS6 개수'를 함께 표시
# ---------------------------------------------------------------------

appendix_mapping <- map_mid_raw %>%
  mutate(
    match_level = ifelse(nchar(hs_code) == 4, "HS4 heading", "HS6"),
    n_hs6_observed = vapply(hs_code, function(x) sum(startsWith(obs_hs6, x)), numeric(1))
  ) %>%
  select(cn_code = cn_code_raw, hs_code, match_level, aggregated_category,
         classification, n_hs6_observed) %>%
  distinct() %>%
  arrange(hs_code)

print(appendix_mapping, n = Inf)
write.csv(appendix_mapping, "appendix_cn_hs6_mapping.csv", row.names = FALSE)


# ---------------------------------------------------------------------
# 17. Placebo: pre-CBAM 기간(2022.01~2023.09)에 가짜 시행시점을 두고 같은 DiD
#     절충적 분류, 같은 고정효과/클러스터. 유의하면 사전 추세 신호.
# ---------------------------------------------------------------------

df_pre <- df_panel_full %>%
  filter(date < ym("2023-10")) %>%
  select(-classification, -Complex) %>%
  left_join(cn_map_middle, by = "hs6") %>%
  mutate(Complex = ifelse(classification_middle == "complex good", 1, 0))

run_placebo <- function(fake_date) {
  d <- df_pre %>% mutate(FakePost = as.integer(date >= ym(fake_date)))
  list(
    value = feols(log_value    ~ FakePost * Complex | hs6 + date, data = d, cluster = ~hs6),
    price = feols(log_price_v2 ~ FakePost * Complex | hs6 + date, data = d, cluster = ~hs6)
  )
}

pl_2022_07 <- run_placebo("2022-07")
pl_2022_12 <- run_placebo("2022-12")

placebo_results <- bind_rows(
  collect_results(pl_2022_07$value, "Placebo 2022.07 - log_value"),
  collect_results(pl_2022_07$price, "Placebo 2022.07 - log_price"),
  collect_results(pl_2022_12$value, "Placebo 2022.12 - log_value"),
  collect_results(pl_2022_12$price, "Placebo 2022.12 - log_price")
)
print(placebo_results, n = Inf, width = Inf)
write.csv(placebo_results, "placebo_results.csv", row.names = FALSE)