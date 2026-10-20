# ============================================================
# 06_shr_validation_and_comparison.R
# 목적: 팀원 05_EWS_integrated 결과(ShR/SR 이원화)를 검증하고,
#       기존 04 RiskScore와 비교하는 분석
#
# 핵심 산출:
#   1) ShR threshold sensitivity: 임계값별 안정기 false alarm, 선행경보, 충격구간 포착
#   2) Component contribution: ShR을 구성요소별로 분해하여 어떤 신호가 점수를 만들었는지 확인
#   3) Component ablation: 특정 구성요소 제거 시 포착력 변화 확인
#   4) 04 RiskScore vs 05 ShR 비교: 민감도/특이도 trade-off 점검
#   5) 데이터 신뢰도 반영 요약: Low confidence 품목 분리 표시
#
# 실행 전제:
#   - 팀원 05_EWS_integrated.R 실행 후 output/05_EWS/tables/05_risk_score_monthly.csv 존재 권장
#   - 본인 04_ts_preprocessing_EWS_trade.R 실행 후 04_ts_ews_output/tables/04_risk_score_monthly.csv 존재 시 비교 수행
# ============================================================

# ------------------------------------------------------------
# 0. 패키지와 경로 설정
# ------------------------------------------------------------
required_pkgs <- c("tidyverse", "lubridate", "scales")
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# 사용자 로컬 프로젝트 폴더
# Windows 경로는 역슬래시(\\) 대신 슬래시(/)를 쓰는 것이 R에서 안전하다.
PROJECT_DIR <- Sys.getenv("PORTFOLIO_PROJECT_DIR", unset = getwd())

OUT_DIR   <- file.path(PROJECT_DIR, "output", "06_validation")
TABLE_DIR <- file.path(OUT_DIR, "tables")
PLOT_DIR  <- file.path(OUT_DIR, "plots")
dir.create(TABLE_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(PLOT_DIR,  showWarnings = FALSE, recursive = TRUE)

# 기간 정의
BASE_START <- as.Date("2015-01-01")
BASE_END   <- as.Date("2020-12-01")
PRE_START  <- as.Date("2021-06-01")
PRE_END    <- as.Date("2021-08-01")
SHOCK_START <- as.Date("2021-09-01")
SHOCK_END   <- as.Date("2022-06-01")
POST_START  <- as.Date("2023-01-01")
POST_END    <- as.Date("2024-12-01")

pick_file <- function(candidates, required = TRUE) {
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) {
    if (required) stop("파일을 찾지 못했습니다. 후보:\n", paste(candidates, collapse = "\n"))
    return(NA_character_)
  }
  hit[1]
}

read_utf8_csv <- function(path) {
  message("[읽기] ", path)
  readr::read_csv(
    path,
    locale = readr::locale(encoding = "UTF-8"),
    show_col_types = FALSE,
    col_types = cols(.default = col_guess(), date = col_date(), item_label = col_character())
  )
}

save_tbl <- function(df, filename) {
  readr::write_csv(df, file.path(TABLE_DIR, filename), na = "")
  message("[저장] ", file.path(TABLE_DIR, filename))
}

save_plot <- function(p, filename, width = 12, height = 7) {
  ggplot2::ggsave(file.path(PLOT_DIR, filename), p, width = width, height = height, dpi = 150)
  message("[저장] ", file.path(PLOT_DIR, filename))
}

# ------------------------------------------------------------
# 1. 팀원 05 결과와 본인 04 결과 불러오기
# ------------------------------------------------------------
team05_file <- pick_file(c(
  file.path(PROJECT_DIR, "output", "05_EWS", "tables", "05_risk_score_monthly.csv"),
  file.path(PROJECT_DIR, "05_EWS", "tables", "05_risk_score_monthly.csv"),
  file.path(PROJECT_DIR, "data", "05_EWS", "tables", "05_risk_score_monthly.csv"),
  "05_risk_score_monthly.csv"
))

risk05 <- read_utf8_csv(team05_file) %>%
  mutate(
    date = as.Date(date),
    year = lubridate::year(date),
    month = lubridate::month(date),
    period_eval = case_when(
      date >= BASE_START & date <= BASE_END ~ "baseline",
      date >= PRE_START & date <= PRE_END ~ "pre_warning",
      date >= SHOCK_START & date <= SHOCK_END ~ "shock",
      date >= POST_START & date <= POST_END ~ "post_shock",
      TRUE ~ "transition"
    ),
    data_confidence = if_else(is.na(data_confidence), "Unknown", data_confidence)
  )

# 04 결과는 있으면 비교하고, 없으면 05 검증만 진행
risk04_file <- pick_file(c(
  file.path(PROJECT_DIR, "data", "04_ts_ews_output", "tables", "04_risk_score_monthly.csv"),
  file.path(PROJECT_DIR, "04_ts_ews_output", "tables", "04_risk_score_monthly.csv"),
  file.path(PROJECT_DIR, "output", "04_ts_ews_output", "tables", "04_risk_score_monthly.csv"),
  "04_risk_score_monthly.csv"
), required = FALSE)

has_04 <- !is.na(risk04_file)
if (has_04) {
  risk04 <- read_utf8_csv(risk04_file) %>%
    mutate(
      date = as.Date(date),
      year = lubridate::year(date),
      month = lubridate::month(date),
      period_eval = case_when(
        date >= BASE_START & date <= BASE_END ~ "baseline",
        date >= PRE_START & date <= PRE_END ~ "pre_warning",
        date >= SHOCK_START & date <= SHOCK_END ~ "shock",
        date >= POST_START & date <= POST_END ~ "post_shock",
        TRUE ~ "transition"
      )
    )
} else {
  message("[안내] 04_risk_score_monthly.csv를 찾지 못해 04 vs 05 비교는 건너뜁니다.")
}

# ------------------------------------------------------------
# 2. ShR 임계값 민감도 분석
# ------------------------------------------------------------
# 평가 지표:
# baseline_alert_rate: 안정기 2015~2020에서 경보가 얼마나 자주 울리는가. 낮을수록 specificity가 좋음.
# pre_alert_months: 2021.06~08 선행경보 구간 3개월 중 몇 개월 포착했는가.
# shock_alert_rate: 2021.09~2022.06 충격 구간 10개월 중 몇 개월 포착했는가.
# post_alert_rate: 2023~2024 사후 구간에서 여전히 높은 위험이 남는가.

threshold_grid <- 2:6

thr_eval_item <- purrr::map_dfr(threshold_grid, function(thr) {
  risk05 %>%
    group_by(item_label) %>%
    summarise(
      threshold = thr,
      data_confidence = first(data_confidence),
      trade_month_ratio = suppressWarnings(first(trade_month_ratio)),
      n_baseline = sum(period_eval == "baseline", na.rm = TRUE),
      baseline_alert_months = sum(period_eval == "baseline" & ShR >= thr, na.rm = TRUE),
      baseline_alert_rate = baseline_alert_months / n_baseline,
      pre_alert_months = sum(period_eval == "pre_warning" & ShR >= thr, na.rm = TRUE),
      pre_alert_any = pre_alert_months > 0,
      shock_alert_months = sum(period_eval == "shock" & ShR >= thr, na.rm = TRUE),
      shock_alert_rate = shock_alert_months / sum(period_eval == "shock", na.rm = TRUE),
      post_alert_months = sum(period_eval == "post_shock" & ShR >= thr, na.rm = TRUE),
      post_alert_rate = post_alert_months / sum(period_eval == "post_shock", na.rm = TRUE),
      max_pre_ShR = max(ShR[period_eval == "pre_warning"], na.rm = TRUE),
      max_shock_ShR = max(ShR[period_eval == "shock"], na.rm = TRUE),
      .groups = "drop"
    )
}) %>%
  mutate(across(c(max_pre_ShR, max_shock_ShR), ~ if_else(is.infinite(.x), NA_real_, .x)))

save_tbl(thr_eval_item, "06_threshold_sensitivity_by_item.csv")

thr_eval_overall <- thr_eval_item %>%
  group_by(threshold) %>%
  summarise(
    n_items = n(),
    n_pre_items = sum(pre_alert_any, na.rm = TRUE),
    mean_baseline_alert_rate = mean(baseline_alert_rate, na.rm = TRUE),
    mean_shock_alert_rate = mean(shock_alert_rate, na.rm = TRUE),
    mean_post_alert_rate = mean(post_alert_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pre_item_capture_rate = n_pre_items / n_items,
    score_summary = mean_shock_alert_rate - mean_baseline_alert_rate
  )

save_tbl(thr_eval_overall, "06_threshold_sensitivity_overall.csv")

p_thr <- thr_eval_overall %>%
  pivot_longer(
    cols = c(pre_item_capture_rate, mean_baseline_alert_rate, mean_shock_alert_rate, mean_post_alert_rate),
    names_to = "metric", values_to = "value"
  ) %>%
  ggplot(aes(x = threshold, y = value, group = metric, linetype = metric)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = threshold_grid) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "ShR 임계값 민감도 분석",
    subtitle = "좋은 임계값은 충격구간 포착률은 높고 안정기 경보율은 낮아야 함",
    x = "ShR threshold", y = "Rate", linetype = "Metric"
  ) +
  theme_minimal()
save_plot(p_thr, "06_01_threshold_sensitivity.png", width = 11, height = 7)

# ------------------------------------------------------------
# 3. ShR 구성요소 기여도 분석
# ------------------------------------------------------------
# 05 코드의 ShR 구성:
# 2*a_price_level + 2*a_price_yoy + 1*a_qty_drop + 1*a_pq + 1*a_stl + 1*a_cusum
component_data <- risk05 %>%
  mutate(
    c_price_level = 2L * as.integer(coalesce(a_price_level, FALSE)),
    c_price_yoy   = 2L * as.integer(coalesce(a_price_yoy, FALSE)),
    c_qty_drop    = 1L * as.integer(coalesce(a_qty_drop, FALSE)),
    c_pq          = 1L * as.integer(coalesce(a_pq, FALSE)),
    c_stl         = 1L * as.integer(coalesce(a_stl, FALSE)),
    c_cusum       = 1L * as.integer(coalesce(a_cusum, FALSE))
  )

component_long <- component_data %>%
  select(item_label, date, period_eval, ShR, starts_with("c_")) %>%
  pivot_longer(starts_with("c_"), names_to = "component", values_to = "points") %>%
  mutate(component = recode(component,
                            c_price_level = "가격 레벨 이탈",
                            c_price_yoy   = "YoY 단가 급등",
                            c_qty_drop    = "YoY 물량 감소",
                            c_pq          = "P-Q 공급압박",
                            c_stl         = "STL 잔차 이상",
                            c_cusum       = "CUSUM 누적상승"))

component_summary <- component_long %>%
  filter(period_eval %in% c("baseline", "pre_warning", "shock", "post_shock")) %>%
  group_by(item_label, period_eval, component) %>%
  summarise(
    mean_points = mean(points, na.rm = TRUE),
    hit_months = sum(points > 0, na.rm = TRUE),
    n_months = n(),
    hit_rate = hit_months / n_months,
    .groups = "drop"
  )

save_tbl(component_summary, "06_component_contribution_by_item_period.csv")

p_comp_shock <- component_summary %>%
  filter(period_eval == "shock") %>%
  ggplot(aes(x = component, y = item_label, fill = mean_points)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", mean_points)), size = 3) +
  labs(
    title = "충격 구간(2021.09~2022.06)의 ShR 구성요소 기여도",
    subtitle = "값은 해당 기간 월평균 점수 기여도. 어떤 신호가 경보를 만들었는지 설명 가능",
    x = NULL, y = NULL, fill = "평균 점수"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p_comp_shock, "06_02_component_contribution_shock.png", width = 12, height = 7)

# ------------------------------------------------------------
# 4. Component ablation: 구성요소 하나씩 제거했을 때 포착력 변화
# ------------------------------------------------------------
ablation_components <- tibble::tribble(
  ~component_removed, ~colname,
  "none", "none",
  "가격 레벨 이탈 제거", "c_price_level",
  "YoY 단가 급등 제거", "c_price_yoy",
  "YoY 물량 감소 제거", "c_qty_drop",
  "P-Q 공급압박 제거", "c_pq",
  "STL 잔차 이상 제거", "c_stl",
  "CUSUM 누적상승 제거", "c_cusum"
)

ablation_eval <- purrr::map_dfr(seq_len(nrow(ablation_components)), function(i) {
  comp <- ablation_components$colname[i]
  label <- ablation_components$component_removed[i]

  dat <- component_data %>%
    mutate(
      ShR_ablate = if (comp == "none") ShR else ShR - .data[[comp]],
      ShR_ablate = pmax(ShR_ablate, 0)
    )

  dat %>%
    group_by(item_label) %>%
    summarise(
      component_removed = label,
      baseline_alert_rate = mean(ShR_ablate >= 4 & period_eval == "baseline", na.rm = TRUE) /
        mean(period_eval == "baseline", na.rm = TRUE),
      pre_alert_months = sum(period_eval == "pre_warning" & ShR_ablate >= 4, na.rm = TRUE),
      shock_alert_months = sum(period_eval == "shock" & ShR_ablate >= 4, na.rm = TRUE),
      shock_alert_rate = shock_alert_months / sum(period_eval == "shock", na.rm = TRUE),
      .groups = "drop"
    )
}) %>%
  mutate(baseline_alert_rate = if_else(is.nan(baseline_alert_rate), NA_real_, baseline_alert_rate))

save_tbl(ablation_eval, "06_component_ablation_by_item.csv")

ablation_overall <- ablation_eval %>%
  group_by(component_removed) %>%
  summarise(
    mean_pre_alert_months = mean(pre_alert_months, na.rm = TRUE),
    mean_shock_alert_rate = mean(shock_alert_rate, na.rm = TRUE),
    mean_baseline_alert_rate = mean(baseline_alert_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_shock_alert_rate), desc(mean_pre_alert_months))

save_tbl(ablation_overall, "06_component_ablation_overall.csv")

p_ablation <- ablation_overall %>%
  pivot_longer(cols = c(mean_shock_alert_rate, mean_baseline_alert_rate),
               names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = reorder(component_removed, value), y = value, linetype = metric, group = metric)) +
  geom_point(size = 2) +
  geom_line(linewidth = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "ShR 구성요소 제거 민감도 분석",
    subtitle = "특정 요소를 제거했을 때 충격구간 포착률과 안정기 경보율이 어떻게 변하는지 확인",
    x = NULL, y = "Rate", linetype = "Metric"
  ) +
  theme_minimal()
save_plot(p_ablation, "06_03_component_ablation.png", width = 12, height = 7)

# ------------------------------------------------------------
# 5. 04 RiskScore vs 05 ShR 비교
# ------------------------------------------------------------
if (has_04) {
  comp_04_05 <- risk05 %>%
    select(item_label, date, ShR, ShR_level, SR, SR_level, monitoring_score, data_confidence, period_eval) %>%
    left_join(
      risk04 %>% select(item_label, date, risk_score_04 = risk_score, risk_level_04 = risk_level),
      by = c("item_label", "date")
    ) %>%
    mutate(
      alert_05_ShR4 = ShR >= 4,
      alert_04_Risk4 = risk_score_04 >= 4,
      diff_04_minus_05 = risk_score_04 - ShR
    )

  save_tbl(comp_04_05, "06_compare_04_riskscore_vs_05_shr_monthly.csv")

  compare_summary <- comp_04_05 %>%
    group_by(item_label, period_eval) %>%
    summarise(
      mean_04 = mean(risk_score_04, na.rm = TRUE),
      mean_05 = mean(ShR, na.rm = TRUE),
      mean_diff_04_minus_05 = mean(diff_04_minus_05, na.rm = TRUE),
      alert_04_months = sum(alert_04_Risk4, na.rm = TRUE),
      alert_05_months = sum(alert_05_ShR4, na.rm = TRUE),
      .groups = "drop"
    )

  save_tbl(compare_summary, "06_compare_04_vs_05_summary.csv")

  p_compare <- comp_04_05 %>%
    filter(!is.na(risk_score_04)) %>%
    ggplot(aes(x = ShR, y = risk_score_04)) +
    geom_jitter(aes(shape = period_eval), width = 0.15, height = 0.15, alpha = 0.45) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    facet_wrap(~ item_label) +
    scale_x_continuous(breaks = 0:8) +
    labs(
      title = "04 통합 RiskScore vs 05 ShR 비교",
      subtitle = "04는 구조위험(HHI/Top1)을 월별 점수에 포함, 05는 ShR을 월별 메인 경보로 분리",
      x = "05 ShR", y = "04 RiskScore", shape = "기간"
    ) +
    theme_minimal()
  save_plot(p_compare, "06_04_compare_04_vs_05_scatter.png", width = 13, height = 8)
}

# ------------------------------------------------------------
# 6. 회의/보고서용 핵심 요약 자동 생성
# ------------------------------------------------------------
key_summary <- list(
  threshold_overall = thr_eval_overall,
  best_threshold_candidate = thr_eval_overall %>%
    arrange(desc(score_summary), desc(pre_item_capture_rate), mean_baseline_alert_rate) %>%
    slice(1),
  ablation_overall = ablation_overall,
  low_confidence_items = risk05 %>%
    distinct(item_label, data_confidence, trade_month_ratio) %>%
    filter(data_confidence %in% c("Low", "Unknown"))
)

sink(file.path(OUT_DIR, "06_validation_text_summary.txt"))
cat("06 검증 분석 요약\n")
cat("============================================================\n\n")
cat("1. 임계값 민감도 분석\n")
print(key_summary$threshold_overall)
cat("\n권장 임계값 후보:\n")
print(key_summary$best_threshold_candidate)
cat("\n2. 구성요소 제거 민감도 분석\n")
print(key_summary$ablation_overall)
cat("\n3. 데이터 신뢰도 Low/Unknown 품목\n")
print(key_summary$low_confidence_items)
cat("\n해석 가이드:\n")
cat("- ShR threshold가 낮으면 선행경보 민감도는 높아지지만 안정기 false alarm이 증가한다.\n")
cat("- threshold=4는 팀원 분석의 기본값이며, threshold=3 또는 5와 비교하여 민감도-특이도 균형을 설명한다.\n")
cat("- ablation에서 특정 구성요소 제거 시 shock_alert_rate가 크게 떨어지면, 해당 구성요소가 충격 탐지에 핵심임을 의미한다.\n")
cat("- 04 RiskScore가 05 ShR보다 높게 나오는 월은 대체로 HHI/Top1 구조위험 점수가 포함된 효과로 해석한다.\n")
sink()

message("\n[완료] 06 검증 분석 산출물이 생성되었습니다: ", OUT_DIR)
