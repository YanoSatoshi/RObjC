## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

ORG_ITEM_STATUS <- list(
  levels = c(
    'canceled',   '取消',
    'paused',     '保留',
    'undefined',  '未承認',
    'open',       '承認',
    'closed',     '停止'
  ),
  labels = c(
    'canceled',   'canceled',
    'paused',     'paused',
    'undefined',  'undefined',
    'open',       'open',
    'closed',     'closed'
  ),
  ordered = TRUE
)


ORG_ORG_STATUS <- list(
  levels = c(
    'closed',  '利用停止',
    'capable', '利用中',
    'filled',  '登録不可'
  ),
  labels = c(
    'closed',  'closed',
    'capable', 'capable',
    'filled',  'filled'
  ),
  ordered = FALSE
)


ORG_ITEM_CONDITION <- list(
  levels = c(
    'open',   '支援中',
    'stop',   '中止',
    'closed', '終了'
  ),
  labels = c(
    'open',   'open',
    'stop',   'stop',
    'closed', 'closed'
  ),
  ordered = FALSE
)




KAKEN_GROUP_LABEL <- list(
  # list(code = int, label_en = str, label_ja = str)
  list(code = 0,   label_en = 'temporary',          label_ja = '一時利用'),
  list(code = 1,   label_en = 'national_univ',      label_ja = '国立大学'),
  list(code = 2,   label_en = 'public_univ',        label_ja = '公立大学'),
  list(code = 3,   label_en = 'private_univ',       label_ja = '私立大学'),
  list(code = 4,   label_en = 'junior_college',     label_ja = '短大・短期大学部'),
  list(code = 5,   label_en = 'nat_inst_mext',      label_ja = '文科研究機関'),
  list(code = 6,   label_en = 'nat_inst_education', label_ja = '教育政策研究所'),
  list(code = 7,   label_en = 'technical_college',  label_ja = '高等専門学校'),
  list(code = 8,   label_en = 'nat_inst_xcpt_mext', label_ja = '文科以外の省庁研究機関'),
  list(code = 9,   label_en = 'public_inst',        label_ja = '公立の研究機関'),
  list(code = 10,  label_en = 'indpd_admin_corp',   label_ja = '独立行政法人'),
  list(code = 11,  label_en = 'inc_foundation',     label_ja = '財団法人'),
  list(code = 12,  label_en = 'corporation',        label_ja = '一般法人'),
  list(code = 13,  label_en = 'un_univ',            label_ja = '国連大学')
) %>%
dplyr::bind_rows() %>%
dplyr::mutate(code = as.integer(code))






KAKEN_PREF_LABEL <- list(
  # list(code = str, pref = str)
  list(code = '01', pref = '北海道'),
  list(code = '11', pref = '青森県'),
  list(code = '12', pref = '岩手県'),
  list(code = '13', pref = '宮城県'),
  list(code = '14', pref = '秋田県'),
  list(code = '15', pref = '山形県'),
  list(code = '16', pref = '福島県'),
  list(code = '21', pref = '茨城県'),
  list(code = '22', pref = '栃木県'),
  list(code = '23', pref = '群馬県'),
  list(code = '24', pref = '埼玉県'),
  list(code = '25', pref = '千葉県'),
  list(code = '26', pref = '東京都'),
  list(code = '27', pref = '神奈川県'),
  list(code = '28', pref = '東京都'),
  list(code = '31', pref = '新潟県'),
  list(code = '32', pref = '富山県'),
  list(code = '33', pref = '石川県'),
  list(code = '34', pref = '福井県'),
  list(code = '35', pref = '山梨県'),
  list(code = '36', pref = '長野県'),
  list(code = '37', pref = '岐阜県'),
  list(code = '38', pref = '静岡県'),
  list(code = '39', pref = '愛知県'),
  list(code = '41', pref = '三重県'),
  list(code = '42', pref = '滋賀県'),
  list(code = '43', pref = '京都府'),
  list(code = '44', pref = '大阪府'),
  list(code = '45', pref = '兵庫県'),
  list(code = '46', pref = '奈良県'),
  list(code = '47', pref = '和歌山県'),
  list(code = '51', pref = '鳥取県'),
  list(code = '52', pref = '島根県'),
  list(code = '53', pref = '岡山県'),
  list(code = '54', pref = '広島県'),
  list(code = '55', pref = '山口県'),
  list(code = '61', pref = '徳島県'),
  list(code = '62', pref = '香川県'),
  list(code = '63', pref = '愛媛県'),
  list(code = '64', pref = '高知県'),
  list(code = '71', pref = '福岡県'),
  list(code = '72', pref = '佐賀県'),
  list(code = '73', pref = '長崎県'),
  list(code = '74', pref = '熊本県'),
  list(code = '75', pref = '大分県'),
  list(code = '76', pref = '宮崎県'),
  list(code = '77', pref = '鹿児島県'),
  list(code = '80', pref = '沖縄県')
) %>% dplyr::bind_rows()






#' org: org種目名\cr
#' kaken: KakenDB種目名\n
#' available: 検索可否。TRUE:KakenDB登録対象種目、FALSE:登録除外種目
KAKEN_CATEGORIES <- list(
  list(org = "特別推進研究", kaken = "特別推進研究", available = TRUE),
  list(org = "基盤研究(S)", kaken = "基盤研究(S)", available = TRUE),
  list(org = "基盤研究(A)", kaken = "基盤研究(A)", available = TRUE),
  list(org = "基盤研究(B)", kaken = "基盤研究(B)", available = TRUE),
  list(org = "基盤研究(C)", kaken = "基盤研究(C)", available = TRUE),
  list(org = "若手研究", kaken = "若手研究", available = TRUE),
  list(org = "挑戦的研究(開拓)", kaken = "挑戦的研究(開拓)", available = TRUE),
  list(org = "挑戦的研究(萌芽)", kaken = "挑戦的研究(萌芽)", available = TRUE),
  list(org = "学術変革領域研究(A)", kaken = "学術変革領域研究(A)", available = TRUE),
  list(org = "学術変革領域研究(A)-領域", kaken = "学術変革領域研究(A)", available = TRUE),
  list(org = "学術変革領域研究(A)-計画", kaken = "学術変革領域研究(A)", available = TRUE),
  list(org = "学術変革領域研究(A)-公募研究", kaken = "学術変革領域研究(A)", available = TRUE),
  list(org = "学術変革領域研究(B)", kaken = "学術変革領域研究(B)", available = TRUE),
  list(org = "学術変革領域研究(B)-領域", kaken = "学術変革領域研究(B)", available = TRUE),
  list(org = "新学術領域研究-計画", kaken = "新学術領域研究(研究領域提案型)", available = TRUE),
  list(org = "新学術領域研究-領域", kaken = "新学術領域研究(研究領域提案型)", available = TRUE),
  list(org = "新学術領域研究-公募研究", kaken = "新学術領域研究(研究領域提案型)", available = TRUE),
  list(org = "研究活動スタート支援", kaken = "研究活動スタート支援", available = TRUE),
  list(org = "国際共同研究加速基金(国際共同研究強化A)", kaken = "国際共同研究加速基金(国際共同研究強化(A))", available = TRUE),
  list(org = "国際共同研究加速基金(国際共同研究強化)", kaken = "国際共同研究加速基金(国際共同研究強化)", available = TRUE),
  list(org = "国際共同研究加速基金(国際共同研究強化B)", kaken = "国際共同研究加速基金(国際共同研究強化(B))", available = TRUE),
  list(org = "国際共同研究加速基金(海外連携研究)", kaken = "国際共同研究加速基金(海外連携研究)", available = TRUE),
  list(org = "国際共同研究加速基金(帰国発展研究)", kaken = "国際共同研究加速基金(帰国発展研究)", available = TRUE),
  list(org = "国際共同研究加速基金(国際先導研究)", kaken = "国際共同研究加速基金(国際先導研究)", available = TRUE),
  list(org = "その他", kaken = "その他", available = FALSE),
  list(org = "奨励研究", kaken = "奨励研究", available = FALSE), # 「研究課題を探す」kaken.nii.ac.jp からなら取得できる。
  list(org = "学振特別研究員DC1", kaken = "特別研究員奨励費", available = FALSE), # 「研究課題を探す」kaken.nii.ac.jp からなら取得できる。
  list(org = "学振特別研究員DC2", kaken = "特別研究員奨励費", available = FALSE), # 「研究課題を探す」kaken.nii.ac.jp からなら取得できる。
  list(org = "学振特別研究員PD", kaken = "特別研究員奨励費", available = FALSE),  # 「研究課題を探す」kaken.nii.ac.jp からなら取得できる。
  list(org = "特別研究員奨励費", kaken = "特別研究員奨励費", available = FALSE),  # 「研究課題を探す」kaken.nii.ac.jp からなら取得できる。
  list(org = "研究成果公開促進費", kaken = "研究成果公開促進費", available = FALSE),
  list(org = "研究成果公開促進費(学術図書)", kaken = "研究成果公開促進費", available = FALSE),
  list(org = "研究成果公開促進費(データベース)", kaken = "研究成果公開促進費", available = FALSE),
  list(org = "研究成果公開促進費(ひらめき☆ときめきサイエンス)", kaken = "研究成果公開促進費", available = FALSE),
  list(org = "創発的研究支援事業", kaken = NA, available = FALSE),
  list(org = "さきがけ", kaken = NA, available = FALSE),
  list(org = "未定", kaken = NA, available = FALSE),
  list(org = "A-STEP産学共同（育成型）", kaken = NA, available = FALSE),
  list(org = "三菱財団自然科学研究助成（一般助成）", kaken = NA, available = FALSE)
) %>%
dplyr::bind_rows()