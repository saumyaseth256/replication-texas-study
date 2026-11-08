library(dplyr)
library(tidyr)
library(stringr)
library(data.table)

###############################################################################
# Download data.zip from OneDrive
###############################################################################

onedrive_zip_url <- "https://penno365-my.sharepoint.com/:u:/g/personal/saumyase_upenn_edu/IQB1eS7wDWmwSYWJwQoRXLWWARO2Z_2lkGp8cpN-e9aak2g?e=wZN8pA"
repo_root <- normalizePath(getwd(), mustWork = TRUE)
data_dir <- file.path(repo_root, "data")
required_files <- c(
  file.path(data_dir, "aeis campus info", "97-98", "ref", "campref.dat"),
  file.path(
    data_dir,
    "aeis campus info",
    "02-03",
    "taks",
    "camptaks311-1belowpanel.dta"
  ),
  file.path(data_dir, "aeis campus info", "02-03", "other", "campothr.dat"),
  file.path(data_dir, "aeis campus info", "02-03", "comp", "campcomp.dta"),
  file.path(data_dir, "peims finance", "Actual99", "ACT9FILE"),
  file.path(data_dir, "peims finance", "Actual08", "ACTUAL_2008F.TXT")
)
if (!all(file.exists(required_files))) {
  message("Downloading replication data...")
  clean_url <- sub("[?].*$", "", onedrive_zip_url)
  download_url <- paste0(clean_url, "?download=1")
  zip_path <- file.path(repo_root, "data_download.zip")
  extraction_dir <- file.path(repo_root, "data_download_temp")
  if (file.exists(zip_path)) {
    unlink(zip_path)
  }
  if (dir.exists(extraction_dir)) {
    unlink(extraction_dir, recursive = TRUE)
  }
  dir.create(extraction_dir, recursive = TRUE)
  old_timeout <- getOption("timeout")
  options(timeout = 7200)
  download.file(
    url = download_url,
    destfile = zip_path,
    mode = "wb",
    method = "libcurl",
    quiet = FALSE
  )
  options(timeout = old_timeout)
  connection <- file(zip_path, open = "rb")
  signature <- readBin(connection, what = "raw", n = 2)
  close(connection)
  if (!identical(signature, charToRaw("PK"))) {
    unlink(zip_path)
    stop(
      "OneDrive returned a web page rather than data.zip. Share the ZIP ",
      "file itself with 'Anyone with the link can view'."
    )
  }
  message("Extracting replication data...")
  unzip(zipfile = zip_path, exdir = extraction_dir)
  if (dir.exists(file.path(extraction_dir, "data"))) {
    extracted_data <- file.path(extraction_dir, "data")
  } else if (dir.exists(file.path(extraction_dir, "aeis campus info")) &&
             dir.exists(file.path(extraction_dir, "peims finance"))) {
    extracted_data <- extraction_dir
  } else {
    stop(
      "The ZIP does not have the expected folder structure. It must contain ",
      "'aeis campus info' and 'peims finance'."
    )
  }
  if (dir.exists(data_dir)) {
    unlink(data_dir, recursive = TRUE)
  }
  dir.create(data_dir, recursive = TRUE)
  files_to_copy <- list.files(
    extracted_data,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  copy_result <- file.copy(
    from = files_to_copy,
    to = data_dir,
    recursive = TRUE,
    copy.mode = TRUE,
    copy.date = TRUE
  )
  if (!all(copy_result)) {
    stop("Some data files could not be copied.")
  }
  unlink(zip_path)
  unlink(extraction_dir, recursive = TRUE)
  missing_files <- required_files[!file.exists(required_files)]
  if (length(missing_files) > 0L) {
    stop(
      "The download completed, but these required files are missing:\n",
      paste(missing_files, collapse = "\n")
    )
  }
  message("Replication data downloaded and extracted successfully.")
} else {
  message("Replication data already exist; skipping download.")
}

dir.create("tables", recursive = TRUE, showWarnings = FALSE)
dir.create("tables/checks",
           recursive = TRUE,
           showWarnings = FALSE)

###############################################################################
# 1. Build all longitudinal source panels
###############################################################################

################ 1997-98
# REF
data_ref_9798 <- read.csv("data/aeis campus info/97-98/ref/campref.dat", header = FALSE)
layout_ref_9798_lines <- readLines("data/aeis campus info/97-98/ref/campref.lyt")
layout_ref_9798_rows <- grep("^\\s*[0-9]{1,3}\\s+", layout_ref_9798_lines, value = TRUE)
layout_ref_9798_names <- toupper(sub("^\\s*[0-9]{1,3}\\s+(\\S+).*", "\\1", layout_ref_9798_rows))
stopifnot(ncol(data_ref_9798) == length(layout_ref_9798_names))
colnames(data_ref_9798) <- layout_ref_9798_names
data_ref_9798 <- data_ref_9798 |>
  mutate(
    CAMPUS = str_pad(as.character(CAMPUS), 9, "left", "0"),
    DISTRICT = str_pad(as.character(DISTRICT), 6, "left", "0")
  ) |>
  filter(
    is.na(PAIRCAMP) | trimws(as.character(PAIRCAMP)) == "",
    CFLCHART == "N",
    C_RATING %in% c("L", "A", "R", "E")
  ) |>
  transmute(
    campus_number = CAMPUS,
    campus_rating = C_RATING,
    grade_span = GRDSPAN,
    campus_type = GRDTYPE
  )
head(data_ref_9798)

# STUD
data_stud_9798 <- read.csv("data/aeis campus info/97-98/stud/campstud.dat", header = FALSE)
layout_stud_9798_lines <- readLines("data/aeis campus info/97-98/stud/campstud.lyt")
layout_stud_9798_rows <- grep("^\\s*[0-9]{1,3}\\s+", layout_stud_9798_lines, value = TRUE)
layout_stud_9798_names <- toupper(sub("^\\s*[0-9]{1,3}\\s+(\\S+).*", "\\1", layout_stud_9798_rows))
stopifnot(ncol(data_stud_9798) == length(layout_stud_9798_names))
colnames(data_stud_9798) <- layout_stud_9798_names
data_stud_9798 <- data_stud_9798 |>
  transmute(
    campus_number = str_pad(as.character(CAMPUS), 9, "left", "0"),
    campus_total_students = CPETALLC,
    campus_asian_students = CPETPACC,
    campus_black_students = CPETBLAC,
    campus_hispanic_students = CPETHISC,
    campus_ecodis_students = CPETECOC,
    campus_lep_students = CPETLEPC,
    campus_sped_students = CPETSPEC,
    campus_giftal_students = CPETGIFC,
    campus_white_students = CPETWHIC,
    campus_native_american_students = CPETINDC,
    campus_voc_students = CPETVOCC,
    campus_grade_EE_students = CPETGEEC,
    campus_grade_PK_students = CPETGPKC,
    campus_grade_KG_students = CPETGKNC,
    campus_grade_01_students = CPETG01C,
    campus_grade_02_students = CPETG02C,
    campus_grade_03_students = CPETG03C,
    campus_grade_04_students = CPETG04C,
    campus_grade_05_students = CPETG05C,
    campus_grade_06_students = CPETG06C,
    campus_grade_07_students = CPETG07C,
    campus_grade_08_students = CPETG08C,
    campus_grade_09_students = CPETG09C,
    campus_grade_10_students = CPETG10C,
    campus_grade_11_students = CPETG11C,
    campus_grade_12_students = CPETG12C
  )
head(data_stud_9798)

# STAF
data_staf_9798 <- read.csv(
  "data/aeis campus info/97-98/staf/campstaf.dat",
  header = FALSE,
  check.names = FALSE
)
layout_staf_9798 <- readLines("data/aeis campus info/97-98/staf/campstaf.lyt")
var_rows_layout_staf_9798 <- grep("^\\s*[0-9]{1,3}\\s+", layout_staf_9798, value = TRUE)
layout_staf_9798 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_staf_9798),
  NAME = toupper(
    sub(
      "^\\s*[0-9]{1,3}\\s+(\\S+).*",
      "\\1",
      var_rows_layout_staf_9798
    )
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_staf_9798
    )
  )
)
stopifnot(ncol(data_staf_9798) == nrow(layout_staf_9798))
colnames(data_staf_9798) <- layout_staf_9798$NAME
data_staf_9798 <- data_staf_9798 |>
  transmute(
    campus_number = str_pad(as.character(CAMPUS), 9, "left", "0"),
    teacher_fte = as.numeric(na_if(as.character(CPSTTOFC), ".")),
    aide_fte = as.numeric(na_if(as.character(CPSETOFC), ".")),
    beginning_teacher_fte = as.numeric(na_if(as.character(CPST00FC), ".")),
    teacher_1_5_fte = as.numeric(na_if(as.character(CPST01FC), ".")),
    teacher_6_10_fte = as.numeric(na_if(as.character(CPST06FC), ".")),
    teacher_11_20_fte = as.numeric(na_if(as.character(CPST11FC), ".")),
    teacher_over_20_fte = as.numeric(na_if(as.character(CPST20FC), ".")),
    teacher_experience = as.numeric(na_if(as.character(CPSTEXPA), ".")),
    year = 1998L
  )
stopifnot(!anyDuplicated(data_staf_9798$campus_number))
head(data_staf_9798)

################ 1998-99
# REF
data_ref_9899 <- read.csv("data/aeis campus info/98-99/ref/campref.dat", header = FALSE)
layout_ref_9899 <- readLines("data/aeis campus info/98-99/ref/campref.lyt")
var_rows_layout_ref_9899 <- grep("^\\s*[0-9]{1,3}\\s+", layout_ref_9899, value = TRUE)
layout_ref_9899 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_ref_9899),
  NAME = sub(
    "^\\s*[0-9]{1,3}\\s+(\\S+).*",
    "\\1",
    var_rows_layout_ref_9899
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_ref_9899
    )
  )
)
stopifnot(ncol(data_ref_9899) == nrow(layout_ref_9899))
colnames(data_ref_9899) <- toupper(layout_ref_9899$NAME)
data_ref_9899$CAMPUS <- str_pad(
  as.character(data_ref_9899$CAMPUS),
  width = 9,
  side = "left",
  pad = "0"
)
data_ref_9899$DISTRICT <- str_pad(
  as.character(data_ref_9899$DISTRICT),
  width = 6,
  side = "left",
  pad = "0"
)
data_ref_9899 <- data_ref_9899 |>
  filter(
    is.na(PAIRCAMP) | trimws(as.character(PAIRCAMP)) == "",
    CFLCHART == "N",
    C_RATING %in% c("L", "A", "R", "E")
  ) |>
  rename(
    campus_number = CAMPUS,
    campus_rating = C_RATING,
    grade_span = GRDSPAN,
    campus_type = GRDTYPE
  ) |>
  select(campus_number, campus_rating, grade_span, campus_type)
head(data_ref_9899)

# STUD
data_stud_9899 <- read.csv("data/aeis campus info/98-99/stud/campstud.dat", header = FALSE)
layout_stud_9899 <- readLines("data/aeis campus info/98-99/stud/campstud.lyt")
var_rows_layout_stud_9899 <- grep("^\\s*[0-9]{1,3}\\s+", layout_stud_9899, value = TRUE)
layout_stud_9899 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_stud_9899),
  NAME = sub(
    "^\\s*[0-9]{1,3}\\s+(\\S+).*",
    "\\1",
    var_rows_layout_stud_9899
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_stud_9899
    )
  )
)
stopifnot(ncol(data_stud_9899) == nrow(layout_stud_9899))
colnames(data_stud_9899) <- toupper(layout_stud_9899$NAME)
data_stud_9899$CAMPUS <- str_pad(
  as.character(data_stud_9899$CAMPUS),
  width = 9,
  side = "left",
  pad = "0"
)
data_stud_9899 <- data_stud_9899 |>
  rename(
    campus_number = CAMPUS,
    campus_total_students = CPETALLC,
    campus_asian_students = CPETPACC,
    campus_black_students = CPETBLAC,
    campus_hispanic_students = CPETHISC,
    campus_ecodis_students = CPETECOC,
    campus_lep_students = CPETLEPC,
    campus_sped_students = CPETSPEC,
    campus_giftal_students = CPETGIFC,
    campus_white_students = CPETWHIC,
    campus_native_american_students = CPETINDC,
    campus_voc_students = CPETVOCC,
    campus_grade_EE_students = CPETGEEC,
    campus_grade_PK_students = CPETGPKC,
    campus_grade_KG_students = CPETGKNC,
    campus_grade_01_students = CPETG01C,
    campus_grade_02_students = CPETG02C,
    campus_grade_03_students = CPETG03C,
    campus_grade_04_students = CPETG04C,
    campus_grade_05_students = CPETG05C,
    campus_grade_06_students = CPETG06C,
    campus_grade_07_students = CPETG07C,
    campus_grade_08_students = CPETG08C,
    campus_grade_09_students = CPETG09C,
    campus_grade_10_students = CPETG10C,
    campus_grade_11_students = CPETG11C,
    campus_grade_12_students = CPETG12C
  ) |>
  select(
    campus_number,
    campus_total_students,
    campus_asian_students,
    campus_black_students,
    campus_hispanic_students,
    campus_ecodis_students,
    campus_lep_students,
    campus_sped_students,
    campus_giftal_students,
    campus_white_students,
    campus_native_american_students,
    campus_voc_students,
    campus_grade_EE_students,
    campus_grade_PK_students,
    campus_grade_KG_students,
    campus_grade_01_students,
    campus_grade_02_students,
    campus_grade_03_students,
    campus_grade_04_students,
    campus_grade_05_students,
    campus_grade_06_students,
    campus_grade_07_students,
    campus_grade_08_students,
    campus_grade_09_students,
    campus_grade_10_students,
    campus_grade_11_students,
    campus_grade_12_students
  )
head(data_stud_9899)

# STAF
data_staf_9899 <- read.csv(
  "data/aeis campus info/98-99/staf/campstaf.dat",
  header = FALSE,
  check.names = FALSE
)
layout_staf_9899 <- readLines("data/aeis campus info/98-99/staf/campstaf.lyt")
var_rows_layout_staf_9899 <- grep("^\\s*[0-9]{1,3}\\s+", layout_staf_9899, value = TRUE)
layout_staf_9899 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_staf_9899),
  NAME = toupper(
    sub(
      "^\\s*[0-9]{1,3}\\s+(\\S+).*",
      "\\1",
      var_rows_layout_staf_9899
    )
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_staf_9899
    )
  )
)
stopifnot(ncol(data_staf_9899) == nrow(layout_staf_9899))
colnames(data_staf_9899) <- layout_staf_9899$NAME
data_staf_9899 <- data_staf_9899 |>
  transmute(
    campus_number = str_pad(as.character(CAMPUS), 9, "left", "0"),
    teacher_fte = as.numeric(na_if(as.character(CPSTTOFC), ".")),
    aide_fte = as.numeric(na_if(as.character(CPSETOFC), ".")),
    beginning_teacher_fte = as.numeric(na_if(as.character(CPST00FC), ".")),
    teacher_1_5_fte = as.numeric(na_if(as.character(CPST01FC), ".")),
    teacher_6_10_fte = as.numeric(na_if(as.character(CPST06FC), ".")),
    teacher_11_20_fte = as.numeric(na_if(as.character(CPST11FC), ".")),
    teacher_over_20_fte = as.numeric(na_if(as.character(CPST20FC), ".")),
    teacher_experience = as.numeric(na_if(as.character(CPSTEXPA), ".")),
    year = 1999L
  )
stopifnot(!anyDuplicated(data_staf_9899$campus_number))
head(data_staf_9899)

################ 1999-00
# REF
data_ref_9900 <- read.csv("data/aeis campus info/99-20/ref/campref.dat", header = FALSE)
layout_ref_9900 <- readLines("data/aeis campus info/99-20/ref/campref.lyt")
var_rows_layout_ref_9900 <- grep("^\\s*[0-9]{1,3}\\s+", layout_ref_9900, value = TRUE)
layout_ref_9900 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_ref_9900),
  NAME = sub(
    "^\\s*[0-9]{1,3}\\s+(\\S+).*",
    "\\1",
    var_rows_layout_ref_9900
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_ref_9900
    )
  )
)
stopifnot(ncol(data_ref_9900) == nrow(layout_ref_9900))
colnames(data_ref_9900) <- toupper(layout_ref_9900$NAME)
data_ref_9900$CAMPUS <- str_pad(
  as.character(data_ref_9900$CAMPUS),
  width = 9,
  side = "left",
  pad = "0"
)
data_ref_9900$DISTRICT <- str_pad(
  as.character(data_ref_9900$DISTRICT),
  width = 6,
  side = "left",
  pad = "0"
)
data_ref_9900 <- data_ref_9900 |>
  filter(
    is.na(PAIRCAMP) | trimws(as.character(PAIRCAMP)) == "",
    CFLCHART == "N",
    C_RATING %in% c("L", "A", "R", "E")
  ) |>
  rename(
    campus_number = CAMPUS,
    campus_rating = C_RATING,
    grade_span = GRDSPAN,
    campus_type = GRDTYPE
  ) |>
  select(campus_number, campus_rating, grade_span, campus_type)
head(data_ref_9900)

# STUD
data_stud_9900 <- read.csv("data/aeis campus info/99-20/stud/campstud.dat", header = FALSE)
layout_stud_9900 <- readLines("data/aeis campus info/99-20/stud/campstud.lyt")
var_rows_layout_stud_9900 <- grep("^\\s*[0-9]{1,3}\\s+", layout_stud_9900, value = TRUE)
layout_stud_9900 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_stud_9900),
  NAME = sub(
    "^\\s*[0-9]{1,3}\\s+(\\S+).*",
    "\\1",
    var_rows_layout_stud_9900
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_stud_9900
    )
  )
)
stopifnot(ncol(data_stud_9900) == nrow(layout_stud_9900))
colnames(data_stud_9900) <- toupper(layout_stud_9900$NAME)
data_stud_9900$CAMPUS <- str_pad(
  as.character(data_stud_9900$CAMPUS),
  width = 9,
  side = "left",
  pad = "0"
)
data_stud_9900 <- data_stud_9900 |>
  rename(
    campus_number = CAMPUS,
    campus_total_students = CPETALLC,
    campus_asian_students = CPETPACC,
    campus_black_students = CPETBLAC,
    campus_hispanic_students = CPETHISC,
    campus_ecodis_students = CPETECOC,
    campus_lep_students = CPETLEPC,
    campus_sped_students = CPETSPEC,
    campus_giftal_students = CPETGIFC,
    campus_white_students = CPETWHIC,
    campus_native_american_students = CPETINDC,
    campus_voc_students = CPETVOCC,
    campus_grade_EE_students = CPETGEEC,
    campus_grade_PK_students = CPETGPKC,
    campus_grade_KG_students = CPETGKNC,
    campus_grade_01_students = CPETG01C,
    campus_grade_02_students = CPETG02C,
    campus_grade_03_students = CPETG03C,
    campus_grade_04_students = CPETG04C,
    campus_grade_05_students = CPETG05C,
    campus_grade_06_students = CPETG06C,
    campus_grade_07_students = CPETG07C,
    campus_grade_08_students = CPETG08C,
    campus_grade_09_students = CPETG09C,
    campus_grade_10_students = CPETG10C,
    campus_grade_11_students = CPETG11C,
    campus_grade_12_students = CPETG12C
  ) |>
  select(
    campus_number,
    campus_total_students,
    campus_asian_students,
    campus_black_students,
    campus_hispanic_students,
    campus_ecodis_students,
    campus_lep_students,
    campus_sped_students,
    campus_giftal_students,
    campus_white_students,
    campus_native_american_students,
    campus_voc_students,
    campus_grade_EE_students,
    campus_grade_PK_students,
    campus_grade_KG_students,
    campus_grade_01_students,
    campus_grade_02_students,
    campus_grade_03_students,
    campus_grade_04_students,
    campus_grade_05_students,
    campus_grade_06_students,
    campus_grade_07_students,
    campus_grade_08_students,
    campus_grade_09_students,
    campus_grade_10_students,
    campus_grade_11_students,
    campus_grade_12_students
  )
head(data_stud_9900)

# STAF
data_staf_9900 <- read.csv(
  "data/aeis campus info/99-20/staf/campstaf.dat",
  header = FALSE,
  check.names = FALSE
)
layout_staf_9900 <- readLines("data/aeis campus info/99-20/staf/campstaf.lyt")
var_rows_layout_staf_9900 <- grep("^\\s*[0-9]{1,3}\\s+", layout_staf_9900, value = TRUE)
layout_staf_9900 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_staf_9900),
  NAME = toupper(
    sub(
      "^\\s*[0-9]{1,3}\\s+(\\S+).*",
      "\\1",
      var_rows_layout_staf_9900
    )
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_staf_9900
    )
  )
)
stopifnot(ncol(data_staf_9900) == nrow(layout_staf_9900))
colnames(data_staf_9900) <- layout_staf_9900$NAME
data_staf_9900 <- data_staf_9900 |>
  transmute(
    campus_number = str_pad(as.character(CAMPUS), 9, "left", "0"),
    teacher_fte = as.numeric(na_if(as.character(CPSTTOFC), ".")),
    aide_fte = as.numeric(na_if(as.character(CPSETOFC), ".")),
    beginning_teacher_fte = as.numeric(na_if(as.character(CPST00FC), ".")),
    teacher_1_5_fte = as.numeric(na_if(as.character(CPST01FC), ".")),
    teacher_6_10_fte = as.numeric(na_if(as.character(CPST06FC), ".")),
    teacher_11_20_fte = as.numeric(na_if(as.character(CPST11FC), ".")),
    teacher_over_20_fte = as.numeric(na_if(as.character(CPST20FC), ".")),
    teacher_experience = as.numeric(na_if(as.character(CPSTEXPA), ".")),
    year = 2000L
  )
stopifnot(!anyDuplicated(data_staf_9900$campus_number))
head(data_staf_9900)

################ 2000-01
# REF
data_ref_0001 <- read.csv("data/aeis campus info/20-01/ref/campref.dat", header = FALSE)
layout_ref_0001 <- readLines("data/aeis campus info/20-01/ref/campref.lyt")
var_rows_layout_ref_0001 <- grep("^\\s*[0-9]{1,3}\\s+", layout_ref_0001, value = TRUE)
layout_ref_0001 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_ref_0001),
  NAME = sub(
    "^\\s*[0-9]{1,3}\\s+(\\S+).*",
    "\\1",
    var_rows_layout_ref_0001
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_ref_0001
    )
  )
)
stopifnot(ncol(data_ref_0001) == nrow(layout_ref_0001))
colnames(data_ref_0001) <- toupper(layout_ref_0001$NAME)
data_ref_0001$CAMPUS <- str_pad(
  as.character(data_ref_0001$CAMPUS),
  width = 9,
  side = "left",
  pad = "0"
)
data_ref_0001$DISTRICT <- str_pad(
  as.character(data_ref_0001$DISTRICT),
  width = 6,
  side = "left",
  pad = "0"
)
data_ref_0001 <- data_ref_0001 |>
  filter(
    is.na(PAIRCAMP) | trimws(as.character(PAIRCAMP)) == "",
    CFLCHART == "N",
    C_RATING %in% c("L", "A", "R", "E")
  ) |>
  rename(
    campus_number = CAMPUS,
    campus_rating = C_RATING,
    grade_span = GRDSPAN,
    campus_type = GRDTYPE
  ) |>
  select(campus_number, campus_rating, grade_span, campus_type)
head(data_ref_0001)

# STUD
data_stud_0001 <- read.csv("data/aeis campus info/20-01/stud/campstud.dat", header = FALSE)
layout_stud_0001 <- readLines("data/aeis campus info/20-01/stud/campstud.lyt")
var_rows_layout_stud_0001 <- grep("^\\s*[0-9]{1,3}\\s+", layout_stud_0001, value = TRUE)
layout_stud_0001 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_stud_0001),
  NAME = sub(
    "^\\s*[0-9]{1,3}\\s+(\\S+).*",
    "\\1",
    var_rows_layout_stud_0001
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_stud_0001
    )
  )
)
stopifnot(ncol(data_stud_0001) == nrow(layout_stud_0001))
colnames(data_stud_0001) <- toupper(layout_stud_0001$NAME)
data_stud_0001$CAMPUS <- str_pad(
  as.character(data_stud_0001$CAMPUS),
  width = 9,
  side = "left",
  pad = "0"
)
data_stud_0001 <- data_stud_0001 |>
  rename(
    campus_number = CAMPUS,
    campus_total_students = CPETALLC,
    campus_asian_students = CPETPACC,
    campus_black_students = CPETBLAC,
    campus_hispanic_students = CPETHISC,
    campus_ecodis_students = CPETECOC,
    campus_lep_students = CPETLEPC,
    campus_sped_students = CPETSPEC,
    campus_giftal_students = CPETGIFC,
    campus_white_students = CPETWHIC,
    campus_native_american_students = CPETINDC,
    campus_voc_students = CPETVOCC,
    campus_grade_EE_students = CPETGEEC,
    campus_grade_PK_students = CPETGPKC,
    campus_grade_KG_students = CPETGKNC,
    campus_grade_01_students = CPETG01C,
    campus_grade_02_students = CPETG02C,
    campus_grade_03_students = CPETG03C,
    campus_grade_04_students = CPETG04C,
    campus_grade_05_students = CPETG05C,
    campus_grade_06_students = CPETG06C,
    campus_grade_07_students = CPETG07C,
    campus_grade_08_students = CPETG08C,
    campus_grade_09_students = CPETG09C,
    campus_grade_10_students = CPETG10C,
    campus_grade_11_students = CPETG11C,
    campus_grade_12_students = CPETG12C
  ) |>
  select(
    campus_number,
    campus_total_students,
    campus_asian_students,
    campus_black_students,
    campus_hispanic_students,
    campus_ecodis_students,
    campus_lep_students,
    campus_sped_students,
    campus_giftal_students,
    campus_white_students,
    campus_native_american_students,
    campus_voc_students,
    campus_grade_EE_students,
    campus_grade_PK_students,
    campus_grade_KG_students,
    campus_grade_01_students,
    campus_grade_02_students,
    campus_grade_03_students,
    campus_grade_04_students,
    campus_grade_05_students,
    campus_grade_06_students,
    campus_grade_07_students,
    campus_grade_08_students,
    campus_grade_09_students,
    campus_grade_10_students,
    campus_grade_11_students,
    campus_grade_12_students
  )
head(data_stud_0001)

# STAF
data_staf_0001 <- read.csv(
  "data/aeis campus info/20-01/staf/campstaf.dat",
  header = FALSE,
  check.names = FALSE
)
layout_staf_0001 <- readLines("data/aeis campus info/20-01/staf/campstaf.lyt")
var_rows_layout_staf_0001 <- grep("^\\s*[0-9]{1,3}\\s+", layout_staf_0001, value = TRUE)
layout_staf_0001 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_staf_0001),
  NAME = toupper(
    sub(
      "^\\s*[0-9]{1,3}\\s+(\\S+).*",
      "\\1",
      var_rows_layout_staf_0001
    )
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_staf_0001
    )
  )
)
stopifnot(ncol(data_staf_0001) == nrow(layout_staf_0001))
colnames(data_staf_0001) <- layout_staf_0001$NAME
data_staf_0001 <- data_staf_0001 |>
  transmute(
    campus_number = str_pad(as.character(CAMPUS), 9, "left", "0"),
    teacher_fte = as.numeric(na_if(as.character(CPSTTOFC), ".")),
    aide_fte = as.numeric(na_if(as.character(CPSETOFC), ".")),
    beginning_teacher_fte = as.numeric(na_if(as.character(CPST00FC), ".")),
    teacher_1_5_fte = as.numeric(na_if(as.character(CPST01FC), ".")),
    teacher_6_10_fte = as.numeric(na_if(as.character(CPST06FC), ".")),
    teacher_11_20_fte = as.numeric(na_if(as.character(CPST11FC), ".")),
    teacher_over_20_fte = as.numeric(na_if(as.character(CPST20FC), ".")),
    teacher_experience = as.numeric(na_if(as.character(CPSTEXPA), ".")),
    year = 2001L
  )
stopifnot(!anyDuplicated(data_staf_0001$campus_number))
head(data_staf_0001)

################ 2001-02
# REF
data_ref_0102 <- read.csv("data/aeis campus info/01-02/ref/campref.dat", header = FALSE)
layout_ref_0102 <- readLines("data/aeis campus info/01-02/ref/campref.lyt")
var_rows_layout_ref_0102 <- grep("^\\s*[0-9]{1,3}\\s+", layout_ref_0102, value = TRUE)
layout_ref_0102 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_ref_0102),
  NAME = sub(
    "^\\s*[0-9]{1,3}\\s+(\\S+).*",
    "\\1",
    var_rows_layout_ref_0102
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_ref_0102
    )
  )
)
stopifnot(ncol(data_ref_0102) == nrow(layout_ref_0102))
colnames(data_ref_0102) <- toupper(layout_ref_0102$NAME)
data_ref_0102$CAMPUS <- str_pad(
  as.character(data_ref_0102$CAMPUS),
  width = 9,
  side = "left",
  pad = "0"
)
data_ref_0102$DISTRICT <- str_pad(
  as.character(data_ref_0102$DISTRICT),
  width = 6,
  side = "left",
  pad = "0"
)
data_ref_0102 <- data_ref_0102 |>
  filter(
    is.na(PAIRCAMP) | trimws(as.character(PAIRCAMP)) == "",
    CFLCHART == "N",
    C_RATING %in% c("L", "A", "R", "E")
  ) |>
  rename(
    campus_number = CAMPUS,
    campus_rating = C_RATING,
    grade_span = GRDSPAN,
    campus_type = GRDTYPE
  ) |>
  select(campus_number, campus_rating, grade_span, campus_type)
head(data_ref_0102)

# STUD
data_stud_0102 <- read.csv("data/aeis campus info/01-02/stud/campstud.dat", header = FALSE)
layout_stud_0102 <- readLines("data/aeis campus info/01-02/stud/campstud.lyt")
var_rows_layout_stud_0102 <- grep("^\\s*[0-9]{1,3}\\s+", layout_stud_0102, value = TRUE)
layout_stud_0102 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_stud_0102),
  NAME = sub(
    "^\\s*[0-9]{1,3}\\s+(\\S+).*",
    "\\1",
    var_rows_layout_stud_0102
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_stud_0102
    )
  )
)
stopifnot(ncol(data_stud_0102) == nrow(layout_stud_0102))
colnames(data_stud_0102) <- toupper(layout_stud_0102$NAME)
data_stud_0102$CAMPUS <- str_pad(
  as.character(data_stud_0102$CAMPUS),
  width = 9,
  side = "left",
  pad = "0"
)
data_stud_0102 <- data_stud_0102 |>
  rename(
    campus_number = CAMPUS,
    campus_total_students = CPETALLC,
    campus_asian_students = CPETPACC,
    campus_black_students = CPETBLAC,
    campus_hispanic_students = CPETHISC,
    campus_ecodis_students = CPETECOC,
    campus_lep_students = CPETLEPC,
    campus_sped_students = CPETSPEC,
    campus_giftal_students = CPETGIFC,
    campus_white_students = CPETWHIC,
    campus_native_american_students = CPETINDC,
    campus_voc_students = CPETVOCC,
    campus_grade_EE_students = CPETGEEC,
    campus_grade_PK_students = CPETGPKC,
    campus_grade_KG_students = CPETGKNC,
    campus_grade_01_students = CPETG01C,
    campus_grade_02_students = CPETG02C,
    campus_grade_03_students = CPETG03C,
    campus_grade_04_students = CPETG04C,
    campus_grade_05_students = CPETG05C,
    campus_grade_06_students = CPETG06C,
    campus_grade_07_students = CPETG07C,
    campus_grade_08_students = CPETG08C,
    campus_grade_09_students = CPETG09C,
    campus_grade_10_students = CPETG10C,
    campus_grade_11_students = CPETG11C,
    campus_grade_12_students = CPETG12C
  ) |>
  select(
    campus_number,
    campus_total_students,
    campus_asian_students,
    campus_black_students,
    campus_hispanic_students,
    campus_ecodis_students,
    campus_lep_students,
    campus_sped_students,
    campus_giftal_students,
    campus_white_students,
    campus_native_american_students,
    campus_voc_students,
    campus_grade_EE_students,
    campus_grade_PK_students,
    campus_grade_KG_students,
    campus_grade_01_students,
    campus_grade_02_students,
    campus_grade_03_students,
    campus_grade_04_students,
    campus_grade_05_students,
    campus_grade_06_students,
    campus_grade_07_students,
    campus_grade_08_students,
    campus_grade_09_students,
    campus_grade_10_students,
    campus_grade_11_students,
    campus_grade_12_students
  )
head(data_stud_0102)

# STAF
data_staf_0102 <- read.csv(
  "data/aeis campus info/01-02/staf/campstaf.dat",
  header = FALSE,
  check.names = FALSE
)
layout_staf_0102 <- readLines("data/aeis campus info/01-02/staf/campstaf.lyt")
var_rows_layout_staf_0102 <- grep("^\\s*[0-9]{1,3}\\s+", layout_staf_0102, value = TRUE)
layout_staf_0102 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_staf_0102),
  NAME = toupper(
    sub(
      "^\\s*[0-9]{1,3}\\s+(\\S+).*",
      "\\1",
      var_rows_layout_staf_0102
    )
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_staf_0102
    )
  )
)
stopifnot(ncol(data_staf_0102) == nrow(layout_staf_0102))
colnames(data_staf_0102) <- layout_staf_0102$NAME
data_staf_0102 <- data_staf_0102 |>
  transmute(
    campus_number = str_pad(as.character(CAMPUS), 9, "left", "0"),
    teacher_fte = as.numeric(na_if(as.character(CPSTTOFC), ".")),
    aide_fte = as.numeric(na_if(as.character(CPSETOFC), ".")),
    beginning_teacher_fte = as.numeric(na_if(as.character(CPST00FC), ".")),
    teacher_1_5_fte = as.numeric(na_if(as.character(CPST01FC), ".")),
    teacher_6_10_fte = as.numeric(na_if(as.character(CPST06FC), ".")),
    teacher_11_20_fte = as.numeric(na_if(as.character(CPST11FC), ".")),
    teacher_over_20_fte = as.numeric(na_if(as.character(CPST20FC), ".")),
    teacher_experience = as.numeric(na_if(as.character(CPSTEXPA), ".")),
    year = 2002L
  )
stopifnot(!anyDuplicated(data_staf_0102$campus_number))
head(data_staf_0102)

################ 2002-03
# REF
data_ref_0203 <- read.csv("data/aeis campus info/02-03/ref/campref.dat", header = FALSE)
layout_ref_0203 <- readLines("data/aeis campus info/02-03/ref/campref.lyt")
var_rows_layout_ref_0203 <- grep("^\\s*[0-9]{1,3}\\s+", layout_ref_0203, value = TRUE)
layout_ref_0203 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_ref_0203),
  NAME = sub(
    "^\\s*[0-9]{1,3}\\s+(\\S+).*",
    "\\1",
    var_rows_layout_ref_0203
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_ref_0203
    )
  )
)
stopifnot(ncol(data_ref_0203) == nrow(layout_ref_0203))
colnames(data_ref_0203) <- toupper(layout_ref_0203$NAME)
data_ref_0203$CAMPUS <- str_pad(
  as.character(data_ref_0203$CAMPUS),
  width = 9,
  side = "left",
  pad = "0"
)
data_ref_0203$DISTRICT <- str_pad(
  as.character(data_ref_0203$DISTRICT),
  width = 6,
  side = "left",
  pad = "0"
)
data_ref_0203 <- data_ref_0203 |>
  filter(CFLCHART == "N") |>
  transmute(
    campus_number = CAMPUS,
    campus_rating = NA_character_,
    grade_span = GRDSPAN,
    campus_type = GRDTYPE
  )
head(data_ref_0203)

# STUD
data_stud_0203 <- read.csv("data/aeis campus info/02-03/stud/campstud.dat", header = FALSE)
layout_stud_0203 <- readLines("data/aeis campus info/02-03/stud/campstud.lyt")
var_rows_layout_stud_0203 <- grep("^\\s*[0-9]{1,3}\\s+", layout_stud_0203, value = TRUE)
layout_stud_0203 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_stud_0203),
  NAME = sub(
    "^\\s*[0-9]{1,3}\\s+(\\S+).*",
    "\\1",
    var_rows_layout_stud_0203
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_stud_0203
    )
  )
)
stopifnot(ncol(data_stud_0203) == nrow(layout_stud_0203))
colnames(data_stud_0203) <- toupper(layout_stud_0203$NAME)
data_stud_0203$CAMPUS <- str_pad(
  as.character(data_stud_0203$CAMPUS),
  width = 9,
  side = "left",
  pad = "0"
)
data_stud_0203 <- data_stud_0203 |>
  rename(
    campus_number = CAMPUS,
    campus_total_students = CPETALLC,
    campus_asian_students = CPETPACC,
    campus_black_students = CPETBLAC,
    campus_hispanic_students = CPETHISC,
    campus_ecodis_students = CPETECOC,
    campus_lep_students = CPETLEPC,
    campus_sped_students = CPETSPEC,
    campus_giftal_students = CPETGIFC,
    campus_white_students = CPETWHIC,
    campus_native_american_students = CPETINDC,
    campus_voc_students = CPETVOCC,
    campus_grade_EE_students = CPETGEEC,
    campus_grade_PK_students = CPETGPKC,
    campus_grade_KG_students = CPETGKNC,
    campus_grade_01_students = CPETG01C,
    campus_grade_02_students = CPETG02C,
    campus_grade_03_students = CPETG03C,
    campus_grade_04_students = CPETG04C,
    campus_grade_05_students = CPETG05C,
    campus_grade_06_students = CPETG06C,
    campus_grade_07_students = CPETG07C,
    campus_grade_08_students = CPETG08C,
    campus_grade_09_students = CPETG09C,
    campus_grade_10_students = CPETG10C,
    campus_grade_11_students = CPETG11C,
    campus_grade_12_students = CPETG12C
  ) |>
  select(
    campus_number,
    campus_total_students,
    campus_asian_students,
    campus_black_students,
    campus_hispanic_students,
    campus_ecodis_students,
    campus_lep_students,
    campus_sped_students,
    campus_giftal_students,
    campus_white_students,
    campus_native_american_students,
    campus_voc_students,
    campus_grade_EE_students,
    campus_grade_PK_students,
    campus_grade_KG_students,
    campus_grade_01_students,
    campus_grade_02_students,
    campus_grade_03_students,
    campus_grade_04_students,
    campus_grade_05_students,
    campus_grade_06_students,
    campus_grade_07_students,
    campus_grade_08_students,
    campus_grade_09_students,
    campus_grade_10_students,
    campus_grade_11_students,
    campus_grade_12_students
  )
head(data_stud_0203)

# STAF
data_staf_0203 <- read.csv(
  "data/aeis campus info/02-03/staf/campstaf.dat",
  header = FALSE,
  check.names = FALSE
)
layout_staf_0203 <- readLines("data/aeis campus info/02-03/staf/campstaf.lyt")
var_rows_layout_staf_0203 <- grep("^\\s*[0-9]{1,3}\\s+", layout_staf_0203, value = TRUE)
layout_staf_0203 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_staf_0203),
  NAME = toupper(
    sub(
      "^\\s*[0-9]{1,3}\\s+(\\S+).*",
      "\\1",
      var_rows_layout_staf_0203
    )
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_staf_0203
    )
  )
)
stopifnot(ncol(data_staf_0203) == nrow(layout_staf_0203))
colnames(data_staf_0203) <- layout_staf_0203$NAME
data_staf_0203 <- data_staf_0203 |>
  transmute(
    campus_number = str_pad(as.character(CAMPUS), 9, "left", "0"),
    teacher_fte = as.numeric(na_if(as.character(CPSTTOFC), ".")),
    aide_fte = as.numeric(na_if(as.character(CPSETOFC), ".")),
    beginning_teacher_fte = as.numeric(na_if(as.character(CPST00FC), ".")),
    teacher_1_5_fte = as.numeric(na_if(as.character(CPST01FC), ".")),
    teacher_6_10_fte = as.numeric(na_if(as.character(CPST06FC), ".")),
    teacher_11_20_fte = as.numeric(na_if(as.character(CPST11FC), ".")),
    teacher_over_20_fte = as.numeric(na_if(as.character(CPST20FC), ".")),
    teacher_experience = as.numeric(na_if(as.character(CPSTEXPA), ".")),
    year = 2003L
  )
stopifnot(!anyDuplicated(data_staf_0203$campus_number))
head(data_staf_0203)

################ 2003-04
# REF
data_ref_0304 <- read.csv("data/aeis campus info/03-04/ref/campref.dat", header = FALSE)
layout_ref_0304 <- readLines("data/aeis campus info/03-04/ref/campref.lyt")
var_rows_layout_ref_0304 <- grep("^\\s*[0-9]{1,3}\\s+", layout_ref_0304, value = TRUE)
layout_ref_0304 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_ref_0304),
  NAME = sub(
    "^\\s*[0-9]{1,3}\\s+(\\S+).*",
    "\\1",
    var_rows_layout_ref_0304
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_ref_0304
    )
  )
)
stopifnot(ncol(data_ref_0304) == nrow(layout_ref_0304))
colnames(data_ref_0304) <- toupper(layout_ref_0304$NAME)
data_ref_0304$CAMPUS <- str_pad(
  as.character(data_ref_0304$CAMPUS),
  width = 9,
  side = "left",
  pad = "0"
)
data_ref_0304$DISTRICT <- str_pad(
  as.character(data_ref_0304$DISTRICT),
  width = 6,
  side = "left",
  pad = "0"
)
data_ref_0304 <- data_ref_0304 |>
  filter(
    is.na(PAIRCAMP) | trimws(as.character(PAIRCAMP)) == "",
    CFLCHART == "N",
    C_RATING %in% c("L", "A", "R", "E")
  ) |>
  rename(
    campus_number = CAMPUS,
    campus_rating = C_RATING,
    grade_span = GRDSPAN,
    campus_type = GRDTYPE
  ) |>
  select(campus_number, campus_rating, grade_span, campus_type)
head(data_ref_0304)

# STUD
data_stud_0304 <- read.csv("data/aeis campus info/03-04/stud/campstud.dat", header = FALSE)
layout_stud_0304 <- readLines("data/aeis campus info/03-04/stud/campstud.lyt")
var_rows_layout_stud_0304 <- grep("^\\s*[0-9]{1,3}\\s+", layout_stud_0304, value = TRUE)
layout_stud_0304 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_stud_0304),
  NAME = sub(
    "^\\s*[0-9]{1,3}\\s+(\\S+).*",
    "\\1",
    var_rows_layout_stud_0304
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_stud_0304
    )
  )
)
stopifnot(ncol(data_stud_0304) == nrow(layout_stud_0304))
colnames(data_stud_0304) <- toupper(layout_stud_0304$NAME)
data_stud_0304$CAMPUS <- str_pad(
  as.character(data_stud_0304$CAMPUS),
  width = 9,
  side = "left",
  pad = "0"
)
data_stud_0304 <- data_stud_0304 |>
  rename(
    campus_number = CAMPUS,
    campus_total_students = CPETALLC,
    campus_asian_students = CPETPACC,
    campus_black_students = CPETBLAC,
    campus_hispanic_students = CPETHISC,
    campus_ecodis_students = CPETECOC,
    campus_lep_students = CPETLEPC,
    campus_sped_students = CPETSPEC,
    campus_giftal_students = CPETGIFC,
    campus_white_students = CPETWHIC,
    campus_native_american_students = CPETINDC,
    campus_voc_students = CPETVOCC,
    campus_grade_EE_students = CPETGEEC,
    campus_grade_PK_students = CPETGPKC,
    campus_grade_KG_students = CPETGKNC,
    campus_grade_01_students = CPETG01C,
    campus_grade_02_students = CPETG02C,
    campus_grade_03_students = CPETG03C,
    campus_grade_04_students = CPETG04C,
    campus_grade_05_students = CPETG05C,
    campus_grade_06_students = CPETG06C,
    campus_grade_07_students = CPETG07C,
    campus_grade_08_students = CPETG08C,
    campus_grade_09_students = CPETG09C,
    campus_grade_10_students = CPETG10C,
    campus_grade_11_students = CPETG11C,
    campus_grade_12_students = CPETG12C
  ) |>
  select(
    campus_number,
    campus_total_students,
    campus_asian_students,
    campus_black_students,
    campus_hispanic_students,
    campus_ecodis_students,
    campus_lep_students,
    campus_sped_students,
    campus_giftal_students,
    campus_white_students,
    campus_native_american_students,
    campus_voc_students,
    campus_grade_EE_students,
    campus_grade_PK_students,
    campus_grade_KG_students,
    campus_grade_01_students,
    campus_grade_02_students,
    campus_grade_03_students,
    campus_grade_04_students,
    campus_grade_05_students,
    campus_grade_06_students,
    campus_grade_07_students,
    campus_grade_08_students,
    campus_grade_09_students,
    campus_grade_10_students,
    campus_grade_11_students,
    campus_grade_12_students
  )
head(data_stud_0304)

# STAF
data_staf_0304 <- read.csv(
  "data/aeis campus info/03-04/staf/campstaf.dat",
  header = FALSE,
  check.names = FALSE
)
layout_staf_0304 <- readLines("data/aeis campus info/03-04/staf/campstaf.lyt")
var_rows_layout_staf_0304 <- grep("^\\s*[0-9]{1,3}\\s+", layout_staf_0304, value = TRUE)
layout_staf_0304 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_staf_0304),
  NAME = toupper(
    sub(
      "^\\s*[0-9]{1,3}\\s+(\\S+).*",
      "\\1",
      var_rows_layout_staf_0304
    )
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_staf_0304
    )
  )
)
stopifnot(ncol(data_staf_0304) == nrow(layout_staf_0304))
colnames(data_staf_0304) <- layout_staf_0304$NAME
data_staf_0304 <- data_staf_0304 |>
  transmute(
    campus_number = str_pad(as.character(CAMPUS), 9, "left", "0"),
    teacher_fte = as.numeric(na_if(as.character(CPSTTOFC), ".")),
    aide_fte = as.numeric(na_if(as.character(CPSETOFC), ".")),
    beginning_teacher_fte = as.numeric(na_if(as.character(CPST00FC), ".")),
    teacher_1_5_fte = as.numeric(na_if(as.character(CPST01FC), ".")),
    teacher_6_10_fte = as.numeric(na_if(as.character(CPST06FC), ".")),
    teacher_11_20_fte = as.numeric(na_if(as.character(CPST11FC), ".")),
    teacher_over_20_fte = as.numeric(na_if(as.character(CPST20FC), ".")),
    teacher_experience = as.numeric(na_if(as.character(CPSTEXPA), ".")),
    year = 2004L
  )
stopifnot(!anyDuplicated(data_staf_0304$campus_number))
head(data_staf_0304)

################ 2004-05
# REF
data_ref_0405 <- read.csv("data/aeis campus info/04-05/ref/campref.dat", header = FALSE)
layout_ref_0405 <- readLines("data/aeis campus info/04-05/ref/campref.lyt")
var_rows_layout_ref_0405 <- grep("^\\s*[0-9]{1,3}\\s+", layout_ref_0405, value = TRUE)
layout_ref_0405 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_ref_0405),
  NAME = sub(
    "^\\s*[0-9]{1,3}\\s+(\\S+).*",
    "\\1",
    var_rows_layout_ref_0405
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_ref_0405
    )
  )
)
stopifnot(ncol(data_ref_0405) == nrow(layout_ref_0405))
colnames(data_ref_0405) <- toupper(layout_ref_0405$NAME)
data_ref_0405$CAMPUS <- str_pad(
  as.character(data_ref_0405$CAMPUS),
  width = 9,
  side = "left",
  pad = "0"
)
data_ref_0405$DISTRICT <- str_pad(
  as.character(data_ref_0405$DISTRICT),
  width = 6,
  side = "left",
  pad = "0"
)
data_ref_0405 <- data_ref_0405 |>
  filter(
    is.na(PAIRCAMP) | trimws(as.character(PAIRCAMP)) == "",
    CFLCHART == "N",
    C_RATING %in% c("L", "A", "R", "E")
  ) |>
  rename(
    campus_number = CAMPUS,
    campus_rating = C_RATING,
    grade_span = GRDSPAN,
    campus_type = GRDTYPE
  ) |>
  select(campus_number, campus_rating, grade_span, campus_type)
head(data_ref_0405)

# STUD
data_stud_0405 <- read.csv("data/aeis campus info/04-05/stud/campstud.dat", header = TRUE)
layout_stud_0405 <- readLines("data/aeis campus info/04-05/stud/campstud.lyt")
var_rows_layout_stud_0405 <- grep("^\\s*[0-9]{1,3}\\s+", layout_stud_0405, value = TRUE)
layout_stud_0405 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_stud_0405),
  NAME = sub(
    "^\\s*[0-9]{1,3}\\s+(\\S+).*",
    "\\1",
    var_rows_layout_stud_0405
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_stud_0405
    )
  )
)
stopifnot(ncol(data_stud_0405) == nrow(layout_stud_0405))
colnames(data_stud_0405) <- toupper(layout_stud_0405$NAME)
data_stud_0405$CAMPUS <- str_pad(
  as.character(data_stud_0405$CAMPUS),
  width = 9,
  side = "left",
  pad = "0"
)
data_stud_0405 <- data_stud_0405 |>
  rename(
    campus_number = CAMPUS,
    campus_total_students = CPETALLC,
    campus_asian_students = CPETPACC,
    campus_black_students = CPETBLAC,
    campus_hispanic_students = CPETHISC,
    campus_ecodis_students = CPETECOC,
    campus_lep_students = CPETLEPC,
    campus_sped_students = CPETSPEC,
    campus_giftal_students = CPETGIFC,
    campus_white_students = CPETWHIC,
    campus_native_american_students = CPETINDC,
    campus_voc_students = CPETVOCC,
    campus_grade_EE_students = CPETGEEC,
    campus_grade_PK_students = CPETGPKC,
    campus_grade_KG_students = CPETGKNC,
    campus_grade_01_students = CPETG01C,
    campus_grade_02_students = CPETG02C,
    campus_grade_03_students = CPETG03C,
    campus_grade_04_students = CPETG04C,
    campus_grade_05_students = CPETG05C,
    campus_grade_06_students = CPETG06C,
    campus_grade_07_students = CPETG07C,
    campus_grade_08_students = CPETG08C,
    campus_grade_09_students = CPETG09C,
    campus_grade_10_students = CPETG10C,
    campus_grade_11_students = CPETG11C,
    campus_grade_12_students = CPETG12C
  ) |>
  select(
    campus_number,
    campus_total_students,
    campus_asian_students,
    campus_black_students,
    campus_hispanic_students,
    campus_ecodis_students,
    campus_lep_students,
    campus_sped_students,
    campus_giftal_students,
    campus_white_students,
    campus_native_american_students,
    campus_voc_students,
    campus_grade_EE_students,
    campus_grade_PK_students,
    campus_grade_KG_students,
    campus_grade_01_students,
    campus_grade_02_students,
    campus_grade_03_students,
    campus_grade_04_students,
    campus_grade_05_students,
    campus_grade_06_students,
    campus_grade_07_students,
    campus_grade_08_students,
    campus_grade_09_students,
    campus_grade_10_students,
    campus_grade_11_students,
    campus_grade_12_students
  )
head(data_stud_0405)

# STAF
data_staf_0405 <- read.csv(
  "data/aeis campus info/04-05/staf/campstaf.dat",
  header = FALSE,
  check.names = FALSE
)
layout_staf_0405 <- readLines("data/aeis campus info/04-05/staf/campstaf.lyt")
var_rows_layout_staf_0405 <- grep("^\\s*[0-9]{1,3}\\s+", layout_staf_0405, value = TRUE)
layout_staf_0405 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_staf_0405),
  NAME = toupper(
    sub(
      "^\\s*[0-9]{1,3}\\s+(\\S+).*",
      "\\1",
      var_rows_layout_staf_0405
    )
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_staf_0405
    )
  )
)
stopifnot(ncol(data_staf_0405) == nrow(layout_staf_0405))
colnames(data_staf_0405) <- layout_staf_0405$NAME
data_staf_0405 <- data_staf_0405 |>
  transmute(
    campus_number = str_pad(as.character(CAMPUS), 9, "left", "0"),
    teacher_fte = as.numeric(na_if(as.character(CPSTTOFC), ".")),
    aide_fte = as.numeric(na_if(as.character(CPSETOFC), ".")),
    beginning_teacher_fte = as.numeric(na_if(as.character(CPST00FC), ".")),
    teacher_1_5_fte = as.numeric(na_if(as.character(CPST01FC), ".")),
    teacher_6_10_fte = as.numeric(na_if(as.character(CPST06FC), ".")),
    teacher_11_20_fte = as.numeric(na_if(as.character(CPST11FC), ".")),
    teacher_over_20_fte = as.numeric(na_if(as.character(CPST20FC), ".")),
    teacher_experience = as.numeric(na_if(as.character(CPSTEXPA), ".")),
    year = 2005L
  )
stopifnot(!anyDuplicated(data_staf_0405$campus_number))
head(data_staf_0405)

################ 2005-06
# REF
data_ref_0506 <- read.csv("data/aeis campus info/05-06/ref/campref.dat", header = TRUE)
layout_ref_0506 <- readLines("data/aeis campus info/05-06/ref/campref.lyt")
var_rows_layout_ref_0506 <- grep("^\\s*[0-9]{1,3}\\s+", layout_ref_0506, value = TRUE)
layout_ref_0506 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_ref_0506),
  NAME = sub(
    "^\\s*[0-9]{1,3}\\s+(\\S+).*",
    "\\1",
    var_rows_layout_ref_0506
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_ref_0506
    )
  )
)
stopifnot(ncol(data_ref_0506) == nrow(layout_ref_0506))
colnames(data_ref_0506) <- toupper(layout_ref_0506$NAME)
data_ref_0506$CAMPUS <- str_pad(
  as.character(data_ref_0506$CAMPUS),
  width = 9,
  side = "left",
  pad = "0"
)
data_ref_0506$DISTRICT <- str_pad(
  as.character(data_ref_0506$DISTRICT),
  width = 6,
  side = "left",
  pad = "0"
)
data_ref_0506 <- data_ref_0506 |>
  filter(
    is.na(PAIRCAMP) | trimws(as.character(PAIRCAMP)) == "",
    CFLCHART == "N",
    C_RATING %in% c("L", "A", "R", "E")
  ) |>
  rename(
    campus_number = CAMPUS,
    campus_rating = C_RATING,
    grade_span = GRDSPAN,
    campus_type = GRDTYPE
  ) |>
  select(campus_number, campus_rating, grade_span, campus_type)
head(data_ref_0506)

# STUD
data_stud_0506 <- read.csv("data/aeis campus info/05-06/stud/campstud.dat", header = TRUE)
layout_stud_0506 <- readLines("data/aeis campus info/05-06/stud/campstud.lyt")
var_rows_layout_stud_0506 <- grep("^\\s*[0-9]{1,3}\\s+", layout_stud_0506, value = TRUE)
layout_stud_0506 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_stud_0506),
  NAME = sub(
    "^\\s*[0-9]{1,3}\\s+(\\S+).*",
    "\\1",
    var_rows_layout_stud_0506
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_stud_0506
    )
  )
)
stopifnot(ncol(data_stud_0506) == nrow(layout_stud_0506))
colnames(data_stud_0506) <- toupper(layout_stud_0506$NAME)
data_stud_0506$CAMPUS <- str_pad(
  as.character(data_stud_0506$CAMPUS),
  width = 9,
  side = "left",
  pad = "0"
)
data_stud_0506 <- data_stud_0506 |>
  rename(
    campus_number = CAMPUS,
    campus_total_students = CPETALLC,
    campus_asian_students = CPETPACC,
    campus_black_students = CPETBLAC,
    campus_hispanic_students = CPETHISC,
    campus_ecodis_students = CPETECOC,
    campus_lep_students = CPETLEPC,
    campus_sped_students = CPETSPEC,
    campus_giftal_students = CPETGIFC,
    campus_white_students = CPETWHIC,
    campus_native_american_students = CPETINDC,
    campus_voc_students = CPETVOCC,
    campus_grade_EE_students = CPETGEEC,
    campus_grade_PK_students = CPETGPKC,
    campus_grade_KG_students = CPETGKNC,
    campus_grade_01_students = CPETG01C,
    campus_grade_02_students = CPETG02C,
    campus_grade_03_students = CPETG03C,
    campus_grade_04_students = CPETG04C,
    campus_grade_05_students = CPETG05C,
    campus_grade_06_students = CPETG06C,
    campus_grade_07_students = CPETG07C,
    campus_grade_08_students = CPETG08C,
    campus_grade_09_students = CPETG09C,
    campus_grade_10_students = CPETG10C,
    campus_grade_11_students = CPETG11C,
    campus_grade_12_students = CPETG12C
  ) |>
  select(
    campus_number,
    campus_total_students,
    campus_asian_students,
    campus_black_students,
    campus_hispanic_students,
    campus_ecodis_students,
    campus_lep_students,
    campus_sped_students,
    campus_giftal_students,
    campus_white_students,
    campus_native_american_students,
    campus_voc_students,
    campus_grade_EE_students,
    campus_grade_PK_students,
    campus_grade_KG_students,
    campus_grade_01_students,
    campus_grade_02_students,
    campus_grade_03_students,
    campus_grade_04_students,
    campus_grade_05_students,
    campus_grade_06_students,
    campus_grade_07_students,
    campus_grade_08_students,
    campus_grade_09_students,
    campus_grade_10_students,
    campus_grade_11_students,
    campus_grade_12_students
  )
head(data_stud_0506)

# STAF
data_staf_0506 <- read.csv(
  "data/aeis campus info/05-06/staf/campstaf.dat",
  header = TRUE,
  check.names = FALSE
)
layout_staf_0506 <- readLines("data/aeis campus info/05-06/staf/campstaf.lyt")
var_rows_layout_staf_0506 <- grep("^\\s*[0-9]{1,3}\\s+", layout_staf_0506, value = TRUE)
layout_staf_0506 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_staf_0506),
  NAME = toupper(
    sub(
      "^\\s*[0-9]{1,3}\\s+(\\S+).*",
      "\\1",
      var_rows_layout_staf_0506
    )
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_staf_0506
    )
  )
)
stopifnot(ncol(data_staf_0506) == nrow(layout_staf_0506))
stopifnot(identical(toupper(colnames(data_staf_0506)), layout_staf_0506$NAME))
colnames(data_staf_0506) <- layout_staf_0506$NAME
data_staf_0506 <- data_staf_0506 |>
  transmute(
    campus_number = str_pad(as.character(CAMPUS), 9, "left", "0"),
    teacher_fte = as.numeric(na_if(as.character(CPSTTOFC), ".")),
    aide_fte = as.numeric(na_if(as.character(CPSETOFC), ".")),
    beginning_teacher_fte = as.numeric(na_if(as.character(CPST00FC), ".")),
    teacher_1_5_fte = as.numeric(na_if(as.character(CPST01FC), ".")),
    teacher_6_10_fte = as.numeric(na_if(as.character(CPST06FC), ".")),
    teacher_11_20_fte = as.numeric(na_if(as.character(CPST11FC), ".")),
    teacher_over_20_fte = as.numeric(na_if(as.character(CPST20FC), ".")),
    teacher_experience = as.numeric(na_if(as.character(CPSTEXPA), ".")),
    year = 2006L
  )
stopifnot(!anyDuplicated(data_staf_0506$campus_number))
head(data_staf_0506)

################ 2006-07
# REF
data_ref_0607 <- read.csv("data/aeis campus info/06-07/ref/campref.dat", header = TRUE)
layout_ref_0607 <- readLines("data/aeis campus info/06-07/ref/campref.lyt")
var_rows_layout_ref_0607 <- grep("^\\s*[0-9]{1,3}\\s+", layout_ref_0607, value = TRUE)
layout_ref_0607 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_ref_0607),
  NAME = sub(
    "^\\s*[0-9]{1,3}\\s+(\\S+).*",
    "\\1",
    var_rows_layout_ref_0607
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_ref_0607
    )
  )
)
stopifnot(ncol(data_ref_0607) == nrow(layout_ref_0607))
colnames(data_ref_0607) <- toupper(layout_ref_0607$NAME)
data_ref_0607$CAMPUS <- str_pad(
  as.character(data_ref_0607$CAMPUS),
  width = 9,
  side = "left",
  pad = "0"
)
data_ref_0607$DISTRICT <- str_pad(
  as.character(data_ref_0607$DISTRICT),
  width = 6,
  side = "left",
  pad = "0"
)
data_ref_0607 <- data_ref_0607 |>
  filter(
    is.na(PAIRCAMP) | trimws(as.character(PAIRCAMP)) == "",
    CFLCHART == "N",
    C_RATING %in% c("L", "A", "R", "E")
  ) |>
  rename(
    campus_number = CAMPUS,
    campus_rating = C_RATING,
    grade_span = GRDSPAN,
    campus_type = GRDTYPE
  ) |>
  select(campus_number, campus_rating, grade_span, campus_type)
head(data_ref_0607)

# STUD
data_stud_0607 <- read.csv("data/aeis campus info/06-07/stud/campstud.dat", header = TRUE)
layout_stud_0607 <- readLines("data/aeis campus info/06-07/stud/campstud.lyt")
var_rows_layout_stud_0607 <- grep("^\\s*[0-9]{1,3}\\s+", layout_stud_0607, value = TRUE)
layout_stud_0607 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_stud_0607),
  NAME = sub(
    "^\\s*[0-9]{1,3}\\s+(\\S+).*",
    "\\1",
    var_rows_layout_stud_0607
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_stud_0607
    )
  )
)
stopifnot(ncol(data_stud_0607) == nrow(layout_stud_0607))
colnames(data_stud_0607) <- toupper(layout_stud_0607$NAME)
data_stud_0607$CAMPUS <- str_pad(
  as.character(data_stud_0607$CAMPUS),
  width = 9,
  side = "left",
  pad = "0"
)
data_stud_0607 <- data_stud_0607 |>
  rename(
    campus_number = CAMPUS,
    campus_total_students = CPETALLC,
    campus_asian_students = CPETPACC,
    campus_black_students = CPETBLAC,
    campus_hispanic_students = CPETHISC,
    campus_ecodis_students = CPETECOC,
    campus_lep_students = CPETLEPC,
    campus_sped_students = CPETSPEC,
    campus_giftal_students = CPETGIFC,
    campus_white_students = CPETWHIC,
    campus_native_american_students = CPETINDC,
    campus_voc_students = CPETVOCC,
    campus_grade_EE_students = CPETGEEC,
    campus_grade_PK_students = CPETGPKC,
    campus_grade_KG_students = CPETGKNC,
    campus_grade_01_students = CPETG01C,
    campus_grade_02_students = CPETG02C,
    campus_grade_03_students = CPETG03C,
    campus_grade_04_students = CPETG04C,
    campus_grade_05_students = CPETG05C,
    campus_grade_06_students = CPETG06C,
    campus_grade_07_students = CPETG07C,
    campus_grade_08_students = CPETG08C,
    campus_grade_09_students = CPETG09C,
    campus_grade_10_students = CPETG10C,
    campus_grade_11_students = CPETG11C,
    campus_grade_12_students = CPETG12C
  ) |>
  select(
    campus_number,
    campus_total_students,
    campus_asian_students,
    campus_black_students,
    campus_hispanic_students,
    campus_ecodis_students,
    campus_lep_students,
    campus_sped_students,
    campus_giftal_students,
    campus_white_students,
    campus_native_american_students,
    campus_voc_students,
    campus_grade_EE_students,
    campus_grade_PK_students,
    campus_grade_KG_students,
    campus_grade_01_students,
    campus_grade_02_students,
    campus_grade_03_students,
    campus_grade_04_students,
    campus_grade_05_students,
    campus_grade_06_students,
    campus_grade_07_students,
    campus_grade_08_students,
    campus_grade_09_students,
    campus_grade_10_students,
    campus_grade_11_students,
    campus_grade_12_students
  )
head(data_stud_0607)

# STAF
data_staf_0607 <- read.csv(
  "data/aeis campus info/06-07/staf/campstaf.dat",
  header = TRUE,
  check.names = FALSE
)
layout_staf_0607 <- readLines("data/aeis campus info/06-07/staf/campstaf.lyt")
var_rows_layout_staf_0607 <- grep("^\\s*[0-9]{1,3}\\s+", layout_staf_0607, value = TRUE)
layout_staf_0607 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_staf_0607),
  NAME = toupper(
    sub(
      "^\\s*[0-9]{1,3}\\s+(\\S+).*",
      "\\1",
      var_rows_layout_staf_0607
    )
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_staf_0607
    )
  )
)
stopifnot(ncol(data_staf_0607) == nrow(layout_staf_0607))
stopifnot(identical(toupper(colnames(data_staf_0607)), layout_staf_0607$NAME))
colnames(data_staf_0607) <- layout_staf_0607$NAME
data_staf_0607 <- data_staf_0607 |>
  transmute(
    campus_number = str_pad(as.character(CAMPUS), 9, "left", "0"),
    teacher_fte = as.numeric(na_if(as.character(CPSTTOFC), ".")),
    aide_fte = as.numeric(na_if(as.character(CPSETOFC), ".")),
    beginning_teacher_fte = as.numeric(na_if(as.character(CPST00FC), ".")),
    teacher_1_5_fte = as.numeric(na_if(as.character(CPST01FC), ".")),
    teacher_6_10_fte = as.numeric(na_if(as.character(CPST06FC), ".")),
    teacher_11_20_fte = as.numeric(na_if(as.character(CPST11FC), ".")),
    teacher_over_20_fte = as.numeric(na_if(as.character(CPST20FC), ".")),
    teacher_experience = as.numeric(na_if(as.character(CPSTEXPA), ".")),
    year = 2007L
  )
stopifnot(!anyDuplicated(data_staf_0607$campus_number))
head(data_staf_0607)

data_ref_longitudinal <- bind_rows(
  data_ref_9798 |> mutate(year = 1998L),
  data_ref_9899 |> mutate(year = 1999L),
  data_ref_9900 |> mutate(year = 2000L),
  data_ref_0001 |> mutate(year = 2001L),
  data_ref_0102 |> mutate(year = 2002L),
  data_ref_0203 |> mutate(year = 2003L),
  data_ref_0304 |> mutate(year = 2004L),
  data_ref_0405 |> mutate(year = 2005L),
  data_ref_0506 |> mutate(year = 2006L),
  data_ref_0607 |> mutate(year = 2007L)
)
data_ref_longitudinal |> group_by(campus_number, year) |> summarise(k = n()) |> arrange(desc(k))
data_stud_longitudinal <- bind_rows(
  data_stud_9798 |> mutate(year = 1998L),
  data_stud_9899 |> mutate(year = 1999L),
  data_stud_9900 |> mutate(year = 2000L),
  data_stud_0001 |> mutate(year = 2001L),
  data_stud_0102 |> mutate(year = 2002L),
  data_stud_0203 |> mutate(year = 2003L),
  data_stud_0304 |> mutate(year = 2004L),
  data_stud_0405 |> mutate(year = 2005L),
  data_stud_0506 |> mutate(year = 2006L),
  data_stud_0607 |> mutate(year = 2007L)
)
data_stud_longitudinal |> group_by(campus_number, year) |> summarise(k = n()) |> arrange(desc(k))
staff_longitudinal <- bind_rows(
  data_staf_9798,
  data_staf_9899,
  data_staf_9900,
  data_staf_0001,
  data_staf_0102,
  data_staf_0203,
  data_staf_0304,
  data_staf_0405,
  data_staf_0506,
  data_staf_0607
) |>
  arrange(year, campus_number)
staff_longitudinal |> group_by(campus_number, year) |> summarise(k = n()) |> arrange(desc(k))

###############################################################################
# 2. Build finance data
###############################################################################

write_finance_outputs <- FALSE
finance_root <- "data/peims finance"
extract_layout_names <- function(layout_path) {
  layout <- readLines(layout_path, warn = FALSE)
  variable_rows <- grep("^\\s*[0-9]{3}\\s+", layout, value = TRUE)
  name_tokens <- sub("^\\s*[0-9]{3}\\s+(\\S+).*$", "\\1", variable_rows)
  description_pattern <- paste0("^\\s*[0-9]{3}\\s+.*?(CHARACTER|NUMERIC)",
                                "\\s+[0-9]+\\s*(.*)$")
  descriptions <- sub(description_pattern, "\\2", variable_rows, perl = TRUE)
  description_matched <- grepl(description_pattern, variable_rows, perl = TRUE)
  descriptions[!description_matched] <- ""
  descriptions <- trimws(descriptions)
  column_names <- name_tokens
  malformed_name <- grepl("(CHARACTER|NUMERIC)$", name_tokens)
  use_description <- malformed_name & nzchar(descriptions)
  column_names[use_description] <- descriptions[use_description]
  column_names[grepl("^PROGRAM_I", column_names)] <- "PROGRAM_INTENT"
  column_names
}
find_one_file <- function(directory, pattern, label) {
  hits <- list.files(
    directory,
    pattern = pattern,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(hits) != 1L) {
    stop(
      label,
      ": expected one matching file in ",
      directory,
      ", but found ",
      length(hits),
      ". Matches: ",
      paste(basename(hits), collapse = ", ")
    )
  }
  hits[[1]]
}
read_peims_file <- function(data_path, layout_path) {
  column_names <- extract_layout_names(layout_path)
  output <- fread(
    data_path,
    header = FALSE,
    colClasses = "character",
    showProgress = FALSE
  )
  if (ncol(output) != length(column_names)) {
    stop(
      "Column-count mismatch for ",
      basename(data_path),
      ": data have ",
      ncol(output),
      " columns but layout has ",
      length(column_names),
      "."
    )
  }
  setnames(output, column_names)
  output
}
read_transaction_partitions <- function(files, layout_path) {
  column_names <- extract_layout_names(layout_path)
  pieces <- lapply(files, function(path) {
    output <- fread(
      path,
      header = FALSE,
      colClasses = "character",
      showProgress = FALSE
    )
    if (ncol(output) != length(column_names)) {
      stop(
        "Column-count mismatch for ",
        basename(path),
        ": data have ",
        ncol(output),
        " columns but layout has ",
        length(column_names),
        "."
      )
    }
    setnames(output, column_names)
    output
  })
  rbindlist(pieces, use.names = TRUE)
}
process_actual_year <- function(end_year_code, finance_root) {
  end_year_code <- as.integer(end_year_code)
  year_suffix <- sprintf("%02d", end_year_code)
  end_calendar_year <- ifelse(end_year_code >= 90,
                              1900L + end_year_code,
                              2000L + end_year_code)
  start_year_code <- (end_year_code - 1L) %% 100L
  finance_school_year <- paste0(sprintf("%02d", start_year_code), year_suffix)
  year_directory <- file.path(finance_root, paste0("Actual", year_suffix))
  if (!dir.exists(year_directory)) {
    stop("Missing directory: ", year_directory)
  }
  message(
    "Processing Actual",
    year_suffix,
    " as finance school year ",
    finance_school_year,
    "..."
  )
  all_files <- list.files(year_directory, full.names = TRUE)
  file_names <- basename(all_files)
  if (year_suffix == "03") {
    transaction_files <- find_one_file(year_directory,
                                       "^ACT03F\\.txt$",
                                       "Actual03 combined transaction data")
    transaction_layout <- find_one_file(year_directory,
                                        "^ACT03D\\.txt$",
                                        "Actual03 combined transaction layout")
  } else if (year_suffix == "99") {
    transaction_files <- find_one_file(year_directory, "^ACT9FILE$", "Actual99 transaction data")
    transaction_layout <- find_one_file(year_directory,
                                        "^ACT9DESC$",
                                        "Actual99 transaction layout")
  } else if (year_suffix == "00") {
    transaction_files <- c(
      find_one_file(year_directory, "^ACT0AFLE$", "Actual00 part A"),
      find_one_file(year_directory, "^ACT0BFLE$", "Actual00 part B")
    )
    transaction_layout <- find_one_file(year_directory,
                                        "^ACT0BDSC$",
                                        "Actual00 transaction layout")
  } else if (year_suffix == "01") {
    transaction_files <- c(
      find_one_file(year_directory, "^ACT1AFLE$", "Actual01 part A"),
      find_one_file(year_directory, "^ACT1BFLE$", "Actual01 part B")
    )
    transaction_layout <- find_one_file(year_directory,
                                        "^ACT1BDSC$",
                                        "Actual01 transaction layout")
  } else if (year_suffix == "04") {
    transaction_files <- find_one_file(year_directory,
                                       "^Actual04All\\.txt$",
                                       "Actual04 transaction data")
    transaction_layout <- find_one_file(
      year_directory,
      "^Actual04Descriptor\\.txt$",
      "Actual04 transaction descriptor"
    )
  } else if (year_suffix == "05") {
    transaction_files <- find_one_file(year_directory,
                                       "^ACTUAL_2005F\\.txt$",
                                       "Actual05 transaction data")
    transaction_layout <- find_one_file(year_directory,
                                        "^ACTUAL_2005D\\.txt$",
                                        "Actual05 transaction layout")
  } else if (year_suffix == "06") {
    transaction_files <- find_one_file(year_directory, "^ACTUAL6F$", "Actual06 transaction data")
    transaction_layout <- find_one_file(year_directory,
                                        "^ACTUAL6D$",
                                        "Actual06 transaction layout")
  } else if (year_suffix == "07") {
    transaction_files <- find_one_file(year_directory,
                                       "^ACTUAL_2007F\\.TXT$",
                                       "Actual07 transaction data")
    transaction_layout <- find_one_file(year_directory,
                                        "^ACTUAL_2007D\\.TXT$",
                                        "Actual07 transaction layout")
  } else if (year_suffix == "08") {
    transaction_files <- find_one_file(year_directory,
                                       "^ACTUAL_2008F\\.TXT$",
                                       "Actual08 transaction data")
    transaction_layout <- find_one_file(year_directory,
                                        "^ACTUAL_2008D\\.TXT$",
                                        "Actual08 transaction layout")
  } else {
    transaction_files <- all_files[grepl(paste0("^ACT", year_suffix, ".*FL\\.txt$"),
                                         file_names,
                                         ignore.case = TRUE) &
                                     !grepl("^CSACT", file_names, ignore.case = TRUE)]
    if (length(transaction_files) == 0L) {
      transaction_files <- all_files[grepl(paste0("^ACT", year_suffix, ".*F\\.txt$"),
                                           file_names,
                                           ignore.case = TRUE) &
                                       !grepl("^CSACT", file_names, ignore.case = TRUE)]
    }
    if (length(transaction_files) == 0L) {
      stop("No ACT transaction files found in ", year_directory)
    }
    transaction_layout <- find_one_file(
      year_directory,
      paste0("^ACT", year_suffix, ".*D(S)?\\.txt$"),
      paste0("Actual", year_suffix, " transaction layout")
    )
  }
  if (year_suffix == "03") {
    campus_reference_file <- find_one_file(year_directory,
                                           "^CAMP03F\\.txt$",
                                           "Actual03 campus-reference data")
    campus_reference_layout <- find_one_file(year_directory,
                                             "^CAMP03D(S)?\\.txt$",
                                             "Actual03 campus-reference layout")
  } else if (year_suffix == "99") {
    fin_unit_file <- find_one_file(year_directory,
                                   "^FIU9FILE$",
                                   "Actual99 financial-unit data")
    fin_unit_layout <- find_one_file(year_directory,
                                     "^FIU9DESC$",
                                     "Actual99 financial-unit layout")
  } else if (year_suffix == "00") {
    fin_unit_file <- find_one_file(year_directory,
                                   "^FIU0FILE$",
                                   "Actual00 financial-unit data")
    fin_unit_layout <- find_one_file(year_directory,
                                     "^FIU0DESC$",
                                     "Actual00 financial-unit layout")
  } else if (year_suffix == "01") {
    fin_unit_file <- find_one_file(year_directory,
                                   "^FIU1FILE\\.txt$",
                                   "Actual01 financial-unit data")
    fin_unit_layout <- find_one_file(year_directory,
                                     "^FIU1DESC\\.txt$",
                                     "Actual01 financial-unit layout")
  } else if (year_suffix == "04") {
    fin_unit_file <- find_one_file(year_directory,
                                   "^FinUnit04\\.txt$",
                                   "Actual04 financial-unit data")
    fin_unit_layout <- find_one_file(
      year_directory,
      "^FinUnit04Descriptor\\.txt$",
      "Actual04 financial-unit layout"
    )
  } else if (year_suffix == "05") {
    fin_unit_file <- find_one_file(year_directory,
                                   "^FIN_UNITF\\.txt$",
                                   "Actual05 financial-unit data")
    fin_unit_layout <- find_one_file(year_directory,
                                     "^FIN_UNITD\\.txt$",
                                     "Actual05 financial-unit layout")
  } else if (year_suffix == "06") {
    fin_unit_file <- find_one_file(year_directory,
                                   "^FINUNITF$",
                                   "Actual06 financial-unit data")
    fin_unit_layout <- find_one_file(year_directory,
                                     "^FINUNITD$",
                                     "Actual06 financial-unit layout")
  } else if (year_suffix == "07") {
    fin_unit_file <- find_one_file(year_directory,
                                   "^FIN_UNIT_2007F\\.TXT$",
                                   "Actual07 financial-unit data")
    fin_unit_layout <- find_one_file(year_directory,
                                     "^FIN_UNIT_2007D\\.TXT$",
                                     "Actual07 financial-unit layout")
  } else if (year_suffix == "08") {
    fin_unit_file <- find_one_file(year_directory,
                                   "^FIN_UNIT_2008F\\.TXT$",
                                   "Actual08 financial-unit data")
    fin_unit_layout <- find_one_file(year_directory,
                                     "^FIN_UNIT_2008D\\.TXT$",
                                     "Actual08 financial-unit layout")
  } else {
    fin_unit_file <- find_one_file(
      year_directory,
      paste0("^FINU", year_suffix, "F\\.txt$"),
      paste0("Actual", year_suffix, " financial-unit data")
    )
    fin_unit_layout <- find_one_file(
      year_directory,
      paste0("^FINU", year_suffix, "D(S)?\\.txt$"),
      paste0("Actual", year_suffix, " financial-unit layout")
    )
  }
  if (year_suffix == "99") {
    fund_file <- find_one_file(year_directory, "^FND9FILE$", "Actual99 fund data")
    fund_layout <- find_one_file(year_directory, "^FND9DESC$", "Actual99 fund layout")
  } else if (year_suffix == "00") {
    fund_file <- find_one_file(year_directory, "^FND0FILE$", "Actual00 fund data")
    fund_layout <- find_one_file(year_directory, "^FND0DESC$", "Actual00 fund layout")
  } else if (year_suffix == "01") {
    fund_file <- find_one_file(year_directory, "^FND1FILE$", "Actual01 fund data")
    fund_layout <- find_one_file(year_directory, "^FND1DESC$", "Actual01 fund layout")
  } else if (year_suffix == "03") {
    fund_file <- find_one_file(year_directory, "^FUND3F\\.txt$", "Actual03 fund data")
    fund_layout <- find_one_file(year_directory, "^FUND3D\\.txt$", "Actual03 fund layout")
  } else if (year_suffix == "04") {
    fund_file <- find_one_file(year_directory, "^Fund04\\.txt$", "Actual04 fund data")
    fund_layout <- find_one_file(year_directory,
                                 "^Fund04Descriptor\\.txt$",
                                 "Actual04 fund layout")
  } else if (year_suffix == "05") {
    fund_file <- find_one_file(year_directory, "^FUND_2005F\\.txt$", "Actual05 fund data")
    fund_layout <- find_one_file(year_directory,
                                 "^FUND_2005D\\.txt$",
                                 "Actual05 fund layout")
  } else if (year_suffix == "06") {
    fund_file <- find_one_file(year_directory, "^FUND06F$", "Actual06 fund data")
    fund_layout <- find_one_file(year_directory, "^FUND06D$", "Actual06 fund layout")
  } else if (year_suffix == "07") {
    fund_file <- find_one_file(year_directory, "^FUND_2007F\\.TXT$", "Actual07 fund data")
    fund_layout <- find_one_file(year_directory,
                                 "^FUND_2007D\\.TXT$",
                                 "Actual07 fund layout")
  } else if (year_suffix == "08") {
    fund_file <- find_one_file(year_directory, "^FUND_2008F\\.TXT$", "Actual08 fund data")
    
    fund_layout <- find_one_file(year_directory,
                                 "^FUND_2008D\\.TXT$",
                                 "Actual08 fund layout")
  } else {
    fund_file <- find_one_file(
      year_directory,
      paste0("^FUND", year_suffix, "F\\.txt$"),
      paste0("Actual", year_suffix, " fund data")
    )
    fund_layout <- find_one_file(
      year_directory,
      paste0("^FUND", year_suffix, "D(S)?\\.txt$"),
      paste0("Actual", year_suffix, " fund layout")
    )
  }
  if (year_suffix == "04") {
    transactions <- rbindlist(lapply(sort(transaction_files), function(path) {
      fread(
        path,
        header = TRUE,
        colClasses = "character",
        showProgress = FALSE
      )
    }), use.names = TRUE)
  } else {
    transactions <- read_transaction_partitions(sort(transaction_files), transaction_layout)
  }
  required_transaction_columns <- c(
    "DISTRICT",
    "FUND",
    "FUNCTION",
    "OBJECT",
    "FIN_UNIT",
    "PROGRAM_INTENT",
    "FUNDYEAR",
    "ACTAMT"
  )
  missing_transaction_columns <- setdiff(required_transaction_columns, names(transactions))
  if (length(missing_transaction_columns) > 0L) {
    stop(
      "Actual",
      year_suffix,
      " is missing required transaction columns: ",
      paste(missing_transaction_columns, collapse = ", ")
    )
  }
  transactions <- transactions |>
    mutate(
      DISTRICT = str_pad(DISTRICT, 6, side = "left", pad = "0"),
      FUND = str_pad(FUND, 3, side = "left", pad = "0"),
      FUNCTION = str_pad(FUNCTION, 2, side = "left", pad = "0"),
      OBJECT = str_pad(OBJECT, 4, side = "left", pad = "0"),
      FIN_UNIT = str_pad(FIN_UNIT, 3, side = "left", pad = "0"),
      PROGRAM_INTENT = str_pad(PROGRAM_INTENT, 2, side = "left", pad = "0"),
      FUNDYEAR = as.integer(FUNDYEAR),
      ACTAMT = as.numeric(ACTAMT),
      CAMPUS_ID = paste0(DISTRICT, FIN_UNIT)
    )
  if (year_suffix == "03") {
    campus_reference <- read_peims_file(campus_reference_file, campus_reference_layout)
    if (!"CAMPUS" %in% names(campus_reference)) {
      stop("")
    }
    campus_units <- campus_reference |>
      transmute(
        CAMPUS_ID = str_pad(CAMPUS, 9, side = "left", pad = "0"),
        DISTRICT = substr(CAMPUS_ID, 1, 6),
        FIN_UNIT = substr(CAMPUS_ID, 7, 9),
        FIN_UNIT_NAME = NA_character_
      ) |>
      distinct(CAMPUS_ID, .keep_all = TRUE)
  } else {
    fin_units <- read_peims_file(fin_unit_file, fin_unit_layout)
    required_fin_unit_columns <- c("DISTRICT", "FIN_UNIT", "FIN_UNIT_TYPE", "FIN_UNIT_NAME")
    missing_fin_unit_columns <- setdiff(required_fin_unit_columns, names(fin_units))
    if (length(missing_fin_unit_columns) > 0L) {
      stop(
        "Actual",
        year_suffix,
        " is missing required FINU columns: ",
        paste(missing_fin_unit_columns, collapse = ", ")
      )
    }
    campus_units <- fin_units |>
      mutate(
        DISTRICT = str_pad(DISTRICT, 6, side = "left", pad = "0"),
        FIN_UNIT = str_pad(FIN_UNIT, 3, side = "left", pad = "0"),
        CAMPUS_ID = paste0(DISTRICT, FIN_UNIT)
      ) |>
      filter(FIN_UNIT_TYPE == "01") |>
      arrange(CAMPUS_ID) |>
      distinct(DISTRICT, FIN_UNIT, .keep_all = TRUE) |>
      select(CAMPUS_ID, DISTRICT, FIN_UNIT, FIN_UNIT_NAME)
  }
  transactions <- transactions |>
    inner_join(campus_units, by = c("CAMPUS_ID", "DISTRICT", "FIN_UNIT"))
  fund_lookup <- read_peims_file(fund_file, fund_layout) |>
    mutate(FUND = str_pad(FUND, 3, side = "left", pad = "0"))
  if (!"FUND" %in% names(fund_lookup)) {
    stop("Actual", year_suffix, " fund lookup has no FUND column.")
  }
  fund_description_columns <- setdiff(names(fund_lookup), c("FUND", "DTUPDATE"))
  fund_description_text <- apply(as.data.frame(fund_lookup)[, fund_description_columns, drop = FALSE], 1, paste, collapse = " ")
  shared_service_funds <- fund_lookup$FUND[grepl("SHARED SERV|(^|[^A-Z])SSA([^A-Z]|$)",
                                                 fund_description_text,
                                                 ignore.case = TRUE)] |>
    unique()
  operating <- transactions |>
    mutate(FUNCTION_NUM = as.integer(FUNCTION),
           OBJECT_NUM = as.integer(OBJECT)) |>
    filter(
      FUNDYEAR == end_calendar_year,
      between(OBJECT_NUM, 6112L, 6499L),
      between(FUNCTION_NUM, 11L, 53L),
      FUND != "600",
      !FUND %in% shared_service_funds
    ) |>
    mutate(
      CRAIG_CATEGORY = case_when(
        # Instruction includes instructional resources and media (function
        # 12): with function 11 alone, Table 4 instruction is 4-6% below the
        # published means while the other categories match.
        FUNCTION %in% c("11", "12") ~ "instruction",
        FUNCTION %in% c("13", "21", "23") ~ "admin_training",
        FUNCTION %in% c("31", "32") ~ "counseling",
        FUNCTION == "36" ~ "extracurricular",
        TRUE ~ "other_operating"
      )
    )
  spending <- operating |>
    group_by(CAMPUS_ID, DISTRICT, FIN_UNIT, FIN_UNIT_NAME, FUNDYEAR) |>
    summarise(
      TOTAL_OPERATING = sum(ACTAMT, na.rm = TRUE),
      INSTRUCTION = sum(ACTAMT[CRAIG_CATEGORY == "instruction"], na.rm = TRUE),
      ADMIN_TRAINING = sum(ACTAMT[CRAIG_CATEGORY == "admin_training"], na.rm = TRUE),
      COUNSELING = sum(ACTAMT[CRAIG_CATEGORY == "counseling"], na.rm = TRUE),
      EXTRACURRICULAR = sum(ACTAMT[CRAIG_CATEGORY == "extracurricular"], na.rm = TRUE),
      OTHER_OPERATING = sum(ACTAMT[CRAIG_CATEGORY == "other_operating"], na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      FINANCE_SCHOOL_YEAR = finance_school_year,
      FINANCE_START_YEAR = end_calendar_year - 1L,
      FINANCE_END_YEAR = end_calendar_year,
      # Craig et al. use the financial outcome in t+1.
      RATING_YEAR = end_calendar_year - 1L,
      SOURCE_DIRECTORY = paste0("Actual", year_suffix),
      .before = CAMPUS_ID
    )
  duplicate_check <- spending |>
    count(FINANCE_SCHOOL_YEAR, CAMPUS_ID) |>
    filter(n > 1L)
  if (nrow(duplicate_check) > 0L) {
    stop("Actual",
         year_suffix,
         " produced duplicate campus-year rows after aggregation.")
  }
  message("Finished ",
          finance_school_year,
          ": ",
          format(nrow(spending), big.mark = ","),
          " campuses.")
  spending
}
actual_year_codes <- c(99L, 0L:8L)
finance_longitudinal_9798_0708 <- bind_rows(lapply(actual_year_codes, process_actual_year, finance_root = finance_root)) |>
  arrange(FINANCE_END_YEAR, CAMPUS_ID)
duplicate_campus_years <- finance_longitudinal_9798_0708 |>
  count(FINANCE_SCHOOL_YEAR, CAMPUS_ID) |>
  filter(n > 1L)
if (nrow(duplicate_campus_years) > 0L) {
  stop("Finance aggregation produced duplicate CAMPUS_ID-year rows.")
}
finance_year_summary <- finance_longitudinal_9798_0708 |>
  group_by(FINANCE_SCHOOL_YEAR, FINANCE_END_YEAR, RATING_YEAR) |>
  summarise(
    CAMPUSES = n(),
    TOTAL_OPERATING_DOLLARS = sum(TOTAL_OPERATING, na.rm = TRUE),
    .groups = "drop"
  )

longitudinal_source_check <- bind_rows(
  data_ref_longitudinal |>
    count(year, name = "rows") |>
    mutate(source = "REF"),
  data_stud_longitudinal |>
    count(year, name = "rows") |>
    mutate(source = "STUD_all"),
  staff_longitudinal |>
    count(year, name = "rows") |>
    mutate(source = "STAFF")
) |>
  select(source, year, rows) |>
  arrange(source, year)

longitudinal_duplicate_check <- tibble(
  source = c("REF", "STUD_all", "STAFF", "FINANCE"),
  duplicate_keys = c(
    sum(duplicated(data_ref_longitudinal[c("campus_number", "year")])),
    sum(duplicated(data_stud_longitudinal[c("campus_number", "year")])),
    sum(duplicated(staff_longitudinal[c("campus_number", "year")])),
    nrow(duplicate_campus_years)
  )
)

stopifnot(
  all(longitudinal_duplicate_check$duplicate_keys == 0L),
  setequal(unique(data_ref_longitudinal$year), 1998:2007),
  setequal(unique(data_stud_longitudinal$year), 1998:2007),
  setequal(unique(staff_longitudinal$year), 1998:2007)
)
if (!exists("write_finance_outputs", inherits = TRUE)) {
  write_finance_outputs <- TRUE
}
if (isTRUE(write_finance_outputs)) {
  write.csv(
    finance_longitudinal_9798_0708,
    file.path(finance_root, "finance_longitudinal_9798_0708.csv"),
    row.names = FALSE
  )
  saveRDS(
    finance_longitudinal_9798_0708,
    file.path(finance_root, "finance_longitudinal_9798_0708.rds")
  )
}

###############################################################################
# 3. Build panel data
###############################################################################

ref_panel <- data_ref_longitudinal |>
  mutate(
    campus_number = str_pad(as.character(campus_number), 9, "left", "0"),
    district = substr(campus_number, 1, 6),
    year = as.integer(year),
    campus_rating = toupper(trimws(as.character(campus_rating)))
  )
stud_panel <- data_stud_longitudinal |>
  mutate(
    campus_number = str_pad(as.character(campus_number), 9, "left", "0"),
    year = as.integer(year),
    across(-c(campus_number, year), ~ suppressWarnings(as.numeric(
      na_if(as.character(.x), ".")
    )))
  )
rating_panel <- ref_panel |>
  filter(campus_rating %in% c("L", "A", "R", "E")) |>
  inner_join(stud_panel, by = c("campus_number", "year")) |>
  filter(campus_total_students >= 200) |>
  mutate(
    rating_value = recode(
      campus_rating,
      L = 1L,
      A = 2L,
      R = 3L,
      E = 4L
    ),
    regime = case_when(
      year %in% 1998:2002 ~ "TAAS",
      year %in% 2004:2007 ~ "TAKS",
      TRUE ~ NA_character_
    )
  )
stopifnot(!anyDuplicated(rating_panel[c("campus_number", "year")]))

###############################################################################
# 4. TABLE 1
###############################################################################

table1_distribution <- rating_panel |>
  filter(!is.na(regime)) |>
  count(year, campus_rating) |>
  complete(
    year = c(1998:2002, 2004:2007),
    campus_rating = c("L", "A", "R", "E"),
    fill = list(n = 0L)
  ) |>
  group_by(year) |>
  mutate(percent = 100 * n / sum(n)) |>
  ungroup() |>
  select(year, campus_rating, percent) |>
  pivot_wider(names_from = campus_rating, values_from = percent) |>
  transmute(
    year,
    low = round(L, 1),
    acceptable = round(A, 1),
    recognized = round(R, 1),
    exemplary = round(E, 1)
  )
table1_current <- rating_panel |>
  filter(year %in% c(1999:2002, 2004:2007)) |>
  transmute(
    campus_number,
    year,
    current_value = rating_value,
    comparison_year = case_when(
      year %in% 1999:2002 ~ year - 1L,
      year == 2004L ~ 2002L,
      year %in% 2005:2007 ~ year - 1L
    )
  )
table1_previous <- rating_panel |>
  transmute(campus_number,
            comparison_year = year,
            previous_value = rating_value)
table1_changes <- table1_current |>
  inner_join(table1_previous, by = c("campus_number", "comparison_year")) |>
  mutate(
    change = case_when(
      current_value < previous_value ~ "lower",
      current_value == previous_value ~ "same",
      current_value > previous_value ~ "higher"
    )
  ) |>
  count(year, change) |>
  group_by(year) |>
  mutate(percent = 100 * n / sum(n)) |>
  ungroup() |>
  select(year, change, percent) |>
  pivot_wider(names_from = change, values_from = percent) |>
  mutate(across(c(lower, same, higher), ~ round(.x, 1)))
table1 <- table1_distribution |>
  left_join(table1_changes, by = "year")
head(table1)
write.csv(table1, "tables/table1_rating_distributions.csv", row.names = FALSE)

###############################################################################
# 5. TABLE 2
###############################################################################

rating_transitions <- table1_current |>
  inner_join(
    rating_panel |>
      transmute(
        campus_number,
        comparison_year = year,
        previous_rating = campus_rating
      ),
    by = c("campus_number", "comparison_year")
  ) |>
  left_join(
    rating_panel |>
      transmute(campus_number, year, current_rating = campus_rating),
    by = c("campus_number", "year")
  )
make_transition_matrix <- function(data) {
  data |>
    count(previous_rating, current_rating) |>
    complete(
      previous_rating = c("L", "A", "R", "E"),
      current_rating = c("L", "A", "R", "E"),
      fill = list(n = 0L)
    ) |>
    group_by(previous_rating) |>
    mutate(percent = 100 * n / sum(n)) |>
    ungroup() |>
    select(previous_rating, current_rating, percent) |>
    pivot_wider(names_from = current_rating, values_from = percent) |>
    mutate(across(c(L, A, R, E), ~ round(.x, 1))) |>
    arrange(match(previous_rating, c("L", "A", "R", "E")))
}
table2a <- rating_transitions |>
  filter(year %in% 1999:2002) |>
  make_transition_matrix() |>
  select(previous_rating, L, A, R, E)
table2b <- rating_transitions |>
  filter(year == 2004L) |>
  make_transition_matrix() |>
  select(previous_rating, L, A, R, E)
table2c <- rating_transitions |>
  filter(year %in% 2005:2007) |>
  make_transition_matrix() |>
  select(previous_rating, L, A, R, E)
head(table2a)
head(table2b)
head(table2c)
write.csv(table2a, "tables/table2a_taas_transitions.csv", row.names = FALSE)
write.csv(table2b, "tables/table2b_transition.csv", row.names = FALSE)
write.csv(table2c, "tables/table2c_taks_transitions.csv", row.names = FALSE)

###############################################################################
# Construct estimated 2002-03 transition rating using 2003-04 rules
###############################################################################

transition_root <- "data/aeis campus info/02-03"

read_transition_file <- function(data_path, layout_path) {
  layout_lines <- readLines(layout_path)
  
  layout_rows <- grep("^\\s*[0-9]{1,3}\\s+", layout_lines, value = TRUE)
  
  column_names <- sub("^\\s*[0-9]{1,3}\\s+(\\S+).*", "\\1", layout_rows)
  
  output <- read.csv(
    data_path,
    header = FALSE,
    colClasses = "character",
    na.strings = c(".", "", "NA")
  )
  
  stopifnot(ncol(output) == length(column_names))
  
  names(output) <- column_names
  output
}

###############################################################################
# 1. Read the TAKS and non-test indicator files
###############################################################################

taks_311_1sem <- read_transition_file(
  file.path(transition_root, "taks", "camptaks311-1belowpanel.dta"),
  file.path(transition_root, "taks", "camptaks311-1belowpanel.lyt")
)

# Group counts of TAKS answer documents, used to size student groups.
taks_participation_0203 <- read_transition_file(
  file.path(transition_root, "taks", "camptakspart.dat"),
  file.path(transition_root, "taks", "camptakspart.lyt")
)

completion_0203 <- read_transition_file(
  file.path(transition_root, "comp", "campcomp.dta"),
  file.path(transition_root, "comp", "campcomp.lyt")
)

dropout_0203 <- read_transition_file(
  file.path(transition_root, "other", "campothr.dat"),
  file.path(transition_root, "other", "campothr.lyt")
)

###############################################################################
# 2. Campus enrollment and grade weights
###############################################################################

build_rating_students <- function(stud_year) {
  data_stud_longitudinal |>
    filter(year == stud_year, as.numeric(campus_total_students) >= 200) |>
    transmute(
      CAMPUS = str_pad(
        as.character(campus_number),
        width = 9,
        side = "left",
        pad = "0"
      ),
      enrollment = as.numeric(campus_total_students),
      black = as.numeric(campus_black_students),
      hispanic = as.numeric(campus_hispanic_students),
      white = as.numeric(campus_white_students),
      economic_disadvantage = as.numeric(campus_ecodis_students),
      
      grade_03 = as.numeric(campus_grade_03_students),
      grade_04 = as.numeric(campus_grade_04_students),
      grade_05 = as.numeric(campus_grade_05_students),
      grade_06 = as.numeric(campus_grade_06_students),
      grade_07 = as.numeric(campus_grade_07_students),
      grade_08 = as.numeric(campus_grade_08_students),
      grade_09 = as.numeric(campus_grade_09_students),
      grade_10 = as.numeric(campus_grade_10_students),
      grade_11 = as.numeric(campus_grade_11_students),
      grade_12 = as.numeric(campus_grade_12_students)
    ) |>
    mutate(
      enrollment_grades_3_9 = rowSums(across(grade_03:grade_09), na.rm = TRUE),
      enrollment_grades_3_10 = rowSums(across(grade_03:grade_10), na.rm = TRUE),
      enrollment_writing = rowSums(across(c(
        grade_04, grade_07
      )), na.rm = TRUE),
      enrollment_science_310 = rowSums(across(c(
        grade_05, grade_10
      )), na.rm = TRUE),
      enrollment_social_310 = rowSums(across(c(
        grade_08, grade_10
      )), na.rm = TRUE),
      enrollment_grades_7_8 = rowSums(across(c(
        grade_07, grade_08
      )), na.rm = TRUE)
    )
}

transition_students <- build_rating_students(2003)

###############################################################################
# 3. Construct the 2003 TAKS measures under the 2003-04 rules
#
# The paper estimates 2002-03 ratings "using the new (2003-2004) rules". In
# 2003-04 TAKS was scored at one SEM below the panel recommendation, so the
# 2003 rates at that standard are used (variables C?311Q?03R).
#
# Validation: applying this calculator (1-SEM rates, tested-grade subgroup
# sizes, whole-number rates, no SDAA) to the 2003-04 TAKS file reproduces
# Online Appendix Table 1 panel A (calculated 2003-04 ratings without required
# improvement): 1.1/63.0/28.4/7.5 vs published 1.1/62.2/30.2/6.5; see
# tables/checks/appendix_table1a_comparison.csv. The earlier choice of two-SEM
# rates with enrollment-based subgroup sizes matched the published transition
# distribution only because two errors offset, and it fails that validation
# (8.4/70.4/18.0/3.3).
###############################################################################

taks_311_1sem_long <- taks_311_1sem |>
  select(CAMPUS, matches("^C[ABHWE]311Q[EMCSRW]03R$")) |>
  pivot_longer(cols = -CAMPUS,
               names_to = "variable",
               values_to = "performance_rate") |>
  extract(variable,
          into = c("group", "subject_code"),
          regex = "^C([ABHWE])311Q([EMCSRW])03R$") |>
  mutate(
    CAMPUS = str_pad(as.character(CAMPUS), 9, "left", "0"),
    performance_rate = as.numeric(performance_rate)
  )

###############################################################################
# 4. Combine Reading and ELA into one accountability subject
###############################################################################

# Tested grades by accountability subject in 2003 and 2004.
subject_tested_grades <- list(
  reading_ela = 3:11,
  mathematics = 3:11,
  writing = c(4, 7),
  social_studies = c(8, 10, 11),
  science = c(5, 10, 11)
)

# Builds one row per campus x student group x subject with the performance
# rate, whether the measure is evaluated, and the Acceptable cutoff.
#   taks_long: CAMPUS, group, subject_code (R/E/M/W/S/C), performance_rate
#   students:  campus enrollment by grade and group (as transition_students)
#   shares:    optional CAMPUS, pB, pH, pW, pE: each group's share of tested
#              students (from TAKS answer documents)
build_taks_measures <- function(taks_long, students, shares = NULL) {
  subject_tested_enrollment <- bind_rows(lapply(names(subject_tested_grades), function(subject_name) {
    grade_columns <- sprintf("grade_%02d", subject_tested_grades[[subject_name]])
    tibble(
      CAMPUS = students$CAMPUS,
      subject = subject_name,
      tested_enrollment = rowSums(as.matrix(students[, grade_columns]), na.rm = TRUE)
    )
  }))
  if (is.null(shares)) {
    shares <- tibble(
      CAMPUS = character(),
      pB = numeric(),
      pH = numeric(),
      pW = numeric(),
      pE = numeric()
    )
  }
  
  wide <- taks_long |>
    pivot_wider(names_from = subject_code, values_from = performance_rate)
  # The 2003-04 accountability file reports Reading/ELA combined as R.
  for (code in c("R", "E", "M", "W", "S", "C")) {
    if (!code %in% names(wide))
      wide[[code]] <- NA_real_
  }
  
  wide |>
    left_join(students, by = "CAMPUS") |>
    mutate(
      reading_ela = case_when(
        !is.na(R) &
          !is.na(E) &
          enrollment_grades_3_9 + grade_10 + grade_11 > 0 ~
          (R * enrollment_grades_3_9 +
             E * (grade_10 + grade_11)) /
          (enrollment_grades_3_9 +
             grade_10 +
             grade_11),!is.na(R) ~ R,!is.na(E) ~ E,
        TRUE ~ NA_real_
      )
    ) |>
    transmute(
      CAMPUS,
      group,
      enrollment,
      black,
      hispanic,
      white,
      economic_disadvantage,
      reading_ela,
      mathematics = M,
      writing = W,
      social_studies = S,
      science = C
    ) |>
    pivot_longer(
      cols = c(reading_ela, mathematics, writing, social_studies, science),
      names_to = "subject",
      values_to = "performance_rate"
    ) |>
    # Minimum-size rules apply to students tested in each subject, so group
    # sizes use enrollment in the tested grades for that subject (campus-wide
    # enrollment would include untested grades such as K-2). Public files do
    # not report subject-by-group tested counts, so each group's tested count
    # is its share of TAKS answer documents (or, if unavailable, its share of
    # enrollment) times tested-grade enrollment.
    left_join(subject_tested_enrollment, by = c("CAMPUS", "subject")) |>
    left_join(shares, by = "CAMPUS") |>
    mutate(
      group_share = case_when(
        group == "A" ~ 1,
        group == "B" ~ coalesce(pB, black / enrollment),
        group == "H" ~ coalesce(pH, hispanic / enrollment),
        group == "W" ~ coalesce(pW, white / enrollment),
        group == "E" ~ coalesce(pE, economic_disadvantage / enrollment)
      ),
      group_enrollment = group_share * tested_enrollment,
      group_evaluated = case_when(
        group == "A" ~ TRUE,
        is.na(group_enrollment) | is.na(tested_enrollment) ~ FALSE,
        group_enrollment >= 50 ~ TRUE,
        group_enrollment >= 30 &
          group_enrollment / tested_enrollment >= 0.10 ~ TRUE,
        TRUE ~ FALSE
      ),
      # TEA evaluates whole-number rates (69.5 rounds to 70).
      performance_rate = floor(performance_rate + 0.5),
      measure_evaluated = group_evaluated &
        !is.na(performance_rate),
      acceptable_cutoff = case_when(subject == "mathematics" ~ 35, subject == "science" ~ 25, TRUE ~ 50)
    ) |>
    select(CAMPUS,
           group,
           subject,
           performance_rate,
           measure_evaluated,
           acceptable_cutoff)
}

taks_participation_shares <- taks_participation_0203 |>
  transmute(
    CAMPUS = str_pad(as.character(CAMPUS), 9, "left", "0"),
    answer_documents = as.numeric(CAADA03D),
    pB = as.numeric(CBADA03D) / answer_documents,
    pH = as.numeric(CHADA03D) / answer_documents,
    pW = as.numeric(CWADA03D) / answer_documents,
    pE = as.numeric(CEADA03D) / answer_documents
  ) |>
  filter(answer_documents > 0) |>
  select(-answer_documents)

taks_subjects <- build_taks_measures(taks_311_1sem_long,
                                     transition_students,
                                     taks_participation_shares)

###############################################################################
# 5. SDAA
#
# The 2003 SDAA rate is kept for the input checks but is not a rating measure.
# Including it pushes about 6% of campuses to L and breaks the Appendix Table 1
# validation; the specification without it reproduces Table 5 (Total "falls to
# A" 70.7 vs published 72.9; instruction 68.6 vs 66.2; student-teacher ratio
# -0.26 vs -0.28).
###############################################################################

sdaa_2003 <- dropout_0203 |>
  transmute(
    CAMPUS = str_pad(as.character(CAMPUS), 9, "left", "0"),
    group = "A",
    subject = "sdaa",
    performance_rate = suppressWarnings(as.numeric(CA0SA03R)),
    measure_evaluated = !is.na(performance_rate),
    acceptable_cutoff = 50
  )

taks_measures <- taks_subjects

###############################################################################
# 6. Apply test-performance standards and exceptions
###############################################################################

score_test_measures <- function(measures) {
  measures |>
    group_by(CAMPUS) |>
    summarise(
      evaluated_measures = sum(measure_evaluated),
      
      failed_acceptable = sum(measure_evaluated &
                                performance_rate < acceptable_cutoff),
      
      failed_below_exception_floor = any(measure_evaluated &
                                           performance_rate < acceptable_cutoff - 5),
      
      passes_recognized_tests = all(!measure_evaluated |
                                      performance_rate >= 70),
      
      passes_exemplary_tests = all(!measure_evaluated |
                                     performance_rate >= 90),
      
      .groups = "drop"
    ) |>
    mutate(
      exceptions_allowed = case_when(
        evaluated_measures <= 5 ~ 0L,
        evaluated_measures <= 10 ~ 1L,
        evaluated_measures <= 15 ~ 2L,
        evaluated_measures >= 16 ~ 3L
      ),
      passes_acceptable_tests =
        failed_acceptable == 0 |
        (
          failed_acceptable <= exceptions_allowed &
            !failed_below_exception_floor
        )
    )
}

test_rating_components <- score_test_measures(taks_measures)

###############################################################################
# 8. Completion rates
###############################################################################

completion_long <- completion_0203 |>
  select(CAMPUS, matches("^C[ABHWE][GEN]C402R$")) |>
  pivot_longer(cols = -CAMPUS,
               names_to = "variable",
               values_to = "value") |>
  extract(variable,
          into = c("group", "component"),
          regex = "^C([ABHWE])([GEN])C402R$") |>
  mutate(CAMPUS = str_pad(CAMPUS, 9, "left", "0"),
         value = as.numeric(value)) |>
  group_by(CAMPUS, group) |>
  summarise(completion_rate = if (all(is.na(value))) {
    NA_real_
  } else {
    sum(value, na.rm = TRUE)
  }, .groups = "drop")

###############################################################################
# 9. Annual dropout rates
###############################################################################

dropout_long <- dropout_0203 |>
  select(CAMPUS, matches("^C[ABHWE]0DR02R$")) |>
  pivot_longer(cols = -CAMPUS,
               names_to = "variable",
               values_to = "dropout_rate") |>
  extract(variable, into = "group", regex = "^C([ABHWE])0DR02R$") |>
  mutate(CAMPUS = str_pad(CAMPUS, 9, "left", "0"),
         dropout_rate = as.numeric(dropout_rate))

###############################################################################
# 9. Apply the completion standards
#
# The public AEIS extracts report completion/dropout rates but not the cohort
# counts and dropout/non-completer counts needed to decide which demographic
# groups meet the official minimum-size rule. Use the all-students completion
# indicator only. Craig et al. explicitly ignore dropout in the transition-year
# calculation because the dropout definition changed between TAAS and TAKS.
###############################################################################

non_test_components <- full_join(completion_long, dropout_long, by = c("CAMPUS", "group")) |>
  filter(group == "A") |>
  left_join(transition_students |>
              select(CAMPUS, grade_12, enrollment_grades_7_8),
            by = "CAMPUS") |>
  mutate(
    completion_noncompleter_proxy =
      grade_12 * pmax(0, 100 - completion_rate) / 100,
    dropout_count_proxy =
      enrollment_grades_7_8 * dropout_rate / 100,
    completion_evaluated =
      !is.na(completion_rate) &
      grade_12 >= 10 &
      completion_noncompleter_proxy >= 10,
    dropout_evaluated =
      !is.na(dropout_rate) &
      enrollment_grades_7_8 >= 10 &
      dropout_count_proxy >= 10
  ) |>
  transmute(
    CAMPUS,
    completion_rate,
    dropout_rate,
    completion_noncompleter_proxy,
    dropout_count_proxy,
    completion_evaluated,
    dropout_evaluated,
    passes_acceptable_non_test =
      (!completion_evaluated | completion_rate >= 75),
    passes_recognized_non_test =
      (!completion_evaluated | completion_rate >= 85),
    passes_exemplary_non_test =
      (!completion_evaluated | completion_rate >= 95)
  )

###############################################################################
# 11. Assign the estimated transition rating
###############################################################################

estimated_transition_rating <- test_rating_components |>
  left_join(non_test_components, by = "CAMPUS") |>
  mutate(
    across(starts_with("passes_"), ~ replace_na(.x, TRUE)),
    estimated_rating_no_ri = case_when(
      passes_exemplary_tests &
        passes_exemplary_non_test ~ "E",
      
      passes_recognized_tests &
        passes_recognized_non_test ~ "R",
      
      passes_acceptable_tests &
        passes_acceptable_non_test ~ "A",
      
      TRUE ~ "L"
    )
  )

###############################################################################
# 12. Compare the estimated rating with the 2001-02 rating
###############################################################################

prior_rating_2002 <- data_ref_longitudinal |>
  filter(year == 2002) |>
  transmute(
    CAMPUS = str_pad(
      as.character(campus_number),
      width = 9,
      side = "left",
      pad = "0"
    ),
    rating_2002 = as.character(campus_rating)
  )

rating_values <- c(L = 1L,
                   A = 2L,
                   R = 3L,
                   E = 4L)

transition_rating_0203 <- estimated_transition_rating |>
  semi_join(transition_students |>
              distinct(CAMPUS), by = "CAMPUS") |>
  filter(evaluated_measures > 0) |>
  inner_join(prior_rating_2002, by = "CAMPUS") |>
  transmute(
    campus_number = CAMPUS,
    district = substr(CAMPUS, 1, 6),
    evaluated_measures,
    exceptions_allowed,
    failed_acceptable,
    failed_below_exception_floor,
    passes_acceptable_tests,
    passes_recognized_tests,
    passes_exemplary_tests,
    passes_acceptable_non_test,
    passes_recognized_non_test,
    passes_exemplary_non_test,
    estimated_rating_no_ri,
    rating_2002,
    rating_increased = as.integer(rating_values[estimated_rating_no_ri] >
                                    rating_values[rating_2002]),
    rating_dropped_to_L = as.integer(estimated_rating_no_ri == "L" &
                                       rating_values[estimated_rating_no_ri] <
                                       rating_values[rating_2002]),
    rating_dropped_to_A = as.integer(estimated_rating_no_ri == "A" &
                                       rating_values[estimated_rating_no_ri] <
                                       rating_values[rating_2002]),
    rating_dropped_to_R = as.integer(estimated_rating_no_ri == "R" &
                                       rating_values[estimated_rating_no_ri] <
                                       rating_values[rating_2002])
  )

###############################################################################
# 13. Check the estimated distribution
###############################################################################

transition_distribution_check <- transition_rating_0203 |>
  count(estimated_rating_no_ri) |>
  complete(estimated_rating_no_ri = c("L", "A", "R", "E"),
           fill = list(n = 0L)) |>
  mutate(
    percent = round(100 * n / sum(n), 1),
    published_percent = case_when(
      estimated_rating_no_ri == "L" ~ 8.6,
      estimated_rating_no_ri == "A" ~ 70.3,
      estimated_rating_no_ri == "R" ~ 18.2,
      estimated_rating_no_ri == "E" ~ 2.8
    ),
    difference_from_published = round(percent - published_percent, 1)
  ) |>
  arrange(match(estimated_rating_no_ri, c("L", "A", "R", "E")))

test_only_distribution_check <- test_rating_components |>
  semi_join(transition_students |>
              distinct(CAMPUS), by = "CAMPUS") |>
  filter(evaluated_measures > 0) |>
  inner_join(prior_rating_2002 |>
               select(CAMPUS), by = "CAMPUS") |>
  mutate(
    rating_tests_only = case_when(
      passes_exemplary_tests ~ "E",
      passes_recognized_tests ~ "R",
      passes_acceptable_tests ~ "A",
      TRUE ~ "L"
    )
  ) |>
  count(rating_tests_only) |>
  complete(rating_tests_only = c("L", "A", "R", "E"),
           fill = list(n = 0L)) |>
  mutate(percent = round(100 * n / sum(n), 1)) |>
  arrange(match(rating_tests_only, c("L", "A", "R", "E")))

transition_direction_check <- transition_rating_0203 |>
  mutate(
    old_value = unname(rating_values[rating_2002]),
    new_value = unname(rating_values[estimated_rating_no_ri]),
    direction = case_when(
      new_value < old_value ~ "lower",
      new_value == old_value ~ "same",
      new_value > old_value ~ "higher"
    )
  ) |>
  count(direction) |>
  complete(direction = c("lower", "same", "higher"),
           fill = list(n = 0L)) |>
  mutate(percent = round(100 * n / sum(n), 1)) |>
  arrange(match(direction, c("lower", "same", "higher")))

###############################################################################
# Add the imputed transition row to Table 1
#
# Table 2B is intentionally not replaced here: it compares the last actual
# TAAS rating in 2001-02 with the first actual TAKS rating in 2003-04. The
# imputed 2002-03 rating is used for Table 1 and the rating-shock regressions.
###############################################################################

table1_transition_row <- tibble(
  year = 2003L,
  low = transition_distribution_check$percent[transition_distribution_check$estimated_rating_no_ri == "L"],
  acceptable = transition_distribution_check$percent[transition_distribution_check$estimated_rating_no_ri == "A"],
  recognized = transition_distribution_check$percent[transition_distribution_check$estimated_rating_no_ri == "R"],
  exemplary = transition_distribution_check$percent[transition_distribution_check$estimated_rating_no_ri == "E"],
  lower = transition_direction_check$percent[transition_direction_check$direction == "lower"],
  same = transition_direction_check$percent[transition_direction_check$direction == "same"],
  higher = transition_direction_check$percent[transition_direction_check$direction == "higher"]
)

###############################################################################
# Table 1 row "2003-2004 without Req Improvement": score the 2003-04 TAKS
# accountability rates with the same calculator, without required improvement.
# The public 2003-04 files have no completion or participation data, so this
# row uses test measures only (completion had no effect in the 2002-03 check).
# Also compares the calculator with Online Appendix Table 1 panel A.
###############################################################################

taks_0304_accountability <- read_transition_file(
  "data/aeis campus info/03-04/taks/camptaks311acc.dat",
  "data/aeis campus info/03-04/taks/camptaks311acc.lyt"
)

taks_0304_long <- taks_0304_accountability |>
  select(CAMPUS, matches("^C[ABHWE]311T[EMCSRW]04R$")) |>
  pivot_longer(cols = -CAMPUS,
               names_to = "variable",
               values_to = "performance_rate") |>
  extract(variable,
          into = c("group", "subject_code"),
          regex = "^C([ABHWE])311T([EMCSRW])04R$") |>
  mutate(
    CAMPUS = str_pad(as.character(CAMPUS), 9, "left", "0"),
    performance_rate = as.numeric(performance_rate)
  )

calculated_rating_0304_no_ri <- score_test_measures(build_taks_measures(taks_0304_long, build_rating_students(2004))) |>
  filter(evaluated_measures > 0) |>
  transmute(
    campus_number = CAMPUS,
    calculated_rating = case_when(
      passes_exemplary_tests ~ "E",
      passes_recognized_tests ~ "R",
      passes_acceptable_tests ~ "A",
      TRUE ~ "L"
    )
  )

rating_0304_comparison <- rating_panel |>
  filter(year == 2004L) |>
  select(campus_number, actual_rating = campus_rating) |>
  inner_join(calculated_rating_0304_no_ri, by = "campus_number") |>
  left_join(
    rating_panel |>
      filter(year == 2002L) |>
      select(campus_number, rating_2002 = campus_rating),
    by = "campus_number"
  )

appendix_table1a <- rating_0304_comparison |>
  count(
    actual_rating = factor(actual_rating, levels = c("L", "A", "R", "E")),
    calculated_rating = factor(calculated_rating, levels = c("L", "A", "R", "E")),
    .drop = FALSE
  ) |>
  mutate(percent = round(100 * n / sum(n), 1)) |>
  left_join(
    tibble(
      actual_rating = factor(rep(c("L", "A", "R", "E"), each = 4), levels = c("L", "A", "R", "E")),
      calculated_rating = factor(rep(c("L", "A", "R", "E"), 4), levels = c("L", "A", "R", "E")),
      published_percent = c(0.9, 0, 0, 0, 0.2, 55.2, 0, 0, 0, 7.0, 30.2, 0, 0, 0, 0, 6.5)
    ),
    by = c("actual_rating", "calculated_rating")
  )
write.csv(appendix_table1a,
          "tables/checks/appendix_table1a_comparison.csv",
          row.names = FALSE)

rated_both <- rating_0304_comparison |> filter(!is.na(rating_2002))
table1_without_ri_row <- tibble(
  year = 2004L,
  low = round(100 * mean(
    rating_0304_comparison$calculated_rating == "L"
  ), 1),
  acceptable = round(100 * mean(
    rating_0304_comparison$calculated_rating == "A"
  ), 1),
  recognized = round(100 * mean(
    rating_0304_comparison$calculated_rating == "R"
  ), 1),
  exemplary = round(100 * mean(
    rating_0304_comparison$calculated_rating == "E"
  ), 1),
  # Changes are relative to 2001-02, as for the 2003-04 row.
  higher = round(100 * mean(rating_values[rated_both$calculated_rating] >
                              rating_values[rated_both$rating_2002]), 1),
  lower = round(100 * mean(rating_values[rated_both$calculated_rating] <
                             rating_values[rated_both$rating_2002]), 1),
  same = round(
    100 * mean(rated_both$calculated_rating ==
                 rated_both$rating_2002),
    1
  ),
  row = "2004_without_required_improvement"
)

table1 <- bind_rows(
  table1 |>
    filter(year != 2003L) |>
    mutate(row = as.character(year)),
  table1_transition_row |>
    mutate(row = "2003_transition_estimate"),
  table1_without_ri_row
) |>
  arrange(year, row)

write.csv(table1, "tables/table1_rating_distributions.csv", row.names = FALSE)

transition_source_check <- tibble(
  check = c(
    "TAKS grades 3-11 one-SEM source campuses",
    "campuses with a nonmissing 2003 SDAA rate",
    "completion source campuses",
    "dropout source campuses",
    "2003 STUD campuses after >=200 filter",
    "campuses with test rating components",
    "campuses matched to a regular 2001-02 rating",
    "duplicate campus IDs in transition object"
  ),
  value = c(
    n_distinct(taks_311_1sem$CAMPUS),
    sum(!is.na(sdaa_2003$performance_rate)),
    n_distinct(completion_0203$CAMPUS),
    n_distinct(dropout_0203$CAMPUS),
    n_distinct(transition_students$CAMPUS),
    n_distinct(test_rating_components$CAMPUS),
    n_distinct(transition_rating_0203$campus_number),
    sum(duplicated(transition_rating_0203$campus_number))
  )
)

transition_measure_check <- transition_rating_0203 |>
  summarise(
    campuses = n(),
    min_evaluated_measures = min(evaluated_measures, na.rm = TRUE),
    median_evaluated_measures = median(evaluated_measures, na.rm = TRUE),
    max_evaluated_measures = max(evaluated_measures, na.rm = TRUE),
    campuses_with_zero_measures = sum(evaluated_measures == 0),
    campuses_using_exceptions = sum(failed_acceptable > 0 &
                                      passes_acceptable_tests, na.rm = TRUE),
    campuses_failing_acceptable_tests = sum(!passes_acceptable_tests, na.rm = TRUE)
  )

transition_non_test_check <- non_test_components |>
  summarise(
    campuses_with_non_test_data = n(),
    completion_nonmissing = sum(!is.na(completion_rate)),
    dropout_nonmissing = sum(!is.na(dropout_rate)),
    completion_evaluated = sum(completion_evaluated, na.rm = TRUE),
    dropout_evaluated = sum(dropout_evaluated, na.rm = TRUE),
    fail_acceptable_non_test = sum(!passes_acceptable_non_test, na.rm = TRUE),
    fail_recognized_non_test = sum(!passes_recognized_non_test, na.rm = TRUE),
    fail_exemplary_non_test = sum(!passes_exemplary_non_test, na.rm = TRUE)
  )

sdaa_check <- sdaa_2003 |>
  summarise(
    campuses = n(),
    nonmissing_sdaa = sum(!is.na(performance_rate)),
    mean_sdaa = mean(performance_rate, na.rm = TRUE),
    below_50 = sum(performance_rate < 50, na.rm = TRUE),
    below_70 = sum(performance_rate < 70, na.rm = TRUE),
    below_90 = sum(performance_rate < 90, na.rm = TRUE)
  )

transition_sample_check <- tibble(
  check = c(
    "regular 2001-02 ratings before 2003 enrollment restriction",
    "final transition sample",
    "final transition campuses with zero evaluated measures"
  ),
  value = c(
    n_distinct(prior_rating_2002$CAMPUS),
    nrow(transition_rating_0203),
    sum(transition_rating_0203$evaluated_measures == 0)
  )
)

stopifnot(
  !anyDuplicated(transition_rating_0203$campus_number),
  all(
    transition_rating_0203$estimated_rating_no_ri %in% c("L", "A", "R", "E")
  ),
  all(transition_rating_0203$rating_2002 %in% c("L", "A", "R", "E")),
  all(
    transition_rating_0203$rating_increased +
      transition_rating_0203$rating_dropped_to_L +
      transition_rating_0203$rating_dropped_to_A +
      transition_rating_0203$rating_dropped_to_R <= 1L
  )
)

dir.create("tables/checks",
           recursive = TRUE,
           showWarnings = FALSE)
write.csv(
  longitudinal_source_check,
  "tables/checks/longitudinal_source_check.csv",
  row.names = FALSE
)
write.csv(
  longitudinal_duplicate_check,
  "tables/checks/longitudinal_duplicate_check.csv",
  row.names = FALSE
)
write.csv(
  transition_distribution_check,
  "tables/checks/transition_distribution_check.csv",
  row.names = FALSE
)
write.csv(
  test_only_distribution_check,
  "tables/checks/transition_test_only_distribution.csv",
  row.names = FALSE
)
write.csv(
  transition_direction_check,
  "tables/checks/transition_direction_check.csv",
  row.names = FALSE
)
write.csv(transition_source_check,
          "tables/checks/transition_source_check.csv",
          row.names = FALSE)
write.csv(
  transition_measure_check,
  "tables/checks/transition_measure_check.csv",
  row.names = FALSE
)
write.csv(
  transition_non_test_check,
  "tables/checks/transition_non_test_check.csv",
  row.names = FALSE
)
write.csv(sdaa_check, "tables/checks/sdaa_check.csv", row.names = FALSE)
write.csv(transition_sample_check,
          "tables/checks/transition_sample_check.csv",
          row.names = FALSE)

saveRDS(
  list(
    longitudinal_source_check = longitudinal_source_check,
    longitudinal_duplicate_check = longitudinal_duplicate_check,
    transition_source_check = transition_source_check,
    transition_measure_check = transition_measure_check,
    transition_non_test_check = transition_non_test_check,
    sdaa_check = sdaa_check,
    transition_sample_check = transition_sample_check,
    test_only_distribution_check = test_only_distribution_check,
    transition_distribution_check = transition_distribution_check,
    transition_direction_check = transition_direction_check
  ),
  "tables/checks/replication_check_bundle.rds"
)

capture.output({
  cat("LONGITUDINAL SOURCE COUNTS\n")
  print(as_tibble(longitudinal_source_check), n = Inf)
  cat("\nDUPLICATE KEY CHECK\n")
  print(as_tibble(longitudinal_duplicate_check), n = Inf)
  cat("\nTRANSITION SOURCE COUNTS\n")
  print(as_tibble(transition_source_check), n = Inf)
  cat("\nTRANSITION MEASURE CHECK\n")
  print(as_tibble(transition_measure_check), width = Inf)
  cat("\nNON-TEST INDICATOR CHECK\n")
  print(as_tibble(transition_non_test_check), width = Inf)
  cat("\nSDAA CHECK\n")
  print(as_tibble(sdaa_check), width = Inf)
  cat("\nTRANSITION SAMPLE CHECK\n")
  print(as_tibble(transition_sample_check), n = Inf)
  cat("\nTEST-ONLY RATING DISTRIBUTION\n")
  print(as_tibble(test_only_distribution_check), n = Inf)
  cat("\nFINAL TRANSITION DISTRIBUTION\n")
  print(as_tibble(transition_distribution_check), n = Inf)
  cat("\nTRANSITION DIRECTION\n")
  print(as_tibble(transition_direction_check), n = Inf)
}, file = "tables/checks/checks_to_send.txt")

message("\n===== TRANSITION RATING CHECKS: SEND THESE OUTPUTS =====")
print(as_tibble(longitudinal_source_check), n = Inf)
print(as_tibble(longitudinal_duplicate_check), n = Inf)
print(as_tibble(transition_source_check), n = Inf)
print(as_tibble(transition_measure_check), width = Inf)
print(as_tibble(transition_non_test_check), width = Inf)
print(as_tibble(sdaa_check), width = Inf)
print(as_tibble(transition_sample_check), n = Inf)
print(as_tibble(test_only_distribution_check), n = Inf)
print(as_tibble(transition_distribution_check), n = Inf)
print(as_tibble(transition_direction_check), n = Inf)

###############################################################################
# 5. Tables 3-4 and common resource construction
###############################################################################

# The sourced file uses data_ref_longitudinal, data_stud_longitudinal,
# staff_longitudinal, and finance_longitudinal_9798_0708 created above.
# If transition_rating_0203 has already been loaded, it also creates the common
# analysis dataset used below.

###############################################################################
# BEGIN INLINED MODULE: replicate_craig_remaining_tables.R
###############################################################################
# Craig, Imberman, and Perdue: Tables 3-10
#
# Run this file after creating:
#   data_ref_longitudinal
#   data_stud_longitudinal
#   finance_longitudinal_9798_0708 (preferred after adding Actual08), or
#   finance_longitudinal_9798_0607
#
# Tables 3 and the expenditure portion of Table 4 run with those objects.
# Later tables run only after the additional derived inputs described at the end
# of this file have been created.

required_objects <- c("data_ref_longitudinal", "data_stud_longitudinal")
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1), envir = .GlobalEnv)]
if (length(missing_objects) > 0L) {
  stop(
    "Run the rating, student, and finance construction code first. Missing: ",
    paste(missing_objects, collapse = ", ")
  )
}
if (exists("finance_longitudinal_9798_0708", envir = .GlobalEnv)) {
  finance_panel <- finance_longitudinal_9798_0708
} else if (exists("finance_longitudinal_9798_0607", envir = .GlobalEnv)) {
  finance_panel <- finance_longitudinal_9798_0607
  warning(
    "Using finance_longitudinal_9798_0607. Add Actual08 and rebuild as ",
    "finance_longitudinal_9798_0708 to populate the t+1 resource outcomes ",
    "for ratings issued in 2006-07."
  )
} else {
  stop(
    "Run the finance construction code first. Expected either ",
    "finance_longitudinal_9798_0708 or finance_longitudinal_9798_0607."
  )
}
dir.create("tables", showWarnings = FALSE, recursive = TRUE)


###############################################################################
# 1. Clean student counts and create the common rating-year sample
###############################################################################

student_panel <- data_stud_longitudinal |>
  mutate(
    campus_number = str_pad(as.character(campus_number), 9, "left", "0"),
    year = as.integer(year),
    across(-c(campus_number, year), ~ {
      value <- suppressWarnings(as.numeric(na_if(as.character(.x), ".")))
      value[value < 0] <- NA_real_
      value
    })
  )

rating_panel <- data_ref_longitudinal |>
  mutate(campus_number = str_pad(as.character(campus_number), 9, "left", "0"),
         year = as.integer(year)) |>
  filter(campus_rating %in% c("L", "A", "R", "E")) |>
  inner_join(student_panel, by = c("campus_number", "year")) |>
  filter(campus_total_students >= 200) |>
  mutate(
    regime = case_when(
      year %in% 1998:2002 ~ "TAAS",
      year %in% 2004:2007 ~ "TAKS",
      TRUE ~ NA_character_
    ),
    campus_rating = factor(campus_rating, levels = c("L", "A", "R", "E"))
  ) |>
  filter(!is.na(regime))

duplicate_rating_rows <- rating_panel |>
  count(campus_number, year) |>
  filter(n > 1L)

if (nrow(duplicate_rating_rows) > 0L) {
  stop("The rating/student panel contains duplicate campus-years.")
}


###############################################################################
# 2. Table 3: School characteristics by rating
###############################################################################

table3_analysis <- rating_panel |>
  mutate(
    pct_asian = 100 * campus_asian_students / campus_total_students,
    pct_black = 100 * campus_black_students / campus_total_students,
    pct_hispanic = 100 * campus_hispanic_students / campus_total_students,
    pct_white = 100 * campus_white_students / campus_total_students,
    pct_economically_disadvantaged =
      100 * campus_ecodis_students / campus_total_students,
    pct_lep = 100 * campus_lep_students / campus_total_students,
    pct_special_education =
      100 * campus_sped_students / campus_total_students,
    pct_gifted = 100 * campus_giftal_students / campus_total_students,
    enrollment = campus_total_students
  )

table3_long <- table3_analysis |>
  select(
    regime,
    campus_rating,
    pct_asian,
    pct_black,
    pct_hispanic,
    pct_white,
    pct_economically_disadvantaged,
    pct_lep,
    pct_special_education,
    pct_gifted,
    enrollment
  ) |>
  pivot_longer(
    cols = -c(regime, campus_rating),
    names_to = "characteristic",
    values_to = "value"
  ) |>
  group_by(regime, campus_rating, characteristic) |>
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    nonmissing = sum(!is.na(value)),
    .groups = "drop"
  ) |>
  mutate(mean = round(mean, 1), sd = round(sd, 1))

table3_observations <- table3_analysis |>
  count(regime, campus_rating, name = "observations")

table3_display <- table3_long |>
  select(regime, campus_rating, characteristic, mean, sd) |>
  pivot_wider(
    names_from = c(regime, campus_rating),
    values_from = c(mean, sd),
    names_glue = "{regime}_{campus_rating}_{.value}"
  )

write.csv(table3_display,
          "tables/table3_school_characteristics.csv",
          row.names = FALSE)

write.csv(table3_observations,
          "tables/table3_observations.csv",
          row.names = FALSE)

print(as_tibble(table3_display), width = Inf)
print(as_tibble(table3_observations), n = Inf)


###############################################################################
# 3. Construct school resources by outcome year
###############################################################################

# BLS CPI-U annual averages. The paper reports expenditures in 2007 dollars.
cpi_u <- tibble(
  outcome_year = 1998:2008,
  cpi = c(
    163.0,
    166.6,
    172.2,
    177.1,
    179.9,
    184.0,
    188.9,
    195.3,
    201.6,
    207.342,
    215.303
  )
)

outcome_enrollment <- student_panel |>
  transmute(campus_number,
            outcome_year = year,
            outcome_enrollment = campus_total_students)

# 2007-08 STUD enrollment is not in the data, but Table 4 uses resources in
# t+1 for TAKS years through 2006-07. Approximate each campus's 2007-08
# enrollment with its 2006-07 enrollment so 2007-08 spending is included.
outcome_enrollment <- bind_rows(
  outcome_enrollment,
  outcome_enrollment |>
    filter(outcome_year == 2007L) |>
    mutate(outcome_year = 2008L) |>
    anti_join(outcome_enrollment, by = c("campus_number", "outcome_year"))
)

resources_by_year <- finance_panel |>
  transmute(
    campus_number = str_pad(as.character(CAMPUS_ID), 9, "left", "0"),
    district = str_pad(as.character(DISTRICT), 6, "left", "0"),
    outcome_year = as.integer(FINANCE_END_YEAR),
    total_operating = as.numeric(TOTAL_OPERATING),
    instruction = as.numeric(INSTRUCTION),
    admin_training = as.numeric(ADMIN_TRAINING),
    counseling = as.numeric(COUNSELING),
    extracurricular = as.numeric(EXTRACURRICULAR)
  ) |>
  left_join(outcome_enrollment, by = c("campus_number", "outcome_year")) |>
  left_join(cpi_u, by = "outcome_year") |>
  mutate(
    inflation_to_2007 = 207.342 / cpi,
    total_operating_pp =
      total_operating / outcome_enrollment * inflation_to_2007,
    instruction_pp =
      instruction / outcome_enrollment * inflation_to_2007,
    admin_training_pp =
      admin_training / outcome_enrollment * inflation_to_2007,
    counseling_pp =
      counseling / outcome_enrollment * inflation_to_2007,
    extracurricular_pp =
      extracurricular / outcome_enrollment * inflation_to_2007
  )

missing_outcome_enrollment <- resources_by_year |>
  filter(is.na(outcome_enrollment)) |>
  count(outcome_year, name = "finance_rows_missing_enrollment")

unexpected_missing_outcome_enrollment <- missing_outcome_enrollment |>
  filter(outcome_year <= max(student_panel$year, na.rm = TRUE))

if (nrow(unexpected_missing_outcome_enrollment) > 0L) {
  warning(
    "Some finance campus-years still have no matching outcome-year STUD ",
    "record. Inspect tables/checks/resource_merge_check.csv."
  )
}

###############################################################################
# 4. Add teacher FTE if a staffing panel has been constructed
###############################################################################

# Expected staff_longitudinal columns:
#   campus_number, year, teacher_fte, beginning_teacher_fte,
#   teacher_1_5_fte, teacher_6_10_fte, teacher_11_20_fte,
#   teacher_over_20_fte, teacher_experience, aide_fte
# where year is the later year of the academic year.

if (exists("staff_longitudinal", envir = .GlobalEnv)) {
  staff_for_merge <- staff_longitudinal |>
    transmute(
      campus_number = str_pad(as.character(campus_number), 9, "left", "0"),
      outcome_year = as.integer(year),
      teacher_fte = as.numeric(teacher_fte),
      beginning_teacher_fte = as.numeric(beginning_teacher_fte),
      teacher_1_5_fte = as.numeric(teacher_1_5_fte),
      teacher_6_10_fte = as.numeric(teacher_6_10_fte),
      teacher_11_20_fte = as.numeric(teacher_11_20_fte),
      teacher_over_20_fte = as.numeric(teacher_over_20_fte),
      teacher_experience = as.numeric(teacher_experience),
      aide_fte = as.numeric(aide_fte)
    )
  
  resources_by_year <- resources_by_year |>
    left_join(staff_for_merge, by = c("campus_number", "outcome_year")) |>
    mutate(
      student_teacher_ratio = if_else(teacher_fte > 0, outcome_enrollment / teacher_fte, NA_real_),
      pct_beginning_teachers = if_else(
        teacher_fte > 0,
        100 * beginning_teacher_fte / teacher_fte,
        NA_real_
      ),
      pct_teachers_1_5 = if_else(teacher_fte > 0, 100 * teacher_1_5_fte / teacher_fte, NA_real_),
      pct_teachers_over_5 = if_else(
        teacher_fte > 0,
        100 * (teacher_6_10_fte + teacher_11_20_fte + teacher_over_20_fte) /
          teacher_fte,
        NA_real_
      ),
      student_aide_ratio = if_else(aide_fte > 0, outcome_enrollment / aide_fte, NA_real_)
    )
} else {
  resources_by_year$student_teacher_ratio <- NA_real_
  message(
    "Table 4 student-teacher ratio is unavailable until staff_longitudinal ",
    "is constructed from the annual campstaf files."
  )
}

###############################################################################
# 5. Table 4: Mean resources in year t+1 by rating in year t
###############################################################################

rating_for_resources <- rating_panel |>
  select(campus_number, rating_year = year, regime, campus_rating)

table4_analysis <- rating_for_resources |>
  mutate(outcome_year = rating_year + 1L) |>
  inner_join(resources_by_year, by = c("campus_number", "outcome_year")) |>
  filter(!is.na(outcome_enrollment), !is.na(total_operating_pp))

table4_long <- table4_analysis |>
  select(
    regime,
    campus_rating,
    total_operating_pp,
    instruction_pp,
    admin_training_pp,
    counseling_pp,
    extracurricular_pp,
    student_teacher_ratio
  ) |>
  pivot_longer(
    cols = -c(regime, campus_rating),
    names_to = "resource",
    values_to = "value"
  ) |>
  group_by(regime, campus_rating, resource) |>
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    nonmissing = sum(!is.na(value)),
    .groups = "drop"
  ) |>
  mutate(mean = round(mean, 1), sd = round(sd, 1))

table4_display <- table4_long |>
  select(regime, campus_rating, resource, mean, sd) |>
  pivot_wider(
    names_from = c(regime, campus_rating),
    values_from = c(mean, sd),
    names_glue = "{regime}_{campus_rating}_{.value}"
  )

table4_observations <- table4_analysis |>
  count(regime, campus_rating, name = "observations")

write.csv(table4_display,
          "tables/table4_mean_resources.csv",
          row.names = FALSE)

write.csv(table4_observations,
          "tables/table4_observations.csv",
          row.names = FALSE)

print(as_tibble(table4_display), width = Inf)
print(as_tibble(table4_observations), n = Inf)


###############################################################################
# 6. Inputs required for Tables 5-10: rating-shock regressions
###############################################################################

# Create transition_rating_0203 from the 2002-03 TAKS files. It must contain:
#   campus_number
#   estimated_rating_no_ri  (L/A/R/E under the 2003-04 rules)
#   rating_increased
#   rating_dropped_to_L
#   rating_dropped_to_A
#   rating_dropped_to_R
#
# The rating must be calculated from subject-by-student-group tested and passing
# counts, minimum cell-size rules, completion/dropout requirements, and the
# 2003-04 cutoffs. Required improvement is intentionally omitted for this
# transition-year estimate.

if (!exists("transition_rating_0203", envir = .GlobalEnv)) {
  message("Tables 5-10 are not estimated: transition_rating_0203 is missing.")
} else {
  transition_demographics <- student_panel |>
    filter(year == 2003L) |>
    mutate(
      pct_asian = 100 * campus_asian_students / campus_total_students,
      pct_black = 100 * campus_black_students / campus_total_students,
      pct_hispanic = 100 * campus_hispanic_students / campus_total_students,
      pct_native =
        100 * campus_native_american_students / campus_total_students,
      pct_economically_disadvantaged =
        100 * campus_ecodis_students / campus_total_students,
      pct_lep = 100 * campus_lep_students / campus_total_students,
      pct_special_education =
        100 * campus_sped_students / campus_total_students,
      pct_gifted = 100 * campus_giftal_students / campus_total_students,
      pct_vocational = 100 * campus_voc_students / campus_total_students,
      pct_grade_EE = 100 * campus_grade_EE_students / campus_total_students,
      pct_grade_PK = 100 * campus_grade_PK_students / campus_total_students,
      pct_grade_KG = 100 * campus_grade_KG_students / campus_total_students,
      pct_grade_01 = 100 * campus_grade_01_students / campus_total_students,
      pct_grade_02 = 100 * campus_grade_02_students / campus_total_students,
      pct_grade_03 = 100 * campus_grade_03_students / campus_total_students,
      pct_grade_04 = 100 * campus_grade_04_students / campus_total_students,
      pct_grade_05 = 100 * campus_grade_05_students / campus_total_students,
      pct_grade_06 = 100 * campus_grade_06_students / campus_total_students,
      pct_grade_07 = 100 * campus_grade_07_students / campus_total_students,
      pct_grade_08 = 100 * campus_grade_08_students / campus_total_students,
      pct_grade_09 = 100 * campus_grade_09_students / campus_total_students,
      pct_grade_10 = 100 * campus_grade_10_students / campus_total_students,
      pct_grade_11 = 100 * campus_grade_11_students / campus_total_students,
      pct_grade_12 = 100 * campus_grade_12_students / campus_total_students
    )
  
  prior_ratings <- rating_panel |>
    filter(year %in% 2000:2002) |>
    select(campus_number, year, campus_rating) |>
    mutate(year = paste0("rating_", year)) |>
    pivot_wider(names_from = year, values_from = campus_rating)
  
  resource_changes <- resources_by_year |>
    filter(outcome_year %in% c(2002L, 2003L, 2004L, 2006L)) |>
    select(
      campus_number,
      outcome_year,
      total_operating_pp,
      instruction_pp,
      admin_training_pp,
      counseling_pp,
      extracurricular_pp,
      student_teacher_ratio
    ) |>
    pivot_wider(
      names_from = outcome_year,
      values_from = c(
        total_operating_pp,
        instruction_pp,
        admin_training_pp,
        counseling_pp,
        extracurricular_pp,
        student_teacher_ratio
      )
    ) |>
    mutate(
      change_0203_total = total_operating_pp_2003 - total_operating_pp_2002,
      change_0203_instruction = instruction_pp_2003 - instruction_pp_2002,
      change_0203_admin_training =
        admin_training_pp_2003 - admin_training_pp_2002,
      change_0203_counseling = counseling_pp_2003 - counseling_pp_2002,
      change_0203_extracurricular =
        extracurricular_pp_2003 - extracurricular_pp_2002,
      change_0203_student_teacher =
        student_teacher_ratio_2003 - student_teacher_ratio_2002,
      change_0304_total = total_operating_pp_2004 - total_operating_pp_2003,
      change_0304_instruction = instruction_pp_2004 - instruction_pp_2003,
      change_0304_admin_training =
        admin_training_pp_2004 - admin_training_pp_2003,
      change_0304_counseling = counseling_pp_2004 - counseling_pp_2003,
      change_0304_extracurricular =
        extracurricular_pp_2004 - extracurricular_pp_2003,
      change_0304_student_teacher =
        student_teacher_ratio_2004 - student_teacher_ratio_2003,
      change_0306_total = total_operating_pp_2006 - total_operating_pp_2003,
      change_0306_instruction = instruction_pp_2006 - instruction_pp_2003,
      change_0306_admin_training =
        admin_training_pp_2006 - admin_training_pp_2003,
      change_0306_counseling = counseling_pp_2006 - counseling_pp_2003,
      change_0306_extracurricular =
        extracurricular_pp_2006 - extracurricular_pp_2003,
      change_0306_student_teacher =
        student_teacher_ratio_2006 - student_teacher_ratio_2003
    )
  # Paper's sample note for Tables 5-10: "Sample is limited to schools that
  # received an L, A, R, or E rating in 1999-2000, 2000-2001, 2001-2002 and
  # 2003-2004." rating_panel already enforces L/A/R/E and >= 200 students.
  rated_2004 <- rating_panel |>
    filter(year == 2004L) |>
    distinct(campus_number)
  rating_shock_analysis <- transition_rating_0203 |>
    mutate(campus_number = str_pad(as.character(campus_number), 9, "left", "0")) |>
    select(-any_of(c(
      "rating_2002", "rating_2001", "rating_2000"
    ))) |>
    inner_join(prior_ratings, by = "campus_number") |>
    semi_join(rated_2004, by = "campus_number") |>
    inner_join(transition_demographics, by = "campus_number") |>
    inner_join(resource_changes, by = "campus_number")
  demographic_changes <- student_panel |>
    filter(year %in% c(2002L, 2003L, 2004L)) |>
    mutate(
      pct_black = 100 * campus_black_students / campus_total_students,
      pct_hispanic = 100 * campus_hispanic_students / campus_total_students,
      pct_economically_disadvantaged =
        100 * campus_ecodis_students / campus_total_students,
      pct_lep = 100 * campus_lep_students / campus_total_students,
      pct_gifted = 100 * campus_giftal_students / campus_total_students,
      pct_special_education =
        100 * campus_sped_students / campus_total_students
    ) |>
    select(
      campus_number,
      year,
      pct_black,
      pct_hispanic,
      pct_economically_disadvantaged,
      pct_lep,
      pct_gifted,
      pct_special_education
    ) |>
    pivot_wider(
      names_from = year,
      values_from = c(
        pct_black,
        pct_hispanic,
        pct_economically_disadvantaged,
        pct_lep,
        pct_gifted,
        pct_special_education
      )
    ) |>
    mutate(
      change_0203_pct_black = pct_black_2003 - pct_black_2002,
      change_0203_pct_hispanic = pct_hispanic_2003 - pct_hispanic_2002,
      change_0203_pct_economically_disadvantaged =
        pct_economically_disadvantaged_2003 -
        pct_economically_disadvantaged_2002,
      change_0203_pct_lep = pct_lep_2003 - pct_lep_2002,
      change_0203_pct_gifted = pct_gifted_2003 - pct_gifted_2002,
      change_0203_pct_special_education =
        pct_special_education_2003 - pct_special_education_2002,
      change_0304_pct_black = pct_black_2004 - pct_black_2003,
      change_0304_pct_hispanic = pct_hispanic_2004 - pct_hispanic_2003,
      change_0304_pct_economically_disadvantaged =
        pct_economically_disadvantaged_2004 -
        pct_economically_disadvantaged_2003,
      change_0304_pct_lep = pct_lep_2004 - pct_lep_2003,
      change_0304_pct_gifted = pct_gifted_2004 - pct_gifted_2003,
      change_0304_pct_special_education =
        pct_special_education_2004 - pct_special_education_2003
    )
  rating_shock_analysis <- rating_shock_analysis |>
    left_join(demographic_changes, by = "campus_number")
  if (exists("staff_longitudinal", envir = .GlobalEnv)) {
    staffing_changes <- resources_by_year |>
      filter(outcome_year %in% c(2003L, 2004L)) |>
      select(
        campus_number,
        outcome_year,
        outcome_enrollment,
        teacher_fte,
        teacher_experience,
        pct_beginning_teachers,
        pct_teachers_1_5,
        pct_teachers_over_5,
        student_aide_ratio
      ) |>
      pivot_wider(
        names_from = outcome_year,
        values_from = c(
          outcome_enrollment,
          teacher_fte,
          teacher_experience,
          pct_beginning_teachers,
          pct_teachers_1_5,
          pct_teachers_over_5,
          student_aide_ratio
        )
      ) |>
      mutate(
        change_0304_enrollment =
          outcome_enrollment_2004 - outcome_enrollment_2003,
        change_0304_teacher_fte = teacher_fte_2004 - teacher_fte_2003,
        change_0304_teacher_experience =
          teacher_experience_2004 - teacher_experience_2003,
        change_0304_pct_beginning_teachers =
          pct_beginning_teachers_2004 - pct_beginning_teachers_2003,
        change_0304_pct_teachers_1_5 =
          pct_teachers_1_5_2004 - pct_teachers_1_5_2003,
        change_0304_pct_teachers_over_5 =
          pct_teachers_over_5_2004 - pct_teachers_over_5_2003,
        change_0304_student_aide_ratio =
          student_aide_ratio_2004 - student_aide_ratio_2003
      )
    rating_shock_analysis <- rating_shock_analysis |>
      left_join(staffing_changes, by = "campus_number")
  }
  write.csv(
    rating_shock_analysis,
    "tables/table5_10_rating_shock_analysis_data.csv",
    row.names = FALSE
  )
  message(
    "The Table 5-10 analysis dataset was created. Estimate each outcome on ",
    "rating_dropped_to_L, rating_dropped_to_A, rating_dropped_to_R, ",
    "rating_increased, prior-year rating indicators, grade shares, and the ",
    "demographic shares. Use clustered robust standard errors."
  )
}


###############################################################################
# 8. Replication-status inventory
###############################################################################

replication_status <- tibble(
  table = c("3", "4", "5", "6", "7", "8", "9", "10"),
  status = c(
    "Runs with current REF/STUD panels; approximate because appeal and special-analysis flags are unavailable",
    "Runs with current finance/staff panels; uses unfiltered STUD enrollment for per-pupil denominators and applies the 200-student rule to the rating sample",
    "Runs with the approximate transition rating; uses changes from 2002-03 to 2003-04",
    "Runs with the approximate transition rating; placebo uses changes from 2001-02 to 2002-03",
    "Runs with the approximate transition rating; uses prior demographic changes from 2001-02 to 2002-03",
    "Runs with the approximate transition rating and expanded staff panel",
    "Estimates all six panels (HHI, share R/E, share falling, >20 schools, single-elementary districts) from the approximate transition rating",
    "Runs with the approximate transition rating; uses changes from 2002-03 to 2005-06"
  )
)

print(replication_status, n = Inf, width = Inf)

write.csv(replication_status,
          "tables/replication_status_tables_3_10.csv",
          row.names = FALSE)
###############################################################################
# END INLINED MODULE: replicate_craig_remaining_tables.R
###############################################################################


###############################################################################
# 6. Cluster-robust regression helpers for Tables 5-10
###############################################################################

cluster_vcov <- function(model, cluster) {
  X <- model.matrix(model)
  estimable <- !is.na(coef(model))
  X <- X[, estimable, drop = FALSE]
  u <- residuals(model)
  cl <- as.factor(cluster)
  N <- nrow(X)
  K <- ncol(X)
  G <- nlevels(cl)
  
  if (length(cl) != N) {
    stop("The cluster vector and regression sample have different lengths.")
  }
  
  if (G < 2) {
    stop("At least two districts are required for clustered standard errors.")
  }
  
  xu <- rowsum(X * as.numeric(u), cl, reorder = FALSE)
  bread <- solve(crossprod(X))
  correction <- (G / (G - 1)) * ((N - 1) / (N - K))
  vcov_cluster <- correction * bread %*% crossprod(xu) %*% bread
  attr(vcov_cluster, "clusters") <- G
  vcov_cluster
}

tidy_cluster_lm <- function(model, cluster, outcome, table_name) {
  vc <- cluster_vcov(model, cluster)
  estimate <- coef(model)
  estimate <- estimate[!is.na(estimate)]
  standard_error <- sqrt(diag(vc))
  statistic <- estimate / standard_error
  number_of_clusters <- attr(vc, "clusters")
  p_value <- 2 * pt(abs(statistic), df = number_of_clusters - 1, lower.tail = FALSE)
  tibble(
    table = table_name,
    outcome,
    term = names(estimate),
    estimate = as.numeric(estimate),
    standard_error = as.numeric(standard_error),
    statistic = as.numeric(statistic),
    p_value = as.numeric(p_value),
    observations = nobs(model),
    districts = number_of_clusters,
    residual_df = model$df.residual,
    model_rank = model$rank
  )
}

shock_terms <- c(
  "rating_dropped_to_L",
  "rating_dropped_to_A",
  "rating_dropped_to_R",
  "rating_increased"
)

demographic_controls <- c(
  "pct_black",
  "pct_hispanic",
  "pct_asian",
  "pct_native",
  "pct_economically_disadvantaged",
  "pct_lep",
  "pct_special_education",
  "pct_gifted",
  "pct_vocational"
)

# Equation (1): outcome on the four rating-shock indicators, L/A/R indicators
# for each of the three prior TAAS years (Exemplary omitted), grade shares, and
# demographic shares. `extra_terms` adds Table 9 moderators/interactions.
fit_rating_shock_model <- function(data, outcome, extra_terms = NULL) {
  # Grade shares sum to 100, so omit grade 12 as the reference category.
  grade_controls <- setdiff(grep("^pct_grade_", names(data), value = TRUE), "pct_grade_12")
  rating_columns <- c("rating_2002", "rating_2001", "rating_2000")
  needed <- unique(
    c(
      outcome,
      shock_terms,
      extra_terms,
      rating_columns,
      grade_controls,
      demographic_controls,
      "district"
    )
  )
  missing_variables <- setdiff(needed, names(data))
  if (length(missing_variables) > 0) {
    stop(
      "Missing variables for outcome ",
      outcome,
      ": ",
      paste(missing_variables, collapse = ", ")
    )
  }
  
  model_data <- data |>
    select(all_of(needed)) |>
    filter(if_all(everything(), ~ !is.na(.x))) |>
    mutate(district = as.factor(district), across(all_of(rating_columns), ~ relevel(factor(
      .x, levels = c("E", "L", "A", "R")
    ), ref = "E"))) |>
    droplevels()
  if (nrow(model_data) == 0) {
    stop("No complete observations for ", outcome)
  }
  
  # Small subsamples (Table 9, panel VI) can leave a prior-rating factor with
  # one level; such a factor is collinear with the intercept, so drop it.
  usable_ratings <- rating_columns[vapply(rating_columns, function(column)
    nlevels(model_data[[column]]) > 1, logical(1))]
  rhs <- paste(
    c(
      shock_terms,
      extra_terms,
      paste0("factor(", usable_ratings, ")"),
      grade_controls,
      demographic_controls
    ),
    collapse = " + "
  )
  model <- lm(as.formula(paste(outcome, "~", rhs)), data = model_data)
  vc <- cluster_vcov(model, model_data$district)
  list(
    model = model,
    cluster = model_data$district,
    vcov = vc,
    clusters = attr(vc, "clusters")
  )
}

run_published_table <- function(data, outcomes, table_name) {
  bind_rows(lapply(outcomes, function(outcome) {
    fit <- fit_rating_shock_model(data, outcome)
    tidy_cluster_lm(fit$model, fit$cluster, outcome, table_name)
  }))
}

###############################################################################
# 7. Estimate Tables 5-8 and 10 when the transition rating is available
###############################################################################

if (exists("rating_shock_analysis", envir = .GlobalEnv)) {
  rating_shock_analysis <- rating_shock_analysis |>
    mutate(district = substr(campus_number, 1, 6))
  
  table5_outcomes <- c(
    "change_0304_total",
    "change_0304_instruction",
    "change_0304_admin_training",
    "change_0304_counseling",
    "change_0304_extracurricular",
    "change_0304_student_teacher"
  )
  table6_outcomes <- c(
    "change_0203_total",
    "change_0203_instruction",
    "change_0203_admin_training",
    "change_0203_counseling",
    "change_0203_extracurricular",
    "change_0203_student_teacher"
  )
  table7_outcomes <- c(
    "change_0203_pct_black",
    "change_0203_pct_hispanic",
    "change_0203_pct_economically_disadvantaged",
    "change_0203_pct_lep",
    "change_0203_pct_gifted",
    "change_0203_pct_special_education"
  )
  table8_outcomes <- c(
    "change_0304_enrollment",
    "change_0304_teacher_fte",
    "change_0304_teacher_experience",
    "change_0304_pct_beginning_teachers",
    "change_0304_pct_teachers_1_5",
    "change_0304_pct_teachers_over_5",
    "change_0304_student_aide_ratio"
  )
  table10_outcomes <- c(
    "change_0306_total",
    "change_0306_instruction",
    "change_0306_admin_training",
    "change_0306_counseling",
    "change_0306_extracurricular",
    "change_0306_student_teacher"
  )
  
  table5 <- run_published_table(rating_shock_analysis, table5_outcomes, "Table 5")
  table6 <- run_published_table(rating_shock_analysis, table6_outcomes, "Table 6")
  table7 <- run_published_table(rating_shock_analysis, table7_outcomes, "Table 7")
  table8 <- run_published_table(rating_shock_analysis, table8_outcomes, "Table 8")
  table10 <- run_published_table(rating_shock_analysis, table10_outcomes, "Table 10")
  
  write.csv(table5, "tables/table5_models.csv", row.names = FALSE)
  write.csv(table6, "tables/table6_models.csv", row.names = FALSE)
  write.csv(table7, "tables/table7_models.csv", row.names = FALSE)
  write.csv(table8, "tables/table8_models.csv", row.names = FALSE)
  write.csv(table10, "tables/table10_models.csv", row.names = FALSE)
  
  # Table 5 joint test that the three "drop" coefficients are all zero
  # (published row "Joint significance test for any drop in rating").
  table5_joint_f <- bind_rows(lapply(table5_outcomes, function(outcome) {
    fit <- fit_rating_shock_model(rating_shock_analysis, outcome)
    drop_terms <- c("rating_dropped_to_L",
                    "rating_dropped_to_A",
                    "rating_dropped_to_R")
    b <- coef(fit$model)[drop_terms]
    V <- fit$vcov[drop_terms, drop_terms]
    f_stat <- as.numeric(t(b) %*% solve(V, b)) / length(drop_terms)
    tibble(
      table = "Table 5",
      outcome = outcome,
      f_statistic = f_stat,
      p_value = pf(f_stat, length(drop_terms), fit$clusters - 1L, lower.tail = FALSE)
    )
  }))
  write.csv(table5_joint_f, "tables/table5_joint_f.csv", row.names = FALSE)
  
  # Table 9 district characteristics. Ratings shares use every campus with an
  # estimated 2002-03 rating in the district, not only the regression sample.
  district_transition <- transition_rating_0203 |>
    mutate(
      campus_number = str_pad(as.character(campus_number), 9, "left", "0"),
      district = substr(campus_number, 1, 6),
      any_drop = rating_dropped_to_L + rating_dropped_to_A + rating_dropped_to_R
    ) |>
    group_by(district) |>
    summarise(
      district_school_count = n(),
      rating_hhi = sum(table(
        factor(estimated_rating_no_ri, levels = c("L", "A", "R", "E"))
      )^2) /
        n()^2,
      share_predicted_R_or_E = mean(estimated_rating_no_ri %in% c("R", "E")),
      share_rating_falls = mean(any_drop > 0, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Elementary-school counts and campus type come from the last TAAS year
  # (2001-02) REF file.
  campus_type_2002 <- ref_panel |>
    filter(year == 2002L) |>
    select(campus_number, campus_type_2002 = campus_type)
  # Elementary-school counts use every campus in the 2002-03 REF file (the
  # transition year); this reproduces the published panel VI sample size.
  district_elementary <- ref_panel |>
    filter(year == 2003L) |>
    group_by(district) |>
    summarise(elementary_schools = sum(campus_type == "E"),
              .groups = "drop")
  
  # Online Appendix Tables 5 and 8 define district size by the rated schools in
  # the estimation sample ("districts with five or more schools that received
  # an L, A, R, or E rating"; "Large District (At Least 20 Schools)").
  table9_analysis <- rating_shock_analysis |>
    left_join(district_transition, by = "district") |>
    left_join(district_elementary, by = "district") |>
    left_join(campus_type_2002, by = "campus_number") |>
    group_by(district) |>
    mutate(district_sample_schools = n()) |>
    ungroup() |>
    mutate(district_20_plus = as.integer(district_sample_schools >= 20L))
  
  write.csv(table9_analysis,
            "tables/table9_interaction_analysis_data.csv",
            row.names = FALSE)
  
  # Each published panel is a separate regression of the Table 5 model plus the
  # moderator and its interaction with every shock indicator (Online Appendix
  # Tables 5-10). Table 9 reports only the "falls to A" main effect and its
  # interaction, stored here as `interaction`.
  table9_specs <- list(
    I_baseline = list(moderator = NULL, filter = quote(TRUE)),
    II_hhi = list(
      moderator = "rating_hhi",
      filter = quote(district_sample_schools >= 5)
    ),
    III_share_R_or_E = list(moderator = "share_predicted_R_or_E", filter = quote(TRUE)),
    IV_share_falls = list(moderator = "share_rating_falls", filter = quote(TRUE)),
    V_over_20_schools = list(moderator = "district_20_plus", filter = quote(TRUE)),
    # Appendix Table 10 has 627 observations: all sample schools in districts
    # with a single elementary school.
    VI_single_elementary = list(
      moderator = NULL,
      filter = quote(elementary_schools == 1)
    )
  )
  
  table9 <- bind_rows(lapply(names(table9_specs), function(panel) {
    spec <- table9_specs[[panel]]
    panel_data <- table9_analysis |> filter(!!spec$filter)
    extra_terms <- NULL
    if (!is.null(spec$moderator)) {
      m <- panel_data[[spec$moderator]]
      panel_data <- panel_data |>
        mutate(
          interaction = rating_dropped_to_A * m,
          interaction_L = rating_dropped_to_L * m,
          interaction_R = rating_dropped_to_R * m,
          interaction_increase = rating_increased * m
        )
      extra_terms <- c(
        spec$moderator,
        "interaction",
        "interaction_L",
        "interaction_R",
        "interaction_increase"
      )
    }
    bind_rows(lapply(table5_outcomes, function(outcome) {
      fit <- fit_rating_shock_model(panel_data, outcome, extra_terms)
      tidy_cluster_lm(fit$model, fit$cluster, outcome, "Table 9") |>
        mutate(panel = panel, .after = table)
    }))
  }))
  
  write.csv(table9, "tables/table9_models.csv", row.names = FALSE)
  
  table9_district_check <- table9 |>
    distinct(panel, observations) |>
    left_join(tibble(
      panel = c(
        "I_baseline",
        "II_hhi",
        "III_share_R_or_E",
        "IV_share_falls",
        "V_over_20_schools",
        "VI_single_elementary"
      ),
      published_observations = c(4958L, 3874L, 4958L, 4958L, 4958L, 627L)
    ), by = "panel")
  write.csv(table9_district_check,
            "tables/checks/table9_sample_check.csv",
            row.names = FALSE)
  print(table9_district_check)
} else {
  message(
    "Tables 5-10 were not estimated because transition_rating_0203 has not ",
    "yet been finalized. Tables 1-4 were still created."
  )
}

###############################################################################
# 8. Reproducibility and missing-input status
###############################################################################

file_status <- tibble(
  input = c(
    "2002-03 grades 3-11 at 1 SEM below panel recommendation",
    "2002-03 SDAA all-students rate (checked, not used in the rating)",
    "2002-03 completion-rate statistics",
    "Actual08 PEIMS finance"
  ),
  needed_for = c(
    "Transition rating for Tables 1 and 5-10",
    "Transition rating for Tables 1 and 5-10",
    "High-school transition rating",
    "Forward resource changes for later TAKS years"
  ),
  present = c(
    file.exists(
      "data/aeis campus info/02-03/taks/camptaks311-1belowpanel.dta"
    ),
    file.exists("data/aeis campus info/02-03/other/campothr.dat"),
    file.exists("data/aeis campus info/02-03/comp/campcomp.dta"),
    file.exists("data/peims finance/Actual08/ACTUAL_2008F.TXT")
  )
)

write.csv(file_status,
          "tables/replication_input_status.csv",
          row.names = FALSE)
print(file_status, n = Inf, width = Inf)

###############################################################################
# 9. End-of-script comprehensive audit bundle
###############################################################################

# Compare every Table 1 entry with the published values. Differences are
# calculated as replication minus published, in percentage points.
table1_published <- tribble(
  ~ year,
  ~ published_low,
  ~ published_acceptable,
  ~ published_recognized,
  ~ published_exemplary,
  ~ published_lower,
  ~ published_same,
  ~ published_higher,
  1998L,
  0.1,
  56.4,
  26.7,
  16.2,
  NA,
  NA,
  NA,
  1999L,
  0.9,
  52.6,
  29.4,
  17.1,
  14.4,
  67.6,
  18.0,
  2000L,
  1.6,
  47.4,
  31.9,
  19.1,
  14.4,
  65.9,
  19.7,
  2001L,
  0.7,
  39.3,
  36.9,
  23.1,
  12.4,
  62.9,
  24.8,
  2002L,
  1.4,
  32.0,
  37.8,
  28.9,
  15.3,
  59.4,
  25.3,
  2003L,
  8.6,
  70.3,
  18.2,
  2.8,
  62.3,
  34.2,
  3.5,
  2004L,
  0.9,
  55.5,
  37.2,
  6.5,
  47.8,
  42.2,
  10.0,
  2005L,
  3.0,
  64.7,
  28.1,
  4.2,
  24.4,
  68.1,
  7.4,
  2006L,
  3.5,
  47.3,
  41.6,
  7.7,
  8.0,
  64.8,
  27.2,
  2007L,
  3.4,
  54.2,
  33.8,
  8.6,
  20.0,
  66.0,
  14.1
)

table1_published_comparison <- table1 |>
  left_join(table1_published, by = "year") |>
  mutate(
    rating_sum = low + acceptable + recognized + exemplary,
    change_sum = lower + same + higher,
    diff_low = round(low - published_low, 1),
    diff_acceptable = round(acceptable - published_acceptable, 1),
    diff_recognized = round(recognized - published_recognized, 1),
    diff_exemplary = round(exemplary - published_exemplary, 1),
    diff_lower = round(lower - published_lower, 1),
    diff_same = round(same - published_same, 1),
    diff_higher = round(higher - published_higher, 1)
  )

# Table 2 published transition matrices.
table2_published_long <- tribble(
  ~ panel,
  ~ previous_rating,
  ~ current_rating,
  ~ published_percent,
  "A_TAAS",
  "L",
  "L",
  11.3,
  "A_TAAS",
  "L",
  "A",
  82.3,
  "A_TAAS",
  "L",
  "R",
  6.4,
  "A_TAAS",
  "L",
  "E",
  0.0,
  "A_TAAS",
  "A",
  "L",
  2.0,
  "A_TAAS",
  "A",
  "A",
  69.1,
  "A_TAAS",
  "A",
  "R",
  25.5,
  "A_TAAS",
  "A",
  "E",
  3.4,
  "A_TAAS",
  "R",
  "L",
  0.1,
  "A_TAAS",
  "R",
  "A",
  24.6,
  "A_TAAS",
  "R",
  "R",
  53.6,
  "A_TAAS",
  "R",
  "E",
  21.7,
  "A_TAAS",
  "E",
  "L",
  0.0,
  "A_TAAS",
  "E",
  "A",
  4.1,
  "A_TAAS",
  "E",
  "R",
  24.6,
  "A_TAAS",
  "E",
  "E",
  71.3,
  "B_TRANSITION",
  "L",
  "L",
  5.6,
  "B_TRANSITION",
  "L",
  "A",
  80.6,
  "B_TRANSITION",
  "L",
  "R",
  12.5,
  "B_TRANSITION",
  "L",
  "E",
  1.4,
  "B_TRANSITION",
  "A",
  "L",
  1.9,
  "B_TRANSITION",
  "A",
  "A",
  75.9,
  "B_TRANSITION",
  "A",
  "R",
  21.3,
  "B_TRANSITION",
  "A",
  "E",
  1.0,
  "B_TRANSITION",
  "R",
  "L",
  0.2,
  "B_TRANSITION",
  "R",
  "A",
  59.3,
  "B_TRANSITION",
  "R",
  "R",
  38.0,
  "B_TRANSITION",
  "R",
  "E",
  2.5,
  "B_TRANSITION",
  "E",
  "L",
  0.1,
  "B_TRANSITION",
  "E",
  "A",
  29.2,
  "B_TRANSITION",
  "E",
  "R",
  53.8,
  "B_TRANSITION",
  "E",
  "E",
  17.0,
  "C_TAKS",
  "L",
  "L",
  21.8,
  "C_TAKS",
  "L",
  "A",
  73.3,
  "C_TAKS",
  "L",
  "R",
  4.8,
  "C_TAKS",
  "L",
  "E",
  0.3,
  "C_TAKS",
  "A",
  "L",
  4.7,
  "C_TAKS",
  "A",
  "A",
  74.0,
  "C_TAKS",
  "A",
  "R",
  20.9,
  "C_TAKS",
  "A",
  "E",
  0.3,
  "C_TAKS",
  "R",
  "L",
  0.2,
  "C_TAKS",
  "R",
  "A",
  33.8,
  "C_TAKS",
  "R",
  "R",
  57.9,
  "C_TAKS",
  "R",
  "E",
  8.1,
  "C_TAKS",
  "E",
  "L",
  0.0,
  "C_TAKS",
  "E",
  "A",
  3.1,
  "C_TAKS",
  "E",
  "R",
  40.5,
  "C_TAKS",
  "E",
  "E",
  54.4
)

table2_calculated_long <- bind_rows(
  table2a |> mutate(panel = "A_TAAS", .before = 1),
  table2b |> mutate(panel = "B_TRANSITION", .before = 1),
  table2c |> mutate(panel = "C_TAKS", .before = 1)
) |>
  pivot_longer(cols = c(L, A, R, E),
               names_to = "current_rating",
               values_to = "calculated_percent")

table2_published_comparison <- table2_calculated_long |>
  left_join(table2_published_long,
            by = c("panel", "previous_rating", "current_rating")) |>
  mutate(difference = round(calculated_percent - published_percent, 1)) |>
  arrange(match(panel, c("A_TAAS", "B_TRANSITION", "C_TAKS")),
          match(previous_rating, c("L", "A", "R", "E")),
          match(current_rating, c("L", "A", "R", "E")))

table2_row_sum_check <- table2_calculated_long |>
  group_by(panel, previous_rating) |>
  summarise(row_sum = sum(calculated_percent), .groups = "drop") |>
  mutate(difference_from_100 = round(row_sum - 100, 2))

# Finance identities and year mappings.
finance_identity_check <- finance_longitudinal_9798_0708 |>
  mutate(
    category_sum =
      INSTRUCTION + ADMIN_TRAINING + COUNSELING +
      EXTRACURRICULAR + OTHER_OPERATING,
    identity_difference = TOTAL_OPERATING - category_sum
  ) |>
  group_by(FINANCE_SCHOOL_YEAR, FINANCE_END_YEAR, RATING_YEAR) |>
  summarise(
    campuses = n(),
    duplicate_campus_ids = sum(duplicated(CAMPUS_ID)),
    missing_total_operating = sum(is.na(TOTAL_OPERATING)),
    negative_total_operating = sum(TOTAL_OPERATING < 0, na.rm = TRUE),
    max_absolute_identity_difference =
      max(abs(identity_difference), na.rm = TRUE),
    total_operating_dollars = sum(TOTAL_OPERATING, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    end_year_mapping_valid =
      FINANCE_END_YEAR == as.integer(substr(FINANCE_SCHOOL_YEAR, 3, 4)) +
      if_else(as.integer(substr(
        FINANCE_SCHOOL_YEAR, 3, 4
      )) < 50, 2000L, 1900L),
    rating_year_mapping_valid = RATING_YEAR == FINANCE_END_YEAR - 1L
  )

negative_finance_rows <- finance_longitudinal_9798_0708 |>
  filter(TOTAL_OPERATING < 0) |>
  select(
    FINANCE_SCHOOL_YEAR,
    FINANCE_END_YEAR,
    CAMPUS_ID,
    DISTRICT,
    FIN_UNIT,
    TOTAL_OPERATING,
    INSTRUCTION,
    ADMIN_TRAINING,
    COUNSELING,
    EXTRACURRICULAR,
    OTHER_OPERATING
  ) |>
  arrange(FINANCE_END_YEAR, TOTAL_OPERATING)

resource_merge_check <- resources_by_year |>
  group_by(outcome_year) |>
  summarise(
    finance_rows = n(),
    duplicate_campus_ids = sum(duplicated(campus_number)),
    missing_enrollment = sum(is.na(outcome_enrollment)),
    nonpositive_enrollment = sum(outcome_enrollment <= 0, na.rm = TRUE),
    missing_total_operating_pp = sum(is.na(total_operating_pp)),
    missing_teacher_fte = if ("teacher_fte" %in% names(resources_by_year)) {
      sum(is.na(teacher_fte))
    } else {
      NA_integer_
    },
    .groups = "drop"
  )

resource_outlier_check <- resources_by_year |>
  filter(is.finite(total_operating_pp)) |>
  group_by(outcome_year) |>
  summarise(
    campuses = n(),
    negative_per_pupil = sum(total_operating_pp < 0),
    minimum = min(total_operating_pp),
    percentile_1 = quantile(total_operating_pp, 0.01, names = FALSE),
    median = median(total_operating_pp),
    percentile_99 = quantile(total_operating_pp, 0.99, names = FALSE),
    maximum = max(total_operating_pp),
    .groups = "drop"
  )

# Coverage among the rated campuses actually used for Table 4. A zero in 2008
# indicates that 2007-08 student enrollment is needed to express Actual08
# spending per pupil.
table4_resource_coverage_check <- rating_for_resources |>
  mutate(outcome_year = rating_year + 1L) |>
  left_join(
    resources_by_year |>
      select(
        campus_number,
        outcome_year,
        total_operating,
        outcome_enrollment,
        total_operating_pp
      ),
    by = c("campus_number", "outcome_year")
  ) |>
  group_by(regime, rating_year, outcome_year) |>
  summarise(
    rated_campuses = n(),
    campuses_with_finance = sum(!is.na(total_operating)),
    campuses_with_enrollment = sum(!is.na(outcome_enrollment)),
    campuses_with_per_pupil_spending = sum(!is.na(total_operating_pp)),
    per_pupil_coverage_percent = round(100 * campuses_with_per_pupil_spending / rated_campuses, 1),
    .groups = "drop"
  )

# Compare the Table 3 observation counts reported under the two regimes.
table3_published_observations <- tribble(
  ~ regime,
  ~ campus_rating,
  ~ published_observations,
  "TAAS",
  "L",
  285L,
  "TAAS",
  "A",
  12111L,
  "TAAS",
  "R",
  8614L,
  "TAAS",
  "E",
  5550L,
  "TAKS",
  "L",
  593L,
  "TAKS",
  "A",
  12537L,
  "TAKS",
  "R",
  7985L,
  "TAKS",
  "E",
  1537L
)

table3_observation_comparison <- table3_observations |>
  mutate(campus_rating = as.character(campus_rating)) |>
  left_join(table3_published_observations,
            by = c("regime", "campus_rating")) |>
  mutate(difference = observations - published_observations)

# Track sample attrition into the regression analysis.
if (exists("rating_shock_analysis", envir = .GlobalEnv)) {
  attrition_transition <- transition_rating_0203 |>
    distinct(campus_number)
  attrition_prior <- attrition_transition |>
    inner_join(prior_ratings |> distinct(campus_number), by = "campus_number")
  attrition_demographics <- attrition_prior |>
    inner_join(transition_demographics |> distinct(campus_number), by = "campus_number")
  attrition_resources <- attrition_demographics |>
    inner_join(resource_changes |> distinct(campus_number), by = "campus_number")
  
  regression_attrition_check <- tibble(
    stage = c(
      "Corrected transition rating",
      "Has ratings in 2000, 2001, and 2002",
      "Has 2003 demographics",
      "Has required resource-year records",
      "Final rating_shock_analysis"
    ),
    campuses = c(
      nrow(attrition_transition),
      nrow(attrition_prior),
      nrow(attrition_demographics),
      nrow(attrition_resources),
      n_distinct(rating_shock_analysis$campus_number)
    )
  ) |>
    mutate(dropped_from_previous = campuses - lag(campuses))
} else {
  regression_attrition_check <- tibble(
    stage = "rating_shock_analysis was not created",
    campuses = NA_integer_,
    dropped_from_previous = NA_integer_
  )
}

# Verify the four mutually exclusive rating-shock indicators.
transition_flag_check <- transition_rating_0203 |>
  mutate(
    previous_value = unname(rating_values[rating_2002]),
    current_value = unname(rating_values[estimated_rating_no_ri]),
    expected_increased = as.integer(current_value > previous_value),
    expected_drop_L = as.integer(current_value < previous_value &
                                   estimated_rating_no_ri == "L"),
    expected_drop_A = as.integer(current_value < previous_value &
                                   estimated_rating_no_ri == "A"),
    expected_drop_R = as.integer(current_value < previous_value &
                                   estimated_rating_no_ri == "R")
  ) |>
  summarise(
    campuses = n(),
    rows_with_multiple_shock_flags = sum(
      rating_increased + rating_dropped_to_L + rating_dropped_to_A +
        rating_dropped_to_R > 1
    ),
    incorrect_increase_flags = sum(rating_increased != expected_increased),
    incorrect_drop_L_flags = sum(rating_dropped_to_L != expected_drop_L),
    incorrect_drop_A_flags = sum(rating_dropped_to_A != expected_drop_A),
    incorrect_drop_R_flags = sum(rating_dropped_to_R != expected_drop_R)
  )

# Check demographic controls and grade-share totals.
if (exists("rating_shock_analysis", envir = .GlobalEnv)) {
  percentage_columns <- intersect(
    c(
      "pct_asian",
      "pct_black",
      "pct_hispanic",
      "pct_native",
      "pct_economically_disadvantaged",
      "pct_lep",
      "pct_special_education",
      "pct_gifted",
      "pct_vocational"
    ),
    names(rating_shock_analysis)
  )
  
  demographic_bounds_check <- bind_rows(lapply(percentage_columns, function(variable_name) {
    values <- rating_shock_analysis[[variable_name]]
    tibble(
      variable = variable_name,
      nonmissing = sum(!is.na(values)),
      missing = sum(is.na(values)),
      below_zero = sum(values < 0, na.rm = TRUE),
      above_100 = sum(values > 100, na.rm = TRUE),
      minimum = min(values, na.rm = TRUE),
      maximum = max(values, na.rm = TRUE)
    )
  }))
  
  grade_share_columns <- grep("^pct_grade_", names(rating_shock_analysis), value = TRUE)
  grade_share_totals <- rowSums(rating_shock_analysis[, grade_share_columns, drop = FALSE], na.rm = FALSE)
  grade_share_check <- tibble(
    campuses = length(grade_share_totals),
    missing_grade_share_totals = sum(is.na(grade_share_totals)),
    minimum_grade_share_total = min(grade_share_totals, na.rm = TRUE),
    median_grade_share_total = median(grade_share_totals, na.rm = TRUE),
    maximum_grade_share_total = max(grade_share_totals, na.rm = TRUE),
    campuses_outside_99_to_101 = sum(grade_share_totals < 99 |
                                       grade_share_totals > 101, na.rm = TRUE)
  )
} else {
  demographic_bounds_check <- tibble()
  grade_share_check <- tibble()
}

# Confirm that every requested model was estimated and contains each shock term.
if (all(vapply(
  c("table5", "table6", "table7", "table8", "table10"),
  exists,
  logical(1),
  envir = .GlobalEnv
))) {
  model_results_audit <- bind_rows(table5, table6, table7, table8, table10)
  
  model_summary_check <- model_results_audit |>
    group_by(table, outcome) |>
    summarise(
      terms = n(),
      observations_min = min(observations),
      observations_max = max(observations),
      districts_min = min(districts),
      districts_max = max(districts),
      missing_estimates = sum(is.na(estimate)),
      missing_standard_errors = sum(is.na(standard_error)),
      missing_p_values = sum(is.na(p_value)),
      .groups = "drop"
    )
  
  main_regression_coefficients <- model_results_audit |>
    filter(term %in% shock_terms) |>
    mutate(shock_order = match(term, shock_terms)) |>
    arrange(table, outcome, shock_order) |>
    select(-shock_order)
  
  table5_published_targets <- tribble(
    ~ outcome,
    ~ published_estimate,
    "change_0304_total",
    72.9,
    "change_0304_instruction",
    66.2,
    "change_0304_admin_training",
    8.0,
    "change_0304_counseling",
    2.3,
    "change_0304_extracurricular",
    -4.1,
    "change_0304_student_teacher",
    -0.28
  )
  
  table5_published_comparison <- main_regression_coefficients |>
    filter(table == "Table 5", term == "rating_dropped_to_A") |>
    inner_join(table5_published_targets, by = "outcome") |>
    mutate(
      difference_from_published = estimate - published_estimate,
      same_sign_as_published = sign(estimate) == sign(published_estimate)
    ) |>
    arrange(match(outcome, table5_published_targets$outcome))
  
  missing_model_shock_terms <- model_results_audit |>
    distinct(table, outcome) |>
    tidyr::crossing(term = shock_terms) |>
    anti_join(
      model_results_audit |>
        distinct(table, outcome, term),
      by = c("table", "outcome", "term")
    )
} else {
  model_summary_check <- tibble()
  main_regression_coefficients <- tibble(
    table = "Model tables were not created",
    outcome = NA_character_,
    term = NA_character_,
    estimate = NA_real_,
    standard_error = NA_real_,
    statistic = NA_real_,
    p_value = NA_real_,
    observations = NA_integer_,
    districts = NA_integer_,
    residual_df = NA_integer_,
    model_rank = NA_integer_
  )
  table5_published_comparison <- tibble(
    outcome = "Model tables were not created",
    estimate = NA_real_,
    published_estimate = NA_real_,
    difference_from_published = NA_real_,
    same_sign_as_published = NA
  )
  missing_model_shock_terms <- tibble(table = "Model tables were not created",
                                      outcome = NA_character_,
                                      term = NA_character_)
}

expected_output_files <- c(
  "tables/table1_rating_distributions.csv",
  "tables/table2a_taas_transitions.csv",
  "tables/table2b_transition.csv",
  "tables/table2c_taks_transitions.csv",
  "tables/table3_school_characteristics.csv",
  "tables/table4_mean_resources.csv",
  "tables/table5_models.csv",
  "tables/table6_models.csv",
  "tables/table7_models.csv",
  "tables/table8_models.csv",
  "tables/table9_interaction_analysis_data.csv",
  "tables/table10_models.csv"
)

output_file_check <- tibble(
  file = expected_output_files,
  exists = file.exists(expected_output_files),
  bytes = ifelse(
    file.exists(expected_output_files),
    file.info(expected_output_files)$size,
    NA_real_
  )
)

known_limitations <- tibble(
  item = c(
    "Appeal, alternative-school, and special-analysis exclusions",
    "Transition subgroup minimum-size rules",
    "Transition completion-rate eligibility",
    "Per-pupil spending for 2007-08",
    "Table 1, 2003-04 without required improvement",
    "Table 5-10 rating-shock coding"
  ),
  status = c(
    "Not fully observed in the public REF files; published samples can differ",
    "Approximated by each group's enrollment share times enrollment in the subject's tested grades; subject-by-group tested counts are not public",
    "Approximated from public completion rates and grade-12 enrollment",
    "2007-08 STUD enrollment is absent; 2006-07 enrollment is used as the 2007-08 per-pupil denominator",
    "Not produced: it requires re-scoring 2003-04 TAKS results under the 2003-04 rules without required improvement",
    "Calculator validated against Online Appendix Table 1A (2003-04 ratings without required improvement); remaining differences come from approximate subgroup tested counts and unavailable appeal/special-analysis flags"
  )
)

audit_assertions <- tribble(
  ~ check,
  ~ passed,
  "No duplicate longitudinal or finance keys",
  all(longitudinal_duplicate_check$duplicate_keys == 0),
  "Finance category sums equal total operating",
  all(
    finance_identity_check$max_absolute_identity_difference < 1e-6
  ),
  "Finance year mappings are internally consistent",
  all(finance_identity_check$end_year_mapping_valid) &
    all(finance_identity_check$rating_year_mapping_valid),
  "Transition sample has no duplicate campuses",!anyDuplicated(transition_rating_0203$campus_number),
  "Transition sample has no zero-measure campuses",
  all(transition_rating_0203$evaluated_measures > 0),
  "Transition rating percentages sum to approximately 100",
  abs(sum(transition_distribution_check$percent) - 100) <= 0.2,
  "Table 1 rating rows sum to approximately 100",
  all(abs(
    table1_published_comparison$rating_sum - 100
  ) <= 0.2),
  "Table 2 transition rows sum to approximately 100",
  all(abs(
    table2_row_sum_check$difference_from_100
  ) <= 0.2),
  "Rating-shock flags are mutually exclusive and correctly coded",
  all(unlist(transition_flag_check[1, -1]) == 0),
  "All requested shock terms appear in every model",
  nrow(missing_model_shock_terms) == 0,
  "Every fitted model uses at least two district clusters",
  nrow(model_summary_check) > 0 &
    all(model_summary_check$districts_min >= 2),
  "All expected output files were created and are nonempty",
  all(output_file_check$exists & output_file_check$bytes > 0)
)

write.csv(
  table1_published_comparison,
  "tables/checks/table1_published_comparison.csv",
  row.names = FALSE
)
write.csv(
  table2_published_comparison,
  "tables/checks/table2_published_comparison.csv",
  row.names = FALSE
)
write.csv(table2_row_sum_check,
          "tables/checks/table2_row_sum_check.csv",
          row.names = FALSE)
write.csv(finance_identity_check,
          "tables/checks/finance_identity_check.csv",
          row.names = FALSE)
write.csv(negative_finance_rows,
          "tables/checks/negative_finance_rows.csv",
          row.names = FALSE)
write.csv(resource_merge_check,
          "tables/checks/resource_merge_check.csv",
          row.names = FALSE)
write.csv(resource_outlier_check,
          "tables/checks/resource_outlier_check.csv",
          row.names = FALSE)
write.csv(
  table4_resource_coverage_check,
  "tables/checks/table4_resource_coverage_check.csv",
  row.names = FALSE
)
write.csv(
  table3_observation_comparison,
  "tables/checks/table3_observation_comparison.csv",
  row.names = FALSE
)
write.csv(
  regression_attrition_check,
  "tables/checks/regression_attrition_check.csv",
  row.names = FALSE
)
write.csv(transition_flag_check,
          "tables/checks/transition_flag_check.csv",
          row.names = FALSE)
write.csv(
  demographic_bounds_check,
  "tables/checks/demographic_bounds_check.csv",
  row.names = FALSE
)
write.csv(grade_share_check,
          "tables/checks/grade_share_check.csv",
          row.names = FALSE)
write.csv(model_summary_check,
          "tables/checks/model_summary_check.csv",
          row.names = FALSE)
write.csv(
  main_regression_coefficients,
  "tables/checks/main_regression_coefficients.csv",
  row.names = FALSE
)
write.csv(
  table5_published_comparison,
  "tables/checks/table5_published_comparison.csv",
  row.names = FALSE
)
write.csv(
  missing_model_shock_terms,
  "tables/checks/missing_model_shock_terms.csv",
  row.names = FALSE
)
write.csv(output_file_check,
          "tables/checks/output_file_check.csv",
          row.names = FALSE)
write.csv(audit_assertions,
          "tables/checks/audit_assertions.csv",
          row.names = FALSE)
write.csv(known_limitations,
          "tables/checks/known_limitations.csv",
          row.names = FALSE)

capture.output({
  cat("AUDIT ASSERTIONS\n")
  print(as_tibble(audit_assertions),
        n = Inf,
        width = Inf)
  cat("\nINPUT FILE STATUS\n")
  print(as_tibble(file_status), n = Inf, width = Inf)
  cat("\nLONGITUDINAL SOURCE COUNTS\n")
  print(as_tibble(longitudinal_source_check), n = Inf)
  cat("\nDUPLICATE KEY CHECK\n")
  print(as_tibble(longitudinal_duplicate_check), n = Inf)
  cat("\nFINANCE IDENTITY AND YEAR CHECK\n")
  print(as_tibble(finance_identity_check),
        n = Inf,
        width = Inf)
  cat("\nNEGATIVE FINANCE ROWS\n")
  print(as_tibble(negative_finance_rows),
        n = Inf,
        width = Inf)
  cat("\nRESOURCE/ENROLLMENT MERGE CHECK\n")
  print(as_tibble(resource_merge_check),
        n = Inf,
        width = Inf)
  cat("\nPER-PUPIL RESOURCE OUTLIER CHECK\n")
  print(as_tibble(resource_outlier_check),
        n = Inf,
        width = Inf)
  cat("\nTABLE 4 RATED-SAMPLE RESOURCE COVERAGE\n")
  print(as_tibble(table4_resource_coverage_check),
        n = Inf,
        width = Inf)
  cat("\nTRANSITION SOURCE COUNTS\n")
  print(as_tibble(transition_source_check), n = Inf)
  cat("\nTRANSITION SAMPLE CHECK\n")
  print(as_tibble(transition_sample_check), n = Inf)
  cat("\nTRANSITION MEASURE CHECK\n")
  print(as_tibble(transition_measure_check), width = Inf)
  cat("\nSDAA CHECK\n")
  print(as_tibble(sdaa_check), width = Inf)
  cat("\nNON-TEST INDICATOR CHECK\n")
  print(as_tibble(transition_non_test_check), width = Inf)
  cat("\nFINAL TRANSITION DISTRIBUTION\n")
  print(as_tibble(transition_distribution_check),
        n = Inf,
        width = Inf)
  cat("\nTRANSITION DIRECTION\n")
  print(as_tibble(transition_direction_check), n = Inf)
  cat("\nRATING-SHOCK FLAG CHECK\n")
  print(as_tibble(transition_flag_check), width = Inf)
  cat("\nTABLE 1 VERSUS PUBLISHED\n")
  print(as_tibble(table1_published_comparison),
        n = Inf,
        width = Inf)
  cat("\nTABLE 2 ROW-SUM CHECK\n")
  print(as_tibble(table2_row_sum_check), n = Inf)
  cat("\nTABLE 2 VERSUS PUBLISHED\n")
  print(as_tibble(table2_published_comparison),
        n = Inf,
        width = Inf)
  cat("\nTABLE 3 OBSERVATIONS VERSUS PUBLISHED\n")
  print(as_tibble(table3_observation_comparison),
        n = Inf,
        width = Inf)
  cat("\nREGRESSION SAMPLE ATTRITION\n")
  print(as_tibble(regression_attrition_check),
        n = Inf,
        width = Inf)
  cat("\nDEMOGRAPHIC BOUNDS CHECK\n")
  print(as_tibble(demographic_bounds_check),
        n = Inf,
        width = Inf)
  cat("\nGRADE-SHARE CHECK\n")
  print(as_tibble(grade_share_check), width = Inf)
  cat("\nMODEL SUMMARY CHECK\n")
  print(as_tibble(model_summary_check),
        n = Inf,
        width = Inf)
  cat("\nMAIN REGRESSION SHOCK COEFFICIENTS\n")
  print(as_tibble(main_regression_coefficients),
        n = Inf,
        width = Inf)
  cat("\nTABLE 5 DROP-TO-A ESTIMATES VERSUS PUBLISHED\n")
  print(as_tibble(table5_published_comparison),
        n = Inf,
        width = Inf)
  cat("\nMISSING MODEL SHOCK TERMS\n")
  print(as_tibble(missing_model_shock_terms),
        n = Inf,
        width = Inf)
  cat("\nOUTPUT FILE CHECK\n")
  print(as_tibble(output_file_check),
        n = Inf,
        width = Inf)
  cat("\nKNOWN LIMITATIONS\n")
  print(as_tibble(known_limitations),
        n = Inf,
        width = Inf)
  cat("\nSESSION INFORMATION\n")
  print(sessionInfo())
}, file = "tables/checks/checks_to_send.txt")

saveRDS(
  list(
    audit_assertions = audit_assertions,
    file_status = file_status,
    longitudinal_source_check = longitudinal_source_check,
    longitudinal_duplicate_check = longitudinal_duplicate_check,
    finance_identity_check = finance_identity_check,
    negative_finance_rows = negative_finance_rows,
    resource_merge_check = resource_merge_check,
    resource_outlier_check = resource_outlier_check,
    table4_resource_coverage_check = table4_resource_coverage_check,
    table3_observation_comparison = table3_observation_comparison,
    transition_source_check = transition_source_check,
    transition_sample_check = transition_sample_check,
    transition_measure_check = transition_measure_check,
    sdaa_check = sdaa_check,
    transition_non_test_check = transition_non_test_check,
    transition_distribution_check = transition_distribution_check,
    transition_direction_check = transition_direction_check,
    transition_flag_check = transition_flag_check,
    table1_published_comparison = table1_published_comparison,
    table2_published_comparison = table2_published_comparison,
    table2_row_sum_check = table2_row_sum_check,
    regression_attrition_check = regression_attrition_check,
    demographic_bounds_check = demographic_bounds_check,
    grade_share_check = grade_share_check,
    model_summary_check = model_summary_check,
    main_regression_coefficients = main_regression_coefficients,
    table5_published_comparison = table5_published_comparison,
    missing_model_shock_terms = missing_model_shock_terms,
    output_file_check = output_file_check,
    known_limitations = known_limitations
  ),
  "tables/checks/full_replication_audit_bundle.rds"
)

message("Comprehensive checks written to tables/checks/checks_to_send.txt")

###############################################################################
# Compare every table with the published values (tables/comparison/) and write
# paper-style versions of our estimates (tables/formatted/).
###############################################################################

source("code/compare_to_published.R")
