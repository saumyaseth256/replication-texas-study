library(dplyr)
library(tidyr)
library(stringr)
library(data.table)

# Table of Contents
# * attempted replication of tables in the paper

#   | 0 | Download data |
#   | 1 | AEIS campus panels, 1997-98 to 2007-08 |
#   | 2 | PEIMS campus expenditures, 1998-99 to 2007-08 |
#   | 3 | Tables 1-2 rating panel |
#   | 4 | Table 1: Ratings by year and changes from the prior year |
#   | 5 | Table 2: Rating-transition matrices |*
#   | 6 | Estimated 2002-03 gap-year ratings |
#   | 6a | Read completion and dropout data |
#   | 6b | Enrollment by grade and student group |
#   | 6c | Calculate gap-year ratings |
#   | 6d | Apply test standards and exceptions |
#   | 6e | Completion rates |
#   | 6f | Annual dropout rates |
#   | 6g | Apply completion standards |
#   | 6h | Combine test and completion ratings |
#   | 6i | Create rating-shock indicators |
#   | 7 | Table 1 estimated-rating row and change panel |*
#   | 8 | Tables 3-4 analysis sample |
#   | 9 | Table 3: School characteristics |*
#   | 10 | Tables 4-10 per-pupil resources |
#   | 10b | Staffing measures |
#   | 11 | Table 4: Resources in year t+1 by rating in year t |*
#   | 12 | Tables 5-10 regression data |
#   | 13 | Regression model and standard errors |
#   | 14 | Tables 5-8 and 10 |*
#   | 15 | Table 9: District-characteristic interactions |*
#   | 16 | Write formatted tables |*

# 0. Download data

onedrive_zip_url <- "https://penno365-my.sharepoint.com/:u:/g/personal/saumyase_upenn_edu/IQCXzTI7_sI8QLfe_AObHVSiAZvU7kb4EDYV7CNotSI22qk?e=R3Aew8"
repo_root <- normalizePath(getwd(), mustWork = TRUE)
data_dir <- file.path(repo_root, "data")
required_files <- c(
  file.path(data_dir, "aeis campus info", "97-98", "ref", "campref.dat"),
  file.path(data_dir, "aeis campus info", "07-08", "campstud.dat"),
  file.path(data_dir, "aeis campus info", "02-03", "other", "campothr.dat"),
  file.path(data_dir, "aeis campus info", "02-03", "comp", "campcomp.dta"),
  file.path(data_dir, "peims finance", "Actual99", "ACT9FILE"),
  file.path(data_dir, "peims finance", "Actual08", "ACTUAL_2008F.TXT"),
  file.path(data_dir, "2004 accountability", "CAMPTKSD.dat")
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
    stop("Error")
  }
  message("Extracting replication data...")
  unzip(zipfile = zip_path, exdir = extraction_dir)
  if (dir.exists(file.path(extraction_dir, "data"))) {
    extracted_data <- file.path(extraction_dir, "data")
  } else if (dir.exists(file.path(extraction_dir, "aeis campus info")) &&
             dir.exists(file.path(extraction_dir, "peims finance"))) {
    extracted_data <- extraction_dir
  } else {
    stop("Error")
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

# 1. AEIS campus panels, 1997-98 to 2007-08

# 1997-98
# Campus reference
data_ref_9798 <- read.csv("data/aeis campus info/97-98/ref/campref.dat", header = FALSE)
layout_ref_9798_lines <- readLines("data/aeis campus info/97-98/ref/campref.lyt")
layout_ref_9798_rows <- grep("^\\s*[0-9]{1,3}\\s+", layout_ref_9798_lines, value = TRUE)
layout_ref_9798_names <- toupper(sub("^\\s*[0-9]{1,3}\\s+(\\S+).*", "\\1", layout_ref_9798_rows))
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
# Student data
data_stud_9798 <- read.csv("data/aeis campus info/97-98/stud/campstud.dat", header = FALSE)
layout_stud_9798_lines <- readLines("data/aeis campus info/97-98/stud/campstud.lyt")
layout_stud_9798_rows <- grep("^\\s*[0-9]{1,3}\\s+", layout_stud_9798_lines, value = TRUE)
layout_stud_9798_names <- toupper(sub("^\\s*[0-9]{1,3}\\s+(\\S+).*", "\\1", layout_stud_9798_rows))
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
# Staff data
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
# 1998-99
# Campus reference
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
# Student data
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
# Staff data
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
# 1999-00
# Campus reference
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
# Student data
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
# Staff data
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
# 2000-01
# Campus reference
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
# Student data
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
# Staff data
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
# 2001-02
# Campus reference
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
# Student data
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
# Staff data
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
# 2002-03
# Campus reference
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
# Student data
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
# Staff data
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
# 2003-04
# Campus reference
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
# Student data
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
# Staff data
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
# 2004-05
# Campus reference
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
# Student data
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
# Staff data
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
# 2005-06
# Campus reference
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
# Student data
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
# Staff data
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
# 2006-07
# Campus reference
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
# Student data
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
# Staff data
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
# 2007-08
# Student data
data_stud_0708 <- read.csv("data/aeis campus info/07-08/campstud.dat", header = TRUE)
layout_stud_0708 <- readLines("data/aeis campus info/07-08/campstud.lyt")
var_rows_layout_stud_0708 <- grep("^\\s*[0-9]{1,3}\\s+", layout_stud_0708, value = TRUE)
layout_stud_0708 <- data.frame(
  POS = sub("^\\s*([0-9]{1,3}).*", "\\1", var_rows_layout_stud_0708),
  NAME = sub(
    "^\\s*[0-9]{1,3}\\s+(\\S+).*",
    "\\1",
    var_rows_layout_stud_0708
  ),
  DESCRIPTION = trimws(
    sub(
      "^\\s*[0-9]{1,3}\\s+\\S+\\s+\\S+\\s+\\d+\\s*",
      "",
      var_rows_layout_stud_0708
    )
  )
)
colnames(data_stud_0708) <- toupper(layout_stud_0708$NAME)
data_stud_0708$CAMPUS <- str_pad(
  as.character(data_stud_0708$CAMPUS),
  width = 9,
  side = "left",
  pad = "0"
)
data_stud_0708 <- data_stud_0708 |>
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
  data_stud_0607 |> mutate(year = 2007L),
  data_stud_0708 |> mutate(year = 2008L)
)
data_staf_longitudinal <- bind_rows(
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

# 2. PEIMS campus expenditures, 1998-99 to 2007-08

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
      FUND != "600",!FUND %in% shared_service_funds
    ) |>
    mutate(
      CRAIG_CATEGORY = case_when(
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
      RATING_YEAR = end_calendar_year - 1L,
      SOURCE_DIRECTORY = paste0("Actual", year_suffix),
      .before = CAMPUS_ID
    )
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

# 3. Tables 1-2 rating panel

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

# 4. Table 1: ratings by year and change from the prior year

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
write.csv(table1, "tables/table1_rating_distributions.csv", row.names = FALSE)

# 5. Table 2: rating transitions (t-1 by t)
# A: TAAS; B: 2001-02 to 2003-04; C: TAKS.

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
write.csv(table2a, "tables/table2a_taas_transitions.csv", row.names = FALSE)
write.csv(table2b, "tables/table2b_transition.csv", row.names = FALSE)
write.csv(table2c, "tables/table2c_taks_transitions.csv", row.names = FALSE)

# 6. Estimated 2002-03 gap-year ratings
# Apply 2003-04 rules without required improvement: E >= 90%; R >= 70%;
# A >= 50% for reading/ELA, writing, and social studies, >= 35% for math,
# and >= 25% for science, with limited exceptions; otherwise L. Include
# high-school completion and minimum-size rules. Compare with 2001-02.

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
  names(output) <- column_names
  output
}

# 6a. Read 2002-03 completion and dropout data

completion_0203 <- read_transition_file(
  file.path(transition_root, "comp", "campcomp.dta"),
  file.path(transition_root, "comp", "campcomp.lyt")
)
dropout_0203 <- read_transition_file(
  file.path(transition_root, "other", "campothr.dat"),
  file.path(transition_root, "other", "campothr.lyt")
)

# 6b. Enrollment by grade and group for campuses with >= 200 students

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

# 6c. Calculate 2003 gap-year ratings

tea_campus_taks <- read.csv(
  file.path("data", "2004 accountability", "CAMPTKSD.dat"),
  colClasses = "character",
  na.strings = c(".", "", "NA")
) |>
  mutate(CAMPUS = str_pad(CAMPUS, 9, "left", "0"))
tea_subject_names <- c(
  R = "reading_ela",
  W = "writing",
  S = "social_studies",
  M = "mathematics",
  C = "science",
  U = "sdaa"
)
tea_measures_long <- tea_campus_taks |>
  select(
    CAMPUS,
    matches("^C[ABEHW]311T[RWSMC]0[34][DNRF]$"),
    matches("^CA0SU0[34][DNRF]$")
  ) |>
  pivot_longer(-CAMPUS, names_to = "variable", values_to = "value") |>
  mutate(
    group = substr(variable, 2, 2),
    subject_code = if_else(grepl("0SU", variable), "U", substr(variable, 7, 7)),
    year = if_else(
      grepl("0SU", variable),
      substr(variable, 6, 7),
      substr(variable, 8, 9)
    ),
    field = substr(variable, nchar(variable), nchar(variable))
  ) |>
  select(-variable) |>
  pivot_wider(
    names_from = c(field, year),
    values_from = value,
    names_sep = ""
  ) |>
  mutate(across(c(D03, R03, D04, R04), ~ suppressWarnings(as.numeric(.x))))
tea_all_students_tested <- tea_measures_long |>
  filter(group == "A") |>
  select(CAMPUS, subject_code, all_tested_03 = D03)
tea_measures_long <- tea_measures_long |>
  left_join(tea_all_students_tested, by = c("CAMPUS", "subject_code")) |>
  mutate(
    subject = unname(tea_subject_names[subject_code]),
    acceptable_cutoff = case_when(subject == "mathematics" ~ 35, subject == "science" ~ 25, TRUE ~ 50),
    meets_size_2003 = case_when(
      group == "A" ~ !is.na(D03) | !is.na(R03),!is.na(D03) &
        !is.na(all_tested_03) ~
        D03 >= 50 | (D03 >= 30 & D03 / all_tested_03 >= 0.10),
      TRUE ~ NA
    ),
    evaluated_2003 = coalesce(meets_size_2003, F04 == "Y"),
    evaluated_2004 = F04 %in% "Y"
  )
tea_measures <- function(measure_year, include_sdaa) {
  rate_column <- paste0("R", measure_year)
  evaluated_column <- paste0("evaluated_20", measure_year)
  tea_measures_long |>
    filter(include_sdaa | subject != "sdaa") |>
    transmute(
      CAMPUS,
      group,
      subject,
      performance_rate = coalesce(.data[[rate_column]], 100),
      measure_evaluated = .data[[evaluated_column]] %in% TRUE,
      acceptable_cutoff
    )
}
taks_measures <- tea_measures("03", include_sdaa = FALSE)

# 6d. Apply 2003 test standards and exceptions

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
      passes_acceptable_tests = failed_acceptable == 0 |
        (
          failed_acceptable <= exceptions_allowed &
            !failed_below_exception_floor
        )
    )
}
test_rating_components <- score_test_measures(taks_measures)

# 6e. Class of 2002 completion rates

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

# 6f. Annual dropout rates (excluded from estimating ratings because definitions changed)

dropout_long <- dropout_0203 |>
  select(CAMPUS, matches("^C[ABHWE]0DR02R$")) |>
  pivot_longer(cols = -CAMPUS,
               names_to = "variable",
               values_to = "dropout_rate") |>
  extract(variable, into = "group", regex = "^C([ABHWE])0DR02R$") |>
  mutate(CAMPUS = str_pad(CAMPUS, 9, "left", "0"),
         dropout_rate = as.numeric(dropout_rate))

# 6g. Apply completion standards: A 75%, R 85%, E 95%
# AEIS lacks cohort counts for group-size rules, so use all students only.

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

# 6h. Combine test and completion ratings

assign_transition_rating <- function(test_components) {
  test_components |>
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
}
estimated_transition_rating <- assign_transition_rating(test_rating_components)

# 6i. Rating-shock indicators
# Sample: >= 200 students, an evaluated measure, and a 2001-02 rating.
# Indicators mark an increase or a drop to L, A, or R.

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
build_transition_rating <- function(estimated) {
  estimated |>
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
      rating_dropped_to_L = as.integer(
        estimated_rating_no_ri == "L" &
          rating_values[estimated_rating_no_ri] <
          rating_values[rating_2002]
      ),
      rating_dropped_to_A = as.integer(
        estimated_rating_no_ri == "A" &
          rating_values[estimated_rating_no_ri] <
          rating_values[rating_2002]
      ),
      rating_dropped_to_R = as.integer(
        estimated_rating_no_ri == "R" &
          rating_values[estimated_rating_no_ri] <
          rating_values[rating_2002]
      )
    )
}
transition_rating_0203 <- build_transition_rating(estimated_transition_rating)

# 7. Table 1 estimated-rating row and change panel

transition_rating_share <- function(rating) {
  round(100 * mean(transition_rating_0203$estimated_rating_no_ri == rating),
        1)
}
transition_change <- rating_values[transition_rating_0203$estimated_rating_no_ri] -
  rating_values[transition_rating_0203$rating_2002]
table1_transition_row <- tibble(
  year = 2003L,
  low = transition_rating_share("L"),
  acceptable = transition_rating_share("A"),
  recognized = transition_rating_share("R"),
  exemplary = transition_rating_share("E"),
  lower = round(100 * mean(transition_change < 0), 1),
  same = round(100 * mean(transition_change == 0), 1),
  higher = round(100 * mean(transition_change > 0), 1)
)
calculated_rating_0304_no_ri <- score_test_measures(tea_measures("04", include_sdaa = TRUE)) |>
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
finance_panel <- finance_longitudinal_9798_0708
dir.create("tables", showWarnings = FALSE, recursive = TRUE)

# 8. Tables 3-4 sample
# Convert counts to numbers; masked and negative values become NA. Keep rated
# campus-years with >= 200 students and split them into TAAS and TAKS regimes.

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

# 9. Table 3: school characteristics by rating and regimes
# Percentages are enrollment shares.

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

# 10. Tables 4-10 per-pupil resources
# Divide spending by same-year enrollment and convert to 2007 dollars.

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

# 10b. Staffing measures for Tables 4 and 8

staff_for_merge <- data_staf_longitudinal |>
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

# 11. Table 4: resources in t+1 by rating in t

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

# 12. Tables 5-10 regression data
# One row per campus with rating shocks, 2002-03 controls, prior ratings, and
# changes in resources, student characteristics, and staffing. Keep campuses
# rated L/A/R/E with >= 200 students in 1999-00, 2000-01, 2001-02, and 2003-04.

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
write.csv(
  rating_shock_analysis,
  "tables/table5_10_rating_shock_analysis_data.csv",
  row.names = FALSE
)

# 13. Regression model and standard errors
# Estimate equation (1) in the paper by OLS. The unchanged-rating group and Exemplary are
# omitted. Cluster standard errors by district with CR1 correction and G - 1 df.

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
fit_rating_shock_model <- function(data, outcome, extra_terms = NULL) {
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

# 14. Tables 5-8 and 10
# Resource changes: Tables 5 and 10; placebo: Table 6; student changes:
# Table 7; staffing changes: Table 8.

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

# 15. Table 9 interactions

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
campus_type_2002 <- ref_panel |>
  filter(year == 2002L) |>
  select(campus_number, campus_type_2002 = campus_type)
district_elementary <- ref_panel |>
  filter(year == 2003L) |>
  group_by(district) |>
  summarise(elementary_schools = sum(campus_type == "E"),
            .groups = "drop")
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
table9_specs <- list(
  I_baseline = list(moderator = NULL, filter = quote(TRUE)),
  II_hhi = list(
    moderator = "rating_hhi",
    filter = quote(district_sample_schools >= 5)
  ),
  III_share_R_or_E = list(moderator = "share_predicted_R_or_E", filter = quote(TRUE)),
  IV_share_falls = list(moderator = "share_rating_falls", filter = quote(TRUE)),
  V_over_20_schools = list(moderator = "district_20_plus", filter = quote(TRUE)),
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

# 16. Write formatted tables in the layout of the paper

source("code/compare_to_published.R")
