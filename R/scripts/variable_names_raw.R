# Continuous variables 

units(data$Age) <- "years"
table1::label(data$Age) <- "Age"

units(data$lesdmax) <- "mm"
table1::label(data$lesdmax) <- "Maximum lesion diameter"

units(data$soldmaxorig) <- "mm"
table1::label(data$soldmaxorig) <- "Maximum diameter of solid component"

units(data$CA125) <- "U/ml"
table1::label(data$CA125) <- "CA-125"

# Cound variables

table1::label(data$famhistovca) <- "Family members with ovarian cancer"

# Dichotomous variables

data$Ascites <- factor(data$Ascites,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
table1::label(data$Ascites) <- "Ascites"

data$papflow <- factor(data$papflow,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
table1::label(data$papflow) <- "Papillations with blood flow"

data$wallreg <- factor(data$wallreg,
  levels = c(0, 1),
  labels = c("Regular", "Irregular")
)
table1::label(data$wallreg) <- "Internal cyst wall regularity"

data$Shadows <- factor(data$Shadows,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
table1::label(data$Shadows) <- "Acoustic shadows"

data$pershistovca <- factor(data$pershistovca,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
table1::label(data$pershistovca) <- "Personal history of ovarian cancer"

data$hormtherapy <- factor(data$hormtherapy,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
table1::label(data$hormtherapy) <- "Hormonal therapy"

data$pain <- factor(data$pain,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
table1::label(data$pain) <- "Abdominal pain"

data$oncocenter <- factor(data$oncocenter,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
table1::label(data$oncocenter) <- "Examined at oncology center"

data$mal <- factor(data$mal,
  levels = c(0, 1),
  labels = c("Benign", "Malignant")
)
table1::label(data$mal) <- "Tumor classification"

# Ordinal variables

data$colscore <- factor(data$colscore,
  levels = c(1, 2, 3, 4),
  labels = c(
    "No flow",
    "Minimal flow",
    "Moderate flow",
    "Very strong flow"
  ),
  ordered = TRUE
)
table1::label(data$colscore) <- "Color score"

data$nrloculescat <- factor(data$nrloculescat,
  levels = c(0, 1, 2, 3, 4, 5, 6),
  labels = c(
    "None",
    "One",
    "Two",
    "Three",
    "Four",
    "Five to ten",
    "More than ten"
  ),
  ordered = TRUE
)
table1::label(data$nrloculescat) <- "Number of locules"

data$papnr <- factor(data$papnr,
  levels = c(0, 1, 2, 3, 4),
  labels = c(
    "None",
    "One",
    "Two",
    "Three",
    "More than three"
  ),
  ordered = TRUE
)
table1::label(data$papnr) <- "Number of papillations"
