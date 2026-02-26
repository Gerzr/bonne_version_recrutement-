library(tidyverse)
library(robotoolbox)
library(janitor)
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(rJava)
library(mailR)
library(stringr)


# Charger les variables d'environnement
api_token <- Sys.getenv("KOBO_API_TOKEN")
form_id <- Sys.getenv("KOBO_FORM_ID")


# Vérifier les variables d'environnement
if (is.na(api_token) ||
    is.na(form_id) || api_token == "" || form_id == "") {
  stop(
    "L'API Token ou l'ID du formulaire est manquant. Vérifiez vos variables d'environnement."
  )
}

# URL de l'API KoboToolbox
url <- paste0("https://kf.kobotoolbox.org/api/v2/assets/",
              form_id,
              "/data/")

# Effectuer la requête GET
response <- GET(url, add_headers(Authorization = paste("Token", api_token)))
# Vérifier la réponse HTTP
if (status_code(response) != 200) {
  cat("Erreur lors de la récupération des données KoboToolbox.\n")
  cat(content(response, as = "text"), "\n")
  stop("Arrêt du script.")
}

# Convertir la réponse JSON en liste
data <- content(response, as = "parsed", simplifyVector = TRUE)$results

# Vérifier si des soumissions existent
if (length(data) == 0) {
  stop("Aucune soumission trouvée dans KoboToolbox.")
}

# Charger la liste des soumissions déjà envoyées
sent_submissions_file <- "sent_submissions.json"
if (file.exists(sent_submissions_file)) {
  sent_submissions <- fromJSON(sent_submissions_file)
} else {
  sent_submissions <- data.frame(id = character(), stringsAsFactors = FALSE)
}

# Extraire les informations des participants
participants <- data %>%
  mutate(
    iid = `_id`,
    email = `Veuillez_entrer_votre_addresse_e_mail`,
    nb_abstract = as.integer(`group_lu9eh78/nbdiplomes`),
    nom = `Veuillez_entrer_votr_en_lettres_capitales`,
    prenom = Veuillez_entrer_votre_ou_vos_pr_nom_s,
    submission_time = `_submission_time`
  ) %>%
  select(iid, email, nb_abstract, nom, prenom, submission_time)

# Vérifier que abstracts_group existe et l'extraire proprement
abstracts_group <- data %>%
  select(`_id`,
         `group_lu9eh78/nbdiplomes`,
         veillez_entre_la_lettre_motiva_001) %>%
  filter(!map_lgl(`group_lu9eh78/nbdiplomes`, is.null)) %>%
  mutate(`group_lu9eh78/nbdiplomes` = map(`group_lu9eh78/nbdiplomes`, as_tibble)) %>%
  unnest(`group_lu9eh78/nbdiplomes`) %>%
  select(`_id`, title = veillez_entre_la_lettre_motiva_001)

# Associer les attestaions aux candidats
submissions <- left_join(participants %>% rename(`_id` = iid), abstracts_group, by = "_id")

# Filtrer les soumissions avec email non vide
submissions <- submissions %>% filter(!is.na(email))

# Remplacer nb_abstract manquant par 0
submissions <- submissions %>% mutate(nb_abstract = replace_na(nb_abstract, 0))

# Grouper les abstracts par email
submissions <- submissions %>%
  group_by(email, nom, prenom, submission_time) %>%
  summarise(title = paste(na.omit(title), collapse = "; "), .groups = "drop")

# Filtrer les nouvelles soumissions
data_df <- as_tibble(data)

new_submissions <- data_df %>%
  filter(!`_id` %in% sent_submissions$id)

if (nrow(new_submissions) == 0) {
  cat("Aucune nouvelle soumission.\n")
  quit(status = 0)
}

# Charger les informations d'authentification pour l'email
email_user <- Sys.getenv("MAIL_USERNAME_GHA")
email_pass <- Sys.getenv("MAIL_PASSWORD_GHA")

if (email_user == "" || email_pass == "") {
  stop("Identifiants email manquants.")
}



# =========================
# FONCTION TELECHARGEMENT
# =========================

download_attachments <- function(attachments_df) {
  if (is.null(attachments_df))
    return(NULL)
  
  files <- c()
  
  for (j in seq_len(nrow(attachments_df))) {
    file_url  <- attachments_df$download_url[j]
    file_name <- attachments_df$media_file_basename[j]
    
    temp_file <- tempfile(fileext = paste0(".", tools::file_ext(file_name)))
    
    res <- GET(
      file_url,
      add_headers(Authorization = paste("Token", api_token)),
      write_disk(temp_file, overwrite = TRUE)
    )
    
    if (status_code(res) == 200) {
      files <- c(files, temp_file)
    }
  }
  
  return(files)
}

for (i in seq_len(nrow(new_submissions))) {
  submission_id <- new_submissions$`_id`[i]
  
  # Récupérer la ligne correspondante dans data
  row_index <- which(data$`_id` == submission_id)
  
  nom     <- data$Veuillez_entrer_votr_en_lettres_capitales[row_index]
  prenom  <- data$Veuillez_entrer_votre_ou_vos_pr_nom_s[row_index]
  email   <- data$Veuillez_entrer_votre_addresse_e_mail[row_index]
  date_sub <- data$`_submission_time`[row_index]
  
  if (is.na(email) || email == "")
    next
  
  # Télécharger les pièces jointes
  attachments_df <- data$`_attachments`[[row_index]]
  
  if (is.null(attachments_df)) {
    attachments_files <- NULL
    types_docs <- character(0)
  } else {
    attachments_files <- download_attachments(attachments_df)
    types_docs <- attachments_df$question_xpath
  }
  
  types_docs <- attachments_df$question_xpath
  
  lettre   <- sum(grepl("motivation", types_docs))
  cv       <- sum(grepl("motiva_001", types_docs))
  diplome  <- sum(grepl("motiva$", types_docs))
  attest   <- sum(grepl("attestations", types_docs))
  
  body_mail <- paste0(
    "<p>Bonjour,</p>",
    "<p>Une nouvelle candidature a été soumise.</p>",
    
    "<h3>Informations du candidat</h3>",
    "<ul>",
    "<li><b>Nom :</b> ",
    nom,
    "</li>",
    "<li><b>Prénom :</b> ",
    prenom,
    "</li>",
    "<li><b>Email :</b> ",
    email,
    "</li>",
    "<li><b>Date de soumission :</b> ",
    date_sub,
    "</li>",
    "</ul>",
    
    "<h3>Documents joints</h3>",
    "<ul>",
    "<li>Lettre de motivation : ",
    lettre,
    "</li>",
    "<li>CV : ",
    cv,
    "</li>",
    "<li>Diplôme(s) : ",
    diplome,
    "</li>",
    "<li>Attestation(s) : ",
    attest,
    "</li>",
    "</ul>",
    
    "<p>Cordialement,<br>",
    "<b>Système automatisé de recrutement</b></p>"
  )
  #"caresp2011@gmail.com"
  send.mail(
    from = email_user,
    to = "koglogerard@gmail.com",
    subject = paste("Nouvelle candidature -", nom, prenom),
    body = body_mail,
    smtp = list(
      host.name = "smtp.gmail.com",
      port = 587,
      user.name = email_user,
      passwd = email_pass,
      tls = TRUE
    ),
    authenticate = TRUE,
    send = TRUE,
    html = TRUE,
    attach.files = attachments_files
  )
  
  cat("Email envoyé pour :", nom, prenom, "\n")
}

# Ajouter les nouveaux IDs à la liste
updated_ids <- unique(c(sent_submissions$id, new_submissions$`_id`))

write_json(data.frame(id = updated_ids), sent_submissions_file, pretty = TRUE)

# Envoi des emails aux nouvelles soumissions
# =========================
# ACCUSE DE RECEPTION CANDIDAT
# =========================

body_ack <- paste0(
  "<p>Bonjour <b>", prenom, " ", nom, "</b>,</p>",
  
  "<p>Nous accusons réception de votre candidature transmise le <b>",
  date_sub, "</b>.</p>",
  
  "<p>Votre dossier a bien été enregistré et sera examiné avec attention par le comité de sélection.</p>",
  
  "<p>Si votre profil est retenu, vous serez contacté(e) pour la suite du processus.</p>",
  
  "<p>Nous vous remercions pour l'intérêt que vous portez à notre recrutement et vous souhaitons bonne chance.</p>",
  
  "<p>Cordialement,</p>",
  "<p><b>Le Secrétariat du Recrutement</b></p>"
)

tryCatch({
  
  send.mail(
    from = email_user,
    to = email,
    subject = "Accusé de réception de votre candidature",
    body = body_ack,
    smtp = list(
      host.name = "smtp.gmail.com",
      port = 587,
      user.name = email_user,
      passwd = email_pass,
      tls = TRUE
    ),
    authenticate = TRUE,
    send = TRUE,
    html = TRUE
  )
  
  cat("Accusé de réception envoyé à :", email, "\n")
  
}, error = function(e) {
  
  cat("Erreur lors de l'envoi à", email, ":", e$message, "\n")
  
})

cat("Accusé de réception envoyé à :", email, "\n")