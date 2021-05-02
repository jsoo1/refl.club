#! /bin/sh
set -e
gcloud builds submit --tag "gcr.io/$PROJECT/refl-club"
gcloud run deploy refl-club --image "gcr.io/$PROJECT/refl-club" --platform managed
