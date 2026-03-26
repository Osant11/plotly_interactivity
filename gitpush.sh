#!/bin/bash
# Usage: bash gitpush.sh "commit message"
MSG="${1:-auto commit}"
cd /home/astos1/plotly_interactivity && git add . && git commit -m "$MSG" && git push
