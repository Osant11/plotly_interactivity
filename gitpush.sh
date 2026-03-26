#!/bin/bash
# Usage: bash gitpush.sh "commit message"
MSG="${1:-auto commit}"
git add . && git commit -m "$MSG" && git push
