#!/bin/bash
rm plots/*crop.pdf
for FILE in plots/*.pdf; do
  echo $FILE
  pdfcrop "${FILE}"
done

rm plots_cv/*crop.pdf
for FILE in plots_cv/*.pdf; do
  echo $FILE
  pdfcrop --margins '0 -25 0 0' "${FILE}"
done
