#!/bin/bash
for FILE in plots/*.pdf; do
  echo $FILE
  pdfcrop "${FILE}"
done
