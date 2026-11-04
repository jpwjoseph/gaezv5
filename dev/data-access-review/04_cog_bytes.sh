#!/usr/bin/env bash
# Bytes transferred by GDAL for windowed and overview reads (needs GDAL >= 3.1 CLI).
set -euo pipefail
G="/vsicurl/https://storage.googleapis.com/fao-gismgr-gaez-v5-data/DATA/GAEZ-V5/MAPSET/RES05-YCX30AS"
export GDAL_DISABLE_READDIR_ON_OPEN=EMPTY_DIR CPL_VSIL_CURL_ALLOWED_EXTENSIONS=.tif \
       GDAL_HTTP_MULTIRANGE=YES GDAL_HTTP_MERGE_CONSECUTIVE_RANGES=YES
OUT="$(mktemp -d)"
bytes() { grep -i "^< content-range" | sed -E 's/.*bytes ([0-9]+)-([0-9]+).*/\1 \2/' |
          awk -v l="$1" '{s+=$2-$1+1; n++} END {printf "%-40s %7.2f MB in %d range requests\n", l, s/1e6, n}'; }
for f in ALF MZE; do gdalinfo "$G/GAEZ-V5.RES05-YCX30AS.HP0120.AGERA5.HIST.$f.HRLM.tif" | grep -E "Size is|LAYOUT|Overviews" ; done
CPL_CURL_VERBOSE=YES gdal_translate -q -projwin 0.1 23.5 16.0 11.7 \
  "$G/GAEZ-V5.RES05-YCX30AS.HP0120.AGERA5.HIST.ALF.HRLM.tif" "$OUT/ner.tif" 2>&1 | bytes "Niger window, alfalfa (file 210 MB)"
CPL_CURL_VERBOSE=YES gdal_translate -q -projwin 2.7 13.9 14.7 4.2 \
  "$G/GAEZ-V5.RES05-YCX30AS.HP0120.AGERA5.HIST.MZE.HRLM.tif" "$OUT/nga.tif" 2>&1 | bytes "Nigeria window, maize (file 287 MB)"
CPL_CURL_VERBOSE=YES gdal_translate -q -outsize 900 450 \
  "$G/GAEZ-V5.RES05-YCX30AS.HP0120.AGERA5.HIST.ALF.HRLM.tif" "$OUT/prev.tif" 2>&1 | bytes "Global 900x450 preview"
rm -rf "$OUT"
