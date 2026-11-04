# Minimal WMTS GetTile server for offline checks of the WMTS client.
# Serves web-mercator PNG tiles warped from a WGS84 GeoTIFF (needs gdalwarp, gdal_translate).
# Usage: python3 06_wmts_mock_server.py <source.tif> <port> <request.log>
import http.server, os, subprocess, sys, tempfile, urllib.parse
SRC, PORT, LOG = sys.argv[1], int(sys.argv[2]), sys.argv[3]
HALF = 20037508.342789244
EXC = ('<?xml version="1.0"?><ows:ExceptionReport xmlns:ows="http://www.opengis.net/ows/1.1">'
       '<ows:Exception exceptionCode="InvalidParameterValue"><ows:ExceptionText>%s</ows:ExceptionText>'
       '</ows:Exception></ows:ExceptionReport>')

class Handler(http.server.BaseHTTPRequestHandler):
    def reply(self, code, ctype, body):
        self.send_response(code); self.send_header("Content-Type", ctype); self.end_headers(); self.wfile.write(body)
    def do_GET(self):
        q = {k.lower(): v[0] for k, v in urllib.parse.parse_qs(urllib.parse.urlparse(self.path).query).items()}
        with open(LOG, "a") as f: f.write(self.path + "\n")
        missing = [k for k in ["service", "request", "layer", "style", "tilematrixset", "tilematrix", "tilerow", "tilecol", "format"] if k not in q]
        if missing: return self.reply(400, "application/xml", (EXC % ("missing " + ",".join(missing))).encode())
        if "BAD" in q["layer"]: return self.reply(400, "application/xml", (EXC % ("Unknown layer " + q["layer"])).encode())
        z, row, col = int(q["tilematrix"]), int(q["tilerow"]), int(q["tilecol"])
        size = 2 * HALF / 2 ** z; xmin = -HALF + col * size; ymax = HALF - row * size
        d = tempfile.mkdtemp(); vrt = os.path.join(d, "t.vrt"); png = os.path.join(d, "t.png")
        subprocess.run(["gdalwarp", "-q", "-of", "VRT", "-t_srs", "EPSG:3857", "-te", str(xmin), str(ymax - size),
                        str(xmin + size), str(ymax), "-ts", "256", "256", "-r", "near", SRC, vrt], check=True)
        subprocess.run(["gdal_translate", "-q", "-of", "PNG", vrt, png], check=True)
        body = open(png, "rb").read()
        subprocess.run(["rm", "-rf", d])
        self.reply(200, "image/png", body)
    def log_message(self, *a): pass

http.server.ThreadingHTTPServer(("127.0.0.1", PORT), Handler).serve_forever()
