"""Regenerate independent fixtures; development only, never used by mgis.

Requires pyproj and geographiclib. Run from this directory with Python.
Runtime tests consume only the committed M literals, without either dependency.
"""
from pathlib import Path
from pyproj import CRS, Transformer, __version__ as proj_version
from geographiclib.geodesic import Geodesic

ROOT = Path(__file__).resolve().parent
WGS = "EPSG:4326"
transforms = []


def check(source, target, point, tolerance=0.001, approximate=False):
    source_crs, target_crs = CRS.from_user_input(source), CRS.from_user_input(target)
    if len(point) == 3:
        source_crs, target_crs = source_crs.to_3d(), target_crs.to_3d()
    transformer = Transformer.from_crs(source_crs, target_crs, always_xy=True)
    expected = list(transformer.transform(*point, errcheck=True))
    transforms.append(dict(source=source, target=target, point=point,
                           expected=expected, tolerance=tolerance, approximate=approximate))


merc = "+proj=merc +datum=WGS84 +lon_0=12 +lat_ts=25 +x_0=12345 +y_0=-54321 +units=m +type=crs"
sphere = "+proj=merc +R=6371000 +datum=WGS84 +units=m +type=crs"
lcc = "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 +datum=WGS84 +units=m +type=crs"
lcc_south = "+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=-27 +lon_0=135 +datum=WGS84 +units=m +type=crs"
lcc_tangent = "+proj=lcc +lat_1=45 +lat_0=45 +lon_0=3 +k=0.999 +datum=WGS84 +units=m +type=crs"
tm = "+proj=tmerc +lat_0=10 +lon_0=12 +k=0.9999 +x_0=25000 +y_0=-10000 +datum=WGS84 +units=m +type=crs"
feet = "+proj=merc +datum=WGS84 +x_0=100 +y_0=-200 +units=us-ft +type=crs"
utm_south = "+proj=utm +zone=56 +south +datum=WGS84 +units=m +type=crs"
utm_north = "+proj=utm +zone=31 +datum=WGS84 +units=m +type=crs"
osgb_geo = "+proj=longlat +datum=OSGB36 +type=crs"
osgb_grid = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +type=crs"
for target, points in [
    ("EPSG:3857", [[0, 0], [2, 49], [-73.9857, 40.7484], [151.2, -33.9], [179, 80]]),
    (merc, [[12, 25], [-10, 55], [40, -35], [2, 85]]),
    (sphere, [[10, 52], [-20, -40]]),
    (lcc, [[-75, 35], [-96, 39], [-120, 50]]),
    (lcc_south, [[151, -33], [135, -27], [115, -20]]),
    (lcc_tangent, [[5, 46]]),
    (tm, [[13, 51], [-20, 20], [40, -30]]),
    (utm_north, [[3, 0], [2.2945, 48.8584], [5, 80]]),
    (utm_south, [[151.2093, -33.8688]]),
    (feet, [[1, 2]]),
]:
    for point in points:
        check(WGS, target, point)
        projected = transforms[-1]["expected"]
        check(target, WGS, projected, 1e-8)

check(osgb_geo, osgb_grid, [1.7179215806451, 52.657570301933])
check(osgb_grid, osgb_geo, [651409.903, 313177.270], 1e-8)
# Explicit bound CRS uses the same published seven-parameter Helmert operation.
bound = osgb_grid.replace("+datum=OSGB36", "+ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")
check(WGS, bound, [-0.1278, 51.5074, 0], 0.001, True)
check(bound, WGS, [530028.746, 180380.095, 0], 0.001, True)

json_crs = [dict(code=code, definition=CRS.from_epsg(code).to_json()) for code in [4326, 3857, 27700, 32631, 32756, 3395, 2154]]
for entry in json_crs:
    if entry["code"] == 27700:
        check(osgb_geo, entry["definition"], [1.7179215806451, 52.657570301933])
    elif entry["code"] == 32756:
        check(WGS, entry["definition"], [151.2093, -33.8688])
    elif entry["code"] == 2154:
        # Test the LCC conversion on its own datum, without assuming a datum
        # transformation is available merely because its ellipsoid is GRS80.
        check(CRS.from_epsg(2154).geodetic_crs.to_json(), entry["definition"], [2.2945, 48.8584])
    else:
        check(WGS, entry["definition"], [2.2945, 48.8584])
distances = []
for first, second in [([0, 0], [1, 0]), ([0, 0], [0, 1]),
                       ([-0.1278, 51.5074], [2.3522, 48.8566]),
                       ([179.9, 10], [-179.9, 10]), ([0, 90], [120, 80]),
                       ([151.2, -33.9], [-73.9, 40.7]), ([1, 2], [1, 2])]:
    expected = Geodesic.WGS84.Inverse(first[1], first[0], second[1], second[0])["s12"]
    distances.append(dict(first=first, second=second, expected=expected))


def m(value):
    if isinstance(value, dict):
        return "[" + ",".join(key + "=" + m(val) for key, val in value.items()) + "]"
    if isinstance(value, list):
        return "{" + ",".join(m(val) for val in value) + "}"
    if isinstance(value, str):
        return '"' + value.replace('"', '""').replace("#(", "#(#)(") + '"'
    if isinstance(value, bool):
        return "true" if value else "false"
    return repr(value)


fixture = dict(transforms=transforms, jsonCRS=json_crs, distances=distances)
header = f"// Generated with pyproj {proj_version} and GeographicLib; see GenerateProjectionReferences.py.\n"
(ROOT / "data" / "projection-references.m").write_text(header + m(fixture) + "\n", encoding="utf-8")
print(f"Generated {len(transforms)} coordinate and {len(distances)} geodesic references.")
