# M-GIS 🗺️

A powerful GIS (Geographic Information System) library for the PowerQuery M-language ecosystem, bringing spatial analysis capabilities to Power BI, Excel, and other M-language environments.

## Features

- **Spatial Data Types**: Support for all standard WKT geometry types (Point, LineString, Polygon, MultiPoint, MultiLineString, MultiPolygon, GeometryCollection)
- **Spatial Indexing**: Built-in QuadTree implementation for efficient spatial queries
- **Spatial Queries**: `Intersects`, `Contains`, `Within`, and `Nearest Neighbor` operations
- **Spatial Joins**: Perform spatial joins between layers with support for `Inner`, `Left Outer`, `Right Outer`, and `Full Outer` joins
- **Well-Known Text (WKT)**: Easy geometry creation from WKT format
- **Layer Management**: Create and query spatial layers with automatic spatial indexing
- **Pure M Projections**: Declare source CRSs with EPSG, PROJ4 or PROJJSON; reproject layers and compare distances in metres

## Installation

1. Download or copy the `mgis.m` file
2. In Power BI/Excel, go to **Get Data** → **Blank Query**
3. Open the Advanced Editor and paste the contents of `mgis.m`
4. Name the query `mgis`
5. Reference it in your queries using `mgis`

## Quick Start

```powerquery
let   
    // Create a table with WKT geometry
    MyTable = #table(
        {"Name", "shape"},
        {
            {"Location A", "POINT(5 5)"},
            {"Location B", "POINT(10 10)"}
        }
    ),
    
    // Create a spatial layer
    MyLayer = mgis[gisLayerCreateFromTableWithWKT](MyTable, "shape")
in
    MyLayer[table]
```

## Projections and distances

The projection engine is implemented entirely in M. It needs no native library,
Python installation, external service, or network connection.

```powerquery
WGS84 = mgis[proj][fromEPSG][#"EPSG:4326"],
WebMercator = mgis[proj][fromEPSG][#"EPSG:3857"],
BritishNationalGrid = mgis[proj][fromEPSG][#"EPSG:27700"],
CustomCRS = mgis[proj][fromProj4](proj4String),
JSONCRS = mgis[proj][fromJSON](jsonString)
```

`fromJSON` also accepts a parsed PROJJSON record. These constructors return CRS
records, which can be passed as the final `projection` argument to layer creation.
EPSG identifiers, PROJ4 strings and PROJJSON strings can also be passed directly.
Declaring a CRS describes the existing coordinates; it does not transform them.
Coordinates always use GIS XY order: longitude/easting first, latitude/northing
second, including when a PROJJSON definition lists latitude before longitude.
Geographic coordinates use degrees. Projected coordinates use their declared units.

For example, compare a geographic layer with a projected layer using one analysis CRS:

```powerquery
let
    WGS84 = mgis[proj][fromEPSG][#"EPSG:4326"],
    UTM31 = mgis[proj][fromProj4]("+proj=utm +zone=31 +datum=WGS84 +units=m"),
    A = mgis[gisLayerCreateFromTableWithXY](
        #table({"Name", "Longitude", "Latitude"}, {{"A", 2.2945, 48.8584}}),
        "Longitude", "Latitude", WGS84
    ),
    B = mgis[gisLayerCreateFromTableWithWKT](
        #table({"Name", "shape"}, {{"B", "POINT(448352.001375365 5411954.90994727)"}}),
        "shape", UTM31
    ),
    Joined = mgis[gisLayerJoinSpatial](
        A, B, mgis[gisLayerQueryOperators][gisNearest], "Left Outer",
        [analysisCRS = UTM31]
    )
in
    Joined[table] // dist is approximately 100 metres
```

Both layers are transformed before querying, and their spatial indexes are rebuilt.
The result's `shape`, spatial index and `TProjection` use the analysis CRS; nested
`layer1` and `layer2` records preserve the original rows and original geometries.
Original attribute columns, including XY columns, retain their input values.
`gisLayerReproject(layer, targetCRS, optional options)` explicitly transforms a layer.
`gisShapeReproject(shape, sourceCRS, targetCRS, optional options)` transforms a shape.
`mgis[proj][transform]({x, y}, sourceCRS, targetCRS, optional options)` transforms a
coordinate pair, or `{x, y, heightMetres}` with an ellipsoidal height.

Nearest-neighbour joins with declared CRSs return `dist` in metres. With
`[mode="Planar", analysisCRS=...]`, distances are measured on the chosen projected
grid, including its scale distortion. With no analysis CRS, planar joins prefer
the first layer's projected CRS, or the second layer's if the first is geographic.
Two geographic point layers default to geodesic mode.

`[mode="Geodesic"]`, `gisNearestGeodesic`, and `gisNearestGeodesicN(k)` transform
both point layers to WGS84 and measure ellipsoidal surface distances in metres.
Geodesic search uses indexed branch-and-bound traversal with cached 3D unit-sphere
boxes. Sphere chord distances multiplied by the ellipsoid's minimum curvature
radius give conservative lower bounds; candidate distances use the ellipsoid.
This also handles queries across the date line and near the poles. It does not
scan every candidate by design, although loose bounds can require visiting many
nodes, as with other spatial indexes.

`mgis[proj][distance](point1, point2, crs, optional options)` measures two coordinates
in the same source CRS. Its default is geodesic distance on that CRS's datum
ellipsoid; `[mode="Planar"]` uses the projected source CRS, or the supplied
`analysisCRS`. Heights are excluded from surface distance.

Existing calls without a CRS retain their coordinate-unit behaviour. A join cannot
mix a declared CRS with an undeclared CRS. Planar metre distances in geographic
degrees are rejected; choose a projected analysis CRS or geodesic mode.

### Current projection coverage

The engine has an extensible family registry. This initial release supports
geographic coordinates (`longlat`), spherical and ellipsoidal Mercator (`merc`,
`webmerc`), sixth-order Transverse Mercator (`tmerc`, `utm`), and Lambert Conformal
Conic (`lcc`, one or two standard parallels). PROJJSON supports EPSG conversion
methods 9807, 1024, 9804, 9805, 9801 and 9802. Only the three EPSG definitions above
are built in; other CRSs in supported families can be supplied as full definitions.
The engine does not yet implement every EPSG CRS or every projection family.

Supported linear units include metres, kilometres, centimetres, millimetres,
international feet and US survey feet, plus explicit PROJ4 `+to_meter` factors.
Custom ellipsoids accept `+a` with `+rf`, `+f` or `+b`, or a spherical `+R`.
Unsupported parameters, methods, axis directions, non-Greenwich prime meridians
and non-2D PROJJSON CRSs produce errors rather than being silently ignored.

Datum changes use explicit three- or seven-parameter position-vector Helmert
operations through WGS84. Approximate operations require
`[allowApproximateDatum=true]`; this includes the built-in OSGB36 Helmert operation.
The EPSG:27700 definition can be used for same-datum projection without this option.
PROJ4 `+nadgrids` references are retained, but grid files are not decoded yet and
required grids never silently fall back to Helmert. For a custom grid or another
datum operation, `mgis[proj][withDatumTransform](crs, intoWGS84, fromWGS84)` attaches
two pure-M functions. Each accepts and returns
`{longitudeRadians, latitudeRadians, ellipsoidalHeightMetres}`; they must implement
the complete datum change in their respective direction.

Geodesic distance currently uses Vincenty's ellipsoidal inverse algorithm.
Nearly antipodal points can fail to converge; this raises a clear error rather
than returning an inaccurate spherical fallback. Geodesic nearest queries support
points only. Planar nearest queries for other geometry types continue to use
envelope centres. Reprojection transforms stored vertices; it does not densify
curved projected edges or split polygons crossing the date line. Transverse
Mercator rejects coordinates outside its supported series domain (`|eta| <= 1`
on the front hemisphere), and projected pole coordinates are rejected.

## API Reference

### Shape Creation

#### `gisShapeCreateFromWKT(wkt as text)`
Creates a shape from Well-Known Text representation.

```powerquery
shape = GISLib[gisShapeCreateFromWKT]("POINT(5 10)")
```

### Layer Creation

#### `gisLayerCreateBlank(geometryColumn as nullable text, capacity as nullable number, optional projection)`
Creates a blank layer with no data.

#### `gisLayerCreateFromTable(table as table, geometryColumn as text, optional projection)`
Creates a layer from a table containing TShape objects.

#### `gisLayerCreateFromTableWithWKT(table as table, wktColumn as text, optional projection)`
Creates a layer from a table with WKT text in the specified column.

```powerquery
Layer = GISLib[gisLayerCreateFromTableWithWKT](MyTable, "shape")
```

#### `gisLayerCreateFromTableWithXY(table as table, xColumn as text, yColumn as text, optional projection)`
Creates a point layer from numeric X and Y columns. Preserves the original columns and adds a `shape` column plus the usual row IDs and spatial index. X is longitude/easting; Y is latitude/northing. Coordinates are used as supplied, without reprojection.

Both coordinate columns must exist and contain non-null numbers. The input table must not already contain a `shape` column.

```powerquery
let
    MyTable = #table(
        {"Name", "XColName", "YColName"},
        {{"Location A", 5, 10}, {"Location B", 15, 20}}
    ),
    MyLayer = mgis[gisLayerCreateFromTableWithXY](MyTable, "XColName", "YColName")
in
    MyLayer[table]
```

#### `gisLayerCreateFromTableWithGeoJSON(table as table, geometryColumn as text, optional projection)`
Creates a layer from a column of GeoJSON geometry or Feature objects, supplied as
JSON strings or parsed records. Supports all seven geometry types. The default
source CRS is WGS84, as in standard GeoJSON; projected GeoJSON requires an explicit
CRS. Null and empty geometries are not supported. `gisShapeCreateFromGeoJSON(json)`
creates an individual shape. Geometry is analysed in 2D.

### Spatial Query Operators

Access operators via `GISLib[gisLayerQueryOperators]`:

- **`gisIntersects`** - Returns shapes whose envelopes intersect the query shape
- **`gisContains`** - Returns shapes whose envelopes contain the query shape
- **`gisWithin`** - Returns shapes whose envelopes are within the query shape
- **`gisNearest`** - Returns the single nearest shape by envelope-centre distance
- **`gisNearestN(k as number)`** - Returns up to k nearest shapes by envelope-centre distance
- **`gisNearestGeodesic`** - Returns the nearest point by ellipsoidal surface distance in metres
- **`gisNearestGeodesicN(k as number)`** - Returns up to k nearest points by ellipsoidal surface distance in metres

### Layer Operations

#### `gisLayerQuerySpatial(layer, shape, operator, optional projection, optional options)`
Queries a layer using a spatial operator. `projection` describes the query shape's
source CRS (default: the layer's CRS); `options` accepts the same analysis settings
as a join. Returns selected original rows with operator columns such as `dist`,
preserving the layer's original CRS and rebuilding its filtered index.

#### `gisLayerQueryRelational(layer, function)`
Filters a layer using an attribute-based function (like `Table.SelectRows`).

#### `gisLayerJoinSpatial(layer1, layer2, operator, joinType, optional options)`
Performs a spatial join between two layers.

**Parameters:**
- `layer1`, `layer2`: The layers to join
- `operator`: Spatial operator (e.g., `gisIntersects`, `gisContains`)
- `joinType`: `"Inner"`, `"Left Outer"`, `"Right Outer"`, or `"Full Outer"`
- `options`: Optional record with `analysisCRS`, `mode` (`"Planar"` or `"Geodesic"`), and `allowApproximateDatum`

**Returns:** A layer with columns:
- `__rowid__`: Unique row identifier
- `layer1`: Record from the first layer
- `layer2`: Record from the second layer
- `shape`: The geometry from the matched row
- Additional columns from the operator (e.g., `dist` for nearest neighbor queries)

#### `gisLayerInsertRows(layer, rows as list)`
Inserts rows into a layer and updates the spatial index.

## Examples

### Example 1: Point-in-Polygon Query

Find which points fall within which zones using a spatial join with the `gisContains` operator.

```powerquery
let
    gisLayerCreateFromTableWithWKT = mgis[gisLayerCreateFromTableWithWKT],
    gisLayerJoinSpatial = mgis[gisLayerJoinSpatial],
    gisContains = mgis[gisLayerQueryOperators][gisContains],

    // Create polygon zones
    Zones = #table(
        {"Name", "shape"},
        {
            {"ZoneA", "POLYGON((0 0, 0 10, 10 10, 10 0, 0 0))"},
            {"ZoneB", "POLYGON((10 0, 10 10, 20 10, 20 0, 10 0))"}
        }
    ),
    ZonesLayer = gisLayerCreateFromTableWithWKT(Zones, "shape"),

    // Create points
    Points = #table(
        {"ID", "shape", "description"},
        {
            {1, "POINT(5 5)", "inside ZoneA"},
            {2, "POINT(15 5)", "inside ZoneB"},
            {3, "POINT(25 5)", "outside both"}
        }
    ),
    PointsLayer = gisLayerCreateFromTableWithWKT(Points, "shape"),

    // Spatial join: which points are within which zones
    Joined = gisLayerJoinSpatial(PointsLayer, ZonesLayer, gisContains, "Left Outer")[table]
in
    Joined
```

**Result:** A table showing each point and the zone it falls within (if any).

### Example 2: Polygon Intersection Analysis

Analyze how sub-zones relate to main zones using multiple spatial operators.

```powerquery
let
    gisLayerCreateFromTableWithWKT = mgis[gisLayerCreateFromTableWithWKT],
    gisLayerJoinSpatial = mgis[gisLayerJoinSpatial],
    gisIntersects = mgis[gisLayerQueryOperators][gisIntersects],
    gisContains = mgis[gisLayerQueryOperators][gisContains],
    gisWithin = mgis[gisLayerQueryOperators][gisWithin],

    // Main zones
    Zones = #table(
        {"Name", "shape", "description"},
        {
            {"ZoneA", "POLYGON((0 0, 0 10, 10 10, 10 0, 0 0))", "Main zone A"},
            {"ZoneB", "POLYGON((10 0, 10 10, 20 10, 20 0, 10 0))", "Main zone B"}
        }
    ),
    ZonesLayer = gisLayerCreateFromTableWithWKT(Zones, "shape"),

    // Sub-zones
    SubZones = #table(
        {"SubName", "shape", "description"},
        {
            {"Sub1", "POLYGON((2 2, 2 4, 4 4, 4 2, 2 2))", "Inside ZoneA"},
            {"Sub2", "POLYGON((9 2, 9 8, 12 8, 12 2, 9 2))", "Overlaps both"},
            {"Sub3", "POLYGON((22 2, 22 8, 24 8, 24 2, 22 2))", "Outside both"}
        }
    ),
    SubZonesLayer = gisLayerCreateFromTableWithWKT(SubZones, "shape"),

    // Find intersections
    Intersections = gisLayerJoinSpatial(SubZonesLayer, ZonesLayer, gisIntersects, "Inner"),
    
    // Find containment (sub-zones fully within zones)
    Contains = gisLayerJoinSpatial(SubZonesLayer, ZonesLayer, gisContains, "Inner")
in
    Intersections[table]
```

**Result:** Analysis of how sub-zones spatially relate to main zones.

### Example 3: Nearest Neighbor Analysis

Find the nearest shop to each house using the `gisNearest` operator.

```powerquery
let
    gisLayerCreateFromTableWithWKT = mgis[gisLayerCreateFromTableWithWKT],
    gisLayerJoinSpatial = mgis[gisLayerJoinSpatial],
    gisNearest = mgis[gisLayerQueryOperators][gisNearest],

    // Houses layer
    Houses = #table(
        {"HouseID", "shape"},
        {
            {"H1", "POINT(1 1)"},
            {"H2", "POINT(4 3)"},
            {"H3", "POINT(9 6)"},
            {"H4", "POINT(14 3)"},
            {"H5", "POINT(18 8)"}
        }
    ),
    HousesLayer = gisLayerCreateFromTableWithWKT(Houses, "shape"),

    // Shops layer
    Shops = #table(
        {"ShopID", "shape"},
        {
            {"S1", "POINT(0 0)"},
            {"S2", "POINT(5 2)"},
            {"S3", "POINT(10 6)"},
            {"S4", "POINT(15 3)"},
            {"S5", "POINT(20 10)"}
        }
    ),
    ShopsLayer = gisLayerCreateFromTableWithWKT(Shops, "shape"),

    // Find nearest shop to each house
    Joined = gisLayerJoinSpatial(HousesLayer, ShopsLayer, gisNearest, "Left Outer"),

    // Extract readable results
    Result = Table.ExpandRecordColumn(
        Table.ExpandRecordColumn(Joined[table], "layer1", {"HouseID"}, {"HouseID"}),
        "layer2",
        {"ShopID"},
        {"NearestShopID"}
    )
in
    Result
```

**Result:** A table showing each house and its nearest shop, including the distance.

To keep only nearest matches within 10 metres, use coordinates in the same projected coordinate system with metre units:

```powerquery
let
    Joined = mgis[gisLayerJoinSpatial](
        HousesLayer,
        ShopsLayer,
        mgis[gisLayerQueryOperators][gisNearest],
        "Inner"
    ),
    Within10m = Table.SelectRows(Joined[table], each [dist] <= 10)
in
    Within10m
```

This returns only houses with a nearest shop at most 10 metres away. Distances use the supplied coordinate units without reprojection; longitude/latitude degrees are not metres. For points the distance is the straight-line distance; for lines and polygons it is the distance between bounding-box centres.

### Example 4: Within Operator

Find zones that are within sub-zones (reverse containment).

```powerquery
let
    gisLayerCreateFromTableWithWKT = mgis[gisLayerCreateFromTableWithWKT],
    gisLayerJoinSpatial = mgis[gisLayerJoinSpatial],
    gisWithin = mgis[gisLayerQueryOperators][gisWithin],

    // Create layers (as in Example 1)
    Zones = #table(
        {"Name", "shape"},
        {
            {"ZoneA", "POLYGON((0 0, 0 10, 10 10, 10 0, 0 0))"},
            {"ZoneB", "POLYGON((10 0, 10 10, 20 10, 20 0, 10 0))"}
        }
    ),
    ZonesLayer = gisLayerCreateFromTableWithWKT(Zones, "shape"),

    Points = #table(
        {"ID", "shape"},
        {
            {1, "POINT(5 5)"},
            {2, "POINT(15 5)"},
            {3, "POINT(7 4)"}
        }
    ),
    PointsLayer = gisLayerCreateFromTableWithWKT(Points, "shape"),

    // Find zones within point envelopes (reverse relationship)
    Joined = gisLayerJoinSpatial(ZonesLayer, PointsLayer, gisWithin, "Left Outer")[table]
in
    Joined
```

## Join Types

The library supports four join types:

- **`"Inner"`**: Returns only matching rows from both layers
- **`"Left Outer"`**: Returns all rows from the first layer, with matches from the second
- **`"Right Outer"`**: Returns all rows from the second layer, with matches from the first
- **`"Full Outer"`**: Returns all rows from both layers, matching where possible

## Important Notes

1. **Envelope-based queries**: All spatial operators (`gisIntersects`, `gisContains`, `gisWithin`) currently perform envelope-based tests, not true geometric operations. This means they test bounding boxes, not the actual geometry shapes.

2. **Performance**: The library uses a QuadTree spatial index for efficient querying. For best performance with large datasets, ensure appropriate capacity settings when creating layers. Nearest-neighbour operators use branch-and-bound traversal: visit closer cells first and skip subtrees whose cached envelope-centre bounds cannot improve the current nearest k results. Queries outside the indexed extent are supported.

3. **Row IDs**: The library automatically manages `__rowid__` columns for internal tracking. You don't need to create these manually.

4. **WKT Format**: Geometries should be in standard WKT format:
   - `POINT(x y)`
   - `LINESTRING(x1 y1, x2 y2, ...)`
   - `POLYGON((x1 y1, x2 y2, ..., x1 y1))` (note: first and last points should match)
   - And all multi-geometries and geometry collections

## Use Cases

- **Retail Analysis**: Find stores within delivery zones, nearest competitor locations
- **Demographics**: Analyze population points within administrative boundaries
- **Logistics**: Route optimization, service area analysis
- **Environmental**: Habitat analysis, pollution zone mapping
- **Real Estate**: Property location analysis, market area definitions

## Tests

Run all M test queries with the installed Excel Power Query engine:

```powershell
pwsh -NoProfile -File .\tests\TestAll.ps1
```

The runner requires Windows and an installed Power Query engine. It automatically uses Windows PowerShell for the .NET Framework engine, fully evaluates each result, and exits with code 1 if any assertion or query fails. Use `-EnginePath` to specify another compatible `Microsoft.MashupEngine.dll` installation, or `-LibraryPath` to test another copy of `mgis.m`.

Tests 1–4 assert the expected matches and distances while still returning example tables. Tests 5–6 cover XY layer creation and nearest-neighbour regressions, including agreement with exhaustive distances and proof that distant branches are pruned. The separate `Test1Projected.m` through `Test6Projected.m` companions repeat these scenarios with declared CRSs while leaving the originals as null-CRS regressions. They cover mixed EPSG:3857/EPSG:4326 joins, metre distances from feet-based analysis, geographic XY creation, and every nearest-neighbour/pruning regression in both metre and feet units. Tests 7–9 cover independent projection/geodesic references, PROJJSON and PROJ4 validation, mixed-CRS queries and joins, metre units, geometry reprojection, GeoJSON, date-line and polar searches, duplicate points, and explicit proof of geodesic branch pruning.

The runner supplies `projectionReferences` from the committed M literals in
`tests/data/projection-references.m`. Their provenance is recorded in
`tests/GenerateProjectionReferences.py`; pyproj and GeographicLib are needed only
to regenerate these independent development fixtures, never to run the tests or
use `mgis`.

## Contributing

Contributions are welcome! This library is designed to bring enterprise-grade GIS capabilities to the M-language ecosystem.

## Author

**Sancarn** - [GitHub](https://github.com/sancarn/mgis)

## License

This project is open source and available for use in both personal and commercial projects.
